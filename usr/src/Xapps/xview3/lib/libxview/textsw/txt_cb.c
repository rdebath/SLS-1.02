#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)txt_cb.c 50.20 91/09/14";
#endif
#endif

/*
 *	(c) Copyright 1990 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */
 
#include <xview_private/primal.h>
#include <xview_private/txt_impl.h>
#include <xview_private/ev_impl.h>
#include <xview/textsw.h>
#include <xview/xv_i18n.h> 

extern void	textsw_update_region();
Xv_private void	textsw_pre_edit_done();


Xv_private	void
textsw_pre_edit_start(ic, client_data, callback_data)
    XIC			ic;
    XPointer		client_data;
    XPointer		callback_data;
{
    Textsw		textsw = (Textsw)client_data;
    Textsw_folio	folio = TEXTSW_PRIVATE(textsw);
    Es_index		first, last_plus_one, ro_point;
 
    if (!EV_MARK_IS_NULL(&folio->temp_mark)) {
	/*  BIG BUG: Should return an error XIMCB_NotDone*/
	return;
    }

    folio->temp_mark = textsw_add_mark_internal(folio,
				EV_GET_INSERT(folio->views), NULL);
    /* Turn off undo and again before inserting pre_edit text */
    folio->state |= (TXTSW_NO_UNDO_RECORDING | TXTSW_NO_AGAIN_RECORDING);
}


Xv_private	void
textsw_pre_edit_done(ic, client_data, callback_data)
    XIC			ic;
    XPointer		client_data;
    XPointer		callback_data;
{
    Textsw		textsw = (Textsw)client_data;
    Textsw_folio	folio = TEXTSW_PRIVATE(textsw);

    /* Make sure there is an pre_edit text region */
    if (EV_MARK_IS_NULL(&folio->temp_mark)) {
	/*  BIG BUG: Should return an error XIMCB_NotStarted */
	return;
    } else {
	textsw_remove_mark_internal(folio, folio->temp_mark);
	EV_INIT_MARK(folio->temp_mark);
	folio->pecb_first_time = TRUE;
	/* Resume undo and again for commited text */
	folio->state &= ~(TXTSW_NO_UNDO_RECORDING | TXTSW_NO_AGAIN_RECORDING);
    }
}


Xv_private	void
textsw_pre_edit_draw(ic, client_data, callback_data)
    XIC			ic;
    XPointer		client_data;
    XIMPreeditDrawCallbackStruct	*callback_data;
{
    Textsw		textsw = (Textsw)client_data;
    Textsw_folio	folio = TEXTSW_PRIVATE(textsw);
    Es_index		first, last_plus_one;

/*
 *  Null string in callback_data implictly means erase the pre-edit text
 *  for commiting.
 */
    if (EV_MARK_IS_NULL(&folio->temp_mark)) {
	/*  BIG BUG: Should return an error XIMCB_NotStarted */
	return;
    }

    if (folio->pecb_first_time == TRUE) {
	(void)ev_get_selection(folio->views, &first,
			       &last_plus_one, EV_SEL_PRIMARY);
	if (first != last_plus_one) {	/* a primary selection exists */
	    if (callback_data->text->length > 0) {
		int	ro_point;

		ro_point = textsw_read_only_boundary_is_at(folio);
		if (!ro_point || first > ro_point)
		    textsw_erase(textsw, first, last_plus_one); 
		else
		    textsw_set_selection(textsw, ES_INFINITY, ES_INFINITY,
					 EV_SEL_PRIMARY);
	    }
	    else
		textsw_set_selection(textsw, ES_INFINITY, ES_INFINITY,
				     EV_SEL_PRIMARY);
	}
	folio->pecb_first_time = FALSE;
    }

    first = textsw_find_mark_internal(folio, folio->temp_mark) +
	    callback_data->chg_first;
    last_plus_one = first + callback_data->chg_length;

    if (callback_data->chg_first == 0)
	ev_remove_all_op_bdry(folio->views, first, last_plus_one,
			EV_SEL_PRIMARY | EV_SEL_SECONDARY, EV_BDRY_TYPE_ONLY);

    if (callback_data->text->length > 0) {
	if ((callback_data->text->string.wide_char) ||
	    (callback_data->text->string.multi_byte)) {

	    if (callback_data->text->encoding_is_wchar)
		textsw_replace(textsw, first, last_plus_one,
			       callback_data->text->string.wide_char,
			       callback_data->text->length);
	    else
		textsw_replace_bytes(textsw, first, last_plus_one,
				     callback_data->text->string.multi_byte,
				     callback_data->text->length);
        }
	textsw_update_region(textsw, callback_data, FALSE);
     } else {
	/* Erase the current pre-edit text */
	textsw_erase(textsw, first, last_plus_one);
    }
}


textsw_set_pre_edit_region(textsw, first, last_plus_one, type)
    Textsw        	    textsw;
    Textsw_mark 	    first, last_plus_one;
    unsigned        	    type;
{
    Textsw_folio	folio = TEXTSW_PRIVATE(textsw);
    ev_set_pre_edit_region(folio->views, first, last_plus_one, type);
    ev_display_range(folio->views, first, last_plus_one);
}


textsw_clear_pre_edit_region(textsw, first, last_plus_one, type)
    Textsw        	    textsw;
    register Es_index 	    first, last_plus_one;
    unsigned        	    type;
{
    Textsw_folio	folio = TEXTSW_PRIVATE(textsw);
    ev_clear_pre_edit_region(folio->views, first, last_plus_one, type);
    ev_display_range(folio->views, first, last_plus_one);
}


extern void
textsw_update_region(textsw, pre_edit_data, reset_region)
    Textsw        	    textsw;
    XIMPreeditDrawCallbackStruct	*pre_edit_data;
    int			    reset_region;
{
    Textsw_folio	folio = TEXTSW_PRIVATE(textsw);
    int			i;
    register unsigned	type;

    if (!EV_MARK_IS_NULL(&folio->temp_mark)) {
	Textsw_index 	    first, last_plus_one, region_start;

	first = region_start = textsw_find_mark_internal(folio,
				folio->temp_mark) + pre_edit_data->chg_first;
	if (reset_region) {
	    ev_remove_all_op_bdry(folio->views, first,
				  first + pre_edit_data->chg_length,
				  EV_SEL_PRIMARY | EV_SEL_SECONDARY,
				  EV_BDRY_TYPE_ONLY);
	}
	for (i = 0; i < pre_edit_data->text->length; i++) {
	    if ((i == (pre_edit_data->text->length - 1)) ||
		(pre_edit_data->text->feedback[i] != pre_edit_data->text->feedback[i+1])) {
		last_plus_one = region_start + i + 1;
		type = 0;
		if (pre_edit_data->text->feedback[i] == XIMReverse)
		    type |= EV_SEL_PRIMARY;
		if (pre_edit_data->text->feedback[i] == XIMUnderline)
		    type |= EV_SEL_SECONDARY;
		ev_set_pre_edit_region(folio->views,
				       first, last_plus_one, type);
		first = last_plus_one;
	    }
	}
	ev_display_range(folio->views, region_start, last_plus_one);
    }
}
