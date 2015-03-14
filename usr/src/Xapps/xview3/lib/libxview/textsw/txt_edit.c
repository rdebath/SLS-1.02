#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)txt_edit.c 20.38 91/09/14";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

/*
 * Programming interface to editing facilities of text subwindows.
 */

#include <xview_private/i18n_impl.h>
#include <xview_private/txt_18impl.h>
#include <xview_private/primal.h>
#include <xview_private/txt_impl.h>
#include <xview_private/ev_impl.h>
#include <xview/pkg.h>
#include <xview/attrol.h>
#include <xview/notice.h>
#include <xview/frame.h>
#include <xview/server.h>

#define THRESHOLD  20
#define UPDATE_SCROLLBAR(_delta, _old_length)\
	((THRESHOLD * _delta) >= _old_length)

Xv_private_data CHAR *shell_prompt;

extern void     textsw_notify_replaced();
pkg_private Seln_rank textsw_acquire_seln();
extern Textsw_index textsw_replace();

pkg_private     Es_handle
textsw_esh_for_span(view, first, last_plus_one, to_recycle)
    Textsw_view_handle view;
    Es_index        first, last_plus_one;
    Es_handle       to_recycle;
{
    Es_handle       esh = FOLIO_FOR_VIEW(view)->views->esh;

    return ((Es_handle)
	    es_get5(esh, ES_HANDLE_FOR_SPAN, first, last_plus_one,
		    to_recycle, 0, 0));
}

pkg_private int
textsw_adjust_delete_span(folio, first, last_plus_one)
    register Textsw_folio folio;
    register Es_index *first, *last_plus_one;
/*
 * Returns: TXTSW_PE_EMPTY_INTERVAL iff *first < *last_plus_one, else
 * TEXTSW_PE_READ_ONLY iff NOTHING should be deleted, else TXTSW_PE_ADJUSTED
 * iff *first adjusted to reflect the constraint imposed by
 * folio->read_only_boundary, else 0.
 */
{
    if (*first >= *last_plus_one)
	return (TXTSW_PE_EMPTY_INTERVAL);
    if TXTSW_IS_READ_ONLY
	(folio)
	    return (TEXTSW_PE_READ_ONLY);
    if (!EV_MARK_IS_NULL(&folio->read_only_boundary)) {
	register Es_index mark_at;
	mark_at = textsw_find_mark_internal(folio,
					    folio->read_only_boundary);
	if AN_ERROR
	    (mark_at == ES_INFINITY)
		return (0);
	if (*last_plus_one <= mark_at)
	    return (TEXTSW_PE_READ_ONLY);
	if (*first < mark_at) {
	    *first = mark_at;
	    return (TXTSW_PE_ADJUSTED);
	}
    }
    return (0);
}

pkg_private int
textsw_esh_failed_msg(view, preamble)
    Textsw_view_handle view;
    char           *preamble;
{
    Textsw_folio    folio = FOLIO_FOR_VIEW(view);
    Es_status       status;
    Xv_Notice	text_notice;
    Frame	frame;

    status = (Es_status)
	es_get(folio->views->esh, ES_STATUS);
    switch (status) {
      case ES_SHORT_WRITE:
	if (TEXTSW_OUT_OF_MEMORY(folio, status)) {
	    frame = FRAME_FROM_FOLIO_OR_VIEW(view);
	    text_notice = xv_get(frame, XV_KEY_DATA, text_notice_key, NULL);

	    if (!text_notice)  {
    	        text_notice = xv_create(frame, NOTICE,
			  NOTICE_LOCK_SCREEN, FALSE,
			  NOTICE_BLOCK_THREAD, TRUE,
		          NOTICE_BUTTON_YES, 
				XV_MSG("Continue"),
		          NOTICE_MESSAGE_STRINGS,
			  (strlen(preamble)) ? preamble : 
				XV_MSG("Action failed -"),
				XV_MSG("The memory buffer is full.\n\
If this is an isolated case, you can circumvent\n\
this condition by undoing the operation you just\n\
performed, storing the contents of the subwindow\n\
to a file using the text menu, and then redoing\n\
the operation.  Or, you can enlarge the size of\n\
this buffer by changing the appropriate value in\n\
the .Xdefaults file (Text.MaxDocumentSize)."),
		              0,
			  XV_SHOW, TRUE,
		          0);

	        xv_set(frame, 
		    XV_KEY_DATA, text_notice_key, text_notice, 
		    NULL);

            }
	    else  {
    	        xv_set(text_notice, 
			NOTICE_LOCK_SCREEN, FALSE,
			NOTICE_BLOCK_THREAD, TRUE,
		        NOTICE_BUTTON_YES, 
				XV_MSG("Continue"),
		        NOTICE_MESSAGE_STRINGS,
			  (strlen(preamble)) ? preamble : 
				XV_MSG("Action failed -"),
				XV_MSG("The memory buffer is full.\n\
If this is an isolated case, you can circumvent\n\
this condition by undoing the operation you just\n\
performed, storing the contents of the subwindow\n\
to a file using the text menu, and then redoing\n\
the operation.  Or, you can enlarge the size of\n\
this buffer by changing the appropriate value in\n\
the .Xdefaults file (Text.MaxDocumentSize)."),
		        0,
			XV_SHOW, TRUE, 
			0);
	    }
	    break;
	}
	/* else fall through */
      case ES_CHECK_ERRNO:
      case ES_CHECK_FERROR:
      case ES_FLUSH_FAILED:
      case ES_FSYNC_FAILED:
      case ES_SEEK_FAILED:{
	    frame = FRAME_FROM_FOLIO_OR_VIEW(view);
	    text_notice = xv_get(frame, XV_KEY_DATA, text_notice_key, NULL);

	    if (!text_notice)  {
    	        text_notice = xv_create(frame, NOTICE,
			  NOTICE_LOCK_SCREEN, FALSE,
			  NOTICE_BLOCK_THREAD, TRUE,
		          NOTICE_BUTTON_YES, XV_MSG("Continue"),
		          NOTICE_MESSAGE_STRINGS,
			  (strlen(preamble)) ? preamble : 
				XV_MSG("Action failed -"),
			XV_MSG("A problem with the file system has been detected.\n\
File system is probably full."),
		              0,
			  XV_SHOW, TRUE,
		          0);

	        xv_set(frame, 
		    XV_KEY_DATA, text_notice_key, text_notice, 
		    NULL);

            }
	    else  {
    	        xv_set(text_notice, 
			NOTICE_LOCK_SCREEN, FALSE,
			NOTICE_BLOCK_THREAD, TRUE,
		        NOTICE_BUTTON_YES, XV_MSG("Continue"),
		        NOTICE_MESSAGE_STRINGS,
			  (strlen(preamble)) ? preamble : 
				XV_MSG("Action failed -"),
			XV_MSG("A problem with the file system has been detected.\n\
File system is probably full."),
		        0,
			XV_SHOW, TRUE, 
			0);
	    }
	    break;
	}
      case ES_REPLACE_DIVERTED:
	break;
      default:
	break;
    }
}

pkg_private     Es_index
textsw_delete_span(view, first, last_plus_one, flags)
    Textsw_view_handle view;
    Es_index        first, last_plus_one;
    register unsigned flags;
/*
 * Returns the change in indices resulting from the operation.  Result is: a)
 * usually < 0, b) 0 if span is empty or in a read_only area, c)
 * ES_CANNOT_SET if ev_delete_span fails
 */
{
    register Textsw_folio folio = FOLIO_FOR_VIEW(view);
    Es_index        result;

    result = (flags & TXTSW_DS_ADJUST)
	? textsw_adjust_delete_span(folio, &first, &last_plus_one)
	: (first >= last_plus_one) ? TXTSW_PE_EMPTY_INTERVAL : 0;
    switch (result) {
      case TEXTSW_PE_READ_ONLY:
      case TXTSW_PE_EMPTY_INTERVAL:
	result = 0;
	break;
      case TXTSW_PE_ADJUSTED:
	if (flags & TXTSW_DS_CLEAR_IF_ADJUST(0)) {
	    textsw_set_selection(VIEW_REP_TO_ABS(view),
				 ES_INFINITY, ES_INFINITY,
				 EV_SEL_BASE_TYPE(flags));
	}
	/* Fall through to do delete on remaining span. */
      default:
	if (flags & TXTSW_DS_SHELVE) {
	    folio->trash = textsw_esh_for_span(view, first, last_plus_one,
					       folio->trash);
	    textsw_acquire_seln(folio, SELN_SHELF);
	}
	switch (ev_delete_span(folio->views, first, last_plus_one,
			       &result)) {
	  case 0:
	    if (flags & TXTSW_DS_RECORD) {
		textsw_record_delete(folio);
	    }
	    break;
	  case 3:
	    textsw_esh_failed_msg(view, 
		XV_MSG("Deletion failed - "));
	    /* Fall through */
	  default:
	    result = ES_CANNOT_SET;
	    break;
	}
	break;
    }
    return (result);
}

pkg_private     Es_index
textsw_do_pending_delete(view, type, flags)
    Textsw_view_handle view;
    unsigned        type;
    int             flags;
{
    register Textsw_folio folio = FOLIO_FOR_VIEW(view);
    int             is_pending_delete;
    Es_index        first, last_plus_one, delta, insert;
    int             result = ev_get_selection(folio->views, &first, &last_plus_one, type);

    is_pending_delete = ((type == EV_SEL_PRIMARY) ?
			 (EV_SEL_PD_PRIMARY & result) :
			 (EV_SEL_PD_SECONDARY & result));

    if (first >= last_plus_one)
	return (0);
    textsw_take_down_caret(folio);
    insert = (flags & TFC_INSERT) ? EV_GET_INSERT(folio->views) : first;
    if (is_pending_delete &&
	(first <= insert) && (insert <= last_plus_one)) {
	delta = textsw_delete_span(view, first, last_plus_one,
				   TXTSW_DS_ADJUST | TXTSW_DS_SHELVE |
				   TXTSW_DS_CLEAR_IF_ADJUST(type));
    } else {
	if (flags & TFC_SEL) {
	    textsw_set_selection(VIEW_REP_TO_ABS(view),
				 ES_INFINITY, ES_INFINITY, type);
	}
	delta = 0;
    }
    return (delta);
}

extern          Textsw_index
textsw_delete(abstract, first, last_plus_one)
    Textsw          abstract;
    Es_index        first, last_plus_one;
{
    Textsw_view_handle view = VIEW_ABS_TO_REP(abstract);
    Textsw_folio    folio = FOLIO_FOR_VIEW(view);
    int             result;

    textsw_take_down_caret(folio);
    result = textsw_delete_span(view, first, last_plus_one,
				TXTSW_DS_ADJUST | TXTSW_DS_SHELVE);
    if (result == ES_CANNOT_SET)
	return 0;
    return -result;
}

extern          Textsw_index
textsw_erase(abstract, first, last_plus_one)
    Textsw          abstract;
    Es_index        first, last_plus_one;
/*
 * This routine is identical to textsw_delete EXCEPT it does not affect the
 * contents of the shelf (useful for client implementing ^W/^U or mailtool).
 */
{
    Textsw_view_handle view = VIEW_ABS_TO_REP(abstract);
    Textsw_folio    folio = FOLIO_FOR_VIEW(view);
    int             result;

    textsw_take_down_caret(folio);
    result = textsw_delete_span(view, first, last_plus_one,
				TXTSW_DS_ADJUST);
    if (result == ES_CANNOT_SET)
	return 0;
    return -result;
}

extern void
textsw_insert_makes_visible(textsw)
    Textsw          textsw;
{
    Textsw_view_handle view = VIEW_ABS_TO_REP(textsw);
    register Textsw_folio folio = FOLIO_FOR_VIEW(view);
    Textsw_enum     old_insert_makes_visible =
    folio->insert_makes_visible;
    int             old_state = folio->state;

    folio->insert_makes_visible = TEXTSW_ALWAYS;
    folio->state |= TXTSW_DOING_EVENT;

    TEXTSW_DO_INSERT_MAKES_VISIBLE(view);

    folio->insert_makes_visible = old_insert_makes_visible;
    folio->state = old_state;

}

pkg_private int
textsw_do_edit(view, unit, direction)
    register Textsw_view_handle view;
    unsigned        unit, direction;
{
    extern struct ei_span_result
                    ev_span_for_edit();
    register Textsw_folio folio = FOLIO_FOR_VIEW(view);
    struct ei_span_result span;
    int             delta;

    span = ev_span_for_edit(folio->views, (int) (unit | direction));
    if ((span.flags >> 16) == 0) {

	/* Don't join with next line for ERASE_LINE_END */

	if ((unit == EV_EDIT_LINE) && (direction == 0)) {
	    Es_index        file_length = es_get_length(folio->views->esh);

	    if (span.last_plus_one < file_length)
		span.last_plus_one--;
	}
	delta = textsw_delete_span(view, span.first, span.last_plus_one,
				   TXTSW_DS_ADJUST);
	if (delta == ES_CANNOT_SET) {
	    delta = 0;
	} else {
	    TEXTSW_DO_INSERT_MAKES_VISIBLE(view);
	    textsw_record_edit(folio, unit, direction);
	    delta = -delta;
	}
    } else
	delta = 0;
    return (delta);
}

extern          Textsw_index
textsw_edit(abstract, unit, count, direction)
    Textsw          abstract;
    register unsigned unit, count;
    unsigned        direction;
{
    Textsw_view_handle view = VIEW_ABS_TO_REP(abstract);
    Textsw_folio    folio = FOLIO_FOR_VIEW(view);
    int             result = 0;

    if (direction)
	direction = EV_EDIT_BACK;
    switch (unit) {
      case TEXTSW_UNIT_IS_CHAR:
	unit = EV_EDIT_CHAR;
	break;
      case TEXTSW_UNIT_IS_WORD:
	unit = EV_EDIT_WORD;
	break;
      case TEXTSW_UNIT_IS_LINE:
	unit = EV_EDIT_LINE;
	break;
      default:
	return 0;
    }
    textsw_take_down_caret(folio);
    for (; count; count--) {
	result += textsw_do_edit(view, unit, direction);
    }
    return (result);
}

pkg_private void
textsw_input_before(view, old_insert_pos, old_length)
    Textsw_view_handle view;
    Es_index       *old_insert_pos, *old_length;
{
    Textsw_folio    folio = FOLIO_FOR_VIEW(view);
    register Ev_chain chain = folio->views;
    Ev_chain_pd_handle private = EV_CHAIN_PRIVATE(chain);
    *old_length = es_get_length(chain->esh);
    *old_insert_pos = EV_GET_INSERT(chain);
    if (private->lower_context != EV_NO_CONTEXT) {
	ev_check_insert_visibility(chain);
    }
}

pkg_private int
textsw_input_partial(view, buf, buf_len)
    Textsw_view_handle view;
    CHAR           *buf;
    long int        buf_len;
{
    int             status;

    status = ev_input_partial(FOLIO_FOR_VIEW(view)->views, buf, buf_len);
    if (status) {
	textsw_esh_failed_msg(view, 
		XV_MSG("Insertion failed - "));
	return SELN_FAILED;
    }
    return (status ? SELN_FAILED : SELN_SUCCESS);
}

pkg_private     Es_index
textsw_input_after(view, old_insert_pos, old_length, record)
    Textsw_view_handle view;
    Es_index        old_insert_pos, old_length;
    int             record;
{
    register Textsw_folio folio = FOLIO_FOR_VIEW(view);
    Es_index        delta;

    delta = ev_input_after(folio->views, old_insert_pos, old_length);
    if (delta != ES_CANNOT_SET) {
	TEXTSW_DO_INSERT_MAKES_VISIBLE(view);
	if (record) {
	    Es_handle       pieces;
	    pieces = textsw_esh_for_span(folio->first_view,
			   old_insert_pos, old_insert_pos + delta, ES_NULL);
	    textsw_record_piece_insert(folio, pieces);
	}
	if ((folio->state & TXTSW_EDITED) == 0)
	    textsw_possibly_edited_now_notify(folio);
	if (folio->notify_level & TEXTSW_NOTIFY_EDIT) {
	    textsw_notify_replaced((Textsw_opaque) folio->first_view,
				 old_insert_pos, old_length, old_insert_pos,
				   old_insert_pos, delta);
	}
	(void) textsw_checkpoint(folio);
    }
    return (delta);
}

pkg_private     Es_index
textsw_do_input(view, buf, buf_len, flag)
    Textsw_view_handle view;
    CHAR           *buf;
    long int        buf_len;
    unsigned        flag;
{
    register Textsw_folio folio = FOLIO_FOR_VIEW(view);
    register Ev_chain chain = folio->views;
    int             record;
    Es_index        delta, old_insert_pos, old_length;

    /* possibly use escape sequences ? */
    if (xv_get((Xv_opaque)XV_SERVER_FROM_WINDOW(VIEW_REP_TO_ABS(view)), SERVER_JOURNALLING))
	if (memchr(buf, shell_prompt[0], buf_len))
	    xv_set((Xv_opaque)XV_SERVER_FROM_WINDOW(VIEW_REP_TO_ABS(view)), SERVER_JOURNAL_SYNC_EVENT, 1, 0);
    textsw_input_before(view, &old_insert_pos, &old_length);
    if (textsw_input_partial(view, buf, buf_len) == ES_CANNOT_SET)
	return (0);
    record = (TXTSW_DO_AGAIN(folio) &&
	      ((folio->func_state & TXTSW_FUNC_AGAIN) == 0));
    delta = textsw_input_after(view, old_insert_pos, old_length,
			       record && (buf_len > 100));
    if (delta == ES_CANNOT_SET)
	return (0);

    if ((int) ev_get(view->e_view, EV_CHAIN_DELAY_UPDATE) == 0) {
	ev_update_chain_display(chain);

	if (flag & TXTSW_UPDATE_SCROLLBAR)
	    textsw_update_scrollbars(folio, TEXTSW_VIEW_NULL);
	else if ((flag & TXTSW_UPDATE_SCROLLBAR_IF_NEEDED) &&
		 UPDATE_SCROLLBAR(delta, old_length))
	    textsw_update_scrollbars(folio, TEXTSW_VIEW_NULL);
    }
    if (record && (buf_len <= 100))
	textsw_record_input(folio, buf, buf_len);
    return (delta);
}

extern          Textsw_index
textsw_insert(abstract, buf, buf_len)
    Textsw          abstract;
    char           *buf;
    long int        buf_len;
{
    Textsw_view_handle view = VIEW_ABS_TO_REP(abstract);
    register Textsw_folio folio = FOLIO_FOR_VIEW(view);
    Es_index        result;
#ifdef OW_I18N
    CHAR           *buf_wcs = MALLOC(buf_len + 1);
    int             buf_wcs_len;

    buf_wcs_len = mbstowcs(buf_wcs, buf, buf_len);
    if ((buf_wcs_len > -1) && (buf_wcs_len <= buf_len))
	buf_wcs[buf_wcs_len] = NULL;
#endif

    textsw_take_down_caret(folio);
#ifdef OW_I18N
    result = textsw_do_input(view, buf_wcs, buf_wcs_len,
			     TXTSW_UPDATE_SCROLLBAR_IF_NEEDED);
    free((char *)buf_wcs);
#else
    result = textsw_do_input(view, buf, buf_len,
			     TXTSW_UPDATE_SCROLLBAR_IF_NEEDED);
#endif
    return (result);
}

#ifdef OW_I18N
extern          Textsw_index
textsw_insert_wcs(abstract, buf, buf_len)
    Textsw          abstract;
    CHAR           *buf;
    long int        buf_len;
{
    Textsw_view_handle view = VIEW_ABS_TO_REP(abstract);
    register Textsw_folio folio = FOLIO_FOR_VIEW(view);
    Es_index        result;

    textsw_take_down_caret(folio);
    result = textsw_do_input(view, buf, buf_len,
			     TXTSW_UPDATE_SCROLLBAR_IF_NEEDED);    
    return (result);
}
#endif OW_I18N

extern          Textsw_index
textsw_replace_bytes(abstract, first, last_plus_one, buf, buf_len)
    Textsw          abstract;
    Es_index        first, last_plus_one;
    char           *buf;
    long int        buf_len;
/*
 * This routine is a placeholder that can be documented without casting the
 * calling sequence to textsw_replace (the preferred name) in concrete.
 */
{
#ifdef OW_I18N
    CHAR	   *buf_wcs = MALLOC(buf_len + 1);
    int		    buf_wcs_len;
    int		    result;
    
    buf_wcs_len = mbstowcs(buf_wcs, buf, buf_len);
    if ((buf_wcs_len > -1) && (buf_wcs_len <= buf_len))
        buf_wcs[buf_wcs_len] = NULL;    
    if (buf_wcs_len > 0)
        result = textsw_replace(abstract, first, last_plus_one, buf_wcs, buf_wcs_len);
    free ((char *)buf_wcs);
    return (result);
#else
    return (textsw_replace(abstract, first, last_plus_one, buf, buf_len));
#endif OW_I18N
}

#ifdef OW_I18N
extern          Textsw_index
textsw_replace_wcs(abstract, first, last_plus_one, buf, buf_len)
    Textsw          abstract;
    Es_index        first, last_plus_one;
    CHAR           *buf;
    long int        buf_len;
/*
 * This routine is a placeholder that can be documented without casting the
 * calling sequence to textsw_replace (the preferred name) in concrete.
 */
{
    Textsw_view_handle	view = VIEW_ABS_TO_REP(abstract);

    if (FOLIO_FOR_VIEW(view)->temp_mark)
	textsw_implicit_commit(view);
    return (textsw_replace(abstract, first, last_plus_one, buf, buf_len));
}
#endif OW_I18N

extern          Textsw_index
textsw_replace(abstract, first, last_plus_one, buf, buf_len)
    Textsw          abstract;
    Es_index        first, last_plus_one;
    CHAR           *buf;
    long int        buf_len;
{
    extern void     textsw_remove_mark_internal();
    pkg_private     Ev_mark_object
                    textsw_add_mark_internal();
    Ev_mark_object  saved_insert_mark;
    Es_index        saved_insert, temp;
    Textsw_view_handle view = VIEW_ABS_TO_REP(abstract);
    register Textsw_folio folio = FOLIO_FOR_VIEW(view);
    register Ev_chain chain = folio->views;
    Es_index        result, insert_result;
    int             lower_context;

    insert_result = 0;
    textsw_take_down_caret(folio);
    /* BUG ALERT: change this to avoid the double paint. */
    if (first < last_plus_one) {
	ev_set(view->e_view, EV_CHAIN_DELAY_UPDATE, TRUE, 0);
	result = textsw_delete_span(view, first, last_plus_one,
				    TXTSW_DS_ADJUST);
	ev_set(view->e_view, EV_CHAIN_DELAY_UPDATE, FALSE, 0);
	if (result == ES_CANNOT_SET) {
	    if (ES_REPLACE_DIVERTED == (Es_status)
		es_get(folio->views->esh, ES_STATUS)) {
		result = 0;
	    }
	}
    } else {
	result = 0;
    }
  
    /* changing none to all should perform correctly */
    if(result == ES_CANNOT_SET && 
       first == 0 && 
       last_plus_one == TEXTSW_INFINITY)
      result = 1;

    if (result == ES_CANNOT_SET) {
	result = 0;
    } else {
	ev_check_insert_visibility(chain);
	lower_context =
	    (int) ev_get(view->e_view, EV_CHAIN_LOWER_CONTEXT);
	ev_set(view->e_view,
	       EV_CHAIN_LOWER_CONTEXT, EV_NO_CONTEXT, 0);

	saved_insert = EV_GET_INSERT(chain);
	saved_insert_mark =
	    textsw_add_mark_internal(folio, saved_insert,
				     TEXTSW_MARK_MOVE_AT_INSERT);
	EV_SET_INSERT(chain, first, temp);
	insert_result += textsw_do_input(view, buf, buf_len,
					 TXTSW_DONT_UPDATE_SCROLLBAR);
	result += insert_result;
	saved_insert = textsw_find_mark_internal(folio, saved_insert_mark);
	if AN_ERROR
	    (saved_insert == ES_INFINITY) {
	} else
	    EV_SET_INSERT(chain, saved_insert, temp);
	textsw_remove_mark_internal(folio, saved_insert_mark);

	ev_set(view->e_view,
	       EV_CHAIN_LOWER_CONTEXT, lower_context, 0);
	ev_scroll_if_old_insert_visible(chain,
					saved_insert, insert_result);
	textsw_update_scrollbars(folio, TEXTSW_VIEW_NULL);
    }
    return (result);
}


