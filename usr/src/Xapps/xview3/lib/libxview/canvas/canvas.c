#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)canvas.c 20.35 91/09/14";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

#include <xview_private/cnvs_impl.h>
#include <xview_private/win_keymap.h>
#include <xview_private/draw_impl.h>

#ifdef	OW_I18N
#include <xview/font.h>
#include <xview/frame.h>
#include <xview/xv_i18n.h> 

extern void canvas_text_start(), canvas_text_draw(), canvas_text_done();
extern void     status_start(), canvas_status_draw(), status_done();
extern Window   lookup_choices_start();
extern void     lookup_choices_draw(),lookup_choices_done();
extern int      lookup_choices_process();
extern void     aux_start(), aux_draw(), aux_done();

Xv_opaque canvas_create_mle();

Attr_attribute	frame_iw_refcnt;
Attr_attribute  input_window_frame;
Attr_attribute  ml_panel_item_public;
#endif /*OW_I18N*/

Attr_attribute  canvas_context_key;
Attr_attribute  canvas_view_context_key;

/* ARGSUSED */
int
canvas_init(parent, canvas_public, avlist)
    Xv_Window       parent;
    Canvas          canvas_public;
    Attr_attribute  avlist[];
{
    Xv_canvas          *canvas_object = (Xv_canvas *) canvas_public;
    Canvas_info        *canvas;
    Xv_Drawable_info   *info;

#ifdef OW_I18N
    Attr_avlist		attrs;
    Frame	    	frame_public;
    Xv_pkg              *frame_type;
    Xv_pkg	   	*result;
    Xv_opaque           use_im;
    XIM                 im;
    Xv_object           serverobj;
#endif /*OW_I18N*/

    DRAWABLE_INFO_MACRO(canvas_public, info);

    if (canvas_context_key == (Attr_attribute) 0) {
	canvas_context_key = xv_unique_key();
#ifdef OW_I18N
	frame_iw_refcnt = xv_unique_key();
	input_window_frame = xv_unique_key();
	ml_panel_item_public = xv_unique_key();
#endif /*OW_I18N*/
    }
    canvas = xv_alloc(Canvas_info);


    /* link to object */
    canvas_object->private_data = (Xv_opaque) canvas;
    canvas->public_self = canvas_public;

    status_set(canvas, fixed_image);
    status_set(canvas, auto_expand);
    status_set(canvas, auto_shrink);
    status_set(canvas, retained);

    /*
     * 1. Make all the paint windows inherit the WIN_DYNAMIC_VISUAL attribute.
     * 2. The Canvas is, by default, a First-Class (primary) focus client.
     */
    xv_set(canvas_public,
	   WIN_INHERIT_COLORS, TRUE,
	   XV_FOCUS_RANK, XV_FOCUS_PRIMARY,
	   0);

 
#ifdef OW_I18N

    canvas->pe_cache =
	    (XIMPreeditDrawCallbackStruct *)
		xv_alloc(XIMPreeditDrawCallbackStruct);
    canvas->pe_cache->text = (XIMText *)xv_alloc(XIMText);

    frame_public = (Frame)xv_get(canvas_public, WIN_FRAME);
    frame_type = (Xv_pkg*)xv_get(frame_public, XV_TYPE);

    if (!strcmp(frame_type->name, "Frame_cmd")) {
	frame_public = (Frame)xv_get(frame_public, XV_OWNER);
	frame_type = (Xv_pkg *)xv_get(frame_public, XV_TYPE);
    }   

    if (!strcmp(frame_type->name, "Frame_base")) {
	Xv_opaque   input_window;
	Frame       input_frame;
	XPointer luc_clientdata = (XPointer)malloc(sizeof(int *));
	XPointer aux_clientdata = (XPointer)malloc(sizeof(int *));

       input_window =                                              
		(Xv_opaque)xv_get(frame_public, FRAME_INPUT_WINDOW);

	if (input_window == NULL) {                                  
	    input_frame = (Frame)canvas_create_mle(frame_public, NULL);
	} else {
	    input_frame = (Frame)xv_get(input_window, XV_KEY_DATA,
					    input_window_frame);
	}
	xv_set(canvas_public,
	    WIN_IM_PREEDIT_START, canvas_text_start, canvas_public,
	    WIN_IM_PREEDIT_DRAW, canvas_text_draw, canvas_public,
	    WIN_IM_PREEDIT_DONE, canvas_text_done, canvas_public,
	    WIN_IM_STATUS_START, status_start, frame_public,
	    WIN_IM_STATUS_DRAW, canvas_status_draw, input_frame,
	    WIN_IM_STATUS_DONE, status_done, frame_public,
	    WIN_IM_LUC_START, lookup_choices_start, luc_clientdata,
	    WIN_IM_LUC_DRAW, lookup_choices_draw, luc_clientdata,
	    WIN_IM_LUC_DONE, lookup_choices_done, luc_clientdata,
	    WIN_IM_LUC_PROCESS, lookup_choices_process, luc_clientdata,
	    0);
    }

#endif /*OW_I18N*/

    return XV_OK;
}


int
canvas_destroy(canvas_public, stat)
    Canvas          canvas_public;
    Destroy_status  stat;
{
    Canvas_info    *canvas = CANVAS_PRIVATE(canvas_public);

#ifdef OW_I18N
    int		    iw_refcnt = 0;
#endif /*OW_I18N*/


    if (stat == DESTROY_CLEANUP) {
#ifdef OW_I18N
	Frame	    	frame_public = 
			    (Frame)xv_get(canvas_public, WIN_FRAME);

	if (canvas->ic) {
	    iw_refcnt = (int) xv_get(frame_public, XV_KEY_DATA,
					frame_iw_refcnt);
	    /* Destroy IC here */
	    XDestroyIC(canvas->ic);
		
	    /* Destroy Input Window here */
	    if (!--iw_refcnt) {
/*
		Xv_opaque	input_window =
		    (Xv_opaque)xv_get(frame_public, FRAME_INPUT_WINDOW);
		xv_destroy(input_window);
		xv_set(frame_public, FRAME_INPUT_WINDOW, NULL, 0);
*/
	    }
	    xv_set(frame_public, XV_KEY_DATA, frame_iw_refcnt, iw_refcnt, 0);
	}
	if(canvas->pe_cache->text->feedback)
	    free(canvas->pe_cache->text->feedback);
	if(canvas->pe_cache->text->string.wide_char) {
	    free(canvas->pe_cache->text->string.wide_char);
	} else if(canvas->pe_cache->text->string.multi_byte) {
	    free(canvas->pe_cache->text->string.multi_byte);
	}
#endif /*OW_I18N*/
	free((char *) canvas);
    }
    return XV_OK;
}


#ifdef	OW_I18N
/*
 * canvas_create_mle(frame, font) calls ml_panel_create_canvas_mle()
 * to create a panel sw and a panel text item for the canvas
 * intermediate text. (Xv_font *)font is the font used to display
 * the intermediate text.
 * We need to access panel->pa_tscreen to let repaint happen but
 * anel_impl.h conflicts with canvas_impl.h, thus, we do the real
 * work in ml_panel_create_canvas_mle.
 */
Xv_opaque
canvas_create_mle(frame, font)
	Frame		frame;
	Xv_font		font;
{
	Xv_font canvas_interm_font;
	Frame	input_frame;

	if (font == (Xv_font)NULL) 
	    canvas_interm_font= (Xv_Font)xv_get(frame, XV_FONT);
	else
	    canvas_interm_font = font;

	input_frame = ml_panel_create_canvas_mle(frame, canvas_interm_font);

	return (input_frame);
}

#endif /*OW_I18N*/
