/*      @(#)cnvs_impl.h 20.35 91/09/14 SMI      */
/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

#include <xview/pkg.h>
#include <xview/attrol.h>
#include <xview/canvas.h>
#include <xview_private/portable.h>

#ifdef OW_I18N
#include <X11/Xresource.h>
#include <xview/xv_i18n.h>
#include <xview_private/i18n_impl.h>
#endif /*OW_I18N*/

#define	CANVAS_PRIVATE(c)	XV_PRIVATE(Canvas_info, Xv_canvas, c)
#define	CANVAS_PUBLIC(canvas)	XV_PUBLIC(canvas)

#define	CANVAS_VIEW_PRIVATE(c)	XV_PRIVATE(Canvas_view_info, Xv_canvas_view, c)
#define	CANVAS_VIEW_PUBLIC(canvas_view)	XV_PUBLIC(canvas_view)


#define	BIT_FIELD(field)	int field : 1


typedef void	(*Function)();

typedef struct {
    Canvas	public_self;	/* back pointer to public self */
    int		margin;		/* view pixwin margin */
    int		width, height;
    int		min_paint_width;
    int		min_paint_height;
    Function 	repaint_proc;
    Function 	resize_proc;
    Attr_avlist	paint_avlist; 	/* cached pw avlist on create */
    Attr_avlist	paint_end_avlist;

    struct {
	BIT_FIELD(auto_expand);		/* auto expand canvas with window */
	BIT_FIELD(auto_shrink);		/* auto shrink canvas with window */
	BIT_FIELD(fixed_image);		/* canvas is a fixed size image */
	BIT_FIELD(retained);		/* canvas is a retained window */
	BIT_FIELD(created);		/* first paint window is created */
	BIT_FIELD(x_canvas);		/* treat canvas as an X drawing surface */
	BIT_FIELD(no_clipping);		/* ignore clip rects on repaint */
	BIT_FIELD(cms_repaint);         /* generate repaint on cms changes */
    } status_bits;

#ifdef OW_I18N

    XIC		     ic;
    XIMPreeditDrawCallbackStruct *pe_cache;

#endif /*OW_I18N*/
} Canvas_info;

typedef struct {
    Canvas_view	public_self;	/* back pointer to public self */
    Canvas_info		*private_canvas;
    Xv_Window		paint_window;
} Canvas_view_info;


#define	status(canvas, field)		((canvas)->status_bits.field)
#define	status_set(canvas, field)	status(canvas, field) = TRUE
#define	status_reset(canvas, field)	status(canvas, field) = FALSE

extern	Attr_attribute	canvas_context_key;
extern  Attr_attribute  canvas_view_context_key;

#ifdef OW_I18N
extern  Attr_attribute  frame_iw_refcnt;
extern  Attr_attribute  input_window_frame;
extern  Attr_attribute  ml_panel_item_public;
#endif /*OW_I18N*/

/* from canvas_input.c */
extern Notify_value	canvas_view_event();
extern Notify_value	canvas_paint_event();

/* from canvas_set.c */
extern Xv_opaque	canvas_set_avlist();

/* from canvas_get.c */
extern Xv_opaque	canvas_get_attr();

/* from canvas_resize.c */
extern void		canvas_resize_paint_window();

/* from canvas_scroll.c */
Pkg_private void	canvas_scroll();
Pkg_private void	canvas_set_scrollbar_object_length();
/* from canvas.c */
Pkg_private int      canvas_init();
Pkg_private int      canvas_destroy();
/*from canvas_view.c */
Pkg_private int      canvas_view_init();
Pkg_private Xv_opaque        canvas_view_get();
Pkg_private Xv_opaque        canvas_paint_get();
Pkg_private Xv_opaque        canvas_paint_set();
Pkg_private int      canvas_view_destroy();

