#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)txt_disp.c 20.22 91/09/14";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

/*
 * Initialization and finalization of text subwindows.
 */

#include <xview_private/i18n_impl.h>
#include <xview_private/primal.h>
#include <xview_private/txt_impl.h>
#include <xview_private/ev_impl.h>
#include <xview/win_notify.h>
#include <xview/pixwin.h>

/* Used as hack to communicate between textsw_display and textsw_display_view
 * to establish who should manage the caret. */
static	int textsw_display_parent;

extern void
textsw_display(abstract)
    Textsw          abstract;
{
    register Textsw_view_handle view = VIEW_ABS_TO_REP(abstract);
    Textsw_folio    textsw = FOLIO_FOR_VIEW(view);

    textsw_hide_caret(textsw);
    textsw_display_parent = 1;
    textsw->state |= TXTSW_DISPLAYED;
    FORALL_TEXT_VIEWS(textsw, view) {
	textsw_display_view(VIEW_REP_TO_ABS(view), &view->rect);
    }
    textsw_show_caret(textsw);
    textsw_display_parent = 0;
}

extern void
textsw_display_view(abstract, rect)
    Textsw          abstract;
    register Rect  *rect;
{
    register Textsw_view_handle view = VIEW_ABS_TO_REP(abstract);

    if (!textsw_display_parent)
	textsw_hide_caret(FOLIO_FOR_VIEW(view));
    textsw_display_view_margins(view, rect);
    if (rect == 0) {
	rect = &view->rect;
    } else if (!rect_intersectsrect(rect, &view->rect)) {
	return;
    }
    ev_display_in_rect(view->e_view, rect);
    textsw_update_scrollbars(FOLIO_FOR_VIEW(view), view);
    if (!textsw_display_parent)
	textsw_show_caret(FOLIO_FOR_VIEW(view));

}

pkg_private void
textsw_display_view_margins(view, rect)
    register Textsw_view_handle view;
    struct rect    *rect;
{
    struct rect     margin;

    margin = view->e_view->rect;
    margin.r_left -= (
	       margin.r_width = (int) ev_get(view->e_view, EV_LEFT_MARGIN));
    /*if (rect == 0) {   || rect_intersectsrect(rect, &margin)) {*/
    /* Always write so will clear up cursor droppings */
    (void) pw_writebackground(PIXWIN_FOR_VIEW(view),
				  margin.r_left, margin.r_top,
				  margin.r_width, margin.r_height,
				  PIX_SRC);
    margin.r_left = rect_right(&view->e_view->rect) + 1;
    margin.r_width = (int) ev_get(view->e_view, EV_RIGHT_MARGIN);
    if (rect == 0 || rect_intersectsrect(rect, &margin)) {
	(void) pw_writebackground(PIXWIN_FOR_VIEW(view),
				  margin.r_left, margin.r_top,
				  margin.r_width, margin.r_height,
				  PIX_SRC);
    }
}

pkg_private void
textsw_repaint(view)
    register Textsw_view_handle view;
{
    if (!(view->state & TXTSW_VIEW_DISPLAYED)) {
	view->state |= TXTSW_VIEW_DISPLAYED;
	view->state |= TXTSW_UPDATE_SCROLLBAR;
    }
    FOLIO_FOR_VIEW(view)->state |= TXTSW_DISPLAYED;

    /* setup correct clipping rectangles -- jcb 5/5/89 */
    /* (note that this code should be in term instead of tty) */
    tty_setup_clip_rectangles(PIXWIN_FOR_VIEW(view));

    (EV_PRIVATE(view->e_view))->state |= EV_VS_SET_CLIPPING;
    textsw_display_view(VIEW_REP_TO_ABS(view), &view->rect);

    tty_clear_clip_rectangles(PIXWIN_FOR_VIEW(view));
}

pkg_private void
textsw_resize(view)
    register Textsw_view_handle view;
{


    (void) win_getsize(WIN_FD_FOR_VIEW(view), &view->rect);

    /* Cannot trust the x and y from openwin */
    view->rect.r_left = view->e_view->rect.r_left;
    view->rect.r_width -= view->rect.r_left;
    view->rect.r_top = view->e_view->rect.r_top;
    view->rect.r_height -= view->rect.r_top;
    (void) ev_set(view->e_view, EV_RECT, &view->rect, 0);
    xv_set(view->drop_site, DROP_SITE_DELETE_REGION,    NULL,
			    DROP_SITE_REGION,           view->rect,
			    NULL);
}

Xv_private void
textsw_do_resize(abstract)
    Textsw          abstract;
/* This routine only exists for the cmdsw. */
{
    register Textsw_view_handle view = VIEW_ABS_TO_REP(abstract);

    textsw_resize(view);
}

extern          Textsw_expand_status
textsw_expand(abstract,
	      start, stop_plus_one, out_buf, out_buf_len, total_chars)
    Textsw          abstract;
    Es_index        start;	/* Entity to start expanding at */
    Es_index        stop_plus_one;	/* 1st ent not expanded */
    CHAR           *out_buf;
    int             out_buf_len;
    int            *total_chars;
/*
 * Expand the contents of the textsw from first to stop_plus_one into the set
 * of characters used to paint them, placing the expanded text into out_buf,
 * returning the number of character placed into out_buf in total_chars, and
 * returning status.
 */
{
    register Textsw_view_handle view = VIEW_ABS_TO_REP(abstract);
    Ev_expand_status status;

    status = ev_expand(view->e_view,
		   start, stop_plus_one, out_buf, out_buf_len, total_chars);
    switch (status) {
      case EV_EXPAND_OKAY:
	return (TEXTSW_EXPAND_OK);
      case EV_EXPAND_FULL_BUF:
	return (TEXTSW_EXPAND_FULL_BUF);
      case EV_EXPAND_OTHER_ERROR:
      default:
	return (TEXTSW_EXPAND_OTHER_ERROR);
    }
}
