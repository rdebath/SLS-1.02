#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)p_paint.c 20.26 89/11/30 Copyr 1984 Sun Micro";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

#include <xview_private/panel_impl.h>
#include <xview_private/draw_impl.h>

static void     panel_repaint_background();
static void	panel_paint_item();
Pkg_private void panel_redisplay_item();
Xv_private void screen_adjust_gc_color();

Pkg_private void
panel_redisplay(panel_public, pw, repaint_area)
    Panel           panel_public;
    Xv_Window       pw;
    Rectlist       *repaint_area;
{
    Xv_Screen      screen;
    GC             *gc_list;

    register Panel_info *panel = PANEL_PRIVATE(panel_public);
    register Item_info *ip;
    Xv_Drawable_info *info;

    if (panel->repaint_proc != NULL) {
	panel->repaint_proc(panel_public, pw, repaint_area);
    }
    /* If this is a PANEL with out Panel Menu Items, then draw a line
     * at the top of the panel.
     */
    if ((int) xv_get(panel_public, XV_Y) == 0 &&
	xv_get(panel_public, WIN_VERTICAL_SCROLLBAR) == NULL &&
	xv_get(panel_public, WIN_HORIZONTAL_SCROLLBAR) == NULL &&
	!(panel->items && is_menu_item(panel->items))) {
        DRAWABLE_INFO_MACRO(pw, info); 
	screen = xv_screen(info);
	gc_list = (GC *)xv_get(screen, SCREEN_OLGC_LIST, pw);
        screen_adjust_gc_color(pw, SCREEN_SET_GC); 
        XDrawLine(xv_display(info), xv_xid(info),
		  gc_list[SCREEN_SET_GC], 
                  0, 0, xv_get(panel_public, XV_WIDTH) - 1, 0);
    }

    /*
     * See which items intersect the rect lists rects and paint them
     */
    panel->status.painted = TRUE;  /* tell items it's OK to paint */
    for (ip = panel->items; ip; ip = ip->next) {
	if (!hidden(ip)) {
	    /* only paint item if intersects with repaint area */
	    if (rl_rectintersects(&ip->rect, repaint_area)) {
		panel_paint_item(ip);
	    }
	}
    }

}


Pkg_private void
panel_display(panel, flag)
    register Panel_info *panel;
    Panel_setting   flag;
{
    register Item_info *ip;
    Rect            *view_rect;
    Xv_Window       pw;


    /* clear if needed */
    if (flag == PANEL_CLEAR) {
	PANEL_EACH_PAINT_WINDOW(panel, pw)
	    view_rect = panel_viewable_rect(panel, pw);
	(void) pw_writebackground(pw,
				  view_rect->r_left, view_rect->r_top,
			  view_rect->r_width, view_rect->r_height, PIX_CLR);
	panel_repaint_background(panel, pw, view_rect);
	PANEL_END_EACH_PAINT_WINDOW

    } else {
	/* paint each hidden item -- only if haven't cleared */
	for (ip = panel->items; ip; ip = ip->next) {
	    if (hidden(ip)) {
		panel_redisplay_item(ip, PANEL_NO_CLEAR);
	    }
	}
    }

    /* Paint each non-hidden item */
    for (ip = panel->items; ip; ip = ip->next)
	panel_paint_item(ip);
}


/*****************************************************************************/
/* panel_paint()                                                             */
/* calls the painting routine for panels or items, as appropriate.           */
/*****************************************************************************/

Xv_public
panel_paint(client_object, flag)
    Panel           client_object;
    Panel_setting   flag;
{
    Panel_info     *object = PANEL_PRIVATE(client_object);

    if (!object || (flag != PANEL_CLEAR && flag != PANEL_NO_CLEAR))
	return NULL;

    if (is_panel(object)) {
	(*object->ops.panel_op_paint) (object, flag);
    } else {
	/*
	 * This is a hack to allow pre & post painting actions for all items.
	 */
	panel_redisplay_item((Item_info *) object, flag);
    }

    return 1;
}


Pkg_private void
panel_redisplay_item(ip, flag)
    register Item_info *ip;
    Panel_setting   flag;
{
    if (flag == PANEL_NONE)
	return;

    /* only clear if specified or hidden */
    if (flag == PANEL_CLEAR || hidden(ip)) {
	/* clear the previous painted item */
	panel_clear_item(ip);
    }
    /* call paint */
    panel_paint_item(ip);
}


static void
panel_paint_item(ip)
    register Item_info *ip;
{
    Panel_info	   *panel;
    Xv_Window       pw;
    Rect            rect;
    Rect	   *view_rect;

    if (!ip->ops.panel_op_paint || hidden(ip))
	return;   /* no paint procedure, or item is not visible */

    panel = ip->panel;
    rect = ip->rect;
    PANEL_EACH_PAINT_WINDOW(panel, pw)
	view_rect = panel_viewable_rect(panel, pw);
        /* if window is retained, paint the item whether in view or not  */
        if ((panel->paint_window->view && xv_get(pw, WIN_RETAINED)) ||
	    rect_intersectsrect(&rect, view_rect)) {
	    /* paint */
	    (*ip->ops.panel_op_paint) (ITEM_PUBLIC(ip));
	    ip->painted_rect = ip->rect;
	}
    PANEL_END_EACH_PAINT_WINDOW
}


Pkg_private void
panel_clear_item(ip)
    register Item_info *ip;
{
    if (ip->ops.panel_op_clear)
	(*ip->ops.panel_op_clear) (ITEM_PUBLIC(ip));
}


Xv_public void
panel_default_clear_item(item_public)
    Panel_item	    item_public;
{
    Item_info	   *ip = ITEM_PRIVATE(item_public);
    Panel_info	   *panel = ip->panel;
    Xv_Window       pw;

    if (!hidden(ip) && !rect_isnull(&ip->painted_rect)) {
	/* Item is visible and was not previously cleared */
	panel_clear_rect(panel, ip->painted_rect);
	PANEL_EACH_PAINT_WINDOW(panel, pw)
	    /* call client to repaint */
	    panel_repaint_background(panel, pw, &ip->painted_rect);
	PANEL_END_EACH_PAINT_WINDOW

	/* nothing is painted */
	    rect_construct(&ip->painted_rect, 0, 0, 0, 0);
    }
}


static void
panel_repaint_background(panel, pw, rect)
    register Panel_info *panel;
    Xv_Window       pw;
    Rect           *rect;
{
    Rectlist        rl, *current_rl;
    Rect            clip_rect;
    extern Rectlist *win_get_damage();

    if (!rect_isnull(rect) && panel->repaint_proc != NULL) {

	clip_rect = *rect;

	rl_initwithrect(&clip_rect, &rl);

	/* get current damage */
	current_rl = win_get_damage(pw);
	/* damage the rectangle */
	win_set_clip(pw, &rl);
	/* call client's repaint proc */
	panel->repaint_proc(PANEL_PUBLIC(panel), pw, &rl);
	win_set_clip(pw, current_rl);

	rl_free(&rl);
    }
}
