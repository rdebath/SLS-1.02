#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)wizzy.c 1.2 91/09/14";
#endif
#endif

#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/wizzy.h>

#define WIZZY_CAN_ACCEPT_KBD_FOCUS

#define WIZZY_PRIVATE(item) \
	XV_PRIVATE(Wizzy_info, Xv_panel_wizzy, item)

/* Item specific definitions */
#define INITIAL_OFFSET	10
#define BLOCK_WIDTH	16
#define BLOCK_HEIGHT	12

/* XView functions */
Pkg_private int wizzy_init();
Pkg_private Xv_opaque wizzy_set_avlist();
Pkg_private Xv_opaque wizzy_get_attr();
Pkg_private int wizzy_destroy();

/* Panel Item Operations
 *
 * Declare all wizzy item handler procedures used in the Ops Vector Table
 */
static void	wizzy_begin_preview();
static void	wizzy_update_preview();
static void	wizzy_cancel_preview();
static void	wizzy_accept_preview();
static void	wizzy_accept_menu();
static void	wizzy_accept_key();
static void	wizzy_paint();
static void	wizzy_remove();
static void	wizzy_restore();
static void	wizzy_layout();
#ifdef WIZZY_CAN_ACCEPT_KBD_FOCUS
static void	wizzy_accept_kbd_focus();
static void	wizzy_yield_kbd_focus();
#endif WIZZY_CAN_ACCEPT_KBD_FOCUS

/*
 * Panel Operations Vector Table for this item.
 *
 * If any of the operations do not apply, then use NULL as the handler address.
 */
static Panel_ops ops = {
    panel_default_handle_event,		/* handle_event() */
    wizzy_begin_preview,		/* begin_preview() */
    wizzy_update_preview,		/* update_preview() */
    wizzy_cancel_preview,		/* cancel_preview() */
    wizzy_accept_preview,		/* accept_preview() */
    wizzy_accept_menu,			/* accept_menu() */
    wizzy_accept_key,			/* accept_key() */
    wizzy_clear,			/* clear() */
    wizzy_paint,			/* paint() */
    wizzy_resize,			/* resize() */
    wizzy_remove,			/* remove() */
    wizzy_restore,			/* restore() */
    wizzy_layout,			/* layout() */
#ifdef WIZZY_CAN_ACCEPT_KBD_FOCUS
    wizzy_accept_kbd_focus,		/* accept_kbd_focus() */
    wizzy_yield_kbd_focus,		/* yield_kbd_focus() */
#else
    NULL,				/* accept_kbd_focus() */
    NULL,				/* yield_kbd_focus() */
#endif WIZZY_CAN_ACCEPT_KBD_FOCUS
    NULL				/* extension: reserved for future use */
};

typedef struct wizzy_info {
    Panel_item      public_self;/* back pointer to object */
    /*
     * Wizzy private data goes here.
     * *** All references to these entries is for illustration
     * *** purposes only.  They are to be replaced with your
     * *** item's private data requirements.
     */
    Rect	    block;	/* Some rectangle within the item */
    GC		    gc;		/* Graphics Context */
    int		    offset;	/* The block's offset */
    Panel	    panel;	/* Panel this item is owned by */
#ifdef WIZZY_CAN_ACCEPT_KBD_FOCUS
    int		    has_kbd_focus; /* TRUE or FALSE */
#endif WIZZY_CAN_ACCEPT_KBD_FOCUS
} Wizzy_info;



/* ========================================================================= */

/* -------------------- XView Functions  -------------------- */
Pkg_private int
wizzy_init(panel, item, avlist)
    Panel           panel;
    Panel_item      item;
    Attr_avlist     avlist;
{
    Xv_panel_wizzy *item_object = (Xv_panel_wizzy *) item;
    Display	   *display;
    Wizzy_info     *dp;
    XGCValues	    values;
    XID		    xid;

    dp = xv_alloc(Wizzy_info);

    item_object->private_data = (Xv_opaque) dp;
    dp->public_self = item;

    /*
     * Initialize non-zero private data
     */
    display = (Display *) XV_DISPLAY_FROM_WINDOW(panel);
    xid = (XID) xv_get(panel, XV_XID);
    values.foreground = BlackPixel(display, 0);
    dp->gc = XCreateGC(display, xid, GCForeground, &values);
    dp->offset = INITIAL_OFFSET;
    dp->panel = panel;

    /*
     * Process avlist for create-only attributes.
     */
    for ( ; *avlist; avlist = attr_next(avlist)) {
        switch ((int) avlist[0]) {
	  /* case <create_only_attr>: */
	  default:
	    break;
	}
    }

    xv_set(item,
	   PANEL_OPS_VECTOR, &ops,
#ifdef WIZZY_CAN_ACCEPT_KBD_FOCUS
	   PANEL_ACCEPT_KEYSTROKE, TRUE,
#endif WIZZY_CAN_ACCEPT_KBD_FOCUS
	   0);

    return XV_OK;
}


Pkg_private Xv_opaque
wizzy_set_avlist(item, avlist)
    Panel_item	    item;
    Attr_avlist	    avlist;
{
    Wizzy_info	   *dp = WIZZY_PRIVATE(item);
    Xv_opaque       result;
    Rect	    value_rect;

    if (*avlist != XV_END_CREATE) {
	/* Call generic item set code to handle layout attributes.
	 * Prevent panel_redisplay_item from being called in item_set_avlist.
	 */
	xv_set(dp->panel, PANEL_NO_REDISPLAY_ITEM, TRUE, 0);
        result = xv_super_set_avlist(item, &xv_panel_wizzy_pkg, avlist);
	xv_set(dp->panel, PANEL_NO_REDISPLAY_ITEM, FALSE, 0);
        if (result != XV_OK)
            return result;
    }
 
    /* Parse Attribute-Value List.  Complete initialization upon
     * receipt of XV_END_CREATE.
     */
    for ( ; *avlist; avlist = attr_next(avlist)) {
        switch ((int) avlist[0]) {
	  case WIZZY_OFFSET:
	    dp->offset = (int) avlist[1];
	    break;
	  case XV_END_CREATE:
	    value_rect = *(Rect *) xv_get(item, PANEL_ITEM_VALUE_RECT);
	    rect_construct(&dp->block,
			   value_rect.r_left + dp->offset,
			   value_rect.r_top,
			   BLOCK_HEIGHT, BLOCK_WIDTH);
	    value_rect = rect_bounding(&value_rect, &dp->block);
	    /* Note: Setting the value rect will cause the item rect to be
	     * recalculated as the enclosing rect containing both the label
	     * and value rects.
	     */
	    xv_set(item,
	    	   PANEL_ITEM_VALUE_RECT, &value_rect,
		   0);
	    break;
	  default:
	    break;
	}
    }
    return XV_OK;	/* return XV_ERROR if something went wrong... */
}


/*ARGSUSED*/
Pkg_private Xv_opaque
wizzy_get_attr(item, status, which_attr, avlist)
    Panel_item      item;
    int            *status;	/* set to XV_ERROR if something goes wrong */
    register Attr_attribute which_attr;
    va_list         avlist;
{
    Wizzy_info	   *dp = WIZZY_PRIVATE(item);

    switch (which_attr) {
      case WIZZY_OFFSET:
	return (Xv_opaque) dp->offset;
      default:
	*status = XV_ERROR;
	return (Xv_opaque) 0;
    }
}
      

/*ARGSUSED*/
Pkg_private int
wizzy_destroy(item, status)
    Panel_item      item;
    Destroy_status  status;
{
    Wizzy_info	   *dp = WIZZY_PRIVATE(item);

    if ((status == DESTROY_CHECKING) || (status == DESTROY_SAVE_YOURSELF))
	return XV_OK;
#ifdef WIZZY_CAN_ACCEPT_KBD_FOCUS
    wizzy_remove(item);
#endif WIZZY_CAN_ACCEPT_KBD_FOCUS
    free(dp);
    return XV_OK;
}



/* --------------------  Panel Item Operations  -------------------- */
/*ARGSUSED*/
static void
wizzy_begin_preview(item, event)
    Panel_item	    item;
    Event          *event;
{
    /*
     * SELECT-down has occurred over the item.
     * Highlight the item to show the active feedback,
     * but do not take any action yet.
     */
}


/*ARGSUSED*/
static void
wizzy_update_preview(item, event)
    Panel_item      item;
    Event          *event;
{
    /*
     * The pointer as been dragged within the item after
     * beginning a preview.  Adjust the highlighting to
     * reflect the new position of the pointer, and update
     * appropriate private data.
     */
}


/*ARGSUSED*/
static void
wizzy_cancel_preview(item, event)
    Panel_item      item;
    Event          *event;
{
    /*
     * The pointer as been dragged out of the item after
     * beginning a preview.  Remove the active feedback
     * (i.e., unhighlight) and clean up any private data.
     */
}


/*ARGSUSED*/
static void
wizzy_accept_preview(item, event)
    Panel_item      item;
    Event          *event;
{
    /*
     * The SELECT button has been released over the item.
     * Remove the active feedback (i.e., unhighlight),
     * paint the busy feedback,
     * perform the action associated with the item,
     * and then remove the busy feedback.
     */
}


/*ARGSUSED*/
static void
wizzy_accept_menu(item, event)
    Panel_item      item;
    Event          *event;
{
    /*
     * The MENU button has been depressed over the item.
     * Show the menu attached to the item, if any.
     */
}


/*ARGSUSED*/
static void
wizzy_accept_key(item, event)
    Panel_item      item;
    Event          *event;
{
    /*
     * A keyboard event has occurred.  Process the key,
     * and update the display.
     */
}


static void
wizzy_clear(item_public)
    Panel_item	    item_public;
{
    /*
     * Clear the item rectangle.  Update any private data as needed.
     */
    panel_default_clear_item(item_public);
}


/*ARGSUSED*/
static void
wizzy_paint(item)
    Panel_item      item;
{
    Display	   *display;
    Wizzy_info	   *dp = WIZZY_PRIVATE(item);
    Panel_paint_window *ppw;	/* ptr to Panel_paint_window structure */
    Xv_Window	    pw;		/* paint window */
    XID		    xid;

    /*
     * Do everything necessary to paint the entire item.
     * Don't go outside of the PANEL_ITEM_RECT, the rectangle describing the
     * boundaries of the item.
     */

    /* Paint the label */
    panel_paint_label(item);

    /* Paint the value.
     * In this wizzy example, we paint something into dp->block.
     */
    display = (Display *) XV_DISPLAY_FROM_WINDOW(dp->panel);
    for (ppw = (Panel_paint_window *)
	     xv_get(dp->panel, PANEL_FIRST_PAINT_WINDOW);
	 ppw;
	 ppw = ppw->next) {
	pw = ppw->pw;	/* pw = the actual window to paint in */
	xid = (XID) xv_get(pw, XV_XID);
	XFillRectangle(display, xid, dp->gc, dp->block.r_left, dp->block.r_top,
		       dp->block.r_width, dp->block.r_height);
    }
}


/*ARGSUSED*/
static void
wizzy_resize(item)
    Panel_item	    item;
{
    /*
     * The panel has been resized.  Recalculate any extend-to-edge dimensions.
     */
}


/*ARGSUSED*/
static void
wizzy_remove(item)
    Panel_item	    item;
{
    /*
     * The item has been made hidden via xv_set(item, XV_SHOW, FALSE, avlist).
     */
#ifdef WIZZY_CAN_ACCEPT_KBD_FOCUS
    Wizzy_info	   *dp = WIZZY_PRIVATE(item);
    Panel_status   *panel_status;

    /*
     * Only reassign the keyboard focus to another item if the panel isn't
     * being destroyed.
     */
    panel_status = (Panel_status *) xv_get(dp->panel, PANEL_STATUS);
    if (!panel_status->destroying &&
	xv_get(dp->panel, PANEL_CARET_ITEM) == item)
	(void) panel_advance_caret(dp->panel);
#endif WIZZY_CAN_ACCEPT_KBD_FOCUS
}


/*ARGSUSED*/
static void
wizzy_restore(item)
    Panel_item	    item;
{
    /*
     * The item has been made visible via xv_set(item, XV_SHOW, TRUE, avlist).
     */
#ifdef WIZZY_CAN_ACCEPT_KBD_FOCUS
    Wizzy_info	   *dp = WIZZY_PRIVATE(item);

    /* If no item has the keyboard focus, then give this item the focus */
    if (!xv_get(dp->panel, PANEL_CARET_ITEM))
        xv_set(dp->panel, PANEL_CARET_ITEM, item, 0);
#endif WIZZY_CAN_ACCEPT_KBD_FOCUS
}


/*ARGSUSED*/
static void
wizzy_layout(item, deltas)
    Panel_item	    item;
    Rect	   *deltas;
{
    /*
     * The item has been moved.  Adjust the item coordinates.
     */
    Wizzy_info	   *dp = WIZZY_PRIVATE(item);

    dp->block.r_left += deltas->r_left;
    dp->block.r_top += deltas->r_top;
}


#ifdef WIZZY_CAN_ACCEPT_KBD_FOCUS
/*ARGSUSED*/
static void
wizzy_accept_kbd_focus(item)
    Panel_item	    item;
{
    /*
     * The keyboard focus has been set to this item.
     * Change the keyboard focus feedback to active, and
     * update private data as necessary.
     */
    Wizzy_info	   *dp = WIZZY_PRIVATE(item);
    Frame	    frame;
    int		    x;
    int		    y;

    dp->has_kbd_focus = TRUE;
    frame = xv_get(dp->panel, WIN_FRAME);
    if (xv_get(dp->panel, PANEL_LAYOUT) == PANEL_HORIZONTAL) {
	xv_set(frame, FRAME_FOCUS_DIRECTION, FRAME_FOCUS_UP, 0);
	x = dp->block.r_left +
	    (dp->block.r_width - FRAME_FOCUS_UP_WIDTH)/2;
	y = dp->block.r_top + dp->block.r_height - FRAME_FOCUS_UP_HEIGHT/2;
    } else {
	xv_set(frame, FRAME_FOCUS_DIRECTION, FRAME_FOCUS_RIGHT, 0);
	x = dp->block.r_left - FRAME_FOCUS_RIGHT_WIDTH/2;
	y = dp->block.r_top +
	    (dp->block.r_height - FRAME_FOCUS_RIGHT_HEIGHT)/2;
    }
    if (x < 0)
	x = 0;
    if (y < 0)
	y = 0;
    panel_show_focus_win(item, frame, x, y);
}


/*ARGSUSED*/
static void
wizzy_yield_kbd_focus(item)
    Panel_item	    item;
{
    /*
     * The keyboard focus has been removed from this item.
     * Change the keyboard focus feedback to inactive, and
     * update private data as necessary.
     */
    Wizzy_info	   *dp = WIZZY_PRIVATE(item);
    Xv_Window	    focus_win;
    Frame	    frame;

    dp->has_kbd_focus = FALSE;
    frame = xv_get(dp->panel, WIN_FRAME);
    focus_win = xv_get(frame, FRAME_FOCUS_WIN);
    xv_set(focus_win, XV_SHOW, FALSE, 0);
}
#endif WIZZY_CAN_ACCEPT_KBD_FOCUS

