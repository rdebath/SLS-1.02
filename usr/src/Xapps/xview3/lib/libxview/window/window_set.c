#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)window_set.c 20.123 91/09/14";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

#include <xview_private/i18n_impl.h>
#include <xview_private/windowimpl.h>
#include <sys/file.h>
#include <assert.h>
#include <xview_private/win_keymap.h>
#include <xview/font.h>
#include <xview/frame.h>
#include <xview/notify.h>
#include <xview/server.h>
#include <xview/fullscreen.h>
#ifdef SVR4 
#include <stdlib.h> 
#endif SVR4

/*
 * Extern
 */
extern 	    void	win_insert_in_front();
Pkg_private void 	win_add_drop_itme();
Pkg_private Xv_opaque 	win_delete_drop_item();


/*
 * Private
 */

static 		void	set_mask_bit(),
			unset_mask_bit(),
#ifdef OW_I18N
			window_set_imcb(),
#endif
			window_set_event_mask(),
			window_grab_selectbutton(),
			win_map(),
			update_rect();
Xv_private 	void	window_release_selectbutton();
Pkg_private	void	window_set_cmap_property();
Pkg_private 	void	sync_rect();


Pkg_private     Xv_opaque
window_set_avlist(win_public, avlist)
    Xv_Window       win_public;
    Attr_attribute  avlist[];
{
    register Window_info 	*win = WIN_PRIVATE(win_public);
    register Attr_avlist  	attrs;
    register Xv_Drawable_info 	*info;
    int             		x, y;
    Rect            		old_rect, new_rect;
    register int    		win_rows = 0, win_cols = 0;
    XSetWindowAttributes        win_attrs;
    unsigned long               win_attrs_mask = 0;
    void                        window_set_softkey_labels();
    short			error = XV_OK;

    DRAWABLE_INFO_MACRO(win_public, info);
    /* Initialize to unset rects */
    rect_construct(&old_rect, EMPTY_VALUE, EMPTY_VALUE,
			      EMPTY_VALUE, EMPTY_VALUE);
    new_rect = old_rect;

    /* reset rect_info on each set except during create phase */
    if (win->created) {
	win->rect_info = 0;
    }
    for (attrs = avlist; *attrs; attrs = attr_next(attrs)) {
	switch (attrs[0]) {
	  case WIN_OUTER_RECT:	/* outer border */
	    new_rect = *(Rect *) (attrs[1]);
	    if (win->has_border)
		rect_borderadjust(&new_rect, WIN_DEFAULT_BORDER_WIDTH);
	    win->rect_info |= WIN_X_SET|WIN_Y_SET|WIN_HEIGHT_SET|WIN_WIDTH_SET;
	    break;

	  case WIN_X_PAINT_WINDOW:
	    /* window being used to support direct X graphics */
	    win->x_paint_window = (int) attrs[1];
	    break;

	  case WIN_NO_CLIPPING:
	    /* dont set the GC clip rects on repaints */
	    win->no_clipping = (int) attrs[1];
	    break;

	  case WIN_BACK:	/* Put the window in the back of all siblings */
	    XLowerWindow(xv_display(info), xv_xid(info));
	    break;

	  case WIN_BACKGROUND_PIXMAP:
	    if (attrs[1]) {
		win->background_pixmap = (Pixmap) attrs[1];
		win_attrs_mask |= CWBackPixmap;
		win_attrs.background_pixmap = (Pixmap) attrs[1];
	    }
	    break;

	  case WIN_BIT_GRAVITY:
	    win_attrs_mask |= CWBitGravity;
	    win_attrs.bit_gravity = (int) attrs[1];
	    break;

	  case WIN_BORDER: {	/* this affects the outer rect */
	    int width  =  xv_get (win_public, XV_WIDTH);
	    int height =  xv_get (win_public, XV_HEIGHT);

	    if (win->has_border == (int) attrs[1])
		break;

	    win->has_border = (int) attrs[1];
	    if (win->has_border) {
		window_set_border(win_public, WIN_DEFAULT_BORDER_WIDTH);
	    } else {
		window_set_border(win_public, 0);
	    }
	    
	    if (win->desired_width == WIN_EXTEND_TO_EDGE) {
		width = win->has_border ? 
		    width-2*WIN_DEFAULT_BORDER_WIDTH: 
		    width+2*WIN_DEFAULT_BORDER_WIDTH;
		new_rect.r_width = width;
	    }
	    if (win->desired_height == WIN_EXTEND_TO_EDGE) {
		height = win->has_border ? 
		    height-2*WIN_DEFAULT_BORDER_WIDTH: 
		    height+2*WIN_DEFAULT_BORDER_WIDTH;
		new_rect.r_height = height;
	    }
	    break;
	  }
	  case WIN_CMD_LINE:
	    if (win->cmdline && ((int)win->cmdline != -1))  {
		free(win->cmdline);
	    }

	    if ((int)attrs[1] != -1)  {
	        win->cmdline = strdup((char *)attrs[1]);
	    }
	    else  {
	        win->cmdline = (char *)attrs[1];
	    }

	    break;
	  case WIN_LOCKDATA:
	    /* lock the window tree for modification (grab server) */
	    win_lockdata(win_public);
	    break;
	  case WIN_ALARM:{
		/* sound the window alarm/bell */
		/* (someday this will be something other than timeval) */
		struct timeval  tv;

		XV_BCOPY((struct timeval *) xv_get(win_public,WIN_ALARM_DATA,tv),
		      &tv, sizeof(struct timeval));
		(void) win_bell(win_public, tv, win_public);
	    }
	    break;

	  case WIN_SAVE_UNDER:
	    win_attrs_mask |= CWSaveUnder;
	    win_attrs.save_under = ((int)attrs[1] == TRUE) ? True : False;
	    break;

	  case WIN_TOP_LEVEL_NO_DECOR:
	    if (!win->top_level && !xv_get(win_public, XV_IS_SUBTYPE_OF,
					   FRAME_CLASS)) {
		xv_error(win_public,
		    ERROR_STRING,
		    XV_MSG("WIN_TOP_LEVEL_NO_DECOR specified for non-top level non-frame window"),
		    0);
	    }
	    win->top_level_no_decor = (int) attrs[1];
	    win_attrs_mask |= CWOverrideRedirect;
	    win_attrs.override_redirect = ((int)attrs[1] == TRUE) ? True:False;
	    break;

	  case WIN_KBD_FOCUS:
	    win->has_kbd = (int) attrs[1];
	    break;

	  case WIN_DESIRED_WIDTH:
	    win->desired_width = (int) attrs[1];
	    break;

	  case WIN_DESIRED_HEIGHT:
	    win->desired_height = (int) attrs[1];
	    break;

	  case WIN_BELOW:{
		int             vp;

		if ((Xv_Window) (attrs[1]) == win_public) {
		    (void) update_rect(win, &old_rect, &new_rect);
		}
		(void) win_appeal_to_owner(TRUE, win, (Xv_opaque) WIN_GET_BELOW,
				  (Xv_opaque) (attrs[1]), (Xv_opaque) & vp);
		new_rect.r_top = vp;
		win->rect_info |= WIN_Y_SET;
	    }
	    break;

	  case WIN_CLIENT_DATA:
	    win->client_data = (Xv_opaque) attrs[1];
	    break;

	  case WIN_COLUMNS:
	    win_cols = (int) attrs[1];
	    break;

	  case WIN_CURSOR:
	    if (win->cursor) {
		(void) xv_set(win->cursor, XV_DECREMENT_REF_COUNT, 0);
	    }
	    win->cursor = (Xv_opaque) attrs[1];
	    (void) xv_set(win->cursor, XV_INCREMENT_REF_COUNT, 0);
	    win_attrs_mask |= CWCursor;
	    win_attrs.cursor = (Cursor)xv_get(win->cursor, XV_XID);
	    break;

	  case WIN_EVENT_PROC:
	    win->event_proc = (void (*) ()) attrs[1];
	    break;

	  case WIN_FIT_HEIGHT:
	    y = (int) xv_get(win_public, WIN_FIT_HEIGHT, attrs[1]);
	    if (y <= 0) {
		sync_rect(win, &old_rect, &new_rect);
		y = new_rect.r_height;
	    }
	    y += (int) attrs[1];/* Avoid lvalue cast */
	    attrs[1] = (Window_attribute) y;
	    new_rect.r_height = y;
	    win->rect_info |= WIN_HEIGHT_SET;
	    break;

	  case WIN_FIT_WIDTH:
	    x = (int) xv_get(win_public, WIN_FIT_WIDTH, attrs[1]);
	    if (x <= 0) {
		sync_rect(win, &old_rect, &new_rect);
		x = new_rect.r_width;
	    }
	    x += (int) attrs[1];/* Avoid lvalue cast */
	    attrs[1] = (Window_attribute) x;
	    new_rect.r_width = x;
	    win->rect_info |= WIN_WIDTH_SET;
	    break;

	  case WIN_FRONT:
	    /* Bring window to top of the heap */
	    win_setlink(win_public, WL_COVERED, 0);

	    break;

	  case XV_HEIGHT:
	    if ((int) (attrs[1]) != WIN_EXTEND_TO_EDGE) {
		new_rect.r_height = (int) (attrs[1]);
		win->rect_info |= WIN_HEIGHT_SET;
	    } else {
		win->desired_height = (int) attrs[1];
	    }
	    break;

	  case WIN_HORIZONTAL_SCROLLBAR:
	    break;

	  case WIN_LAYOUT_PROC:
	    win->layout_proc = (int (*) ()) attrs[1];
	    break;

	  case WIN_MENU:
	    if (win->menu) {
		(void) xv_set(win->menu, XV_DECREMENT_REF_COUNT, 0);
	    }
	    win->menu = (Xv_opaque) attrs[1];
	    if (win->menu) {
		(void) xv_set(win->menu, XV_INCREMENT_REF_COUNT, 0);
	    }
	    break;

	  case WIN_MOUSE_XY:
	    (void) win_setmouseposition(win_public, (short) attrs[1],
					(short) attrs[2]);
	    break;

	  case WIN_NOTIFY_SAFE_EVENT_PROC:
	    /*
	     * NOTICE: Here we interpose on the existing event_func, maybe
	     * this should be called WIN_INTERPOSE_EVENT_PROC
	     */
	    (void) notify_interpose_event_func(win_public,
				(Notify_value(*) ()) attrs[1], NOTIFY_SAFE);
	    win->notify_safe_event_proc = (void (*) ()) attrs[1];
	    break;

	  case WIN_NOTIFY_IMMEDIATE_EVENT_PROC:
	    (void) notify_interpose_event_func(win_public,
			   (Notify_value(*) ()) attrs[1], NOTIFY_IMMEDIATE);
	    win->notify_immediate_event_proc = (void (*) ()) attrs[1];
	    break;

	  case WIN_PERCENT_HEIGHT:
	    /* can't do % height of no owner */
	    if (!win->owner)
		break;
	    attrs[1] = (Window_attribute)
		((((int) window_get(WIN_PUBLIC(win->owner), WIN_HEIGHT) -
		   win->owner->top_margin - win->owner->bottom_margin) *
		  (int) attrs[1] / 100) -
		 win->owner->row_gap);
	    new_rect.r_height = (int) attrs[1];
	    win->rect_info |= WIN_HEIGHT_SET;

	    break;

	  case WIN_PERCENT_WIDTH:
	    /* can't do % width of no owner */
	    if (!win->owner)
		break;

	    attrs[1] = (Window_attribute)
		((((int) window_get(WIN_PUBLIC(win->owner), WIN_WIDTH) -
		   win->owner->left_margin - win->owner->right_margin) *
		  (int) attrs[1] / 100) - win->owner->column_gap);
	    new_rect.r_width = (int) attrs[1];
	    win->rect_info |= WIN_WIDTH_SET;
	    break;

	  case WIN_RECT:
	    new_rect = *(Rect *) (attrs[1]);
	    win->rect_info |= WIN_X_SET | WIN_Y_SET;
	    if (new_rect.r_width != WIN_EXTEND_TO_EDGE) {
		win->rect_info |= WIN_WIDTH_SET;
	    } else {
		win->desired_width = WIN_EXTEND_TO_EDGE;
	    }
	    if (new_rect.r_height != WIN_EXTEND_TO_EDGE) {
		win->rect_info |= WIN_HEIGHT_SET;
	    } else {
		win->desired_height = WIN_EXTEND_TO_EDGE;
	    }
	    break;

	  case WIN_RIGHT_OF:{
		int             vp;
		if ((Xv_Window) (attrs[1]) == win_public) {
		    (void) update_rect(win, &old_rect, &new_rect);
		}
		(void) win_appeal_to_owner(TRUE, win,
			    (Xv_opaque) WIN_GET_RIGHT_OF, 
			    (Xv_opaque) (attrs[1]), (Xv_opaque) & vp);
		new_rect.r_left = vp;
		win->rect_info |= WIN_X_SET;
	    }
	    break;


	  case WIN_ROWS:
	    win_rows = (int) attrs[1];
	    break;

	  case XV_SHOW:
	  case WIN_MAP:
	    if ((int) attrs[1]) {
	        (void) update_rect(win, &old_rect, &new_rect);

		if (attrs[0] == XV_SHOW)
		    /* Bring window to top of the stack */
		    win_setlink(win_public, WL_COVERED, 0);

		/* Register drop sites on map of window */
		if (win->dropSites) {
		    Win_drop_site_list *node = win->dropSites;
		    Xv_opaque frame = win_get_top_level(win_public);
		    Window_info *frame_private = WIN_PRIVATE(frame);

		    while (node = (Win_drop_site_list *)
					(XV_SL_SAFE_NEXT(node))) {
		        win_add_drop_interest(frame_private, node->drop_item);
		    }
		}
		/* If we are mapping the frame, update the drop site 
		 * interest property.  dropInterest is only set on top
		 * level windows.
		 */
		/* XXX: Should check to see if anything has changed, if not
		 * don't write property again.
		 */
		if (win->dropInterest) 
		    win_update_dnd_property(win);
	    } else {
		/* Unregister drop sites on unmap of window */
		if (win->dropSites) {
		    Win_drop_site_list *node = win->dropSites;
		    Xv_opaque frame = win_get_top_level(win_public);
		    Window_info *frame_private = WIN_PRIVATE(frame);

		    while (node = (Win_drop_site_list *)
					(XV_SL_SAFE_NEXT(node))) {
		        win_delete_drop_item(frame_private, node->drop_item,
					     Win_Drop_Interest);
		    }
		}
	    }
	    win_map(win, (int) attrs[1]);
	    break;

	  case WIN_RETAINED:
	    win_attrs_mask |= CWBackingStore;
	    win_attrs.backing_store =
		((int)attrs[1] == TRUE) ? Always : NotUseful;
	    break;

	  case WIN_VERTICAL_SCROLLBAR:
	    break;

	  case WIN_IS_IN_FULLSCREEN_MODE:
	    win->in_fullscreen_mode = (int) (attrs[1]);
	    break;

	  case WIN_WINDOW_GRAVITY:
	    win_attrs_mask |= CWWinGravity;
	    win_attrs.win_gravity = (int) attrs[1];
	    break;

	  case XV_WIDTH:
	    if ((int) (attrs[1]) != WIN_EXTEND_TO_EDGE) {
		new_rect.r_width = (int) (attrs[1]);
		win->rect_info |= WIN_WIDTH_SET;
	    } else {
		win->desired_width = (int) attrs[1];
	    }
	    break;

	  case XV_X:
	    new_rect.r_left = (int) (attrs[1]);
	    win->rect_info |= WIN_X_SET;
	    break;

	  case XV_Y:
	    new_rect.r_top = (int) (attrs[1]);
	    win->rect_info |= WIN_Y_SET;
	    break;

	  case WIN_GRAB_ALL_INPUT:
	    if (attrs[1])
		(void) win_grabio(win_public);
	    else
		(void) win_releaseio(win_public);
	    break;

	  case WIN_INPUT_MASK:
	    window_set_event_mask(win_public, (Inputmask *) attrs[1],
		&win_attrs, &win_attrs_mask);
	    break;


	  case WIN_CONSUME_EVENT:
	  case WIN_IGNORE_EVENT:{
		Inputmask       im;

		(void) win_xmask_to_im(win->xmask, &im);
		if (attrs[0] == WIN_CONSUME_EVENT) {
		    set_mask_bit(&im, attrs[1]);
		} else {
		    unset_mask_bit(&im, attrs[1], win_public);
		}
		window_set_event_mask(win_public, &im, &win_attrs,
			&win_attrs_mask);
		break;
	    }

	  case WIN_CONSUME_X_EVENT_MASK:
		if (win->xmask != (unsigned int)attrs[1]) {
		    win->xmask |= (unsigned int)attrs[1];
		    win_attrs_mask |= CWEventMask;
		    win_attrs.event_mask = win->xmask;;
		}
		break;

	  case WIN_IGNORE_X_EVENT_MASK:
		if (win->xmask & (unsigned int)attrs[1]) {
		    window_grab_selectbutton(win_public, win->xmask,
						  (unsigned int)attrs[1], NULL);
		    win->xmask &= ~(unsigned int)attrs[1];
		    win_attrs_mask |= CWEventMask;
		    win_attrs.event_mask = win->xmask;;
		}
		break;

	  case WIN_X_EVENT_MASK:
		window_grab_selectbutton(win_public, win->xmask,
						  (unsigned int)attrs[1], NULL);
		win->xmask = (unsigned int)attrs[1];
		win_attrs_mask |= CWEventMask;
		win_attrs.event_mask = win->xmask;;
		break;

	  case WIN_CONSUME_EVENTS:
	  case WIN_IGNORE_EVENTS:{
		register int    i;
		Inputmask       im;

		(void) win_xmask_to_im(win->xmask, &im);

		for (i = 1; attrs[i]; i++) {
		    if (attrs[0] == WIN_CONSUME_EVENTS)
			set_mask_bit(&im, attrs[i]);
		    else
			unset_mask_bit(&im, attrs[i], win_public);
		}
		window_set_event_mask(win_public, &im, &win_attrs,
			&win_attrs_mask);
		break;
	    }

	  case WIN_OWNER:
	    /* BUG: should we tell the old or new owner? */
	    win->owner = WIN_PRIVATE((Xv_Window) attrs[1]);
	    break;

	  case WIN_PARENT:{
		Rect            rect;

		(void) update_rect(win, &old_rect, &new_rect);
		win_appeal_to_owner(FALSE, win, (Xv_opaque) WIN_GET_RECT,
				    (Xv_opaque) & rect);
		win_set_parent(win_public, attrs[1], rect.r_left, rect.r_top);

		break;
	    }

	  case WIN_RECT_INFO:
	    win->rect_info = (int) attrs[1];
	    break;

	  case WIN_REMOVE_CARET:
	    /* Ignore: handled by the package with the caret
	     * (e.g., panel, textsw)
	     */
	    break;

	  case WIN_SET_FOCUS:
	    if (!xv_no_focus(info) && (win->xmask & FocusChangeMask))
		win_set_kbd_focus(win_public, xv_xid(info));
	    else
		error = (Xv_opaque) XV_ERROR;
	    break;

	  case XV_END_CREATE:
	    {
		register Window_info *owner = win->owner;

		(void) update_rect(win, &old_rect, &new_rect);

		/*
		 * if win is not root window, then call owner's layout proc
		 * and map the window if necessary.
		 */
		if (owner) {
		    /* tell the parent we have created this child */
		    (owner->layout_proc) (WIN_PUBLIC(owner), win_public,
					  WIN_CREATE,
					  xv_get(win_public, XV_NAME));
		    if (win->map) {
			win_insert(win_public);
		    }
		}

		/* Make sure window's get right colormap installed by wmgr */
		window_set_cmap_property(win_public);

		/* gross fix me */
		/* update_rect is called at end of this function */
		/* for END_CREATE must force before setting */
		/* win->created to TRUE so RESIZE event not posted */
		/* that is why we return directly from this case */
		/* this works because we know this is the only */
		/* attribute being processed on the set */

		(void) update_rect(win, &old_rect, &new_rect);

		/* must set created bit after all work -- for once  */
		/* created bit is set any change to windows rect gets */
		/* posted to client. By delaying to here no RESIZE event */
		/* is generated on create */

		win->created = TRUE;

		return (error);
	    }

	  case XV_FONT:
	    if ((attrs[1]) && (win->font != (Xv_font) attrs[1])) {
		(void) xv_set(win->font, XV_DECREMENT_REF_COUNT, 0);
		win->font = (Xv_font) attrs[1];
		(void) xv_set(win->font, XV_INCREMENT_REF_COUNT, 0);
	    }
	    break;

	  case WIN_GLYPH_FONT:
	    win->glyph_font = (Xv_Font) attrs[1];
	    break;

	  case XV_TOP_MARGIN:
	    win->top_margin = (int) attrs[1];
	    break;

	  case XV_BOTTOM_MARGIN:
	    win->bottom_margin = (int) attrs[1];
	    break;

	  case XV_LEFT_MARGIN:
	    win->left_margin = (int) attrs[1];
	    break;

	  case XV_RIGHT_MARGIN:
	    win->right_margin = (int) attrs[1];
	    break;

	  case WIN_ROW_HEIGHT:
	    win->row_height = (int) attrs[1];
	    break;

	  case WIN_COLUMN_WIDTH:
	    win->column_width = (int) attrs[1];
	    break;

	  case WIN_ROW_GAP:
	    win->row_gap = (int) attrs[1];
	    break;

	  case WIN_COLUMN_GAP:
	    win->column_gap = (int) attrs[1];
	    break;

	  case WIN_COLLAPSE_EXPOSURES:
	    win->collapse_exposures = (int) attrs[1];
	    break;

          case WIN_SOFT_FNKEY_LABELS:
            win->softkey_flag = TRUE;
	    window_set_softkey_labels(info,(char *) attrs[1]);
            break;

	  case WIN_CMS: {
	    Cms			cms;

	    cms = (Cms)attrs[1];
	    if (xv_cms(info) != cms)
	      window_set_cms(win_public, cms, 0, 
			     (int)(xv_get(cms, CMS_SIZE, 0) - 1));
	    break;
          }
	    
	  case WIN_CMS_CHANGE:
	      /*
	       * Handle this to prevent default error message. The 
	       * WIN_CMS_CHANGE attribute is used to notify objects
	       * subclassed from the window object about changes in their 
	       * colormap segment.
	       */
	       break;

	  case WIN_CMS_NAME:
	    window_set_cms_name(win_public, (char *) attrs[1]);
	    break;

	  case WIN_FOREGROUND_COLOR:{
		int cms_fg;

		cms_fg = (unsigned long)attrs[1];
		if (xv_cms_fg(info) != cms_fg) {
		    window_set_cms(win_public, xv_cms(info), xv_cms_bg(info),
									cms_fg);
		}
		break;
	  }

	  case WIN_BACKGROUND_COLOR:{
		int cms_bg;

		cms_bg = (unsigned long)attrs[1];
		if (xv_cms_bg(info) != cms_bg) {
		    window_set_cms(win_public, xv_cms(info), cms_bg,
							       xv_cms_fg(info));
		}
		break;
	  }

	  case WIN_COLOR_INFO: {
		Xv_Color_info	*color_info;

		color_info = (Xv_Color_info *)attrs[1];
		if ((xv_cms(info) != color_info->cms) || 
		    (xv_cms_bg(info) != color_info->cms_bg) || 
		    (xv_cms_fg(info) != color_info->cms_fg)) {
		    window_set_cms(win_public, color_info->cms,
					color_info->cms_bg, color_info->cms_fg);
		}
		break;
	  }

	  case WIN_CMS_DATA:
	    window_set_cms_data(win_public, (Xv_cmsdata *) attrs[1]);
	    break;

	  case WIN_INHERIT_COLORS:
	    win->inherit_colors = (Bool) attrs[1];
	    break;

#ifdef OW_I18N
          case WIN_IC_PREEDIT_START:
           	win->start_pecb_struct.callback = (XIMProc)attrs[1];
	  	win->start_pecb_struct.client_data = (XPointer)attrs[2];
		if (win->win_use_im && win->xic)
			window_set_imcb(win_public, win->xic);
		break;

          case WIN_IC_PREEDIT_DRAW:
   		win->draw_pecb_struct.callback = (XIMProc)attrs[1];
               	win->draw_pecb_struct.client_data = (XPointer)attrs[2];
		if (win->win_use_im && win->xic)
			window_set_imcb(win_public, win->xic);
                break;

          case WIN_IC_PREEDIT_DONE:
   		win->done_pecb_struct.callback = (XIMProc)attrs[1];
               	win->done_pecb_struct.client_data = (XPointer)attrs[2];
		if (win->win_use_im && win->xic)
			window_set_imcb(win_public, win->xic);
                break;

          case WIN_IC_STATUS_START:
                win->start_stcb_struct.callback = (XIMProc)attrs[1];
		win->start_stcb_struct.client_data = (XPointer)attrs[2];
		if (win->win_use_im && win->xic)
			window_set_imcb(win_public, win->xic);
                break;
 
          case WIN_IC_STATUS_DRAW:
                win->draw_stcb_struct.callback = (XIMProc)attrs[1];
                win->draw_stcb_struct.client_data = (XPointer)attrs[2];
		if (win->win_use_im && win->xic)
			window_set_imcb(win_public, win->xic);
                break;

          case WIN_IC_STATUS_DONE:
                win->done_stcb_struct.callback = (XIMProc)attrs[1];
                win->done_stcb_struct.client_data = (XPointer)attrs[2];
		if (win->win_use_im && win->xic)
			window_set_imcb(win_public, win->xic);
                break;

          case WIN_IC_LUC_START:
                win->start_luc_struct.callback = (XIMProc)attrs[1];
                win->start_luc_struct.client_data = (XPointer)attrs[2];
		if (win->win_use_im && win->xic)
			window_set_imcb(win_public, win->xic);
                break;

          case WIN_IC_LUC_DRAW:
                win->draw_luc_struct.callback = (XIMProc)attrs[1];
                win->draw_luc_struct.client_data = (XPointer)attrs[2];
		if (win->win_use_im && win->xic)
			window_set_imcb(win_public, win->xic);
                break;

          case WIN_IC_LUC_DONE:
                win->done_luc_struct.callback = (XIMProc)attrs[1];
                win->done_luc_struct.client_data = (XPointer)attrs[2];
		if (win->win_use_im && win->xic)
			window_set_imcb(win_public, win->xic);
                break;

          case WIN_IC_LUC_PROCESS:
                win->process_luc_struct.callback = (XIMProc)attrs[1];
 		win->process_luc_struct.client_data  = (XPointer)attrs[2];
		if (win->win_use_im && win->xic)
			window_set_imcb(win_public, win->xic);
		break;
 
	  case WIN_IC_CONVERSION:
		if (win->win_use_im && win->xic) {

			int	status;

             		if (!XGetICValue(win->xic, XNExtSun_Conversion,
			   &status, NULL)){

			   win->win_ic_conversion = (Bool) attrs[1];
                	   /*
                 	    * This IM supports Sun Extensions
                 	    */
			   if (win->win_ic_conversion) 
			      XSetICValue(win->xic, XNExtSun_Conversion, 					  XIMEnable, NULL);
			   else
			      XSetICValue(win->xic, XNExtSun_Conversion, 					  XIMDisable, NULL);
             		} else {
                	   /*
                 	    * This IM does not support Sun Extension.
                 	    */
                	   xv_error(NULL, ERROR_STRING,
                        	    XV_MSG("IM does not support Sun Extenstions"),
                        	    ERROR_PKG, WINDOW,
                                    NULL);
             		}
		}
		break;

	  case WIN_IC_RESET:
             if (win->xic) {
 
                int        status;
 
                if(!XGetICValue(win->xic, XNExtSun_Conversion, &status, NULL)){
 
		   if (win->win_ic_committed) {
			XFree(win->win_ic_committed);
			win->win_ic_committed = (char *)NULL;
		   }
		   if (win->win_ic_committed_wcs) {
			xv_free(win->win_ic_committed_wcs);
			win->win_ic_committed_wcs = (wchar_t *)NULL;
		   }
                   /*
                    * This IM supports Sun Extensions
                    */
                   win->win_ic_committed = (char *)XmbResetIC(win->xic);
		   win->win_ic_committed_wcs = (wchar_t *)mbstowcsdup(win->win_ic_committed);
                   win->win_ic_conversion = FALSE;
 
                } else {
 
                   /*
                    * This IM does not support Sun Extension.
                    */
                   xv_error(NULL, ERROR_STRING,
                           XV_MSG("IM does not support Sun Extenstions"),
                           ERROR_PKG, WINDOW,
                           NULL); 
                   return (Xv_opaque) NULL;
                } 
 
             } else {
 
                   xv_error(NULL, ERROR_STRING,
                               XV_MSG("Invalid IC: Unable to reset IC. "),             
                               ERROR_PKG, WINDOW,
                               0);
                return (Xv_opaque) NULL;
             }
             break; 

          case WIN_IC:
            /* if this window was created with WIN_USE_IM false
               then WIN_IC should not be set */
            if (win->win_use_im)
                win->xic = (XIC) attrs[1];
            break;
#endif

	  case WIN_ADD_DROP_ITEM:
	    win_add_drop_item(win, (Xv_opaque)attrs[1]);
	    break;

	  case WIN_DELETE_DROP_ITEM:
	    (void) win_delete_drop_item(win, (Xv_opaque)attrs[1],
	    		   	        Win_Drop_Site);
	    break;

	  case WIN_ADD_DROP_INTEREST:
	     win_add_drop_interest(win, (Xv_opaque)attrs[1]);
 	     break;

	  case WIN_DELETE_DROP_INTEREST:
	     win_delete_drop_item(win, (Xv_opaque)attrs[1],
				      Win_Drop_Interest);
 	     break;

	  default:
#ifdef OW_I18N
	    if (attrs[0])
#endif
	    xv_check_bad_attr(&xv_window_pkg, attrs[0]);
	    /* BUG: should we reuturn attrs[0] here? */
	    break;

	}
    }

    if (win_attrs_mask != 0) {
	XChangeWindowAttributes(xv_display(info), xv_xid(info),
		win_attrs_mask, &win_attrs);
    }

    if (win_cols) {
	new_rect.r_width = (win_cols * (actual_column_width(win) +
			    win->column_gap)) + win->left_margin +
			    win->right_margin;
	win->rect_info |= WIN_WIDTH_SET;
    }
    if (win_rows) {
	new_rect.r_height = (win_rows * (actual_row_height(win) +
					 win->row_gap)) + win->top_margin +
							  win->bottom_margin;
	win->rect_info |= WIN_HEIGHT_SET;
    }
    (void) update_rect(win, &old_rect, &new_rect);

    return (error);
}

/*
 * Sync new_rect with old_rect. If r_left of old_rect is EMPTY_VALUE, get the
 * latest rect using xv_get(). Any field in new_rect with EMPTY_VALUE will be
 * replaced with old_rect's value.
 */
Pkg_private void
sync_rect(win, old_rect, new_rect)
    Window_info    *win;
    Rect           *old_rect;
    Rect           *new_rect;
{
    if (old_rect->r_left == EMPTY_VALUE) {
	    *old_rect = *(Rect *) xv_get(WIN_PUBLIC(win), WIN_RECT);
    }
    if (new_rect->r_left == EMPTY_VALUE)
	new_rect->r_left = old_rect->r_left;

    if (new_rect->r_top == EMPTY_VALUE)
	new_rect->r_top = old_rect->r_top;

    if (new_rect->r_width == EMPTY_VALUE)
	new_rect->r_width = old_rect->r_width;

    if (new_rect->r_height == EMPTY_VALUE)
	new_rect->r_height = old_rect->r_height;
}

/*
 * Set the rect on win to new_rect if changed.
 */
static void
update_rect(win, old_rect, new_rect)
    Window_info    *win;
    Rect           *old_rect;
    Rect           *new_rect;
{
    sync_rect(win, old_rect, new_rect);

    if (!rect_equal(old_rect, new_rect)) {
	win_appeal_to_owner(TRUE, win, (Xv_opaque) WIN_ADJUST_RECT,
			    (Xv_opaque) new_rect);
	*old_rect = *new_rect;
    }
}


static void
set_mask_bit(mask, value)
    register Inputmask *mask;
    Window_input_event value;
{
    int             i;

    switch (value) {

      case WIN_NO_EVENTS:
	(void) input_imnull(mask);
	break;

      case WIN_MOUSE_BUTTONS:
	for (i = 1; i <= (BUT_LAST - BUT_FIRST); i++)
	    win_setinputcodebit(mask, BUT(i));
	mask->im_flags |= IM_NEGEVENT;
	break;

      case WIN_LEFT_KEYS:
	for (i = 1; i < 16; i++)
	    win_setinputcodebit(mask, KEY_LEFT(i));
	break;

      case WIN_TOP_KEYS:
	for (i = 1; i < 16; i++)
	    win_setinputcodebit(mask, KEY_TOP(i));
	break;

      case WIN_RIGHT_KEYS:
	for (i = 1; i < 16; i++)
	    win_setinputcodebit(mask, KEY_RIGHT(i));
	break;

      case WIN_ASCII_EVENTS:
	mask->im_flags |= IM_ASCII;
	mask->im_flags |= IM_META;
	break;

      case WIN_UP_ASCII_EVENTS:
	mask->im_flags |= IM_NEGASCII;
	mask->im_flags |= IM_NEGMETA;
	break;

      case WIN_META_EVENTS:
	mask->im_flags |= IM_META;
	break;

      case WIN_UP_META_EVENTS:
	mask->im_flags |= IM_NEGMETA;
	break;

      case WIN_UP_EVENTS:
	mask->im_flags |= IM_NEGEVENT;
	break;

      case WIN_IN_TRANSIT_EVENTS:
	mask->im_flags |= IM_INTRANSIT;
	break;

      case WIN_SUNVIEW_FUNCTION_KEYS:
      case WIN_EDIT_KEYS:
      case WIN_MOTION_KEYS:
      case WIN_TEXT_KEYS:
	/*
	 * BUG:  We should do a better job than this work around This a hack,
	 * we turn on key left 1 to make sure we get key press events.
	 */
	win_setinputcodebit(mask, KEY_LEFT(1));
	break;

      default:
	if (isworkstationdevid((int) value))
	    win_setinputcodebit(mask, (int) value);
	else {
	    /*
	     * BUG:  We should do a better job than this work around This a
	     * hack, we turn on key left 1 to make sure we get key press
	     * events.
	     */
	    win_setinputcodebit(mask, KEY_LEFT(1));
	}

	break;
    }
}


static void
unset_mask_bit(mask, value, win_public)
    register Inputmask *mask;
    Window_input_event value;
    Xv_Window       win_public;
{
    int             i;

    switch (value) {
      case WIN_NO_EVENTS:
	(void) input_imnull(mask);
	break;

      case WIN_MOUSE_BUTTONS:
	for (i = 1; i <= (BUT_LAST - BUT_FIRST); i++)
	    win_unsetinputcodebit(mask, BUT(i));
	mask->im_flags &= ~IM_NEGEVENT;
	break;

      case WIN_ASCII_EVENTS:
	mask->im_flags &= ~IM_ASCII;
	mask->im_flags &= ~IM_META;
	break;

      case WIN_UP_ASCII_EVENTS:
	mask->im_flags &= ~IM_NEGASCII;
	mask->im_flags &= ~IM_NEGMETA;
	break;

      case WIN_META_EVENTS:
	mask->im_flags &= ~IM_META;
	break;

      case WIN_UP_META_EVENTS:
	mask->im_flags &= ~IM_NEGMETA;
	break;

      case WIN_UP_EVENTS:
	mask->im_flags &= ~IM_NEGEVENT;
	break;

      case WIN_IN_TRANSIT_EVENTS:
	mask->im_flags &= ~IM_INTRANSIT;
	break;

      default:
	if (isworkstationdevid((int) value))
	    win_unsetinputcodebit(mask, (int) value);
	else
	    win_keymap_unset_smask(win_public, (u_short) value);

	break;
    }
}


static void
win_map(win, flag)
    Window_info    *win;
    int             flag;
{
    if (win->map != flag) {
	win->map = flag;

	/* Acts as initialize value prior to object's existence */
	if (!win->created)
	    return;

	(void) win_appeal_to_owner(FALSE, win,
			 (Xv_opaque) ((win->map) ? WIN_INSERT : WIN_REMOVE),
				   (Xv_opaque) flag);
    }
}

static void
window_set_event_mask(window, im, win_attrs, win_attrs_mask)
    Xv_Window       window;
    Inputmask      *im;
    XSetWindowAttributes    *win_attrs;
    unsigned long           *win_attrs_mask;
{
    Window_info    *win = WIN_PRIVATE(window);
    unsigned int    xevent_mask = win_im_to_xmask(window, im);

    if (xevent_mask != win->xmask) {
        window_grab_selectbutton(window, win->xmask, xevent_mask, im);
	win->xmask = xevent_mask;
	(*win_attrs_mask) |= CWEventMask;
	win_attrs->event_mask = xevent_mask;
    }
}

/*
 * Do a passive grab on the select button if the window is interested in kbd.
 * This eliminates the race condition of clicking in a new window and typing
 * characters before the focus actually changes
 */
static void
window_grab_selectbutton(window, oldmask, newmask, newim)
    Xv_Window       window;
    unsigned long   oldmask, newmask;
    Inputmask	   *newim;
{
    Xv_Drawable_info *info;
    unsigned long     newxmask;
    unsigned long     grab_button_mask;
    Bool	      kbd_use = False;

    if (xv_get(window, WIN_IS_IN_FULLSCREEN_MODE))
	return;

    if (newim)
	kbd_use = (Bool) win_getinputcodebit(newim, KBD_USE);

    DRAWABLE_INFO_MACRO(window, info);

    if ((newmask & FocusChangeMask) && (kbd_use) && (!fullscreendebug)) {
	newxmask = newmask;
	grab_button_mask = ButtonPressMask | ButtonReleaseMask |
	    (newxmask & EnterWindowMask) |
	    (newxmask & LeaveWindowMask);
	if (newxmask & ButtonMotionMask)
	    grab_button_mask |= Button1MotionMask;
	XGrabButton(xv_display(info), Button1, None, xv_xid(info), FALSE,
		  grab_button_mask, GrabModeSync, GrabModeSync, None, None);
    } else if (oldmask & FocusChangeMask)
	XUngrabButton(xv_display(info), Button1, None, xv_xid(info));
}

Xv_private void
window_release_selectbutton(window, event)
    Xv_Window       window;
    Event          *event;
{
    Window_info	     *win = WIN_PRIVATE(window);

    if (xv_get(window, WIN_IS_IN_FULLSCREEN_MODE))
	return;

    if (event_action(event) == ACTION_SELECT &&
	event_is_down(event) &&
	(win->xmask & FocusChangeMask) &&
	(!fullscreendebug)) {
        Xv_Drawable_info *info;

	DRAWABLE_INFO_MACRO(window, info);
	XAllowEvents(xv_display(info), AsyncBoth, CurrentTime);
	XFlush(xv_display(info));
    }
}

Xv_private void
window_set_softkey_labels(info,string)
   Xv_Drawable_info 	*info;
   char 		*string;
{

	XTextProperty	*text_prop;
        Xv_server       server;
	Atom		label_prop;


	text_prop = (XTextProperty *) xv_malloc(sizeof(XTextProperty));
        server    = (Xv_server) xv_server(info);
	label_prop = xv_get(server,SERVER_ATOM,"_OL_LABEL_HOLDER");

	text_prop->value     = (unsigned char *) string;
	text_prop->encoding  = XA_STRING;
	text_prop->format    = 8;
	text_prop->nitems    = strlen(string);

	XSetTextProperty(xv_display(info),xv_xid(info), text_prop,label_prop);
	XFree((char *)text_prop);

	return;

}

#ifdef OW_I18N
static void
window_set_imcb(window, xic)
    Xv_Window       window;
    XIC		    xic;
{

    Window_info    *win = WIN_PRIVATE(window);
    XIM             im;
    XIMStyles       style;
    XIMStyle        supported_styles = 0;
    int             i;
    Xv_object       server;

    server = XV_SERVER_FROM_WINDOW(window);

    /* Get the im form server in order to pass to XCreateIC */
    im = (XIM)xv_get(server, XV_IM);
    if (!im)
       return;
    XGetIMValues(im, XNQueryInputStyle, &style, NULL);

    for (i = 0; i < style.count_styles; i++)
       supported_styles |= style.supported_styles[i];

    if ((supported_styles & XIMPreeditCallbacks) &&
        (supported_styles & XIMStatusCallbacks) &&
        (supported_styles & XIMLookupCallbacks) &&
        (supported_styles & XIMAuxCallbacks)) {

        if (win->start_pecb_struct.callback ||
            win->draw_pecb_struct.callback ||
            win->done_pecb_struct.callback)
    		XSetICValue(xic, 
			XNPreeditStartCallback, &win->start_pecb_struct, 
		     	XNPreeditDrawCallback, &win->draw_pecb_struct,
		     	XNPreeditDoneCallback, &win->done_pecb_struct,
                        XNStatusStartCallback, &win->start_stcb_struct,
                        XNStatusDrawCallback, &win->draw_stcb_struct,
                        XNStatusDoneCallback, &win->done_stcb_struct,

                        XNLookupStartCallback, &win->start_luc_struct,
                        XNLookupDrawCallback, &win->draw_luc_struct,
                        XNLookupProcessCallback, &win->process_luc_struct,
                        XNLookupDoneCallback, &win->done_luc_struct,
 
                        XNAuxStartCallback, &win->start_aux_struct,
                        XNAuxDrawCallback, &win->draw_aux_struct,
			XNAuxDoneCallback, &win->done_aux_struct, 
			0);

   }
}
#endif /* OW_I18N */
