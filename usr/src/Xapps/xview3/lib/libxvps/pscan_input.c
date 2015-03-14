#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)pscan_input.c 1.19 91/04/24";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

#include <stdio.h>
#include <xview/scrollbar.h>
#include <xview/xv_xrect.h>
#include <xview/rect.h>
#include <xview/rectlist.h>
#include <xview/notify.h>
#include "pscan_impl.h"

extern Rectlist	       *win_get_damage();
extern int		win_translate_xy();
static void     	pscan_inform_repaint();
static void     	pscan_inform_resize();
static void     	pscan_inform_scroll();
static Rectlist	       *pscan_get_damage(/* Xv_window view */);
Event		       *pscanvas_xlate_event_xy();

/*
 * Handle events for the view.  These events are passed on to the
 * pscanvas client WIN_EVENT_PROC.
 */

/* ARGSUSED */
Notify_value
psview_event(view_public, event, arg, type)
Xv_Window       view_public;
Event          *event;
Notify_arg      arg;
Notify_event_type type;
{
	PSview_info    *psview = PSVIEW_PRIVATE((PSview) view_public);
	PScanvas_info  *pscan = psview->private_pscanvas;
	PScanvas        pscan_public;
	char           *help_data;
	Notify_value    result;

	/* Pass on the event to the PScanvas client WIN_EVENT_PROC */
	result = notify_next_event_func(view_public,
					(Notify_event) event, arg, type);
	ps_gsave();

	switch (event_action(event)) {
	case WIN_REPAINT:

		/*
		 * Paint handles setcanvas separately  since it is
		 * responsible for setting NeWS_canvas initially
		 */
		pscan_inform_repaint(pscan, view_public);
		break;

	case WIN_RESIZE:
		if (pscan->NeWS_canvas >= 0)
			ps_setcanvas(pscan->NeWS_canvas);

		/*
		 * scrollbars have already been updated, and the view resized
		 */
		pscan_inform_resize(pscan, view_public, event);
		break;

	case ACTION_HELP:
		if (event_is_down(event)) {
			pscan_public = PSCANVAS_PUBLIC(pscan);
			if ((Attr_pkg) xv_get(pscan_public, WIN_TYPE) ==
			    ATTR_PKG_PSCANVAS) {
				help_data = (char *) xv_get(pscan_public,
							    XV_HELP_DATA);
				if (help_data)
					xv_help_show(view_public, help_data,
						     event);
			}
		}
		return NOTIFY_DONE;

	case SCROLLBAR_REQUEST:
		if (pscan->NeWS_canvas >= 0)
			ps_setcanvas(pscan->NeWS_canvas);
		pscan_inform_scroll(pscan, view_public, (Scrollbar) arg);
		break;

	default:
		break;
	}

	ps_grestore();
	ps_flush_PostScript();
	return (result);
}

static Rectlist *
pscan_get_damage(view)
Xv_Window	view;
{
	static Rectlist	damage;
	Rectlist       *win_damage;

	if (!(win_damage = win_get_damage(view))) {
		damage = rl_null;
		return(NULL);
	}

	rl_copy(win_damage, &damage);

	return(&damage);
}

static void
pscan_inform_resize(pscan, view_public, event)
register PScanvas_info *pscan;
Xv_Window		view_public;
Event		       *event;
{
	PSview_info    *psview = PSVIEW_PRIVATE((PSview) view_public);

	if (!event->ie_xevent) {
		fprintf(stderr, "WIN_RESIZE event with no ie_xevent\n");
		return;
	}

	if (event->ie_xevent->type == ConfigureNotify) {

#ifdef	DEBUG
		fprintf(stderr, "xconfigure: %d %d %d %d\n",
			event->ie_xevent->xconfigure.x,
			event->ie_xevent->xconfigure.y,
			event->ie_xevent->xconfigure.width,
			event->ie_xevent->xconfigure.height);
#endif	/* DEBUG */
	}
	else {
		fprintf(stderr, "WIN_RESIZE event with type %d ie_xevent\n",
			event->ie_xevent->type);
		return;
	}

#ifdef	DEBUG
	fprintf(stderr, "view size as of WIN_RESIZE: %d %d %d %d\n\n",
		(int) xv_get(view_public, XV_X),
		(int) xv_get(view_public, XV_Y),
		(int) xv_get(view_public, XV_WIDTH),
		(int) xv_get(view_public, XV_HEIGHT));
#endif	/* DEBUG */

	if (pscan->sync)
		XSync((Display *) XV_DISPLAY_FROM_WINDOW(view_public), 0);
	if (pscan->resize_proc) {
		(*pscan->resize_proc) (PSCANVAS_PUBLIC(pscan),
				       event->ie_xevent->xconfigure.width,
				       event->ie_xevent->xconfigure.height);
	}
	else {
		int		dy;

		/*
		 * REMIND: the following dy is empirical. I would have
		 * expected the correct dy to be psview->r_height - (int)
		 * xv_get(view_public, XV_HEIGHT);
		 * 
		 * REMIND: need to also get the xid, gsave, do
		 * pscanvas_xid(xid), reshape the canvas, and grestore.
		 */
		dy = -(psview->r_height -
		       (int) xv_get(view_public, XV_HEIGHT));
		psview->r_height = (int) xv_get(view_public, XV_HEIGHT);
		/* pscanvas_fix_translation(dy); */
	}
	if (pscan->sync)
		pscanvas_sync();
}

/*
 * tell the client to repaint the paint window.
 */
static void
pscan_inform_repaint(pscan, view)
PScanvas_info  *pscan;
Xv_Window	view;
{
	Rectlist       *damage	= pscan_get_damage(view);
	Display	       *dpy	= (Display *) XV_DISPLAY_FROM_WINDOW(view);
	Xv_xrectlist   *rp	= NULL;
	XID		xid	= (XID) xv_get(view, XV_XID);
	Xv_xrectlist	xrects;

#ifdef	DEBUG
	(void) fprintf(stderr, "view size as of WIN_REPAINT: %d %d %d %d\n\n",
		       (int) xv_get(view, XV_X),
		       (int) xv_get(view, XV_Y),
		       (int) xv_get(view, XV_WIDTH),
		       (int) xv_get(view, XV_HEIGHT));
#endif	/* DEBUG */

	/* If there is no repaint_proc, just return */
	if (!pscan->repaint_proc) {
		return;
	}

	/*
	 * Be sure that the NeWS_canvas token is set to point at the NeWS
	 * canvas
	 */
	if (pscan->NeWS_canvas == -1) {
		int		is_canvas;

		ps_token_from_xid(xid, &is_canvas);
		pscan->NeWS_canvas = is_canvas;
	}

	/*
	 * Only repaint if there really is a NeWScanvas
	 */
	if (pscan->NeWS_canvas >= 0) {
		ps_setcanvas(pscan->NeWS_canvas);
		pscanvas_flip();

		/*
		 * If there is no damage on the view, pass NULL xrectangle
		 * array and a count of zero to let the application know that
		 * there is no clipping.
		 */
		if (pscan->sync)
			XSync(dpy, 0);

		if (damage) {
			rp = &xrects;
			xrects.count =
				win_convert_to_x_rectlist(damage,
							  rp->rect_array,
							  XV_MAX_XRECTS);

			rl_free(damage);
		}

		/* call the client repaint_proc */
		(*pscan->repaint_proc) (PSCANVAS_PUBLIC(pscan),
					pscan->NeWS_canvas,
					dpy,
					xid,
					rp);
	}

	if (pscan->sync)
		pscanvas_sync();

	return;
}

/*
 * Scroll the window by adjusting the ctm. REMIND: how do we cause copyarea
 * and repaint? REMIND: documentation should emphasize that XView scrollbars
 * only know units of multiples of pixels, so non-pixel world coordinates may
 * result in some scrolling anomolies. REMIND: documentation should tell
 * users to set SCROLLBAR_OBJECT_LENGTH, SCROLLBAR_PIXELS_PER_UNIT,
 * SCROLLBAR_VIEW_LENGTH, and SCROLLBAR_VIEW_START explicitly when creating
 * the scrollbar.
 */
static void
pscan_inform_scroll(pscan, view, sb)
PScanvas_info  *pscan;
Xv_Window       view;
Scrollbar       sb;
{
	int		bbox[4];
	register int	cntr;
	Display	       *dpy = (Display *) XV_DISPLAY_FROM_WINDOW(view);
	int		dist;
	register Xv_xrectlist *rp = NULL;
	Scrollbar_setting sb_type;
	Xv_xrectlist	xrects;

	/*
	 * Calculate the scroll distance in scrollbar units (not) pixels. If
	 * dist == 0, then just return.
	 */
	dist = (int) xv_get(sb, SCROLLBAR_VIEW_START);

	dist -= (int) xv_get(sb, SCROLLBAR_LAST_VIEW_START);

#ifdef	DEBUG
	(void) fprintf(stderr, "dist\t\t\t= %d\n", dist);
#endif	/* DEBUG */

	if (!dist) {

#ifdef	DEBUG
		(void) fputs("\ndist = 0, returning\n", stderr);
#endif	/* DEBUG */

		return;
	}

	/* Get the scrollbar type (horizontal or vertical) */
	sb_type = (Scrollbar_setting) xv_get(sb, SCROLLBAR_DIRECTION);

	if (pscan->sync)
		XSync(dpy, 0);

	/* If there is a canvas scroll proc installed, call that ... */
	if (pscan->scroll_proc) {
		(*pscan->scroll_proc) (PSCANVAS_PUBLIC(pscan),
				       pscan->NeWS_canvas,
				       dpy,
				       xv_get(view, XV_XID),
				       sb_type,
				       dist);
		if (pscan->sync)
			pscanvas_sync();

		return;
	}

	/* Convert dist from scrollbar units to pixels */
	dist *= (int) xv_get(sb, SCROLLBAR_PIXELS_PER_UNIT);

#ifdef	DEBUG
	(void) fprintf(stderr, "dist (in pixels)\t= %d\n", dist);
#endif	/* DEBUG */

	ps_setcanvas(pscan->NeWS_canvas);
	ps_clear_cliprects();
	pscanvas_flip();

	/* Scroll the canvas, and set the clippath */
	if (sb_type == SCROLLBAR_VERTICAL) {
		pscanvas_scroll_by(0, dist);
	}
	else {
		pscanvas_scroll_by(dist, 0);
	}

	rp = &xrects;

	ps_flush_PostScript();

	/*
	 * Get the clippath rectangle from the canvas.	Do this backwards
	 * because the stack contains: x y w h
	 */
	for (cntr = 3; cntr >= 0; cntr--) {
		if (!pscanvas_getint(VALUETAG, &bbox[cntr])) {

#ifdef	DEBUG
			(void) fprintf(stderr, "%s:%s: %s failed\n",
				       "pscan_input.c",
				       "pscan_inform_scroll",
				       "pscanvas_getint()");
#endif	/* DEBUG */

			break;
		}
	}

	/* Load up the xrect structure */
	if (rp) {
		/* Normalize the clip rectangle to X coordinates */
		if (bbox[3] < 0) {
			bbox[1] += bbox[3];
			bbox[3] = -bbox[3];
		}
		if (bbox[2] < 0) {
			bbox[0] += bbox[2];
			bbox[2] = -bbox[2];
		}

		/* Fill the first rectangle in the Xv_xrectlist */
		rp->rect_array[0].x		= (short) bbox[0];
		rp->rect_array[0].y		= (short) (bbox[1] - 1);
		rp->rect_array[0].width		= (u_short) bbox[2];
		rp->rect_array[0].height	= (u_short) (bbox[3] + 1);

		/* Set the count in the Xv_xrectlist */
		rp->count = 1;

#ifdef	DEBUG
		(void) fprintf(stderr,
			       "%s: (x y w h)\t= %d %d %d %d\n",
			       "pscan_input.c:pscan_inform_scroll",
			       (int) rp->rect_array[0].x,
			       (int) rp->rect_array[0].y,
			       (u_int) rp->rect_array[0].width,
			       (u_int) rp->rect_array[0].height);
#endif	/* DEBUG */
	}

	/*
	 * If there is a canvas repaint proc, call it to repair any damage
	 */
	if (pscan->repaint_proc) {
		(*pscan->repaint_proc) (PSCANVAS_PUBLIC(pscan),
					pscan->NeWS_canvas,
					dpy,
					(XID) xv_get(view, XV_XID),
					rp);
	}

	if (pscan->sync)
		pscanvas_sync();

	return;
}



/*
 * Translate the event coordinates from the view window to the underlying
 * canvas
 */
Event *
pscanvas_xlate_event_xy(pscan, event)
Xv_Window       pscan;
register Event *event;
{
	int		dist_x	= 0;
	int		dist_y	= 0;
	static Event	ret;
	Scrollbar	sb;

	bcopy((caddr_t) event, (caddr_t) &ret, sizeof(Event));

	if ((sb = (Scrollbar) xv_get(pscan, WIN_VERTICAL_SCROLLBAR))) {
		dist_y	= (int) xv_get(sb, SCROLLBAR_VIEW_START);
		dist_y *= (int) xv_get(sb, SCROLLBAR_PIXELS_PER_UNIT);
		event_set_y(&ret, event_y(event) + dist_y);
	}

	if ((sb = (Scrollbar) xv_get(pscan, WIN_HORIZONTAL_SCROLLBAR))) {
		dist_x	= (int) xv_get(sb, SCROLLBAR_VIEW_START);
		dist_x *= (int) xv_get(sb, SCROLLBAR_PIXELS_PER_UNIT);
		event_set_x(&ret, event_x(event) + dist_x);
	}

	return(&ret);
}
