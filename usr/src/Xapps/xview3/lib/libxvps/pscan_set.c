#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)pscan_set.c 1.20 90/11/28";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/xv_xrect.h>
#include "pscan_impl.h"

extern Display *xv_default_display;

static void
xv_pscanvas_ps_proc(client, fd)
Notify_client	client;
int		fd;
{
	PScanvas	pscanvas_public = (PScanvas) client;
	PScanvas_info  *pscan = PSCANVAS_PRIVATE(pscanvas_public);

	if (pscan->sync)
		XSync((Display *) xv_default_display, 0);

	pscan->input_proc(client, fd);

	if (pscan->sync)
		pscanvas_sync();
}


Pkg_private	Xv_opaque
xv_pscanvas_set_avlist (pscanvas_public, avlist)
PScanvas	pscanvas_public;
Attr_avlist	avlist;
{
	PScanvas_attr			attr;
	register int			cntr;
	PScanvas_info		       *pscan =
	PSCANVAS_PRIVATE(pscanvas_public);
	Xv_Window			viewzero;
	static XSetWindowAttributes	wattr;
	Xv_xrectlist		       *xrects;
	register XRectangle	       *rptr;
	Window				xid;


	for (attr = (PScanvas_attr) avlist[0]; attr;
	     avlist = attr_next(avlist), attr = (PScanvas_attr) avlist[0]) {
		switch (attr) {
		case PSCANVAS_SYNC:
			if ((int) avlist[1])
				pscan->sync = TRUE;
			else
				pscan->sync = FALSE;
			break;
		case PSCANVAS_INPUT_PROC:
			pscan->input_proc = (void (*) ()) avlist[1];
			notify_set_input_func( (Notify_client) pscanvas_public, 
					      (Notify_func) xv_pscanvas_ps_proc,
					      psio_fileno(PostScript));

			/* REMIND */
			/* make input possible */
			break;
		case PSCANVAS_REPAINT_PROC:
			pscan->repaint_proc = (void (*) ()) avlist[1];
			break;
		case PSCANVAS_RESIZE_PROC:
			pscan->resize_proc = (void (*) ()) avlist[1];
			break;
		case PSCANVAS_SCROLL_PROC:
			pscan->scroll_proc = (void (*) ()) avlist[1];
			break;
		case PSCANVAS_CLIPRECTS:

			/* If there is no NeWS_canvas, bail out */
			if (pscan->NeWS_canvas == -1) {
				break;
			}

			/*
			 * Set the canvas, and clear the current path
			 */
			ps_setcanvas(pscan->NeWS_canvas);
			ps_newpath();

			/* If there is no rectlist, reset the clip-path ... */
			if (!(xrects = (Xv_xrectlist *) avlist[1])) {
				ps_clear_cliprects();
			}
			else {
				/* ... otherwise, set the clip-path */
				rptr = xrects->rect_array;
				for (cntr = 0; cntr < xrects->count; cntr++) {

					/*
					 * Add 2 pixels to width and height,
					 * seems to be a bug in the xrects sent
					 */
					rptr[cntr].width += 2;
					rptr[cntr].height += 2;
					pscanvas_rectpath(rptr[cntr].x,
							  rptr[cntr].y,
							  rptr[cntr].width,
							  rptr[cntr].height);
				}
				/* Set the clip-path to be the current path */
				ps_clipcanvas();
			}


			/*
			 * Flip the canvas after a clipcanvas ... clipcanvas
			 * resets the coordinates
			 */
			pscanvas_flip();
			break;

		case XV_END_CREATE:
			break;

		default:
			xv_check_bad_attr(&xv_pscanvas_pkg, attr);
			break;
		}
	}

	return ((Xv_opaque) XV_OK);
}

Pkg_private	Xv_opaque
xv_psview_set_avlist (psview_public, avlist)
    PSview		psview_public;
    Attr_avlist		avlist;
{
    PSview_info	*psview = PSVIEW_PRIVATE(psview_public);
    PScanvas_info  *pscan = psview->private_pscanvas;
    PScanvas_attr	attr;
    static XSetWindowAttributes wattr;
    Window		xid;
    NeWStoken		Ncanvas;
   

    for (attr = (PScanvas_attr) avlist[0]; attr;
         avlist = attr_next(avlist), attr = (PScanvas_attr) avlist[0]) {
        switch (attr) {
	case XV_END_CREATE:
	    xv_set(psview_public,
		WIN_CONSUME_EVENTS, WIN_NO_EVENTS,
			LOC_WINENTER, LOC_WINEXIT, WIN_RESIZE, WIN_REPAINT, 0,
	    0);
	    /* Synchronize here to make sure all X components exist */
	    XSync((Display *)xv_default_display, 0);
	    if (PostScript) {
	      pscanvas_sync();
	      ps_flush_PostScript();
	    }
	    /* Make sure the window really exists on the server */
	    xid = (Window) xv_get(psview_public, XV_XID);
	    wattr.bit_gravity = ForgetGravity;
	    wattr.background_pixmap = None;
	    /*
	     * REMIND
	     * xv_default_display => XV_DISPLAY_FROM_WINDOW(psview_public)
	     */
	    XChangeWindowAttributes(xv_default_display, xid,
		CWBitGravity|CWBackPixmap, &wattr);


	    /* This is where the NEWSTOKEN is set for the canvas */
	    ps_token_from_xid(xid,&Ncanvas);
	    if (Ncanvas != -1) {
	      ps_setcanvas(Ncanvas);
	      pscan->NeWS_canvas=Ncanvas;
/*	      pscanvas_init_transform();*/
	    }
	    ps_flush_PostScript();

	    /* Save current shape.  */
	    psview->r_height = (int) xv_get(psview_public, XV_HEIGHT);
	    break;
        default:
            xv_check_bad_attr(&xv_pscanvas_pkg, attr);
            break;

	}
    }
    return ((Xv_opaque) XV_OK);
}
