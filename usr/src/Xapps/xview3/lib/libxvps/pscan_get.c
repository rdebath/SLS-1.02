#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)pscan_get.c 1.3 89/10/13";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */
#include "pscan_impl.h"
/*
#include <varargs.h>
*/

Pkg_private	Xv_opaque
xv_pscanvas_get_attr (pscanvas_public, status, attr, valist)
    PScanvas		pscanvas_public;
    int			*status;
    PScanvas_attr	attr;
    va_list		valist;
{
    PScanvas_info	*pscan = PSCANVAS_PRIVATE(pscanvas_public);

    switch (attr) {
    case PSCANVAS_SYNC:
	return (Xv_opaque) pscan->sync;
    case PSCANVAS_NEWSTOKEN:
	return (Xv_opaque) pscan->NeWS_canvas;
    case PSCANVAS_INPUT_PROC:
	return (Xv_opaque) pscan->input_proc;
    case PSCANVAS_REPAINT_PROC:
	return (Xv_opaque) pscan->repaint_proc;
    case PSCANVAS_RESIZE_PROC:
	return (Xv_opaque) pscan->resize_proc;
    case PSCANVAS_SCROLL_PROC:
	return (Xv_opaque) pscan->scroll_proc;
    case WIN_TYPE:
	return (Xv_opaque) PSCANVAS_TYPE;
    case OPENWIN_VIEW_CLASS:
	return (Xv_opaque) &xv_psview_pkg;
    default:
	xv_check_bad_attr(&xv_pscanvas_pkg, attr);
	*status = XV_ERROR;
	return (Xv_opaque) 0;
    }
}

Pkg_private	Xv_opaque
xv_psview_get_attr (psview_public, status, attr, valist)
    PSview		psview_public;
    int			*status;
    PScanvas_attr	attr;
    va_list		valist;
{
    PSview_info	*psview = PSVIEW_PRIVATE(psview_public);

    switch (attr) {
    case PSVIEW_PSCANVAS:
	return (Xv_opaque) psview->private_pscanvas->public_self;
    default:
	xv_check_bad_attr(&xv_pscanvas_pkg, attr);
	*status = XV_ERROR;
	return (Xv_opaque) 0;
    }
}
