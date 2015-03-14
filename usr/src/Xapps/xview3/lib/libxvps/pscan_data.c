#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)pscan_data.c 1.2 89/10/09";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */
#include "pscan_impl.h"

Xv_pkg          xv_pscanvas_pkg = {
    "PScanvas", ATTR_PKG_PSCANVAS,
    sizeof(Xv_pscanvas),
    &xv_openwin_pkg,
    xv_pscanvas_init,
    xv_pscanvas_set_avlist,
    xv_pscanvas_get_attr,
    xv_pscanvas_destroy,
    NULL			/* no find proc */
};

Xv_pkg		xv_psview_pkg = {
    "PSview", ATTR_PKG_PSCANVAS,
    sizeof(Xv_psview),
    &xv_window_pkg,
    xv_psview_init,
    xv_psview_set_avlist,
    xv_psview_get_attr,
    xv_psview_destroy,
    NULL			/* no find proc */
};
