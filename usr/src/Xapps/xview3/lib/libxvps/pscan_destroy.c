
#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)pscan_destroy.c 1.1 89/10/06";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */
#include "pscan_impl.h"
#include <xview/notify.h>

Pkg_private int
xv_pscanvas_destroy (pscanvas_public, status)
    PScanvas		pscanvas_public;
    Destroy_status	status;
{
    PScanvas_info	*pscan = PSCANVAS_PRIVATE(pscanvas_public);
    extern int		n_pscans;

    if (status == DESTROY_CLEANUP) {
	if (--n_pscans == 0)
	    ps_close_PostScript();
	free ((char *) pscan);
    }

    return XV_OK;
}

Pkg_private int
xv_psview_destroy (psview_public, status)
    PSview		psview_public;
    Destroy_status	status;
{
    PSview_info	*psview = PSVIEW_PRIVATE(psview_public);

    if (status == DESTROY_CLEANUP) {
	free ((char *) psview);
    }

    return XV_OK;
}
