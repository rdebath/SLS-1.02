#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)svr_data.c 1.13 91/09/14";
#endif
#endif
/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

#include <xview_private/svr_impl.h>

Xv_pkg          xv_server_pkg = {
    "Server",
    ATTR_PKG_SERVER,
    sizeof(Xv_server_struct),
    &xv_generic_pkg,
    server_init,
    server_set_avlist,
    server_get_attr,
    server_destroy,
    NULL			/* no find proc */
};
