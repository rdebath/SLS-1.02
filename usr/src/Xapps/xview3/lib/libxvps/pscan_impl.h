#ident "@(#)pscan_impl.h	1.17 11/29/90 Copyright 1990 Sun Microsystems, Inc."

#ifndef pscanvas_impl_defined
#define pscanvas_impl_defined

#define NeWStoken int
#ifndef FILE
#include <stdio.h>
#endif FILE
#include <xview/pkg.h>
#include <xview/attrol.h>
#include <xview_private/portable.h>
/*
#include "pscan_ps.h"
*/
#include "pscanvas.h"
#include <X11/Xlib.h>
#ifndef PSMACROS_H
#include <NeWS/psmacros.h>		/* standard cdef macros */
#endif	PSMACROS_H

/* Tag for CPS */
#define VALUETAG 2006

/* flag values */
#define PSCANVAS_X_PAINT	0x0001

#define PSCANVAS_PRIVATE(nc)	XV_PRIVATE(PScanvas_info, Xv_pscanvas, nc)
#define PSCANVAS_PUBLIC(nc)	XV_PUBLIC(nc)

#define PSVIEW_PRIVATE(nv)	XV_PRIVATE(PSview_info, Xv_psview, nv)
#define PSVIEW_PUBLIC(nv)	XV_PUBLIC(nv)

typedef void	(*Function)();

typedef struct pscanvas_info {
    PScanvas		public_self;	/* back pointer to object */
    int 		sync;
    unsigned int	flags;
    Function		input_proc;
    Function		resize_proc;
    Function		repaint_proc;
    Function		scroll_proc;
    NeWStoken		NeWS_canvas;
    NeWStoken		framebuffer;
} PScanvas_info;

typedef struct psview_info {
    PSview		public_self;	/* back pointer to object */
    PScanvas_info	*private_pscanvas;
    int			r_height;	/* for comparison on resize */
} PSview_info;

Pkg_private int xv_pscanvas_init();
Pkg_private int xv_pscanvas_destroy();
Pkg_private Xv_opaque xv_pscanvas_set_avlist();
Pkg_private Xv_opaque xv_pscanvas_get_attr();

Pkg_private int xv_psview_init();
Pkg_private int xv_psview_destroy();
Pkg_private Xv_opaque xv_psview_set_avlist();
Pkg_private Xv_opaque xv_psview_get_attr();

#endif	pscanvas_impl_defined
