/*
 * $XConsortium: miwideline.h,v 1.7 90/11/19 15:16:41 keith Exp $
 *
 * Copyright 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

#include "mispans.h"

/* 
 * interface data to span-merging polygon filler
 */

typedef struct _SpanData {
    SpanGroup	fgGroup, bgGroup;
} SpanDataRec, *SpanDataPtr;

extern SpanDataPtr  miSetupSpanData ();

/*
 * Polygon edge description for integer wide-line routines
 */

typedef struct _PolyEdge {
    int	    height;	/* number of scanlines to process */
    int	    x;		/* starting x coordinate */
    int	    stepx;	/* fixed integral dx */
    int	    signdx;	/* variable dx sign */
    int	    e;		/* initial error term */
    int	    dy;
    int	    dx;
} PolyEdgeRec, *PolyEdgePtr;

#define SQSECANT 108.856472512142 /* 1/sin^2(11/2) - miter limit constant */

/*
 * types for general polygon routines
 */

typedef struct _PolyVertex {
    double  x, y;
} PolyVertexRec, *PolyVertexPtr;

typedef struct _PolySlope {
    int	    dx, dy;
    double  k;	    /* x0 * dy - y0 * dx */
} PolySlopeRec, *PolySlopePtr;

/*
 * Line face description for caps/joins
 */

typedef struct _LineFace {
    double  xa, ya;
    int	    dx, dy;
    int	    x, y;
    double  k;
} LineFaceRec, *LineFacePtr;

/*
 * macros for polygon fillers
 */

#define MIPOLYRELOADLEFT    if (!left_height && left_count) { \
	    	    	    	left_height = left->height; \
	    	    	    	left_x = left->x; \
	    	    	    	left_stepx = left->stepx; \
	    	    	    	left_signdx = left->signdx; \
	    	    	    	left_e = left->e; \
	    	    	    	left_dy = left->dy; \
	    	    	    	left_dx = left->dx; \
	    	    	    	--left_count; \
	    	    	    	++left; \
			    }

#define MIPOLYRELOADRIGHT   if (!right_height && right_count) { \
	    	    	    	right_height = right->height; \
	    	    	    	right_x = right->x; \
	    	    	    	right_stepx = right->stepx; \
	    	    	    	right_signdx = right->signdx; \
	    	    	    	right_e = right->e; \
	    	    	    	right_dy = right->dy; \
	    	    	    	right_dx = right->dx; \
	    	    	    	--right_count; \
	    	    	    	++right; \
			}

#define MIPOLYSTEPLEFT  left_x += left_stepx; \
    	    	    	left_e += left_dx; \
    	    	    	if (left_e > 0) \
    	    	    	{ \
	    	    	    left_x += left_signdx; \
	    	    	    left_e -= left_dy; \
    	    	    	}

#define MIPOLYSTEPRIGHT right_x += right_stepx; \
    	    	    	right_e += right_dx; \
    	    	    	if (right_e > 0) \
    	    	    	{ \
	    	    	    right_x += right_signdx; \
	    	    	    right_e -= right_dy; \
    	    	    	}

#ifdef NOINLINEICEIL
#define ICEIL(x) ((int)ceil(x))
#else
#ifdef __GNUC__
static __inline int ICEIL(x)
    double x;
{
    int _cTmp = x;
    return ((x == _cTmp) || (x < 0.0)) ? _cTmp : _cTmp+1;
}
#else
#define ICEIL(x) ((((x) == (_cTmp = (x))) || ((x) < 0.0)) ? _cTmp : _cTmp+1)
#define ICEILTEMPDECL static int _cTmp;
#endif
#endif
