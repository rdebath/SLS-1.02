/*
 * $XConsortium: miwidearc.c,v 1.6 91/08/23 12:14:41 gildea Exp $
 *
 * Copyright 1990 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

#include <math.h>
#include "X.h"
#include "Xprotostr.h"
#include "misc.h"
#include "gcstruct.h"
#include "scrnintstr.h"
#include "pixmapstr.h"
#include "windowstr.h"
#include "mi.h"
#include "mifillarc.h"
#include "miwideline.h"

#if (defined(SVR4) || defined(SYSV) && defined(SYSV386)) && __STDC__
extern double hypot(double, double);
#endif

typedef struct _dpoint {
    double	x, y;
} DPointRec, *DPointPtr;

#define m_abs(x)    ((x) < 0 ? -(x) : (x))

#define HALFCIRCLE (180 * 64)

#define DELTA	    1e-4

#define PointsCoincide(p1,p2)	(m_abs ((p1)->x - (p2)->x) < DELTA && \
				 m_abs ((p1)->y - (p2)->y) < DELTA)

#define FullAngle(a)	(m_abs(a) >= FULLCIRCLE)
#define TruncAngle2(a2)	((a2) > FULLCIRCLE ? FULLCIRCLE : \
			    ((a2) < -FULLCIRCLE ? -FULLCIRCLE : (a2)))

#ifndef M_PI
#define M_PI	3.14159265358979323846264338327950288419716993937510582097
#endif

#define Dsin(d)		sin((double)d*(M_PI/11520.0))
#define Dcos(d)		cos((double)d*(M_PI/11520.0))

#define AllocateSpans(pDraw,pGC,nspans,spanData,spanRec,points,widths,pixel,oldPixel,retval) {\
    if (!spanData) \
    { \
    	points = (DDXPointPtr)ALLOCATE_LOCAL(sizeof(DDXPointRec) * (nspans)); \
    	if (!points) \
	    return retval; \
    	widths = (int *)ALLOCATE_LOCAL(sizeof(int) * (nspans)); \
    	if (!widths) \
    	{ \
	    DEALLOCATE_LOCAL(points); \
	    return retval; \
    	} \
    	oldPixel = pGC->fgPixel; \
    	if (pixel != oldPixel) \
    	{ \
    	    DoChangeGC (pGC, GCForeground, (XID *)&pixel, FALSE); \
    	    ValidateGC (pDraw, pGC); \
    	} \
    } \
    else \
    { \
	spanRec.points = (DDXPointPtr) xalloc (sizeof(DDXPointRec) * nspans); \
	if (!spanRec.points) \
	    return retval; \
	spanRec.widths = (int *) xalloc (nspans * sizeof (int)); \
	if (!spanRec.widths) \
	{ \
	    xfree (spanRec.points); \
	    return retval; \
	} \
	points = spanRec.points; \
	widths = spanRec.widths; \
    } \
}

#define DeallocateSpans(pDraw,pGC,nspans,spanData,spanRec,points,widths,pixel,oldPixel) {\
    if (!spanData) \
    { \
    	(*pGC->ops->FillSpans)(pDraw, pGC, nspans, points, widths, FALSE); \
    	DEALLOCATE_LOCAL(widths); \
    	DEALLOCATE_LOCAL(points); \
    	if (pixel != oldPixel) \
    	{ \
	    DoChangeGC (pGC, GCForeground, (XID *)&oldPixel, FALSE); \
	    ValidateGC (pDraw, pGC); \
    	} \
    } \
    else \
    { \
	SpanGroup   *group; \
	if (pixel == pGC->fgPixel) \
	    group = &spanData->fgGroup; \
	else \
	    group = &spanData->bgGroup; \
	spanRec.count = nspans; \
	miAppendSpans (group, &spanRec); \
    } \
}


typedef struct _ellipseWalk {
    int	    x;
    int	    topy;
    int	    vx, cvx;
    int	    vy, cvy;
    int	    lx, rx;
    int	    err;
    int	    xoff;
    int	    nobottom;
} EllipseWalkRec, *EllipseWalkPtr;

#define StepEllipse(e,y) {\
    while ((e)->err < 0) {\
	(e)->err += (e)->vx; \
	(e)->vx  += (e)->cvx; \
	(e)->x++; \
    } \
    (e)->rx = (e)->x; \
    (e)->lx = (e)->xoff - (e)->x; \
    (e)->nobottom = 0; \
    if ((e)->err == 0) { \
	(e)->lx--; \
	if (y == (e)->topy) { \
	    (e)->rx++; \
	    (e)->nobottom = 1; \
	} \
    } \
    (e)->err += (e)->vy; \
    (e)->vy += (e)->cvy; \
}

#define StepEllipseV(_err,_vx,_cvx,_vy,_cvy,_x,_xoff,_rx,_lx,_topy,_nobottom) {\
    while (_err < 0) {\
	_err += _vx; \
	_vx  += _cvx; \
	_x++; \
    } \
    _rx = _x; \
    _lx = _xoff - _x; \
    _nobottom = 0; \
    if (_err == 0) { \
	_lx--; \
	if (ey == _topy) { \
	    _rx++; \
	    _nobottom = 1; \
	} \
    } \
    _err += _vy; \
    _vy += _cvy; \
}

#define FetchEllipse(e,_err,_vx,_cvx,_vy,_cvy,_x,_xoff,_rx,_lx,_topy,_nobottom) {\
    _err = (e)->err; \
    _vx = (e)->vx; \
    _cvx = (e)->cvx; \
    _vy = (e)->vy; \
    _cvy = (e)->cvy; \
    _x = (e)->x; \
    _xoff = (e)->xoff; \
    _rx = (e)->rx; \
    _lx = (e)->lx; \
    _topy = (e)->topy; \
    _nobottom = 0; \
}

typedef struct _faceWalk {
    int	    x;
    int	    topy;
    int	    err;
    int	    dx;
    int	    dy;
    int	    jumpx;
    int	    stepx;
    int	    left;
    int	    top;
} FaceWalkRec, *FaceWalkPtr;

typedef struct _slice {
    FaceWalkRec	edge1, edge2;
    Bool	hasTop, hasBottom;
    Bool	bothTop, bothBottom;
    DPointRec	start, end;
} SliceRec, *SlicePtr;

#define StepFace(f) {\
    if (((f)->err -= (f)->dx) < 0) {\
	(f)->err += (f)->dy; \
	(f)->x += (f)->stepx; \
    } \
    (f)->x += (f)->jumpx; \
}

#define UpperClip(xl,xr,y,slice) {\
    int	l, r, t; \
    if (slice->hasTop) { \
	l = (xl); \
	r = (xr); \
	if (slice->bothTop) { \
	    t = r; \
	    if (slice->edge2.x < t) \
		t = slice->edge2.x; \
	    if (l < t) \
		FillSpan(l,t,y); \
	    if (slice->edge1.x > l) \
		l = slice->edge1.x; \
	} else { \
	    if (slice->edge1.top && slice->edge1.x > l) \
	    	l = slice->edge1.x; \
	    if (slice->edge2.top && slice->edge2.x < r) \
	    	r = slice->edge2.x; \
	} \
	if (l < r) \
	    FillSpan(l,r,y);\
    } \
}
    
#define LowerClip(xl,xr,y,slice) {\
    int	l, r, t; \
    if (slice->hasBottom) { \
	l = (xl); \
	r = (xr); \
	if (slice->bothBottom) { \
	    t = r; \
	    if (slice->edge1.x < t) \
		t = slice->edge1.x; \
	    if (l < t) \
		FillSpan (l,t,y); \
	    if (slice->edge2.x > l) \
		l = slice->edge2.x; \
	} else { \
	    if (!slice->edge2.top && slice->edge2.x > l) \
	    	l = slice->edge2.x; \
	    if (!slice->edge1.top && slice->edge1.x < r) \
	    	r = slice->edge1.x; \
	} \
	if (l < r) \
	    FillSpan(l,r,y);\
    } \
}
    
# define DASH_MAP_SIZE	91

# define dashIndexToAngle(di)	((((double) (di)) * 90.0) / ((double) DASH_MAP_SIZE - 1))
# define xAngleToDashIndex(xa)	((((long) (xa)) * (DASH_MAP_SIZE - 1)) / (90 * 64))
# define dashIndexToXAngle(di)	((((long) (di)) * (90 * 64)) / (DASH_MAP_SIZE - 1))
# define dashXAngleStep	(((double) (90 * 64)) / ((double) (DASH_MAP_SIZE - 1)))

typedef struct _dashMap {
    Bool	ready;
    double	map[DASH_MAP_SIZE];
} DashMapRec, *DashMapPtr;

static void computeDashMap ();
static int  computeAngleFromPath ();
static double	angleToLength ();
static int	lengthToAngle ();

typedef struct _joinEllipse {
    double  x, y;
    double  x1, y1;
    double  dx1, dy1;
    double  x2, y2;
    double  dx2, dy2;
} JoinEllipseRec, *JoinEllipsePtr;

typedef struct _joinEllipseWalk {
    double  x, y;
    double  a, b;
} JoinEllipseWalkRec, *JoinEllipseWalkPtr;

static void
SetupFace(x,y,height,dx,dy,face,first)
    int		x, y;	    /* doubled center coordinates */
    int		height;	    /* doubled line height */
    int		dx, dy;	    /* dx, dy from ellipse center to outside edge */
    FaceWalkPtr	face;
    Bool	first;	    /* first angle */
{
    Bool	negative_dx, negative_dy;
    int		top_y;
    int		xady;
    int		top_x;
    Bool	left;

    negative_dx = FALSE;
    left = !first;
    if (dx < 0)
    {
	dx = -dx;
	negative_dx = TRUE;
    }
    negative_dy = FALSE;
    if (dy < 0)
    {
	dy = -dy;
	negative_dy = TRUE;
	left = !left;
    }
    if (dy != 0)
    {
	top_y = y - height;
	if (top_y & 1)
	    top_y++;

    	face->jumpx = dx / dy;
    	face->stepx = 1;
	/* xady at y is zero, so at top_y, xady is: */
	xady = (top_y - y) * dx;

    	if (negative_dx)
    	{
	    face->jumpx = -face->jumpx;
	    face->stepx = -1;
	    xady = -xady;
    	}
	xady = xady + x * dy;
	if (xady <= 0)
	    top_x = (xady / (dy << 1));	    
	else
	    top_x = ((xady + (dy << 1) - 1) / (dy << 1));

    	face->dx = (dx % dy) << 1;
    	face->dy = dy << 1;
	face->err = top_x * (dy << 1) - xady;
	if (negative_dx)
	    face->err = ((dy << 1) - 1) - face->err;

	face->x = top_x;
	face->topy = top_y >> 1;
	face->left = left;
	face->top = !negative_dy;
    }
    else
    {
	left = !negative_dx;
	face->dx = dx;
	face->dy = 0;
	face->jumpx = 0;
	face->stepx = 1;
	if (negative_dx)
	    face->stepx = -1;
	face->err = 0;
	face->topy = (y - height + 1) >> 1;
	face->x = left ? -65535 : 65535;
	face->left = left;
	if (first)
	    face->top = negative_dx;
	else
	    face->top = !negative_dx;
    }
}

static void
SetupSlice(arc,lw,angle1,angle2,slice,swap)
    xArc	*arc;
    int		lw;	/* line width */
    int		angle1, angle2;
    SlicePtr	slice;
    Bool	swap;
{
    int	    dx, dy;
    double  d_dx, d_dy;
    DPointPtr	start, end;
    int	    c_x, c_y;
    
    start = &slice->start;
    end = &slice->end;
    if (swap)
    {
	start = &slice->end;
	end = &slice->start;
    }
    c_x = (arc->x << 1) + arc->width;
    c_y = (arc->y << 1) + arc->height;

    miEllipseAngleToSlope (angle1, arc->width, arc->height, &dx, &dy, &d_dx, &d_dy);
    start->x = (c_x / 2.0) + d_dx;
    start->y = (c_y / 2.0) + d_dy;
    SetupFace(c_x, c_y, arc->height + lw, -dx, dy, &slice->edge2, TRUE);

    miEllipseAngleToSlope (angle2, arc->width, arc->height, &dx, &dy, &d_dx, &d_dy);
    end->x = (c_x / 2.0) + d_dx;
    end->y = (c_y / 2.0) + d_dy;
    SetupFace(c_x, c_y, arc->height + lw, -dx, dy, &slice->edge1, FALSE);
}

static void
miWideArcSliceSetup (arc, lw, angle1, spanAngle, slice)
    xArc	    *arc;
    int		    lw;
    int		    angle1, spanAngle;
    SliceRec	    *slice;
{
    int	    angle2;
    Bool    swap;

    if (angle1 < 0)
	angle1 = FULLCIRCLE - (-angle1 % FULLCIRCLE);
    if (angle1 >= FULLCIRCLE)
	angle1 = angle1 % FULLCIRCLE;
    if (spanAngle < -FULLCIRCLE)
	spanAngle = -FULLCIRCLE;
    else if (spanAngle > FULLCIRCLE)
	spanAngle = FULLCIRCLE;
    if (spanAngle < 0)
    {
	angle2 = angle1;
	angle1 = angle2 + spanAngle;
	if (angle1 < 0)
	    angle1 += FULLCIRCLE;
	spanAngle = -spanAngle;
	swap = TRUE;
    }
    else
    {
	angle2 = angle1 + spanAngle;
	if (angle2 >= FULLCIRCLE)
	    angle2 -= FULLCIRCLE;
	swap = FALSE;
    }
    if (spanAngle > HALFCIRCLE)
    {
	slice->hasTop = TRUE;
	slice->hasBottom = TRUE;
    } else {
	slice->hasTop = FALSE;
	slice->hasBottom = FALSE;
	if (angle1 == 0)
	    slice->hasTop = TRUE;
	else if (angle1 == HALFCIRCLE)
	    slice->hasBottom = TRUE;
	if (0 < angle1 && angle1 < HALFCIRCLE ||
		 0 < angle2 && angle2 < HALFCIRCLE)
	    slice->hasTop = TRUE;
	if (HALFCIRCLE < angle1 && angle1 < FULLCIRCLE ||
	    HALFCIRCLE < angle2 && angle2 < FULLCIRCLE)
	    slice->hasBottom = TRUE;
    }
    SetupSlice (arc, lw, angle1, angle2, slice, swap);
    slice->bothTop = FALSE;
    slice->bothBottom = FALSE;
    if (spanAngle > HALFCIRCLE && slice->edge1.top == slice->edge2.top &&
	angle1 != 0 && angle1 != HALFCIRCLE && angle2 != 0 && angle2 != HALFCIRCLE)
    {
	slice->bothTop = slice->edge1.top;
	slice->bothBottom = !slice->bothTop;
    }
}

static void
SetupEllipse (x, y, a, b, e)
    int		    x, y;   /* doubled coordinates of ellipse center */
    int		    a, b;   /* ellipse width/height */
    EllipseWalkPtr  e;
{
    int	    a2, b2;
    int	    cx, cy;

    if (a == 0 || b == 0)
    {
	e->topy = 65536;
	return;
    }

    a2 = a * a;
    b2 = b * b;
    cx  = (b2 << 2);
    cy  = (a2 << 2);

    e->vx  = cx;
    e->cvx = (b2 << 3);
    e->vy  = cy - cy * b;
    e->cvy = (a2 << 3);

    e->err = 0;
    /*
     * Now adjust the error terms for possible half-pixel repositioning
     */
    e->topy = (y - b);
    e->x = x >> 1;
    e->xoff = x + 1;
    if (x & 1)
    {
	e->x++;
	e->err += b2;
	e->vx  += cx;
    }
    if (e->topy & 1)
    {
	e->topy++;
	e->err += a2 - (a2 << 1) * b;
	e->vy  += cy;
    } else if (x & 1) {
	e->topy += 2;
	e->err += e->vy;
	e->vy += e->cvy;
    }
    e->topy = e->topy >> 1;
}

miFullWideArc (pDraw, pGC, parc)
    DrawablePtr	pDraw;
    GCPtr	pGC;
    xArc	*parc;
{
    DDXPointPtr	points;
    DDXPointPtr	ptsTop, ptsBottom;
    int		*widths;
    int		*widsTop, *widsBottom;
    int		nspans;
    EllipseWalkRec  inner, outer;
    int		    cx, cy;
    int		    hi, wi, ho, wo;
    int		    ey;
    int		    u_lx, l_lx;
    int		    center_y;
    int		    Oerr,Ovx,Ocvx,Ovy,Ocvy,Ox,Oxoff,Orx,Olx,Otopy,Onobottom;
    int		    Ierr,Ivx,Icvx,Ivy,Icvy,Ix,Ixoff,Irx,Ilx,Itopy,Inobottom;
    int		    x, y;

#define FillSpanTop(span_y, x1, x2) {\
    ptsTop->x = x1; \
    ptsTop->y = span_y; \
    ptsTop++; \
    *widsTop++ = x2 - x1; \
}

#define FillSpanBottom(span_y, x1, x2) {\
    --ptsBottom; \
    ptsBottom->x = x1; \
    ptsBottom->y = span_y; \
    *--widsBottom = x2 - x1; \
}

    x = parc->x;
    y = parc->y;
    cx = (x << 1) + parc->width;
    cy = (y << 1) + parc->height;
    wo = parc->width + pGC->lineWidth;
    ho = parc->height + pGC->lineWidth;
    wi = parc->width - pGC->lineWidth;
    hi = parc->height - pGC->lineWidth;
    if (hi <= 0 || wi <= 0)
    {
	hi = 0;
	wi = 0;
    }

    nspans = hi + ho;
    if ((wi & 1) && !((cy - hi) & 1))
	nspans--;
    if ((wo & 1) && !((cy - ho) & 1))
	nspans--;
    points = (DDXPointPtr)ALLOCATE_LOCAL(sizeof(DDXPointRec) * nspans);
    if (!points)
	return;
    widths = (int *)ALLOCATE_LOCAL(sizeof(int) * nspans);
    if (!widths)
    {
	DEALLOCATE_LOCAL(points);
	return;
    }
    SetupEllipse (cx, cy, wo, ho, &outer);
    FetchEllipse(&outer,Oerr,Ovx,Ocvx,Ovy,Ocvy,Ox,Oxoff,Orx,Olx,Otopy,Onobottom)
    SetupEllipse (cx, cy, wi, hi, &inner);
    FetchEllipse(&inner,Ierr,Ivx,Icvx,Ivy,Icvy,Ix,Ixoff,Irx,Ilx,Itopy,Inobottom)
    center_y = cy >> 1;
    ptsTop = points;
    ptsBottom = points + nspans;
    widsTop = widths;
    widsBottom = widths + nspans;
    for (ey = Otopy; ey <= center_y; ey++) {
	StepEllipseV(Oerr,Ovx,Ocvx,Ovy,Ocvy,Ox,Oxoff,Orx,Olx,Otopy,Onobottom)
	u_lx = Olx;
	l_lx = Olx;
	if (ey >= Itopy) {
	    StepEllipseV(Ierr,Ivx,Icvx,Ivy,Icvy,Ix,Ixoff,Irx,Ilx,Itopy,Inobottom)
	    u_lx = Irx;
	    FillSpanTop (ey, Olx, Ilx);
	    if (ey != (cy - ey) && !Inobottom) {
		FillSpanBottom (cy - ey, Olx, Ilx);
		l_lx = Irx;
	    }
	}
	FillSpanTop (ey, u_lx, Orx);
	if (ey != (cy - ey) && !Onobottom) {
	    FillSpanBottom (cy - ey, l_lx, Orx);
	}
    }
    (*pGC->ops->FillSpans)(pDraw, pGC, nspans, points, widths, TRUE);
    DEALLOCATE_LOCAL(widths);
    DEALLOCATE_LOCAL(points);
}

#define FillSpan(el,er,ey) { \
    pts->x = (el); \
    pts->y = (ey); \
    pts++; \
    *wids++ = (er) - (el); \
}
    
static void
miFillSlice (pDraw, pGC, pixel, spanData, outer, inner, slice, nspans, cy)
    DrawablePtr	    pDraw;
    GCPtr	    pGC;
    unsigned long   pixel;
    SpanDataPtr	    spanData;
    EllipseWalkPtr  outer, inner;
    SlicePtr	    slice;
    int		    nspans;
    int		    cy;
{
    DDXPointPtr	points, pts;
    int		*widths, *wids;
    int		ey;
    int		u_lx, l_lx;
    unsigned long   oldPixel;
    Spans	spanRec;
    int		center_y;

    center_y = cy>>1;
    /*
     * XXX there is a bug which causes more spans to be generated
     * than would seem possible; avoid it by overallocating...
     */
    AllocateSpans(pDraw,pGC,nspans*2,spanData,spanRec,points,widths,pixel,oldPixel,)
    pts = points;
    wids = widths;
    if (outer->topy != slice->edge1.topy)
    {
	StepFace(&slice->edge1);
	StepFace(&slice->edge2);
    }
    for (ey = outer->topy; ey <= center_y; ey++) {
	StepEllipse(outer, ey);
	u_lx = outer->lx;
	l_lx = outer->lx;
	if (ey >= inner->topy) {
	    StepEllipse(inner, ey);
	    u_lx = inner->rx;
	    UpperClip(outer->lx, inner->lx, ey, slice)
	    if (ey != (cy - ey) && !inner->nobottom) {
		LowerClip(outer->lx, inner->lx, cy - ey, slice);
		l_lx = inner->rx;
	    }
	}
	UpperClip(u_lx, outer->rx, ey, slice);
	if (ey != (cy - ey) && !outer->nobottom) {
	    LowerClip(l_lx, outer->rx, cy - ey, slice);
	}
	StepFace(&slice->edge1);
	StepFace(&slice->edge2);
    }
    DeallocateSpans(pDraw,pGC,pts-points,spanData,spanRec,points,widths,pixel,oldPixel)
}

static int
miSetupWideEllipse (parc, lw, outer, inner)
    xArc	    *parc;
    EllipseWalkPtr  outer, inner;
{
    int		    cx, cy;
    int		    hi, wi, ho, wo;
    cx = (parc->x << 1) + parc->width;
    cy = (parc->y << 1) + parc->height;
    wo = parc->width + lw;
    ho = parc->height + lw;
    wi = parc->width - lw;
    hi = parc->height - lw;
    if (hi <= 0 || wi <= 0)
    {
	hi = 0;
	wi = 0;
    }
    SetupEllipse (cx, cy, wo, ho, outer);
    SetupEllipse (cx, cy, wi, hi, inner);
    return cy;
}

static void
miWideArc (pDraw, pGC, spanData, parc, slicep)
    DrawablePtr	pDraw;
    GCPtr	pGC;
    SpanDataPtr	spanData;
    xArc	*parc;
    SliceRec	*slicep;
{
    EllipseWalkRec  outer, inner;
    int		    cy;
    SliceRec	    slice;

    slice = *slicep;
    cy = miSetupWideEllipse (parc, pGC->lineWidth, &outer, &inner);
    miFillSlice (pDraw, pGC, pGC->fgPixel, spanData, &outer, &inner, &slice,
		 (parc->height + pGC->lineWidth)<< 1, cy);
}

miWideDashedArc (pDraw, pGC, spanData, pDashOffset, pDashIndex, parc, slicep)
    DrawablePtr	pDraw;
    GCPtr	pGC;
    SpanDataPtr	spanData;
    int		*pDashOffset, *pDashIndex;
    xArc	*parc;
    SliceRec	*slicep;
{
    EllipseWalkRec  outer, inner, outer_t, inner_t;
    int		    cy;
    SliceRec	    slice;
    SliceRec	    slice_t;
    int		    a1, a2, apart;
    int		    signa2;
    FaceWalkRec	    dashEdge;
    int		    dashIndex;
    int		    dashRemain;
    Bool	    begining, ending;
    DashMapRec	    dashMap;
    Bool	    backwards;
    unsigned char   *pDash;
    Bool	    haveProjecting;
    Bool	    haveRound;
    unsigned long   pixel;
    int		    nspans;
    Bool	    fullCircle, firstCap = FALSE, lastCap = FALSE;
    int		    lastDashIndex, lastDashOffset;

    pDash = pGC->dash;
    dashIndex = *pDashIndex;
    dashRemain = pDash[dashIndex] - *pDashOffset;
    computeDashMap (parc, &dashMap);
    a1 = parc->angle1;
    a2 = parc->angle2;
    fullCircle = FALSE;
    if (backwards = a2 < 0)
    {
	if (a2 < -FULLCIRCLE)
	    a2 = -FULLCIRCLE;
	if (a2 == -FULLCIRCLE)
	    fullCircle = TRUE;
    }
    else
    {
	if (a2 > FULLCIRCLE)
	    a2 = FULLCIRCLE;
	if (a2 == FULLCIRCLE)
	    fullCircle = TRUE;
    }

    haveProjecting = pGC->lineStyle == LineOnOffDash &&
		     pGC->capStyle == CapProjecting;
    haveRound = pGC->lineStyle == LineOnOffDash &&
		pGC->capStyle == CapRound;

    /*
     * Compute whether the first/last dashes need caps for full-circle arcs
     */
    if (fullCircle && (haveProjecting || haveRound))
    {
	double	sidelen = dashMap.map[DASH_MAP_SIZE - 1];
	int	dist;

	lastDashIndex = dashIndex;
	lastDashOffset = *pDashOffset;
	dist = sidelen * 4;
	miStepDash (dist, &lastDashIndex, pGC->dash, pGC->numInDashList, &lastDashOffset);
	if (!(dashIndex & 1) && (lastDashIndex & 1))
	    firstCap = TRUE;
	else if ((dashIndex & 1) && !(lastDashIndex & 1))
	    lastCap = TRUE;
    }

    while (a1 <= -FULLCIRCLE)
	a1 += FULLCIRCLE;
    while (a1 >= FULLCIRCLE)
	a1 -= FULLCIRCLE;
    
    cy = miSetupWideEllipse (parc, pGC->lineWidth, &outer, &inner);
    begining = TRUE;
    nspans = (parc->height + pGC->lineWidth) << 1;
    while (a2)
    {
	apart = computeAngleFromPath (a1, a1+a2, &dashMap, &dashRemain, backwards);
	ending = dashRemain > 0;
	pixel = dashIndex & 1 ? pGC->bgPixel : pGC->fgPixel;
	if (!(dashIndex & 1) || pGC->lineStyle == LineDoubleDash)
	{
	    outer_t = outer;
	    inner_t = inner;
	    miWideArcSliceSetup(parc, pGC->lineWidth, a1, apart-a1, &slice);
	    if (haveProjecting)
	    {
		miComputeArcProject (parc, pGC->lineWidth, &slice, TRUE, TRUE);
	    	if (begining && !firstCap)
		    if (backwards)
		    	slice.edge1 = slicep->edge1;
		    else
		    	slice.edge2 = slicep->edge2;
	    	if (ending && !lastCap)
		    if (backwards)
		    	slice.edge2 = slicep->edge2;
		    else
		    	slice.edge1 = slicep->edge1;
	    }
	    if (haveRound)
	    {
		slice_t = slice;
		if (!begining || firstCap)
		    miArcCap (pDraw, pGC, pixel, parc, spanData, &slice_t, TRUE);
	    }
	    miFillSlice (pDraw, pGC, pixel,
		     	 spanData, &outer_t, &inner_t, &slice, nspans, cy);
	    if (haveRound && (!ending || lastCap))
		miArcCap (pDraw, pGC, pixel, parc, spanData, &slice_t, FALSE);
	}
	if (!ending)
	{
	    ++dashIndex;
	    if (dashIndex == pGC->numInDashList)
		dashIndex = 0;
	    dashRemain = pDash[dashIndex];
	}
	a2 -= (apart - a1);
	a1 = apart;
	begining = 0;
    }
    *pDashIndex = dashIndex;
    *pDashOffset = pDash[dashIndex] - dashRemain;    
}

static Bool
SetupJoinEllipse (join, edge)
    JoinEllipsePtr	join;
    JoinEllipseWalkPtr	edge;
{
    double  dx, dy;
    double  t;
    double  ci, co, t_ci, t_co;
    double  ya, xa;
    double  ya2, xa2;
    double  a2, b2;

    dx = join->x1 - join->x2;
    dy = join->y1 - join->y2;
    if (join->dx1 == 0)
	return FALSE;
    if (join->dx2 == 0)
	return FALSE;

    ci = dy - dx * join->dy1 / join->dx1;

    co = dy - dx * join->dy2 / join->dx2;

    if (co == -ci)
	return FALSE;

    ya = - (dy * ci) / (co + ci);

    t = 2 * co * ya + dy * dy;
    if (t == 0)
	return FALSE;
    xa = (dx * (dy - co) * ya) / t;

    ya2 = ya * ya;
    xa2 = xa * xa;

    t = dx + 2 * xa;
    if (t == 0)
	return FALSE;
    b2 = ya2 - (xa2 * dy * (dy + 2 * ya)) / (dx * t);

    t = b2 - ya2;
    if (t == 0)
	return FALSE;
    a2 = (b2 * xa2) / t;

    if (a2 < 0 || b2 < 0)
	return FALSE;
    edge->x = join->x + join->x2 - xa;
    edge->y = join->y - (join->y2 - ya);
    edge->a = sqrt (a2);
    edge->b = sqrt (b2);
    return TRUE;
}
    
static
StepFaceMultiple (face, y_adjust)
    FaceWalkPtr	face;
    int		y_adjust;
{
    int		x_adjust;
    int		err;
    int		h;

    err = face->err - y_adjust * face->dx;
    x_adjust = 0;
    if (err < 0)
    {
	x_adjust = (-err + face->dy - 1) / face->dy;
	err = err + x_adjust * face->dy;
    }
    face->x = face->x + x_adjust * face->stepx + y_adjust * face->jumpx;
    face->err = err;
    face->topy = face->topy + y_adjust;
}

static void
DrawJoinEllipse (pDraw, pGC, pixel, spanData, join, edge)
    DrawablePtr		pDraw;
    GCPtr		pGC;
    unsigned long	pixel;
    SpanDataPtr		spanData;
    JoinEllipseWalkPtr	join;
    FaceWalkPtr		edge;
{
    int			y, top_y, bot_y;
    double		d_y, d_x, y_off;
    double		a, a2, b2;
    int			x_l, x_r;
    unsigned long	oldPixel;
    Spans		spanRec;
    DDXPointPtr		points, pts;
    int			*widths, *wids;
    double		left_x, right_x;
    double		left_e, right_e;
    int			left_ix, right_ix;
    Bool		on_top;
    double		v_y, c_y;
    double		left_v_x, left_c_x, right_v_x, right_c_x;
    d_y = join->y - join->b;
    y_off = ceil (d_y);
    top_y = y_off;
    y_off -= d_y;
    d_y = join->b - y_off;
    bot_y = ceil (join->y + join->b);

    if (top_y >= bot_y)
	return;

    if (edge->dy)
	StepFaceMultiple (edge, top_y - edge->topy);

    a = join->a;
    a2 = a * a;
    b2 = join->b * join->b;
    AllocateSpans(pDraw,pGC,bot_y-top_y,spanData,spanRec,points,widths,pixel,oldPixel,)
    pts = points;
    wids = widths;

    left_x = ceil (join->x);
    left_ix = left_x;
    left_x = left_x - join->x;

    right_x = floor (join->x);
    right_ix = right_x;
    right_x = right_x - join->x;

#define EllipseError(x)	(b2 * (x) * (x) + a2 * d_y * d_y - a2 * b2)

    left_e = EllipseError (left_x);
    right_e = EllipseError (right_x);

    c_y =  2 * a2;
    v_y = - c_y * d_y + a2;
    
    left_c_x = 2 * b2;
    left_v_x = - left_c_x * left_x + b2;

    right_c_x = 2 * b2;
    right_v_x = right_c_x * right_x + b2;

    on_top = TRUE;
    for (y = top_y; y < bot_y; y++)
    {
	if (on_top)
	{
	    while (left_e <= 0)
	    {
	    	left_x -= 1.0;
	    	left_ix -= 1;

		left_e = left_e + left_v_x;
		left_v_x = left_v_x + left_c_x;
	    }
	    while (right_e < 0)
	    {
	    	right_x += 1.0;
	    	right_ix += 1;
		right_e = right_e + right_v_x;
		right_v_x = right_v_x + right_c_x;
	    }
	    x_l = left_ix + 1;
	    x_r = right_ix;
	    if (d_y == a)
		x_r++;
	}
	else
	{
	    while (left_e > 0 && left_v_x > 0)
	    {
		left_x += 1.0;
		left_ix += 1;
		left_v_x = left_v_x - left_c_x;
		left_e = left_e - left_v_x;
	    }
	    while (right_e >= 0 && right_v_x > 0)
	    {
		right_x -= 1.0;
		right_ix -= 1;
		right_v_x = right_v_x - right_c_x;
		right_e = right_e - right_v_x;
	    }
	    x_l = left_ix;
	    x_r = right_ix + 1;
	}
	if (edge->dy == 0)
	{
	    if (d_y < 0 == edge->top)
		x_l = x_r;
	}
	else
	{
	    if (edge->left)
 	    {
	    	if (x_l < edge->x)
		    x_l = edge->x;
	    }
 	    else
 	    {
	    	if (x_r > edge->x)
		    x_r = edge->x;
	    }
	}
	if (x_r > x_l)
	{
	    pts->x = x_l;
	    pts->y = y;
	    pts++;
	    *wids++ = x_r - x_l;
	}
	StepFace (edge);
	left_e = left_e + v_y;
	right_e = right_e + v_y;
	v_y = v_y + c_y;
	d_y--;
	if (d_y < 0)
	    on_top = FALSE;
    }
    DeallocateSpans(pDraw,pGC,pts-points,spanData,spanRec,points,widths,pixel,oldPixel)
}

miArcJoin (pDraw, pGC, pixel, prevArc, nextArc, spanData, rightFace, leftFace)
    DrawablePtr	    pDraw;
    GCPtr	    pGC;
    unsigned long   pixel;
    xArc	    *prevArc, *nextArc;
    SpanDataPtr	    spanData;
    SliceRec	    *rightFace, *leftFace;
{
}

#define ScaleEdge(a,b,c,d)  (a*b / sqrt(b*b*c*c + d*d*a*a))

miArcCap (pDraw, pGC, pixel, parc, spanData, slice, first)
    DrawablePtr	    pDraw;
    GCPtr	    pGC;
    unsigned long   pixel;
    xArc	    *parc;
    SpanDataPtr	    spanData;
    SliceRec	    *slice;
    Bool	    first;
{
    double	i_x, i_y, o_x, o_y;
    double	i_r, o_r;
    double	a, b, lw;
    double	i_a, i_b, o_a, o_b;
    FaceWalkRec	face;
    double	dx, dy;
    double	i_dx, i_dy, o_dx, o_dy;
    double	x, y;
    JoinEllipseRec  join;
    JoinEllipseWalkRec	edge;
    Bool	left;

    left = first;
    if (parc->angle2 < 0)
	left = !left;

    if (left)
	face = slice->edge2;
    else
	face = slice->edge1;

    face.left = !face.left;
    if (!face.top)
    {
	face.x = (parc->x << 1) + parc->width - face.x;
	if (face.stepx == 1)
	{
	    if (face.err)
	    {
		face.err = face.dy - face.err;
		face.x++;
	    }
	    face.err = (face.dy - 1) - face.err;
	}
	else
	{
	    face.err = (face.dy - 1) - face.err;
	    if (face.err)
	    {
		face.err = face.dy - face.err;
		face.x++;
	    }
	}
	face.stepx = -face.stepx;
	face.jumpx = -face.jumpx;
    }
    if (parc->width == parc->height)
    {
	if (first)
	{
	    edge.x = slice->start.x;
	    edge.y = slice->start.y;
	}
	else
	{
	    edge.x = slice->end.x;
	    edge.y = slice->end.y;
	}
	edge.b = edge.a = pGC->lineWidth / 2.0;
    }
    else
    {
    	dx = -(face.stepx * face.dx + face.dy * face.jumpx);
    	dy = face.dy;
	if (!face.top)
	{
	    dy = -dy;
	    dx = -dx;
	}
    	a = parc->width / 2.0;
    	b = parc->height / 2.0;
    	join.x = parc->x + a;
    	join.y = parc->y + b;
    	lw = pGC->lineWidth / 2.0;
    
    	if (lw > a || lw > b)
    	{
	    i_a = 0;
	    i_b = 0;
	    join.x1 = 0;
	    join.y1 = 0;
	    join.dx1 = -dy;
	    join.dy1 = dx;
	    if (lw < b)
		join.dx1 = 0;
	    if (lw < a)
		join.dy1 = 0;
    	}
    	else
    	{
	    i_a = a - lw;
	    i_b = b - lw;
    	    i_r = ScaleEdge(i_a, i_b, dx, dy);
    	    join.x1 = i_r * dx;
    	    join.y1 = i_r * dy;
    	    join.dx1 = - i_a * i_a * join.y1;
    	    join.dy1 =   i_b * i_b * join.x1;
	}
    
    	o_a = a + lw;
    	o_b = b + lw;
    	o_r = ScaleEdge(o_a, o_b, dx, dy);
    	join.x2 = o_r * dx;
    	join.y2 = o_r * dy;
    	join.dx2 = - o_a * o_a * join.y2;
    	join.dy2 =   o_b * o_b * join.x2;
    
    	if (!SetupJoinEllipse (&join, &edge))
	    return;
    }

    DrawJoinEllipse (pDraw, pGC, pixel, spanData, &edge, &face);
}

miComputeArcProject (parc, lw, slice, projectLeft, projectRight)
    xArc	*parc;
    int		lw;
    SlicePtr	*slice;
    Bool	projectLeft, projectRight;
{
    DashMapRec	map;
    double	hlw = lw / 2.0;
    double	len_t;
    int		angle2;
    int		angle1;

    computeDashMap(parc, &map);
    angle2 = parc->angle2;
    if (angle2 < 0)
    {
	hlw = -hlw;
    }

    angle1 = parc->angle1;
    angle2 = angle1 + angle2;
    if (projectLeft)
    {
    	len_t = angleToLength(angle1 + FULLCIRCLE, &map);
    	angle1 = lengthToAngle (len_t - hlw, &map) - FULLCIRCLE;
    }
    if (projectRight)
    {
    	len_t = angleToLength(angle2 + FULLCIRCLE, &map);
    	angle2 = lengthToAngle (len_t + hlw, &map) - FULLCIRCLE;
    }
    miWideArcSliceSetup (parc,lw,angle1,angle2-angle1,slice);
}

miFlushSpanData (pDraw, pGC, spanData)
    DrawablePtr	pDraw;
    GCPtr	pGC;
    SpanDataPtr	spanData;
{
    miCleanupSpanData (pDraw, pGC, spanData);
    if (pGC->lineStyle == LineDoubleDash)
	miInitSpanGroup (&spanData->bgGroup);
    miInitSpanGroup (&spanData->fgGroup);
}


void
miPolyArc (pDraw, pGC, narcs, parcs)
    DrawablePtr	pDraw;
    GCPtr	pGC;
    int		narcs;
    xArc	*parcs;
{
    SpanDataRec	    spanDataRec;
    SpanDataPtr	    spanData;
    Bool	    isDashed;
    int		    dashIndex, dashOffset;
    int		    idashIndex, idashOffset;
    Bool	    projectLeft, projectRight;
    xArc	    *prevArc, *nextArc, *firstArc;
    SliceRec	    prevSlice, curSlice, nextSlice;
    Bool	    joinPrev, joinCur, joinNext, joinWithPrev, joinWithNext;
    Bool	    haveNext;
    Bool	    hasProject;
    unsigned long   pixel = pGC->fgPixel;

    if (pGC->lineWidth == 0)
    {
	miZeroPolyArc (pDraw, pGC, narcs, parcs);
	return;
    }
    if (pGC->miTranslate && (pDraw->x | pDraw->y))
    {
	int	i, x, y;

	x = pDraw->x;
	y = pDraw->y;
	for (i = 0; i < narcs; i++)
	{
	    parcs[i].x += x;
	    parcs[i].y += y;
	}
    }
    isDashed = pGC->lineStyle != LineSolid;
    spanData = miSetupSpanData (pGC, &spanDataRec, narcs + 1);
    idashIndex = 0;
    if (isDashed)
    {
	idashOffset = 0;
	miStepDash ((int) pGC->dashOffset, &idashIndex, pGC->dash,
		    (int) pGC->numInDashList, &idashOffset);
    }
    dashIndex = idashIndex;
    dashOffset = idashOffset;
    hasProject = pGC->capStyle == CapProjecting;
    prevArc = parcs + (narcs - 1);
    joinPrev = !FullAngle (prevArc->angle2);
    joinWithPrev = FALSE;
    if (joinPrev)
    {
	miWideArcSliceSetup (prevArc, pGC->lineWidth,
			     prevArc->angle1, prevArc->angle2,
			     &prevSlice);
    }
    joinCur = !FullAngle (parcs->angle2);
    projectLeft = FALSE;
    projectRight = FALSE;
    if (joinCur)
    {
	miWideArcSliceSetup (parcs, pGC->lineWidth,
			     parcs->angle1, parcs->angle2,
			     &curSlice);
	joinWithPrev = joinPrev && 
		       (pGC->lineStyle == LineDoubleDash || !(dashIndex & 1)) &&
		       PointsCoincide (&curSlice.start, &prevSlice.end);
	if (!joinWithPrev)
	    projectLeft = hasProject;
    }

    firstArc = parcs;
    while (narcs--)
    {
	nextArc = parcs + 1;
	if (!narcs)
	    nextArc = firstArc;
	joinNext = !FullAngle (nextArc->angle2);
	joinWithNext = FALSE;
	if (joinNext)
	{
	    miWideArcSliceSetup (nextArc, pGC->lineWidth,
				 nextArc->angle1, nextArc->angle2,
				 &nextSlice);
	    joinWithNext = joinCur &&
			   PointsCoincide (&curSlice.end, &nextSlice.start);
	    if (!joinWithNext)
		projectRight = hasProject;
	}

	if (joinWithPrev)
	{
	    miArcJoin (pDraw, pGC, spanData, &prevSlice, &curSlice);
	}
 	else
 	{
	    if (spanData)
		miFlushSpanData (pDraw, pGC, spanData);
	    if (joinCur && pGC->capStyle == CapRound)
	    {
		if (isDashed)
		    pixel = dashIndex & 1 ? pGC->bgPixel : pGC->fgPixel;
		miArcCap (pDraw, pGC, pixel, parcs, spanData, &curSlice, TRUE);
	    }
	}
	if (projectRight || projectLeft)
	{
	    miComputeArcProject (parcs, pGC->lineWidth, &curSlice, projectRight, projectLeft);
	    projectLeft = FALSE;
	    projectRight = FALSE;
	}
	if (isDashed)
	{
	    int	prevDashIndex;

	    if (!joinWithPrev)
	    {
		dashOffset = idashOffset;
		dashIndex = idashIndex;
	    }
	    prevDashIndex = dashIndex;
	    miWideDashedArc (pDraw, pGC, spanData, &dashOffset, &dashIndex,
			     parcs, &curSlice);
	    joinWithNext = joinWithNext &&
			   (pGC->lineStyle == LineDoubleDash || !(dashIndex & 1));
	}
	else
	{
	    if (!joinCur)
		miFullWideArc (pDraw, pGC, parcs);
	    else
	    {
	    	miWideArc (pDraw, pGC, spanData, parcs, &curSlice);
	    }
	}
	/* XXX an on/off dashed arc which finishes an off-dash at
	 * the arc end will end up with a cap
	 */
	if (joinCur && !joinWithNext && pGC->capStyle == CapRound)
	{
	    if (isDashed)
		pixel = dashIndex & 1 ? pGC->bgPixel : pGC->fgPixel;
	    miArcCap (pDraw, pGC, pixel, parcs, spanData, &curSlice, FALSE);
	}

	prevSlice = curSlice;
	curSlice = nextSlice;
	joinPrev = joinCur;
	joinCur = joinNext;
	joinWithPrev = joinWithNext;
	parcs++;
    }
    if (spanData)
	miCleanupSpanData (pDraw, pGC, spanData);
}

/*
 * a polygonal approximation to the arc for computing arc lengths
 */

static void
computeDashMap (arcp, map)
    xArc	*arcp;
    DashMapPtr	map;
{
    int	di;
    double	a, x, y, prevx, prevy, dist;

    for (di = 0; di < DASH_MAP_SIZE; di++) {
	a = dashIndexToXAngle (di);
	x = ((double) arcp->width / 2.0) * Dcos (a);
	y = ((double) arcp->height / 2.0) * Dsin (a);
	if (di == 0)
 	{
	    map->map[di] = 0.0;
	}
 	else
 	{
	    dist = hypot (x - prevx, y - prevy);
	    map->map[di] = map->map[di - 1] + dist;
	}
	prevx = x;
	prevy = y;
    }
    map->ready = TRUE;
}

static double
angleToLength (angle, map)
    int		angle;
    DashMapPtr  map;
{
    double  len, excesslen, sidelen = map->map[DASH_MAP_SIZE - 1], totallen;
    int	    di;
    int	    excess;
    Bool    oddSide = FALSE;

    totallen = 0;
    if (angle >= 0) {
	while (angle >= 90 * 64) {
	    angle -= 90 * 64;
	    totallen += sidelen;
	    oddSide = !oddSide;
	}
    } else {
	while (angle < 0) {
	    angle += 90 * 64;
	    totallen -= sidelen;
	    oddSide = !oddSide;
	}
    }
    if (oddSide)
	angle = 90 * 64 - angle;
	    
    di = xAngleToDashIndex (angle);
    excess = angle - dashIndexToXAngle (di);

    len = map->map[di];
    /*
     * linearly interpolate between this point and the next
     */
    if (excess > 0) {
	excesslen = (map->map[di + 1] - map->map[di]) *
			((double) excess) / dashXAngleStep;
	len += excesslen;
    }
    if (oddSide)
	totallen += (sidelen - len);
    else
	totallen += len;
    return totallen;
}

/*
 * len is along the arc, but may be more than one rotation
 */

static int
lengthToAngle (len, map)
    double	    len;
    DashMapPtr  map;
{
    double	sidelen = map->map[DASH_MAP_SIZE - 1];
    int	angle, angleexcess;
    Bool	oddSide = FALSE;
    int	a0, a1, a;

    angle = 0;
    /*
     * step around the ellipse, subtracting sidelens and
     * adding 90 degrees.  oddSide will tell if the
     * map should be interpolated in reverse
     */
    if (len >= 0) {
	if (sidelen == 0)
	    return 2 * FULLCIRCLE;	/* infinity */
	while (len >= sidelen) {
	    angle += 90 * 64;
	    len -= sidelen;
	    oddSide = !oddSide;
	}
    } else {
	if (sidelen == 0)
	    return -2 * FULLCIRCLE;	/* infinity */
	while (len < 0) {
	    angle -= 90 * 64;
	    len += sidelen;
	    oddSide = !oddSide;
	}
    }
    if (oddSide)
	len = sidelen - len;
    a0 = 0;
    a1 = DASH_MAP_SIZE - 1;
    /*
     * binary search for the closest pre-computed length
     */
    while (a1 - a0 > 1) {
	a = (a0 + a1) / 2;
	if (len > map->map[a])
	    a0 = a;
	else
	    a1 = a;
    }
    angleexcess = dashIndexToXAngle (a0);
    /*
     * linearly interpolate to the next point
     */
    angleexcess += (len - map->map[a0]) /
		    (map->map[a0+1] - map->map[a0]) * dashXAngleStep;
    if (oddSide)
	angle += (90 * 64) - angleexcess;
    else
	angle += angleexcess;
    return angle;
}

/*
 * compute the angle of an ellipse which cooresponds to
 * the given path length.  Note that the correct solution
 * to this problem is an eliptic integral, we'll punt and
 * approximate (it's only for dashes anyway).  This
 * approximation uses a polygon.
 *
 * The remaining portion of len is stored in *lenp -
 * this will be negative if the arc extends beyond
 * len and positive if len extends beyond the arc.
 */

static int
computeAngleFromPath (startAngle, endAngle, map, lenp, backwards)
    int		startAngle, endAngle;
    DashMapPtr  map;
    int		*lenp;
    Bool	backwards;
{
    int	a0, a1, a;
    double	len0;
    int	len;

    a0 = startAngle;
    a1 = endAngle;
    len = *lenp;
    if (backwards) {
	/*
	 * flip the problem around to always be
	 * forwards
	 */
	a0 = FULLCIRCLE - a0;
	a1 = FULLCIRCLE - a1;
    }
    if (a1 < a0)
        a1 += FULLCIRCLE;
    len0 = angleToLength (a0, map);
    a = lengthToAngle (len0 + len, map);
    if (a > a1) {
	a = a1;
	len -= angleToLength (a1, map) - len0;
    } else
	len = 0;
    if (backwards)
	a = FULLCIRCLE - a;
    *lenp = len;
    return a;
}
