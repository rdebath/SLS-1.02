/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
/* $XConsortium: miarc.c,v 5.41 92/05/17 10:50:34 rws Exp $ */
/* Author: Keith Packard */

#include <math.h>
#include "X.h"
#include "Xprotostr.h"
#include "misc.h"
#include "gcstruct.h"
#include "scrnintstr.h"
#include "pixmapstr.h"
#include "windowstr.h"
#include "mifpoly.h"
#include "mi.h"
#include "mifillarc.h"
#include "Xfuncproto.h"

#if defined(SVR4) && __STDC__
extern double hypot(double, double);
#endif
static double miDsin(), miDcos(), miDasin(), miDatan2();
double	cbrt(
#if NeedFunctionPrototypes
	     double
#endif
);

#ifdef ICEILTEMPDECL
ICEILTEMPDECL
#endif

/*
 * some interesting sematic interpretation of the protocol:
 *
 * Self intersecting arcs (i.e. those spanning 360 degrees) 
 *  never join with other arcs, and are drawn without caps
 *  (unless on/off dashed, in which case each dash segment
 *  is capped, except when the last segment meets the
 *  first segment, when no caps are drawn)
 *
 * double dash arcs are drawn in two parts, first the
 *  odd dashes (drawn in background) then the even dashes
 *  (drawn in foreground).  This means that overlapping
 *  sections of foreground/background are drawn twice,
 *  first in background then in foreground.  The double-draw
 *  occurs even when the function uses the destination values
 *  (e.g. xor mode).  This is the same way the wide-line
 *  code works and should be "fixed".
 *
 * the wide arc code will never be "correct" -- the protocol
 *  document specifies exact pixelization which is impossible
 *  when calculating pixel positions with complicated floating-
 *  point expressions.
 */

# define todeg(xAngle)	(((double) (xAngle)) / 64.0)

#ifndef X_AXIS
# define X_AXIS 0
# define Y_AXIS 1
#endif

# define RIGHT_END	0
# define LEFT_END	1

typedef struct _miArcJoin {
	int	arcIndex0, arcIndex1;
	int	phase0, phase1;
	int	end0, end1;
} miArcJoinRec, *miArcJoinPtr;

typedef struct _miArcCap {
	int		arcIndex;
	int		end;		
} miArcCapRec, *miArcCapPtr;

typedef struct _miArcFace {
	SppPointRec	clock;
	SppPointRec	center;
	SppPointRec	counterClock;
} miArcFaceRec, *miArcFacePtr;

typedef struct _miArcData {
	xArc		arc;
	int		render;		/* non-zero means render after drawing */
	int		join;		/* related join */
	int		cap;		/* related cap */
	int		selfJoin;	/* final dash meets first dash */
	miArcFaceRec	bounds[2];
	double		x0, y0, x1, y1;
} miArcDataRec, *miArcDataPtr;

/*
 * This is an entire sequence of arcs, computed and categorized according
 * to operation.  miDashArcs generates either one or two of these.
 */

typedef struct _miPolyArc {
	int		narcs;
	miArcDataPtr	arcs;
	int		ncaps;
	miArcCapPtr	caps;
	int		njoins;
	miArcJoinPtr	joins;
} miPolyArcRec, *miPolyArcPtr;

#define GCValsFunction		0
#define GCValsForeground 	1
#define GCValsBackground 	2
#define GCValsLineWidth 	3
#define GCValsCapStyle 		4
#define GCValsJoinStyle		5
#define GCValsMask		(GCFunction | GCForeground | GCBackground | \
				 GCLineWidth | GCCapStyle | GCJoinStyle)
static XID gcvals[6];

extern void miFillSppPoly();
static void fillSpans(), span(), drawArc(), drawQuadrant(), drawZeroArc();
static void miFreeArcs(), miArcJoin(), miArcCap(), miRoundCap();
static int computeAngleFromPath(), miGetArcPts();
static miPolyArcPtr miComputeArcs ();

#undef max
#undef min

#if defined (__GNUC__) && defined (__STDC__) && !defined (__STRICT_ANSI__)
#define USE_INLINE
#endif

#ifdef USE_INLINE
inline static const int max (const int x, const int y)
{
	return x>y? x:y;
}

inline static const int min (const int x, const int y)
{
	return x<y? x:y;
}

inline static const double fmax (const double x, const double y)
{
	return x>y? x:y;
}

inline static const double fmin (const double x, const double y)
{
	return x<y? x:y;
}

#else

static int
max (x, y)
{
	return x>y? x:y;
}

static int
min (x, y)
{
	return x<y? x:y;
}

static double
fmax (a, b)
double	a,b;
{
	return a > b? a : b;
}

static double
fmin (a, b)
double	a, b;
{
	return a < b ? a : b;
}

#endif

/*
 * draw one segment of the arc using the arc spans generation routines
 */

static void
miArcSegment(pDraw, pGC, tarc, right, left)
    DrawablePtr   pDraw;
    GCPtr         pGC;
    xArc          tarc;
    miArcFacePtr	right, left;
{
    int l = pGC->lineWidth;
    int a0, a1, startAngle, endAngle;
    miArcFacePtr	temp;

    if (tarc.width == 0 || tarc.height == 0) {
    	drawZeroArc (pDraw, pGC, tarc, left, right);
	return;
    }

    if (pGC->miTranslate) {
	tarc.x += pDraw->x;
	tarc.y += pDraw->y;
    }

    a0 = tarc.angle1;
    a1 = tarc.angle2;
    if (a1 > FULLCIRCLE)
	a1 = FULLCIRCLE;
    else if (a1 < -FULLCIRCLE)
	a1 = -FULLCIRCLE;
    if (a1 < 0) {
    	startAngle = a0 + a1;
	endAngle = a0;
	temp = right;
	right = left;
	left = temp;
    } else {
	startAngle = a0;
	endAngle = a0 + a1;
    }
    /*
     * bounds check the two angles
     */
    if (startAngle < 0)
	startAngle = FULLCIRCLE - (-startAngle) % FULLCIRCLE;
    if (startAngle >= FULLCIRCLE)
	startAngle = startAngle % FULLCIRCLE;
    if (endAngle < 0)
	endAngle = FULLCIRCLE - (-endAngle) % FULLCIRCLE;
    if (endAngle > FULLCIRCLE)
	endAngle = (endAngle-1) % FULLCIRCLE + 1;
    if ((startAngle == endAngle) && a1) {
	startAngle = 0;
	endAngle = FULLCIRCLE;
    }

    drawArc ((int) tarc.x, (int) tarc.y,
             (int) tarc.width, (int) tarc.height, l, startAngle, endAngle,
	     right, left);
}

/*

Three equations combine to describe the boundaries of the arc

x^2/w^2 + y^2/h^2 = 1			ellipse itself
(X-x)^2 + (Y-y)^2 = r^2			circle at (x, y) on the ellipse
(Y-y) = (X-x)*w^2*y/(h^2*x)		normal at (x, y) on the ellipse

These lead to a quartic relating Y and y

y^4 - (2Y)y^3 + (Y^2 + (h^4 - w^2*r^2)/(w^2 - h^2))y^2
    - (2Y*h^4/(w^2 - h^2))y + (Y^2*h^4)/(w^2 - h^2) = 0

The reducible cubic obtained from this quartic is

z^3 - (3N)z^2 - 2V = 0

where

N = (Y^2 + (h^4 - w^2*r^2/(w^2 - h^2)))/6
V = w^2*r^2*Y^2*h^4/(4 *(w^2 - h^2)^2)

Let

t = z - N
p = -N^2
q = -N^3 - V

Then we get

t^3 + 3pt + 2q = 0

The discriminant of this cubic is

D = q^2 + p^3

When D > 0, a real root is obtained as

z = N + cbrt(-q+sqrt(D)) + cbrt(-q-sqrt(D))

When D < 0, a real root is obtained as

z = N - 2m*cos(acos(-q/m^3)/3)

where

m = sqrt(|p|) * sign(q)

Given a real root Z of the cubic, the roots of the quartic are the roots
of the two quadratics

y^2 + ((b+A)/2)y + (Z + (bZ - d)/A) = 0

where 

A = +/- sqrt(8Z + b^2 - 4c)
b, c, d are the cubic, quadratic, and linear coefficients of the quartic

Some experimentation is then required to determine which solutions
correspond to the inner and outer boundaries.

*/

typedef struct {
    short lx, lw, rx, rw;
} miArcSpan;

typedef struct {
    miArcSpan *spans;
    int count1, count2, k;
    char top, bot, hole;
} miArcSpanData;

typedef struct {
    unsigned long lrustamp;
    unsigned short lw;
    unsigned short width, height;
    miArcSpanData *spdata;
} arcCacheRec;

#define CACHESIZE 25

static arcCacheRec arcCache[CACHESIZE];
static unsigned long lrustamp;
static arcCacheRec *lastCacheHit = &arcCache[0];
static RESTYPE cacheType;

/*
 * External so it can be called when low on memory.
 * Call with a zero ID in that case.
 */
/*ARGSUSED*/
int
miFreeArcCache (data, id)
    pointer	    data;
    XID		    id;
{
    int k;
    arcCacheRec *cent;

    if (id)
	cacheType = 0;

    for (k = CACHESIZE, cent = &arcCache[0]; --k >= 0; cent++)
    {
	if (cent->spdata)
	{
	    cent->lrustamp = 0;
	    cent->lw = 0;
	    xfree(cent->spdata);
	    cent->spdata = NULL;
	}
    }
    lrustamp = 0;
}

static void
miComputeCircleSpans(lw, parc, spdata)
    int lw;
    xArc *parc;
    miArcSpanData *spdata;
{
    register miArcSpan *span;
    int doinner;
    register int x, y, e;
    int xk, yk, xm, ym, dx, dy;
    register int slw, inslw;
    int inx, iny, ine;
    int inxk, inyk, inxm, inym;

    doinner = -lw;
    slw = parc->width - doinner;
    y = parc->height >> 1;
    dy = parc->height & 1;
    dx = 1 - dy;
    MIWIDEARCSETUP(x, y, dy, slw, e, xk, xm, yk, ym);
    inslw = parc->width + doinner;
    if (inslw > 0)
    {
	spdata->hole = spdata->top;
	MIWIDEARCSETUP(inx, iny, dy, inslw, ine, inxk, inxm, inyk, inym);
    }
    else
    {
	spdata->hole = FALSE;
	doinner = -y;
    }
    spdata->count1 = -doinner - spdata->top;
    spdata->count2 = y + doinner;
    span = spdata->spans;
    while (y)
    {
	MIFILLARCSTEP(slw);
	span->lx = dy - x;
	if (++doinner <= 0)
 	{
	    span->lw = slw;
	}
	else
	{
	    MIFILLINARCSTEP(inslw);
	    span->lw = x - inx;
	    span->rx = dy - inx + inslw;
	    span->rw = inx - x + slw - inslw;
	}
	span++;
    }
    if (spdata->bot)
    {
	if (spdata->count2)
	    spdata->count2--;
	else
	{
	    span[-1].rw = 0;
	    spdata->count1--;
	}
    }
}

static void
miComputeEllipseSpans(lw, parc, spdata)
    int lw;
    xArc *parc;
    miArcSpanData *spdata;
{
    register miArcSpan *span;
    double w, h, r, xorg;
    double Hs, Hf, WH, K, Vk, Nk, Fk, Vr, N, Nc, Z, rs;
    double A, T, b, d, x, y, t, inx, outx, hepp, hepm;
    int flip, solution;

    w = parc->width / 2.0;
    h = parc->height / 2.0;
    r = lw / 2.0;
    rs = r * r;
    Hs = h * h;
    WH = w * w - Hs;
    Nk = w * r;
    Vk = (Nk * Hs) / (WH + WH);
    Hf = Hs * Hs;
    Nk = (Hf - Nk * Nk) / WH;
    Fk = Hf / WH;
    hepp = h + EPSILON;
    hepm = h - EPSILON;
    K = h + ((lw - 1) >> 1);
    span = spdata->spans;
    if (parc->width & 1)
	xorg = .5;
    else
	xorg = 0.0;
    if (spdata->top)
    {
	span->lx = 0;
	span->lw = 1;
	span++;
    }
    spdata->count1 = 0;
    spdata->count2 = 0;
    spdata->hole = (spdata->top &&
		    parc->height * lw <= parc->width * parc->width &&
		    lw < parc->height);
    for (; K > 0.0; K -= 1.0)
    {
	N = (K * K + Nk) / 6.0;
	Nc = N * N * N;
	Vr = Vk * K;
	t = Nc + Vr * Vr;
	d = Nc + t;
	if (d < 0.0) {
	    d = Nc;
	    b = N;
	    if (b < 0.0 == t < 0.0)
	    {
		b = -b;
		d = -d;
	    }
	    Z = N - 2.0 * b * cos(acos(-t / d) / 3.0);
	    if (Z < 0.0 == Vr < 0.0)
		flip = 2;
	    else
		flip = 1;
	}
	else
	{
	    d = Vr * sqrt(d);
	    Z = N + cbrt(t + d) + cbrt(t - d);
	    flip = 0;
	}
	A = sqrt((Z + Z) - Nk);
	T = (Fk - Z) * K / A;
	inx = 0.0;
	solution = FALSE;
	b = -A + K;
	d = b * b - 4 * (Z + T);
	if (d >= 0)
	{
	    d = sqrt(d);
	    y = (b + d) / 2;
	    if ((y >= 0.0) && (y < hepp))
	    {
		solution = TRUE;
		if (y > hepm)
		    y = h;
		t = y / h;
		x = w * sqrt(1 - (t * t));
		t = K - y;
		t = sqrt(rs - (t * t));
		if (flip == 2)
		    inx = x - t;
		else
		    outx = x + t;
	    }
	}
	b = A + K;
	d = b * b - 4 * (Z - T);
	/* Because of the large magnitudes involved, we lose enough precision
	 * that sometimes we end up with a negative value near the axis, when
	 * it should be positive.  This is a workaround.
	 */
	if (d < 0 && !solution)
	    d = 0.0;
	if (d >= 0) {
	    d = sqrt(d);
	    y = (b + d) / 2;
	    if (y < hepp)
	    {
		if (y > hepm)
		    y = h;
		t = y / h;
		x = w * sqrt(1 - (t * t));
		t = K - y;
		inx = x - sqrt(rs - (t * t));
	    }
	    y = (b - d) / 2;
	    if (y >= 0.0)
	    {
		if (y > hepm)
		    y = h;
		t = y / h;
		x = w * sqrt(1 - (t * t));
		t = K - y;
		t = sqrt(rs - (t * t));
		if (flip == 1)
		    inx = x - t;
		else
		    outx = x + t;
	    }
	}
	span->lx = ICEIL(xorg - outx);
	if (inx <= 0.0)
	{
	    spdata->count1++;
	    span->lw = ICEIL(xorg + outx) - span->lx;
	}
	else
	{
	    spdata->count2++;
	    span->lw = ICEIL(xorg - inx) - span->lx;
	    span->rx = ICEIL(xorg + inx);
	    span->rw = ICEIL(xorg + outx) - span->rx;
	}
	span++;
    }
    if (spdata->bot)
    {
	outx = w + r;
	if (r >= h)
	    inx = 0.0;
	else if (Nk < 0.0 && -Nk < Hs)
	    inx = w * sqrt(1 + Nk / Hs) - sqrt(rs + Nk);
	else
	    inx = w - r;
	span->lx = ICEIL(xorg - outx);
	if (inx <= 0.0)
	{
	    span->lw = ICEIL(xorg + outx) - span->lx;
	    span->rw = 0;
	}
	else
	{
	    span->lw = ICEIL(xorg - inx) - span->lx;
	    span->rx = ICEIL(xorg + inx);
	    span->rw = ICEIL(xorg + outx) - span->rx;
	}
    }
    if (spdata->hole)
    {
	span = &spdata->spans[spdata->count1];
	span->lw = -span->lx;
	span->rx = 1;
	span->rw = span->lw;
	spdata->count1--;
	spdata->count2++;
    }
}

static miArcSpanData *
miComputeWideEllipse(lw, parc, mustFree)
    int		   lw;
    register xArc *parc;
    Bool	  *mustFree;
{
    register miArcSpanData *spdata;
    register arcCacheRec *cent, *lruent;
    register int k;
    arcCacheRec fakeent;

    if (!lw)
	lw = 1;
    if (parc->height <= 1500)
    {
	*mustFree = FALSE;
	cent = lastCacheHit;
	if (cent->lw == lw &&
	    cent->width == parc->width && cent->height == parc->height)
	{
	    cent->lrustamp = ++lrustamp;
	    return cent->spdata;
	}
	lruent = &arcCache[0];
	for (k = CACHESIZE, cent = lruent; --k >= 0; cent++)
	{
	    if (cent->lw == lw &&
		cent->width == parc->width && cent->height == parc->height)
	    {
		cent->lrustamp = ++lrustamp;
		lastCacheHit = cent;
		return cent->spdata;
	    }
	    if (cent->lrustamp < lruent->lrustamp)
		lruent = cent;
	}
	if (!cacheType)
	{
	    cacheType = CreateNewResourceType(miFreeArcCache);
	    (void) AddResource(FakeClientID(0), cacheType, NULL);
	}
    } else {
	lruent = &fakeent;
	lruent->spdata = NULL;
	*mustFree = TRUE;
    }
    k = (parc->height >> 1) + ((lw - 1) >> 1);
    spdata = lruent->spdata;
    if (!spdata || spdata->k != k)
    {
	if (spdata)
	    xfree(spdata);
	spdata = (miArcSpanData *)xalloc(sizeof(miArcSpanData) +
					 sizeof(miArcSpan) * (k + 2));
	lruent->spdata = spdata;
	if (!spdata)
	{
	    lruent->lrustamp = 0;
	    lruent->lw = 0;
	    return spdata;
	}
	spdata->spans = (miArcSpan *)(spdata + 1);
	spdata->k = k;
    }
    spdata->top = !(lw & 1) && !(parc->width & 1);
    spdata->bot = !(parc->height & 1);
    lruent->lrustamp = ++lrustamp;
    lruent->lw = lw;
    lruent->width = parc->width;
    lruent->height = parc->height;
    lastCacheHit = lruent;
    if (parc->width == parc->height)
	miComputeCircleSpans(lw, parc, spdata);
    else
	miComputeEllipseSpans(lw, parc, spdata);
    return spdata;
}

static void
miFillWideEllipse(pDraw, pGC, parc)
    DrawablePtr	pDraw;
    GCPtr	pGC;
    xArc	*parc;
{
    DDXPointPtr points;
    register DDXPointPtr pts;
    int *widths;
    register int *wids;
    miArcSpanData *spdata;
    Bool mustFree;
    register miArcSpan *span;
    register int xorg, yorgu, yorgl;
    register int n;

    yorgu = parc->height + pGC->lineWidth;
    n = (sizeof(int) * 2) * yorgu;
    widths = (int *)ALLOCATE_LOCAL(n + (sizeof(DDXPointRec) * 2) * yorgu);
    if (!widths)
	return;
    points = (DDXPointPtr)((char *)widths + n);
    spdata = miComputeWideEllipse(pGC->lineWidth, parc, &mustFree);
    if (!spdata)
    {
	DEALLOCATE_LOCAL(widths);
	return;
    }
    pts = points;
    wids = widths;
    span = spdata->spans;
    xorg = parc->x + (parc->width >> 1);
    yorgu = parc->y + (parc->height >> 1);
    yorgl = yorgu + (parc->height & 1);
    if (pGC->miTranslate)
    {
	xorg += pDraw->x;
	yorgu += pDraw->y;
	yorgl += pDraw->y;
    }
    yorgu -= spdata->k;
    yorgl += spdata->k;
    if (spdata->top)
    {
	pts->x = xorg;
	pts->y = yorgu - 1;
	pts++;
	*wids++ = 1;
	span++;
    }
    for (n = spdata->count1; --n >= 0; )
    {
	pts[0].x = xorg + span->lx;
	pts[0].y = yorgu;
	wids[0] = span->lw;
	pts[1].x = pts[0].x;
	pts[1].y = yorgl;
	wids[1] = wids[0];
	yorgu++;
	yorgl--;
	pts += 2;
	wids += 2;
	span++;
    }
    if (spdata->hole)
    {
	pts[0].x = xorg;
	pts[0].y = yorgl;
	wids[0] = 1;
	pts++;
	wids++;
    }
    for (n = spdata->count2; --n >= 0; )
    {
	pts[0].x = xorg + span->lx;
	pts[0].y = yorgu;
	wids[0] = span->lw;
	pts[1].x = xorg + span->rx;
	pts[1].y = pts[0].y;
	wids[1] = span->rw;
	pts[2].x = pts[0].x;
	pts[2].y = yorgl;
	wids[2] = wids[0];
	pts[3].x = pts[1].x;
	pts[3].y = pts[2].y;
	wids[3] = wids[1];
	yorgu++;
	yorgl--;
	pts += 4;
	wids += 4;
	span++;
    }
    if (spdata->bot)
    {
	if (!span->rw)
	{
	    pts[0].x = xorg + span->lx;
	    pts[0].y = yorgu;
	    wids[0] = span->lw;
	    pts++;
	    wids++;
	}	
	else
	{
	    pts[0].x = xorg + span->lx;
	    pts[0].y = yorgu;
	    wids[0] = span->lw;
	    pts[1].x = xorg + span->rx;
	    pts[1].y = pts[0].y;
	    wids[1] = span->rw;
	    pts += 2;
	    wids += 2;
	}
    }
    if (mustFree)
	xfree(spdata);
    (*pGC->ops->FillSpans)(pDraw, pGC, pts - points, points, widths, FALSE);

    DEALLOCATE_LOCAL(widths);
}

/*
 * miPolyArc strategy:
 *
 * If arc is zero width and solid, we don't have to worry about the rasterop
 * or join styles.  For wide solid circles, we use a fast integer algorithm.
 * For wide solid ellipses, we use special case floating point code.
 * Otherwise, we set up pDrawTo and pGCTo according to the rasterop, then
 * draw using pGCTo and pDrawTo.  If the raster-op was "tricky," that is,
 * if it involves the destination, then we use PushPixels to move the bits
 * from the scratch drawable to pDraw. (See the wide line code for a
 * fuller explanation of this.)
 */

void
miPolyArc(pDraw, pGC, narcs, parcs)
    DrawablePtr	pDraw;
    GCPtr	pGC;
    int		narcs;
    xArc	*parcs;
{
    register int		i;
    xArc			*parc;
    int				xMin, xMax, yMin, yMax;
    int				dx, dy;
    int				xOrg, yOrg;
    int				width;
    Bool			fTricky;
    DrawablePtr			pDrawTo;
    unsigned long		fg, bg;
    GCPtr			pGCTo;
    miPolyArcPtr		polyArcs;
    int				cap[2], join[2];
    int				iphase;

    width = pGC->lineWidth;
    if(width == 0 && pGC->lineStyle == LineSolid)
    {
	for(i = narcs, parc = parcs; --i >= 0; parc++)
	    miArcSegment( pDraw, pGC, *parc,
 	    (miArcFacePtr) 0, (miArcFacePtr) 0 );
	fillSpans (pDraw, pGC);
    }
    else 
    {
	if ((pGC->lineStyle == LineSolid) && narcs)
	{
	    while (parcs->width && parcs->height &&
		   (parcs->angle2 >= FULLCIRCLE ||
		    parcs->angle2 <= -FULLCIRCLE))
	    {
		miFillWideEllipse(pDraw, pGC, parcs);
		if (!--narcs)
		    return;
		parcs++;
	    }
	}

	/* Set up pDrawTo and pGCTo based on the rasterop */
	switch(pGC->alu)
	{
	  case GXclear:		/* 0 */
	  case GXcopy:		/* src */
	  case GXcopyInverted:	/* NOT src */
	  case GXset:		/* 1 */
	    fTricky = FALSE;
	    pDrawTo = pDraw;
	    pGCTo = pGC;
	    break;
	  default:
	    fTricky = TRUE;

	    xMin = yMin = MAXSHORT;
	    xMax = yMax = MINSHORT;

	    for(i = narcs, parc = parcs; --i >= 0; parc++)
	    {
		xMin = min (xMin, parc->x);
		yMin = min (yMin, parc->y);
		xMax = max (xMax, (parc->x + (int) parc->width));
		yMax = max (yMax, (parc->y + (int) parc->height));
	    }

	    pGCTo = GetScratchGC(1, pDraw->pScreen);
	    if (!pGCTo)
		return;
	    gcvals[GCValsFunction] = GXcopy;
	    gcvals[GCValsForeground] = 1;
	    gcvals[GCValsBackground] = 0;
	    gcvals[GCValsLineWidth] = pGC->lineWidth;
	    gcvals[GCValsCapStyle] = pGC->capStyle;
	    gcvals[GCValsJoinStyle] = pGC->joinStyle;
	    DoChangeGC(pGCTo, GCValsMask, gcvals, 0);
    
    	    xOrg = xMin - (width + 1)/2;
	    yOrg = yMin - (width + 1)/2;
	    dx = (xMax - xMin) + width + 1;
	    dy = (yMax - yMin) + width + 1;
	    for(i = narcs, parc = parcs; --i >= 0; parc++)
	    {
		parc->x -= xOrg;
		parc->y -= yOrg;
	    }
	    if (pGC->miTranslate)
	    {
		xOrg += pDraw->x;
		yOrg += pDraw->y;
	    }

	    /* allocate a 1 bit deep pixmap of the appropriate size, and
	     * validate it */
	    pDrawTo = (DrawablePtr)(*pDraw->pScreen->CreatePixmap)
					(pDraw->pScreen, dx, dy, 1);
	    if (!pDrawTo)
	    {
		FreeScratchGC(pGCTo);
		return;
	    }
	    ValidateGC(pDrawTo, pGCTo);
	    miClearDrawable(pDrawTo, pGCTo);
	}

	fg = pGC->fgPixel;
	bg = pGC->bgPixel;
	if ((pGC->fillStyle == FillTiled) ||
	    (pGC->fillStyle == FillOpaqueStippled))
	    bg = fg; /* the protocol sez these don't cause color changes */

	polyArcs = miComputeArcs (parcs, narcs, pGC);

	if (!polyArcs)
	{
	    if (fTricky) {
		(*pDraw->pScreen->DestroyPixmap) ((PixmapPtr)pDrawTo);
		FreeScratchGC (pGCTo);
	    }
	    return;
	}

	cap[0] = cap[1] = 0;
	join[0] = join[1] = 0;
	for (iphase = ((pGC->lineStyle == LineDoubleDash) ? 1 : 0);
 	     iphase >= 0;
	     iphase--)
	{
	    if (iphase == 1) {
		DoChangeGC (pGC, GCForeground, (XID *)&bg, 0);
		ValidateGC (pDraw, pGC);
	    } else if (pGC->lineStyle == LineDoubleDash) {
		DoChangeGC (pGC, GCForeground, (XID *)&fg, 0);
		ValidateGC (pDraw, pGC);
	    }
	    for (i = 0; i < polyArcs[iphase].narcs; i++) {
		miArcDataPtr	arcData;

		arcData = &polyArcs[iphase].arcs[i];
		miArcSegment(pDrawTo, pGCTo, arcData->arc,
			     &arcData->bounds[RIGHT_END],
			     &arcData->bounds[LEFT_END]);
		if (polyArcs[iphase].arcs[i].render) {
		    fillSpans (pDrawTo, pGCTo);
		    /*
		     * don't cap self-joining arcs
		     */
		    if (polyArcs[iphase].arcs[i].selfJoin &&
		        cap[iphase] < polyArcs[iphase].arcs[i].cap)
		    	cap[iphase]++;
		    while (cap[iphase] < polyArcs[iphase].arcs[i].cap) {
			int	arcIndex, end;
			miArcDataPtr	arcData0;

			arcIndex = polyArcs[iphase].caps[cap[iphase]].arcIndex;
			end = polyArcs[iphase].caps[cap[iphase]].end;
			arcData0 = &polyArcs[iphase].arcs[arcIndex];
			miArcCap (pDrawTo, pGCTo,
 				  &arcData0->bounds[end], end,
				  arcData0->arc.x, arcData0->arc.y,
				  (double) arcData0->arc.width / 2.0,
 				  (double) arcData0->arc.height / 2.0);
			++cap[iphase];
		    }
		    while (join[iphase] < polyArcs[iphase].arcs[i].join) {
			int	arcIndex0, arcIndex1, end0, end1;
			int	phase0, phase1;
			miArcDataPtr	arcData0, arcData1;
			miArcJoinPtr	joinp;

			joinp = &polyArcs[iphase].joins[join[iphase]];
			arcIndex0 = joinp->arcIndex0;
			end0 = joinp->end0;
			arcIndex1 = joinp->arcIndex1;
			end1 = joinp->end1;
			phase0 = joinp->phase0;
			phase1 = joinp->phase1;
			arcData0 = &polyArcs[phase0].arcs[arcIndex0];
			arcData1 = &polyArcs[phase1].arcs[arcIndex1];
			miArcJoin (pDrawTo, pGCTo,
				  &arcData0->bounds[end0],
 				  &arcData1->bounds[end1],
				  arcData0->arc.x, arcData0->arc.y,
				  (double) arcData0->arc.width / 2.0,
 				  (double) arcData0->arc.height / 2.0,
				  arcData1->arc.x, arcData1->arc.y,
				  (double) arcData1->arc.width / 2.0,
 				  (double) arcData1->arc.height / 2.0);
			++join[iphase];
		    }
		    if (fTricky) {
			if (pGC->serialNumber != pDraw->serialNumber)
			    ValidateGC (pDraw, pGC);
		    	(*pGC->ops->PushPixels) (pGC, pDrawTo, pDraw, dx,
					    dy, xOrg, yOrg);
			miClearDrawable ((DrawablePtr) pDrawTo, pGCTo);
		    }
		}
	    }
	}
	miFreeArcs(polyArcs, pGC);

	if(fTricky)
	{
	    (*pGCTo->pScreen->DestroyPixmap)((PixmapPtr)pDrawTo);
	    FreeScratchGC(pGCTo);
	}
    }
}

static double
angleBetween (center, point1, point2)
	SppPointRec	center, point1, point2;
{
	double	a1, a2, a;
	
	/*
	 * reflect from X coordinates back to ellipse
	 * coordinates -- y increasing upwards
	 */
	a1 = miDatan2 (- (point1.y - center.y), point1.x - center.x);
	a2 = miDatan2 (- (point2.y - center.y), point2.x - center.x);
	a = a2 - a1;
	if (a <= -180.0)
		a += 360.0;
	else if (a > 180.0)
		a -= 360.0;
	return a;
}

static
translateBounds (b, x, y, fx, fy)
miArcFacePtr	b;
int		x, y;
double		fx, fy;
{
	b->clock.x -= x + fx;
	b->clock.y -= y + fy;
	b->center.x -= x + fx;
	b->center.y -= y + fy;
	b->counterClock.x -= x + fx;
	b->counterClock.y -= y + fy;
}

static void
miArcJoin (pDraw, pGC, pLeft, pRight,
	   xOrgLeft, yOrgLeft, xFtransLeft, yFtransLeft,
	   xOrgRight, yOrgRight, xFtransRight, yFtransRight)
	DrawablePtr	pDraw;
	GCPtr		pGC;
	miArcFacePtr	pRight, pLeft;
	int		xOrgRight, yOrgRight;
	double		xFtransRight, yFtransRight;
	int		xOrgLeft, yOrgLeft;
	double		xFtransLeft, yFtransLeft;
{
	SppPointRec	center, corner, otherCorner;
	SppPointRec	poly[5], e;
	SppPointPtr	pArcPts;
	int		cpt;
	SppArcRec	arc;
	miArcFaceRec	Right, Left;
	int		polyLen;
	int		xOrg, yOrg;
	double		xFtrans, yFtrans;
	double		a;
	double		ae, ac2, ec2, bc2, de;
	double		width;
	
	xOrg = (xOrgRight + xOrgLeft) / 2;
	yOrg = (yOrgRight + yOrgLeft) / 2;
	xFtrans = (xFtransLeft + xFtransRight) / 2;
	yFtrans = (yFtransLeft + yFtransRight) / 2;
	Right = *pRight;
	translateBounds (&Right, xOrg - xOrgRight, yOrg - yOrgRight,
				 xFtrans - xFtransRight, yFtrans - yFtransRight);
	Left = *pLeft;
	translateBounds (&Left, xOrg - xOrgLeft, yOrg - yOrgLeft,
				 xFtrans - xFtransLeft, yFtrans - yFtransLeft);
	pRight = &Right;
	pLeft = &Left;

	if (pRight->clock.x == pLeft->counterClock.x &&
	    pRight->clock.y == pLeft->counterClock.y)
		return;
	center = pRight->center;
	if (0 <= (a = angleBetween (center, pRight->clock, pLeft->counterClock))
 	    && a <= 180.0)
 	{
		corner = pRight->clock;
		otherCorner = pLeft->counterClock;
	} else {
		a = angleBetween (center, pLeft->clock, pRight->counterClock);
		corner = pLeft->clock;
		otherCorner = pRight->counterClock;
	}
	switch (pGC->joinStyle) {
	case JoinRound:
		width = (pGC->lineWidth ? pGC->lineWidth : 1);

		arc.x = center.x - width/2;
		arc.y = center.y - width/2;
		arc.width = width;
		arc.height = width;
		arc.angle1 = -miDatan2 (corner.y - center.y, corner.x - center.x);
		arc.angle2 = a;
		pArcPts = (SppPointPtr) xalloc (3 * sizeof (SppPointRec));
		if (!pArcPts)
		    return;
		pArcPts[0].x = otherCorner.x;
		pArcPts[0].y = otherCorner.y;
		pArcPts[1].x = center.x;
		pArcPts[1].y = center.y;
		pArcPts[2].x = corner.x;
		pArcPts[2].y = corner.y;
		if( cpt = miGetArcPts(&arc, 3, &pArcPts))
		{
			/* by drawing with miFillSppPoly and setting the endpoints of the arc
			 * to be the corners, we assure that the cap will meet up with the
			 * rest of the line */
			miFillSppPoly(pDraw, pGC, cpt, pArcPts, xOrg, yOrg, xFtrans, yFtrans);
		}
		xfree(pArcPts);
		return;
	case JoinMiter:
		/*
		 * don't miter arcs with less than 11 degrees between them
		 */
		if (a < 169.0) {
			poly[0] = corner;
			poly[1] = center;
			poly[2] = otherCorner;
			bc2 = (corner.x - otherCorner.x) * (corner.x - otherCorner.x) +
			      (corner.y - otherCorner.y) * (corner.y - otherCorner.y);
			ec2 = bc2 / 4;
			ac2 = (corner.x - center.x) * (corner.x - center.x) +
			      (corner.y - center.y) * (corner.y - center.y);
			ae = sqrt (ac2 - ec2);
			de = ec2 / ae;
			e.x = (corner.x + otherCorner.x) / 2;
			e.y = (corner.y + otherCorner.y) / 2;
			poly[3].x = e.x + de * (e.x - center.x) / ae;
			poly[3].y = e.y + de * (e.y - center.y) / ae;
			poly[4] = corner;
			polyLen = 5;
			break;
		}
	case JoinBevel:
		poly[0] = corner;
		poly[1] = center;
		poly[2] = otherCorner;
		poly[3] = corner;
		polyLen = 4;
		break;
	}
	miFillSppPoly (pDraw, pGC, polyLen, poly, xOrg, yOrg, xFtrans, yFtrans);
}

/*ARGSUSED*/
static void
miArcCap (pDraw, pGC, pFace, end, xOrg, yOrg, xFtrans, yFtrans)
	DrawablePtr	pDraw;
	GCPtr		pGC;
	miArcFacePtr	pFace;
	int		end;
	int		xOrg, yOrg;
	double		xFtrans, yFtrans;
{
	SppPointRec	corner, otherCorner, center, endPoint, poly[5];

	corner = pFace->clock;
	otherCorner = pFace->counterClock;
	center = pFace->center;
	switch (pGC->capStyle) {
	case CapProjecting:
		poly[0].x = otherCorner.x;
		poly[0].y = otherCorner.y;
		poly[1].x = corner.x;
		poly[1].y = corner.y;
		poly[2].x = corner.x -
 				(center.y - corner.y);
		poly[2].y = corner.y +
 				(center.x - corner.x);
		poly[3].x = otherCorner.x -
 				(otherCorner.y - center.y);
		poly[3].y = otherCorner.y +
 				(otherCorner.x - center.x);
		poly[4].x = otherCorner.x;
		poly[4].y = otherCorner.y;
		miFillSppPoly (pDraw, pGC, 5, poly, xOrg, yOrg, xFtrans, yFtrans);
		break;
	case CapRound:
		/*
		 * miRoundCap just needs these to be unequal.
		 */
		endPoint = center;
		endPoint.x = endPoint.x + 100;
		miRoundCap (pDraw, pGC, center, endPoint, corner, otherCorner, 0,
			    -xOrg, -yOrg, xFtrans, yFtrans);
		break;
	}
}

/* MIROUNDCAP -- a private helper function
 * Put Rounded cap on end. pCenter is the center of this end of the line
 * pEnd is the center of the other end of the line. pCorner is one of the
 * two corners at this end of the line.  
 * NOTE:  pOtherCorner must be counter-clockwise from pCorner.
 */
/*ARGSUSED*/
static void
miRoundCap(pDraw, pGC, pCenter, pEnd, pCorner, pOtherCorner, fLineEnd,
     xOrg, yOrg, xFtrans, yFtrans)
    DrawablePtr	pDraw;
    GCPtr	pGC;
    SppPointRec	pCenter, pEnd;
    SppPointRec	pCorner, pOtherCorner;
    int		fLineEnd, xOrg, yOrg;
    double	xFtrans, yFtrans;
{
    int		cpt;
    double	width;
    double	miDatan2 ();
    SppArcRec	arc;
    SppPointPtr	pArcPts;

    width = (pGC->lineWidth ? pGC->lineWidth : 1);

    arc.x = pCenter.x - width/2;
    arc.y = pCenter.y - width/2;
    arc.width = width;
    arc.height = width;
    arc.angle1 = -miDatan2 (pCorner.y - pCenter.y, pCorner.x - pCenter.x);
    if(PTISEQUAL(pCenter, pEnd))
	arc.angle2 = - 180.0;
    else {
	arc.angle2 = -miDatan2 (pOtherCorner.y - pCenter.y, pOtherCorner.x - pCenter.x) - arc.angle1;
	if (arc.angle2 < 0)
	    arc.angle2 += 360.0;
    }
    pArcPts = (SppPointPtr) NULL;
    if( cpt = miGetArcPts(&arc, 0, &pArcPts))
    {
	/* by drawing with miFillSppPoly and setting the endpoints of the arc
	 * to be the corners, we assure that the cap will meet up with the
	 * rest of the line */
	miFillSppPoly(pDraw, pGC, cpt, pArcPts, -xOrg, -yOrg, xFtrans, yFtrans);
    }
    xfree(pArcPts);
}

/*
 * To avoid inaccuracy at the cardinal points, use trig functions
 * which are exact for those angles
 */

#ifndef M_PI
#define M_PI	3.14159265358979323846
#endif
#ifndef M_PI_2
#define M_PI_2	1.57079632679489661923
#endif

# define Dsin(d)	((d) == 0.0 ? 0.0 : ((d) == 90.0 ? 1.0 : sin(d*M_PI/180.0)))
# define Dcos(d)	((d) == 0.0 ? 1.0 : ((d) == 90.0 ? 0.0 : cos(d*M_PI/180.0)))
# define mod(a,b)	((a) >= 0 ? (a) % (b) : (b) - (-a) % (b))

static double
miDcos (a)
double	a;
{
	int	i;

	if (floor (a/90) == a/90) {
		i = (int) (a/90.0);
		switch (mod (i, 4)) {
		case 0: return 1;
		case 1: return 0;
		case 2: return -1;
		case 3: return 0;
		}
	}
	return cos (a * M_PI / 180.0);
}

static double
miDsin (a)
double	a;
{
	int	i;

	if (floor (a/90) == a/90) {
		i = (int) (a/90.0);
		switch (mod (i, 4)) {
		case 0: return 0;
		case 1: return 1;
		case 2: return 0;
		case 3: return -1;
		}
	}
	return sin (a * M_PI / 180.0);
}

static double
miDasin (v)
double	v;
{
    if (v == 0)
	return 0.0;
    if (v == 1.0)
	return 90.0;
    if (v == -1.0)
	return -90.0;
    return asin(v) * (180.0 / M_PI);
}

static double
miDatan2 (dy, dx)
double	dy, dx;
{
    if (dy == 0) {
	if (dx >= 0)
	    return 0.0;
	return 180.0;
    } else if (dx == 0) {
	if (dy > 0)
	    return 90.0;
	return -90.0;
    } else if (fabs (dy) == fabs (dx)) {
	if (dy > 0) {
	    if (dx > 0)
		return 45.0;
	    return 135.0;
	} else {
	    if (dx > 0)
		return 315.0;
	    return 225.0;
	}
    } else {
	return atan2 (dy, dx) * (180.0 / M_PI);
    }
}

#define REALLOC_STEP 10		/* how often to realloc */
#define NOARCCOMPRESSION	/* don't bother with this stuff */

/* MIGETARCPTS -- Converts an arc into a set of line segments -- a helper
 * routine for filled arc and line (round cap) code.
 * Returns the number of points in the arc.  Note that it takes a pointer
 * to a pointer to where it should put the points and an index (cpt).
 * This procedure allocates the space necessary to fit the arc points.
 * Sometimes it's convenient for those points to be at the end of an existing
 * array. (For example, if we want to leave a spare point to make sectors
 * instead of segments.)  So we pass in the Xalloc()ed chunk that contains the
 * array and an index saying where we should start stashing the points.
 * If there isn't an array already, we just pass in a null pointer and 
 * count on Xrealloc() to handle the null pointer correctly.
 */
static int
miGetArcPts(parc, cpt, ppPts)
    SppArcPtr	parc;	/* points to an arc */
    int		cpt;	/* number of points already in arc list */
    SppPointPtr	*ppPts; /* pointer to pointer to arc-list -- modified */
{
    double 	st,	/* Start Theta, start angle */
                et,	/* End Theta, offset from start theta */
		dt,	/* Delta Theta, angle to sweep ellipse */
		cdt,	/* Cos Delta Theta, actually 2 cos(dt) */
    		x0, y0,	/* the recurrence formula needs two points to start */
		x1, y1,
		x2, y2, /* this will be the new point generated */
		xc, yc; /* the center point */
    int		count, i;
    SppPointPtr	poly;
    DDXPointRec last;		/* last point on integer boundaries */
#ifndef NOARCCOMPRESSION
    double	xt, yt;
    int		axis, npts = 2;
#endif

    /* The spec says that positive angles indicate counterclockwise motion.
     * Given our coordinate system (with 0,0 in the upper left corner), 
     * the screen appears flipped in Y.  The easiest fix is to negate the
     * angles given */
    
    st = - parc->angle1;

    et = - parc->angle2;

    /* Try to get a delta theta that is within 1/2 pixel.  Then adjust it
     * so that it divides evenly into the total.
     * I'm just using cdt 'cause I'm lazy.
     */
    cdt = fmax(parc->width, parc->height)/2.0;
    if(cdt <= 0)
	return 0;
    if (cdt < 1.0)
	cdt = 1.0;
    dt = miDasin ( 1.0 / cdt ); /* minimum step necessary */
    count = et/dt;
    count = abs(count) + 1;
    dt = et/count;	
    count++;

    cdt = 2 * miDcos(dt);
#ifdef NOARCCOMPRESSION
    if (!(poly = (SppPointPtr) xrealloc((pointer)*ppPts,
					(cpt + count) * sizeof(SppPointRec))))
	return(0);
#else				/* ARCCOMPRESSION */
    if (!(poly = (SppPointPtr) xrealloc((pointer)*ppPts,
					(cpt + 2) * sizeof(SppPointRec))))
	return(0);
#endif				/* ARCCOMPRESSION */
    *ppPts = poly;

    xc = parc->width/2.0;		/* store half width and half height */
    yc = parc->height/2.0;
#ifndef NOARCCOMPRESSION
    axis = (xc >= yc) ? X_AXIS : Y_AXIS;
#endif
    
    x0 = xc * miDcos(st);
    y0 = yc * miDsin(st);
    x1 = xc * miDcos(st + dt);
    y1 = yc * miDsin(st + dt);
    xc += parc->x;		/* by adding initial point, these become */
    yc += parc->y;		/* the center point */

    poly[cpt].x = (xc + x0);
    poly[cpt].y = (yc + y0);
    last.x = ROUNDTOINT( poly[cpt + 1].x = (xc + x1) );
    last.y = ROUNDTOINT( poly[cpt + 1].y = (yc + y1) );

    for(i = 2; i < count; i++)
    {
	x2 = cdt * x1 - x0;
	y2 = cdt * y1 - y0;

#ifdef NOARCCOMPRESSION
 	poly[cpt + i].x = (xc + x2);
 	poly[cpt + i].y = (yc + y2);
#else				/* ARCCOMPRESSION */
	xt = xc + x2;
	yt = yc + y2;
 	if (((axis == X_AXIS) ?
	     (ROUNDTOINT(yt) != last.y) :
	     (ROUNDTOINT(xt) != last.x)) ||
	    i > count - 3)	/* insure 2 at the end */
 	{
	    /* allocate more space if we are about to need it */
	    /* include 1 extra in case minor axis swaps */
 	    if ((npts - 2) % REALLOC_STEP == 0)
	    {
 		if (!(poly = (SppPointPtr)
		      xrealloc((pointer) poly,
			       ((npts + REALLOC_STEP + cpt) *
				sizeof(SppPointRec)))))
		    return(0);
		*ppPts = poly;
	    }
	    /* check if we just switched direction in the minor axis */
	    if (((poly[cpt + npts - 2].y - poly[cpt + npts - 1].y > 0.0) ?
		 (yt - poly[cpt + npts - 1].y > 0.0) :
		 (poly[cpt + npts - 1].y - yt > 0.0)) ||
		((poly[cpt + npts - 2].x - poly[cpt + npts - 1].x > 0.0) ?
		 (xt - poly[cpt + npts - 1].x > 0.0) :
		 (poly[cpt + npts - 1].x - xt > 0.0)))
	    {
		/* Since the minor axis direction just switched, the final *
		 * point before the change must be included, or the        *
		 * following segment will begin before the minor swap.     */
		poly[cpt + npts].x = xc + x1;
		poly[cpt + npts].y = yc + y1;
		npts++;
		if ((npts - 2) % REALLOC_STEP == 0)
		{
		    if (!(poly = (SppPointPtr)
			  xrealloc((pointer) poly,
				   ((npts + REALLOC_STEP + cpt) *
				    sizeof(SppPointRec)))))
			return(0);
		    *ppPts = poly;
		}
	    }
 	    last.x = ROUNDTOINT( poly[cpt + npts].x = xt );
 	    last.y = ROUNDTOINT( poly[cpt + npts].y = yt );
 	    npts++;
 	}
#endif				/* ARCCOMPRESSION */

	x0 = x1; y0 = y1;
	x1 = x2; y1 = y2;
    }
#ifndef NOARCCOMPRESSION	/* i.e.:  ARCCOMPRESSION */
    count = i = npts;
#endif				/* ARCCOMPRESSION */
    /* adjust the last point */
    if (abs(parc->angle2) >= 360.0)
	poly[cpt +i -1] = poly[0];
    else {
	poly[cpt +i -1].x = (miDcos(st + et) * parc->width/2.0 + xc);
	poly[cpt +i -1].y = (miDsin(st + et) * parc->height/2.0 + yc);
    }

    return(count);
}

struct arcData {
	double	x0, y0, x1, y1;
	int	selfJoin;
};

# define ADD_REALLOC_STEP	20

static void
addCap (capsp, ncapsp, sizep, end, arcIndex)
	miArcCapPtr	*capsp;
	int		*ncapsp, *sizep;
	int		end, arcIndex;
{
	int newsize;
	miArcCapPtr	cap;

	if (*ncapsp == *sizep)
	{
	    newsize = *sizep + ADD_REALLOC_STEP;
	    cap = (miArcCapPtr) xrealloc (*capsp,
					  newsize * sizeof (**capsp));
	    if (!cap)
		return;
	    *sizep = newsize;
	    *capsp = cap;
	}
	cap = &(*capsp)[*ncapsp];
	cap->end = end;
	cap->arcIndex = arcIndex;
	++*ncapsp;
}

static void
addJoin (joinsp, njoinsp, sizep, end0, index0, phase0, end1, index1, phase1)
	miArcJoinPtr	*joinsp;
	int		*njoinsp, *sizep;
	int		end0, index0, end1, index1;
{
	int newsize;
	miArcJoinPtr	join;

	if (*njoinsp == *sizep)
	{
	    newsize = *sizep + ADD_REALLOC_STEP;
	    join = (miArcJoinPtr) xrealloc (*joinsp,
					    newsize * sizeof (**joinsp));
	    if (!join)
		return;
	    *sizep = newsize;
	    *joinsp = join;
	}
	join = &(*joinsp)[*njoinsp];
	join->end0 = end0;
	join->arcIndex0 = index0;
	join->phase0 = phase0;
	join->end1 = end1;
	join->arcIndex1 = index1;
	join->phase1 = phase1;
	++*njoinsp;
}

static miArcDataPtr
addArc (arcsp, narcsp, sizep, xarc)
	miArcDataPtr	*arcsp;
	int		*narcsp, *sizep;
	xArc		xarc;
{
	int newsize;
	miArcDataPtr	arc;

	if (*narcsp == *sizep)
	{
	    newsize = *sizep + ADD_REALLOC_STEP;
	    arc = (miArcDataPtr) xrealloc (*arcsp,
					   newsize * sizeof (**arcsp));
	    if (!arc)
		return (miArcDataPtr)NULL;
	    *sizep = newsize;
	    *arcsp = arc;
	}
	arc = &(*arcsp)[*narcsp];
	arc->arc = xarc;
	++*narcsp;
	return arc;
}

static void
miFreeArcs(arcs, pGC)
    miPolyArcPtr arcs;
    GCPtr pGC;
{
	int iphase;

	for (iphase = ((pGC->lineStyle == LineDoubleDash) ? 1 : 0);
 	     iphase >= 0;
	     iphase--)
	{
	    if (arcs[iphase].narcs > 0)
		xfree(arcs[iphase].arcs);
	    if (arcs[iphase].njoins > 0)
		xfree(arcs[iphase].joins);
	    if (arcs[iphase].ncaps > 0)
		xfree(arcs[iphase].caps);
	}
	xfree(arcs);
}

/*
 * map angles to radial distance.  This only deals with the first quadrant
 */

/*
 * a polygonal approximation to the arc for computing arc lengths
 */

# define DASH_MAP_SIZE	91

# define dashIndexToAngle(di)	((((double) (di)) * 90.0) / ((double) DASH_MAP_SIZE - 1))
# define xAngleToDashIndex(xa)	((((long) (xa)) * (DASH_MAP_SIZE - 1)) / (90 * 64))
# define dashIndexToXAngle(di)	((((long) (di)) * (90 * 64)) / (DASH_MAP_SIZE - 1))
# define dashXAngleStep	(((double) (90 * 64)) / ((double) (DASH_MAP_SIZE - 1)))

typedef struct {
	double	map[DASH_MAP_SIZE];
} dashMap;

static void
computeDashMap (arcp, map)
	xArc	*arcp;
	dashMap	*map;
{
	int	di;
	double	a, x, y, prevx, prevy, dist;

	for (di = 0; di < DASH_MAP_SIZE; di++) {
		a = dashIndexToAngle (di);
		x = ((double) arcp->width / 2.0) * miDcos (a);
		y = ((double) arcp->height / 2.0) * miDsin (a);
		if (di == 0) {
			map->map[di] = 0.0;
		} else {
			dist = hypot (x - prevx, y - prevy);
			map->map[di] = map->map[di - 1] + dist;
		}
		prevx = x;
		prevy = y;
	}
}

/* this routine is a bit gory */

static miPolyArcPtr
miComputeArcs (parcs, narcs, pGC)
	xArc	*parcs;
	int	narcs;
	GCPtr	pGC;
{
	int		isDashed, isDoubleDash;
	int		dashOffset;
	miPolyArcPtr	arcs;
	int		start, i, j, k, nexti, nextk;
	int		joinSize[2];
	int		capSize[2];
	int		arcSize[2];
	int		angle2;
	double		a0, a1;
	struct arcData	*data;
	miArcDataPtr	arc;
	xArc		xarc;
	int		iphase, prevphase, joinphase;
	int		arcsJoin;
	int		selfJoin;

	int		iDash, dashRemaining;
	int		iDashStart, dashRemainingStart, iphaseStart;
	int		startAngle, spanAngle, endAngle, backwards;
	int		prevDashAngle, dashAngle;
	dashMap		map;

	isDashed = !(pGC->lineStyle == LineSolid);
	isDoubleDash = (pGC->lineStyle == LineDoubleDash);
	dashOffset = pGC->dashOffset;

	data = (struct arcData *) ALLOCATE_LOCAL (narcs * sizeof (struct arcData));
	if (!data)
	    return (miPolyArcPtr)NULL;
	arcs = (miPolyArcPtr) xalloc (sizeof (*arcs) * (isDoubleDash ? 2 : 1));
	if (!arcs)
	{
	    DEALLOCATE_LOCAL(data);
	    return (miPolyArcPtr)NULL;
	}
	for (i = 0; i < narcs; i++) {
		a0 = todeg (parcs[i].angle1);
		angle2 = parcs[i].angle2;
		if (angle2 > FULLCIRCLE)
			angle2 = FULLCIRCLE;
		else if (angle2 < -FULLCIRCLE)
			angle2 = -FULLCIRCLE;
		data[i].selfJoin = angle2 == FULLCIRCLE || angle2 == -FULLCIRCLE;
		a1 = todeg (parcs[i].angle1 + angle2);
		data[i].x0 = parcs[i].x + (double) parcs[i].width / 2 * (1 + miDcos (a0));
		data[i].y0 = parcs[i].y + (double) parcs[i].height / 2 * (1 - miDsin (a0));
		data[i].x1 = parcs[i].x + (double) parcs[i].width / 2 * (1 + miDcos (a1));
		data[i].y1 = parcs[i].y + (double) parcs[i].height / 2 * (1 - miDsin (a1));
	}

	for (iphase = 0; iphase < (isDoubleDash ? 2 : 1); iphase++) {
		arcs[iphase].njoins = 0;
		arcs[iphase].joins = 0;
		joinSize[iphase] = 0;
		
		arcs[iphase].ncaps = 0;
		arcs[iphase].caps = 0;
		capSize[iphase] = 0;
		
		arcs[iphase].narcs = 0;
		arcs[iphase].arcs = 0;
		arcSize[iphase] = 0;
	}

	iphase = 0;
	if (isDashed) {
		iDash = 0;
		dashRemaining = pGC->dash[0];
	 	while (dashOffset > 0) {
			if (dashOffset >= dashRemaining) {
				dashOffset -= dashRemaining;
				iphase = iphase ? 0 : 1;
				iDash++;
				if (iDash == pGC->numInDashList)
				    iDash = 0;
				dashRemaining = pGC->dash[iDash];
			} else {
				dashRemaining -= dashOffset;
				dashOffset = 0;
			}
		}
		iDashStart = iDash;
		dashRemainingStart = dashRemaining;
	}
	iphaseStart = iphase;

	for (i = narcs - 1; i >= 0; i--) {
		j = i + 1;
		if (j == narcs)
			j = 0;
		if (data[i].selfJoin || i == j ||
		     (UNEQUAL (data[i].x1, data[j].x0) ||
		      UNEQUAL (data[i].y1, data[j].y0)))
 		{
			if (iphase == 0 || isDoubleDash)
				addCap (&arcs[iphase].caps, &arcs[iphase].ncaps,
	 				&capSize[iphase], RIGHT_END, 0);
			break;
		}
	}
	start = i + 1;
	if (start == narcs)
		start = 0;
	i = start;
	for (;;) {
		j = i + 1;
		if (j == narcs)
			j = 0;
		nexti = i+1;
		if (nexti == narcs)
			nexti = 0;
		if (isDashed) {
			/*
			 * precompute an approximation map
			 */
			computeDashMap (&parcs[i], &map);
			/*
			 * compute each individual dash segment using the path
			 * length function
			 */
			startAngle = parcs[i].angle1;
			spanAngle = parcs[i].angle2;
			if (spanAngle > FULLCIRCLE)
				spanAngle = FULLCIRCLE;
			else if (spanAngle < -FULLCIRCLE)
				spanAngle = -FULLCIRCLE;
			if (startAngle < 0)
				startAngle = FULLCIRCLE - (-startAngle) % FULLCIRCLE;
			if (startAngle >= FULLCIRCLE)
				startAngle = startAngle % FULLCIRCLE;
			endAngle = startAngle + spanAngle;
			backwards = spanAngle < 0;
			dashAngle = startAngle;
			selfJoin = data[i].selfJoin &&
 				    (iphase == 0 || isDoubleDash);
			/*
			 * add dashed arcs to each bucket
			 */
			arc = 0;
			while (dashAngle != endAngle) {
				prevDashAngle = dashAngle;
				dashAngle = computeAngleFromPath (prevDashAngle, endAngle,
							&map, &dashRemaining, backwards);
				/* avoid troubles with huge arcs and small dashes */
				if (dashAngle == prevDashAngle) {
					if (backwards)
						dashAngle--;
					else
						dashAngle++;
				}
				if (iphase == 0 || isDoubleDash) {
					xarc = parcs[i];
					spanAngle = prevDashAngle;
    					if (spanAngle < 0)
					    spanAngle = FULLCIRCLE - (-spanAngle) % FULLCIRCLE;
					if (spanAngle >= FULLCIRCLE)
					    spanAngle = spanAngle % FULLCIRCLE;
					xarc.angle1 = spanAngle;
					spanAngle = dashAngle - prevDashAngle;
					if (backwards) {
						if (dashAngle > prevDashAngle)
							spanAngle = - FULLCIRCLE + spanAngle;
					} else {
						if (dashAngle < prevDashAngle)
							spanAngle = FULLCIRCLE + spanAngle;
					}
					if (spanAngle > FULLCIRCLE)
					    spanAngle = FULLCIRCLE;
					if (spanAngle < -FULLCIRCLE)
					    spanAngle = -FULLCIRCLE;
					xarc.angle2 = spanAngle;
					arc = addArc (&arcs[iphase].arcs, &arcs[iphase].narcs,
 							&arcSize[iphase], xarc);
					if (!arc)
					    goto arcfail;
					/*
					 * cap each end of an on/off dash
					 */
					if (!isDoubleDash) {
						if (prevDashAngle != startAngle) {
							addCap (&arcs[iphase].caps,
 								&arcs[iphase].ncaps,
 								&capSize[iphase], RIGHT_END,
 								arc - arcs[iphase].arcs);
							
						}
						if (dashAngle != endAngle) {
							addCap (&arcs[iphase].caps,
 								&arcs[iphase].ncaps,
 								&capSize[iphase], LEFT_END,
 								arc - arcs[iphase].arcs);
						}
					}
					arc->cap = arcs[iphase].ncaps;
					arc->join = arcs[iphase].njoins;
					arc->render = 0;
					arc->selfJoin = 0;
					if (dashAngle == endAngle)
						arc->selfJoin = selfJoin;
				}
				prevphase = iphase;
				if (dashRemaining <= 0) {
					++iDash;
					if (iDash == pGC->numInDashList)
						iDash = 0;
					iphase = iphase ? 0:1;
					dashRemaining = pGC->dash[iDash];
				}
			}
			/*
			 * make sure a place exists for the position data when
			 * drawing a zero-length arc
			 */
			if (startAngle == endAngle) {
				prevphase = iphase;
				if (!isDoubleDash && iphase == 1)
					prevphase = 0;
				arc = addArc (&arcs[prevphase].arcs, &arcs[prevphase].narcs,
					      &arcSize[prevphase], parcs[i]);
				if (!arc)
				    goto arcfail;
				arc->join = arcs[prevphase].njoins;
				arc->cap = arcs[prevphase].ncaps;
				arc->selfJoin = data[i].selfJoin;
			}
		} else {
			arc = addArc (&arcs[iphase].arcs, &arcs[iphase].narcs,
 				      &arcSize[iphase], parcs[i]);
			if (!arc)
			    goto arcfail;
			arc->join = arcs[iphase].njoins;
			arc->cap = arcs[iphase].ncaps;
			arc->selfJoin = data[i].selfJoin;
			prevphase = iphase;
		}
		if (prevphase == 0 || isDoubleDash)
			k = arcs[prevphase].narcs - 1;
		if (iphase == 0 || isDoubleDash)
			nextk = arcs[iphase].narcs;
		if (nexti == start) {
			nextk = 0;
			if (isDashed) {
				iDash = iDashStart;
				iphase = iphaseStart;
				dashRemaining = dashRemainingStart;
			}
		}
		arcsJoin = narcs > 1 && i != j &&
	 		    ISEQUAL (data[i].x1, data[j].x0) &&
			    ISEQUAL (data[i].y1, data[j].y0) &&
			    !data[i].selfJoin && !data[j].selfJoin;
		if (arc)
		{
			if (arcsJoin)
				arc->render = 0;
			else
				arc->render = 1;
		}
		if (arcsJoin &&
		    (prevphase == 0 || isDoubleDash) &&
		    (iphase == 0 || isDoubleDash))
 		{
			joinphase = iphase;
			if (isDoubleDash) {
				if (nexti == start)
					joinphase = iphaseStart;
				/*
				 * if the join is right at the dash,
				 * draw the join in foreground
				 * This is because the foreground
				 * arcs are computed second, the results
				 * of which are needed to draw the join
				 */
				if (joinphase != prevphase)
					joinphase = 0;
			}
			if (joinphase == 0 || isDoubleDash) {
				addJoin (&arcs[joinphase].joins,
 					 &arcs[joinphase].njoins,
 					 &joinSize[joinphase],
	 				 LEFT_END, k, prevphase,
	 				 RIGHT_END, nextk, iphase);
				arc->join = arcs[prevphase].njoins;
			}
		} else {
			/*
			 * cap the left end of this arc
			 * unless it joins itself
			 */
			if ((prevphase == 0 || isDoubleDash) &&
			    !arc->selfJoin)
			{
				addCap (&arcs[prevphase].caps, &arcs[prevphase].ncaps,
 					&capSize[prevphase], LEFT_END, k);
				arc->cap = arcs[prevphase].ncaps;
			}
			if (isDashed && !arcsJoin) {
				iDash = iDashStart;
				iphase = iphaseStart;
				dashRemaining = dashRemainingStart;
			}
			nextk = arcs[iphase].narcs;
			if (nexti == start) {
				nextk = 0;
				iDash = iDashStart;
				iphase = iphaseStart;
				dashRemaining = dashRemainingStart;
			}
			/*
			 * cap the right end of the next arc.  If the
			 * next arc is actually the first arc, only
			 * cap it if it joins with this arc.  This
			 * case will occur when the final dash segment
			 * of an on/off dash is off.  Of course, this
			 * cap will be drawn at a strange time, but that
			 * hardly matters...
			 */
			if ((iphase == 0 || isDoubleDash) &&
			    (nexti != start || arcsJoin && isDashed))
				addCap (&arcs[iphase].caps, &arcs[iphase].ncaps,
 					&capSize[iphase], RIGHT_END, nextk);
		}
		i = nexti;
		if (i == start)
			break;
	}
	/*
	 * make sure the last section is rendered
	 */
	for (iphase = 0; iphase < (isDoubleDash ? 2 : 1); iphase++)
		if (arcs[iphase].narcs > 0) {
			arcs[iphase].arcs[arcs[iphase].narcs-1].render = 1;
			arcs[iphase].arcs[arcs[iphase].narcs-1].join =
			         arcs[iphase].njoins;
			arcs[iphase].arcs[arcs[iphase].narcs-1].cap =
			         arcs[iphase].ncaps;
		}
	DEALLOCATE_LOCAL(data);
	return arcs;
arcfail:
	miFreeArcs(arcs, pGC);
	DEALLOCATE_LOCAL(data);
	return (miPolyArcPtr)NULL;
}

static double
angleToLength (angle, map)
	int	angle;
	dashMap	*map;
{
	double	len, excesslen, sidelen = map->map[DASH_MAP_SIZE - 1], totallen;
	int	di;
	int	excess;
	Bool	oddSide = FALSE;

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
	double	len;
	dashMap	*map;
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
	int	startAngle, endAngle;	/* normalized absolute angles in *64 degrees */
	dashMap	*map;
	int	*lenp;
	int	backwards;
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

/*
 * scan convert wide arcs.
 */

/*
 * draw zero width/height arcs
 */

static void
drawZeroArc (pDraw, pGC, tarc, left, right)
    DrawablePtr   pDraw;
    GCPtr         pGC;
    xArc          tarc;
    miArcFacePtr	right, left;
{
	double	x0, y0, x1, y1, w, h, x, y;
	double	xmax, ymax, xmin, ymin;
	int	a0, a1;
	double	a, startAngle, endAngle;
	double	l, lx, ly;

	l = pGC->lineWidth;
	if (l == 0)
		l = 1;
	l /= 2;
	a0 = tarc.angle1;
	a1 = tarc.angle2;
	if (a1 > FULLCIRCLE)
		a1 = FULLCIRCLE;
	else if (a1 < -FULLCIRCLE)
		a1 = -FULLCIRCLE;
	w = tarc.width / 2.0;
	h = tarc.height / 2.0;
	/*
	 * play in X coordinates right away
	 */
	startAngle = - ((double) a0 / 64.0);
	endAngle = - ((double) (a0 + a1) / 64.0);
	
	xmax = -w;
	xmin = w;
	ymax = -h;
	ymin = h;
	a = startAngle;
	for (;;)
	{
		x = w * miDcos(a);
		y = h * miDsin(a);
		if (a == startAngle)
		{
			x0 = x;
			y0 = y;
		}
		if (a == endAngle)
		{
			x1 = x;
			y1 = y;
		}
		if (x > xmax)
			xmax = x;
		if (x < xmin)
			xmin = x;
		if (y > ymax)
			ymax = y;
		if (y < ymin)
			ymin = y;
		if (a == endAngle)
			break;
		if (a1 < 0)	/* clockwise */
		{
			if (floor (a / 90.0) == floor (endAngle / 90.0))
				a = endAngle;
			else
				a = 90 * (floor (a/90.0) + 1);
		}
		else
		{
			if (ceil (a / 90.0) == ceil (endAngle / 90.0))
				a = endAngle;
			else
				a = 90 * (ceil (a/90.0) - 1);
		}
	}
	lx = ly = l;
	if ((x1 - x0) + (y1 - y0) < 0)
	    lx = ly = -l;
	if (h)
	    ly = 0.0;
	else
	    lx = 0.0;
	if (right)
	{
	    right->center.x = x0;
	    right->center.y = y0;
	    right->clock.x = x0 - lx;
	    right->clock.y = y0 - ly;
	    right->counterClock.x = x0 + lx;
	    right->counterClock.y = y0 + ly;
	}
	if (left)
 	{
	    left->center.x = x1;
	    left->center.y = y1;
	    left->clock.x = x1 + lx;
	    left->clock.y = y1 + ly;
	    left->counterClock.x = x1 - lx;
	    left->counterClock.y = y1 - ly;
	}
	
	x0 = xmin;
	x1 = xmax;
	y0 = ymin;
	y1 = ymax;
	if (ymin != y1) {
		xmin = -l;
		xmax = l;
	} else {
		ymin = -l;
		ymax = l;
	}
	if (xmax != xmin && ymax != ymin) {
		int	minx, maxx, miny, maxy;
		xRectangle  rect;

		minx = ICEIL (xmin + w) + tarc.x;
		maxx = ICEIL (xmax + w) + tarc.x;
		miny = ICEIL (ymin + h) + tarc.y;
		maxy = ICEIL (ymax + h) + tarc.y;
		rect.x = minx;
		rect.y = miny;
		rect.width = maxx - minx;
		rect.height = maxy - miny;
		(*pGC->ops->PolyFillRect) (pDraw, pGC, 1, &rect);
	}
}

# define BINARY_LIMIT	(0.1)
# define NEWTON_LIMIT	(0.0000001)

struct bound {
	double	min, max;
};

struct line {
	double	m, b;
	int	valid;
};

/*
 * these are all y value bounds
 */

struct arc_bound {
	struct bound	ellipse;
	struct bound	inner;
	struct bound	outer;
	struct bound	right;
	struct bound	left;
};

# define BINARY_TABLE_SIZE	(512)

struct accelerators {
	double		tail_y;
	double		h2;
	double		w2;
	double		h4;
	double		w4;
	double		h2mw2;
	double		wh2mw2;
	double		wh4;
	struct line	left, right;
	double		innerTable[BINARY_TABLE_SIZE+1];
	double		outerTable[BINARY_TABLE_SIZE+1];
	char		innerValid[BINARY_TABLE_SIZE+1];
	char		outerValid[BINARY_TABLE_SIZE+1];
};

struct arc_def {
	double	w, h, l;
	double	a0, a1;
};

#ifdef USE_INLINE
inline static const double Sqrt (const double x)
#else
static double
Sqrt (x)
double	x;
#endif
{
	if (x < 0) {
		if (x > -NEWTON_LIMIT)
			return 0;
		else
			FatalError ("miarc.c: Sqrt of negative number %g\n", x);
	}
	return sqrt (x);
}

#define boundedLt(value, bounds) \
	((bounds).min <= (value) && (value) < (bounds).max)

#define boundedLe(value, bounds)\
	((bounds).min <= (value) && (value) <= (bounds).max)

/*
 * this computes the ellipse y value associated with the
 * bottom of the tail.
 */

# define CUBED_ROOT_2	1.2599210498948732038115849718451499938964
# define CUBED_ROOT_4	1.5874010519681993173435330390930175781250

static void
tailEllipseY (def, acc)
	struct arc_def		*def;
	struct accelerators	*acc;
{
	double		t;

	acc->tail_y = 0.0;
	if (def->w == def->h)
	    return;
	t = def->l * def->w;
	if (def->w > def->h) {
	    if (t < acc->h2 + acc->h2)
		return;
	} else {
	    if (t > acc->h2 + acc->h2)
		return;
	}
	t = def->h * t;
	t = (CUBED_ROOT_4 * acc->h2 - cbrt(t * t)) / acc->h2mw2;
	if (t > 0.0)
	    acc->tail_y = def->h / CUBED_ROOT_2 * sqrt(t);
}

/*
 * inverse functions -- compute edge coordinates
 * from the ellipse
 */

static double
outerXfromXY (x, y, def, acc)
	double			x, y;
	struct arc_def		*def;
	struct accelerators	*acc;
{
	return x + (x * acc->h2 * def->l) /
		   (2 * Sqrt (x*x *acc->h4 + y*y * acc->w4));
}

static double
outerXfromY (y, def, acc)
	double			y;
	struct arc_def		*def;
	struct accelerators	*acc;
{
	double	x;

	x = def->w * Sqrt ((acc->h2 - (y*y)) / acc->h2);

	return x + (x * acc->h2 * def->l) /
		   (2 * Sqrt (x*x *acc->h4 + y*y * acc->w4));
}

static double
outerYfromXY (x, y, def, acc)
	double		x, y;
	struct arc_def		*def;
	struct accelerators	*acc;
{
	return y + (y * acc->w2 * def->l) /
		   (2 * Sqrt (x*x * acc->h4 + y*y * acc->w4));
}

static double
outerYfromY (y, def, acc)
	double	y;
	struct arc_def		*def;
	struct accelerators	*acc;
{
	double	x;

	x = def->w * Sqrt ((acc->h2 - (y*y)) / acc->h2);

	return y + (y * acc->w2 * def->l) /
		   (2 * Sqrt (x*x * acc->h4 + y*y * acc->w4));
}

static double
innerXfromXY (x, y, def, acc)
	double			x, y;
	struct arc_def		*def;
	struct accelerators	*acc;
{
	return x - (x * acc->h2 * def->l) /
		   (2 * Sqrt (x*x * acc->h4 + y*y * acc->w4));
}

static double
innerXfromY (y, def, acc)
	double			y;
	struct arc_def		*def;
	struct accelerators	*acc;
{
	double	x;

	x = def->w * Sqrt ((acc->h2 - (y*y)) / acc->h2);
	
	return x - (x * acc->h2 * def->l) /
		   (2 * Sqrt (x*x * acc->h4 + y*y * acc->w4));
}

static double
innerYfromXY (x, y, def, acc)
	double			x, y;
	struct arc_def		*def;
	struct accelerators	*acc;
{
	return y - (y * acc->w2 * def->l) /
		   (2 * Sqrt (x*x * acc->h4 + y*y * acc->w4));
}

static double
innerYfromY (y, def, acc)
	double	y;
	struct arc_def		*def;
	struct accelerators	*acc;
{
	double	x;

	x = def->w * Sqrt ((acc->h2 - (y*y)) / acc->h2);

	return y - (y * acc->w2 * def->l) /
		   (2 * Sqrt (x*x * acc->h4 + y*y * acc->w4));
}

static void
computeLine (x1, y1, x2, y2, line)
	double		x1, y1, x2, y2;
	struct line	*line;
{
	if (y1 == y2)
		line->valid = 0;
	else {
		line->m = (x1 - x2) / (y1 - y2);
		line->b = x1  - y1 * line->m;
		line->valid = 1;
	}
}

static double
intersectLine (y, line)
	double		y;
	struct line	*line;
{
	return line->m * y + line->b;
}

/*
 * compute various accelerators for an ellipse.  These
 * are simply values that are used repeatedly in
 * the computations
 */

static void
computeAcc (def, acc)
	struct arc_def		*def;
	struct accelerators	*acc;
{
	int	i;

	acc->h2 = def->h * def->h;
	acc->w2 = def->w * def->w;
	acc->h4 = acc->h2 * acc->h2;
	acc->w4 = acc->w2 * acc->w2;
	acc->h2mw2 = acc->h2 - acc->w2;
	acc->wh2mw2 = def->w * acc->h2mw2;
	acc->wh4 = def->w * acc->h4;
	tailEllipseY (def, acc);
	for (i = 0; i <= BINARY_TABLE_SIZE; i++)
		acc->innerValid[i] = acc->outerValid[i] = 0;
}
		
/*
 * compute y value bounds of various portions of the arc,
 * the outer edge, the ellipse and the inner edge.
 */

static void
computeBound (def, bound, acc, right, left)
	struct arc_def		*def;
	struct arc_bound	*bound;
	struct accelerators	*acc;
	miArcFacePtr		right, left;
{
	double		t;
	double		innerTaily;
	double		tail_y;
	struct bound	innerx, outerx;
	struct bound	ellipsex;

	bound->ellipse.min = Dsin (def->a0) * def->h;
	bound->ellipse.max = Dsin (def->a1) * def->h;
	if (def->a0 == 45 && def->w == def->h)
		ellipsex.min = bound->ellipse.min;
	else
		ellipsex.min = Dcos (def->a0) * def->w;
	if (def->a1 == 45 && def->w == def->h)
		ellipsex.max = bound->ellipse.max;
	else
		ellipsex.max = Dcos (def->a1) * def->w;
	bound->outer.min = outerYfromXY (ellipsex.min, bound->ellipse.min, def, acc);
	bound->outer.max = outerYfromXY (ellipsex.max, bound->ellipse.max, def, acc);
	bound->inner.min = innerYfromXY (ellipsex.min, bound->ellipse.min, def, acc);
	bound->inner.max = innerYfromXY (ellipsex.max, bound->ellipse.max, def, acc);

	outerx.min = outerXfromXY (ellipsex.min, bound->ellipse.min, def, acc);
	outerx.max = outerXfromXY (ellipsex.max, bound->ellipse.max, def, acc);
	innerx.min = innerXfromXY (ellipsex.min, bound->ellipse.min, def, acc);
	innerx.max = innerXfromXY (ellipsex.max, bound->ellipse.max, def, acc);
	
	/*
	 * save the line end points for the
	 * cap code to use.  Careful here, these are
	 * in cartesean coordinates (y increasing upwards)
	 * while the cap code uses inverted coordinates
	 * (y increasing downwards)
	 */

	if (right) {
		right->counterClock.y = bound->outer.min;
		right->counterClock.x = outerx.min;
		right->center.y = bound->ellipse.min;
		right->center.x = ellipsex.min;
		right->clock.y = bound->inner.min;
		right->clock.x = innerx.min;
	}

	if (left) {
		left->clock.y = bound->outer.max;
		left->clock.x = outerx.max;
		left->center.y = bound->ellipse.max;
		left->center.x = ellipsex.max;
		left->counterClock.y = bound->inner.max;
		left->counterClock.x = innerx.max;
	}

	bound->left.min = bound->inner.max;
	bound->left.max = bound->outer.max;
	bound->right.min = bound->inner.min;
	bound->right.max = bound->outer.min;

	computeLine (innerx.min, bound->inner.min, outerx.min, bound->outer.min,
		      &acc->right);
	computeLine (innerx.max, bound->inner.max, outerx.max, bound->outer.max,
		     &acc->left);

	if (bound->inner.min > bound->inner.max) {
		t = bound->inner.min;
		bound->inner.min = bound->inner.max;
		bound->inner.max = t;
	}
	tail_y = acc->tail_y;
	if (tail_y > bound->ellipse.max)
		tail_y = bound->ellipse.max;
	else if (tail_y < bound->ellipse.min)
		tail_y = bound->ellipse.min;
	innerTaily = innerYfromY (tail_y, def, acc);
	if (bound->inner.min > innerTaily)
		bound->inner.min = innerTaily;
	if (bound->inner.max < innerTaily)
		bound->inner.max = innerTaily;
}

/*
 * using newtons method and a binary search, compute the ellipse y value
 * associated with the given edge value (either outer or
 * inner).  This is the heart of the scan conversion code and
 * is generally called three times for each span.  It should
 * be optimized further.
 *
 * the general idea here is to solve the equation:
 *
 *                               w^2 * l
 *   edge_y = y + y * -------------------------------
 *                    2 * sqrt (x^2 * h^4 + y^2 * w^4)
 *
 * for y.  (x, y) is a point on the ellipse, so x can be
 * found from y:
 *
 *                ( h^2 - y^2 )
 *   x = w * sqrt ( --------- )
 *                (    h^2    )
 *
 * The information given at the start of the search
 * is two points which are known to bound the desired
 * solution, a binary search starts with these two points
 * and converges close to a solution, which is then
 * refined with newtons method.  Newtons method
 * cannot be used in isolation as it does not always
 * converge to the desired solution without a close
 * starting point, the binary search simply provides
 * that point.  Increasing the solution interval for
 * the binary search will certainly speed up the
 * solution, but may generate a range which causes
 * the newtons method to fail.  This needs study.
 */

# define binaryIndexFromY(y, def)	(((y) / (def)->h) * ((double) BINARY_TABLE_SIZE))
# define yFromBinaryIndex(i, def)	((((double) i) / ((double) BINARY_TABLE_SIZE)) * (def)->h)

#ifdef notdef

static double
binaryValue (i, def, acc, valid, table, f)
	int			i;
	struct arc_def		*def;
	struct accelerators	*acc;
	char			*valid;
	double			*table;
	double			(*f)();
{
	if (!valid[i]) {
		valid[i] = 1;
		table[i] = f (yFromBinaryIndex (i, def), def, acc);
	}
	return table[i];
}
#else

# define binaryValue(i, def, acc, valid, table, f)\
	(valid[i] ? table[i] : (valid[i] = 1, table[i] = f (yFromBinaryIndex (i, def), def, acc)))
#endif

static double
ellipseY (edge_y, def, acc, outer, y0, y1)
	double			edge_y;
	struct arc_def		*def;
	struct accelerators	*acc;
	register double		y0, y1;
{
	register double	w, l, h2, h4, w2, w4, x, y2;
	double		newtony, binaryy;
	double		value0, value1;
	double		newtonvalue, binaryvalue;
	double		minY, maxY;
	double		binarylimit;
	double		(*f)();
	int		index0, index1, newindex;
	char		*valid;
	double		*table;
	
	/*
	 * compute some accelerators
	 */
	w = def->w;
	if (outer) {
		f = outerYfromY;
		l = def->l;
		table = acc->outerTable;
		valid = acc->outerValid;
	} else {
		f = innerYfromY;
		l = -def->l;
		table = acc->innerTable;
		valid = acc->innerValid;
	}
	h2 = acc->h2;
	h4 = acc->h4;
	w2 = acc->w2;
	w4 = acc->w4;

	/*
	 * make sure the arguments are in the right order
	 */
	if (y0 > y1) {
		binaryy = y0;
		y0 = y1;
		y1 = binaryy;
	}
	maxY = y1;
	minY = y0;

	index0 = binaryIndexFromY (y0, def);
	index1 = binaryIndexFromY (y1, def);
	if (index0 == index1) {
		value0 = f (y0, def, acc) - edge_y;
		if (value0 == 0)
			return y0;
		value1 = f (y1, def, acc) - edge_y;
		if (value1 == 0)
			return y1;
		if (value0 > 0 == value1 > 0)
			return -1.0;
	} else {
		/*
	 	 * round index0 up, index1 down
	 	 */
		index0++;
		value0 = binaryValue (index0, def, acc, valid, table, f) - edge_y;
		if (value0 == 0)
			return yFromBinaryIndex (index0, def);
		value1 = binaryValue (index1, def, acc, valid, table, f) - edge_y;
		if (value1 == 0)
			return yFromBinaryIndex (index1, def);
		/*
	 	 * make sure the result lies between the restricted end points
	 	 */
		if (value0 > 0 == value1 > 0) {
			if (y0 == y1)
				return -1.0;
			binaryvalue = f(y0, def, acc) - edge_y;
			if (binaryvalue > 0 != value0 > 0) {
				/*
			 	 * restrict the search to the small portion at
			 	 * the begining
			 	 */
				index1 = index0;
				value1 = value0;
				value0 = binaryvalue;
				y1 = yFromBinaryIndex (index0, def);
			} else {
				binaryvalue = f(y1, def, acc) - edge_y;
				if (binaryvalue > 0 == value1 > 0)
					return -1.0;	/* an illegal value */
				/*
			 	 * restrict the search to the small portion at
			 	 * the end
			 	 */
				index0 = index1;
				value0 = value1;
				value1 = binaryvalue;
				y0 = yFromBinaryIndex (index1, def);
			}
		} else {
			/*
		 	 * restrict the search to the inside portion
		 	 */
			y0 = yFromBinaryIndex (index0, def);
			y1 = yFromBinaryIndex (index1, def);
		}
	}
	binarylimit = (value1 - value0) / 25.0;
	binarylimit = fabs (binarylimit);
	if (binarylimit < BINARY_LIMIT)
		binarylimit = BINARY_LIMIT;
	/*
	 * binary search for a while
	 */
	while (fabs (value1) > binarylimit) {
		if (y0 == y1 || value0 == value1)
			return -1.0;

		if (index1 > index0 + 1) {
			newindex = (index1 + index0) / 2;
			binaryy = yFromBinaryIndex (newindex, def);
			binaryvalue = binaryValue (newindex,
				def, acc, valid, table, f) - edge_y;
		} else {
			binaryy = (y0 + y1) / 2;
			/*
		 	 * inline expansion of the function
		 	 */
	
			y2 = binaryy*binaryy;
			x = w * Sqrt ((h2 - (y2)) / h2);
	
			binaryvalue = ( binaryy + (binaryy * w2 * l) /
			      	      (2 * Sqrt (x*x * h4 + y2 * w4))) - edge_y;
			newindex = -1;
		}
		if (binaryvalue > 0 == value0 > 0) {
			y0 = binaryy;
			value0 = binaryvalue;
			if (newindex > 0)
				index0 = newindex;
		} else {
			y1 = binaryy;
			value1 = binaryvalue;
			if (newindex > 0)
				index1 = newindex;
		}
	}

	/*
	 * clean up the estimate with newtons method
	 */

	while (fabs (value1) > NEWTON_LIMIT) {
		newtony = y1 - value1 * (y1 - y0) / (value1 - value0);
		if (newtony > maxY)
			newtony = maxY;
		if (newtony < minY)
			newtony = minY;
		/*
		 * inline expansion of the function
		 */

		y2 = newtony*newtony;
		x = w * Sqrt ((h2 - (y2)) / h2);

		newtonvalue = ( newtony + (newtony * w2 * l) /
			      (2 * Sqrt (x*x * h4 + y2 * w4))) - edge_y;

		if (newtonvalue == 0)
			return newtony;
		if (fabs (value0) > fabs (value1)) {
			y0 = newtony;
			value0 = newtonvalue;
		} else {
			y1 = newtony;
			value1 = newtonvalue;
		}
	}
	return y1;
}

#ifdef notdef
static double
ellipseX (ellipse_y, def, acc)
	double			ellipse_y;
	struct arc_def		*def;
	struct accelerators	*acc;
{
	return def->w / def->h * Sqrt (acc->h2 - ellipse_y * ellipse_y);
}
#endif

static double
outerX (outer_y, def, bound, acc)
	register double		outer_y;
	register struct arc_def	*def;
	struct arc_bound	*bound;
	struct accelerators	*acc;
{
	double	y;

	/*
	 * special case for circles
	 */
	if (def->w == def->h) {
		register double	x;

		x = def->w + def->l/2.0;
		x = Sqrt (x * x - outer_y * outer_y);
		return x;
	}
	if (outer_y == bound->outer.min)
		y = bound->ellipse.min;
	if (outer_y == bound->outer.max)
		y = bound->ellipse.max;
	else
		y = ellipseY (outer_y, def, acc, 1,
			      bound->ellipse.min, bound->ellipse.max);
	return outerXfromY (y, def, acc);
}

/*
 * this equation has two solutions -- it's not a function
 */

static void
innerXs (inner_y, def, bound, acc, innerX1p, innerX2p)
	register double		inner_y;
	struct arc_def		*def;
	struct arc_bound	*bound;
	struct accelerators	*acc;
	double			*innerX1p, *innerX2p;
{
	register double	x1, x2;
	double		xalt, y0, y1, altY, ellipse_y1, ellipse_y2;

	/*
	 * special case for circles
	 */
	if (def->w == def->h) {
		x1 = def->w - def->l/2.0;
		x2 = Sqrt (x1 * x1 - inner_y * inner_y);
		if (x1 < 0)
			x2 = -x2;
		*innerX1p = *innerX2p = x2;
		return;
	}
	if (boundedLe (acc->tail_y, bound->ellipse)) {
		if (def->h > def->w) {
			y0 = bound->ellipse.min;
			y1 = acc->tail_y;
			altY = bound->ellipse.max;
		} else {
			y0 = bound->ellipse.max;
			y1 = acc->tail_y;
			altY = bound->ellipse.min;
		}
		ellipse_y1 = ellipseY (inner_y, def, acc, 0, y0, y1);
		ellipse_y2 = ellipseY (inner_y, def, acc, 0, y1, altY);
		if (ellipse_y1 == -1.0)
			ellipse_y1 = ellipse_y2;
		if (ellipse_y2 == -1.0)
			ellipse_y2 = ellipse_y1;
	} else {
		ellipse_y1 = ellipseY (inner_y, def, acc, 0,
				       bound->ellipse.min, bound->ellipse.max);
		ellipse_y2 = ellipse_y1;
	}
	x2 = x1 = innerXfromY (ellipse_y1, def, acc);
	if (ellipse_y1 != ellipse_y2)
		x2 = innerXfromY (ellipse_y2, def, acc);
	if (acc->left.valid && boundedLe (inner_y, bound->left)) {
		xalt = intersectLine (inner_y, &acc->left);
		if (xalt < x2 && xalt < x1)
			x2 = xalt;
		if (xalt < x1)
			x1 = xalt;
	}
	if (acc->right.valid && boundedLe (inner_y, bound->right)) {
		xalt = intersectLine (inner_y, &acc->right);
		if (xalt < x2 && xalt < x1)
			x2 = xalt;
		if (xalt < x1)
			x1 = xalt;
	}
	*innerX1p = x1;
	*innerX2p = x2;
}

/*
 * this section computes the x value of the span at y 
 * intersected with the specified face of the ellipse.
 *
 * this is the min/max X value over the set of normal
 * lines to the entire ellipse,  the equation of the
 * normal lines is:
 *
 *     ellipse_x h^2                   h^2
 * x = ------------ y + ellipse_x (1 - --- )
 *     ellipse_y w^2                   w^2
 *
 * compute the derivative with-respect-to ellipse_y and solve
 * for zero:
 *    
 *       (w^2 - h^2) ellipse_y^3 + h^4 y
 * 0 = - ----------------------------------
 *       h w ellipse_y^2 sqrt (h^2 - ellipse_y^2)
 *
 *             (   h^4 y     )
 * ellipse_y = ( ----------  ) ^ (1/3)
 *             ( (h^2 - w^2) )
 *
 * The other two solutions to the equation are imaginary.
 *
 * This gives the position on the ellipse which generates
 * the normal with the largest/smallest x intersection point.
 *
 * Now compute the second derivative to check whether
 * the intersection is a minimum or maximum:
 *
 *    h (y0^3 (w^2 - h^2) + h^2 y (3y0^2 - 2h^2))
 * -  -------------------------------------------
 *          w y0^3 (sqrt (h^2 - y^2)) ^ 3
 *
 * as we only care about the sign,
 *
 * - (y0^3 (w^2 - h^2) + h^2 y (3y0^2 - 2h^2))
 *
 * or (to use accelerators),
 *
 * y0^3 (h^2 - w^2) - h^2 y (3y0^2 - 2h^2) 
 *
 */

/*
 * computes the position on the ellipse whose normal line
 * intersects the given scan line maximally
 */

static double
hookEllipseY (scan_y, bound, acc, left)
	double			scan_y;
	struct arc_bound	*bound;
	struct accelerators	*acc;
{
	double	ret;

	if (acc->h2mw2 == 0) {
		if (scan_y > 0 && !left || scan_y < 0 && left)
			return bound->ellipse.min;
		return bound->ellipse.max;
	}
	ret = (acc->h4 * scan_y) / (acc->h2mw2);
	if (ret >= 0)
		return cbrt (ret);
	else
		return -cbrt (-ret);
}

/*
 * computes the X value of the intersection of the
 * given scan line with the right side of the lower hook
 */

static double
hookX (scan_y, def, bound, acc, left)
	double			scan_y;
	struct arc_def		*def;
	struct arc_bound	*bound;
	struct accelerators	*acc;
	int			left;
{
	double	ellipse_y, x;
	double	maxMin;

	if (def->w != def->h) {
		ellipse_y = hookEllipseY (scan_y, bound, acc, left);
		if (boundedLe (ellipse_y, bound->ellipse)) {
			/*
		 	 * compute the value of the second
		 	 * derivative
		 	 */
			maxMin = ellipse_y*ellipse_y*ellipse_y * acc->h2mw2 -
		 	 acc->h2 * scan_y * (3 * ellipse_y*ellipse_y - 2*acc->h2);
			if ((left && maxMin > 0) || (!left && maxMin < 0)) {
				if (ellipse_y == 0)
					return def->w + left ? -def->l/2 : def->l/2;
				x = (acc->h2 * scan_y - ellipse_y * acc->h2mw2) *
					Sqrt (acc->h2 - ellipse_y * ellipse_y) /
			 		(def->h * def->w * ellipse_y);
				return x;
			}
		}
	}
	if (left) {
		if (acc->left.valid && boundedLe (scan_y, bound->left)) {
			x = intersectLine (scan_y, &acc->left);
		} else {
			if (acc->right.valid)
				x = intersectLine (scan_y, &acc->right);
			else
				x = def->w - def->l/2;
		}
	} else {
		if (acc->right.valid && boundedLe (scan_y, bound->right)) {
			x = intersectLine (scan_y, &acc->right);
		} else {
			if (acc->left.valid)
				x = intersectLine (scan_y, &acc->left);
			else
				x = def->w - def->l/2;
		}
	}
	return x;
}

/*
 * generate the set of spans with
 * the given y coordinate
 */

static void
arcSpan (y, def, bounds, acc)
	double			y;
	struct arc_def		*def;
	struct arc_bound	*bounds;
	struct accelerators	*acc;
{
	double	innerx1, innerx2, outerx1, outerx2;

	if (boundedLe (y, bounds->inner)) {
		/*
		 * intersection with inner edge
		 */
		innerXs (y, def, bounds, acc, &innerx1, &innerx2);
	} else {
		/*
		 * intersection with left face
		 */
		innerx2 = innerx1 = hookX (y, def, bounds, acc, 1);
		if (acc->right.valid && boundedLe (y, bounds->right))
		{
			innerx2 = intersectLine (y, &acc->right);
			if (innerx2 < innerx1)
				innerx1 = innerx2;
		}
	}
	if (boundedLe (y, bounds->outer)) {
		/*
		 * intersection with outer edge
		 */
		outerx1 = outerx2 = outerX (y, def, bounds, acc);
	} else {
		/*
		 * intersection with right face
		 */
		outerx2 = outerx1 = hookX (y, def, bounds, acc, 0);
		if (acc->left.valid && boundedLe (y, bounds->left))
 		{
			outerx2 = intersectLine (y, &acc->left);
			if (outerx2 < outerx1)
				outerx2 = outerx1;
		}
	}
	/*
	 * there are a very few cases when two spans will be
	 * generated.
	 */
	if (innerx1 <= outerx1 &&
 	    outerx1 <  innerx2 &&
 	    innerx2 <= outerx2)
 	{
		span (innerx1, outerx1);
		span (innerx2, outerx2);
	} else
		span (innerx1, outerx2);
}

/*
 * create whole arcs out of pieces.  This code is
 * very bad.
 */

static double	arcXcenter, arcYcenter;
static int	arcXoffset, arcYoffset;

static struct finalSpan	**finalSpans = NULL;
static int		finalMiny = 0, finalMaxy = -1;
static int		finalSize = 0;

static int		nspans = 0;	/* total spans, not just y coords */

struct finalSpan {
	struct finalSpan	*next;
	int			min, max;	/* x values */
};

static struct finalSpan    *freeFinalSpans, *tmpFinalSpan;

# define allocFinalSpan()   (freeFinalSpans ?\
				((tmpFinalSpan = freeFinalSpans), \
				 (freeFinalSpans = freeFinalSpans->next), \
				 (tmpFinalSpan->next = 0), \
				 tmpFinalSpan) : \
			     realAllocSpan ())

# define SPAN_CHUNK_SIZE    128

struct finalSpanChunk {
	struct finalSpan	data[SPAN_CHUNK_SIZE];
	struct finalSpanChunk	*next;
};

static struct finalSpanChunk	*chunks;

struct finalSpan *
realAllocSpan ()
{
	register struct finalSpanChunk	*newChunk;
	register struct finalSpan	*span;
	register int			i;

	newChunk = (struct finalSpanChunk *) xalloc (sizeof (struct finalSpanChunk));
	if (!newChunk)
		return (struct finalSpan *) NULL;
	newChunk->next = chunks;
	chunks = newChunk;
	freeFinalSpans = span = newChunk->data + 1;
	for (i = 1; i < SPAN_CHUNK_SIZE-1; i++) {
		span->next = span+1;
		span++;
	}
	span->next = 0;
	span = newChunk->data;
	span->next = 0;
	return span;
}

static void
disposeFinalSpans ()
{
	struct finalSpanChunk	*chunk, *next;

	for (chunk = chunks; chunk; chunk = next) {
		next = chunk->next;
		xfree (chunk);
	}
	chunks = 0;
	freeFinalSpans = 0;
	xfree(finalSpans);
	finalSpans = 0;
}

static void
fillSpans (pDrawable, pGC)
    DrawablePtr	pDrawable;
    GCPtr	pGC;
{
	register struct finalSpan	*span;
	register DDXPointPtr		xSpan;
	register int			*xWidth;
	register int			i;
	register struct finalSpan	**f;
	register int			spany;
	DDXPointPtr			xSpans;
	int				*xWidths;

	if (nspans == 0)
		return;
	xSpan = xSpans = (DDXPointPtr) xalloc (nspans * sizeof (DDXPointRec));
	xWidth = xWidths = (int *) xalloc (nspans * sizeof (int));
	if (xSpans && xWidths)
	{
	    i = 0;
	    f = finalSpans;
	    for (spany = finalMiny; spany <= finalMaxy; spany++, f++) {
		    for (span = *f; span; span=span->next) {
			    if (span->max <= span->min)
				    continue;
			    xSpan->x = span->min;
			    xSpan->y = spany;
			    ++xSpan;
			    *xWidth++ = span->max - span->min;
			    ++i;
		    }
	    }
	    (*pGC->ops->FillSpans) (pDrawable, pGC, i, xSpans, xWidths, TRUE);
	}
	disposeFinalSpans ();
	xfree (xSpans);
	xfree (xWidths);
	finalMiny = 0;
	finalMaxy = -1;
	finalSize = 0;
	nspans = 0;
}

# define SPAN_REALLOC	100

# define findSpan(y) ((finalMiny <= (y) && (y) <= finalMaxy) ? \
			  &finalSpans[(y) - finalMiny] : \
			  realFindSpan (y))

static struct finalSpan **
realFindSpan (y)
{
	struct finalSpan	**newSpans;
	int			newSize, newMiny, newMaxy;
	int			change;
	int			i;

	if (y < finalMiny || y > finalMaxy) {
		if (!finalSize) {
			finalMaxy = y - (SPAN_REALLOC - 1);
			finalMiny = finalMaxy + 1;
		}
		if (y < finalMiny)
			change = finalMiny - y;
		else
			change = y - finalMaxy;
		if (change > SPAN_REALLOC)
			change += SPAN_REALLOC;
		else
			change = SPAN_REALLOC;
		newSize = finalSize + change;
		newSpans = (struct finalSpan **) xalloc
 					(newSize * sizeof (struct finalSpan *));
		if (!newSpans)
		    return (struct finalSpan **)NULL;
		newMiny = finalMiny;
		newMaxy = finalMaxy;
		if (y < finalMiny)
			newMiny = finalMiny - change;
		else
			newMaxy = finalMaxy + change;
		if (finalSpans) {
			bcopy ((char *) finalSpans,
	 		       ((char *) newSpans) + (finalMiny-newMiny) * sizeof (struct finalSpan *),
			       finalSize * sizeof (struct finalSpan *));
			xfree (finalSpans);
		}
		if ((i = finalMiny - newMiny) > 0)
			bzero ((char *)newSpans, i * sizeof (struct finalSpan *));
		if ((i = newMaxy - finalMaxy) > 0)
			bzero ((char *)(newSpans + newSize - i),
			       i * sizeof (struct finalSpan *));
		finalSpans = newSpans;
		finalMaxy = newMaxy;
		finalMiny = newMiny;
		finalSize = newSize;
	}
	return &finalSpans[y - finalMiny];
}

static void
newFinalSpan (y, xmin, xmax)
    int		y;
    register int	xmin, xmax;
{
	register struct finalSpan	*x;
	register struct finalSpan	**f;
	struct finalSpan		*oldx;
	struct finalSpan		*prev;

	f = findSpan (y);
	if (!f)
	    return;
	oldx = 0;
	for (;;) {
		prev = 0;
		for (x = *f; x; x=x->next) {
			if (x == oldx) {
				prev = x;
				continue;
			}
			if (x->min <= xmax && xmin <= x->max) {
				if (oldx) {
					oldx->min = min (x->min, xmin);
					oldx->max = max (x->max, xmax);
					if (prev)
						prev->next = x->next;
					else
						*f = x->next;
					--nspans;
				} else {
					x->min = min (x->min, xmin);
					x->max = max (x->max, xmax);
					oldx = x;
				}
				xmin = oldx->min;
				xmax = oldx->max;
				break;
			}
			prev = x;
		}
		if (!x)
			break;
	}
	if (!oldx) {
		x = allocFinalSpan ();
		if (x)
		{
		    x->min = xmin;
		    x->max = xmax;
		    x->next = *f;
		    *f = x;
		    ++nspans;
		}
	}
}

static void
deleteFinalSpan (y, xmin, xmax)
    int		y;
    register int	xmin, xmax;
{
	register struct finalSpan	*x;
	register struct finalSpan	**f;
	int				newmax;

	f = findSpan (y);
	if (!f)
	    return;
	for (x = *f; x; x=x->next) {
	    if (x->min <= xmin && xmax <= x->max) {
		if (x->min == xmin) {
		    x->min = xmax;
		} else if (x->max == xmax) {
		    x->max = xmin;
		} else {
		    newmax = x->max;
		    x->max = xmin;
		    newFinalSpan (y, xmax, newmax);
		}
		return;
	    } else if (x->min <= xmax && xmin <= x->max) {
		if (x->min <= xmin) {
		    x->max = xmin;
		} else {
		    x->min = xmax;
		    if (x->min > x->max)
			x->min = x->max;
		}
	    }
	}
}

static void
mirrorSppPoint (quadrant, sppPoint)
	int		quadrant;
	SppPointPtr	sppPoint;
{
	switch (quadrant) {
	case 0:
		break;
	case 1:
		sppPoint->x = -sppPoint->x;
		break;
	case 2:
		sppPoint->x = -sppPoint->x;
		sppPoint->y = -sppPoint->y;
		break;
	case 3:
		sppPoint->y = -sppPoint->y;
		break;
	}
	/*
	 * and translate to X coordinate system
	 */
	sppPoint->y = -sppPoint->y;
}

static double	spanY;

static int	quadrantMask;

static void
span (left, right)
	double	left, right;
{
	register int	mask = quadrantMask, bit;
	register double	min, max, y;
	int	xmin, xmax, spany;

	while (mask) {
		bit = lowbit (mask);
		mask &= ~bit;
		switch (bit) {
		case 1:
			min = left;
			max = right;
			y = spanY;
			break;
		case 2:
			min = -right;
			max = -left;
			y = spanY;
			break;
		case 4:
			min = -right;
			max = -left;
			y = -spanY;
			break;
		case 8:
			min = left;
			max = right;
			y = -spanY;
			break;
		default:
			FatalError ("miarc.c: illegal quadrant mask bit %d\n", bit);
		}
		xmin = ICEIL (min + arcXcenter) + arcXoffset;
		xmax = ICEIL (max + arcXcenter) + arcXoffset;
		spany = ICEIL (arcYcenter - y) + arcYoffset;

		if (xmax > xmin)
			newFinalSpan (spany, xmin, xmax);
	}
}

static void
unspan (left, right)
double	left, right;
{
	register int	mask = quadrantMask, bit;
	register double	min, max, y;
	int	xmin, xmax, spany;

	while (mask) {
		bit = lowbit (mask);
		mask &= ~bit;
		switch (bit) {
		case 1:
			min = left;
			max = right;
			y = spanY;
			break;
		case 2:
			min = -right;
			max = -left;
			y = spanY;
			break;
		case 4:
			min = -right;
			max = -left;
			y = -spanY;
			break;
		case 8:
			min = left;
			max = right;
			y = -spanY;
			break;
		default:
			FatalError ("miarc.c: illegal quadrant mask bit %d\n", bit);
		}
		xmin = ICEIL (min + arcXcenter) + arcXoffset;
		xmax = ICEIL (max + arcXcenter) + arcXoffset;
		spany = ICEIL (arcYcenter - y) + arcYoffset;

		if (xmax > xmin)
			deleteFinalSpan (spany, xmin, xmax);
	}
}

/*
 * split an arc into pieces which are scan-converted
 * in the first-quadrant and mirrored into position.
 * This is necessary as the scan-conversion code can
 * only deal with arcs completely contained in the
 * first quadrant.
 */

static void
drawArc (x0, y0, w, h, l, a0, a1, right, left)
	int	x0, y0, w, h, l, a0, a1;
	miArcFacePtr	right, left;	/* save end line points */
{
	struct arc_def		def;
	struct accelerators	acc;
	int			startq, endq, curq;
	int			rightq, leftq, righta, lefta;
	miArcFacePtr		passRight, passLeft;
	int			q0, q1, mask;
	struct band {
		int	a0, a1;
		int	mask;
	}	band[5], sweep[20];
	int			bandno, sweepno;
	int			i, j;
	int			flipRight = 0, flipLeft = 0;			
	int			copyEnd = 0;

	def.w = ((double) w) / 2;
	def.h = ((double) h) / 2;
	arcXoffset = x0;
	arcYoffset = y0;
	arcXcenter = def.w;
	arcYcenter = def.h;
	def.l = (double) l;
	if (l == 0)
		def.l = 1.0;
	if (a1 < a0)
		a1 += 360 * 64;
	startq = a0 / (90 * 64);
	if (a0 == a1)
	    endq = startq;
	else
	    endq = (a1-1) / (90 * 64);
	bandno = 0;
	curq = startq;
	for (;;) {
		switch (curq) {
		case 0:
			if (a0 > 90 * 64)
				q0 = 0;
			else
				q0 = a0;
			if (a1 < 360 * 64)
				q1 = min (a1, 90 * 64);
			else
				q1 = 90 * 64;
			if (curq == startq && a0 == q0) {
				righta = q0;
				rightq = curq;
			}
			if (curq == endq && a1 == q1) {
				lefta = q1;
				leftq = curq;
			}
			break;
		case 1:
			if (a1 < 90 * 64)
				q0 = 0;
			else
				q0 = 180 * 64 - min (a1, 180 * 64);
			if (a0 > 180 * 64)
				q1 = 90 * 64;
			else
				q1 = 180 * 64 - max (a0, 90 * 64);
			if (curq == startq && 180 * 64 - a0 == q1) {
				righta = q1;
				rightq = curq;
			}
			if (curq == endq && 180 * 64 - a1 == q0) {
				lefta = q0;
				leftq = curq;
			}
			break;
		case 2:
			if (a0 > 270 * 64)
				q0 = 0;
			else
				q0 = max (a0, 180 * 64) - 180 * 64;
			if (a1 < 180 * 64)
				q1 = 90 * 64;
			else
				q1 = min (a1, 270 * 64) - 180 * 64;
			if (curq == startq && a0 - 180*64 == q0) {
				righta = q0;
				rightq = curq;
			}
			if (curq == endq && a1 - 180 * 64 == q1) {
				lefta = q1;
				leftq = curq;
			}
			break;
		case 3:
			if (a1 < 270 * 64)
				q0 = 0;
			else
				q0 = 360 * 64 - min (a1, 360 * 64);
			q1 = 360 * 64 - max (a0, 270 * 64);
			if (curq == startq && 360 * 64 - a0 == q1) {
				righta = q1;
				rightq = curq;
			}
			if (curq == endq && 360 * 64 - a1 == q0) {
				lefta = q0;
				leftq = curq;
			}
			break;
		}
		band[bandno].a0 = q0;
		band[bandno].a1 = q1;
		band[bandno].mask = 1 << curq;
		bandno++;
		if (curq == endq)
			break;
		curq++;
		if (curq == 4) {
			a0 = 0;
			a1 -= 360 * 64;
			curq = 0;
			endq -= 4;
		}
	}
	sweepno = 0;
	for (;;) {
		q0 = 90 * 64;
		mask = 0;
		/*
		 * find left-most point
		 */
		for (i = 0; i < bandno; i++)
			if (band[i].a0 <= q0) {
				q0 = band[i].a0;
				q1 = band[i].a1;
				mask = band[i].mask;
			}
		if (!mask)
			break;
		/*
		 * locate next point of change
		 */
		for (i = 0; i < bandno; i++)
			if (!(mask & band[i].mask)) {
				if (band[i].a0 == q0) {
					if (band[i].a1 < q1)
						q1 = band[i].a1;
					mask |= band[i].mask;
 				} else if (band[i].a0 < q1)
					q1 = band[i].a0;
			}
		/*
		 * create a new sweep
		 */
		sweep[sweepno].a0 = q0;
		sweep[sweepno].a1 = q1;
		sweep[sweepno].mask = mask;
		sweepno++;
		/*
		 * subtract the sweep from the affected bands
		 */
		for (i = 0; i < bandno; i++)
			if (band[i].a0 == q0) {
				band[i].a0 = q1;
				/*
				 * check if this band is empty
				 */
				if (band[i].a0 == band[i].a1)
					band[i].a1 = band[i].a0 = 90 * 64 + 1;
			}
	}
	computeAcc (&def, &acc);
	for (j = 0; j < sweepno; j++) {
		mask = sweep[j].mask;
		passRight = passLeft = 0;
 		if (mask & (1 << rightq)) {
			if (sweep[j].a0 == righta)
				passRight = right;
			else if (sweep[j].a1 == righta) {
				passLeft = right;
				flipRight = 1;
			}
		}
		if (mask & (1 << leftq)) {
			if (sweep[j].a1 == lefta)
			{
				if (passLeft)
					copyEnd = 1;
				passLeft = left;
			}
			else if (sweep[j].a0 == lefta) {
				if (passRight)
					copyEnd = 1;
				passRight = left;
				flipLeft = 1;
			}
		}
		drawQuadrant (&def, &acc, sweep[j].a0, sweep[j].a1, mask, 
 			      passRight, passLeft);
	}
	/*
	 * when copyEnd is set, both ends of the arc were computed
	 * at the same time; drawQuadrant only takes one end though,
	 * so the left end will be the only one holding the data.  Copy
	 * it from there.
	 */
	if (copyEnd)
		*right = *left;
	/*
	 * mirror the coordinates generated for the
	 * faces of the arc
	 */
	if (right) {
		mirrorSppPoint (rightq, &right->clock);
		mirrorSppPoint (rightq, &right->center);
		mirrorSppPoint (rightq, &right->counterClock);
		if (flipRight) {
			SppPointRec	temp;

			temp = right->clock;
			right->clock = right->counterClock;
			right->counterClock = temp;
		}
	}
	if (left) {
		mirrorSppPoint (leftq,  &left->counterClock);
		mirrorSppPoint (leftq,  &left->center);
		mirrorSppPoint (leftq,  &left->clock);
		if (flipLeft) {
			SppPointRec	temp;

			temp = left->clock;
			left->clock = left->counterClock;
			left->counterClock = temp;
		}
	}
}

static void
drawQuadrant (def, acc, a0, a1, mask, right, left)
	struct arc_def		*def;
	struct accelerators	*acc;
	int			a0, a1;
	int			mask;
	miArcFacePtr		right, left;
{
	struct arc_bound	bound;
	double			miny, maxy, y;
	int			minIsInteger;
	double			fromInt;

	def->a0 = ((double) a0) / 64.0;
	def->a1 = ((double) a1) / 64.0;
	fromInt = def->h - floor (def->h);
	computeBound (def, &bound, acc, right, left);
	y = fmin (bound.inner.min, bound.outer.min);
	miny = ICEIL(y - fromInt) + fromInt;
	minIsInteger = y == miny;
	y = fmax (bound.inner.max, bound.outer.max);
	maxy = floor (y - fromInt) + fromInt;
	for (y = miny; y <= maxy; y = y + 1.0) {
		if (y == miny && minIsInteger)
			quadrantMask = mask & 0xc;
		else
			quadrantMask = mask;
		spanY = y;
		arcSpan (y, def, &bound, acc);
	}
	/*
	 * add the pixel at the top of the arc if
	 * this segment terminates exactly at the
	 * top of the arc, and the outside
	 * point is exactly an integer (width
	 * is integral and (line width/2)
	 * is integral)
	 */
	if (a1 == 90 * 64 && (mask & 1) &&
	    def->w == floor (def->w) &&
	    def->l/2 == floor (def->l/2))
 	{
		quadrantMask = 1;
		spanY = def->h + def->l/2;
		span (0.0, 1.0);
		spanY = def->h - def->l/2;
		unspan (0.0, 1.0);
	}
}
