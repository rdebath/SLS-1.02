/*****************************************************************************
Copyright 1988, 1989 by Digital Equipment Corporation, Maynard, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Digital not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************************/

#include "x11perf.h"
#include "bitmaps.h"

#define NUM_POINTS 4   /* 4 points to a trapezoid */
static XPoint *points;
static GC      pgc;

int InitTrapezoids(xp, p, reps)
    XParms  xp;
    Parms   p;
    int     reps;
{
    int     i, numPoints;
    int     rows;
    int     x, y;
    int     size, skew;
    XPoint  *curPoint;

    pgc = xp->fggc;

    size = p->special;
    numPoints = (p->objects) * NUM_POINTS;  
    points = (XPoint *)malloc(numPoints * sizeof(XPoint));
    curPoint = points;
    x = size;
    y = 0;
    rows = 0;
    skew = size;

    for (i = 0; i != p->objects; i++, curPoint += NUM_POINTS) {
	curPoint[0].x = x - skew;
	curPoint[0].y = y;
	curPoint[1].x = x - skew + size;
	curPoint[1].y = y;
	curPoint[2].x = x + skew;
	curPoint[2].y = y + size;
	curPoint[3].x = x + skew - size;
	curPoint[3].y = y + size;

	skew--;
	if (skew < 0) skew = size;

	y += size;
	rows++;
	if (y + size > HEIGHT || rows == MAXROWS) {
	    rows = 0;
	    y = 0;
	    x += 2 * size;
	    if (x + size > WIDTH) {
		x = size;
	    }
	}
    }

    SetFillStyle(xp, p);
    return reps;
}

void DoTrapezoids(xp, p, reps)
    XParms  xp;
    Parms   p;
    int     reps;
{
    int     i, j;
    XPoint  *curPoint;

    for (i = 0; i != reps; i++) {
        curPoint = points;
        for (j = 0; j != p->objects; j++) {
            XFillPolygon(xp->d, xp->w, pgc, curPoint, NUM_POINTS, Convex, 
			 CoordModeOrigin);
            curPoint += NUM_POINTS;
	}
        if (pgc == xp->bggc)
            pgc = xp->fggc;
        else
            pgc = xp->bggc;
    }
}

void EndTrapezoids(xp, p)
    XParms  xp;
    Parms   p;
{
    free(points);
}

