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
/* $XConsortium: mipoly.c,v 5.0 89/06/09 15:08:32 keith Exp $ */
/*
 *  mipoly.c
 *
 *  Written by Brian Kelleher; June 1986
 *
 *  Draw polygons.  This routine translates the point by the
 *  origin if pGC->miTranslate is non-zero, and calls
 *  to the appropriate routine to actually scan convert the
 *  polygon.
 */
#include "X.h"
#include "windowstr.h"
#include "gcstruct.h"
#include "pixmapstr.h"
#include "mi.h"
#include "miscstruct.h"


void
miFillPolygon(dst, pgc, shape, mode, count, pPts)
    DrawablePtr		dst;
    register GCPtr	pgc;
    int			shape, mode;
    register int	count;
    DDXPointPtr		pPts;
{
    int			i;
    register int	xorg, yorg;
    register DDXPointPtr ppt;

    if (count == 0)
	return;

    ppt = pPts;
    if (pgc->miTranslate)
    {
	xorg = dst->x;
	yorg = dst->y;

        if (mode == CoordModeOrigin) 
        {
	        for (i = 0; i<count; i++) 
                {    
	            ppt->x += xorg;
	            ppt++->y += yorg;
	        }
        }
        else 
        {
	    ppt->x += xorg;
	    ppt++->y += yorg;
	    for (i = 1; i<count; i++) 
            {
	        ppt->x += (ppt-1)->x;
	        ppt->y += (ppt-1)->y;
	        ppt++;
	    }
        }
    }
    else
    {
	if (mode == CoordModePrevious)
        {
	    ppt++;
	    for (i = 1; i<count; i++) 
            {
	        ppt->x += (ppt-1)->x;
	        ppt->y += (ppt-1)->y;
	        ppt++;
	    }
        }
    }
    if (shape == Convex)
	miFillConvexPoly(dst, pgc, count, pPts);
    else
	miFillGeneralPoly(dst, pgc, count, pPts);
}
