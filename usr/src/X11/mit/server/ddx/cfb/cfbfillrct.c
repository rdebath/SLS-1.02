/*
 * Fill rectangles.
 */

/*
Copyright 1989 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the software
without specific, written prior permission.  M.I.T. makes no
representations about the suitability of this software for any
purpose.  It is provided "as is" without express or implied warranty.
*/

/* $XConsortium: cfbfillrct.c,v 5.13 90/05/15 18:40:19 keith Exp $ */

#include "X.h"
#include "Xmd.h"
#include "servermd.h"
#include "gcstruct.h"
#include "window.h"
#include "pixmapstr.h"
#include "scrnintstr.h"
#include "windowstr.h"

#include "cfb.h"
#include "cfbmskbits.h"
#include "mergerop.h"

#if PPW == 4
extern void cfb8FillRectOpaqueStippled32();
extern void cfb8FillRectTransparentStippled32();
extern void cfb8FillRectStippledUnnatural();
#endif

extern void cfbFillRectSolidCopy(), cfbFillRectSolidXor (), cfbFillRectSolidGeneral ();

extern void cfbFillRectTile32Copy (), cfbFillRectTile32General ();

extern void cfbFillBoxTileOddCopy ();
extern void cfbFillBoxTileOddGeneral ();
extern void cfbFillBoxTile32sCopy ();
extern void cfbFillBoxTile32sGeneral ();

void
cfbFillBoxTileOdd (pDrawable, n, rects, tile, xrot, yrot)
    DrawablePtr	pDrawable;
    int		n;
    BoxPtr	rects;
    PixmapPtr	tile;
    int		xrot, yrot;
{
    if (tile->drawable.width & PIM)
	cfbFillBoxTileOddCopy (pDrawable, n, rects, tile, xrot, yrot, GXcopy, ~0);
    else
	cfbFillBoxTile32sCopy (pDrawable, n, rects, tile, xrot, yrot, GXcopy, ~0);
}

void
cfbFillRectTileOdd (pDrawable, pGC, nBox, pBox)
    DrawablePtr	pDrawable;
    GCPtr	pGC;
    int		nBox;
    BoxPtr	pBox;
{
    int	xrot, yrot;
    void    (*fill)();

    xrot = pDrawable->x + pGC->patOrg.x;
    yrot = pDrawable->y + pGC->patOrg.y;
    if (pGC->tile.pixmap->drawable.width & PIM)
    {
    	fill = cfbFillBoxTileOddGeneral;
    	if ((pGC->planemask & PMSK) == PMSK)
    	{
	    if (pGC->alu == GXcopy)
	    	fill = cfbFillBoxTileOddCopy;
    	}
    }
    else
    {
    	fill = cfbFillBoxTile32sGeneral;
    	if ((pGC->planemask & PMSK) == PMSK)
    	{
	    if (pGC->alu == GXcopy)
	    	fill = cfbFillBoxTile32sCopy;
    	}
    }
    (*fill) (pDrawable, nBox, pBox, pGC->tile.pixmap, xrot, yrot, pGC->alu, pGC->planemask);
}

#define NUM_STACK_RECTS	1024

void
cfbPolyFillRect(pDrawable, pGC, nrectFill, prectInit)
    DrawablePtr pDrawable;
    register GCPtr pGC;
    int		nrectFill; 	/* number of rectangles to fill */
    xRectangle	*prectInit;  	/* Pointer to first rectangle to fill */
{
    xRectangle	    *prect;
    RegionPtr	    prgnClip;
    register BoxPtr pbox;
    register BoxPtr pboxClipped;
    BoxPtr	    pboxClippedBase;
    BoxPtr	    pextent;
    BoxRec	    stackRects[NUM_STACK_RECTS];
    cfbPrivGC	    *priv;
    int		    numRects;
    void	    (*BoxFill)();
    int		    n;
    int		    xorg, yorg;

    priv = (cfbPrivGC *) pGC->devPrivates[cfbGCPrivateIndex].ptr;
    prgnClip = priv->pCompositeClip;

    BoxFill = 0;
    switch (pGC->fillStyle)
    {
    case FillSolid:
	switch (priv->rop) {
	case GXcopy:
	    BoxFill = cfbFillRectSolidCopy;
	    break;
	case GXxor:
	    BoxFill = cfbFillRectSolidXor;
	    break;
	default:
	    BoxFill = cfbFillRectSolidGeneral;
	    break;
	}
	break;
    case FillTiled:
	if (!((cfbPrivGCPtr) pGC->devPrivates[cfbGCPrivateIndex].ptr)->
							pRotatedPixmap)
	    BoxFill = cfbFillRectTileOdd;
	else
	{
	    if (pGC->alu == GXcopy && (pGC->planemask & PMSK) == PMSK)
		BoxFill = cfbFillRectTile32Copy;
	    else
		BoxFill = cfbFillRectTile32General;
	}
	break;
#if (PPW == 4)
    case FillStippled:
	if (!((cfbPrivGCPtr) pGC->devPrivates[cfbGCPrivateIndex].ptr)->
							pRotatedPixmap)
	    BoxFill = cfb8FillRectStippledUnnatural;
	else
	    BoxFill = cfb8FillRectTransparentStippled32;
	break;
    case FillOpaqueStippled:
	if (!((cfbPrivGCPtr) pGC->devPrivates[cfbGCPrivateIndex].ptr)->
							pRotatedPixmap)
	    BoxFill = cfb8FillRectStippledUnnatural;
	else
	    BoxFill = cfb8FillRectOpaqueStippled32;
	break;
#endif
    }
    prect = prectInit;
    xorg = pDrawable->x;
    yorg = pDrawable->y;
    if (xorg || yorg)
    {
	prect = prectInit;
	n = nrectFill;
	while(n--)
	{
	    prect->x += xorg;
	    prect->y += yorg;
	    prect++;
	}
    }

    prect = prectInit;

    numRects = REGION_NUM_RECTS(prgnClip) * nrectFill;
    if (numRects > NUM_STACK_RECTS)
    {
	pboxClippedBase = (BoxPtr)ALLOCATE_LOCAL(numRects * sizeof(BoxRec));
	if (!pboxClippedBase)
	    return;
    }
    else
	pboxClippedBase = stackRects;

    pboxClipped = pboxClippedBase;
	
    if (REGION_NUM_RECTS(prgnClip) == 1)
    {
	int x1, y1, x2, y2, bx2, by2;

	pextent = REGION_RECTS(prgnClip);
	x1 = pextent->x1;
	y1 = pextent->y1;
	x2 = pextent->x2;
	y2 = pextent->y2;
    	while (nrectFill--)
    	{
	    if ((pboxClipped->x1 = prect->x) < x1)
		pboxClipped->x1 = x1;
    
	    if ((pboxClipped->y1 = prect->y) < y1)
		pboxClipped->y1 = y1;
    
	    bx2 = (int) prect->x + (int) prect->width;
	    if (bx2 > x2)
		bx2 = x2;
	    pboxClipped->x2 = bx2;
    
	    by2 = (int) prect->y + (int) prect->height;
	    if (by2 > y2)
		by2 = y2;
	    pboxClipped->y2 = by2;

	    prect++;
	    if ((pboxClipped->x1 < pboxClipped->x2) &&
		(pboxClipped->y1 < pboxClipped->y2))
	    {
		pboxClipped++;
	    }
    	}
    }
    else
    {
	int x1, y1, x2, y2, bx2, by2;

	pextent = (*pGC->pScreen->RegionExtents)(prgnClip);
	x1 = pextent->x1;
	y1 = pextent->y1;
	x2 = pextent->x2;
	y2 = pextent->y2;
    	while (nrectFill--)
    	{
	    BoxRec box;
    
	    if ((box.x1 = prect->x) < x1)
		box.x1 = x1;
    
	    if ((box.y1 = prect->y) < y1)
		box.y1 = y1;
    
	    bx2 = (int) prect->x + (int) prect->width;
	    if (bx2 > x2)
		bx2 = x2;
	    box.x2 = bx2;
    
	    by2 = (int) prect->y + (int) prect->height;
	    if (by2 > y2)
		by2 = y2;
	    box.y2 = by2;
    
	    prect++;
    
	    if ((box.x1 >= box.x2) || (box.y1 >= box.y2))
	    	continue;
    
	    n = REGION_NUM_RECTS (prgnClip);
	    pbox = REGION_RECTS(prgnClip);
    
	    /* clip the rectangle to each box in the clip region
	       this is logically equivalent to calling Intersect()
	    */
	    while(n--)
	    {
		pboxClipped->x1 = max(box.x1, pbox->x1);
		pboxClipped->y1 = max(box.y1, pbox->y1);
		pboxClipped->x2 = min(box.x2, pbox->x2);
		pboxClipped->y2 = min(box.y2, pbox->y2);
		pbox++;

		/* see if clipping left anything */
		if(pboxClipped->x1 < pboxClipped->x2 && 
		   pboxClipped->y1 < pboxClipped->y2)
		{
		    pboxClipped++;
		}
	    }
    	}
    }
    if (pboxClipped != pboxClippedBase)
	(*BoxFill) (pDrawable, pGC,
		    pboxClipped-pboxClippedBase, pboxClippedBase);
    if (pboxClippedBase != stackRects)
    	DEALLOCATE_LOCAL(pboxClippedBase);
}
