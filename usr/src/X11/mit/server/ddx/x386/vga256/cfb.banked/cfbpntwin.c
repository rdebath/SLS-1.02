/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/cfb.banked/cfbpntwin.c,v 1.11 1992/08/29 11:12:59 dawes Exp $ */
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

#include "X.h"

#include "windowstr.h"
#include "regionstr.h"
#include "pixmapstr.h"
#include "scrnintstr.h"

#include "cfb.h"
#include "cfbmskbits.h"
#include "vgaBank.h"

#ifdef SPEEDUP
void (*ourcfbFillBoxSolid)();
#else
extern void (*ourcfbFillBoxSolid)();
static void cfbPaintArea32(), cfbPaintAreaSolid();

extern void miPaintWindow();

void
cfbPaintWindow(pWin, pRegion, what)
    WindowPtr	pWin;
    RegionPtr	pRegion;
    int		what;
{
    register cfbPrivWin	*pPrivWin;

    pPrivWin = (cfbPrivWin *)(pWin->devPrivates[cfbWindowPrivateIndex].ptr);

    switch (what) {
    case PW_BACKGROUND:
	switch (pWin->backgroundState) {
	case None:
	    return;
	case ParentRelative:
	    do {
		pWin = pWin->parent;
	    } while (pWin->backgroundState == ParentRelative);
	    (*pWin->drawable.pScreen->PaintWindowBackground)(pWin, pRegion,
							     what);
	    return;
	case BackgroundPixmap:
	    if (pPrivWin->fastBackground)
	    {
		cfbFillBoxTile32 ((DrawablePtr)pWin,
				  (int)REGION_NUM_RECTS(pRegion),
				  REGION_RECTS(pRegion),
				  pPrivWin->pRotatedBackground);
		return;
	    }
	    else
	    {
		cfbFillBoxTileOdd ((DrawablePtr)pWin,
				   (int)REGION_NUM_RECTS(pRegion),
				   REGION_RECTS(pRegion),
				   pWin->background.pixmap,
				   (int) pWin->drawable.x, (int) pWin->drawable.y);
		return;
	    }
	    break;
	case BackgroundPixel:
	    (*ourcfbFillBoxSolid) ((DrawablePtr)pWin,
			     (int)REGION_NUM_RECTS(pRegion),
			     REGION_RECTS(pRegion),
			     pWin->background.pixel,
                             0,
			     GXcopy);
	    return;
    	}
    	break;
    case PW_BORDER:
	if (pWin->borderIsPixel)
	{
	    (*ourcfbFillBoxSolid) ((DrawablePtr)pWin,
			     (int)REGION_NUM_RECTS(pRegion),
			     REGION_RECTS(pRegion),
			     pWin->border.pixel,
                             0,
			     GXcopy);
	    return;
	}
	else if (pPrivWin->fastBorder)
	{
	    cfbFillBoxTile32 ((DrawablePtr)pWin,
			      (int)REGION_NUM_RECTS(pRegion),
			      REGION_RECTS(pRegion),
			      pPrivWin->pRotatedBorder);
	    return;
	}
	else if (pWin->border.pixmap->drawable.width >= PPW/2)
	{
	    cfbFillBoxTileOdd ((DrawablePtr)pWin,
			       (int)REGION_NUM_RECTS(pRegion),
			       REGION_RECTS(pRegion),
			       pWin->border.pixmap,
			       (int) pWin->drawable.x, (int) pWin->drawable.y);
	    return;
	}
	break;
    }
    miPaintWindow (pWin, pRegion, what);
}
#endif /* SPEEDUP */

extern unsigned char * fastFillSolidGXcopy();
extern unsigned char * fastFillSolidGXor();
extern unsigned char * fastFillSolidGXxor();
extern unsigned char * fastFillSolidGXand();
extern unsigned char * fastFillSolidGXset();


void
#ifdef SPEEDUP
speedupcfbFillBoxSolid (pDrawable, nBox, pBox, pixel1, pixel2, alu)
#else
stdcfbFillBoxSolid (pDrawable, nBox, pBox, pixel1, pixel2, alu)
#endif
    DrawablePtr	    pDrawable;
    int		    nBox;
    BoxPtr	    pBox;
    unsigned long   pixel1;
    unsigned long   pixel2;
    int	            alu;
{
#ifdef SPEEDUP
    unsigned long   *pdstBase;
    register unsigned char *pdstb;
    int		    widthDst;
    int             h;
    unsigned long   fill1;
    int             m;
    int		    w;

    cfbGetLongWidthAndPointer(pDrawable, widthDst, pdstBase);
    fill1 = PFILL(pixel1);
    for (; nBox; nBox--, pBox++)
    {

        pdstb = (unsigned char*)(pdstBase+pBox->y1*widthDst)+pBox->x1;

    	h = pBox->y2 - pBox->y1;
	w = pBox->x2 - pBox->x1;

        SpeedUpBox(pdstb, fill1, h, w, widthDst << 2);
    }
#else /* SPEEDUP */
    unsigned char   *pdstBase;
    unsigned long   fill2;
    unsigned char   *pdst;
    register int    hcount, vcount, count;
    int             widthPitch;
    Bool            flag;
    unsigned char * (* func)();
    int		    widthDst;
    int             h;
    unsigned long   fill1;
    int             m;
    int		    w;

    if (pDrawable->type == DRAWABLE_WINDOW)
    {
	pdstBase = (unsigned char *)
	(((PixmapPtr)(pDrawable->pScreen->devPrivate))->devPrivate.ptr);
	widthDst = (int)
		  (((PixmapPtr)(pDrawable->pScreen->devPrivate))->devKind);
    }
    else
    {
	pdstBase = (unsigned char *)(((PixmapPtr)pDrawable)->devPrivate.ptr);
	widthDst = (int)(((PixmapPtr)pDrawable)->devKind);
    }

    flag = CHECKSCREEN(pdstBase);
    fill1 = PFILL(pixel1);
    fill2 = PFILL(pixel2);

    switch (alu) {
    case GXcopy: func = fastFillSolidGXcopy; break;
    case GXor:   func = fastFillSolidGXor;   break;
    case GXand:  func = fastFillSolidGXand;  break;
    case GXxor:  func = fastFillSolidGXxor;  break;
    case GXset:  func = fastFillSolidGXset;  break;
    default: return; break;
    }

    for (; nBox; nBox--, pBox++)
    {
    	pdst = pdstBase + pBox->y1 * widthDst + pBox->x1;

    	h = pBox->y2 - pBox->y1;
	w = pBox->x2 - pBox->x1;
	widthPitch = widthDst - w;

	SETRWF(flag,pdst);
	vcount = 0;

	while ( h || vcount ) {
	  if (vcount == 0) {
	    hcount = h;
	    if (flag) {
	      hcount = min( hcount,((unsigned char *)vgaWriteTop - pdst) / widthDst);
	      if (hcount == 0) vcount = w;
	    }
	  }
	  if (vcount != 0) {
	    hcount = 1;
	    if (((count = (unsigned char *)vgaWriteTop - pdst) == 0) ||
		(count >= vcount)) {
	      count = vcount;
	      vcount = 0;
	      h--;
	    } else {
	      vcount -= count;
	    }
	  } else {
	    count = w;
	    h -= hcount;
	  }
	  if (vcount) 
	    pdst = (func)(pdst,fill1,fill2,hcount,count,0,0);
	  else
	    pdst = (func)(pdst,fill1,fill2,hcount,count,w,widthPitch);
	  CHECKRWOF(flag,pdst);
	}
    }
#endif /* SPEEDUP */
}

/* This can be further optimized but it is tricky */
#ifndef SPEEDUP
void
cfbFillBoxTile32 (pDrawable, nBox, pBox, tile)
    DrawablePtr	    pDrawable;
    int		    nBox;	/* number of boxes to fill */
    BoxPtr 	    pBox;	/* pointer to list of boxes to fill */
    PixmapPtr	    tile;	/* rotated, expanded tile */
{
    register int srcpix;	
    int *psrc;		/* pointer to bits in tile, if needed */
    int *lpsrc, *fpsrc; /* loop version of psrc */
    int tileHeight;	/* height of the tile */

    int nlwDst;		/* width in longwords of the dest pixmap */
    int w;		/* width of current box */
    register int h;	/* height of current box */
    register unsigned long startmask;
    register unsigned long endmask; /* masks for reggedy bits at either end of line */
    register unsigned long notstartmask;
    register unsigned long notendmask;

    int nlwMiddle;	/* number of longwords between sides of boxes */
    int nlwExtra;	/* to get from right of box to left of next span */
    register unsigned int nlw;	/* loop version of nlwMiddle */
    register unsigned long *p;	        /* pointer to bits we're writing */
    int y;		/* current scan line */
    Bool flag;

    unsigned long *pbits;/* pointer to start of pixmap */

    tileHeight = tile->drawable.height;
    psrc = (int *)tile->devPrivate.ptr;

    if (pDrawable->type == DRAWABLE_WINDOW)
    {
	pbits = (unsigned long *)
		(((PixmapPtr)
		  (pDrawable->pScreen->devPrivate))->devPrivate.ptr);
	nlwDst = (int)
		  (((PixmapPtr)
		    (pDrawable->pScreen->devPrivate))->devKind) >> 2;
    }
    else
    {
	pbits = (unsigned long *)(((PixmapPtr)pDrawable)->devPrivate.ptr);
	nlwDst = (int)(((PixmapPtr)pDrawable)->devKind) >> 2;
    }

    flag = CHECKSCREEN(pbits);
    while (nBox--)
    {
	w = pBox->x2 - pBox->x1;
	h = pBox->y2 - pBox->y1;
	y = pBox->y1;
	p = pbits + (pBox->y1 * nlwDst) + (pBox->x1 >> PWSH);

	if(flag)p=vgaSetReadWrite(p);

	lpsrc = &psrc[y % tileHeight];
	fpsrc = &psrc[tileHeight];
	if ( ((pBox->x1 & PIM) + w) < PPW)
	{
	    maskpartialbits(pBox->x1, w, startmask);
	    notstartmask = ~startmask;
	    nlwExtra = nlwDst;
	    while (h--)
	    {
		srcpix = *lpsrc++;
		if (lpsrc == fpsrc) lpsrc = psrc;
		*p = (*p & notstartmask) | (srcpix & startmask);
		p += nlwExtra;
		if(flag && (void*)p >= vgaWriteTop)
		  p = vgaReadWriteNext(p);
	    }
	}
	else
	{
	    maskbits(pBox->x1, w, startmask, endmask, nlwMiddle);
	    notstartmask = ~startmask;
	    notendmask = ~endmask;
	    nlwExtra = nlwDst - nlwMiddle;

	    if (startmask && endmask)
	    {
		nlwExtra -= 1;
		while (h--)
		  {
		    srcpix = *lpsrc++;
		    if (lpsrc == fpsrc) lpsrc = psrc;
		    nlw = nlwMiddle;
		    *p = (*p & notstartmask) | (srcpix & startmask);
		    p++;
		    if(flag && (void*)p >= vgaWriteTop)
		      p = vgaReadWriteNext(p);
		    while (nlw--) {
		      *p++ = srcpix;
		      if(flag && (void*)p >= vgaWriteTop)
			p = vgaReadWriteNext(p);
		    }
		    *p = (*p & notendmask) | (srcpix & endmask);
		    p += nlwExtra;
		    if(flag && (void*)p >= vgaWriteTop)
		      p = vgaReadWriteNext(p);
		  }
	      }
	    else if (startmask && !endmask)
	      {
		nlwExtra -= 1;
		while (h--)
		  {
		    srcpix = *lpsrc++;
		    if (lpsrc == fpsrc) lpsrc = psrc;
		    nlw = nlwMiddle;
		    *p = (*p & notstartmask) | (srcpix & startmask);
		    p++;
		    if(flag && (void*)p >= vgaWriteTop)
		      p = vgaReadWriteNext(p);
		    while (nlw--) {
		      *p++ = srcpix;
		      if(flag && (void*)p >= vgaWriteTop)
			p = vgaReadWriteNext(p);
		    }
		    p += nlwExtra;
		    if(flag && (void*)p >= vgaWriteTop)
		      p = vgaReadWriteNext(p);
		  }
	      }
	    else if (!startmask && endmask)
	      {
		while (h--)
		  {
		    srcpix = *lpsrc++;
		    if (lpsrc == fpsrc) lpsrc = psrc;
		    nlw = nlwMiddle;
		    while (nlw--) {
			*p++ = srcpix;
			if(flag && (void*)p >= vgaWriteTop)
			  p = vgaReadWriteNext(p);
		      }
		    *p = (*p & notendmask) | (srcpix & endmask);
		    p += nlwExtra;
		    if(flag && (void*)p >= vgaWriteTop)
		      p = vgaReadWriteNext(p);
		}
	    }
	    else /* no ragged bits at either end */
	    {
		while (h--)
		{
		    srcpix = *lpsrc++;
		    if (lpsrc == fpsrc) lpsrc = psrc;
		    nlw = nlwMiddle;
		    while (nlw--) {
			*p++ = srcpix;
			if(flag && (void*)p >= vgaWriteTop)
			  p = vgaReadWriteNext(p);
		      }
		    p += nlwExtra;
		    if(flag && (void*)p >= vgaWriteTop)
		      p = vgaReadWriteNext(p);
		  }
	    }
	}
        pBox++;
    }
}
#endif /* SPEEDUP */
