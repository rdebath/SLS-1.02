/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/cfb.banked/cfbtileodd.c,v 1.6 1992/08/29 11:13:13 dawes Exp $ */
/*
 * Fill odd tiled rectangles and spans.
 * no depth dependencies.
 */

/*
Copyright 1989 by the Massachusetts Institute of Technology
Copyright 1992 by James Tsillas, Arlignton, Massachusetts

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of M.I.T.  not be used in
advertising or publicity pertaining to distribution of the software
without specific, written prior permission.  Neither M.I.T. nor James
Tsillas make any representations about the suitability of this software
for any purpose.  It is provided "as is" without express or implied
warranty.  */

/* $XConsortium: cfbtileodd.c,v 1.11 91/04/10 11:41:50 keith Exp $ */

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
#include "cfb8bit.h"

#include "mergerop.h"
#include "vgaBank.h"
#include "vgaFasm.h"

#define LastTileBits {\
    tmp = bits; \
    if (tileEndPart) \
	bits = (*pSrc & tileEndMask) | BitRight (*pSrcLine, tileEndLeftShift); \
    else \
	bits = *pSrc; \
}

#define ResetTileBits {\
    pSrc = pSrcLine; \
    nlwSrc = widthSrc;\
    if (tileEndPart) { \
	if (PPW - xoff + tileEndPart <= PPW) {\
	    bits = *pSrc++; \
	    nlwSrc--; \
	} else \
	    bits = BitLeft(tmp, tileEndLeftShift) | \
		   BitRight(bits, tileEndRightShift); \
	xoff = (xoff + xoffStep) & PIM; \
	leftShift = xoff << (5-PWSH); \
	rightShift = 32 - leftShift; \
    }\
}

#define NextTileBits {\
    if (nlwSrc == 1) {\
	LastTileBits\
    } else { \
    	if (nlwSrc == 0) {\
	    ResetTileBits\
    	} \
	tmp = bits; \
	bits = *pSrc++; \
    }\
    nlwSrc--; \
}

void
MROP_NAME(cfbFillBoxTileOdd) (pDrawable, nBox, pBox, tile, xrot, yrot, alu, planemask)
    DrawablePtr	    pDrawable;
    int		    nBox;	/* number of boxes to fill */
    register BoxPtr pBox;	/* pointer to list of boxes to fill */
    PixmapPtr	    tile;	/* tile */
    int		    xrot, yrot;
    int		    alu;
    unsigned long   planemask;
{
    int tileWidth;	/* width of tile in pixels */
    int tileHeight;	/* height of the tile */
    int widthSrc;

    int widthDst;		/* width in longwords of the dest pixmap */
    int w;		/* width of current box */
    int h;		/* height of current box */
    unsigned long startmask;
    unsigned long endmask;	/* masks for reggedy bits at either end of line */
    int nlwMiddle;	/* number of longwords between sides of boxes */
    int nlwSrc;		/* number of whole longwords in source */
    
    register int nlw;	/* loop version of nlwMiddle */
    int srcy;		/* current tile y position */
    int srcx;		/* current tile x position */
    int xoffDst, xoffSrc;
    int leftShift, rightShift;

    MROP_DECLARE_REG()

    unsigned long *pDstBase;	/* pointer to start of dest */
    unsigned long *pDstLine;	/* poitner to start of dest box */
    unsigned long *pSrcBase;	/* pointer to start of source */
    unsigned long *pSrcLine;	/* pointer to start of source line */
    register unsigned long *pDst;
    register unsigned long *pSrc;
    register unsigned long bits, tmp;
    register int	   nlwPart;
    int xoffStart, xoff;
    int leftShiftStart, rightShiftStart, nlwSrcStart;
    unsigned long tileEndMask;
    int tileEndLeftShift, tileEndRightShift;
    int	xoffStep;
    int tileEndPart;
    int needFirst;
    unsigned long   narrow[2];
    unsigned long   narrowMask;
    int	    narrowShift;
    Bool    narrowTile;

    MROP_INITIALIZE (alu, planemask)

    tileHeight = tile->drawable.height;
    tileWidth = tile->drawable.width;
    widthSrc = tile->devKind >> 2;
    narrowTile = FALSE;
    if (widthSrc == 1)
    {
	narrowShift = tileWidth;
	narrowMask = cfbendpartial [tileWidth];
	tileWidth *= 2;
	widthSrc = 2;
	narrowTile = TRUE;
    }
    pSrcBase = (unsigned long *)tile->devPrivate.ptr;

    cfbGetLongWidthAndPointer (pDrawable, widthDst, pDstBase)

    BANK_FLAG(pDstBase)

    tileEndPart = tileWidth & PIM;
    tileEndMask = cfbendpartial[tileEndPart];
    tileEndLeftShift = (tileEndPart) << (5-PWSH);
    tileEndRightShift = 32 - tileEndLeftShift;
    xoffStep = PPW - tileEndPart;
    /*
     * current assumptions: tile > 32 bits wide.
     */
    while (nBox--)
    {
	w = pBox->x2 - pBox->x1;
	h = pBox->y2 - pBox->y1;
	modulus (pBox->x1 - xrot, tileWidth, srcx);
	modulus (pBox->y1 - yrot, tileHeight, srcy);
	xoffDst = pBox->x1 & PIM;
	if (xoffDst + w < PPW)
	{
	    maskpartialbits(pBox->x1, w, startmask);
	    endmask = 0;
	    nlwMiddle = 0;
	}
	else
	{
	    maskbits (pBox->x1, w, startmask, endmask, nlwMiddle)
	}
	pDstLine = pDstBase + (pBox->y1 * widthDst) + (pBox->x1 >> PWSH);
	pSrcLine = pSrcBase + (srcy * widthSrc);
	xoffSrc = srcx & PIM;
	if (xoffSrc >= xoffDst)
	{
	    xoffStart = xoffSrc - xoffDst;
	    needFirst = 1;
	}
	else
	{
	    xoffStart = PPW - (xoffDst - xoffSrc);
	    needFirst = 0;
	}
	leftShiftStart = (xoffStart) << (5-PWSH);
	rightShiftStart = 32 - leftShiftStart;
	nlwSrcStart = widthSrc - (srcx >> PWSH);
	while (h--)
	{
	    /* XXX only works when narrowShift >= PPW/2 */
	    if (narrowTile)
	    {
		tmp = pSrcLine[0] & narrowMask;
		narrow[0] = tmp | SCRRIGHT (tmp, narrowShift);
		narrow[1] = SCRLEFT (tmp, PPW - narrowShift) |
			    SCRRIGHT(tmp, 2 * narrowShift - PPW);
		pSrcLine = narrow;
	    }
	    xoff = xoffStart;
	    leftShift = leftShiftStart;
	    rightShift = rightShiftStart;
	    nlwSrc = nlwSrcStart;
	    pSrc = pSrcLine + (srcx >> PWSH);
	    pDst = pDstLine;
	    SETRW(pDst);
	    bits = 0;
	    if (needFirst)
	    {
		NextTileBits
	    }
	    if (startmask)
	    {
		NextTileBits
		tmp = BitLeft(tmp, leftShift);
 		if (rightShift != 32)
		    tmp |= BitRight(bits,rightShift);
		*pDst = MROP_MASK (tmp, *pDst, startmask);
		++pDst; CHECKRWO(pDst);
	    }
	    nlw = nlwMiddle;
	    while (nlw)
	    {
		if (nlwSrc > 1)
		{
		    nlwPart = nlw;
		    if (nlwPart >= nlwSrc)
			nlwPart = nlwSrc - 1;
		    nlw -= nlwPart;
		    nlwSrc -= nlwPart;
		    if (rightShift != 32)
		    {
			while (nlwPart--)
			{
			    tmp = bits;
			    bits = *pSrc++;
			    *pDst = MROP_SOLID(BitLeft(tmp, leftShift) |
					      BitRight (bits, rightShift),
					      *pDst);
			    ++pDst; CHECKRWO(pDst);
			}
		    }
		    else
		    {
			if (nlwPart)
			{
			    *pDst = MROP_SOLID (bits, *pDst);
			    ++pDst; CHECKRWO(pDst);
			    nlwPart--;
			    while (nlwPart--)
			    {
				*pDst = MROP_SOLID(*pSrc, *pDst);
				++pDst; ++pSrc; CHECKRWO(pDst);
			    }
			    bits = *pSrc++;
			}
		    }
		}
		else
		{
		    NextTileBits
		    if (rightShift != 32)
		    {
			*pDst = MROP_SOLID(BitLeft(tmp, leftShift) |
					   BitRight(bits, rightShift),
					   *pDst);
			++pDst; CHECKRWO(pDst);
		    }
		    else
		    {
			*pDst = MROP_SOLID (tmp, *pDst);
			++pDst; CHECKRWO(pDst);
		    }
		    nlw--;
		}
	    }
	    if (endmask)
	    {
		NextTileBits
		if (rightShift == 32)
		    bits = 0;
		*pDst = MROP_MASK (BitLeft(tmp, leftShift) |
				   BitRight(bits,rightShift),
				   *pDst, endmask);
	    }
	    pDstLine += widthDst;
	    pSrcLine += widthSrc;
	    if (++srcy == tileHeight)
	    {
		srcy = 0;
		pSrcLine = pSrcBase;
	    }
	}
	pBox++;
    }
}

void
MROP_NAME(cfbFillSpanTileOdd) (pDrawable, n, ppt, pwidth, tile, xrot, yrot, alu, planemask)
    DrawablePtr	pDrawable;
    int		n;
    DDXPointPtr	ppt;
    int		*pwidth;
    PixmapPtr	tile;
    int		xrot, yrot;
    int		alu;
    unsigned long   planemask;
{
    int tileWidth;	/* width of tile in pixels */
    int tileHeight;	/* height of the tile */
    int widthSrc;

    int widthDst;		/* width in longwords of the dest pixmap */
    int w;		/* width of current span */
    unsigned long startmask;
    unsigned long endmask;	/* masks for reggedy bits at either end of line */
    int nlwSrc;		/* number of whole longwords in source */
    
    register int nlw;	/* loop version of nlwMiddle */
    int srcy;		/* current tile y position */
    int srcx;		/* current tile x position */
    int xoffDst, xoffSrc;
    int leftShift, rightShift;

    MROP_DECLARE_REG()

    unsigned long *pDstBase;	/* pointer to start of dest */
    unsigned long *pDstLine;	/* poitner to start of dest box */
    unsigned long *pSrcBase;	/* pointer to start of source */
    unsigned long *pSrcLine;	/* pointer to start of source line */
    register unsigned long *pDst;
    register unsigned long *pSrc;
    register unsigned long bits, tmp;
    register int	   nlwPart;
    int xoffStart, xoff;
    int leftShiftStart, rightShiftStart, nlwSrcStart;
    unsigned long tileEndMask;
    int tileEndLeftShift, tileEndRightShift;
    int	xoffStep;
    int tileEndPart;
    int needFirst;
    unsigned long   narrow[2];
    unsigned long   narrowMask;
    int	    narrowShift;
    Bool    narrowTile;

    MROP_INITIALIZE (alu, planemask)

    tileHeight = tile->drawable.height;
    tileWidth = tile->drawable.width;
    widthSrc = tile->devKind >> 2;
    narrowTile = FALSE;
    if (widthSrc == 1)
    {
	narrowShift = tileWidth;
	narrowMask = cfbendpartial [tileWidth];
	tileWidth *= 2;
	widthSrc = 2;
	narrowTile = TRUE;
    }
    pSrcBase = (unsigned long *)tile->devPrivate.ptr;

    cfbGetLongWidthAndPointer (pDrawable, widthDst, pDstBase)

    BANK_FLAG(pDstBase)

    tileEndPart = tileWidth & PIM;
    tileEndMask = cfbendpartial[tileEndPart];
    tileEndLeftShift = (tileEndPart) << (5-PWSH);
    tileEndRightShift = 32 - tileEndLeftShift;
    xoffStep = PPW - tileEndPart;
    while (n--)
    {
	w = *pwidth++;
	modulus (ppt->x - xrot, tileWidth, srcx);
	modulus (ppt->y - yrot, tileHeight, srcy);
	xoffDst = ppt->x & PIM;
	if (xoffDst + w < PPW)
	{
	    maskpartialbits(ppt->x, w, startmask);
	    endmask = 0;
	    nlw = 0;
	}
	else
	{
	    maskbits (ppt->x, w, startmask, endmask, nlw)
	}
	pDstLine = pDstBase + (ppt->y * widthDst) + (ppt->x >> PWSH);
	pSrcLine = pSrcBase + (srcy * widthSrc);
	xoffSrc = srcx & PIM;
	if (xoffSrc >= xoffDst)
	{
	    xoffStart = xoffSrc - xoffDst;
	    needFirst = 1;
	}
	else
	{
	    xoffStart = PPW - (xoffDst - xoffSrc);
	    needFirst = 0;
	}
	leftShiftStart = (xoffStart) << (5-PWSH);
	rightShiftStart = 32 - leftShiftStart;
	nlwSrcStart = widthSrc - (srcx >> PWSH);
	/* XXX only works when narrowShift >= PPW/2 */
	if (narrowTile)
	{
	    tmp = pSrcLine[0] & narrowMask;
	    narrow[0] = tmp | SCRRIGHT (tmp, narrowShift);
	    narrow[1] = SCRLEFT (tmp, PPW - narrowShift) |
			SCRRIGHT(tmp, 2 * narrowShift - PPW);
	    pSrcLine = narrow;
	}
	xoff = xoffStart;
	leftShift = leftShiftStart;
	rightShift = rightShiftStart;
	nlwSrc = nlwSrcStart;
	pSrc = pSrcLine + (srcx >> PWSH);
	pDst = pDstLine;
	SETRW(pDst);
	bits = 0;
	if (needFirst)
	{
	    NextTileBits
	}
	if (startmask)
	{
	    NextTileBits
	    tmp = BitLeft(tmp, leftShift);
	    if (rightShift != 32)
		tmp |= BitRight(bits,rightShift);
	    *pDst = MROP_MASK (tmp, *pDst, startmask);
	    ++pDst; CHECKRWO(pDst);
	}
	if (tileEndPart)
	{
	    while (nlw)
	    {
		if (nlwSrc > 1)
		{
		    nlwPart = nlw;
		    if (nlwPart >= nlwSrc)
			nlwPart = nlwSrc - 1;
		    nlw -= nlwPart;
		    nlwSrc -= nlwPart;
		    if (rightShift != 32)
		    {
			while (nlwPart--)
			{
			    tmp = bits;
			    bits = *pSrc++;
			    *pDst = MROP_SOLID(BitLeft(tmp, leftShift) |
					      BitRight (bits, rightShift),
					      *pDst);
			    ++pDst; CHECKRWO(pDst);
			}
		    }
		    else
		    {
			if (nlwPart)
			{
			    *pDst = MROP_SOLID (bits, *pDst);
			    ++pDst; CHECKRWO(pDst);
			    nlwPart--;
			    while (nlwPart--)
			    {
				*pDst = MROP_SOLID(*pSrc, *pDst);
				++pDst; ++pSrc; CHECKRWO(pDst);
			    }
			    bits = *pSrc++;
			}
		    }
		}
		else
		{
		    NextTileBits
		    if (rightShift != 32)
		    {
			*pDst = MROP_SOLID(BitLeft(tmp, leftShift) |
					   BitRight(bits, rightShift),
					   *pDst);
			++pDst; CHECKRWO(pDst);
		    }
		    else
		    {
			*pDst = MROP_SOLID (tmp, *pDst);
			++pDst; CHECKRWO(pDst);
		    }
		    nlw--;
		}
	    }
	}
	else
	{
	    if (leftShift)
	    {
		while (nlw)
		{
		    if (nlwSrc == 0)
		    {
			nlwSrc = widthSrc;
			pSrc = pSrcLine;
		    }
		    nlwPart = nlw;
		    if (nlwPart > nlwSrc)
			nlwPart = nlwSrc;
		    nlw -= nlwPart;
		    nlwSrc -= nlwPart;
		    while (nlwPart--)
		    {
			tmp = bits;
			bits = *pSrc++;
			*pDst = MROP_SOLID(BitLeft(tmp, leftShift) |
					   BitRight(bits, rightShift),
					   *pDst);
			++pDst; CHECKRWO(pDst);
		    }
		}
	    }
	    else
	    {
		while (nlw)
		{
		    if (nlwSrc == 0)
		    {
			nlwSrc = widthSrc;
			pSrc = pSrcLine;
		    }
		    nlwPart = nlw;
		    if (nlwPart > nlwSrc)
			nlwPart = nlwSrc;
		    nlw -= nlwPart;
		    nlwSrc -= nlwPart;
		    if (nlwPart)
		    {
			*pDst = MROP_SOLID (bits, *pDst);
			++pDst; CHECKRWO(pDst);
			nlwPart--;
			while (nlwPart--)
			{
			    *pDst = MROP_SOLID (*pSrc, *pDst);
			    ++pDst; ++pSrc; CHECKRWO(pDst);
			}
			bits = *pSrc++;
		    }
		}
	    }
	}
	if (endmask)
	{
	    NextTileBits
	    if (rightShift == 32)
		bits = 0;
	    *pDst = MROP_MASK (BitLeft(tmp, leftShift) |
			       BitRight(bits,rightShift),
			       *pDst, endmask);
	}
	ppt++;
    }
}

# include "fastblt.h"

#define IncSrcPtr   psrc++; if (!--srcRemaining) { srcRemaining = widthSrc; psrc = psrcStart; }

MROP_NAME(cfbFillBoxTile32s) (pDrawable, nBox, pBox, tile, xrot, yrot, alu, planemask)
    DrawablePtr	    pDrawable;
    int		    nBox;	/* number of boxes to fill */
    register BoxPtr pBox;	/* pointer to list of boxes to fill */
    PixmapPtr	    tile;	/* tile */
    int		    xrot, yrot;
    int		    alu;
    unsigned long   planemask;
{
  int	tileWidth;	/* width of tile */
  int tileHeight;	/* height of the tile */
  int	widthSrc;	/* width in longwords of the source tile */
  
  int widthDst;	/* width in longwords of the dest pixmap */
  int w;		/* width of current box */
  int h;		/* height of current box */
  int maxh;
  unsigned long startmask;
  unsigned long endmask;	/* masks for reggedy bits at either end of line */
  int nlMiddle;	/* number of longwords between sides of boxes */
  
  int srcy;		/* current tile y position */
  int srcx;		/* current tile x position */
  int	srcRemaining;	/* number of longwords remaining in source */
  int xoffDst, xoffSrc;
  int	srcStart;	/* number of longwords source offset at left of box */
  int	leftShift, rightShift;
  
  MROP_DECLARE_REG()
    
  unsigned long	    *pdstBase;	/* pointer to start of dest */
  unsigned long	    *pdstLine;	/* poitner to start of dest box */
  unsigned long	    *psrcBase;	/* pointer to start of source */
  unsigned long	    *psrcLine;	/* pointer to fetch point of source */
  unsigned long	    *psrcStart;	/* pointer to start of source line */
  register unsigned long  *pdst EDI;
  register unsigned long  *psrc ESI;
  register unsigned long  bits, bits1;
  int nlTemp;
  int nlTemp2;
  int fnl;
  unsigned long *pdstT;
  MROP_INITIALIZE (alu, planemask)
    
  psrcBase = (unsigned long *)tile->devPrivate.ptr;
  tileHeight = tile->drawable.height;
  tileWidth = tile->drawable.width;
  widthSrc = tileWidth >> PWSH;

  cfbGetLongWidthAndPointer (pDrawable, widthDst, pdstBase)
    
  BANK_FLAG(pdstBase)

  CLD;

  while (nBox--)
    {
      w = pBox->x2 - pBox->x1;
      h = pBox->y2 - pBox->y1;
      
      /* set up source */
      modulus (pBox->x1 - xrot, tileWidth, srcx);
      modulus (pBox->y1 - yrot, tileHeight, srcy);
      xoffSrc = srcx & PIM;
      srcStart = (srcx >> PWSH);
      psrcStart = psrcBase + (srcy * widthSrc);
      psrcLine = psrcStart + srcStart;
      
      /* set up dest */
      xoffDst = pBox->x1 & PIM;
      pdstLine = pdstBase + (pBox->y1 * widthDst) + (pBox->x1 >> PWSH);
      /* set up masks */
      if (xoffDst + w < PPW) {
	maskpartialbits(pBox->x1, w, startmask);
	endmask = 0;
	nlMiddle = 0;
      }
      else {
	maskbits (pBox->x1, w, startmask, endmask, nlMiddle)
      }
      
      if (xoffSrc == xoffDst) {
	if (pdstLine < (unsigned long *)VGABASE) {
	  maxh = h;
	  h = 0;
	  goto fastpath1;
	}
	pdstT = pdstLine;
	SETRW(pdstLine);
	while (h) {
	  maxh = min(h,((int*)vgaWriteTop - (int*)pdstLine) / widthDst);
	  pdstT += (maxh * widthDst);
	  h -= maxh;
	fastpath1:
	  while (maxh--) {                                             /***/
	    psrc = psrcLine; pdst = pdstLine;                          /***/
 	    srcRemaining = widthSrc - srcStart;                        /***/
	    if (startmask) {                                           /***/
	      *pdst = MROP_MASK (*psrc, *pdst, startmask);             /***/
	      pdst++; IncSrcPtr }                                      /***/
	    nlTemp = nlMiddle;
	    fCopyAL1 (psrc, pdst, nlTemp, srcRemaining, 
		      widthSrc, psrcStart);
	    if (endmask) *pdst = MROP_MASK (*psrc, *pdst, endmask);    /***/
	    pdstLine += widthDst; psrcLine += widthSrc;                /***/
	    psrcStart += widthSrc;                                     /***/
	    if (++srcy == tileHeight) {                                /***/
	      psrcStart = psrcBase; psrcLine = psrcStart + srcStart;   /***/
	      srcy = 0; }                                              /***/
	  }                                                            /***/
	  if (h) {
	    h--;
	    psrc = psrcLine; srcRemaining = widthSrc - srcStart;
	    if (pdstLine >= (unsigned long *)vgaWriteTop) {
	      pdstLine = pdstT;
	      SETRW(pdstLine);
	    }
	    pdst = pdstLine;
	    if (startmask) {
	      *pdst = MROP_MASK (*psrc, *pdst, startmask);
	      pdst++; IncSrcPtr 
	      if (pdst >= (unsigned long *)vgaWriteTop)
		pdst = vgaReadWriteNext(pdst);
	    }
	    nlTemp2 = (int*)vgaWriteTop - (int*)pdst;
	    nlTemp = min(nlMiddle, nlTemp2);
	    nlTemp2 = nlMiddle - nlTemp;
	    fCopyAL2 (psrc, pdst, nlTemp, srcRemaining, widthSrc,
		     psrcStart);
	    if (nlTemp2) {
	      pdst = vgaReadWriteNext(pdst);
	      fCopyAL3 (psrc, pdst, nlTemp2, srcRemaining,
		       widthSrc, psrcStart);
	    }
	    if (endmask) {
	      if (pdst >= (unsigned long *)vgaWriteTop)
		pdst = vgaReadWriteNext(pdst);
	      *pdst = MROP_MASK (*psrc, *pdst, endmask);
	    }
	    pdstT += widthDst; pdstLine += widthDst;
	    if (pdstLine >= (unsigned long *)vgaWriteTop) {
	      pdstLine = pdstT; SETRW(pdstLine);
	    }
	    psrcLine += widthSrc; psrcStart += widthSrc;
	    if (++srcy == tileHeight) {
	      psrcStart = psrcBase; psrcLine = psrcStart + srcStart;
	      srcy = 0; }
	  }
	}
      }
      else
	{
	  if (xoffSrc > xoffDst) {
	    leftShift = (xoffSrc - xoffDst) << (5 - PWSH);
	    rightShift = 32 - leftShift;
	  }
	  else {
	    rightShift = (xoffDst - xoffSrc) << (5 - PWSH);
	    leftShift = 32 - rightShift;
	  }
	  if (pdstLine < (unsigned long *)VGABASE) {
	    maxh = h;
	    h = 0;
	    goto fastpath2;
	  }
	  pdstT = pdstLine;
	  SETRW(pdstLine);
	  while (h) {
	    maxh = min(h, ((int*)vgaWriteTop - (int*)pdstLine) / widthDst);
	    h -= maxh;
	    pdstT += (maxh * widthDst);
	  fastpath2:
	    while (maxh--) {
	      psrc = psrcLine; pdst = pdstLine; bits = 0;
	      srcRemaining = widthSrc - srcStart;
	      if (xoffSrc > xoffDst) {
		bits = *psrc; IncSrcPtr
		}
	      if (startmask) {
		bits1 = BitLeft(bits,leftShift); bits = *psrc; IncSrcPtr
		  bits1 |= BitRight(bits,rightShift);
		*pdst = MROP_MASK(bits1, *pdst, startmask); pdst++;
	      }
	      nlTemp = nlMiddle;
	      while (nlTemp) {
		fnl = nlTemp;
		if (fnl > srcRemaining)
		  fnl = srcRemaining;
		nlTemp -= fnl; srcRemaining -= fnl;
		DuffL (fnl, label000,
		       fCopyUL(psrc, pdst, bits, leftShift, rightShift);)
		if (!srcRemaining) {
		  srcRemaining = widthSrc;
		  psrc = psrcStart;
		}
	      }
	      if (endmask) {
		bits1 = BitLeft(bits, leftShift);
		if (BitLeft(endmask, rightShift)) {
		  bits = *psrc;
		  bits1 |= BitRight(bits, rightShift);
		}
		*pdst = MROP_MASK (bits1, *pdst, endmask);
	      }
	      pdstLine += widthDst; psrcLine += widthSrc;
	      psrcStart += widthSrc;
	      if (++srcy == tileHeight) {
		psrcStart = psrcBase; psrcLine = psrcStart + srcStart;
		srcy = 0;
	      }
	    }
	    if (h) {
	      h--;
	      psrc = psrcLine; bits = 0;
	      srcRemaining = widthSrc - srcStart;
	      if (pdstLine >= (unsigned long *)vgaWriteTop) {
		pdstLine = pdstT;
		SETRW(pdstLine);
	      }
	      pdst = pdstLine;
	      if (xoffSrc > xoffDst) {
		bits = *psrc; IncSrcPtr
		}
	      if (startmask) {
		bits1 = BitLeft(bits,leftShift); bits = *psrc; IncSrcPtr
		  bits1 |= BitRight(bits,rightShift);
		*pdst = MROP_MASK(bits1, *pdst, startmask); pdst++;
		if (pdst >= (unsigned long *)vgaWriteTop)
		  pdst = vgaReadWriteNext(pdst);
	      }
	      nlTemp2 = (int*)vgaWriteTop - (int*)pdst;
	      nlTemp = min(nlMiddle, nlTemp2);
	      nlTemp2 = nlMiddle - nlTemp;
	      while (nlTemp) {
		fnl = nlTemp;
		if (fnl > srcRemaining)
		  fnl = srcRemaining;
		nlTemp -= fnl; srcRemaining -= fnl;
		while (fnl--) {
		  bits1 = BitLeft(bits, leftShift); bits = *psrc++;
		  *pdst = MROP_SOLID (bits1 | 
				      BitRight(bits, rightShift), *pdst);
		  pdst++; 
		}
		if (!srcRemaining) {
		  srcRemaining = widthSrc;
		  psrc = psrcStart;
		}
	      }
	      if (nlTemp2) {
		pdst = vgaReadWriteNext(pdst);
		while (nlTemp2) {
		  fnl = nlTemp2;
		  if (fnl > srcRemaining)
		    fnl = srcRemaining;
		  nlTemp2 -= fnl; srcRemaining -= fnl;
		  while (fnl--) {
		    bits1 = BitLeft(bits, leftShift); bits = *psrc++;
		    *pdst = MROP_SOLID (bits1 | 
					BitRight(bits, rightShift), *pdst);
		    pdst++; 
		  }
		  if (!srcRemaining) {
		    srcRemaining = widthSrc;
		    psrc = psrcStart;
		  }
		}
	      }
	      if (endmask) {
		if (pdst >= (unsigned long *)vgaWriteTop)
		  pdst = vgaReadWriteNext(pdst);
		bits1 = BitLeft(bits, leftShift);
		if (BitLeft(endmask, rightShift)) {
		  bits = *psrc;
		  bits1 |= BitRight(bits, rightShift);
		}
		*pdst = MROP_MASK (bits1, *pdst, endmask);
	      }
	      pdstT += widthDst; pdstLine += widthDst;
	      if (pdstLine >= (unsigned long *)vgaWriteTop) {
		pdstLine = pdstT; SETRW(pdstLine);
	      }
	      psrcLine += widthSrc; psrcStart += widthSrc;
	      if (++srcy == tileHeight) {
		psrcStart = psrcBase; psrcLine = psrcStart + srcStart;
		srcy = 0;
	      }
	    }
	  }
	}
      pBox++;
    }
}

MROP_NAME(cfbFillSpanTile32s) (pDrawable, n, ppt, pwidth, tile, xrot, yrot, alu, planemask)
    DrawablePtr	pDrawable;
    int		n;
    DDXPointPtr	ppt;
    int		*pwidth;
    PixmapPtr	tile;
    int		xrot, yrot;
    int		alu;
    unsigned long   planemask;
{
    int	tileWidth;	/* width of tile */
    int tileHeight;	/* height of the tile */
    int	widthSrc;	/* width in longwords of the source tile */

    int widthDst;	/* width in longwords of the dest pixmap */
    int w;		/* width of current box */
    unsigned long startmask;
    unsigned long endmask;	/* masks for reggedy bits at either end of line */
    int nlMiddle;	/* number of longwords between sides of boxes */
    
    register int nl;	/* loop version of nlMiddle */
    int srcy;		/* current tile y position */
    int srcx;		/* current tile x position */
    int	srcRemaining;	/* number of longwords remaining in source */
    int xoffDst, xoffSrc;
    int	srcStart;	/* number of longwords source offset at left of box */
    int	leftShift, rightShift;

    MROP_DECLARE_REG()

    unsigned long	    *pdstBase;	/* pointer to start of dest */
    unsigned long	    *pdstLine;	/* poitner to start of dest box */
    unsigned long	    *psrcBase;	/* pointer to start of source */
    unsigned long	    *psrcLine;	/* pointer to fetch point of source */
    unsigned long	    *psrcStart;	/* pointer to start of source line */
    register unsigned long  *pdst;
    register unsigned long  *psrc;
    register unsigned long  bits, bits1;
    register int	    nlTemp;

    MROP_INITIALIZE (alu, planemask)

    psrcBase = (unsigned long *)tile->devPrivate.ptr;
    tileHeight = tile->drawable.height;
    tileWidth = tile->drawable.width;
    widthSrc = tileWidth >> PWSH;

    cfbGetLongWidthAndPointer (pDrawable, widthDst, pdstBase)

    BANK_FLAG(pdstBase)

    while (n--)
    {
	w = *pwidth++;

	/* set up source */
	modulus (ppt->x - xrot, tileWidth, srcx);
	modulus (ppt->y - yrot, tileHeight, srcy);
	xoffSrc = srcx & PIM;
	srcStart = (srcx >> PWSH);
	psrcStart = psrcBase + (srcy * widthSrc);
	psrcLine = psrcStart + srcStart;

	/* set up dest */
	xoffDst = ppt->x & PIM;
	pdstLine = pdstBase + (ppt->y * widthDst) + (ppt->x >> PWSH);
	/* set up masks */
	if (xoffDst + w < PPW)
	{
	    maskpartialbits(ppt->x, w, startmask);
	    endmask = 0;
	    nlMiddle = 0;
	}
	else
	{
	    maskbits (ppt->x, w, startmask, endmask, nlMiddle)
	}

	if (xoffSrc == xoffDst)
	{
	    psrc = psrcLine;
	    pdst = pdstLine;
	    SETRW(pdst);
	    srcRemaining = widthSrc - srcStart;
	    if (startmask)
	    {
		*pdst = MROP_MASK (*psrc, *pdst, startmask);
		pdst++; CHECKRWO(pdst);
		IncSrcPtr
	    }
	    nlTemp = nlMiddle;
	    while (nlTemp)
	    {
		nl = nlTemp;
		if (nl > srcRemaining)
		    nl = srcRemaining;

		nlTemp -= nl;
		srcRemaining -= nl;

#ifdef LARGE_INSTRUCTION_CACHE
#ifdef FAST_CONSTANT_OFFSET_MODE

		psrc += nl & (UNROLL-1);
		pdst += nl & (UNROLL-1);

#define BodyOdd(n) pdst[-n] = MROP_SOLID (psrc[-n], pdst[-n]);
#define BodyEven(n) pdst[-n] = MROP_SOLID (psrc[-n], pdst[-n]);

#define LoopReset \
pdst += UNROLL; \
psrc += UNROLL;

#else

#define BodyOdd(n)  *pdst = MROP_SOLID (*psrc, *pdst); pdst++; psrc++; CHECKRWO(pdst);
#define BodyEven(n) BodyOdd(n)

#define LoopReset   ;

#endif
		PackedLoop

#undef BodyOdd
#undef BodyEven
#undef LoopReset

#else
		DuffL(nl, label1,
			*pdst = MROP_SOLID (*psrc, *pdst);
			pdst++; psrc++; CHECKRWO(pdst); )
#endif
		if (!srcRemaining)
		{
		    srcRemaining = widthSrc;
		    psrc = psrcStart;
		}
	    }
	    if (endmask)
	    {
		*pdst = MROP_MASK (*psrc, *pdst, endmask);
	    }
	}
	else
	{
	    if (xoffSrc > xoffDst)
	    {
		leftShift = (xoffSrc - xoffDst) << (5 - PWSH);
		rightShift = 32 - leftShift;
	    }
	    else
	    {
		rightShift = (xoffDst - xoffSrc) << (5 - PWSH);
		leftShift = 32 - rightShift;
	    }
	    psrc = psrcLine;
	    pdst = pdstLine;
	    SETRW(pdst);
	    bits = 0;
	    srcRemaining = widthSrc - srcStart;
	    if (xoffSrc > xoffDst)
	    {
		bits = *psrc;
		IncSrcPtr
	    }
	    if (startmask)
	    {
		bits1 = BitLeft(bits,leftShift);
		bits = *psrc;
		IncSrcPtr
		bits1 |= BitRight(bits,rightShift);
		*pdst = MROP_MASK(bits1, *pdst, startmask);
		pdst++; CHECKRWO(pdst);
	    }
	    nlTemp = nlMiddle;
	    while (nlTemp)
	    {
		nl = nlTemp;
		if (nl > srcRemaining)
		    nl = srcRemaining;

		nlTemp -= nl;
		srcRemaining -= nl;

#ifdef LARGE_INSTRUCTION_CACHE
		bits1 = bits;

#ifdef FAST_CONSTANT_OFFSET_MODE

		psrc += nl & (UNROLL-1);
		pdst += nl & (UNROLL-1);

#define BodyOdd(n) \
bits = psrc[-n]; \
pdst[-n] = MROP_SOLID(BitLeft(bits1, leftShift) | BitRight(bits, rightShift), pdst[-n]);

#define BodyEven(n) \
bits1 = psrc[-n]; \
pdst[-n] = MROP_SOLID(BitLeft(bits, leftShift) | BitRight(bits1, rightShift), pdst[-n]);

#define LoopReset \
pdst += UNROLL; \
psrc += UNROLL;

#else

#define BodyOdd(n) \
bits = *psrc++; \
*pdst = MROP_SOLID(BitLeft(bits1, leftShift) | BitRight(bits, rightShift), *pdst); \
pdst++; CHECKRWO(pdst);
	       
#define BodyEven(n) \
bits1 = *psrc++; \
*pdst = MROP_SOLID(BitLeft(bits, leftShift) | BitRight(bits1, rightShift), *pdst); \
pdst++; CHECKRWO(pdst);

#define LoopReset   ;

#endif	/* !FAST_CONSTANT_OFFSET_MODE */

		PackedLoop

#undef BodyOdd
#undef BodyEven
#undef LoopReset

#else
		DuffL (nl,label2,
		    bits1 = BitLeft(bits, leftShift);
		    bits = *psrc++;
		    *pdst = MROP_SOLID (bits1 | BitRight(bits, rightShift), *pdst);
		    pdst++; CHECKRWO(pdst);
		)
#endif
		if (!srcRemaining)
		{
		    srcRemaining = widthSrc;
		    psrc = psrcStart;
		}
	    }

	    if (endmask)
	    {
		bits1 = BitLeft(bits, leftShift);
		if (BitLeft(endmask, rightShift))
		{
		    bits = *psrc;
		    bits1 |= BitRight(bits, rightShift);
		}
		*pdst = MROP_MASK (bits1, *pdst, endmask);
	    }
	}
	ppt++;
    }
}
