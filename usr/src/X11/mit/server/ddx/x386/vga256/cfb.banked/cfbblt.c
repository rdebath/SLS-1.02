/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/cfb.banked/cfbblt.c,v 1.11 1992/08/29 11:12:40 dawes Exp $ */
/*
 * cfb copy area
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

Author: Keith Packard

*/
/* $XConsortium: cfbblt.c,v 1.7 91/05/06 15:13:21 rws Exp $ */

#include	"X.h"
#include	"Xmd.h"
#include	"Xproto.h"
#include	"gcstruct.h"
#include	"windowstr.h"
#include	"scrnintstr.h"
#include	"pixmapstr.h"
#include	"regionstr.h"
#include	"cfb.h"
#include	"cfbmskbits.h"
#include	"cfb8bit.h"
#include	"fastblt.h"
#include	"mergerop.h"
#include        "vgaBank.h"

#if (MROP == Mcopy) && defined(SPEEDUP)
int (*ourcfbDoBitbltCopy)();
#endif


#ifdef notdef /* XXX fails right now, walks off end of pixmaps */
#if defined (FAST_UNALIGNED_READS) && (PPW == 4)
#define DO_UNALIGNED_BITBLT
#endif
#endif

#if (MROP == Mcopy)
#ifdef SPEEDUP
speedupcfbDoBitbltCopy(pSrc, pDst, alu, prgnDst, pptSrc, planemask)
#endif
#else
MROP_NAME(cfbDoBitblt)(pSrc, pDst, alu, prgnDst, pptSrc, planemask)
#endif
    DrawablePtr	    pSrc, pDst;
    int		    alu;
    RegionPtr	    prgnDst;
    DDXPointPtr	    pptSrc;
    unsigned long   planemask;
{
    unsigned long *psrcBase, *pdstBase;	
				/* start of src and dst bitmaps */
    int widthSrc, widthDst;	/* add to get to same position in next line */

    BoxPtr pbox;
    int nbox;

    BoxPtr pboxTmp, pboxNext, pboxBase, pboxNew1, pboxNew2;
				/* temporaries for shuffling rectangles */
    DDXPointPtr pptTmp, pptNew1, pptNew2;
				/* shuffling boxes entails shuffling the
				   source points too */
    int w, h;
    int xdir;			/* 1 = left right, -1 = right left/ */
    int ydir;			/* 1 = top down, -1 = bottom up */

    unsigned long *psrcLine, *pdstLine;	
				/* pointers to line with current src and dst */
    register unsigned long *psrc;/* pointer to current src longword */
    register unsigned long *pdst;/* pointer to current dst longword */

    MROP_DECLARE_REG()

				/* following used for looping through a line */
    unsigned long startmask, endmask;	/* masks for writing ends of dst */
    int nlMiddle;		/* whole longwords in dst */
    int xoffSrc, xoffDst;
    register int leftShift, rightShift;
    register unsigned long bits;
    register unsigned long bits1;
    register int nl;		/* temp copy of nlMiddle */

				/* place to store full source word */
    int nstart;			/* number of ragged bits at start of dst */
    int nend;			/* number of ragged bits at end of dst */
    int srcStartOver;		/* pulling nstart bits from src
				   overflows into the next word? */
    int careful;
    int tmpSrc;

#ifdef SPEEDUP
    void SpeedUpBitBlt();
#endif

    MROP_INITIALIZE(alu,planemask);

    cfbGetLongWidthAndPointer (pSrc, widthSrc, psrcBase)

    cfbGetLongWidthAndPointer (pDst, widthDst, pdstBase)

    BANK_FLAG_BOTH(psrcBase,pdstBase)

    /* XXX we have to err on the side of safety when both are windows,
     * because we don't know if IncludeInferiors is being used.
     */
    careful = ((pSrc == pDst) ||
	       ((pSrc->type == DRAWABLE_WINDOW) &&
		(pDst->type == DRAWABLE_WINDOW)));

    pbox = REGION_RECTS(prgnDst);
    nbox = REGION_NUM_RECTS(prgnDst);

    pboxNew1 = NULL;
    pptNew1 = NULL;
    pboxNew2 = NULL;
    pptNew2 = NULL;
    if (careful && (pptSrc->y < pbox->y1))
    {
        /* walk source botttom to top */
	ydir = -1;
	widthSrc = -widthSrc;
	widthDst = -widthDst;

	if (nbox > 1)
	{
	    /* keep ordering in each band, reverse order of bands */
	    pboxNew1 = (BoxPtr)ALLOCATE_LOCAL(sizeof(BoxRec) * nbox);
	    if(!pboxNew1)
		return;
	    pptNew1 = (DDXPointPtr)ALLOCATE_LOCAL(sizeof(DDXPointRec) * nbox);
	    if(!pptNew1)
	    {
	        DEALLOCATE_LOCAL(pboxNew1);
	        return;
	    }
	    pboxBase = pboxNext = pbox+nbox-1;
	    while (pboxBase >= pbox)
	    {
	        while ((pboxNext >= pbox) &&
		       (pboxBase->y1 == pboxNext->y1))
		    pboxNext--;
	        pboxTmp = pboxNext+1;
	        pptTmp = pptSrc + (pboxTmp - pbox);
	        while (pboxTmp <= pboxBase)
	        {
		    *pboxNew1++ = *pboxTmp++;
		    *pptNew1++ = *pptTmp++;
	        }
	        pboxBase = pboxNext;
	    }
	    pboxNew1 -= nbox;
	    pbox = pboxNew1;
	    pptNew1 -= nbox;
	    pptSrc = pptNew1;
        }
    }
    else
    {
	/* walk source top to bottom */
	ydir = 1;
    }

    if (careful && (pptSrc->x < pbox->x1))
    {
	/* walk source right to left */
        xdir = -1;

	if (nbox > 1)
	{
	    /* reverse order of rects in each band */
	    pboxNew2 = (BoxPtr)ALLOCATE_LOCAL(sizeof(BoxRec) * nbox);
	    pptNew2 = (DDXPointPtr)ALLOCATE_LOCAL(sizeof(DDXPointRec) * nbox);
	    if(!pboxNew2 || !pptNew2)
	    {
		if (pptNew2) DEALLOCATE_LOCAL(pptNew2);
		if (pboxNew2) DEALLOCATE_LOCAL(pboxNew2);
		if (pboxNew1)
		{
		    DEALLOCATE_LOCAL(pptNew1);
		    DEALLOCATE_LOCAL(pboxNew1);
		}
	        return;
	    }
	    pboxBase = pboxNext = pbox;
	    while (pboxBase < pbox+nbox)
	    {
	        while ((pboxNext < pbox+nbox) &&
		       (pboxNext->y1 == pboxBase->y1))
		    pboxNext++;
	        pboxTmp = pboxNext;
	        pptTmp = pptSrc + (pboxTmp - pbox);
	        while (pboxTmp != pboxBase)
	        {
		    *pboxNew2++ = *--pboxTmp;
		    *pptNew2++ = *--pptTmp;
	        }
	        pboxBase = pboxNext;
	    }
	    pboxNew2 -= nbox;
	    pbox = pboxNew2;
	    pptNew2 -= nbox;
	    pptSrc = pptNew2;
	}
    }
    else
    {
	/* walk source left to right */
        xdir = 1;
    }

    while(nbox--)
    {
	w = pbox->x2 - pbox->x1;
	h = pbox->y2 - pbox->y1;

#if (MROP) == Mcopy && defined(SPEEDUP)
        if ((planemask&0xFF) == 0xFF) {
            if (h | w)
                SpeedUpBitBlt(psrcBase, pdstBase, widthSrc << 2, widthDst << 2,
                             pptSrc->x, pptSrc->y, pbox->x1, pbox->y1,
                             w, h, xdir, ydir);
            pbox++;
            pptSrc++;
            continue;
        }
#endif


	if (ydir == -1) /* start at last scanline of rectangle */
	{
	    psrcLine = psrcBase + ((pptSrc->y+h-1) * -widthSrc);
	    pdstLine = pdstBase + ((pbox->y2-1) * -widthDst);
	}
	else /* start at first scanline */
	{
	    psrcLine = psrcBase + (pptSrc->y * widthSrc);
	    pdstLine = pdstBase + (pbox->y1 * widthDst);
	}
	if ((pbox->x1 & PIM) + w <= PPW)
	{
	    maskpartialbits (pbox->x1, w, startmask);
	    endmask = 0;
	    nlMiddle = 0;
	}
	else
	{
	    maskbits(pbox->x1, w, startmask, endmask, nlMiddle);
	}
	if (xdir == 1)
	{
	    xoffSrc = pptSrc->x & PIM;
	    xoffDst = pbox->x1 & PIM;
	    pdstLine += (pbox->x1 >> PWSH);
	    psrcLine += (pptSrc->x >> PWSH);
	    if (xoffSrc == xoffDst)
	    {
		while (h--)
		{
		    psrc = psrcLine;
		    pdst = pdstLine;
		    SETR(psrc);
		    SETW(pdst);
		    pdstLine += widthDst;
		    psrcLine += widthSrc;
		    if (startmask)
		    {
		        bits = *psrc;
		        PUSHR();
			*pdst = MROP_MASK(bits, *pdst, startmask);
			POPR();
			psrc++; CHECKRO(psrc);
			pdst++; CHECKWO(pdst);
		    }
		    nl = nlMiddle;

#if MROP == Mcopy
		    DuffL(nl, label1,
			    *pdst = *psrc;
			    pdst++; CHECKWO(pdst); psrc++; CHECKRO(psrc); )
#else
		    DuffL(nl, label1,
			    bits = *psrc;
			    PUSHR();
			    *pdst = MROP_SOLID (bits, *pdst);
			    POPR();
			    pdst++; CHECKWO(pdst); psrc++; CHECKRO(psrc); )
#endif

		    if (endmask)
		      {
			bits = *psrc;
			PUSHR();
			*pdst = MROP_MASK(bits, *pdst, endmask);
			POPR();
		      }
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
		while (h--)
		{
		    psrc = psrcLine;
		    pdst = pdstLine;
		    SETR(psrc);
		    SETW(pdst);
		    pdstLine += widthDst;
		    psrcLine += widthSrc;
		    bits = 0;
		    if (xoffSrc > xoffDst)
		      {
			bits = *psrc++;
			CHECKRO(psrc);
		      }
		    if (startmask)
		    {
			bits1 = BitLeft(bits,leftShift);
			bits = *psrc++;
			CHECKRO(psrc);
			bits1 |= BitRight(bits,rightShift);
			PUSHR();
			*pdst = MROP_MASK(bits1, *pdst, startmask);
			POPR();
			pdst++;
			CHECKWO(pdst);
		    }
		    nl = nlMiddle;

#if MROP != Mcopy
		    DuffL (nl,label2,
			bits1 = BitLeft(bits, leftShift);
			bits = *psrc++; CHECKRO(psrc);
			PUSHR();
			*pdst = MROP_SOLID (bits1 | BitRight(bits, rightShift),
                                            *pdst);
			POPR();
			pdst++; CHECKWO(pdst);
		    )
#else
		    DuffL (nl,label2,
			bits1 = BitLeft(bits, leftShift);
			bits = *psrc++; CHECKRO(psrc);
			*pdst = MROP_SOLID (bits1 | BitRight(bits, rightShift),
                                            *pdst);
			pdst++; CHECKWO(pdst);
		    )
#endif

		    if (endmask)
		    {
			bits1 = BitLeft(bits, leftShift);
			if (BitLeft(endmask, rightShift))
			{
			    bits = *psrc;
			    bits1 |= BitRight(bits, rightShift);
			}
			PUSHR();
			*pdst = MROP_MASK (bits1, *pdst, endmask);
			POPR();
		    }
		}
	    }
	}
	else	/* xdir == -1 */
	{
	    xoffSrc = (pptSrc->x + w - 1) & PIM;
	    xoffDst = (pbox->x2 - 1) & PIM;
	    pdstLine += ((pbox->x2-1) >> PWSH) + 1;
	    psrcLine += ((pptSrc->x+w - 1) >> PWSH) + 1;
	    if (xoffSrc == xoffDst)
	    {
		while (h--)
		{
		    psrc = psrcLine;
		    pdst = pdstLine;
		    SETR(psrc);
		    SETW(pdst);
		    pdstLine += widthDst;
		    psrcLine += widthSrc;
		    if (endmask)
		    {
			pdst--; CHECKWU(pdst);
			psrc--; CHECKRU(psrc);
			bits = *psrc;
			PUSHR();
			*pdst = MROP_MASK (bits, *pdst, endmask);
			POPR();
		    }
		    nl = nlMiddle;

#if MROP == Mcopy
		    DuffL(nl,label3,
			 --pdst; CHECKWU(pdst); --psrc; CHECKRU(psrc); 
			 *pdst = *psrc;)
#else
		    DuffL(nl,label3,
			 --pdst; CHECKWU(pdst); --psrc; CHECKRU(psrc); 
			 bits = *psrc;
			 PUSHR();
			 *pdst = MROP_SOLID (bits, *pdst);
			 POPR();)
#endif

		    if (startmask)
		    {
			--pdst; CHECKWU(pdst);
			--psrc; CHECKRU(psrc);
			bits = *psrc;
			PUSHR();
			*pdst = MROP_MASK(bits, *pdst, startmask);
			POPR();
		    }
		}
	    }
	    else
	    {
		if (xoffDst > xoffSrc)
		{
		    rightShift = (xoffDst - xoffSrc) << (5 - PWSH);
		    leftShift = 32 - rightShift;
		}
		else
		{
		    leftShift = (xoffSrc - xoffDst) << (5 - PWSH);
		    rightShift = 32 - leftShift;
		}
		while (h--)
		{
		    psrc = psrcLine;
		    pdst = pdstLine;
		    SETR(psrc);
		    SETW(pdst);
		    pdstLine += widthDst;
		    psrcLine += widthSrc;
		    bits = 0;
		    if (xoffDst > xoffSrc) {
		        --psrc; CHECKRU(psrc);
			bits = *psrc;
		      }
		    if (endmask)
		    {
			bits1 = BitRight(bits, rightShift);
			--psrc; CHECKRU(psrc);
			bits = *psrc;
			bits1 |= BitLeft(bits, leftShift);
			pdst--; CHECKWU(pdst);
			PUSHR();
			*pdst = MROP_MASK(bits1, *pdst, endmask);
			POPR();
		    }
		    nl = nlMiddle;

#if MROP != Mcopy
		    DuffL (nl, label4,
			bits1 = BitRight(bits, rightShift);
			--psrc; CHECKRU(psrc);
			bits = *psrc;
			--pdst; CHECKWU(pdst);
			PUSHR();
			*pdst = MROP_SOLID(bits1 | BitLeft(bits, leftShift),
                                           *pdst);
			POPR();
		    )
#else
		    DuffL (nl, label4,
			bits1 = BitRight(bits, rightShift);
			--psrc; CHECKRU(psrc);
			bits = *psrc;
			--pdst; CHECKWU(pdst);
			*pdst = MROP_SOLID(bits1 | BitLeft(bits, leftShift),
                                           *pdst);
		    )
#endif

		    if (startmask)
		    {
			bits1 = BitRight(bits, rightShift);
			if (BitRight (startmask, leftShift))
			{
			    --psrc; CHECKRU(psrc);
			    bits = *psrc;
			    bits1 |= BitLeft(bits, leftShift);
			}
			--pdst; CHECKWU(pdst);
			PUSHR();
			*pdst = MROP_MASK(bits1, *pdst, startmask);
			POPR();
		    }
		}
	    }
	}
	pbox++;
	pptSrc++;
    }
    if (pboxNew2)
    {
	DEALLOCATE_LOCAL(pptNew2);
	DEALLOCATE_LOCAL(pboxNew2);
    }
    if (pboxNew1)
    {
	DEALLOCATE_LOCAL(pptNew1);
	DEALLOCATE_LOCAL(pboxNew1);
    }
}
