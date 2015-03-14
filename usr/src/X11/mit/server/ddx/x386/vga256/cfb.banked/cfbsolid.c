/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/cfb.banked/cfbsolid.c,v 1.12 1992/09/27 13:36:23 dawes Exp $ */
/*
 * $XConsortium: cfbsolid.c,v 1.4 91/04/26 21:33:55 keith Exp $
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
#include "cfbrrop.h"
#include "vgaBank.h"
#include "vgaFasm.h"


#if defined(SPEEDUP) && (RROP == GXcopy)
void (*ourcfbFillRectSolidCopy)();
#endif

void
#if (RROP == GXcopy)
#ifdef SPEEDUP
speedupcfbFillRectSolidCopy (pDrawable, pGC, nBox, pBox)
#else
stdcfbFillRectSolidCopy (pDrawable, pGC, nBox, pBox)
#endif
#else
RROP_NAME(cfbFillRectSolid) (pDrawable, pGC, nBox, pBox)
#endif
    DrawablePtr	    pDrawable;
    GCPtr	    pGC;
    int		    nBox;
    BoxPtr	    pBox;
{
    RROP_DECLARE
#ifdef SPEEDUP
    unsigned long   *pdstBase, *pdstRect;
    int		    h;
    int		    w;
    int		    widthDst;

    cfbGetLongWidthAndPointer (pDrawable, widthDst, pdstBase)
#endif /* SPEEDUP */

    RROP_FETCH_GC(pGC)
#ifdef SPEEDUP    
#if (RROP == GXcopy)
    for (; nBox; nBox--, pBox++)
    {
    	pdstRect = pdstBase + pBox->y1 * widthDst;
    	h = pBox->y2 - pBox->y1;
	w = pBox->x2 - pBox->x1;

        SpeedUpBox((unsigned char*)pdstRect + pBox->x1,
			      rrop_xor, h, w, widthDst << 2);
    }
#endif /* GXcopy */
#else /* SPEEDUP */
#if RROP == GXcopy
    stdcfbFillBoxSolid (pDrawable, nBox, pBox, rrop_xor, 0, GXcopy);
#endif /* GXcopy */
#endif /* SPEEDUP */
#if RROP == GXxor
    stdcfbFillBoxSolid (pDrawable, nBox, pBox, rrop_xor, 0, GXxor);
#endif /* GXxor */
#if RROP == GXor
    stdcfbFillBoxSolid (pDrawable, nBox, pBox, rrop_or, 0, GXor);
#endif /* GXor */
#if RROP == GXand
    stdcfbFillBoxSolid (pDrawable, nBox, pBox, rrop_and, 0, GXand);
#endif /* GXand */
#if RROP == GXset
    stdcfbFillBoxSolid (pDrawable, nBox, pBox, rrop_and, rrop_xor, GXset);
#endif /* GXset */
}

#ifndef SPEEDUP

void
RROP_NAME(cfbSolidSpans) (pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int		nInit;			/* number of spans to fill */
    DDXPointPtr pptInit;		/* pointer to list of start points */
    int		*pwidthInit;		/* pointer to list of n widths */
    int 	fSorted;
{
    unsigned long   *pdstBase;
    int		    widthDst;

    RROP_DECLARE
    
    register unsigned long  *pdst EDI;
    int	    nlmiddle, nl;
    register unsigned long  startmask, endmask;
    int	    w;
    int			    x;
    
				/* next three parameters are post-clip */
    int		    n;		/* number of spans to fill */
    DDXPointPtr	    ppt;	/* pointer to list of start points */
    int		    *pwidthFree;/* copies of the pointers to free */
    DDXPointPtr	    pptFree;
    int		    *pwidth;
    cfbPrivGCPtr    devPriv;

    devPriv = (cfbPrivGCPtr)pGC->devPrivates[cfbGCPrivateIndex].ptr;
    RROP_FETCH_GCPRIV(devPriv)
    n = nInit * miFindMaxBand(devPriv->pCompositeClip);
    pwidthFree = (int *)ALLOCATE_LOCAL(n * sizeof(int));
    pptFree = (DDXPointRec *)ALLOCATE_LOCAL(n * sizeof(DDXPointRec));
    if(!pptFree || !pwidthFree)
    {
	if (pptFree) DEALLOCATE_LOCAL(pptFree);
	if (pwidthFree) DEALLOCATE_LOCAL(pwidthFree);
	return;
    }
    pwidth = pwidthFree;
    ppt = pptFree;
    n = miClipSpans(devPriv->pCompositeClip,
		     pptInit, pwidthInit, nInit,
		     ppt, pwidth, fSorted);

    cfbGetLongWidthAndPointer (pDrawable, widthDst, pdstBase)

    BANK_FLAG(pdstBase)

    CLD;

    while (n--)
    {
	x = ppt->x;
	pdst = pdstBase + (ppt->y * widthDst);
	++ppt;
	w = *pwidth++;
	if (!w)
	    continue;
#if PPW == 4
	if (w <= 4)
	{
	    register char   *addrb;

	    addrb = ((char *) pdst) + x;
	    SETRW(addrb);
	    while (w--)
	    {
		RROP_SOLID (addrb);
		addrb++; CHECKRWO(addrb);
	    }
	}
#else
	if ((x & PIM) + width <= PPW)
	{
	    pdst += x >> PWSH;
	    SETRW(pdst);
	    maskpartialbits (x, w, startmask);
	    RROP_SOLID_MASK (pdst, startmask);
	}
#endif
	else
	{
	    pdst += x >> PWSH;
	    SETRW(pdst);
	    maskbits (x, w, startmask, endmask, nlmiddle);
	    if (startmask)
	    {
		RROP_SOLID_MASK (pdst, startmask);
		++pdst; CHECKRWO(pdst);
	    }
	    
	    RROP_SPAN_STD(pdst,nlmiddle,SO_1);

	    if (endmask)
	    {
	        CHECKRWO(pdst);
		RROP_SOLID_MASK (pdst, endmask);
	    }
	}
    }
    DEALLOCATE_LOCAL(pptFree);
    DEALLOCATE_LOCAL(pwidthFree);
}
#endif /* SPEEDUP */
