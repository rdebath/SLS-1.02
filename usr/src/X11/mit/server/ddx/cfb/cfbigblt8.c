/*
 * $XConsortium: cfbigblt8.c,v 1.3 91/07/14 13:50:41 keith Exp $
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

#include	"X.h"
#include	"Xmd.h"
#include	"Xproto.h"
#include	"cfb.h"
#include	"fontstruct.h"
#include	"dixfontstr.h"
#include	"gcstruct.h"
#include	"windowstr.h"
#include	"scrnintstr.h"
#include	"pixmapstr.h"
#include	"regionstr.h"
#include	"cfbmskbits.h"
#include	"cfb8bit.h"

#if (PPW == 4)

void
cfbImageGlyphBlt8 (pDrawable, pGC, x, y, nglyph, ppci, pglyphBase)
    DrawablePtr	    pDrawable;
    GCPtr	    pGC;
    int		    x, y;
    unsigned int    nglyph;
    CharInfoPtr	    *ppci;
    unsigned char   *pglyphBase;
{
    ExtentInfoRec info;		/* used by QueryGlyphExtents() */
    xRectangle backrect;
    int		fillStyle;
    int		alu;
    int		fgPixel;
    int		rop;
    int		xor;
    int		and;
    int		pm;
    cfbPrivGC	    *priv;

    QueryGlyphExtents(pGC->font, ppci, (unsigned long)nglyph, &info);

    if (info.overallWidth >= 0)
    {
    	backrect.x = x;
    	backrect.width = info.overallWidth;
    }
    else
    {
	backrect.x = x + info.overallWidth;
	backrect.width = -info.overallWidth;
    }
    backrect.y = y - FONTASCENT(pGC->font);
    backrect.height = FONTASCENT(pGC->font) + FONTDESCENT(pGC->font);

    priv = (cfbPrivGC *) pGC->devPrivates[cfbGCPrivateIndex].ptr;
    /* this code cheats by knowing that ValidateGC isn't
     * necessary for PolyFillRect
     */
    rop = priv->rop;
    xor = priv->xor;
    and = priv->and;
    alu = pGC->alu;
    fgPixel = pGC->fgPixel;
    fillStyle = pGC->fillStyle;

    pGC->fillStyle = FillSolid;
    pGC->fgPixel = pGC->bgPixel;
    pGC->alu = GXcopy;
    pm = pGC->planemask & PMSK;
    if (pm == PMSK)
    {
	priv->rop = GXcopy;
	priv->xor = PFILL(pGC->bgPixel);
	priv->and = 0;
    }
    else
    {
	priv->rop = cfbReduceRasterOp (GXcopy, pGC->bgPixel, pm,
				       &priv->and, &priv->xor);
    }

    (*pGC->ops->PolyFillRect) (pDrawable, pGC, 1, &backrect);

    pGC->fgPixel = fgPixel;

    if (pm == PMSK)
	priv->xor = PFILL(pGC->fgPixel);
    else
    {
	priv->rop = cfbReduceRasterOp (GXcopy, pGC->fgPixel, pm,
				       &priv->and, &priv->xor);
    }

    cfbPolyGlyphBlt8 (pDrawable, pGC, x, y, nglyph, ppci, pglyphBase);
    
    priv->rop = rop;
    priv->and = and;
    priv->xor = xor;
    pGC->alu = alu;
    pGC->fillStyle = fillStyle;
}
#endif
