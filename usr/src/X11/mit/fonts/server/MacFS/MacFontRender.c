/***********************************************************************
Copyright 1991 by Apple Computer, Inc, Cupertino, California
			All Rights Reserved

Permission to use, copy, modify, and distribute this software
for any purpose and without fee is hereby granted, provided
that the above copyright notice appear in all copies.

APPLE MAKES NO WARRANTY OR REPRESENTATION, EITHER EXPRESS,
OR IMPLIED, WITH RESPECT TO THIS SOFTWARE, ITS QUALITY,
PERFORMANCE, MERCHANABILITY, OR FITNESS FOR A PARTICULAR
PURPOSE. AS A RESULT, THIS SOFTWARE IS PROVIDED "AS IS,"
AND YOU THE USER ARE ASSUMING THE ENTIRE RISK AS TO ITS
QUALITY AND PERFORMANCE. IN NO EVENT WILL APPLE BE LIABLE 
FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL
DAMAGES RESULTING FROM ANY DEFECT IN THE SOFTWARE.

THE WARRANTY AND REMEDIES SET FORTH ABOVE ARE EXCLUSIVE
AND IN LIEU OF ALL OTHERS, ORAL OR WRITTEN, EXPRESS OR
IMPLIED.

***********************************************************************/
/************************************************************************
Copyright 1989 by Digital Equipment Corporation, Maynard, Massachusetts,
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

************************************************************************/

#include "MacFont.h"
#include <ctype.h>
#include "fontfilest.h"
/* use bitmap structure */
#include "bitmap.h"

extern int  bitmapGetGlyphs(), bitmapGetMetrics();
extern int  bitmapGetBitmaps(), bitmapGetExtents();
void        MacFontUnloadFont();

int
MacFontRenderFont(pFont, entry, vals, bit, byte, glyph, scan)
    FontPtr     pFont;
    FontEntryPtr entry;
    FontScalablePtr vals;
    int         bit, byte, glyph, scan;
{
    xCharInfo  *min,
               *max;
    BitmapFontPtr  bitmapFont;
	MacBitmapFontRecPtr pMacPriv;
    char        fullName[MAXFONTNAMELEN];
	int			ret;
	int			starttime = GetTimeInMillis();

    strcpy(fullName, entry->name.name);
    FontParseXLFDName(fullName, vals, FONT_XLFD_REPLACE_VALUE);

    pFont->fontPrivate = 0;
    pFont->info.defaultCh = 0;
    pFont->bit = bit;
    pFont->byte = byte;
    pFont->glyph = glyph = ((glyph == 1) ? 2 : glyph); /* Must be even for QD */
    pFont->scan = scan;
    pFont->info.anamorphic = FALSE;

    bitmapFont = (BitmapFontPtr) 
				 xalloc(sizeof(BitmapFontRec) + sizeof(MacBitmapFontRec));
    if (!bitmapFont)
		return AllocError;

	pMacPriv = (MacBitmapFontRecPtr)(bitmapFont + 1);

    if ((ret = OpenMacFont(pFont, pMacPriv, fullName, vals->pixel, vals->x, vals->y))
					 != Successful) {
		xfree(bitmapFont); /* which also frees the tag along pMacPriv */
		return ret;
	}

    pFont->fontPrivate = (pointer) bitmapFont;
    bitmapFont->metrics = 0;
    bitmapFont->ink_metrics = 0;
    bitmapFont->bitmaps = 0;
    bitmapFont->encoding = 0;
    bitmapFont->pDefault = NULL;
    bitmapFont->bitmapExtra = 0;

    
    if (ConvertMacFont(pFont, pMacPriv, bit, byte, glyph, scan) != Successful)
		goto BAILOUT; /* Only error return from ConvertMacFont is AllocError */
    
    if (pFont -> info.defaultCh) {
	int         r,
	            c,
	            cols;

	r = pFont->info.defaultCh >> 8;
	c = pFont->info.defaultCh & 0xFF;
	if (pFont->info.firstRow <= r && r <= pFont->info.lastRow &&
		pFont->info.firstCol <= c && c <= pFont->info.lastCol) {
	    cols = pFont->info.lastCol - pFont->info.firstCol + 1;
            r = r - pFont->info.firstRow;
            c = c - pFont->info.firstCol;
	    bitmapFont->pDefault = bitmapFont->encoding[r * cols + c];
	}
    }
    bitmapComputeFontBounds(pFont);
    if (FontCouldBeTerminal(&pFont->info)) {
	MacFontPadToTerminal(pFont);
	bitmapComputeFontBounds(pFont);
    }
    FontComputeInfoAccelerators(&pFont->info);
    if (bitmapFont->bitmapExtra)
	FontComputeInfoAccelerators(&bitmapFont->bitmapExtra->info);
    if (pFont->info.constantMetrics)
	bitmapAddInkMetrics(pFont);
    if (bitmapFont->bitmapExtra)
	bitmapFont->bitmapExtra->info.inkMetrics = pFont->info.inkMetrics;

    bitmapComputeFontInkBounds(pFont);

    /* generate properties */
    min = &pFont->info.ink_minbounds;
    max = &pFont->info.ink_maxbounds;

	if (pMacPriv->xHeight == 0)
		pMacPriv->xHeight = min->ascent;

	if (pMacPriv->quadWidth == 0)
		pMacPriv->quadWidth = (min->characterWidth + max->characterWidth) / 2;

	pMacPriv->weight = bitmapComputeWeight(pFont);

    MacFontComputedProps(fullName, vals, &(pFont->info), pMacPriv);

    pFont->get_bitmaps = bitmapGetBitmaps;
    pFont->get_extents = bitmapGetExtents;
    pFont->get_glyphs = bitmapGetGlyphs;
    pFont->get_metrics = bitmapGetMetrics;
    pFont->unload_font = MacFontUnloadFont;
	(void) NoticeF("%s in %d ms.", fullName, GetTimeInMillis() - starttime);
    return Successful;

BAILOUT:
    if (pFont->fontPrivate)
		MacFontFreeFontBits(pFont); /* *pFont itself freed by caller. */
    return AllocError;
}

MacFontFreeFontBits(pFont)
    FontPtr     pFont;
{
    BitmapFontPtr  bitmapFont;
	MacBitmapFontRecPtr pMacPriv;
    int         i;

    bitmapFont = (BitmapFontPtr) pFont->fontPrivate;
    xfree(bitmapFont->ink_metrics);
    xfree(bitmapFont->encoding);
    for (i = 0; i < bitmapFont->num_chars; i++)
	xfree(bitmapFont->metrics[i].bits);
    xfree(bitmapFont->metrics);
    xfree(pFont->info.props);
    xfree(pFont->info.isStringProp); /* XXX missing in bdf version ?! */

	pMacPriv = (MacBitmapFontRecPtr)(bitmapFont + 1);
	if (pMacPriv->pgp) {
		ClosePort(pMacPriv->pgp);
		DisposPtr(pMacPriv->pgp);
	}
	if (pMacPriv->pfm)
		DisposPtr(pMacPriv->pfm);

    xfree(bitmapFont); /* which also frees the MacBitmapFontRec tag-along */
}

void
MacFontUnloadFont(pFont)
    FontPtr     pFont;
{
    MacFontFreeFontBits (pFont);
    xfree(pFont);
}

static Bool
MacFontPadToTerminal(pFont)
    FontPtr     pFont;
{
    BitmapFontPtr  bitmapFont;
    BitmapExtraPtr bitmapExtra;
    int         i;
    int         new_size;
    CharInfoRec new;
    int         w,
                h;

    bitmapFont = (BitmapFontPtr) pFont->fontPrivate;
    new.metrics.ascent = pFont->info.fontAscent;
    new.metrics.descent = pFont->info.fontDescent;
    new.metrics.leftSideBearing = 0;
    new.metrics.rightSideBearing = pFont->info.minbounds.characterWidth;
    new.metrics.characterWidth = new.metrics.rightSideBearing;
    new_size = BYTES_FOR_GLYPH(&new, pFont->glyph);
    for (i = 0; i < bitmapFont->num_chars; i++) {
	new.bits = (char *) xalloc(new_size);
	if (!new.bits)
	    return FALSE;
	FontCharReshape(pFont, &bitmapFont->metrics[i], &new);
	xfree(bitmapFont->metrics[i].bits);
	bitmapFont->metrics[i] = new;
    }
    bitmapExtra = bitmapFont->bitmapExtra;
    if (bitmapExtra) {
	w = GLYPHWIDTHPIXELS(&new);
	h = GLYPHHEIGHTPIXELS(&new);
	for (i = 0; i < GLYPHPADOPTIONS; i++)
	    bitmapExtra->bitmapsSizes[i] = bitmapFont->num_chars *
		(BYTES_PER_ROW(w, 1 << i) * h);
    }
    return TRUE;
}
