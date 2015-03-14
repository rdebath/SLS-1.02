/*
 * $XConsortium: bitmaputils.c,v 1.3 91/05/31 14:23:06 rws Exp $
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
 */

#include    "fontfilest.h"
#include    "bitmap.h"

#ifndef MAXSHORT
#define MAXSHORT    32767
#endif

#ifndef MINSHORT
#define MINSHORT    -32768
#endif

static xCharInfo initMinMetrics = {
MAXSHORT, MAXSHORT, MAXSHORT, MAXSHORT, MAXSHORT, 0xFFFF};
static xCharInfo initMaxMetrics = {
MINSHORT, MINSHORT, MINSHORT, MINSHORT, MINSHORT, 0x0000};

#define MINMAX(field,ci) \
	if (minbounds->field > (ci)->field) \
	     minbounds->field = (ci)->field; \
	if (maxbounds->field < (ci)->field) \
	     maxbounds->field = (ci)->field;

void
bitmapComputeFontBounds(pFont)
    FontPtr     pFont;
{
    BitmapFontPtr  bitmapFont = (BitmapFontPtr) pFont->fontPrivate;
    int         nchars;
    int         r,
                c;
    CharInfoPtr ci,
               *pci;
    int         maxOverlap;
    int         overlap;
    xCharInfo  *minbounds,
               *maxbounds;
    int         i;
    int		numneg = 0, numpos = 0;

    if (bitmapFont->bitmapExtra) {
	minbounds = &bitmapFont->bitmapExtra->info.minbounds;
	maxbounds = &bitmapFont->bitmapExtra->info.maxbounds;
    } else {
	minbounds = &pFont->info.minbounds;
	maxbounds = &pFont->info.maxbounds;
    }
    *minbounds = initMinMetrics;
    *maxbounds = initMaxMetrics;
    maxOverlap = MINSHORT;
    nchars = bitmapFont->num_chars;
    for (i = 0, ci = bitmapFont->metrics; i < nchars; i++, ci++) {
	MINMAX(ascent, &ci->metrics);
	MINMAX(descent, &ci->metrics);
	MINMAX(leftSideBearing, &ci->metrics);
	MINMAX(rightSideBearing, &ci->metrics);
	MINMAX(characterWidth, &ci->metrics);
	if (ci->metrics.characterWidth < 0)
	    numneg++;
	else
	    numpos++;
	minbounds->attributes &= ci->metrics.attributes;
	maxbounds->attributes |= ci->metrics.attributes;
	overlap = ci->metrics.rightSideBearing - ci->metrics.characterWidth;
	if (maxOverlap < overlap)
	    maxOverlap = overlap;
    }
    if (bitmapFont->bitmapExtra) {
	if (numneg > numpos)
	    bitmapFont->bitmapExtra->info.drawDirection = RightToLeft;
	else
	    bitmapFont->bitmapExtra->info.drawDirection = LeftToRight;
	bitmapFont->bitmapExtra->info.maxOverlap = maxOverlap;
	minbounds = &pFont->info.minbounds;
	maxbounds = &pFont->info.maxbounds;
	*minbounds = initMinMetrics;
	*maxbounds = initMaxMetrics;
	pci = bitmapFont->encoding;
	maxOverlap = MINSHORT;
	for (r = pFont->info.firstRow; r <= pFont->info.lastRow; r++) {
	    for (c = pFont->info.firstCol; c <= pFont->info.lastCol; c++) {
		ci = *pci++;
		if (ci) {
		    MINMAX(ascent, &ci->metrics);
		    MINMAX(descent, &ci->metrics);
		    MINMAX(leftSideBearing, &ci->metrics);
		    MINMAX(rightSideBearing, &ci->metrics);
		    MINMAX(characterWidth, &ci->metrics);
		    if (ci->metrics.characterWidth < 0)
			numneg++;
		    else
			numpos++;
		    minbounds->attributes &= ci->metrics.attributes;
		    maxbounds->attributes |= ci->metrics.attributes;
		    overlap = ci->metrics.rightSideBearing -
			ci->metrics.characterWidth;
		    if (maxOverlap < overlap)
			maxOverlap = overlap;
		}
	    }
	}
    }
    if (numneg > numpos)
	pFont->info.drawDirection = RightToLeft;
    else
	pFont->info.drawDirection = LeftToRight;
    pFont->info.maxOverlap = maxOverlap;
}

void
bitmapComputeFontInkBounds(pFont)
    FontPtr     pFont;
{
    BitmapFontPtr  bitmapFont = (BitmapFontPtr) pFont->fontPrivate;
    int         nchars;
    int         r,
                c;
    CharInfoPtr *pci,
                cit;
    xCharInfo  *ci;
    int         offset;
    xCharInfo  *minbounds,
               *maxbounds;
    int         i;

    if (!bitmapFont->ink_metrics) {
	if (bitmapFont->bitmapExtra) {
	    bitmapFont->bitmapExtra->info.ink_minbounds = bitmapFont->bitmapExtra->info.minbounds;
	    bitmapFont->bitmapExtra->info.ink_maxbounds = bitmapFont->bitmapExtra->info.maxbounds;
	}
	pFont->info.ink_minbounds = pFont->info.minbounds;
	pFont->info.ink_maxbounds = pFont->info.maxbounds;
    } else {
	if (bitmapFont->bitmapExtra) {
	    minbounds = &bitmapFont->bitmapExtra->info.ink_minbounds;
	    maxbounds = &bitmapFont->bitmapExtra->info.ink_maxbounds;
	} else {
	    minbounds = &pFont->info.ink_minbounds;
	    maxbounds = &pFont->info.ink_maxbounds;
	}
	*minbounds = initMinMetrics;
	*maxbounds = initMaxMetrics;
	nchars = bitmapFont->num_chars;
	for (i = 0, ci = bitmapFont->ink_metrics; i < nchars; i++, ci++) {
	    MINMAX(ascent, ci);
	    MINMAX(descent, ci);
	    MINMAX(leftSideBearing, ci);
	    MINMAX(rightSideBearing, ci);
	    MINMAX(characterWidth, ci);
	    minbounds->attributes &= ci->attributes;
	    maxbounds->attributes |= ci->attributes;
	}
	if (bitmapFont->bitmapExtra) {
	    minbounds = &pFont->info.ink_minbounds;
	    maxbounds = &pFont->info.ink_maxbounds;
	    *minbounds = initMinMetrics;
	    *maxbounds = initMaxMetrics;
	    pci = bitmapFont->encoding;
	    for (r = pFont->info.firstRow; r <= pFont->info.lastRow; r++) {
		for (c = pFont->info.firstCol; c <= pFont->info.lastCol; c++) {
		    cit = *pci++;
		    if (cit) {
			offset = cit - bitmapFont->metrics;
			ci = &bitmapFont->ink_metrics[offset];
			MINMAX(ascent, ci);
			MINMAX(descent, ci);
			MINMAX(leftSideBearing, ci);
			MINMAX(rightSideBearing, ci);
			MINMAX(characterWidth, ci);
			minbounds->attributes &= ci->attributes;
			maxbounds->attributes |= ci->attributes;
		    }
		}
	    }
	}
    }
}

Bool
bitmapAddInkMetrics(pFont)
    FontPtr     pFont;
{
    BitmapFontPtr  bitmapFont;
    int         i;

    bitmapFont = (BitmapFontPtr) pFont->fontPrivate;
    bitmapFont->ink_metrics = (xCharInfo *) xalloc(bitmapFont->num_chars * sizeof(xCharInfo));
    if (!bitmapFont->ink_metrics)
	return FALSE;
    for (i = 0; i < bitmapFont->num_chars; i++)
	FontCharInkMetrics(pFont, &bitmapFont->metrics[i], &bitmapFont->ink_metrics[i]);
    pFont->info.inkMetrics = TRUE;
    return TRUE;
}

/* ARGSUSED */
int
bitmapComputeWeight(pFont)
    FontPtr     pFont;
{
    return 10;
}
