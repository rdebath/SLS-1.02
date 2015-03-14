/*
 * $XConsortium: fontutil.c,v 1.3 91/05/30 19:08:01 keith Exp $
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

#include    "fontmisc.h"
#include    "fontstruct.h"

void
GetGlyphs(font, count, chars, fontEncoding, glyphcount, glyphs)
    FontPtr     font;
    unsigned long count;
    unsigned char *chars;
    FontEncoding fontEncoding;
    unsigned long *glyphcount;	/* RETURN */
    CharInfoPtr *glyphs;	/* RETURN */
{
    (*font->get_glyphs) (font, count, chars, fontEncoding, glyphcount, glyphs);
}

#define MIN(a,b)    ((a)<(b)?(a):(b))
#define MAX(a,b)    ((a)>(b)?(a):(b))

void
QueryGlyphExtents(pFont, charinfo, count, info)
    FontPtr     pFont;
    xCharInfo **charinfo;
    unsigned long count;
    ExtentInfoRec *info;
{
    register unsigned long i;
    xCharInfo  *pCI;

    info->drawDirection = pFont->info.drawDirection;

    info->fontAscent = pFont->info.fontAscent;
    info->fontDescent = pFont->info.fontDescent;

    if (count != 0) {

	pCI = *charinfo++;
	info->overallAscent = pCI->ascent;
	info->overallDescent = pCI->descent;
	info->overallLeft = pCI->leftSideBearing;
	info->overallRight = pCI->rightSideBearing;
	info->overallWidth = pCI->characterWidth;

	if (pFont->info.constantMetrics && pFont->info.noOverlap) {
	    info->overallWidth *= count;
	    info->overallRight += (info->overallWidth -
				   pCI->characterWidth);
	} else {
	    for (i = 1; i < count; i++) {
		pCI = *charinfo++;
		info->overallAscent = MAX(
					  info->overallAscent,
					  pCI->ascent);
		info->overallDescent = MAX(
					   info->overallDescent,
					   pCI->descent);
		info->overallLeft = MIN(
					info->overallLeft,
				  info->overallWidth + pCI->leftSideBearing);
		info->overallRight = MAX(
					 info->overallRight,
				 info->overallWidth + pCI->rightSideBearing);
		/*
		 * yes, this order is correct; overallWidth IS incremented
		 * last
		 */
		info->overallWidth += pCI->characterWidth;
	    }
	}
    } else {
	info->overallAscent = 0;
	info->overallDescent = 0;
	info->overallWidth = 0;
	info->overallLeft = 0;
	info->overallRight = 0;
    }
}

Bool
QueryTextExtents(pFont, count, chars, info)
    FontPtr     pFont;
    unsigned long count;
    unsigned char *chars;
    ExtentInfoRec *info;
{
    xCharInfo **charinfo;
    unsigned long n;
    FontEncoding encoding;
    int         cm;
    int		i;
    unsigned long   t;
    xCharInfo	*defaultChar = 0;
    char	defc[2];
    int		firstReal;

    charinfo = (xCharInfo **) xalloc(count * sizeof(xCharInfo *));
    if (!charinfo)
	return FALSE;
    encoding = TwoD16Bit;
    if (pFont->info.lastRow == 0)
	encoding = Linear16Bit;
    (*pFont->get_metrics) (pFont, count, chars, encoding, &n, charinfo);

    /* Do default character substitution as get_metrics doesn't */

#define IsNonExistantChar(ci) ((ci)->ascent == 0 && \
			       (ci)->descent == 0 && \
			       (ci)->leftSideBearing == 0 && \
			       (ci)->rightSideBearing == 0 && \
			       (ci)->characterWidth == 0)

    firstReal = n;
    defc[0] = pFont->info.defaultCh >> 8;
    defc[1] = pFont->info.defaultCh;
    (*pFont->get_metrics) (pFont, 1, defc, encoding, &t, &defaultChar);
    if (IsNonExistantChar (defaultChar))
	defaultChar = 0;
    for (i = 0; i < n; i++)
    {
	if (IsNonExistantChar (charinfo[i]))
	{
	    if (!defaultChar)
		continue;
	    charinfo[i] = defaultChar;
	}
	if (firstReal == n)
	    firstReal = i;
    }
    cm = pFont->info.constantMetrics;
    pFont->info.constantMetrics = FALSE;
    QueryGlyphExtents(pFont, charinfo + firstReal, n - firstReal, info);
    pFont->info.constantMetrics = cm;
    xfree(charinfo);
    return TRUE;
}
