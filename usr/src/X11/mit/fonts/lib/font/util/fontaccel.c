/*
 * $XConsortium: fontaccel.c,v 1.2 91/05/11 09:16:34 keith Exp $
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

FontComputeInfoAccelerators(pFontInfo)
    FontInfoPtr pFontInfo;
{
    pFontInfo->noOverlap = FALSE;
    if (pFontInfo->maxOverlap <= pFontInfo->minbounds.leftSideBearing)
	pFontInfo->noOverlap = TRUE;

    if ((pFontInfo->minbounds.ascent == pFontInfo->maxbounds.ascent) &&
	    (pFontInfo->minbounds.descent == pFontInfo->maxbounds.descent) &&
	    (pFontInfo->minbounds.leftSideBearing ==
	     pFontInfo->maxbounds.leftSideBearing) &&
	    (pFontInfo->minbounds.rightSideBearing ==
	     pFontInfo->maxbounds.rightSideBearing) &&
	    (pFontInfo->minbounds.characterWidth ==
	     pFontInfo->maxbounds.characterWidth) &&
      (pFontInfo->minbounds.attributes == pFontInfo->maxbounds.attributes)) {
	pFontInfo->constantMetrics = TRUE;
	if ((pFontInfo->maxbounds.leftSideBearing == 0) &&
		(pFontInfo->maxbounds.rightSideBearing ==
		 pFontInfo->maxbounds.characterWidth) &&
		(pFontInfo->maxbounds.ascent == pFontInfo->fontAscent) &&
		(pFontInfo->maxbounds.descent == pFontInfo->fontDescent))
	    pFontInfo->terminalFont = TRUE;
	else
	    pFontInfo->terminalFont = FALSE;
    } else {
	pFontInfo->constantMetrics = FALSE;
	pFontInfo->terminalFont = FALSE;
    }
    if (pFontInfo->minbounds.characterWidth == pFontInfo->maxbounds.characterWidth)
	pFontInfo->constantWidth = TRUE;
    else
	pFontInfo->constantWidth = FALSE;

    if ((pFontInfo->minbounds.leftSideBearing >= 0) &&
	    (pFontInfo->maxOverlap <= 0) &&
	    (pFontInfo->minbounds.ascent >= -pFontInfo->fontDescent) &&
	    (pFontInfo->maxbounds.ascent <= pFontInfo->fontAscent) &&
	    (-pFontInfo->minbounds.descent <= pFontInfo->fontAscent) &&
	    (pFontInfo->maxbounds.descent <= pFontInfo->fontDescent))
	pFontInfo->inkInside = TRUE;
    else
	pFontInfo->inkInside = FALSE;
}

FontCouldBeTerminal(pFontInfo)
    FontInfoPtr pFontInfo;
{
    if ((pFontInfo->minbounds.leftSideBearing >= 0) &&
	    (pFontInfo->maxbounds.rightSideBearing <= pFontInfo->maxbounds.characterWidth) &&
	    (pFontInfo->minbounds.characterWidth == pFontInfo->maxbounds.characterWidth) &&
	    (pFontInfo->maxbounds.ascent <= pFontInfo->fontAscent) &&
	    (pFontInfo->maxbounds.descent <= pFontInfo->fontDescent) &&
	    (pFontInfo->maxbounds.leftSideBearing != 0 ||
	     pFontInfo->minbounds.rightSideBearing != pFontInfo->minbounds.characterWidth ||
	     pFontInfo->minbounds.ascent != pFontInfo->fontAscent ||
	     pFontInfo->minbounds.descent != pFontInfo->fontDescent)) {
	return TRUE;
    }
    return FALSE;
}
