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
#ifndef FONTSTRUCT_H
#define FONTSTRUCT_H 1
#include "font.h"
#include "misc.h"

typedef struct _CharInfo {
    xCharInfo	metrics;	/* info preformatted for Queries */
    unsigned	byteOffset:24;	/* byte offset of the raster from pGlyphs */
    Bool	exists:1;	/* true iff glyph exists for this char */
    unsigned	pad:7;		/* must be zero for now */
} CharInfoRec;

typedef struct _FontInfo {
    unsigned int	version1;   /* version stamp */
    unsigned int	allExist;
    unsigned int	drawDirection;
    unsigned int	noOverlap;	/* true if:
					 * max(rightSideBearing-characterWidth)
					 * <= minbounds->metrics.leftSideBearing
					 */
    unsigned int	constantMetrics;
    unsigned int	terminalFont;	/* Should be deprecated!  true if:
					   constant metrics &&
					   leftSideBearing == 0 &&
					   rightSideBearing == characterWidth &&
					   ascent == fontAscent &&
					   descent == fontDescent
					*/
    unsigned int	linear:1;	/* true if firstRow == lastRow */
    unsigned int	constantWidth:1;  /* true if minbounds->metrics.characterWidth
					   *      == maxbounds->metrics.characterWidth
					   */
    unsigned int	inkInside:1;    /* true if for all defined glyphs:
					 * leftSideBearing >= 0 &&
					 * rightSideBearing <= characterWidth &&
					 * -fontDescent <= ascent <= fontAscent &&
					 * -fontAscent <= descent <= fontDescent
					 */
    unsigned int	inkMetrics:1;	/* ink metrics != bitmap metrics */
					/* used with terminalFont */
					/* see font's pInk{CI,Min,Max} */
    unsigned int	padding:28;
    unsigned int	firstCol;
    unsigned int	lastCol;
    unsigned int	firstRow;
    unsigned int	lastRow;
    unsigned int	nProps;
    unsigned int	lenStrings; /* length in bytes of string table */
    unsigned int	chDefault;  /* default character */ 
    int			fontDescent; /* minimum for quality typography */
    int			fontAscent;  /* minimum for quality typography */
    CharInfoRec		minbounds;  /* MIN of glyph metrics over all chars */
    CharInfoRec		maxbounds;  /* MAX of glyph metrics over all chars */
    unsigned int	pixDepth;   /* intensity bits per pixel */
    unsigned int	glyphSets;  /* number of sets of glyphs, for
					    sub-pixel positioning */
    unsigned int	version2;   /* version stamp double-check */
} FontInfoRec;

typedef struct _ExtentInfo {
    DrawDirection	drawDirection;
    int			fontAscent;
    int			fontDescent;
    int			overallAscent;
    int			overallDescent;
    int			overallWidth;
    int			overallLeft;
    int			overallRight;
} ExtentInfoRec;

#endif /* FONTSTRUCT_H */

