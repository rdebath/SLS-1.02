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
/**********/
/* ERRORS */
/**********/
#define BITFONT_ERR					0x8099				/* Outline metrics was called with a bitmap font */

#define IS_OUTLINE					0x0000		
#define SET_OUTLINEPREFERRED		0x0001

#define OUTLINE_METRICS				0x0008
#define GET_OUTLINEPREFERRED		0x0009
#define SET_PRESERVE_GLYPHS			0x000a
#define GET_PRESERVE_GLYPHS			0x000b
#define FLUSH_FONTS					0x000c


/*						Boolean IsOutline
**
**	IsOutline indicates whether or not the current grafport setting
**	with the numer and denom loads a Spline Font.  FMSwapFont is called
**	from inside of this function.
**
*/
extern pascal Boolean IsOutline(Point numer, Point denom)
	= {0x7000 + IS_OUTLINE,0xA854};

/*						SetOutlinePreferred
**
**	Sets a mode to choose a matching spline font over an exact bitmap match.
**
*/
extern pascal SetOutlinePreferred(Boolean outlinePreferred)
	= {0x7000 + SET_OUTLINEPREFERRED,0xA854};

/*						Boolean GetOutlinePreferred
**
**	Gets the state of the OutlinePreferred flag.
**
*/
extern pascal Boolean GetOutlinePreferred()
	= {0x7000 + GET_OUTLINEPREFERRED,0xA854};

/*						OSErr OutlineMetrics
**
**	Uses count, textPtr, numer and denom with the current grafport to
**	load a spline font and return yMax, yMin, advance widths, left side bearings and
**	Rects.  A nil is passed for metrics not wanted.
**
*/
extern pascal OSErr OutlineMetrics(	short count, Ptr textPtr, Point numer, Point denom,
									short *yMax, short *yMin, Fixed *awArray, Fixed *lsbArray, Rect *boundsArray)
	= {0x7000 + OUTLINE_METRICS,0xA854};

/*						SetPreserveGlyph
**
**	Sets a line height state specifying that all bits of the spline font bitmaps
**	should be blitted (e.g., characters above the ascender or chars below the descender).
**	Otherwise, squash the character to fit into the ascender and descender.
**	Set the flag true if all bits should be blitted outside of the line height.  
**	Set false if characters that go outside line height should be squashed.
**
*/
extern pascal SetPreserveGlyph(Boolean preserveGlyphs)
	= {0x7000 + SET_PRESERVE_GLYPHS,0xA854};

/*						GetPreserveGlyph
**
**	Gets the mode of the state of preserving glyphs.
**
*/
extern pascal Boolean GetPreserveGlyph()
	= {0x7000 + GET_PRESERVE_GLYPHS,0xA854};

/*						FlushFonts
**
**	FlushFonts flushed the font managers caches (i.e., width tables, sfnt caches)
**
*/
extern pascal OSErr FlushFonts()
	= {0x7000 + FLUSH_FONTS,0xA854};
