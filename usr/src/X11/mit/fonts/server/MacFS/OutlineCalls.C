#include <types.h>
#include <memory.h>
#include <quickdraw.h>

#include "OutlineCalls.h"
/*						Boolean IsOutline
**
**	IsOutline indicates whether or not the current grafport setting
**	with the numer and denom loads a Spline Font.  FMSwapFont is called
**	from inside of this function.
**
*/
Boolean CIsOutline(Point numer, Point denom)
{
	return IsOutline(numer, denom);
}

/*						SetOutlinePreferred
**
**	Sets a mode to choose a matching spline font over an exact bitmap match.
**
*/
void CSetOutlinePreferred(Boolean outlinePreferred)
{
	SetOutlinePreferred(outlinePreferred);
}

/*						Boolean GetOutlinePreferred
**
**	Gets the state of the OutlinePreferred flag.
**
*/
Boolean CGetOutlinePreferred()
{
	return GetOutlinePreferred();
}

/*						OSErr OutlineMetrics
**
**	Uses count, textPtr, numer and denom with the current grafport to
**	load a spline font and return yMax, yMin, advance widths, left side bearings and
**	Rects.  A nil is passed for metrics not wanted.
**
*/
OSErr COutlineMetrics(	short count, Ptr textPtr, Point numer, Point denom,
						short *yMax, short *yMin, Fixed *awArray, Fixed *lsbArray, Rect *boundsArray)
{
	return OutlineMetrics(count,textPtr,numer,denom,yMax,yMin,awArray,lsbArray,boundsArray);
}

/*						SetPreserveGlyph
**
**	Sets a line height state specifying that all bits of the spline font bitmaps
**	should be blitted (e.g., characters above the ascender or chars below the descender).
**	Otherwise, squash the character to fit into the ascender and descender.
**	Set the flag true if all bits should be blitted outside of the line height.  
**	Set false if characters that go outside line height should be squashed.
**
*/
void CSetPreserveGlyph(Boolean preserveGlyphs)
{
	SetPreserveGlyph(preserveGlyphs);
}

/*						GetPreserveGlyph
**
**	Gets the mode of the state of preserving glyphs.
**
*/
Boolean CGetPreserveGlyph()
{
	return GetPreserveGlyph();
}

/*						FlushFonts
**
**	FlushFonts flushed the font managers caches (i.e., width tables, sfnt caches)
**
*/
OSErr CFlushFonts()
{
	return FlushFonts();
}
