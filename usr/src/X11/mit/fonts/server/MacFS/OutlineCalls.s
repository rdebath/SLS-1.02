#
#Copyright 1991 by Apple Computer, Inc, Cupertino, California
#			All Rights Reserved
#
#Permission to use, copy, modify, and distribute this software
#for any purpose and without fee is hereby granted, provided
#that the above copyright notice appear in all copies.
#
#APPLE MAKES NO WARRANTY OR REPRESENTATION, EITHER EXPRESS,
#OR IMPLIED, WITH RESPECT TO THIS SOFTWARE, ITS QUALITY,
#PERFORMANCE, MERCHANABILITY, OR FITNESS FOR A PARTICULAR
#PURPOSE. AS A RESULT, THIS SOFTWARE IS PROVIDED "AS IS,"
#AND YOU THE USER ARE ASSUMING THE ENTIRE RISK AS TO ITS
#QUALITY AND PERFORMANCE. IN NO EVENT WILL APPLE BE LIABLE 
#FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL
#DAMAGES RESULTING FROM ANY DEFECT IN THE SOFTWARE.
#
#THE WARRANTY AND REMEDIES SET FORTH ABOVE ARE EXCLUSIVE
#AND IN LIEU OF ALL OTHERS, ORAL OR WRITTEN, EXPRESS OR
#IMPLIED.
#
# /**********/
# /* ERRORS */
# /**********/
# /*						Boolean IsOutline
# **
# **	IsOutline indicates whether or not the current grafportsetting
# **	with the numer and denom loads a Spline Font.  FMSwapFont is called
# **	from inside of this function.
# **
# */
# extern pascal Boolean IsOutline(Point numer, Point denom)
# = {0x7000 + 0x0000,0xA854};
# /*						SetOutlinePreferred
# **
# **	Sets a mode to choose a matching spline font over an exact bitmap match.
# **
# */
# extern pascal SetOutlinePreferred(Boolean outlinePreferred)
# = {0x7000 + 0x0001,0xA854};
# -- 40 "./OutlineCalls.h"
# /*						Boolean GetOutlinePreferred
# **
# **	Gets the state of the OutlinePreferred flag.
# **
# */
# extern pascal Boolean GetOutlinePreferred()
# = {0x7000 + 0x0009,0xA854};
# /*						OSErr OutlineMetrics
# **
# **	Uses count,textPtr, numer and denom with the current grafport to
# **	load a spline font and return yMax, yMin, advance widths, left side bearings and
# **	Rects.  A nil is passed for metrics not wanted.
# **
# */
# extern pascal OSErr OutlineMetrics(short count, PtrtextPtr, Point numer, Point denom,
#short *yMax, short *yMin, Fixed *awArray, Fixed *lsbArray, Rect *boundsArray)
# = {0x7000 + 0x0008,0xA854};
# -- 60 "./OutlineCalls.h"
# /*						SetPreserveGlyph
# **
# **	Sets a line height state specifying that all bits of the spline font bitmaps
# **	should be blitted (e.g., characters above the ascender or chars below the descender).
# **	Otherwise, squash the character to fit into the ascender and descender.
# **	Set the flag true if all bits should be blitted outside of the line height.  
# **	Set false if characters that go outside line height should be squashed.
# **
# */
# extern pascal SetPreserveGlyph(Boolean preserveGlyphs)
# = {0x7000 + 0x000a,0xA854};
# /*						GetPreserveGlyph
# **
# **	Gets the mode of the state of preserving glyphs.
# **
# */
# extern pascal Boolean GetPreserveGlyph()
# = {0x7000 + 0x000b,0xA854};
# -- 80 "./OutlineCalls.h"
# /*						FlushFonts
# **
# **	FlushFonts flushed the font managers caches (i.e., width tables, sfnt caches)
# **
# */
# extern pascal OSErr FlushFonts()
# = {0x7000 + 0x000c,0xA854};
# /*						Boolean IsOutline
# **
# **	IsOutline indicates whether or not the current grafportsetting
# **	with the numer and denom loads a Spline Font.  FMSwapFont is called
# **	from inside of this function.
# **
# */
# Boolean CIsOutline(Point numer, Point denom)
# {
	file	"OutlineCalls.c"
	text
	global	CIsOutline
CIsOutline:
	link.l	%fp,&F%1
	movm.l	&M%1,(4,%sp)
	fmovm	&FPM%1,(FPO%1,%sp)
# return IsOutline(numer, denom);
	mov.l	%d2,-4(%fp)
	mov.l	%a5,-8(%fp)
	sub.l	&2,%sp
	lea.l	8(%fp),%a0
	mov.l	0(%a0),-(%sp)
	lea.l	12(%fp),%a0
	mov.l	0(%a0),-(%sp)
	mov.l	CurrentA5,%a5
#ASM
	short	0x7000
	short	0xa854
#ASMEND

#	addp.w	&8,%sp
	mov.l	-4(%fp),%d2
	mov.l	-8(%fp),%a5
	mov.b	(%sp)+,%d0
	and.l   &0377,%d0
	bra	L70
# }
L70:
#OPTD
	fmovm	(FPO%1,%sp),&FPM%1
	movm.l	(4,%sp),&M%1
	unlk	%fp
	rts
	set	O%1,8
	set	F%1,-12
	set	FPO%1,4
	set	FPM%1,0x0000
	set	M%1,0x0000
	set	CurrentA5,	0x0904
# /*						SetOutlinePreferred
# **
# **	Sets a mode to choose a matching spline font over an exact bitmap match.
# **
# */
# void CSetOutlinePreferred(Boolean outlinePreferred)
# {
	global	CSetOutlinePreferred
CSetOutlinePreferred:
	link.l	%fp,&F%2
	movm.l	&M%2,(4,%sp)
	fmovm	&FPM%2,(FPO%2,%sp)
# SetOutlinePreferred(outlinePreferred);
	mov.l	%d2,-4(%fp)
	mov.l	%a5,-8(%fp)
	sub.l	&4,%sp
	mov.b	11(%fp),%d0
	mov.b	%d0,-(%sp)
	mov.l	CurrentA5,%a5
#ASM
	short	0x7001
	short	0xa854
#ASMEND

#	addp.w	&2,%sp
	mov.l	-4(%fp),%d2
	mov.l	-8(%fp),%a5
	mov.l	(%sp)+,%d0
# }
L72:
#OPTD
	fmovm	(FPO%2,%sp),&FPM%2
	movm.l	(4,%sp),&M%2
	unlk	%fp
	rts
	set	O%2,8
	set	F%2,-12
	set	FPO%2,4
	set	FPM%2,0x0000
	set	M%2,0x0000
	set	CurrentA5,	0x0904
# /*						Boolean GetOutlinePreferred
# **
# **	Gets the state of the OutlinePreferred flag.
# **
# */
# Boolean CGetOutlinePreferred()
# {
	global	CGetOutlinePreferred
CGetOutlinePreferred:
	link.l	%fp,&F%3
	movm.l	&M%3,(4,%sp)
	fmovm	&FPM%3,(FPO%3,%sp)
# return GetOutlinePreferred();
	mov.l	%d2,-4(%fp)
	mov.l	%a5,-8(%fp)
	sub.l	&2,%sp
	mov.l	CurrentA5,%a5
#ASM
	short	0x7009
	short	0xa854
#ASMEND

	mov.l	-4(%fp),%d2
	mov.l	-8(%fp),%a5
	mov.b	(%sp)+,%d0
	and.l   &0377,%d0
	bra	L74
# }
L74:
#OPTD
	fmovm	(FPO%3,%sp),&FPM%3
	movm.l	(4,%sp),&M%3
	unlk	%fp
	rts
	set	O%3,8
	set	F%3,-12
	set	FPO%3,4
	set	FPM%3,0x0000
	set	M%3,0x0000
	set	CurrentA5,	0x0904
# /*						OSErr OutlineMetrics
# **
# **	Uses count,textPtr, numer and denom with the current grafport to
# **	load a spline font and return yMax, yMin, advance widths, left side bearings and
# **	Rects.  A nil is passed for metrics not wanted.
# **
# */
# OSErr COutlineMetrics(short count, PtrtextPtr, Point numer, Point denom,
#short *yMax, short *yMin, Fixed *awArray, Fixed *lsbArray, Rect *boundsArray)
# {
	global	COutlineMetrics
COutlineMetrics:
	link.l	%fp,&F%4
	movm.l	&M%4,(4,%sp)
	fmovm	&FPM%4,(FPO%4,%sp)
# return OutlineMetrics(counttextPtr,numer,denom,yMax,yMin,awArray,lsbArray,boundsArray);
	mov.l	%d2,-4(%fp)
	mov.l	%a5,-8(%fp)
	sub.l	&2,%sp
	mov.w	10(%fp),-(%sp)
	mov.l	12(%fp),-(%sp)
	lea.l	16(%fp),%a0
	mov.l	0(%a0),-(%sp)
	lea.l	20(%fp),%a0
	mov.l	0(%a0),-(%sp)
	mov.l	24(%fp),-(%sp)
	mov.l	28(%fp),-(%sp)
	mov.l	32(%fp),-(%sp)
	mov.l	36(%fp),-(%sp)
	mov.l	40(%fp),-(%sp)
	mov.l	CurrentA5,%a5
#ASM
	short	0x7008
	short	0xa854
#ASMEND

#	addp.w	&34,%sp
	mov.l	-4(%fp),%d2
	mov.l	-8(%fp),%a5
	mov.w	(%sp)+,%d0
	ext.l   %d0
	bra	L76
# }
L76:
#OPTD
	fmovm	(FPO%4,%sp),&FPM%4
	movm.l	(4,%sp),&M%4
	unlk	%fp
	rts
	set	O%4,8
	set	F%4,-12
	set	FPO%4,4
	set	FPM%4,0x0000
	set	M%4,0x0000
	set	CurrentA5,	0x0904
# /*						SetPreserveGlyph
# **
# **	Sets a line height state specifying that all bits of the spline font bitmaps
# **	should be blitted (e.g., characters above the ascender or chars below the descender).
# **	Otherwise, squash the character to fit into the ascender and descender.
# **	Set the flag true if all bits should be blitted outside of the line height.  
# **	Set false if characters that go outside line height should be squashed.
# **
# */
# -- 60 "OutlineCalls.c"
# void CSetPreserveGlyph(Boolean preserveGlyphs)
# {
	global	CSetPreserveGlyph
CSetPreserveGlyph:
	link.l	%fp,&F%5
	movm.l	&M%5,(4,%sp)
	fmovm	&FPM%5,(FPO%5,%sp)
# SetPreserveGlyph(preserveGlyphs);
	mov.l	%d2,-4(%fp)
	mov.l	%a5,-8(%fp)
	sub.l	&4,%sp
	mov.b	11(%fp),%d0
	mov.b	%d0,-(%sp)
	mov.l	CurrentA5,%a5
#ASM
	short	0x700a
	short	0xa854
#ASMEND

#	addp.w	&2,%sp
	mov.l	-4(%fp),%d2
	mov.l	-8(%fp),%a5
	mov.l	(%sp)+,%d0
# }
L78:
#OPTD
	fmovm	(FPO%5,%sp),&FPM%5
	movm.l	(4,%sp),&M%5
	unlk	%fp
	rts
	set	O%5,8
	set	F%5,-12
	set	FPO%5,4
	set	FPM%5,0x0000
	set	M%5,0x0000
	set	CurrentA5,	0x0904
# /*						GetPreserveGlyph
# **
# **	Gets the mode of the state of preserving glyphs.
# **
# */
# Boolean CGetPreserveGlyph()
# {
	global	CGetPreserveGlyph
CGetPreserveGlyph:
	link.l	%fp,&F%6
	movm.l	&M%6,(4,%sp)
	fmovm	&FPM%6,(FPO%6,%sp)
# return GetPreserveGlyph();
	mov.l	%d2,-4(%fp)
	mov.l	%a5,-8(%fp)
	sub.l	&2,%sp
	mov.l	CurrentA5,%a5
#ASM
	short	0x700b
	short	0xa854
#ASMEND

	mov.l	-4(%fp),%d2
	mov.l	-8(%fp),%a5
	mov.b	(%sp)+,%d0
	and.l   &0377,%d0
	bra	L80
# }
L80:
#OPTD
	fmovm	(FPO%6,%sp),&FPM%6
	movm.l	(4,%sp),&M%6
	unlk	%fp
	rts
	set	O%6,8
	set	F%6,-12
	set	FPO%6,4
	set	FPM%6,0x0000
	set	M%6,0x0000
	set	CurrentA5,	0x0904
# /*						FlushFonts
# **
# **	FlushFonts flushed the font managers caches (i.e., width tables, sfnt caches)
# **
# */
# -- 80 "OutlineCalls.c"
# OSErr CFlushFonts()
# {
	global	CFlushFonts
CFlushFonts:
	link.l	%fp,&F%7
	movm.l	&M%7,(4,%sp)
	fmovm	&FPM%7,(FPO%7,%sp)
# return FlushFonts();
	mov.l	%d2,-4(%fp)
	mov.l	%a5,-8(%fp)
	sub.l	&2,%sp
	mov.l	CurrentA5,%a5
#ASM
	short	0x700c
	short	0xa854
#ASMEND

	mov.l	-4(%fp),%d2
	mov.l	-8(%fp),%a5
	mov.w	(%sp)+,%d0
	ext.l   %d0
	bra	L82
# }
L82:
#OPTD
	fmovm	(FPO%7,%sp),&FPM%7
	movm.l	(4,%sp),&M%7
	unlk	%fp
	rts
	set	O%7,8
	set	F%7,-12
	set	FPO%7,4
	set	FPM%7,0x0000
	set	M%7,0x0000
	set	CurrentA5,	0x0904
