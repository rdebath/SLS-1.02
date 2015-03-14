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
	global	MacInitFonts
MacInitFonts:
	link.l	%fp,&F%7
	movm.l	&M%7,(4,%sp)
	fmovm	&FPM%7,(FPO%7,%sp)
	mov.l	%d2,-4(%fp)
	mov.l	%a5,-8(%fp)
	sub.l	&2,%sp
	mov.l	CurrentA5,%a5
#ASM
	short	0x7000
	short	0xa8fe
#ASMEND

	mov.l	-4(%fp),%d2
	mov.l	-8(%fp),%a5
	mov.w	(%sp)+,%d0
	ext.l   %d0
	fmovm	(FPO%7,%sp),&FPM%7
	movm.l	(4,%sp),&M%7
	unlk	%fp
	rts
	set	F%7,-12
	set	FPO%7,4
	set	FPM%7,0x0000
	set	M%7,0x0000
	set	CurrentA5,	0x0904
