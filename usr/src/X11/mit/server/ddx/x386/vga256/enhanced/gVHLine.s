/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/gVHLine.s,v 1.14 1992/08/29 11:10:01 dawes Exp $ */
/*******************************************************************************
			Copyright 1992 by Glenn G. Lai 

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Glenn G. Lai not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

Glenn G. Lai DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

Glenn G. Lai
P.O. Box 4314
Austin, Tx 78765
(glenn@cs.utexas.edu)
5/11/92
*******************************************************************************/

#include "assembler.h"

#include "vgaAsm.h"

	.file "gVHLine.s"

#if !defined(SYSV) && !defined(SVR4)
#define GlennsInit _GlennsInit
#define GlennsVLineGXcopy _GlennsVLineGXcopy
#define GlennsHLineGXcopy _GlennsHLineGXcopy
#define UglyHFillGXcopy _UglyHFillGXcopy
#endif

	.data
.copyright:
	STRING	"Copyright 1992 by Glenn G. Lai" 
/*
	void GlennsHLineGXcopy(dst, fill, len, screenWidth);
	void GLennsVLineGXcopy(dst, fill, len, screenWidth);
*/
#define THRESHOLD	$100
	.data
	ALIGNDATA4
.vgaWriteTop:
	.long	0
	.text
	ALIGNTEXT4
	.globl  GlennsInit
GlennsInit:
	movl	vgaWriteBottom, %eax
	addl	$0x103ff, %eax
	movl	%eax, .vgaWriteTop
.return:
	ret
	ALIGNTEXT4
	.globl	GlennsVLineGXcopy
GlennsVLineGXcopy:
	movl	12(%esp), %ecx
	orl	%ecx, %ecx
	jz	.return
.nonNullVLine:
	movl	4(%esp), %eax
	subl	VGABASE, %eax
	jnc	.drawOneMVLine
/* VLine in a pixmap or an undrawable window */
	addl	VGABASE, %eax
	movl	8(%esp), %edx
	pushl	%esi
	movl	20(%esp), %esi
.vLine:
	movb	%dl, (%eax)
	addl	%esi, %eax
	decl	%ecx
	jnz	.vLine
	popl	%esi
	ret
	ALIGNTEXT4
.drawOneMVLine:
	pushl 	%ebp
	movl 	%esp,%ebp
	pushl 	%edi
	pushl	%esi
	pushl	%ebx
	movl	12(%ebp), %ebx
	movl	20(%ebp), %esi
	movl 	%eax, %edi
	shrl	$16, %eax
	movb	%al, .segment
	andl	$0xffff, %edi
	addl	vgaWriteBottom, %edi
.nextSegment:
	movl	$0x3cd, %edx
	OUT_B	(%dx)
	movl	$0, %edx
	movl	vgaWriteTop, %eax
	addl	%esi, %eax
	decl	%eax
	subl	%edi, %eax
	divl	%esi
	cmpl	%eax, %ecx
	jg	.moreBytes
	movl	%ecx, %eax
.moreBytes:
	subl	%eax, %ecx
.nextVPoint:
	movb	%bl, (%edi)
	addl	%esi, %edi
	decl	%eax
	jnz	.nextVPoint
	orl	%ecx, %ecx
	jz	.done
	subl	$0x10000, %edi
	movb	.segment, %al
	incb	%al
	movb	%al, .segment
	jmp	.nextSegment
	.globl	GlennsHLineGXcopy
	ALIGNTEXT4
GlennsHLineGXcopy:
	movl	12(%esp), %ecx
	orl	%ecx, %ecx
	jz	.return
.nonNullHLine:
	movl	4(%esp), %eax
	subl	VGABASE, %eax
	jnc	.drawOneMHLine
	addl	VGABASE, %eax
	cmpl	$3, %ecx
	jle	.shortHLine
	pushl	%edi
	movl	%eax, %edi
	movl	12(%esp), %eax
	testl	$1, %edi
	jz	.k
	stosb
	decl	%ecx
.k:
	test	$2, %edi
	jz	.kk
	stosw
	subl	$2, %ecx
.kk:
	movl	%ecx, %edx
	andl	$3, %edx
	shrl	$2, %ecx
	orl	%ecx, %ecx
	jz	.zzz
	rep
	stosl
.zzz:
	testl	$1, %edx
	jz	.kkkk
	stosb
.kkkk:
	testl	$2, %edx
	jz	.kkkkk
	movw	%ax, (%edi)
.kkkkk:
	popl	%edi
	ret
	ALIGNTEXT4
.shortHLine:
	movl	8(%esp), %edx
.g:	movb	%dl, (%eax)
	incl	%eax
	decl	%ecx
	jnz	.g
	ret

	.globl	UglyHFillGXcopy
	ALIGNTEXT4
.drawOneMHLine:
UglyHFillGXcopy:
	cmpl	$1, %ecx
	jne	.twoOrMoreHPs
	movl	%eax, %ecx
	shrl	$16, %eax
	movl	$0x3cd, %edx
	OUT_B	(%dx)
	andl	$0xffff, %ecx
	addl	vgaWriteBottom, %ecx
	movl	8(%esp), %eax
	movb	%al, (%ecx)
	ret
.twoOrMoreHPs:
/* Make THRESHOLD >= 3!!! */
	cmpl	THRESHOLD, %ecx
	jge	.pMode

	pushl	%edi
	movl 	%eax, %edi
        andl    $0x0ffff, %edi
        addl    vgaWriteBottom, %edi
        shrl    $16, %eax
        movl    $0x3cd, %edx
        OUT_B    (%dx)
        movl    12(%esp), %eax
        testl   $1, %edi
        jz      .hLineLeftAligned
        decl    %ecx
        stosb
/* At least one pixel left here */
.hLineLeftAligned:
        shrl    $1, %ecx
        jz      .hLineLastPixel
        rep
        stosw
        jnc     .hLineEnd
.hLineLastPixel:
        movb    %al, (%edi)
.hLineEnd:
        popl    %edi
        ret


	.data
	ALIGNDATA4
.startMaskTable:
	.value	0x0f02
	.value	0x0802
	.value	0x0c02
	.value	0x0e02
.endMaskTable:
	.value	0x0f02
	.value	0x0102
	.value	0x0302
	.value	0x0702
.segment:
	.byte	0
	.text
	ALIGNTEXT4
.pMode:

	pushl 	%ebp
	movl 	%esp,%ebp
	pushl 	%edi
	pushl	%esi
	pushl	%ebx
	movl	12(%ebp), %ebx
	movl 	%eax, %edi

	movl	$0x3c4, %edx
	movl	$0x0604, %eax 
	OUT_W	(%dx)
	movl	$0x3ce, %edx
	movl	$0x0001, %eax
	OUT_W	(%dx)
	movl	$0xff08, %eax
	OUT_W	(%dx)
	movl	$0x0003, %eax
	OUT_W	(%dx)
	movl	$0x4005, %eax
	OUT_W	(%dx)
.compute256k:
	movl	%edi, %eax
	shrl	$18, %eax
	movl	$0x3cd, %edx
	OUT_B	(%dx)
	andl	$0x3ffff, %edi
	movl	%edi, %esi
	addl	$3, %esi
	andl	$0xfffffffc, %esi
	subl	%edi, %esi
	shll	$1, %esi
	movw	.startMaskTable(%esi), %ax
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	shrl	$2, %edi
	addl	vgaWriteBottom, %edi
	shrl	$1, %esi
	jz	.middle
	subl	%esi, %ecx
	movb	%bl, (%edi)
	incl	%edi
	movl	$0x0f02, %eax
	OUT_W	(%dx)
.middle:
	movl	%ecx, %esi
	andl	$3, %esi
	pushl	%esi
	shrl	$2, %ecx
	orl	%ecx, %ecx
	jz	.end
	testl	$1, %edi
	jz	.kkk
	movb	%bl, (%edi)
	incl	%edi
	decl	%ecx
.kkk:
	movl	%ecx, %esi
	andl	$3, %esi
	shrl	$2, %ecx
	orl	%ecx, %ecx
	jz	.mmm
	movl	%ebx, %eax
	rep
	stosl
.mmm:
	testl	$2, %esi
	jz	.lll
	movw	%bx, (%edi)
	addl	$2, %edi
.lll:
	testl	$1, %esi
	jz	.end
	movb	%bl, (%edi)
	incl	%edi
.end:
	popl	%esi
	shll	$1, %esi
	jz	.pDone
	movw	.endMaskTable(%esi), %ax
	OUT_W	(%dx)
	movb	%bl, (%edi)
.pDone:
	movl	$0x0c04, %eax
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	$0x0f02, %eax
	OUT_W	(%dx)
.done:
	leal 	-12(%ebp), %esp
	popl 	%ebx
	popl 	%esi
	popl 	%edi
	leave
	ret

