/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/gFillCopy.s,v 1.13 1992/08/29 11:09:58 dawes Exp $ */
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

	.file "gFillCopy.s"

#if !defined(SYSV) && !defined(SVR4)
#define GlennsFillSolidGXcopy _GlennsFillSolidGXcopy
#define UglyHFillGXcopy _UglyHFillGXcopy
#endif

	.data
.copyright:
	STRING	"Copyright 1992 by Glenn G. Lai" 
/*
    void GlennsFillSolidGXcopy(dst, fill, h, w, screen width)
*/
/* Must be at least 18 */
#define THRESHOLD	$100
	.data
	ALIGNDATA4
.pixmapGXcopyTable:
	.long	0, .pixmapGXcopy1D, .pixmapGXcopy2D, .pixmapGXcopy3D
.pixmapMiddle:
	.long	0
	.text
	ALIGNTEXT4
	.globl	GlennsFillSolidGXcopy
GlennsFillSolidGXcopy:
	movl	12(%esp), %eax
	orl	16(%esp), %eax
	jz	.return
.nonNullBox:
	movl	4(%esp), %eax
	cmpl	VGABASE, %eax
	jnc	.drawableWindow
	pushl 	%ebp
	movl 	%esp,%ebp
	pushl 	%edi
	pushl	%esi
	pushl	%ebx
.pixmapOrUndrawableWindow:
	movl	12(%ebp), %ebx
	movl	20(%ebp), %ecx
	movl	16(%ebp), %edx
	movl	24(%ebp), %esi
	movl	%eax, %edi
	addl	$3, %eax
	andl	$0xfffffffc, %eax
	movl	%eax, .pixmapMiddle
	subl	%edi, %eax
	cmpl	%eax, %ecx
	jg	.fillLeft
	movl	%ecx, %eax
.fillLeft:
	subl	%eax, %ecx
	sall	$2, %eax
	jz	.fillRight
	call	*.pixmapGXcopyTable(%eax)
.fillRight:
	movl	%ecx, %eax
	andl	$3, %eax
	jz	.fillMiddle
	movl	%ecx, %edi
	andl	$0xfffffffc, %edi
	addl	.pixmapMiddle, %edi
	sall	$2, %eax
	call	*.pixmapGXcopyTable(%eax)
.fillMiddle:
	sarl	$2, %ecx
	jz	.done
	movl	%ebx, %eax
	movl	.pixmapMiddle, %ebx
.fillMiddle1:
	pushl	%ecx
	movl	%ebx, %edi
	rep
	stosl
	addl	%esi, %ebx
	popl	%ecx
	decl	%edx
	jnz	.fillMiddle1
	jmp	.done
	ALIGNTEXT4
.pixmapGXcopy1D:
	movl	%edx, %eax
.p1D:
	movb	%bl, (%edi)
	addl	%esi, %edi
	decl	%eax
	jnz	.p1D
	ret
	ALIGNTEXT4
.pixmapGXcopy2D:
	movl	%edx, %eax
.p2D:
	movw	%bx, (%edi)
	addl	%esi, %edi
	decl	%eax
	jnz	.p2D
	ret
	ALIGNTEXT4
.pixmapGXcopy3D:
	movl	%edx, %eax
.p3D:
	movb	%bl, (%edi)
	movw	%bx, 1(%edi)
	addl	%esi, %edi
	decl	%eax
	jnz	.p3D
	ret
	ALIGNTEXT4
.drawableWindow:
	subl	VGABASE, %eax
	movl	12(%esp), %ecx
	cmpl	$1, %ecx
	jne	.oneOrMoreHLines
	movl	16(%esp), %ecx


	jmp	UglyHFillGXcopy


	ALIGNTEXT4
.oneOrMoreHLines:
	pushl 	%ebp
	movl 	%esp,%ebp
	pushl 	%edi
	movl	%eax, %edi
	pushl	%esi
	pushl	%ebx
	movl	20(%ebp), %eax
	cmpl	$2, %eax
	jg	.isPModeOK
	movl	12(%ebp), %ebx
	je	.vvLine
	movl	%edi, %eax
	shrl	$16, %eax
	movl	$0x3cd, %edx
	andl	$0x0ffff, %edi
	addl	vgaWriteBottom, %edi
.nextV:
	OUT_B	(%dx)
	movl	vgaWriteTop, %esi
	addl	$0x3ff, %esi
	subl	%edi, %esi
	shrl	$10, %esi
	cmpl	%esi, %ecx
	jg	.moreVs
	movl	%ecx, %esi
.moreVs:
	subl	%esi, %ecx
.oneV:
	movb	%bl, (%edi)
	addl	$0x400, %edi
	decl	%esi
	jnz	.oneV
	orl	%ecx, %ecx
	jz	.done
	subl	$0x10000, %edi
	incb	%al
	jmp	.nextV
/* Two vertical lines */
.vvLine:
	movl	%edi, %eax
	shrl	$16, %eax
	movl	$0x3cd, %edx
	andl	$0x0ffff, %edi
	addl	vgaWriteBottom, %edi
.nextVV:
	OUT_B	(%dx)
	movl	vgaWriteTop, %esi
	addl	$0x3ff, %esi
	subl	%edi, %esi
	shrl	$10, %esi
	cmpl	%esi, %ecx
	jg	.moreVVs
	movl	%ecx, %esi
.moreVVs:
	subl	%esi, %ecx
.oneVV:
	movw	%bx, (%edi)
	addl	$0x400, %edi
	decl	%esi
	jnz	.oneVV
	orl	%ecx, %ecx
	jz	.done
	subl	$0x10000, %edi
	incb	%al
	jmp	.nextVV
	.text
	ALIGNTEXT4
/* THRESHOLD must be >= 18 */
.isPModeOK:
	cmpl	THRESHOLD, %eax
	jge	.pMode
	movl	%edi, %eax
	shrl	$16, %eax
	movb	%al, .writeSegment
	movl	$0x3cd, %edx
	OUT_B	(%dx)
	andl	$0x0ffff, %edi
	movl	$0x103ff, %esi
	subl	%edi, %esi	
	shrl	$10, %esi
	movl	16(%ebp), %ecx
	movl	20(%ebp), %edx
	addl	vgaWriteBottom, %edi
	movl	%edi, %ebx
	testl	$1, %ebx
	jz	.gFillA
	decl	%edx
	shrl	$1, %edx
	jc	.gFillUR
.gFillU:
	movl	12(%ebp), %eax
	cmpl	%esi, %ecx
	jg	.gFillU1
	movl	%ecx, %esi
.gFillU1:
	subl	%esi, %ecx
	pushl	%ecx
.gFillU2:
	movl	%ebx, %edi
	addl	$0x400, %ebx
	stosb
	movl	%edx, %ecx
	rep	
	stosw
	decl	%esi
	jnz	.gFillU2
	popl	%ecx
	orl	%ecx, %ecx
	jz	.done
	subl	$0x10000, %ebx
	movb	.writeSegment, %al
	incb	%al
	movb	%al, .writeSegment
	movl	%edx, %esi
	movl	$0x3cd, %edx
	OUT_B	(%dx)
	movl	%esi, %edx
	movl	$64, %esi
	jmp	.gFillU
	ALIGNTEXT4
.gFillUR:
	movl	12(%ebp), %eax
	cmpl	%esi, %ecx
	jg	.gFillUR1
	movl	%ecx, %esi
.gFillUR1:
	subl	%esi, %ecx
	pushl	%ecx
.gFillUR2:
	movl	%ebx, %edi
	addl	$0x400, %ebx
	stosb
	movl	%edx, %ecx
	rep	
	stosw
	movb	%al, (%edi)
	decl	%esi
	jnz	.gFillUR2
	popl	%ecx
	orl	%ecx, %ecx
	jz	.done
	subl	$0x10000, %ebx
	movb	.writeSegment, %al
	incb	%al	
	movb	%al, .writeSegment
	movl	%edx, %esi
	movl	$0x3cd, %edx
	OUT_B	(%dx)
	movl	%esi, %edx
	movl	$64, %esi
	jmp	.gFillUR
	ALIGNTEXT4
.gFillA:
	shrl	$1, %edx
	jc	.gFillAR
.gFillA3:
	movl	12(%ebp), %eax
	cmpl	%esi, %ecx
	jg	.gFillA1
	movl	%ecx, %esi
.gFillA1:
	subl	%esi, %ecx
	pushl	%ecx
.gFillA2:
	movl	%ebx, %edi
	addl	$0x400, %ebx
	movl	%edx, %ecx
	rep	
	stosw
	decl	%esi
	jnz	.gFillA2
	popl	%ecx
	orl	%ecx, %ecx
	jz	.done
	subl	$0x10000, %ebx
	movb	.writeSegment, %al
	incb	%al
	movb	%al, .writeSegment
	movl	%edx, %esi
	movl	$0x3cd, %edx
	OUT_B	(%dx)
	movl	%esi, %edx
	movl	$64, %esi
	jmp	.gFillA3
	ALIGNTEXT4
.gFillAR:
	movl	12(%ebp), %eax
	cmpl	%esi, %ecx
	jg	.gFillAR1
	movl	%ecx, %esi
.gFillAR1:
	subl	%esi, %ecx
	pushl	%ecx
.gFillAR2:
	movl	%ebx, %edi
	addl	$0x400, %ebx
	movl	%edx, %ecx
	rep	
	stosw
	movb	%al, (%edi)
	decl	%esi
	jnz	.gFillAR2
	popl	%ecx
	orl	%ecx, %ecx
	jz	.done
	subl	$0x10000, %ebx
	movb	.writeSegment, %al
	incb	%al
	movb	%al, .writeSegment
	movl	%edx, %esi
	movl	$0x3cd, %edx
	OUT_B	(%dx)
	movl	%esi, %edx
	movl	$64, %esi
	jmp	.gFillAR
	.data
	ALIGNDATA4
.middleCount:
	.long	0
.funcPtr:
	.long	0
.lMaskTable:
	.long	.lMaskTable00, .lMaskTable01, .lMaskTable10, .lMaskTable11
.pTable:
	.long	.pM, .pMR, .pLM, .pLMR
.lMaskTable00:
	.value	0x0002
	.value	0x0102
	.value	0x0302
	.value	0x0702
	.value	0x0f02
.lMaskTable01:
	.value	0x0002
	.value	0x0202
	.value	0x0602
	.value	0x0e02
.lMaskTable10:
	.value	0x0002
	.value	0x0402
	.value	0x0c02
.lMaskTable11:
	.value	0x0002
	.value	0x0802
.startMaskTable:
	.value	0x0002
	.value	0x0802
	.value	0x0c02
	.value	0x0e02
.endMaskTable:
	.value	0x0002
	.value	0x0102
	.value	0x0302
	.value	0x0702
.startMask:
	.value	0
.endMask:
	.value	0
.writeSegment:
	.byte	0
	.text
/* Assume video memory mapped to a long-word boundary */
	ALIGNTEXT4
.pMode:
	movl	%edi, %esi
	addl	$3, %esi
	andl	$0xfffffffc, %esi
	subl	%edi, %esi
	subl	%esi, %eax
	movl	%eax, %ecx
	andl	$3, %eax
	movl	$.pTable, %ebx
	sall	$1, %esi
	jz	.pNoL
	addl	$8, %ebx
.pNoL:
	movw	.startMaskTable(%esi), %dx
	movw	%dx, .startMask
	sarl	$2, %ecx
	movl	%ecx, .middleCount
	sall	$1, %eax
	jz	.pNoR
	addl	$4, %ebx
.pNoR:
	movl	(%ebx), %ebx
	movl	%ebx, .funcPtr
	movw	.endMaskTable(%eax), %dx
	movw	%dx, .endMask
	movl	%edi, %eax
	shrl	$18, %eax
	movb	%al, .writeSegment
	movl	$0x3cd, %edx
	OUT_B	(%dx)
	andl	$0x3ffff, %edi
	sarl	$2, %edi
	movl	$0x100ff, %esi
	subl	%edi, %esi
	sarl	$8, %esi
	addl	vgaWriteBottom, %edi
/***************************/
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
/***************************/
	movl	16(%ebp), %ecx
.pLoop:
	cmpl	%ecx, %esi
	jl	.pLoop1
	movl	%ecx, %esi
.pLoop1:
	subl	%esi, %ecx
	pushl	%ecx
	call	*.funcPtr
	popl	%ecx
	orl	%ecx, %ecx
	jz	.pDone
	subl	$0x10000, %edi
	movb	.writeSegment, %al
	incb	%al
	movb	%al, .writeSegment
	movl	$0x3cd, %edx
	OUT_B	(%dx)
	movl	$256, %esi
	jmp	.pLoop
	ALIGNTEXT4
.pLMR:
	movw	.endMask, %ax
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	12(%ebp), %eax
	movl	%esi, %ebx
	movl	%edi, %edx
	addl	.middleCount, %edx
	incl	%edx
.pLMR1:
	movb	%al, (%edx)
	addl	$0x100, %edx
	decl	%ebx
	jnz	.pLMR1
.pLM:
	movw	.startMask, %ax
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	12(%ebp), %eax
	movl	%esi, %ebx
	movl	%edi, %ecx
.pLM1:
	movb	%al, (%ecx)
	addl	$0x100, %ecx
	decl	%ebx
	jnz	.pLM1
	pushl	%ecx
	movw	$0x0f02, %ax
	OUT_W	(%dx)
	movl	12(%ebp), %eax
	movl	.middleCount, %ebx
	movl	%edi, %edx
	incl	%edx
	testl	$1, %edx
	jnz	.pLU
	shrl	$1, %ebx
	jc	.pLAA
.pLA:
	movl	%edx, %edi
	movl	%ebx, %ecx
	rep
	stosw
	addl	$0x100, %edx
	decl	%esi
	jnz	.pLA
	popl	%edi
	ret
/****************************/
	ALIGNTEXT4
.pLAA:
	movl	%edx, %edi
	movl	%ebx, %ecx
	rep
	stosw
	movb	%al, (%edi)
	addl	$0x100, %edx
	decl	%esi
	jnz	.pLAA
	popl	%edi
	ret
/****************************/
	ALIGNTEXT4
.pLU:
	decl	%ebx
	shrl	$1, %ebx
	jc	.pLUU
.pLU1:
	movl	%edx, %edi
	stosb
	movl	%ebx, %ecx
	rep
	stosw
	addl	$0x100, %edx
	decl	%esi
	jnz	.pLU1
	popl	%edi
	ret
/****************************/
	ALIGNTEXT4
.pLUU:
	movl	%edx, %edi
	stosb
	movl	%ebx, %ecx
	rep
	stosw
	movb	%al, (%edi)
	addl	$0x100, %edx
	decl	%esi
	jnz	.pLUU
	popl	%edi
	ret
/* %esi: height; %edi: destination */
	ALIGNTEXT4
.pMR:
	movw	.endMask, %ax
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	12(%ebp), %eax
	movl	%esi, %ebx
	movl	%edi, %edx
	addl	.middleCount, %edx
.pMR1:
	movb	%al, (%edx)
	addl	$0x100, %edx
	decl	%ebx
	jnz	.pMR1
.pM:
	movw	$0x0f02, %ax
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	12(%ebp), %eax
	movl	.middleCount, %ebx
	movl	%edi, %edx
	testl	$1, %edx
	jnz	.pU
	shrl	$1, %ebx
	jc	.pAA
.pA:
	movl	%ebx, %ecx
	rep
	stosw
	addl	$0x100, %edx
	movl	%edx, %edi
	decl	%esi
	jnz	.pA
	ret
/*****************************/
	ALIGNTEXT4
.pAA:
	movl	%ebx, %ecx
	rep
	stosw
	movb	%al, (%edi)
	addl	$0x100, %edx
	movl	%edx, %edi
	decl	%esi
	jnz	.pAA
	ret
/******************************/
	ALIGNTEXT4
.pU:
	decl	%ebx
	shrl	$1, %ebx
	jc	.pUU
.pU1:
	stosb
	movl	%ebx, %ecx
	rep
	stosw
	addl	$0x100, %edx
	movl	%edx, %edi
	decl	%esi
	jnz	.pU1
	ret
/******************************/
	ALIGNTEXT4
.pUU:
	stosb
	movl	%ebx, %ecx
	rep
	stosw
	movb	%al, (%edi)
	addl	$0x100, %edx
	movl	%edx, %edi
	decl	%esi
	jnz	.pUU
	ret
	ALIGNTEXT4
.pDone:
	movl	$0x0c04, %eax
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	$0x0f02, %eax
	OUT_W	(%dx)
.done:
	popl 	%ebx
	popl 	%esi
	popl 	%edi
	leave
.return:
	ret

