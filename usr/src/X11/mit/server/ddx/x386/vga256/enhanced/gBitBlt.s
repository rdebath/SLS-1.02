/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/gBitBlt.s,v 1.15 1992/08/29 11:09:56 dawes Exp $ */
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
5/2/92
*******************************************************************************/

#include "assembler.h"

#include "vgaAsm.h"

	.file "gBitBlt.s"

#if !defined(SYSV) && !defined(SVR4)
#define WinWin _WinWin
#define WinPix _WinPix
#define PixWin _PixWin
#define PixPix _PixPix
#endif

	.data
.copyright:
	STRING	"Copyright 1992 by Glenn G. Lai"
	ALIGNDATA4
.dstLeft:
	.long	0
.dstMiddle:
	.long	0
.dstRight:
	.long	0
.srcLeft:
	.long	0
.srcMiddle:
	.long	0
.srcRight:
	.long	0
.leftCount:
	.long	0
.middleCount:
	.long	0
.rightCount:
	.long	0
.heightCount:
	.long	0
.heightLeft:
	.long	0
.winWinLRTable:
	.long	.wBoxLR, .wbBoxLR, .bwBoxLR, .bBoxLR
.winWinRLTable:
	.long	.bBoxRL, .bwBoxRL, .wbBoxRL, .wBoxRL
.shortWinWinTable:
	.long	0, .shortWW1, .shortWW2
	.long	.shortWW3, .shortWW4, .shortWW5
.winWinPLRTable:
	.long	.p10, .p11
.winWinPRLTable:
	.long	.p00, .p01
.shortPixPixTable:
	.long	0, .shortPixPix1, .shortPixPix2
	.long	.shortPixPix3, .shortPixPix4
.funcPtr:
	.long	0
.winWinSegment:
	.long	0
.leftMaskTable:
	.value	0x0f02, 0x0802, 0x0c02, 0x0e02
.rightMaskTable:
	.value	0x0f02, 0x0102, 0x0302, 0x0702
.leftMask:
	.value	0
.rightMask:
	.value	0
.winWinRead:
	.byte	0
.winWinWrite:
	.byte	0
.segment:
	.byte	0
	.text
	ALIGNTEXT4
/*  WinWin(src, dst, h, w, offset, ydir, xdir, hack) */
	.globl	WinWin
/* THRESHOLD must be >= 10 */
#define	THRESHOLD $10
WinWin:
	movl	4(%esp), %eax
	cmpl	8(%esp), %eax
	je	.return
	movl	16(%esp), %eax
	orl	20(%esp), %eax
	jz	.return
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	movl	8(%ebp), %esi
	movl	12(%ebp), %edi
/********************************/
	subl	VGABASE, %esi
	subl	VGABASE, %edi
	movl	%esi, %eax
	movl	%edi, %ebx
	andl	$3, %eax
	andl	$3, %ebx
	cmpl	%eax, %ebx
	jne	.winWinNotP
	cmpl	THRESHOLD, 20(%ebp)
	jge	.winWinP
.winWinNotP:
	movl	%esi, %eax
	sarl	$12, %eax
	andb	$0x0f0, %al
	movl	%edi, %ebx
	sarl	$16, %ebx
	orb	%bl, %al
	movl	$0x3cd, %edx
	OUT_B	(%dx)
	movb	%al, .segment
/********************************/
	andl	$0x0ffff, %esi
	andl	$0x0ffff, %edi
	cmpl	$1, 28(%ebp)
	jne	.winWinUp
/********************************/
	cld
	movl	$0xffff0000, .winWinSegment
	movb	$0x10, .winWinRead
	movb	$1, .winWinWrite
	movl	$.winWinLRTable, %ebx
	movl	%ebx, .funcPtr
	movl	$0x103ff, %ebx
	subl	%esi, %ebx
	sarl	$10, %ebx
	movl	$0x103ff, %ecx
	subl	%edi, %ecx
	sarl	$10, %ecx
	jmp	.winWin1
/********************************/
	ALIGNTEXT4
.winWinUp:
	std
	movl	$0x10000, .winWinSegment
	movb	$0xf0, .winWinRead
	movb	$0xff, .winWinWrite
	movl	$.winWinRLTable, %ebx
	movl	%ebx, .funcPtr
	movl	%esi, %ebx
	movl	%edi, %ecx
	sarl	$10, %ebx
	sarl	$10, %ecx
	incl	%ebx
	incl	%ecx
.winWin1:
	addl	vgaReadBottom, %esi
	addl	vgaWriteBottom, %edi
	movl	%ebx, %edx
	cmpl	%ecx, %edx
	jl	.winWin2
	movl	%ecx, %edx
.winWin2:
	movl	16(%ebp), %eax
	cmpl	%edx, %eax
	jg	.winWin3
	movl	%eax, %edx
.winWin3:
	subl	%edx, %eax
	movl	%eax, .heightLeft
	subl	%edx, %ebx
	subl	%edx, %ecx
/********************************/
	movl	20(%ebp), %eax
	cmpl	$5, %eax
	jg	.longWinWin
	cld
	sall	$2, %eax
	movl	.shortWinWinTable(%eax), %eax
	movl	%eax, .funcPtr
	jmp	.winWinLoop
.longWinWin:
	movl	.funcPtr, %eax
	testl	$1, %esi
	jz	.longWinWin1
	addl	$8, %eax
.longWinWin1:
	testl	$1, %edi
	jz	.longWinWin2
	addl	$4, %eax
.longWinWin2:
	movl	(%eax), %eax
	movl	%eax, .funcPtr
/********************************/
.winWinLoop:
	pushl	%ebx
	pushl	%ecx
	movl	24(%ebp), %ebx
	movl	20(%ebp), %ecx
	call	*.funcPtr
	popl	%ecx
	popl	%ebx
	cmpl	$0, .heightLeft
	je	.done
	movb	.segment, %al
	orl	%ebx, %ebx
	jnz	.winWin4
	addl	.winWinSegment, %esi
	addb	.winWinRead, %al
	movl	%ecx, %edx
	movl	$64, %ebx
.winWin4:
	orl	%ecx, %ecx
	jnz	.winWin5
	addl	.winWinSegment, %edi
	addb	.winWinWrite, %al
	movl	%ebx, %edx
	movl	$64, %ecx
.winWin5:
	orl	%edx, %edx
	jnz	.winWin6
	movl	$64, %edx
.winWin6:
	cmpl	%edx, .heightLeft
	jg	.winWin7
	movl	.heightLeft, %edx
.winWin7:
	subl	%edx, .heightLeft
	subl	%edx, %ebx
	subl	%edx, %ecx
	pushl	%edx
	movb	%al, .segment
	movl	$0x3cd, %edx
	OUT_B	(%dx)
	popl	%edx
	jmp	.winWinLoop
/********************************/
	ALIGNTEXT4
.shortWW5:
	testl	$1, %esi
	jnz	.shortWW5U
	subl	$4, %ebx
.shortWW51:
	movb	4(%esi), %al
	movsl
	movb	%al, (%edi)
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.shortWW51
	ret
/********************************/
	ALIGNTEXT4
.shortWW5U:
	decl	%ebx
.shortWW5U1:
	movl	1(%esi), %eax
	movsb
	movl	%eax, (%edi)
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.shortWW5U1
	ret
/********************************/
	ALIGNTEXT4
.shortWW4:
	movl	(%esi), %eax
	movl	%eax, (%edi)
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.shortWW4
	ret
/********************************/
	ALIGNTEXT4
.shortWW3:
	testl	$1, %esi
	jnz	.shortWW3U
	subl	$2, %ebx
.shortWW31:
	movb	2(%esi), %al
	movsw
	movb	%al, (%edi)
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.shortWW31
	ret
/********************************/
	ALIGNTEXT4
.shortWW3U:
	decl	%ebx
.shortWW3U1:
	movw	1(%esi), %ax
	movsb
	movw	%ax, (%edi)
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.shortWW3U1
	ret
/********************************/
	ALIGNTEXT4
.shortWW2:
	movw	(%esi), %ax
	movw	%ax, (%edi)
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.shortWW2
	ret
/***************************/
	ALIGNTEXT4
.shortWW1:
	movb	(%esi), %al
	movb	%al, (%edi)
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.shortWW1
	ret
/***************************/
	ALIGNTEXT4
.bBoxLR:
	decl	%ecx
	movl	%ecx, %eax
	sarl	$2, %eax
	andl	$3, %ecx
	jnz	.bBoxLRR
.bBoxLR1:
	movsb
	movl	%eax, %ecx
	rep
	movsl
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.bBoxLR1
	ret
/***************************/
	ALIGNTEXT4
.bBoxLRR:
	movl	%ecx, .rightCount
.bBoxLRR1:
	movsb
	movl	%eax, %ecx
	rep
	movsl
	movl	.rightCount, %ecx
	rep
	movsb
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.bBoxLRR1
	ret
/***************************/
	ALIGNTEXT4
.wBoxLR:
	movl	%ecx, %eax
	shrl	$2, %eax
	andl	$3, %ecx
	jnz	.wBoxLRR
.wBoxLR1:
	movl	%eax, %ecx
	rep
	movsl
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.wBoxLR1
	ret
/***************************/
	ALIGNTEXT4
.wBoxLRR:
	movl	%ecx, .rightCount
.wBoxLRR1:
	movl	%eax, %ecx
	rep
	movsl
	movl	.rightCount, %ecx
	rep
	movsb
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.wBoxLRR1
	ret
/***************************/
	ALIGNTEXT4
.bwBoxLR:
	movl	%edx, .heightCount
	decl	%ecx
	movl	%ecx, %eax
	sarl	$2, %ecx
	movl	%ecx, .middleCount
	andl	$3, %eax
	jnz	.bwBoxLRR
.bwBoxLR1:
	movb	(%esi), %dl
	incl	%esi
	movl	.middleCount, %ecx
.bwBoxLR2:
	lodsl
	roll	$8, %eax
	xchgb	%al, %dl
	stosl
	decl	%ecx
	jnz	.bwBoxLR2
	movb	%dl, (%edi)
	incl	%edi
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	.heightCount
	jnz	.bwBoxLR1
	ret
/***************************/
	ALIGNTEXT4
.bwBoxLRR:
	movl	%eax, .rightCount
.bwBoxLRR1:
	movb	(%esi), %dl
	incl	%esi
	movl	.middleCount, %ecx
.bwBoxLRR2:
	lodsl
	roll	$8, %eax
	xchgb	%al, %dl
	stosl
	decl	%ecx
	jnz	.bwBoxLRR2
	movb	%dl, (%edi)
	incl	%edi
	movl	.rightCount, %ecx
	rep
	movsb
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	.heightCount
	jnz	.bwBoxLRR1
	ret
/***************************/
	ALIGNTEXT4
.wbBoxLR:
	movl	%edx, .heightCount
	subl	$2, %ecx
	movl	%ecx, %eax
	sarl	$2, %ecx
	movl	%ecx, .middleCount
	andl	$3, %eax
	jnz	.wbBoxLRR
.wbBoxLR2:
	lodsw
	stosb
	movb	%ah, %dl
	movl	.middleCount, %ecx
.wbBoxLR1:
	lodsl
	roll	$8, %eax
	xchgb	%al, %dl
	stosl
	decl	%ecx
	jnz	.wbBoxLR1
	movb	%dl, (%edi)
	incl	%edi
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	.heightCount
	jnz	.wbBoxLR2
	ret
/***************************/
	ALIGNTEXT4
.wbBoxLRR:
	movl	%eax, .rightCount
.wbBoxLRR1:
	lodsw
	stosb
	movb	%ah, %dl
	movl	.middleCount, %ecx
.wbBoxLRR2:
	lodsl
	roll	$8, %eax
	xchgb	%al, %dl
	stosl
	decl	%ecx
	jnz	.wbBoxLRR2
	movb	%dl, (%edi)
	incl	%edi
	movl	.rightCount, %ecx
	rep
	movsb
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	.heightCount
	jnz	.wbBoxLRR1
	ret
/***************************/
	ALIGNTEXT4
.bBoxRL:
	decl	%ecx
	movl	%ecx, %eax
	sarl	$2, %eax
	andl	$3, %ecx
	jnz	.bBoxRLL
	addl	$3, %ebx
.bBoxRL1:
	movsb
	subl	$3, %esi
	subl	$3, %edi
	movl	%eax, %ecx
	rep
	movsl
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.bBoxRL1
	ret
/***************************/
	ALIGNTEXT4
.bBoxRLL:
	movl	%ecx, .leftCount
.bBoxRLL1:
	movsb
	subl	$3, %esi
	subl	$3, %edi
	movl	%eax, %ecx
	rep
	movsl
	addl	$3, %esi
	addl	$3, %edi
	movl	.leftCount, %ecx
	rep
	movsb
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.bBoxRLL1
	ret
/***************************/
	ALIGNTEXT4
.wBoxRL:
	movl	%ecx, %eax
	sarl	$2, %eax
	andl	$3, %ecx
	jnz	.wBoxRLL
	addl	$3, %ebx
.wBoxRL1:
	subl	$3, %esi
	subl	$3, %edi
	movl	%eax, %ecx
	rep
	movsl
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.wBoxRL1
	ret
/***************************/
	ALIGNTEXT4
.wBoxRLL:
	movl	%ecx, .leftCount
.wBoxRLL1:
	subl	$3, %esi
	subl	$3, %edi
	movl	%eax, %ecx
	rep
	movsl
	addl	$3, %esi
	addl	$3, %edi
	movl	.leftCount, %ecx
	rep
	movsb
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.wBoxRLL1
	ret
/***************************/
	ALIGNTEXT4
.bwBoxRL:
	movl	%edx, .heightCount
	decl	%ecx
	movl	%ecx, %eax
	sarl	$2, %ecx
	movl	%ecx, .middleCount
	andl	$3, %eax
	jnz	.bwBoxRLL
	addl	$3, %ebx
.bwBoxRL1:
	movb	(%esi), %dl
	subl	$4, %esi
	subl	$3, %edi
	movl	.middleCount, %ecx
.bwBoxRL2:
	lodsl
	xchgb	%al, %dl
	rorl	$8, %eax
	stosl
	decl	%ecx
	jnz	.bwBoxRL2
	movb	%dl, 3(%edi)
	decl	%edi
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	.heightCount
	jnz	.bwBoxRL1
	ret
/***************************/
	ALIGNTEXT4
.bwBoxRLL:
	movl	%eax, .leftCount
.bwBoxRLL1:
	movb	(%esi), %dl
	subl	$4, %esi
	subl	$3, %edi
	movl	.middleCount, %ecx
.bwBoxRLL2:
	lodsl
	xchgb	%al, %dl
	rorl	$8, %eax
	stosl
	decl	%ecx
	jnz	.bwBoxRLL2
	movb	%dl, 3(%edi)
	addl	$3, %esi
	addl	$2, %edi
	movl	.leftCount, %ecx
	rep
	movsb
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	.heightCount
	jnz	.bwBoxRLL1
	ret
/***************************/
	ALIGNTEXT4
.wbBoxRL:
	movl	%edx, .heightCount
	subl	$2, %ecx
	movl	%ecx, %eax
	sarl	$2, %ecx
	movl	%ecx, .middleCount
	andl	$3, %eax
	jnz	.wbBoxRLL
	addl	$3, %ebx
.wbBoxRL1:
	decl	%esi
	movw	(%esi), %ax
	movb	%ah, (%edi)
	movb	%al, %dl
	subl	$4, %esi
	subl	$4, %edi
	movl	.middleCount, %ecx
.wbBoxRL2:
	lodsl
	xchgb	%al, %dl
	rorl	$8, %eax
	stosl
	decl	%ecx
	jnz	.wbBoxRL2
	movb	%dl, 3(%edi)
	decl	%edi
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	.heightCount
	jnz	.wbBoxRL1
	ret
/***************************/
	ALIGNTEXT4
.wbBoxRLL:
	movl	%eax, .leftCount
.wbBoxRLL1:
	decl	%esi
	movw	(%esi), %ax
	movb	%ah, (%edi)
	movb	%al, %dl
	subl	$4, %esi
	subl	$4, %edi
	movl	.middleCount, %ecx
.wbBoxRLL2:
	lodsl
	xchgb	%al, %dl
	rorl	$8, %eax
	stosl
	decl	%ecx
	jnz	.wbBoxRLL2
	movb	%dl, 3(%edi)
	addl	$3, %esi
	addl	$2, %edi
	movl	.leftCount, %ecx
	rep
	movsb
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	.heightCount
	jnz	.wbBoxRLL1
	ret
/***************************/
	ALIGNTEXT4
.winWinP:
	movl	$0x0604, %eax
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	$0x4105, %eax
	movl	$0x3ce, %edx
	OUT_W	(%dx)
/***************************/
	movl	%esi, %eax
	sarl	$18, %eax
	salb	$4, %al
	movl	%edi, %ebx
	sarl	$18, %ebx
	orb	%bl, %al
	movb	%al, .segment
	andl	$0x03ffff, %esi
	andl	$0x03ffff, %edi
	movl	20(%ebp), %ecx
	cld
	movl	$.winWinPLRTable, .funcPtr
	cmpl	$1, 32(%ebp)
	je	.winWinPLR
	movl	$.winWinPRLTable, .funcPtr
.winWinPLR:
	cmpl	$1, 28(%ebp)
	je	.winWinPDown
.winWinPUp:
	std
	movl	.funcPtr, %eax
	movl	(%eax), %eax
	movl	%eax, .funcPtr
	movl	%esi, %eax
	sarl	$2, %eax
	addl	vgaReadBottom, %eax
	movl	%eax, .srcRight
	movl	%edi, %eax
	sarl	$2, %eax
	addl	vgaWriteBottom, %eax
	movl	%eax, .dstRight
	movl	%esi, %eax
	incl	%eax
	andl	$3, %eax
	subl	%eax, %ecx
	sall	$1, %eax
	movw	.rightMaskTable(%eax), %ax
	movw	%ax, .rightMask
	movl	%ecx, %eax
	sarl	$2, %ecx
	movl	%ecx, .middleCount
	andl	$3, %eax
	sall	$1, %eax
	movw	.leftMaskTable(%eax), %ax
	movw	%ax, .leftMask
	movl	%esi, %eax
	subl	$3, %eax
	sarl	$2, %eax
	addl	vgaReadBottom, %eax
	movl	%eax, .srcMiddle
	subl	%ecx, %eax
	movl	%eax, .srcLeft
	movl	%edi, %eax
	subl	$3, %eax
	sarl	$2, %eax
	addl	vgaWriteBottom, %eax
	movl	%eax, .dstMiddle
	subl	%ecx, %eax
	movl	%eax, .dstLeft
/***************************/
	cmpl	$1, 36(%ebp)
	je	.hack
	cld
	movl	.srcLeft, %eax
	incl	%eax
	movl	%eax, .srcMiddle
	movl	.dstLeft, %eax
	incl	%eax
	movl	%eax, .dstMiddle
.hack:
	sarl	$2, %esi
	sarl	$2, %edi
	movl	%esi, %ebx
	movl	%edi, %ecx
	sarl	$8, %ebx
	sarl	$8, %ecx
	incl	%ebx
	incl	%ecx
	jmp	.winWinP1
/***************************/
	ALIGNTEXT4
.winWinPDown:
	movl	.funcPtr, %eax
	movl	4(%eax), %eax
	movl	%eax, .funcPtr
	movl	%esi, %eax
	sarl	$2, %eax
	addl	vgaReadBottom, %eax
	movl	%eax, .srcLeft
	movl	%edi, %eax
	sarl	$2, %eax
	addl	vgaWriteBottom, %eax
	movl	%eax, .dstLeft
	movl	%esi, %eax
	addl	$3, %eax
	andl	$0xfffffffc, %eax
	subl	%esi, %eax
	subl	%eax, %ecx
	sall	$1, %eax
	movw	.leftMaskTable(%eax), %ax
	movw	%ax, .leftMask
	movl	%ecx, %eax
	sarl	$2, %ecx
	movl	%ecx, .middleCount
	andl	$3, %eax
	sall	$1, %eax
	movw	.rightMaskTable(%eax), %ax
	movw	%ax, .rightMask
	movl	%esi, %eax
	addl	$3, %eax
	sarl	$2, %eax
	addl	vgaReadBottom, %eax
	movl	%eax, .srcMiddle
	addl	%ecx, %eax
	movl	%eax, .srcRight
	movl	%edi, %eax
	addl	$3, %eax
	sarl	$2, %eax
	addl	vgaWriteBottom, %eax
	movl	%eax, .dstMiddle
	addl	%ecx, %eax
	movl	%eax, .dstRight
/********************************/
	sarl	$2, %esi
	sarl	$2, %edi
	movl	$0x100ff, %ebx
	subl	%esi, %ebx
	sarl	$8, %ebx
	movl	$0x100ff, %ecx
	subl	%edi, %ecx
	sarl	$8, %ecx
.winWinP1:
	movl	%ebx, %edx
	cmpl	%ecx, %edx
	jl	.winWinP2
	movl	%ecx, %edx
.winWinP2:
	movl	16(%ebp), %eax
.winWinPLoop:
	cmpl	%edx, %eax
	jg	.winWinP3
	movl	%eax, %edx
.winWinP3:
	subl	%edx, %eax
	subl	%edx, %ebx
	subl	%edx, %ecx
	movl	%eax, .heightLeft
	movl	%edx, .heightCount
	movb	.segment, %al
	movl	$0x3cd, %edx
	OUT_B	(%dx)
	pushl	%ebx
	pushl	%ecx
	call	*.funcPtr
	popl	%ecx
	popl	%ebx
	movl	.heightLeft, %eax
	orl	%eax, %eax
	jz	.pDone
	orl	%ebx, %ebx
	jnz	.winWinP4
	movl	%ecx, %edx
	movl	$256, %ebx
.winWinP4:
	orl	%ecx, %ecx
	jnz	.winWinP5
	movl	%ebx, %edx
	movl	$256, %ecx
.winWinP5:
	orl	%edx, %edx
	jnz	.winWinPLoop
	movl	$256, %edx
	jmp	.winWinPLoop
/***************************/
	ALIGNTEXT4
.p00:
	movw	.rightMask, %ax
	cmpw	$0x0f02, %ax
	je	.p00a
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	.srcRight, %esi
	movl	.dstRight, %edi
	movl	.heightCount, %edx
.p00b:
	movb	(%esi), %al
	movb	%al, (%edi)
	subl	$0x100, %esi
	subl	$0x100, %edi
	decl	%edx
	jnz	.p00b
	cmpl	vgaReadBottom, %esi
	jnc	.p00c
	addl	$0x10000, %esi
.p00c:
	cmpl	vgaWriteBottom, %edi
	jnc	.p00d
	addl	$0x10000, %edi
.p00d:
	movl	%esi, .srcRight
	movl	%edi, .dstRight
	movw	$0x0f02, %ax
.p00a:
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	.srcMiddle, %eax
	movl	.dstMiddle, %ebx
	movl	.heightCount, %edx
.p00e:
	movl	%eax, %esi
	movl	%ebx, %edi
	movl	.middleCount, %ecx
	rep
	movsb
	subl	$0x100, %eax
	subl	$0x100, %ebx
	decl	%edx
	jnz	.p00e
	movb	.segment, %cl
	cmpl	vgaReadBottom, %eax
	jnc	.p00f
	subb	$16, %cl
	addl	$0x10000, %eax
.p00f:
	cmpl	vgaWriteBottom, %ebx
	jnc	.p00g
	decb	%cl
	addl	$0x10000, %ebx
.p00g:
	movl	%eax, .srcMiddle
	movl	%ebx, .dstMiddle
	movb	%cl, .segment
	movw	.leftMask, %ax
	cmpw	$0x0f02, %ax
	je	.p00h
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	.srcLeft, %esi
	movl	.dstLeft, %edi
	movl	.heightCount, %edx
.p00i:
	movb	(%esi), %al
	movb	%al, (%edi)
	subl	$0x100, %esi
	subl	$0x100, %edi
	decl	%edx
	jnz	.p00i
	cmpl	vgaReadBottom, %esi
	jnc	.p00j
	addl	$0x10000, %esi
.p00j:
	cmpl	vgaWriteBottom, %edi
	jnc	.p00k
	addl	$0x10000, %edi
.p00k:
	movl	%esi, .srcLeft
	movl	%edi, .dstLeft
.p00h:
	ret
/***************************/
	ALIGNTEXT4
.p01:
	movw	.rightMask, %ax
	cmpw	$0x0f02, %ax
	je	.p01a
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	.srcRight, %esi
	movl	.dstRight, %edi
	movl	.heightCount, %edx
.p01b:
	movb	(%esi), %al
	movb	%al, (%edi)
	addl	$0x100, %esi
	addl	$0x100, %edi
	decl	%edx
	jnz	.p01b
	cmpl	vgaReadTop, %esi
	jc	.p01c
	subl	$0x10000, %esi
.p01c:
	cmpl	vgaWriteTop, %edi
	jc	.p01d
	subl	$0x10000, %edi
.p01d:
	movl	%esi, .srcRight
	movl	%edi, .dstRight
	movw	$0x0f02, %ax
.p01a:
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	.srcMiddle, %eax
	movl	.dstMiddle, %ebx
	movl	.heightCount, %edx
.p01e:
	movl	%eax, %esi
	movl	%ebx, %edi
	movl	.middleCount, %ecx
	rep
	movsb
	addl	$0x100, %eax
	addl	$0x100, %ebx
	decl	%edx
	jnz	.p01e
	movb	.segment, %cl
	cmpl	vgaReadTop, %eax
	jc	.p01f
	addb	$16, %cl
	subl	$0x10000, %eax
.p01f:
	cmpl	vgaWriteTop, %ebx
	jc	.p01g
	incb	%cl
	subl	$0x10000, %ebx
.p01g:
	movl	%eax, .srcMiddle
	movl	%ebx, .dstMiddle
	movb	%cl, .segment
	movw	.leftMask, %ax
	cmpw	$0x0f02, %ax
	je	.p01h
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	.srcLeft, %esi
	movl	.dstLeft, %edi
	movl	.heightCount, %edx
.p01i:
	movb	(%esi), %al
	movb	%al, (%edi)
	addl	$0x100, %esi
	addl	$0x100, %edi
	decl	%edx
	jnz	.p01i
	cmpl	vgaReadTop, %esi
	jc	.p01j
	subl	$0x10000, %esi
.p01j:
	cmpl	vgaWriteTop, %edi
	jc	.p01k
	subl	$0x10000, %edi
.p01k:
	movl	%esi, .srcLeft
	movl	%edi, .dstLeft
.p01h:
	ret
/***************************/
	ALIGNTEXT4
.p10:
	movw	.leftMask, %ax
	cmpw	$0x0f02, %ax
	je	.p10a
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	.srcLeft, %esi
	movl	.dstLeft, %edi
	movl	.heightCount, %edx
.p10b:
	movb	(%esi), %al
	movb	%al, (%edi)
	subl	$0x100, %esi
	subl	$0x100, %edi
	decl	%edx
	jnz	.p10b
	cmpl	vgaReadBottom, %esi
	jnc	.p10c
	addl	$0x10000, %esi
.p10c:
	cmpl	vgaWriteBottom, %edi
	jnc	.p10d
	addl	$0x10000, %edi
.p10d:
	movl	%esi, .srcLeft
	movl	%edi, .dstLeft
	movw	$0x0f02, %ax
.p10a:
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	.srcMiddle, %eax
	movl	.dstMiddle, %ebx
	movl	.heightCount, %edx
.p10e:
	movl	%eax, %esi
	movl	%ebx, %edi
	movl	.middleCount, %ecx
	rep
	movsb
	subl	$0x100, %eax
	subl	$0x100, %ebx
	decl	%edx
	jnz	.p10e
	movb	.segment, %cl
	cmpl	vgaReadBottom, %eax
	jnc	.p10f
	subb	$16, %cl
	addl	$0x10000, %eax
.p10f:
	cmpl	vgaWriteBottom, %ebx
	jnc	.p10g
	decb	%cl
	addl	$0x10000, %ebx
.p10g:
	movl	%eax, .srcMiddle
	movl	%ebx, .dstMiddle
	movb	%cl, .segment
	movw	.rightMask, %ax
	cmpw	$0x0f02, %ax
	je	.p10h
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	.srcRight, %esi
	movl	.dstRight, %edi
	movl	.heightCount, %edx
.p10i:
	movb	(%esi), %al
	movb	%al, (%edi)
	subl	$0x100, %esi
	subl	$0x100, %edi
	decl	%edx
	jnz	.p10i
	cmpl	vgaReadBottom, %esi
	jnc	.p10j
	addl	$0x10000, %esi
.p10j:
	cmpl	vgaWriteBottom, %edi
	jnc	.p10k
	addl	$0x10000, %edi
.p10k:
	movl	%esi, .srcRight
	movl	%edi, .dstRight
.p10h:
	ret
/***************************/
	ALIGNTEXT4
.p11:
	movw	.leftMask, %ax
	cmpw	$0x0f02, %ax
	je	.p11a
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	.srcLeft, %esi
	movl	.dstLeft, %edi
	movl	.heightCount, %edx
.p11b:
	movb	(%esi), %al
	movb	%al, (%edi)
	addl	$0x100, %esi
	addl	$0x100, %edi
	decl	%edx
	jnz	.p11b
	cmpl	vgaReadTop, %esi
	jc	.p11c
	subl	$0x10000, %esi
.p11c:
	cmpl	vgaWriteTop, %edi
	jc	.p11d
	subl	$0x10000, %edi
.p11d:
	movl	%esi, .srcLeft
	movl	%edi, .dstLeft
	movw	$0x0f02, %ax
.p11a:
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	.srcMiddle, %eax
	movl	.dstMiddle, %ebx
	movl	.heightCount, %edx
.p11e:
	movl	%eax, %esi
	movl	%ebx, %edi
	movl	.middleCount, %ecx
	rep
	movsb
	addl	$0x100, %eax
	addl	$0x100, %ebx
	decl	%edx
	jnz	.p11e
	movb	.segment, %cl
	cmpl	vgaReadTop, %eax
	jc	.p11f
	addb	$16, %cl
	subl	$0x10000, %eax
.p11f:
	cmpl	vgaWriteTop, %ebx
	jc	.p11g
	incb	%cl
	subl	$0x10000, %ebx
.p11g:
	movl	%eax, .srcMiddle
	movl	%ebx, .dstMiddle
	movb	%cl, .segment
	movw	.rightMask, %ax
	cmpw	$0x0f02, %ax
	je	.p11h
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	.srcRight, %esi
	movl	.dstRight, %edi
	movl	.heightCount, %edx
.p11i:
	movb	(%esi), %al
	movb	%al, (%edi)
	addl	$0x100, %esi
	addl	$0x100, %edi
	decl	%edx
	jnz	.p11i
	cmpl	vgaReadTop, %esi
	jc	.p11j
	subl	$0x10000, %esi
.p11j:
	cmpl	vgaWriteTop, %edi
	jc	.p11k
	subl	$0x10000, %edi
.p11k:
	movl	%esi, .srcRight
	movl	%edi, .dstRight
.p11h:
	ret
/*  void PixWin(src, dst, h, w, srcOffset, dstOffset) */
	.data
	ALIGNDATA4
.shortPixWinTable:
	.long	0, .shortPixWin1, .shortPixWin2
	.long	.shortPixWin3, .shortPixWin4
/********************************/
	.text
	ALIGNTEXT4
	.globl	PixWin
PixWin:
	movl	12(%esp), %eax
	orl	16(%esp), %eax
	jz	.return
	cld
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	movl	28(%ebp), %ebx
	movl	20(%ebp), %ecx
	movl	8(%ebp), %esi	
	movl	12(%ebp), %edi
	subl	VGABASE, %edi
	movl	%edi, %eax
	andl	$0x0ffff, %edi
	sarl	$16, %eax
	movb	%al, .segment
	movl	$0x3cd, %edx
	OUT_B	(%dx)
	movl	$0x103ff, %edx
	subl	%edi, %edx
	addl	vgaWriteBottom, %edi
	sarl	$10, %edx
	cmpl	$4, %ecx
	jle	.shortPixWin
	testl	$1, %edi
	jnz	.pixUWin
.pixWin:
	movl	%ecx, %eax
	andl	$3, %eax
	movl	%eax, .rightCount
	sarl	$2, %ecx
	movl	%ecx, .middleCount
	movl	16(%ebp), %ecx
.pixWinLoop:
	cmpl	%edx, %ecx
	jg	.pixWinMore
	movl	%ecx, %edx
.pixWinMore:
	subl	%edx, %ecx
	pushl	%ecx
	movl	24(%ebp), %eax
.pixWin2:
	movl	.middleCount, %ecx
	rep
	movsl
	movl	.rightCount, %ecx
	orl	%ecx, %ecx
	jz	.pixWin1
	rep
	movsb
.pixWin1:
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.pixWin2
	popl	%ecx
	orl	%ecx, %ecx
	jz	.pixWinDone
	subl	$0x10000, %edi
	movb	.segment, %al
	incb	%al
	movb	%al, .segment
	movl	$0x3cd, %edx
	OUT_B	(%dx)	
	movl	$64, %edx
	jmp	.pixWinLoop
/********************************/
	ALIGNTEXT4
.pixUWin:
	decl	%ecx
	movl	%ecx, %eax
	andl	$3, %eax
	movl	%eax, .rightCount
	sarl	$2, %ecx
	movl	%ecx, .middleCount
	movl	16(%ebp), %ecx
.pixUWinLoop:
	cmpl	%edx, %ecx
	jg	.pixUWinMore
	movl	%ecx, %edx
.pixUWinMore:
	subl	%edx, %ecx
	pushl	%ecx
	movl	24(%ebp), %eax
.pixUWin2:
	movsb
	movl	.middleCount, %ecx
	rep
	movsl
	movl	.rightCount, %ecx
	orl	%ecx, %ecx
	jz	.pixUWin1
	rep
	movsb
.pixUWin1:
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.pixUWin2
	popl	%ecx
	orl	%ecx, %ecx
	jz	.pixWinDone
	subl	$0x10000, %edi
	movb	.segment, %al
	incb	%al
	movb	%al, .segment
	movl	$0x3cd, %edx
	OUT_B	(%dx)	
	movl	$64, %edx	
	jmp	.pixUWinLoop
/**********************************/
	ALIGNTEXT4
.shortPixWin:
	sall	$2, %ecx
	movl	.shortPixWinTable(%ecx), %eax
	movl	%eax, .funcPtr
	movl	16(%ebp), %ecx
.shortPixWinLoop:
	cmpl	%edx, %ecx
	jg	.shortPixWinMore
	movl	%ecx, %edx
.shortPixWinMore:
	subl	%edx, %ecx
	movl	24(%ebp), %eax
	call	*.funcPtr
	orl	%ecx, %ecx
	jz	.pixWinDone
	subl	$0x10000, %edi
	movb	.segment, %al
	incb	%al
	movb	%al, .segment
	movl	$0x3cd, %edx
	OUT_B	(%dx)	
	movl	$64, %edx
	jmp	.shortPixWinLoop
/********************************/
	ALIGNTEXT4
.shortPixWin1:
.shortWinPix1:
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.shortPixWin1
	ret
/********************************/
	ALIGNTEXT4
.shortPixWin2:
.shortWinPix2:
	movsw
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.shortPixWin2
	ret
/********************************/
	ALIGNTEXT4
.shortPixWin3:
	testl	$1, %edi
	jnz	.shortPixWin31
.shortPixWin32:
	movsw
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.shortPixWin32
	ret
/********************************/
	ALIGNTEXT4
.shortPixWin31:
.shortWinPix31:
	movsb
	movsw
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.shortPixWin31
	ret
/********************************/
	ALIGNTEXT4
.shortPixWin4:
.shortWinPix4:
	movsl
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.shortPixWin4
	ret
/********************************/
	ALIGNTEXT4
.winPixDone:
.pixWinDone:
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
/*  void WinPix(src, dst, h, w, srcOffset, dstOffset) */
	.data
	ALIGNDATA4
.shortWinPixTable:
	.long	0, .shortWinPix1, .shortWinPix2
	.long	.shortWinPix3, .shortWinPix4
/********************************/
	.text
	ALIGNTEXT4
	.globl	WinPix
WinPix:
	movl	12(%esp), %eax
	orl	16(%esp), %eax
	jz	.return
	cld
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	movl	28(%ebp), %ebx
	movl	20(%ebp), %ecx
	movl	8(%ebp), %esi	
	movl	12(%ebp), %edi
	subl	VGABASE, %esi
	movl	%esi, %eax
	andl	$0x0ffff, %esi
	sarl	$12, %eax
	movb	%al, .segment
	movl	$0x3cd, %edx
	OUT_B	(%dx)
	movl	$0x103ff, %edx
	subl	%esi, %edx
	addl	vgaReadBottom, %esi
	sarl	$10, %edx
	cmpl	$4, %ecx
	jle	.shortWinPix
	testl	$1, %esi
	jnz	.winUPix
.winPix:
	movl	%ecx, %eax
	andl	$3, %eax
	movl	%eax, .rightCount
	sarl	$2, %ecx
	movl	%ecx, .middleCount
	movl	16(%ebp), %ecx
.winPixLoop:
	cmpl	%edx, %ecx
	jg	.winPixMore
	movl	%ecx, %edx
.winPixMore:
	subl	%edx, %ecx
	pushl	%ecx
	movl	24(%ebp), %eax
.winPix2:
	movl	.middleCount, %ecx
	rep
	movsl
	movl	.rightCount, %ecx
	orl	%ecx, %ecx
	jz	.winPix1
	rep
	movsb
.winPix1:
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.winPix2
	popl	%ecx
	orl	%ecx, %ecx
	jz	.winPixDone
	subl	$0x10000, %esi
	movb	.segment, %al
	addb	$16, %al
	movb	%al, .segment
	movl	$0x3cd, %edx
	OUT_B	(%dx)	
	movl	$64, %edx
	jmp	.winPixLoop
/********************************/
	ALIGNTEXT4
.winUPix:
	decl	%ecx
	movl	%ecx, %eax
	andl	$3, %eax
	movl	%eax, .rightCount
	sarl	$2, %ecx
	movl	%ecx, .middleCount
	movl	16(%ebp), %ecx
.winUPixLoop:
	cmpl	%edx, %ecx
	jg	.winUPixMore
	movl	%ecx, %edx
.winUPixMore:
	subl	%edx, %ecx
	pushl	%ecx
	movl	24(%ebp), %eax
.winUPix2:
	movsb
	movl	.middleCount, %ecx
	rep
	movsl
	movl	.rightCount, %ecx
	orl	%ecx, %ecx
	jz	.winUPix1
	rep
	movsb
.winUPix1:
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.winUPix2
	popl	%ecx
	orl	%ecx, %ecx
	jz	.winPixDone
	subl	$0x10000, %esi
	movb	.segment, %al
	addb	$16, %al	
	movb	%al, .segment
	movl	$0x3cd, %edx
	OUT_B	(%dx)	
	movl	$64, %edx	
	jmp	.winUPixLoop
/**********************************/
	ALIGNTEXT4
.shortWinPix:
	sall	$2, %ecx
	movl	.shortWinPixTable(%ecx), %eax
	movl	%eax, .funcPtr
	movl	16(%ebp), %ecx
.shortWinPixLoop:
	cmpl	%edx, %ecx
	jg	.shortWinPixMore
	movl	%ecx, %edx
.shortWinPixMore:
	subl	%edx, %ecx
	movl	24(%ebp), %eax
	call	*.funcPtr
	orl	%ecx, %ecx
	jz	.winPixDone
	subl	$0x10000, %esi
	movb	.segment, %al
	addb	$16, %al
	movb	%al, .segment
	movl	$0x3cd, %edx
	OUT_B	(%dx)	
	movl	$64, %edx
	jmp	.shortWinPixLoop
/********************************/
	ALIGNTEXT4
.shortWinPix3:
	testl	$1, %esi
	jnz	.shortWinPix31
.shortWinPix32:
	movsw
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	.shortWinPix32
	ret
/*  void PixPix(src, dst, h, w, srcOffset, dstOffset, ydir) */
	.globl	PixPix
PixPix:
	movl	12(%esp), %eax
	orl	16(%esp), %eax
	jz	.return
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	movl	8(%ebp), %esi
	movl	12(%ebp), %edi
	movl	20(%ebp), %edx
	movl	28(%ebp), %ebx
	cmpl	$4, %edx
	jle	.shortPixPix
	movl	16(%ebp), %eax
	movl	%eax, .heightCount
	cmpl	$1, 32(%ebp)
	jne	.pixPixRL
	cld
	movl	%esi, %eax
	addl	$3, %eax
	andl	$0xfffffffc, %eax
	subl	%esi, %eax
	movl	%eax, .leftCount
	subl	%eax, %edx
	movl	%edx, %eax
	sarl	$2, %edx
	jz	.pixPixLRLR
	andl	$3, %eax
	jz	.pixPixLRLMorM
	movl	%eax, .rightCount
	movl	24(%ebp), %eax
	cmpl	$0, .leftCount
	je	.pixPixLRMR
/********************************/
.pixPixLRLMR:
	movl	.leftCount, %ecx
	rep
	movsb
	movl	%edx, %ecx
	rep
	movsl
	movl	.rightCount, %ecx
	rep
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	.heightCount
	jnz	.pixPixLRLMR
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
/********************************/
	ALIGNTEXT4
.pixPixLRMR:
	movl	%edx, %ecx
	rep
	movsl
	movl	.rightCount, %ecx
	rep
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	.heightCount
	jnz	.pixPixLRMR
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
/********************************/
	ALIGNTEXT4
.pixPixLRLMorM:
	movl	24(%ebp), %eax
	cmpl	$0, .leftCount
	je	.pixPixLRM
.pixPixLRLM:
	movl	.leftCount, %ecx
	rep
	movsb
	movl	%edx, %ecx
	rep
	movsl
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	.heightCount
	jnz	.pixPixLRLM
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret	
/********************************/
	ALIGNTEXT4
.pixPixLRM:
	movl	%edx, %ecx
	rep
	movsl
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	.heightCount
	jnz	.pixPixLRM
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
/********************************/
	ALIGNTEXT4
.pixPixLRLR:
	movl	24(%ebp), %eax
	movl	20(%ebp), %edx
	subl	$4, %edx
.pixPixLRLR1:
	movsl
	movl	%edx, %ecx
	rep
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	.heightCount
	jnz	.pixPixLRLR1
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
/********0************************/
	ALIGNTEXT4
.pixPixRL:
	std
	movl	%esi, %eax
	incl	%eax
	andl	$3, %eax
	movl	%eax, .rightCount
	subl	%eax, %edx
	movl	%edx, %eax
	sarl	$2, %edx
	jz	.pixPixRLLR
	andl	$3, %eax
	jz	.pixPixRLMRorM
	movl	%eax, .leftCount
	movl	24(%ebp), %eax
	cmpl	$0, .rightCount
	je	.pixPixRLLM
.pixPixRLLMR:
	movl	.rightCount, %ecx
	rep
	movsb
	subl	$3, %esi
	subl	$3, %edi
	movl	%edx, %ecx
	rep
	movsl
	addl	$3, %esi
	addl	$3, %edi
	movl	.leftCount, %ecx
	rep
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	.heightCount
	jnz	.pixPixRLLMR
	popl	%ebx
	popl	%esi
	popl	%edi
	cld
	leave
	ret	
/********************************/
	ALIGNTEXT4
.pixPixRLLM:
	subl	$3, %esi
	subl	$3, %edi
	subl	$3, %eax
	subl	$3, %ebx
.pixPixRLLM1:
	movl	%edx, %ecx
	rep
	movsl
	movl	.leftCount, %ecx
	addl	$3, %esi
	addl	$3, %edi
	rep
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	.heightCount
	jnz	.pixPixRLLM1
	popl	%ebx
	popl	%esi
	popl	%edi
	cld
	leave
	ret		
/********************************/
	ALIGNTEXT4
.pixPixRLMRorM:
	movl	24(%ebp), %eax
	cmp	$0, .rightCount
	je	.pixPixRLM
	addl	$3, %eax
	addl	$3, %ebx
.pixPixRLMR:
	movl	.rightCount, %ecx
	rep
	movsb
	subl	$3, %esi
	subl	$3, %edi
	movl	%edx, %ecx
	rep
	movsl
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	.heightCount
	jnz	.pixPixRLMR
	popl	%ebx
	popl	%esi
	popl	%edi
	cld
	leave
	ret
/********************************/
	ALIGNTEXT4
.pixPixRLM:
	subl	$3, %esi
	subl	$3, %edi
.pixPixRLM1:
	movl	%edx, %ecx
	rep
	movsl
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	.heightCount
	jnz	.pixPixRLM1
	popl	%ebx
	popl	%esi
	popl	%edi
	cld
	leave
	ret	
/********************************/
	ALIGNTEXT4
.pixPixRLLR:
	movl	24(%ebp), %eax
	movl	20(%ebp), %edx
.pixPixRLLR1:
	movl	%edx, %ecx
	rep
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	.heightCount
	jnz	.pixPixRLLR1
	popl	%ebx
	popl	%esi
	popl	%edi
	cld
	leave
	ret
/********************************/
	ALIGNTEXT4
.shortPixPix:
	cld
	movl	16(%ebp), %ecx
	movl	24(%ebp), %eax
	sall	$2, %edx
	jmp	*.shortPixPixTable(%edx)
/********************************/
	ALIGNTEXT4
.shortPixPix4:
	movsl
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%ecx
	jnz	.shortPixPix4
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
/********************************/
	ALIGNTEXT4
.shortPixPix3:
	testl	$1, %esi
	jnz	.shortPixPix3U
	incl	%eax
	incl	%ebx
.shortPixPix31:
	movb	2(%esi), %dl
	movsw
	movb	%dl, (%edi)
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%ecx
	jnz	.shortPixPix31
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
/********************************/
	ALIGNTEXT4
.shortPixPix3U:
	addl	$2, %eax
	addl	$2, %ebx
.shortPixPix3U1:
	movw	1(%esi), %dx
	movsb
	movw	%dx, (%edi)
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%ecx
	jnz	.shortPixPix3U1
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
/********************************/
	ALIGNTEXT4
.shortPixPix2:
	movsw
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%ecx
	jnz	.shortPixPix2
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
/********************************/
	ALIGNTEXT4
.shortPixPix1:
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%ecx
	jnz	.shortPixPix1
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
/********************************/
	ALIGNTEXT4
.pDone:
	movl	$0x4005, %eax
	movl	$0x3ce, %edx
	OUT_W	(%dx)
	movl	$0x0c04, %eax
	movl	$0x3c4, %edx
	OUT_W	(%dx)
	movl	$0x0f02, %eax
	OUT_W	(%dx)
.done:
	cld
	popl %ebx
	popl %esi
	popl %edi
	leave
.return:
	ret
