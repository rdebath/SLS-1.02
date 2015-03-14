/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/suBitBlt2.s,v 1.3 1992/08/29 11:10:05 dawes Exp $ */
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
7/27/92
*******************************************************************************/
	.file	"suBitBlt2.s"

#include "assembler.h"
#include "vgaAsm.h"

#if !defined(SYSV) && !defined(SVR4)
#define PixWin _PixWin
#define WinPix _WinPix
#define PixPix _PixPix
#endif

/*  void PixWin(src, dst, height, width, sWidth, dWidth) */

#define src	8(%ebp)
#define dst	12(%ebp)
#define height	16(%ebp)
#define width	20(%ebp)
#define sWidth	24(%ebp)
#define dWidth	28(%ebp)

        .data
copyright:
        STRING "Copyright 7/27/1992 by Glenn G. Lai"
	ALIGNDATA4
speedUpTop:
	.long	0
sOffset:
	.long	0
dOffset:
	.long	0
allowance:
	.long	0
lCount:
	.long	0
mCount:
	.long	0
rCount:
	.long	0
heightCount:
	.long	0
func:
	.long	0
sPixWinTable:
	.long	0, sPixWin1, sPixWin2, sPixWin3, sPixWin4
pixWinTable:
	.long	pwM, pwMR, pwLM, pwLMR
segment:
	.byte	0
/*************************************/
	.text
	ALIGNTEXT4
	.globl	PixWin
PixWin:
	movl	12(%esp), %eax
	orl	16(%esp), %eax
	jz	return

	cld
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx

	movl	dWidth, %eax
	movl	width, %ecx
	subl	%ecx, %eax
	movl	%eax, dOffset
	addl	vgaWriteTop, %eax
	movl	%eax, speedUpTop

	movl	sWidth, %eax
	subl	%ecx, %eax
	movl	%eax, sOffset
	
	movl	src, %esi	
	movl	dst, %edi
	subl	VGABASE, %edi
	cmpl	$5, %ecx
	jnc	pixWin1
	movl	sPixWinTable(,%ecx,4), %eax
	movl	%eax, func
	jmp	pixWin2
/*************************************/
        ALIGNTEXT4
pixWin1:
	movl	$pixWinTable, %eax
	testl	$1, %edi
	jz	pixWin3
	addl	$8, %eax
	decl	%ecx
pixWin3:
	movl	%ecx, %ebx
	andl	$3, %ebx
	jz	pixWin4
	addl	$4, %eax
pixWin4:
	movl	(%eax), %eax
	movl	%eax, func
	movl	%ebx, rCount
	shrl	$2, %ecx
	movl	%ecx, mCount
pixWin2:
	movl	%edi, %eax
	shrl	$16, %eax
	movb	%al, segment

	andl	$0xffff, %edi
	addl	vgaWriteBottom, %edi
        jmp     pwLoop3
/*************************************/
        ALIGNTEXT4
pwLoop:
        cmpl    vgaWriteTop, %edi
        jc      pwLoop1
        subl    $0x10000, %edi
        movb    segment, %al
	incb	%al
        movb    %al, segment
pwLoop3:
        movw    $0x3cd, %dx
	OUT_B(%dx)
pwLoop1:
	movl	speedUpTop, %eax
	subl	%edi, %eax
	xorl	%edx, %edx
	divl	dWidth

	orl	%eax, %eax
	jz	pwPartial

	movl	height, %ebx
        cmpl    %ebx, %eax
        jc      pwLoop2
        movl    %ebx, %eax
pwLoop2:
        movl    %eax, allowance
	movl	%eax, %edx
        movl    sOffset, %eax
        movl    dOffset, %ebx

        call    *func

        movl    height, %eax
        subl    allowance, %eax
        movl    %eax, height
        jnz     pwLoop

        popl    %ebx
        popl    %esi
        popl    %edi
        leave
return:
        ret
/********************************/
        ALIGNTEXT4
pwM:
wpM:
	movl	mCount, %ecx
	rep
	movsl
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	pwM
	ret
/********************************/
        ALIGNTEXT4
pwMR:
wpMR:
	movl	mCount, %ecx
	rep
	movsl
	movl	rCount, %ecx
	rep
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	pwMR
	ret
/********************************/
        ALIGNTEXT4
pwLM:
wpLM:
	movsb
	movl	mCount, %ecx
	rep
	movsl
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	pwLM
	ret
/********************************/
        ALIGNTEXT4
pwLMR:
wpLMR:
	movsb
	movl	mCount, %ecx
	rep
	movsl
	movl	rCount, %ecx
	rep
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	pwLMR
	ret
/********************************/
pwPartial:
	subl	dOffset, %edx
	movl	%edx, %ecx
	movl	width, %ebx
	subl	%ecx, %ebx

        testl   $1, %edi
        jz      pwPartial1
	movsb
        decl    %ecx
        jz      pwPartial2
pwPartial1:
        shrl    $1, %ecx
        rep
	movsw
pwPartial2:
        subl    $0x10000, %edi
        movb    segment, %al
        incb    %al
        movb    %al, segment
        movw    $0x3cd, %dx
	OUT_B(%dx)

        movl    %ebx, %ecx
        shrl    $1, %ecx
        jz      pwPartial3
        rep
        movsw
        jnc     pwPartial4
pwPartial3:
        movsb
pwPartial4:
        addl    sOffset, %esi
        addl    dOffset, %edi
        decl    height
        jnz     pwLoop1

        popl    %ebx
        popl    %esi
        popl    %edi
        leave
        ret
/********************************/
        ALIGNTEXT4
sPixWin1:
sWinPix1:
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	sPixWin1
	ret
/********************************/
        ALIGNTEXT4
sPixWin2:
sWinPix2:
	movsw
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	sPixWin2
	ret
/********************************/
        ALIGNTEXT4
sPixWin3:
sWinPix3:
	testl	$1, %edi
	jnz	sPixWin31
sPixWin32:
	movsw
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	sPixWin32
	ret
/********************************/
        ALIGNTEXT4
sPixWin31:
sWinPix31:
	movsb
	movsw
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	sPixWin31
	ret
/********************************/
        ALIGNTEXT4
sPixWin4:
sWinPix4:
	movsl
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	sPixWin4
	ret
/********************************/
        ALIGNTEXT4
winPixDone:
pixWinDone:
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret

/*  void WinPix(src, dst, h, w, srcOffset, dstOffset) */
	.data
        ALIGNDATA4
sWinPixTable:
	.long	0, sWinPix1, sWinPix2, sWinPix3, sWinPix4
winPixTable:
        .long   wpM, wpMR, wpLM, wpLMR
/********************************/
	.text
        ALIGNTEXT4
	.globl	WinPix
WinPix:
	movl	12(%esp), %eax
	orl	16(%esp), %eax
	jz	return

	cld
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx

	movl	sWidth, %eax
	movl	width, %ecx
	subl	%ecx, %eax
	movl	%eax, sOffset
	addl	vgaWriteTop, %eax
	movl	%eax, speedUpTop

	movl	dWidth, %eax
	subl	%ecx, %eax
	movl	%eax, dOffset
	
	movl	src, %esi	
	movl	dst, %edi
	subl	VGABASE, %esi
	cmpl	$5, %ecx
	jnc	winPix1
	movl	sWinPixTable(,%ecx,4), %eax
	movl	%eax, func
	jmp	winPix2
/********************************/
        ALIGNTEXT4
winPix1:
	movl	$winPixTable, %eax
	testl	$1, %esi
	jz	winPix3
	addl	$8, %eax
	decl	%ecx
winPix3:
	movl	%ecx, %ebx
	andl	$3, %ebx
	jz	winPix4
	addl	$4, %eax
winPix4:
	movl	(%eax), %eax
	movl	%eax, func
	movl	%ebx, rCount
	shrl	$2, %ecx
	movl	%ecx, mCount
winPix2:
	movl	%esi, %eax
	shrl	$16, %eax
	shlb	$4, %al
	movb	%al, segment

	andl	$0xffff, %esi
	addl	vgaWriteBottom, %esi
        jmp     wpLoop3
/********************************/
        ALIGNTEXT4
wpLoop:
        cmpl    vgaWriteTop, %esi
        jc      wpLoop1
        subl    $0x10000, %esi
        movb    segment, %al
	addb	$16, %al
        movb    %al, segment
wpLoop3:
        movw    $0x3cd, %dx
        OUT_B(%dx)
wpLoop1:
	movl	speedUpTop, %eax
	subl	%esi, %eax
	xorl	%edx, %edx
	divl	sWidth

	orl	%eax, %eax
	jz	wpPartial

	movl	height, %ebx
        cmpl    %ebx, %eax
        jc      wpLoop2
        movl    %ebx, %eax
wpLoop2:
        movl    %eax, allowance
	movl	%eax, %edx
        movl    sOffset, %eax
        movl    dOffset, %ebx

        call    *func

        movl    height, %eax
        subl    allowance, %eax
        movl    %eax, height
        jnz     wpLoop

        popl    %ebx
        popl    %esi
        popl    %edi
        leave
        ret
/********************************/
wpPartial:
	subl	sOffset, %edx
	movl	%edx, %ecx
	movl	width, %ebx
	subl	%ecx, %ebx

        testl   $1, %esi
        jz      wpPartial1
	movsb
        decl    %ecx
        jz      wpPartial2
wpPartial1:
        shrl    $1, %ecx
        rep
	movsw
wpPartial2:
        subl    $0x10000, %esi
        movb    segment, %al
	addb	$16, %al
        movb    %al, segment
        movw    $0x3cd, %dx
        OUT_B(%dx)

        movl    %ebx, %ecx
        shrl    $1, %ecx
        jz      wpPartial3
        rep
        movsw
        jnc     wpPartial4
wpPartial3:
        movsb
wpPartial4:
        addl    sOffset, %esi
        addl    dOffset, %edi
        decl    height
        jnz     wpLoop1

        popl    %ebx
        popl    %esi
        popl    %edi
        leave
        ret
/********************************/
	.data
        ALIGNDATA4
sPixPixTable:
        .long   0, sPixPix1, sPixPix2, sPixPix3, sPixPix4
/********************************/
	.text
        ALIGNTEXT4
/*  void PixPix(src, dst, h, w, srcOffset, dstOffset, ydir) */
	.globl	PixPix
PixPix:
	movl	12(%esp), %eax
	orl	16(%esp), %eax
	jz	return
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	movl	8(%ebp), %esi
	movl	12(%ebp), %edi
	cmpl	%esi, %edi
	jz	pixPixDone
	movl	20(%ebp), %edx
	movl	28(%ebp), %ebx
	cmpl	$4, %edx
	jle	sPixPix
	movl	16(%ebp), %eax
	movl	%eax, heightCount
	cmpl	$1, 32(%ebp)
	jne	pixPixRL
	cld
	movl	%esi, %eax
	addl	$3, %eax
	andl	$0xfffffffc, %eax
	subl	%esi, %eax
	movl	%eax, lCount
	subl	%eax, %edx
	movl	%edx, %eax
	sarl	$2, %edx
	jz	pixPixLRLR
	andl	$3, %eax
	jz	pixPixLRLMorM
	movl	%eax, rCount
	movl	24(%ebp), %eax
	cmpl	$0, lCount
	je	pixPixLRMR
/********************************/
pixPixLRLMR:
	movl	lCount, %ecx
	rep
	movsb
	movl	%edx, %ecx
	rep
	movsl
	movl	rCount, %ecx
	rep
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	heightCount
	jnz	pixPixLRLMR
pixPixDone:
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
/********************************/
        ALIGNTEXT4
pixPixLRMR:
	movl	%edx, %ecx
	rep
	movsl
	movl	rCount, %ecx
	rep
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	heightCount
	jnz	pixPixLRMR
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
/********************************/
        ALIGNTEXT4
pixPixLRLMorM:
	movl	24(%ebp), %eax
	cmpl	$0, lCount
	je	pixPixLRM
pixPixLRLM:
	movl	lCount, %ecx
	rep
	movsb
	movl	%edx, %ecx
	rep
	movsl
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	heightCount
	jnz	pixPixLRLM
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret	
/********************************/
        ALIGNTEXT4
pixPixLRM:
	movl	%edx, %ecx
	rep
	movsl
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	heightCount
	jnz	pixPixLRM
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
/********************************/
        ALIGNTEXT4
pixPixLRLR:
	movl	24(%ebp), %eax
	movl	20(%ebp), %edx
	subl	$4, %edx
pixPixLRLR1:
	movsl
	movl	%edx, %ecx
	rep
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	heightCount
	jnz	pixPixLRLR1
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
/********0************************/
        ALIGNTEXT4
pixPixRL:
	std
	movl	%esi, %eax
	incl	%eax
	andl	$3, %eax
	movl	%eax, rCount
	subl	%eax, %edx
	movl	%edx, %eax
	sarl	$2, %edx
	jz	pixPixRLLR
	andl	$3, %eax
	jz	pixPixRLMRorM
	movl	%eax, lCount
	movl	24(%ebp), %eax
	cmpl	$0, rCount
	je	pixPixRLLM
pixPixRLLMR:
	movl	rCount, %ecx
	rep
	movsb
	subl	$3, %esi
	subl	$3, %edi
	movl	%edx, %ecx
	rep
	movsl
	addl	$3, %esi
	addl	$3, %edi
	movl	lCount, %ecx
	rep
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	heightCount
	jnz	pixPixRLLMR
	popl	%ebx
	popl	%esi
	popl	%edi
	cld
	leave
	ret	
/********************************/
        ALIGNTEXT4
pixPixRLLM:
	subl	$3, %esi
	subl	$3, %edi
	subl	$3, %eax
	subl	$3, %ebx
pixPixRLLM1:
	movl	%edx, %ecx
	rep
	movsl
	movl	lCount, %ecx
	addl	$3, %esi
	addl	$3, %edi
	rep
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	heightCount
	jnz	pixPixRLLM1
	popl	%ebx
	popl	%esi
	popl	%edi
	cld
	leave
	ret		
/********************************/
        ALIGNTEXT4
pixPixRLMRorM:
	movl	24(%ebp), %eax
	cmp	$0, rCount
	je	pixPixRLM
	addl	$3, %eax
	addl	$3, %ebx
pixPixRLMR:
	movl	rCount, %ecx
	rep
	movsb
	subl	$3, %esi
	subl	$3, %edi
	movl	%edx, %ecx
	rep
	movsl
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	heightCount
	jnz	pixPixRLMR
	popl	%ebx
	popl	%esi
	popl	%edi
	cld
	leave
	ret
/********************************/
        ALIGNTEXT4
pixPixRLM:
	subl	$3, %esi
	subl	$3, %edi
pixPixRLM1:
	movl	%edx, %ecx
	rep
	movsl
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	heightCount
	jnz	pixPixRLM1
	popl	%ebx
	popl	%esi
	popl	%edi
	cld
	leave
	ret	
/********************************/
        ALIGNTEXT4
pixPixRLLR:
	movl	24(%ebp), %eax
	movl	20(%ebp), %edx
pixPixRLLR1:
	movl	%edx, %ecx
	rep
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	heightCount
	jnz	pixPixRLLR1
	popl	%ebx
	popl	%esi
	popl	%edi
	cld
	leave
	ret
/********************************/
        ALIGNTEXT4
sPixPix:
	cld
	movl	16(%ebp), %ecx
	movl	24(%ebp), %eax
	jmp	*sPixPixTable(,%edx,4)
/********************************/
        ALIGNTEXT4
sPixPix4:
	movsl
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%ecx
	jnz	sPixPix4
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
/********************************/
        ALIGNTEXT4
sPixPix3:
	testl	$1, %esi
	jnz	sPixPix3U
	incl	%eax
	incl	%ebx
sPixPix31:
	movb	2(%esi), %dl
	movsw
	movb	%dl, (%edi)
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%ecx
	jnz	sPixPix31
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
/********************************/
        ALIGNTEXT4
sPixPix3U:
	addl	$2, %eax
	addl	$2, %ebx
sPixPix3U1:
	movw	1(%esi), %dx
	movsb
	movw	%dx, (%edi)
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%ecx
	jnz	sPixPix3U1
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
/********************************/
        ALIGNTEXT4
sPixPix2:
	movsw
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%ecx
	jnz	sPixPix2
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
/********************************/
        ALIGNTEXT4
sPixPix1:
	movsb
	addl	%eax, %esi
	addl	%ebx, %edi
	decl	%ecx
	jnz	sPixPix1
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
	ret
