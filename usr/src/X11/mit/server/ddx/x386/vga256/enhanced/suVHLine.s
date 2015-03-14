/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/suVHLine.s,v 1.4 1992/09/09 05:14:46 dawes Exp $ */
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
8/21/92
*******************************************************************************/
	.file "suVHLine.s"

#include "assembler.h"
#include "vgaAsm.h"

#if !defined(SYSV) && !defined(SVR4)
#define SpeedUpVLine _SpeedUpVLine
#define SpeedUpVLine1 _SpeedUpVLine1
#define SpeedUpHLine _SpeedUpHLine
#define SpeedUpHLine1 _SpeedUpHLine1
#endif

/*
	void SpeedUpHLine(dst, fill, len, screen);
	void SpeedUpVLine(dst, fill, len, screen);
*/

#define dst     8(%ebp)
#define fill    12(%ebp)
#define len	16(%ebp)
#define screen  20(%ebp)
/****************************************/
	.data
copyright:
	STRING	"Copyright 8/21/1992 by Glenn G. Lai" 
	ALIGNDATA4
speedUpTop:
	.long	0
lMaskTable:
	.value	0x0f02
	.value	0x0802
	.value	0x0c02
	.value	0x0e02
rMaskTable:
	.value	0x0f02
	.value	0x0102
	.value	0x0302
	.value	0x0702
segment:
	.byte	0
/****************************************/
	.text
	ALIGNTEXT4
	.globl SpeedUpVLine
	.globl SpeedUpVLine1
SpeedUpVLine:
	movl	12(%esp), %ecx
	orl	%ecx, %ecx
	jz	return
SpeedUpVLine1:
	movl	4(%esp), %eax
	cmpl	VGABASE, %eax
	jc	pixmapV
windowV:
	cmpl	$1, %ecx
	jne	moreThanOnePoint
        movl    %eax, %ecx
        shrl    $16, %eax
        movl    $0x3cd, %edx
	OUT_B(%dx)
        andl    $0xffff, %ecx
        addl    vgaWriteBottom, %ecx
        movl    8(%esp), %eax
        movb    %al, (%ecx)
        ret
/****************************************/
        ALIGNTEXT4
moreThanOnePoint:
	pushl 	%ebp
	movl 	%esp, %ebp
	pushl 	%edi
	pushl	%esi
	pushl	%ebx

	movl	screen, %esi
	movl	%esi, %ebx
	decl	%ebx
	addl	vgaWriteTop, %ebx
	movl	%ebx, speedUpTop

        movl    %eax, %edi
        shrl    $16, %eax
        movb    %al, segment

        andl    $0xffff, %edi
        addl    vgaWriteBottom, %edi

	movl	fill, %ebx
	movl	len, %ecx
wVLoop:
	movl    $0x3cd, %edx
        OUT_B(%dx)

	xorl	%edx, %edx
	movl	speedUpTop, %eax
	subl	%edi, %eax
	divl	%esi

	cmpl	%ecx, %eax
	jc	wVLoop1
	movl	%ecx, %eax
wVLoop1:
	subl	%eax, %ecx
wvLoop2:
	movb	%bl, (%edi)
	addl	%esi, %edi
	decl	%eax
	jnz	wvLoop2

	orl	%ecx, %ecx
	jz	npDone

        subl    $0x10000, %edi
        movb    segment, %al
        incb    %al
        movb    %al, segment
	jmp	wVLoop
/*****************************************/
        ALIGNTEXT4
pixmapV:
	pushl	%edi
	pushl	%esi
	movl	16(%esp), %edx
	movl	24(%esp), %esi
	movl	%ecx, %edi
	andl	$3, %edi
	movl	pixmapVTable(,%edi,4), %edi
	shrl	$2, %ecx
	jz	pixmapV4
pixmapV5:
	movb	%dl, (%eax)
	addl	%esi, %eax
	movb	%dl, (%eax)
	addl	%esi, %eax
	movb	%dl, (%eax)
	addl	%esi, %eax
	movb	%dl, (%eax)
	addl	%esi, %eax
	decl	%ecx
	jnz	pixmapV5
pixmapV4:
	jmp	*%edi
/*******************/
	.data
        ALIGNDATA4
pixmapVTable:
	.long	pixmapV0, pixmapV1, pixmapV2, pixmapV3
/*******************/
	.text
        ALIGNTEXT4
pixmapV3:
	movb	%dl, (%eax)
	addl	%esi, %eax
pixmapV2:
	movb	%dl, (%eax)
	addl	%esi, %eax
pixmapV1:
	movb	%dl, (%eax)
pixmapV0:
	popl	%esi
	popl	%edi
	ret
/*****************************************/
	.globl	SpeedUpHLine
	.globl	SpeedUpHLine1
        ALIGNTEXT4
SpeedUpHLine:
	movl	12(%esp), %ecx
	orl	%ecx, %ecx
	jz	return
SpeedUpHLine1:
	movl	4(%esp), %eax
	subl	VGABASE, %eax
	jc	pixmapH
windowH:
	cmpl	$1, %ecx
	jne	windowH1
        movl    %eax, %ecx
        shrl    $16, %eax
        movw    $0x3cd, %dx
        OUT_B(%dx)
        andl    $0xffff, %ecx
        addl    vgaWriteBottom, %ecx
        movl    8(%esp), %eax
        movb    %al, (%ecx)
        ret
/****************/
        ALIGNTEXT4
windowH1:
/* Change THRESHOLD at YOUR OWN RISK!!! */ 
#define THRESHOLD	$80
	cmpl	THRESHOLD, %ecx
	jnc	pMode
windowH8:
	pushl	%edi
	movl	%eax, %edi
	shrl	$16, %eax
	movb	%al, segment
	movw	$0x3cd, %dx
        OUT_B(%dx)

	andl	$0xffff, %edi
	addl	vgaWriteBottom, %edi
	movl	vgaWriteTop, %eax
	subl	%edi, %eax

	xorl	%edx, %edx
	cmpl	%ecx, %eax
	jnc	windowH7

	movl	%ecx, %edx
	movl	%eax, %ecx
	subl	%eax, %edx
windowH7:
	movl	12(%esp), %eax
	testl	$1, %edi
	jz	windowH2
	stosb
	decl	%ecx
windowH2:
	shrl	$1, %ecx
	jz	windowH3
	rep
	stosw
	jnc	windowH4
windowH3:
	stosb
windowH4:
	orl	%edx, %edx
	jnz	windowH5
	popl	%edi
	ret
	ALIGNTEXT4
windowH5:
	movl	%edx, %ecx
	movb	segment, %al
	incb	%al
        movl    $0x3cd, %edx
        OUT_B(%dx)
	movl	12(%esp), %eax
	subl	$0x10000, %edi
	shrl	$1, %ecx
	jz	windowH6
	rep
	stosw
	jnc	windowH9
windowH6:
	stosb
windowH9:
	popl	%edi
	ret
/****************************/
        ALIGNTEXT4
pixmapH:
	addl	VGABASE, %eax
        cmpl    $7, %ecx
        jc	pixmapH0

	pushl	%edi
	movl	%eax, %edi
	movl	12(%esp), %eax

	testl	$1, %edi
	jz	pixmapH1
	stosb
	decl	%ecx
pixmapH1:
	testl	$2, %edi
	jz	pixmapH2
	stosw
	subl	$2, %ecx
pixmapH2:
	movl	%ecx, %edx
	shrl	$2, %ecx
	rep
	stosl
	testl	$2, %edx
	jz	pixmapH4
	stosw
pixmapH4:
	testl	$1, %edx
	jz	pixmapH5
	movb	%al, (%edi)
pixmapH5:
	popl	%edi
	ret
/****************/
        ALIGNTEXT4
pixmapH0:
        movl    8(%esp), %edx
pixmapH01:
	movb    %dl, (%eax)
        incl    %eax
        decl    %ecx
        jnz     pixmapH01
        ret
/****************************/
        ALIGNTEXT4
pMode:
	movl	%eax, %edx
	andl	$0x3ffff, %edx
	subl	$0x40000, %edx
	negl	%edx
	cmpl	%ecx, %edx
	jc	windowH8	

	pushl 	%ebp
	movl 	%esp,%ebp
	pushl 	%edi
	pushl	%esi
	pushl	%ebx
/***************************/
	movw	$0x3c4, %dx
	movw	$0x0604, %ax 
        OUT_W(%dx)
	movw	$0x3ce, %dx
	movw	$0x0001, %ax
        OUT_W(%dx)
	movw	$0x0003, %ax
        OUT_W(%dx)
	movw	$0x4005, %ax
        OUT_W(%dx)
	movw	$0xff08, %ax
        OUT_W(%dx)
/***************************/
	movl	fill, %ebx
	movl 	dst, %edi
	movl	%edi, %eax
	shrl	$18, %eax
	decw	%dx
        OUT_B(%dx)
	movw	$0x3c4, %dx

	andl	$0x3ffff, %edi
	movl	%edi, %eax
	shrl	$2, %edi
	addl	vgaWriteBottom, %edi

	movl	%eax, %esi
	addl	$3, %esi
	andl	$0xfffffffc, %esi
	subl	%eax, %esi
	jz	pMode1

	movw	lMaskTable(,%esi,2), %ax
        OUT_W(%dx)
	subl	%esi, %ecx
	movb	%bl, (%edi)
	incl	%edi
pMode1:
	movw	$0x0f02, %ax
        OUT_W(%dx)

	movl	%ebx, %eax
	movl	%ecx, %esi
	andl	$3, %esi
	shrl	$2, %ecx

	testl	$1, %edi
	jz	pMode2
	stosb
	decl	%ecx
pMode2:
	shrl	$1, %ecx
	rep
	stosw
	jnc	pMode3
	stosb
pMode3:
	shll	$1, %esi
	jz	pMode4
	movw	rMaskTable(%esi), %ax
        OUT_W(%dx)
	movb	%bl, (%edi)
	movw	$0x0f02, %ax
        OUT_W(%dx)
pMode4:
	movw	$0x0c04, %ax
        OUT_W(%dx)
npDone:
	popl 	%ebx
	popl 	%esi
	popl 	%edi
	leave
return:
	ret
