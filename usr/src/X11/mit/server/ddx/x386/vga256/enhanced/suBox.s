/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/suBox.s,v 1.3 1992/08/29 11:10:07 dawes Exp $ */
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
	.file	"suBox.s"

#include "assembler.h"
#include "vgaAsm.h"

#if !defined(SYSV) && !defined(SVR4)
#define SpeedUpBox _SpeedUpBox
#define SpeedUpHLine1 _SpeedUpHLine1
#define SpeedUpVLine1 _SpeedUpVLine1
#endif

/*
    void SpeedUpBox(dst, fill, h, w, screen)
*/

#define dst	8(%ebp)
#define fill	12(%ebp)
#define height	16(%ebp)
#define width	20(%ebp)
#define screen	24(%ebp)

	.data
copyright:
	STRING "Copyright 7/27/1992 by Glenn G. Lai" 
	ALIGNDATA4
pixTable:
	.long	pix0D, pix1D, pix2D, pix3D
pixMiddle:
	.long	0
/****************************************/
	.text
	ALIGNTEXT4
	.globl SpeedUpBox
SpeedUpBox:
	movl	12(%esp), %eax
	movl	16(%esp), %ecx
	movl	%eax, %edx
	orl	%ecx, %edx
	jz	return

	cmpl	$1, %eax
	jne	notHLine

	movl	%ecx, 12(%esp)
	movl	20(%esp), %eax
	movl	%eax, 16(%esp)
	jmp	SpeedUpHLine1
        ALIGNTEXT4
notHLine:
	cmpl	$1, %ecx
	jne	notVHLine

	movl	%eax, 12(%esp)
	movl	%eax, %ecx
	movl	20(%esp), %eax
	movl	%eax, 16(%esp)
	jmp	SpeedUpVLine1
/****************************************/
        ALIGNTEXT4
notVHLine:
	movl	4(%esp), %eax
	cmpl	VGABASE, %eax
	jnc	window

	pushl 	%ebp
	movl 	%esp, %ebp
	pushl 	%edi
	pushl	%esi
	pushl	%ebx
pix:
	movl	fill, %ebx
	movl	width, %ecx
	movl	height, %edx
	movl	screen, %esi
	movl	%eax, %edi

	addl	$3, %eax
	andl	$0xfffffffc, %eax
	movl	%eax, pixMiddle

	subl	%edi, %eax
	cmpl	%eax, %ecx
	jnc	fillLeft
	movl	%ecx, %eax
fillLeft:
	subl	%eax, %ecx
	call	*pixTable(,%eax,4)
fillRight:
	movl	%ecx, %eax
	andl	$3, %eax
	jz	fillMiddle

	movl	%ecx, %edi
	andl	$0xfffffffc, %edi
	addl	pixMiddle, %edi
	call	*pixTable(,%eax,4)
fillMiddle:
	sarl	$2, %ecx
	jz	done
	movl	%ecx, tmp
	movl	%ebx, %eax
	movl	pixMiddle, %ebx
fillMiddle1:
	movl	tmp, %ecx
	movl	%ebx, %edi
	rep
	stosl
	addl	%esi, %ebx
	decl	%edx
	jnz	fillMiddle1
	jmp	done
/****************/
        ALIGNTEXT4
pix0D:
	ret
/****************/
        ALIGNTEXT4
pix1D:
	movl	%edx, %eax
p1D:
	movb	%bl, (%edi)
	addl	%esi, %edi
	decl	%eax
	jnz	p1D
	ret
/****************/
        ALIGNTEXT4
pix2D:
	movl	%edx, %eax
p2D:
	movw	%bx, (%edi)
	addl	%esi, %edi
	decl	%eax
	jnz	p2D
	ret
/****************/
        ALIGNTEXT4
pix3D:
	movl	%edx, %eax
p3D:
	movb	%bl, (%edi)
	movw	%bx, 1(%edi)
	addl	%esi, %edi
	decl	%eax
	jnz	p3D
	ret
/****************************************/
	.data
        ALIGNDATA4
npTable:
	.long	npM, npMR, npLM, npLMR
/****************************************/
	.text
        ALIGNTEXT4
window:
	subl	VGABASE, %eax
	pushl 	%ebp
	movl 	%esp,%ebp
	pushl 	%edi
	movl	%eax, %edi
	pushl	%esi
	pushl	%ebx

/* Change THRESHOLD at your own risk!!! */ 
#define THRESHOLD	$22
	cmpl	THRESHOLD, %ecx
	jge	pMode

	movl	screen, %eax
	subl	%ecx, %eax
	movl	%eax, offset
	addl	vgaWriteTop, %eax
	movl	%eax, speedUpTop

	movl	$npTable, %eax
	testl	$1, %edi
	jz	window1
	addl	$8, %eax
	decl	%ecx
window1:
	shrl	$1, %ecx
	jz	window2
	jnc	window3
	addl	$4, %eax
	jmp	window3
window2:
	movl	$npTable, %eax
	movl	$1, %ecx
window3:
	movl	(%eax), %eax
	movl	%eax, func
	movl	%ecx, mCount

	movl	%edi, %eax
	shrl	$16, %eax
	movb	%al, segment
	
	andl	$0xffff, %edi
	addl	vgaWriteBottom, %edi
	jmp	npLoop3
        ALIGNTEXT4
npLoop:
	cmpl	vgaWriteTop, %edi
	jc	npLoop1
	subl	$0x10000, %edi
	movb	segment, %al
	incb	%al
        movb    %al, segment
npLoop3:
        movw    $0x3cd, %dx
	OUT_B(%dx)
npLoop1:
	movl	speedUpTop, %eax
	subl	%edi, %eax
	xorl	%edx, %edx 
	divl	screen

	orl	%eax, %eax
	jz 	npPartial

	movl	height, %ebx
	cmpl	%ebx, %eax
	jc	npLoop2
	movl	%ebx, %eax
npLoop2:
	subl	%eax, %ebx
	movl	%ebx, height
	movl	%eax, %edx

	movl	fill, %eax
	movl	mCount, %ebx
	movl	offset, %esi

	call	*func

	cmpl	$0, height
	jnz	npLoop

	popl 	%ebx
	popl 	%esi
	popl 	%edi
	leave
	ret
/****************************************/
        ALIGNTEXT4
npM:
	movl	%ebx, %ecx
	rep
	stosw
	addl	%esi, %edi
	decl	%edx
	jnz	npM
	ret
/****************************************/
        ALIGNTEXT4
npMR:
	movl	%ebx, %ecx
	rep
	stosw
	stosb
	addl	%esi, %edi
	decl	%edx
	jnz	npMR
	ret
/****************************************/
        ALIGNTEXT4
npLM:
	movl	%ebx, %ecx
	stosb
	rep
	stosw
	addl	%esi, %edi
	decl	%edx
	jnz	npLM
	ret
/****************************************/
        ALIGNTEXT4
npLMR:
	movl	%ebx, %ecx
	stosb
	rep
	stosw
	stosb
	addl	%esi, %edi
	decl	%edx
	jnz	npLMR
	ret
/****************************************/
        ALIGNTEXT4
npPartial:
	subl	offset, %edx

	movl	fill, %eax
	movl	width, %ebx
	movl	%edx, %ecx
	subl	%ecx, %ebx

	testl	$1, %edi
	jz	npPartial1
	stosb
	decl	%ecx
	jz	npPartial2
npPartial1:
	shrl	$1, %ecx
	rep
	stosw
npPartial2:
	subl	$0x10000, %edi
	movb	segment, %al
	incb	%al
	movb	%al, segment
	movw	$0x3cd, %dx
        OUT_B(%dx)

	movl	fill, %eax
	movl	%ebx, %ecx
	shrl	$1, %ecx
	jz	npPartial3
	rep
	stosw
	jnc	npPartial4
npPartial3:
	stosb
npPartial4:
	addl	offset, %edi
	decl	height
	jnz	npLoop1

	popl 	%ebx
	popl 	%esi
	popl 	%edi
	leave
	ret
/****************************************/
	.data
        ALIGNDATA4
lCount:
	.long	0
mCount:
	.long	0
rCount:
	.long	0
offset:
	.long	0
allowance:
	.long	0
func:
	.long	0
speedUpTop:
	.long	0
tmp:
	.long	0
pCorrect:
	.long	0
pTable:
	.long	pM, pMR, pLM, pLMR
lMaskTable:
	.value	0x0f02, 0x0802, 0x0c02, 0x0e02
rMaskTable:
	.value	0x0f02, 0x0102, 0x0302, 0x0702
lMask:
	.value	0
rMask:
	.value	0
segment:
	.byte	0
/****************************************/
	.text
        ALIGNTEXT4
pMode:
	movl	%edi, %esi
	addl	$3, %esi
	andl	$0xfffffffc, %esi
	subl	%edi, %esi

	subl	%esi, %ecx
	movl	%ecx, %eax
	andl	$3, %eax

	movl	$0, tmp
	movl	$pTable, %ebx
	xorl	%edx, %edx
	sall	$1, %esi
	jz	pNoL

	addl	$8, %ebx
	movl	$1, %edx
	movl	$1, tmp
pNoL:
	movl	%edx, pCorrect
	movw	lMaskTable(%esi), %dx
	movw	%dx, lMask

	sarl	$2, %ecx
	movl	%ecx, mCount

	sall	$1, %eax
	jz	pNoR
	addl	$4, %ebx
	incl	tmp
pNoR:
	movl	(%ebx), %ebx
	movl	%ebx, func
	movw	rMaskTable(%eax), %dx
	movw	%dx, rMask

	movl	screen, %eax
	sarl	$2, %eax
	movl	%eax, screen
	subl	%ecx, %eax
	movl	%eax, offset
	subl	tmp, %eax
	addl	vgaWriteTop, %eax
	movl	%eax, speedUpTop
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
	movl	%edi, %eax
	shrl	$18, %eax
	movb	%al, segment

        andl    $0x3ffff, %edi
        sarl    $2, %edi
        addl    vgaWriteBottom, %edi
	jmp	pLoop3
/***************************/
        ALIGNTEXT4
pLoop:
	cmpl    vgaWriteTop, %edi
        jc      pLoop1
        subl    $0x10000, %edi
        movb    segment, %al
        incb    %al
        movb    %al, segment
pLoop3:
        movw    $0x3cd, %dx
        OUT_B(%dx)
pLoop1:
        movl    speedUpTop, %eax
	subl	%edi, %eax
        xorl    %edx, %edx
        divl    screen

	orl	%eax, %eax
        jz      pPartial

        movl    height, %ebx
        cmpl    %ebx, %eax
        jc      pLoop2
        movl    %ebx, %eax
pLoop2:
        movl    %eax, allowance
        movl    mCount, %ebx
        movl    offset, %esi

        call    *func

	subl	pCorrect, %edi
pLoop4:
        movl    height, %ebx
        subl    allowance, %ebx
        movl    %ebx, height
        jnz     pLoop

        movw    $0x3c4, %dx
        movw    $0x0f02, %ax
        OUT_W(%dx)
        movw    $0x0c04, %ax
        OUT_W(%dx)

        popl    %ebx
        popl    %esi
        popl    %edi
        leave
        ret
/****************************************/
        ALIGNTEXT4
pPartial:
	movl	vgaWriteTop, %ecx
	subl	%edi, %ecx

	movw	lMask, %ax
        movw    $0x3c4, %dx
        OUT_W(%dx)

        movl    fill, %eax
        movl    mCount, %ebx

	cmpl	$0, pCorrect
	je	pPartial1

	stosb
	decl	%ecx
	movw	$0x0f02, %ax
        OUT_W(%dx)
        movl    fill, %eax
pPartial1:
	subl	%ecx, %ebx
	orl	%ecx, %ecx
	jz	pPartial2
	shrl	$1, %ecx
	jnc	pPartial3
	stosb
	jz	pPartial2
pPartial3:
	rep
	stosw
pPartial2:
        subl    $0x10000, %edi
	movb	segment, %al
	incb	%al
	movb	%al, segment
	movw	$0x3cd, %dx
        OUT_B(%dx)

	movl	fill, %eax
	movl	%ebx, %ecx
	orl	%ecx, %ecx
	jz	pPartial4

        shrl    $1, %ecx
	jz	pPartial6
        rep
        stosw
	jnc	pPartial4
pPartial6:
	stosb
pPartial4:
	movw	rMask, %ax
	cmpw	$0x0f02, %ax
	je	pPartial5
        movw    $0x3c4, %dx
        OUT_W(%dx)
	movl	fill, %eax
	movb	%al, (%edi)
pPartial5:
	addl	offset, %edi
	subl	pCorrect, %edi
        decl    height
        jnz     pLoop1

        movw    $0x3c4, %dx
        movw    $0x0f02, %ax
        OUT_W(%dx)
        movw    $0x0c04, %ax
        OUT_W(%dx)
done:
        popl    %ebx
        popl    %esi
        popl    %edi
        leave
return:
        ret
/****************************/
        ALIGNTEXT4
pMR:
        movw    rMask, %ax
        movw    $0x3c4, %dx
        OUT_W(%dx)

        movl    fill, %eax
        movl    allowance, %ebx
        movl    %edi, %edx
        addl    mCount, %edx
        movl    screen, %esi
pMR1:
        movb    %al, (%edx)
        addl    %esi, %edx
	decl	%ebx
	jnz	pMR1
	jmp	pM
/****************************/
        ALIGNTEXT4
pLMR:
	movw	rMask, %ax
	movw	$0x3c4, %dx
        OUT_W(%dx)

        movl    fill, %eax
        movl    allowance, %ebx
	leal	1(%edi), %edx
	addl	mCount, %edx
        movl    screen, %esi
pLMR1:
        movb    %al, (%edx)
        addl    %esi, %edx
	decl	%ebx
	jnz	pLMR1
/****************************/
pLM:
	movw	lMask, %ax
	movw	$0x3c4, %dx
        OUT_W(%dx)

        movl    fill, %eax
        movl    allowance, %ebx
	movl	%edi, %edx
        movl    screen, %esi
pLM1:
        movb    %al, (%edx)
        addl    %esi, %edx
	decl	%ebx
	jnz	pLM1

	incl	%edi
/****************************/
pM:
	movw	$0x0f02, %ax
	movw	$0x3c4, %dx
        OUT_W(%dx)

	movl	fill, %eax
	movl	mCount, %ebx
	movl	allowance, %edx
	movl	offset, %esi
	testl	$1, %edi
	jnz	pM1
	shrl	$1, %ebx
	jc	pM2
pM3:
	movl	%ebx, %ecx
	rep
	stosw
	addl	%esi, %edi
	decl	%edx
	jnz	pM3
	ret
/****************************/
        ALIGNTEXT4
pM2:
	movl	%ebx, %ecx
	rep
	stosw
	stosb
	addl	%esi, %edi
	decl	%edx
	jnz	pM2
	ret
/****************************/
        ALIGNTEXT4
pM1:
	decl	%ebx
	shrl	$1, %ebx
	jc	pM4
pM5:
	stosb
	movl	%ebx, %ecx
	rep
	stosw
	addl	%esi, %edi
	decl	%edx
	jnz	pM5
	ret
/****************************/
        ALIGNTEXT4
pM4:
	stosb
	movl	%ebx, %ecx
	rep
	stosw
	stosb
	addl	%esi, %edi
	decl	%edx
	jnz	pM4
	ret
