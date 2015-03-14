/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/suBitBlt.s,v 1.4 1992/08/29 11:10:04 dawes Exp $ */
/*******************************************************************************
			Copyr 1992 by Glenn G. Lai 

                        All Rs Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyr notice appear in all copies and that
both that copyr notice and this permission notice appear in 
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
8/17/92
*******************************************************************************/
	.file	"suBitBlt.s"

#include "assembler.h"
#include "vgaAsm.h"

#if !defined(SYSV) && !defined(SVR4)
#define WinWin _WinWin
#endif

/*  WinWin(src, dst, h, w, xdir, ydir, screen, special) */

#define src	8(%ebp)
#define dst	12(%ebp)
#define height	16(%ebp)
#define width	20(%ebp)
#define xdir	24(%ebp)
#define ydir	28(%ebp)
#define screen	32(%ebp)
#define special	36(%ebp)

	.data
copyright:
	STRING	"Copyright 8/17/1992 by Glenn G. Lai"
	ALIGNDATA4
tmp:
	.long	0
speedUpBound:
	.long	0
sM:
	.long	0
dM:
	.long	0
sM1:
	.long	0
dM1:
	.long	0
func:
	.long	0
partial:
	.long	0
npPartialJump:
	.long	0
pPartialJump:
	.long	0
pMOffset:
	.long	0
pLROffset:
	.long	0
allowance:
	.long	0
lCount:
	.long	0
mCount:
	.long	0
rCount:
	.long	0
hCount:
	.long	0
offset:
	.long	0
wwLRTable:
	.long	wBoxLR, wbBoxLR, bwBoxLR, bBoxLR
wwRLTable:
	.long	bBoxRL, bwBoxRL, wbBoxRL, wBoxRL
pTable:
        .long   pM, pMR, pLM, pLMR
pTable1:
        .long   pM, pMRa, pLMa, pLMRa
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
	.text
	ALIGNTEXT4
#if !defined(SYSV) && !defined(SVR4)
#define WinWin _WinWin
#endif
	.globl WinWin
#define	THRESHOLD $10
WinWin:
	movl	4(%esp), %eax
	cmpl	8(%esp), %eax
	je	return

	movl	16(%esp), %eax
	orl	20(%esp), %eax
	jz	return

	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	cld

	movl	width, %ecx
	movl	src, %esi
	movl	dst, %edi

	cmpl	$1, special
	je	nP

	movl	%esi, %eax
	movl	%edi, %ebx
	subl	%ebx, %eax
	andl	$3, %eax
	jnz	nP

	cmpl	THRESHOLD, %ecx
	jnc	pMode
nP:
	movl	%esi, %eax
	shrl	$12, %eax
	andb	$0x0f0, %al
	movl	%edi, %ebx
	shrl	$16, %ebx
	orb	%bl, %al
	movb	%al, segment

	andl	$0xffff, %esi
	andl	$0xffff, %edi
	addl	vgaReadBottom, %esi
	addl	vgaWriteBottom, %edi

	movl	$wwLRTable, %eax
	cmpl	$1, special
	jne	wwLeftToRight
	std
	movl	$wwRLTable, %eax
wwLeftToRight:
	testl	$1, %esi
	jz	ww3
	addl	$8, %eax
ww3:
	testl	$1, %edi
	jz	ww4
	addl	$4, %eax
ww4:
	cmpl	$6, %ecx
	jnc	ww2

	movl	$sWW, func
	jmp	ww5
	ALIGNTEXT4
ww2:
	movl	(%eax), %eax
	movl	%eax, func
ww5:
	cmpl	$1, special
	jne	npUpOrDown
npRL:
	movl	screen, %eax
	negl	%eax
	addl	%ecx, %eax
	movl	%eax, offset
	addl	vgaWriteBottom, %eax
	decl	%eax
	movl	%eax, speedUpBound
	movb	segment, %al
	jmp	npRL2
/***************************/
        ALIGNTEXT4
npRL5:
	movb	segment, %al
	cmpl	vgaReadBottom, %esi
	jnc	npRL1
	subb	$16, %al
	addl	$0x10000, %esi
npRL1:
	cmpl	vgaWriteBottom, %edi
	jnc	npRL7
	decb	%al
	addl	$0x10000, %edi
npRL7:
	movb	%al, segment
npRL2:
	movw	$0x3cd, %dx
	OUT_B(%dx)
npRL6:
	movl	%esi, %eax
	movl	%edi, %ebx
	subl	speedUpBound, %eax
	subl	speedUpBound, %ebx

	movl	screen, %ecx
	xorl	%edx, %edx
	divl	%ecx
	xchgl	%eax, %ebx

	xorl	%edx, %edx
	divl	%ecx
	
	cmpl	%ebx, %eax
	jc	npRL3

	movl	%ebx, %eax
npRL3:
	orl	%eax, %eax
	jz	npRLPartial

	movl	height, %ebx
	cmpl	%ebx, %eax
	jc	npRL4

	movl	%ebx, %eax
npRL4:
	subl	%eax, %ebx
	movl	%ebx, height

	movl	offset, %ebx
	movl	width, %ecx
	movl	%eax, %edx	
	call	*func

	cmpl	$0, height
	jne	npRL5

        cld
        popl %ebx
        popl %esi
        popl %edi
        leave
return:
        ret
/***************************/
        ALIGNTEXT4
npRLPartial:
	movl	width, %ebx
	movw	$0x3cd, %dx
npRLPartial5:
	movl	%esi, %eax
	movl	%edi, %ecx
	subl	vgaReadBottom, %eax
	subl	vgaWriteBottom, %ecx

	cmpl	%eax, %ecx
	jc	npRLPartial1

	movl	%eax, %ecx
npRLPartial1:
	incl	%ecx
	cmpl	%ebx, %ecx
	jc	npRLPartial2

	movl	%ebx, %ecx
npRLPartial2:
	subl	%ecx, %ebx

	rep
	movsb

	movb	segment, %al
	cmpl	vgaReadBottom, %esi
	jnc	npRLPartial3

	addl	$0x10000, %esi
	subb	$16, %al
npRLPartial3:
	cmpl	vgaWriteBottom, %edi
	jnc	npRLPartial4

	addl	$0x10000, %edi
	decb	%al
npRLPartial4:
	movb	%al, segment
	OUT_B(%dx)

	orl	%ebx, %ebx
	jnz	npRLPartial5

	movl	offset, %eax
	addl	%eax, %esi
	addl	%eax, %edi
	
	decl	height
	jnz	npRL5

        cld
        popl %ebx
        popl %esi
        popl %edi
        leave
        ret
/***************************/
npUpOrDown:
	cmpl	$1, ydir
	je	npDown
npUp:
	movl	screen, %eax
	movl	%eax, %ebx
	negl	%ebx
	movl	%ebx, screen
	movl	width, %ecx
	subl	%ecx, %eax
	movl	%eax, offset
	movl	$npUp3, npPartialJump
        movb    segment, %al
	jmp	npUp5
/***************************/
        ALIGNTEXT4
npUp3:
        movb    segment, %al
        cmpl    vgaReadBottom, %esi
        jnc     npUp4
        subb    $16, %al
	addl	$0x10000, %esi
npUp4:
        cmpl    vgaWriteBottom, %edi
        jnc     npUp6
        decb    %al
	addl	$0x10000, %edi
npUp6:
	movb	%al, segment
npUp5:
        movw    $0x3cd, %dx
        OUT_B(%dx)

	movl	width, %ebx
	movl	vgaReadTop, %eax
	subl	%esi, %eax
	cmpl	%ebx, %eax
	jc	npPartial

	movl	vgaWriteTop, %eax
	subl	%edi, %eax
	cmpl	%ebx, %eax
	jc	npPartial

	movl	%esi, %eax
	movl	%edi, %ebx
	subl	vgaReadBottom, %eax
	subl	vgaWriteBottom, %ebx

	movl	screen, %ecx
	xorl	%edx, %edx
	divl	%ecx
	xchgl	%eax, %ebx

	xorl	%edx, %edx
	divl	%ecx

	cmpl	%ebx, %eax
	jc	npUp1

	movl	%ebx, %eax
npUp1:
	incl	%eax
	movl	height, %ebx
	cmpl	%ebx, %eax
	jc	npUp2

	movl	%ebx, %eax
npUp2:
	subl	%eax, %ebx
	movl	%ebx, height

        movl    offset, %ebx
        movl    width, %ecx
        movl    %eax, %edx
        call    *func

        cmpl    $0, height
        jne     npUp3

	cld
        popl %ebx
        popl %esi
        popl %edi
        leave
        ret
/***************************/
        ALIGNTEXT4
npDown:
	movl	screen, %eax
	subl	width, %eax
	movl	%eax, offset
	addl	vgaWriteTop, %eax
	movl	%eax, speedUpBound
	movl	$npDown3, npPartialJump
	movb	segment, %al
	jmp	npDown5
/***************************/
        ALIGNTEXT4
npDown3:
        movb	segment, %al
        cmpl	vgaReadTop, %esi
        jc	npDown4
        addb    $16, %al
	subl	$0x10000, %esi
npDown4:
        cmpl    vgaWriteTop, %edi
        jc	npDown6
        incb	%al
	subl	$0x10000, %edi
npDown6:
	movb	%al, segment
npDown5:
        movw    $0x3cd, %dx
        OUT_B(%dx)

	movl	speedUpBound, %eax
	movl	speedUpBound, %ebx
        subl	%esi, %eax
        subl	%edi, %ebx

        movl    screen, %ecx
        xorl    %edx, %edx
        divl    %ecx
        xchgl	%eax, %ebx

        xorl    %edx, %edx
        divl    %ecx

        cmpl    %ebx, %eax
        jc      npDown1

        movl    %ebx, %eax
npDown1:
	orl	%eax, %eax
	jz	npPartial

        movl    height, %ebx
        cmpl    %ebx, %eax
        jc      npDown2

        movl    %ebx, %eax
npDown2:
        subl    %eax, %ebx
        movl    %ebx, height

        movl    offset, %ebx
        movl    width, %ecx
        movl    %eax, %edx
        call    *func

        cmpl    $0, height
        jne     npDown3

	cld
        popl %ebx
        popl %esi
        popl %edi
        leave
        ret
/***************************/
        ALIGNTEXT4
bBoxLR:
	decl	%ecx
	movl	%ecx, %eax
	shrl	$1, %eax
	andl	$1, %ecx
	jnz	bBoxLRR
bBoxLR1:
	movsb
	movl	%eax, %ecx
	rep
	movsw
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	bBoxLR1
	ret
/***************************/
        ALIGNTEXT4
bBoxLRR:
	movsb
	movl	%eax, %ecx
	rep
	movsw
	movsb
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	bBoxLRR
	ret
/***************************/
        ALIGNTEXT4
wBoxLR:
	movl	%ecx, %eax
	shrl	$1, %eax
	andl	$1, %ecx
	jnz	wBoxLRR
wBoxLR1:
	movl	%eax, %ecx
	rep
	movsw
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	wBoxLR1
	ret
/***************************/
        ALIGNTEXT4
wBoxLRR:
	movl	%eax, %ecx
	rep
	movsw
	movsb
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	wBoxLRR
	ret
/***************************/
        ALIGNTEXT4
bwBoxLR:
	movl	%edx, hCount
	decl	%ecx
	movl	%ecx, %eax
	sarl	$2, %ecx
	movl	%ecx, mCount
	andl	$3, %eax
	jnz	bwBoxLRR
bwBoxLR1:
	movb	(%esi), %dl
	incl	%esi
	movl	mCount, %ecx
bwBoxLR2:
	lodsl
	roll	$8, %eax
	xchgb	%al, %dl
	stosl
	decl	%ecx
	jnz	bwBoxLR2
	movb	%dl, (%edi)
	incl	%edi
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	hCount
	jnz	bwBoxLR1
	ret
/***************************/
        ALIGNTEXT4
bwBoxLRR:
	movl	%eax, rCount
bwBoxLRR1:
	movb	(%esi), %dl
	incl	%esi
	movl	mCount, %ecx
bwBoxLRR2:
	lodsl
	roll	$8, %eax
	xchgb	%al, %dl
	stosl
	decl	%ecx
	jnz	bwBoxLRR2
	movb	%dl, (%edi)
	incl	%edi
	movl	rCount, %ecx
	rep
	movsb
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	hCount
	jnz	bwBoxLRR1
	ret
/***************************/
        ALIGNTEXT4
wbBoxLR:
	movl	%edx, hCount
	subl	$2, %ecx
	movl	%ecx, %eax
	shrl	$2, %ecx
	movl	%ecx, mCount
	andl	$3, %eax
	jnz	wbBoxLRR
wbBoxLR2:
	lodsw
	stosb
	movb	%ah, %dl
	movl	mCount, %ecx
wbBoxLR1:
	lodsl
	roll	$8, %eax
	xchgb	%al, %dl
	stosl
	decl	%ecx
	jnz	wbBoxLR1
	movb	%dl, (%edi)
	incl	%edi
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	hCount
	jnz	wbBoxLR2
	ret
/***************************/
        ALIGNTEXT4
wbBoxLRR:
	movl	%eax, rCount
wbBoxLRR1:
	lodsw
	stosb
	movb	%ah, %dl
	movl	mCount, %ecx
wbBoxLRR2:
	lodsl
	roll	$8, %eax
	xchgb	%al, %dl
	stosl
	decl	%ecx
	jnz	wbBoxLRR2
	movb	%dl, (%edi)
	incl	%edi
	movl	rCount, %ecx
	rep
	movsb
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	hCount
	jnz	wbBoxLRR1
	ret
/***************************/
        ALIGNTEXT4
bBoxRL:
	decl	%ecx
	movl	%ecx, %eax
	sarl	$2, %eax
	andl	$3, %ecx
	jnz	bBoxRLL
	addl	$3, %ebx
bBoxRL1:
	movsb
	subl	$3, %esi
	subl	$3, %edi
	movl	%eax, %ecx
	rep
	movsl
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	bBoxRL1
	ret
/***************************/
        ALIGNTEXT4
bBoxRLL:
	movl	%ecx, lCount
bBoxRLL1:
	movsb
	subl	$3, %esi
	subl	$3, %edi
	movl	%eax, %ecx
	rep
	movsl
	addl	$3, %esi
	addl	$3, %edi
	movl	lCount, %ecx
	rep
	movsb
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	bBoxRLL1
	ret
/***************************/
        ALIGNTEXT4
wBoxRL:
	movl	%ecx, %eax
	sarl	$2, %eax
	andl	$3, %ecx
	jnz	wBoxRLL
	addl	$3, %ebx
wBoxRL1:
	subl	$3, %esi
	subl	$3, %edi
	movl	%eax, %ecx
	rep
	movsl
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	wBoxRL1
	ret
/***************************/
        ALIGNTEXT4
wBoxRLL:
	movl	%ecx, lCount
wBoxRLL1:
	subl	$3, %esi
	subl	$3, %edi
	movl	%eax, %ecx
	rep
	movsl
	addl	$3, %esi
	addl	$3, %edi
	movl	lCount, %ecx
	rep
	movsb
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	%edx
	jnz	wBoxRLL1
	ret
/***************************/
        ALIGNTEXT4
bwBoxRL:
	movl	%edx, hCount
	decl	%ecx
	movl	%ecx, %eax
	sarl	$2, %ecx
	movl	%ecx, mCount
	andl	$3, %eax
	jnz	bwBoxRLL
	addl	$3, %ebx
bwBoxRL1:
	movb	(%esi), %dl
	subl	$4, %esi
	subl	$3, %edi
	movl	mCount, %ecx
bwBoxRL2:
	lodsl
	xchgb	%al, %dl
	rorl	$8, %eax
	stosl
	decl	%ecx
	jnz	bwBoxRL2
	movb	%dl, 3(%edi)
	decl	%edi
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	hCount
	jnz	bwBoxRL1
	ret
/***************************/
        ALIGNTEXT4
bwBoxRLL:
	movl	%eax, lCount
bwBoxRLL1:
	movb	(%esi), %dl
	subl	$4, %esi
	subl	$3, %edi
	movl	mCount, %ecx
bwBoxRLL2:
	lodsl
	xchgb	%al, %dl
	rorl	$8, %eax
	stosl
	decl	%ecx
	jnz	bwBoxRLL2
	movb	%dl, 3(%edi)
	addl	$3, %esi
	addl	$2, %edi
	movl	lCount, %ecx
	rep
	movsb
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	hCount
	jnz	bwBoxRLL1
	ret
/***************************/
        ALIGNTEXT4
wbBoxRL:
	movl	%edx, hCount
	subl	$2, %ecx
	movl	%ecx, %eax
	sarl	$2, %ecx
	movl	%ecx, mCount
	andl	$3, %eax
	jnz	wbBoxRLL
	addl	$3, %ebx
wbBoxRL1:
	decl	%esi
	movw	(%esi), %ax
	movb	%ah, (%edi)
	movb	%al, %dl
	subl	$4, %esi
	subl	$4, %edi
	movl	mCount, %ecx
wbBoxRL2:
	lodsl
	xchgb	%al, %dl
	rorl	$8, %eax
	stosl
	decl	%ecx
	jnz	wbBoxRL2
	movb	%dl, 3(%edi)
	decl	%edi
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	hCount
	jnz	wbBoxRL1
	ret
/***************************/
        ALIGNTEXT4
wbBoxRLL:
	movl	%eax, lCount
wbBoxRLL1:
	decl	%esi
	movw	(%esi), %ax
	movb	%ah, (%edi)
	movb	%al, %dl
	subl	$4, %esi
	subl	$4, %edi
	movl	mCount, %ecx
wbBoxRLL2:
	lodsl
	xchgb	%al, %dl
	rorl	$8, %eax
	stosl
	decl	%ecx
	jnz	wbBoxRLL2
	movb	%dl, 3(%edi)
	addl	$3, %esi
	addl	$2, %edi
	movl	lCount, %ecx
	rep
	movsb
	addl	%ebx, %esi
	addl	%ebx, %edi
	decl	hCount
	jnz	wbBoxRLL1
	ret
/***************************/
        ALIGNTEXT4
npPartial:
	movl	width, %ebx
	movw	$0x3cd, %dx
npPartial5:
	movl	vgaReadTop, %eax
	movl	vgaWriteTop, %ecx
	subl	%esi, %eax
	subl	%edi, %ecx

	cmpl	%eax, %ecx
	jc	npPartial1

	movl	%eax, %ecx
npPartial1:
        cmpl    %ebx, %ecx
        jc      npPartial2

	movl	%ebx, %ecx
npPartial2:
	subl	%ecx, %ebx

	rep
	movsb

	movb	segment, %al
	cmpl	vgaReadTop, %esi
	jc	npPartial3

	subl	$0x10000, %esi
	addb	$16, %al
npPartial3:
	cmpl	vgaWriteTop, %edi
	jc	npPartial4

	subl	$0x10000, %edi
	incb	%al
npPartial4:
	movb	%al, segment
        OUT_B(%dx)

	orl	%ebx, %ebx
	jnz	npPartial5

	movl	offset, %eax
	addl	%eax, %esi
	addl	%eax, %edi
	
	decl	height
	jz	npPartial6
	jmp	*npPartialJump
        ALIGNTEXT4
npPartial6:
        popl %ebx
        popl %esi
        popl %edi
        leave
        ret
/***************************/
        ALIGNTEXT4
sWW:
	movl	%ecx, %eax	
sWW1:
	movl	%eax, %ecx
	rep
	movsb
        addl    %ebx, %esi
        addl    %ebx, %edi
        decl    %edx
        jnz     sWW1
	ret
/***************************/
        ALIGNTEXT4
pMode:
        movw    $0x0604, %ax
        movw    $0x3c4, %dx
        OUT_W(%dx)
        movw    $0x4105, %ax
        movw    $0x3ce, %dx
        OUT_W(%dx)

	movl	%esi, %eax
	movl	$pTable, %ebx
	movl	width, %ecx

	addl	$3, %eax
	andl	$0xfffffffc, %eax
	subl	%esi, %eax
	subl	%eax, %ecx

	xorl	%edx, %edx
	sall	$1, %eax
	jz	pNoL

	addl	$8, %ebx
	incl	%edx
pNoL:
	movw	lMaskTable(%eax), %ax
	movw	%ax, lMask
	
	movl	%ecx, %eax
	andl	$3, %eax

	sall	$1, %eax
	jz	pNoR
	
	addl	$4, %ebx
	incl	%edx
pNoR:
	cmpl	$1, xdir
	je	pModeLR
	addl	$16, %ebx
pModeLR:
	movl	(%ebx), %ebx
	movl	%ebx, func

	movw	rMaskTable(%eax), %ax
	movw	%ax, rMask

	shrl	$2, %ecx
	movl	%ecx, mCount

	addl	%ecx, %edx
	movl	%edx, width

	movl	%esi, %eax
	shrl	$14, %eax
	andb	$0xf0, %al
	movl	%edi, %ebx
	shrl	$18, %ebx
	orb	%bl, %al
	movb	%al, segment

	shrl	$2, %esi
	andl	$0xffff, %esi
	addl	vgaReadBottom, %esi	
	shrl	$2, %edi
	andl	$0xffff, %edi
	addl	vgaWriteBottom, %edi	

        movl    screen, %ebx
        sarl    $2, %ebx
	movl	%ebx, %eax

        decl    %ebx
        movl    %ebx, pLROffset

        incl    %ebx
        subl    %ecx, %ebx
        movl    %ebx, pMOffset
/***************************/
	cmpl	$1, ydir
	je	pDown
pUp:
        movl    %eax, %ebx
        negl    %ebx
        movl    %ebx, screen
        subl    width, %eax
        movl    %eax, offset
        movl    $pUp3, pPartialJump
        movb    segment, %al
        jmp     pUp5
/***************************/
        ALIGNTEXT4
pUp3:
        movb    segment, %al
        cmpl    vgaReadBottom, %esi
        jnc     pUp4
        subb    $16, %al
        addl    $0x10000, %esi
pUp4:
        cmpl    vgaWriteBottom, %edi
        jnc     pUp6
        decb    %al
        addl    $0x10000, %edi
pUp6:
        movb    %al, segment
pUp5:
        movw    $0x3cd, %dx
        OUT_B(%dx)

        movl    %esi, sM
        movl    %edi, dM

        movl    width, %ebx
        movl    vgaReadTop, %eax
        subl    %esi, %eax
        cmpl    %ebx, %eax
        jc      pPartial

        movl    vgaWriteTop, %eax
        subl    %edi, %eax
        cmpl    %ebx, %eax
        jc      pPartial

        movl    %esi, %eax
        movl    %edi, %ebx
        subl    vgaReadBottom, %eax
        subl    vgaWriteBottom, %ebx

        movl    screen, %ecx
        xorl    %edx, %edx
        divl    %ecx
        xchgl   %eax, %ebx

        xorl    %edx, %edx
        divl    %ecx

        cmpl    %ebx, %eax
        jc      pUp1

        movl    %ebx, %eax
pUp1:
        incl    %eax
        movl    height, %ebx
        cmpl    %ebx, %eax
        jc      pUp2

        movl    %ebx, %eax
pUp2:
        subl    %eax, %ebx
        movl    %ebx, height

        movl    %eax, allowance
        call    *func

        cmpl    $0, height
        jne     pUp3

        movw    $0x3c4, %dx
        movw    $0x0f02, %ax
        OUT_W(%dx)
        movw    $0x0c04, %ax
        OUT_W(%dx)
        movw    $0x4005, %ax
        movw    $0x3ce, %dx
        OUT_W(%dx)

        popl %ebx
        popl %esi
        popl %edi
        leave
        ret
/***************************/
        ALIGNTEXT4
pDown:
        movl    %eax, screen
        subl    width, %eax
        movl    %eax, offset
        addl    vgaWriteTop, %eax
        movl    %eax, speedUpBound
        movl    $pDown3, pPartialJump
        movb    segment, %al
        jmp     pDown5
/***************************/
        ALIGNTEXT4
pDown3:
        movb    segment, %al
        cmpl    vgaReadTop, %esi
        jc      pDown4
        addb    $16, %al
        subl    $0x10000, %esi
pDown4:
        cmpl    vgaWriteTop, %edi
        jc      pDown6
        incb    %al
        subl    $0x10000, %edi
pDown6:
        movb    %al, segment
pDown5:
        movw    $0x3cd, %dx
        OUT_B(%dx)

        movl    %esi, sM
        movl    %edi, dM

        movl    speedUpBound, %eax
        movl    speedUpBound, %ebx
        subl    %esi, %eax
        subl    %edi, %ebx

        movl    screen, %ecx
        xorl    %edx, %edx
        divl    %ecx
        xchgl   %eax, %ebx

        xorl    %edx, %edx
        divl    %ecx

        cmpl    %ebx, %eax
        jc      pDown1

        movl    %ebx, %eax
pDown1:
        orl     %eax, %eax
        jz      pPartial

        movl    height, %ebx
        cmpl    %ebx, %eax
        jc      pDown2

        movl    %ebx, %eax
pDown2:
        subl    %eax, %ebx
        movl    %ebx, height

        movl    %eax, allowance
        call    *func

        cmpl    $0, height
        jne     pDown3

        movw    $0x3c4, %dx
        movw    $0x0f02, %ax
        OUT_W(%dx)
        movw    $0x0c04, %ax
        OUT_W(%dx)
        movw    $0x4005, %ax
        movw    $0x3ce, %dx
        OUT_W(%dx)

        popl %ebx
        popl %esi
        popl %edi
        leave
        ret
/************************************/
        ALIGNTEXT4
pLMR:
	movw	$0x3c4, %dx
        movw    lMask, %ax
        OUT_W(%dx)

	movl	pLROffset, %eax
	movl	allowance, %edx
pLMR1:
	movsb
	addl	%eax, %esi
	addl	%eax, %edi
	decl	%edx
	jnz	pLMR1

	movl	%esi, sM1
	movl	%edi, dM1

	movw	$0x3c4, %dx
	movw	$0x0f02, %ax
        OUT_W(%dx)

	movl	sM, %esi
	movl	dM, %edi
	incl	%esi
	incl	%edi

	movl	pMOffset, %eax
	movl	mCount, %ebx
	movl	allowance, %edx
pLMR2:
	movl	%ebx, %ecx
	rep
	movsb
	addl	%eax, %esi
	addl	%eax, %edi
	decl	%edx
	jnz	pLMR2

	movw	$0x3c4, %dx
        movw    rMask, %ax
        OUT_W(%dx)

	movl	pLROffset, %eax
	movl	allowance, %edx

	movl	sM, %esi
	movl	dM, %edi
	movl	mCount, %ecx
	incl	%ecx
	addl	%ecx, %esi
	addl	%ecx, %edi
pLMR3:
        movsb
        addl    %eax, %esi
        addl    %eax, %edi
        decl    %edx
        jnz     pLMR3

	movl	sM1, %esi
	movl	dM1, %edi

	ret
/************************************/
        ALIGNTEXT4
pLMRa:
        movw    $0x3c4, %dx
        movw    rMask, %ax
        OUT_W(%dx)

        movl    pLROffset, %eax
        movl    allowance, %edx

        movl    mCount, %ecx
        incl    %ecx
        addl    %ecx, %esi
        addl    %ecx, %edi
pLMRa3:
        movsb
        addl    %eax, %esi
        addl    %eax, %edi
        decl    %edx
        jnz     pLMRa3

	movw	$0x3c4, %dx
        movw    $0x0f02, %ax
        OUT_W(%dx)

	movl	sM, %esi
	movl	dM, %edi
	incl	%esi
	incl	%edi

	movl	pMOffset, %eax
	movl	mCount, %ebx
	movl	allowance, %edx
pLMRa2:
	movl	%ebx, %ecx
	rep
	movsb
	addl	%eax, %esi
	addl	%eax, %edi
	decl	%edx
	jnz	pLMRa2

	movw	$0x3c4, %dx
        movw    lMask, %ax
        OUT_W(%dx)

	movl	pLROffset, %eax
	movl	allowance, %edx

	movl	sM, %esi
	movl	dM, %edi
pLMRa1:
	movsb
	addl	%eax, %esi
	addl	%eax, %edi
	decl	%edx
	jnz	pLMRa1

	ret
/************************************/
        ALIGNTEXT4
pLM:
	movw	$0x3c4, %dx
        movw    lMask, %ax
        OUT_W(%dx)

	movl	pLROffset, %eax
	movl	allowance, %edx
pLM1:
	movsb
	addl	%eax, %esi
	addl	%eax, %edi
	decl	%edx
	jnz	pLM1

	movl	%esi, sM1
	movl	%edi, dM1

	movw	$0x3c4, %dx
	movw	$0x0f02, %ax
        OUT_W(%dx)

	movl	sM, %esi
	movl	dM, %edi
	incl	%esi
	incl	%edi

	movl	pMOffset, %eax
	movl	mCount, %ebx
	movl	allowance, %edx
pLM2:
	movl	%ebx, %ecx
	rep
	movsb
	addl	%eax, %esi
	addl	%eax, %edi
	decl	%edx
	jnz	pLM2

	movl	sM1, %esi
	movl	dM1, %edi

	ret
/************************************/
        ALIGNTEXT4
pLMa:
	movw	$0x3c4, %dx
	movw	$0x0f02, %ax
        OUT_W(%dx)

	incl	%esi
	incl	%edi

	movl	pMOffset, %eax
	movl	mCount, %ebx
	movl	allowance, %edx
pLMa2:
	movl	%ebx, %ecx
	rep
	movsb
	addl	%eax, %esi
	addl	%eax, %edi
	decl	%edx
	jnz	pLMa2

	movw	$0x3c4, %dx
        movw    lMask, %ax
        OUT_W(%dx)

	movl	pLROffset, %eax
	movl	allowance, %edx

	movl	sM, %esi
	movl	dM, %edi
pLMa1:
	movsb
	addl	%eax, %esi
	addl	%eax, %edi
	decl	%edx
	jnz	pLMa1

	ret
/************************************/
        ALIGNTEXT4
pMR:
	movw	$0x3c4, %dx
	movw	$0x0f02, %ax
        OUT_W(%dx)

	movl	pMOffset, %eax
	movl	mCount, %ebx
	movl	allowance, %edx
pMR2:
	movl	%ebx, %ecx
	rep
	movsb
	addl	%eax, %esi
	addl	%eax, %edi
	decl	%edx
	jnz	pMR2

	movl	%esi, sM1
	movl	%edi, dM1

	movw	$0x3c4, %dx
        movw    rMask, %ax
        OUT_W(%dx)

	movl	pLROffset, %eax
	movl	allowance, %edx

	movl	sM, %esi
	movl	dM, %edi
	movl	mCount, %ecx
	addl	%ecx, %esi
	addl	%ecx, %edi
pMR3:
        movsb
        addl    %eax, %esi
        addl    %eax, %edi
        decl    %edx
        jnz     pMR3

	movl	sM1, %esi
	movl	dM1, %edi

	ret
/************************************/
        ALIGNTEXT4
pMRa:
	movw	$0x3c4, %dx
        movw    rMask, %ax
        OUT_W(%dx)

	movl	pLROffset, %eax
	movl	allowance, %edx

	movl	mCount, %ecx
	addl	%ecx, %esi
	addl	%ecx, %edi
pMRa3:
        movsb
        addl    %eax, %esi
        addl    %eax, %edi
        decl    %edx
        jnz     pMRa3

	movw	$0x3c4, %dx
	movw	$0x0f02, %ax
        OUT_W(%dx)

	movl	sM, %esi
	movl	dM, %edi

	movl	pMOffset, %eax
	movl	mCount, %ebx
	movl	allowance, %edx
pMRa2:
	movl	%ebx, %ecx
	rep
	movsb
	addl	%eax, %esi
	addl	%eax, %edi
	decl	%edx
	jnz	pMRa2

	ret
/************************************/
        ALIGNTEXT4
pM:
	movl	pMOffset, %eax
	movl	mCount, %ebx
	movl	allowance, %edx
pM2:
	movl	%ebx, %ecx
	rep
	movsb
	addl	%eax, %esi
	addl	%eax, %edi
	decl	%edx
	jnz	pM2

	ret
/************************************/
        ALIGNTEXT4
pPartial:
	movl	mCount, %ebx
	movl	$0, tmp

	movw	lMask, %ax
	cmpw	$0x0f02, %ax
	je	pPartial7
pPartial9:
	movw	$0x3c4, %dx
        OUT_W(%dx)
	movsb
	
	movw	$0x0f02, %ax
        OUT_W(%dx)
	jmp	pPartial6
        ALIGNTEXT4
pPartial7:
	movw	$0x3c4, %dx
        OUT_W(%dx)
pPartial5:
        movl    vgaReadTop, %eax
        movl    vgaWriteTop, %ecx
        subl    %esi, %eax
        subl    %edi, %ecx

        cmpl    %eax, %ecx
        jc      pPartial1

        movl    %eax, %ecx
pPartial1:
        cmpl    %ebx, %ecx
        jc      pPartial2

        movl    %ebx, %ecx
pPartial2:
        subl    %ecx, %ebx

        rep
        movsb
pPartial6:
        movb    segment, %al
        cmpl    vgaReadTop, %esi
        jc      pPartial3

        subl    $0x10000, %esi
        addb    $16, %al
pPartial3:
        cmpl    vgaWriteTop, %edi
        jc      pPartial4

        subl    $0x10000, %edi
        incb    %al
pPartial4:
        movb    %al, segment
	movw	$0x3cd, %dx
        OUT_B(%dx)

        orl     %ebx, %ebx
        jnz     pPartial5

	cmpl	$1, tmp
	je	pPartial8

	movl	$1, tmp
        movw    rMask, %ax
        cmpw    $0x0f02, %ax
        jne	pPartial9
pPartial8:
        movl    vgaReadTop, %eax
        movl    offset, %eax
        addl    %eax, %esi
        addl    %eax, %edi

        decl	height
	jz	pPartial10
	jmp	*pPartialJump
        ALIGNTEXT4
pPartial10:
        movw    $0x3c4, %dx
        movw    $0x0f02, %ax
        OUT_W(%dx)
        movw    $0x0c04, %ax
        OUT_W(%dx)
        movw    $0x4005, %ax
        movw    $0x3ce, %dx
        OUT_W(%dx)

        popl %ebx
        popl %esi
        popl %edi
        leave
        ret
