/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/suLine.s,v 1.3 1992/08/29 11:10:08 dawes Exp $ */
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
8/18/92
*******************************************************************************/
	.file	"suLine.s"

#include "assembler.h"
#include "vgaAsm.h"

#if !defined(SYSV) && !defined(SVR4)
#define SpeedUpBresS _SpeedUpBresS
#endif

/*  SpeedUpBresS(rop, and, xor, addrl, nlwidth, signdx, signdy, axis,
		 x1, y1, e, e1, e2, len);
*/

#define rop	8(%ebp)
#define AND	12(%ebp)
#define XOR	16(%ebp)
#define addrl   20(%ebp)
#define nlwidth	24(%ebp)
#define signdx	28(%ebp)
#define signdy	32(%ebp)
#define axis	36(%ebp)
#define x1	40(%ebp)
#define y1	44(%ebp)
#define e	48(%ebp)
#define e1	52(%ebp)
#define e2	56(%ebp)
#define len	60(%ebp)
#define GXcopy	$3
#define Y_AXIS	$1

	.data
copyright:
	STRING "Copyright 8/18/1992 by Glenn G. Lai" 

	ALIGNDATA4
speedUpBound1:
	.long	0
speedUpBound2:
	.long	0
jump:
	.long	0
e3:
	.long	0
mix:
	.long	0
allowance:
	.long	0
tmp:
	.long	0
tmp1:
	.long	0
carefulJump:
	.long	0
divisor:
	.long	0
segment:
	.byte	0
/****************************/
	.text
	ALIGNTEXT4
	.globl	SpeedUpBresS
SpeedUpBresS: 
	cmpl 	$0, 56(%esp)
	jz	return

	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx

	movl	e2, %eax
	subl	e1, %eax
	movl	%eax, e3

	movl	nlwidth, %eax
	shll	$2, %eax
	movl	%eax, nlwidth
	movl	%eax, divisor

	imull	y1, %eax
	addl	addrl, %eax
	addl	x1, %eax
	movl	%eax, %edi

	movl	nlwidth, %eax
	movl	%eax, %ebx

	addl	vgaWriteBottom, %eax
	movl	%eax, speedUpBound2

	negl	%ebx
	movl	%ebx, %eax
	addl	vgaWriteTop, %ebx
	movl	%ebx, speedUpBound1

	cmpl	$0, signdy
	jge	goingDown

	movl	%eax, nlwidth
goingDown:
	movl	e, %eax
	subl	e1, %eax
	movl	%eax, e

	subl	VGABASE, %edi
	jc	pixmap
/****************************/
window:
	movl	%edi, %eax
	shrl	$16, %eax
	movb	%al, %ah
	shlb	$4, %ah
	orb	%ah, %al
	movb	%al, segment
	movw	$0x3cd, %dx
	OUT_B(%dx)

	andl	$0xffff, %edi
	addl	vgaWriteBottom, %edi

	movl	e, %ebx
	movl	$downLoop2, carefulJump

	cmpl	$1, signdy
	je	downLoop

	movl	$upLoop2, carefulJump
	jmp	upLoop
/****************************/
        ALIGNTEXT4
upLoop:
	cmpl	speedUpBound2, %edi
        jc	careful
upLoop2:
	movl	%edi, %eax
        subl	speedUpBound2, %eax
        xorl    %edx, %edx
        divl    divisor
	incl	%eax

        movl    len, %ecx
        cmpl    %ecx, %eax
        jc      upLoop1

        movl    %ecx, %eax
upLoop1:
        subl    %eax, %ecx
        movl    %ecx, len

        call    line

        cmpl    $0, len
        jnz     upLoop
done:
        popl    %ebx
        popl    %esi
        popl    %edi
        leave
return:
        ret
/****************************/
        ALIGNTEXT4
downLoop:
        cmpl    speedUpBound1, %edi
	jnc	careful
downLoop2:
        movl    speedUpBound1, %eax
        subl    %edi, %eax
        xorl    %edx, %edx
        divl	divisor
        incl    %eax

        movl    len, %ecx
        cmpl    %ecx, %eax
        jc      downLoop1

        movl    %ecx, %eax
downLoop1:
        subl    %eax, %ecx
        movl    %ecx, len

        call    line

        cmpl    $0, len
        jnz     downLoop

        popl    %ebx
        popl    %esi
        popl    %edi
        leave
        ret
/****************************/
        ALIGNTEXT4
line:
	movl	e1, %ecx
	movl	nlwidth, %esi

	cmpl	GXcopy, rop
	jne	wMix
/****************************/
wSet:
	movl	%eax, %edx
	movl	XOR, %eax
	cmpl	Y_AXIS, axis
	je	setY
/****************************/
setX:
	cmpl	$1, signdx
	jne	setXLeft
	jmp	setXRight
/****************************/
        ALIGNTEXT4
setXRight:
	movb	%al, (%edi)
        addl    %ecx, %ebx
        jl	setXRight1

        addl    %esi, %edi
        addl    e3, %ebx
setXRight1:
	incl	%edi
	decl	%edx
	jnz	setXRight
	ret
/****************************/
        ALIGNTEXT4
setXLeft:
	movb	%al, (%edi)
        addl    %ecx, %ebx
        jl	setXLeft1

        addl    %esi, %edi
        addl    e3, %ebx
setXLeft1:
	decl	%edi
	decl	%edx
	jnz	setXLeft
	ret
/****************************/
        ALIGNTEXT4
setY:
	cmpl	$1, signdx
	jne	setYLeft
	jmp	setYRight
/****************************/
        ALIGNTEXT4
setYRight:
	movb	%al, (%edi)
        addl    %ecx, %ebx
        jl	setYRight1

	incl	%edi
        addl    e3, %ebx
setYRight1:
	addl	%esi, %edi
	decl	%edx
	jnz	setYRight
	ret
/****************************/
        ALIGNTEXT4
setYLeft:
	movb	%al, (%edi)
        addl    %ecx, %ebx
        jl	setYLeft1

	decl	%edi
        addl    e3, %ebx
setYLeft1:
	addl	%esi, %edi
	decl	%edx
	jnz	setYLeft
	ret
/****************************/
        ALIGNTEXT4
wMix:
	movl	%eax, allowance
	movl	XOR, %eax
	movl	AND, %edx
	movb	%al, %dl
	movl	%edx, mix

        cmpl    Y_AXIS, axis
        je      mixY
/****************************/
mixX:
        cmpl    $1, signdx
        jne     mixXLeft
        jmp     mixXRight
/****************************/
        ALIGNTEXT4
mixXRight:
	movb	(%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
        movb    %al, (%edi)

        addl    %ecx, %ebx
        jl      mixXRight1

        addl    %esi, %edi
        addl    e3, %ebx
mixXRight1:
        incl    %edi
	decl	allowance
        jnz     mixXRight
        ret
/****************************/
        ALIGNTEXT4
mixXLeft:
	movb	(%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
        movb    %al, (%edi)

        addl    %ecx, %ebx
        jl      mixXLeft1

        addl    %esi, %edi
        addl    e3, %ebx
mixXLeft1:
        decl    %edi
        decl    allowance
        jnz     mixXLeft
        ret
/****************************/
        ALIGNTEXT4
mixY:
        cmpl    $1, signdx
        jne     mixYLeft
        jmp     mixYRight
/****************************/
        ALIGNTEXT4
mixYRight:
	movb	(%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
        movb    %al, (%edi)

        addl    %ecx, %ebx
        jl      mixYRight1

        incl    %edi
        addl    e3, %ebx
mixYRight1:
        addl    %esi, %edi
        decl    allowance
        jnz     mixYRight
        ret
/****************************/
        ALIGNTEXT4
mixYLeft:
	movb	(%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
        movb    %al, (%edi)

        addl    %ecx, %ebx
        jl      mixYLeft1

        decl    %edi
        addl    e3, %ebx
mixYLeft1:
        addl    %esi, %edi
        decl    allowance
        jnz     mixYLeft
        ret
/****************************/
        ALIGNTEXT4
careful:
	movl	XOR, %eax
        movl    e1, %ecx
	movl	$0, tmp

	cmpl	Y_AXIS, axis
	je	carefulY
/****************************/
carefulX:
	movl	signdx, %esi

	cmpl	GXcopy, rop
	jne	cXMix0

	movl	len, %edx
	movl	$cXSet2, jump
	jmp	cXSet
/****************************/
        ALIGNTEXT4
cXSet:
	movb	%al, (%edi)
	addl	%ecx, %ebx
	jl	cXSet1

	incl	tmp
	addl	nlwidth, %edi
	addl	e3, %ebx
cXSet1:
	addl	%esi, %edi

	cmpl	vgaWriteTop, %edi
	jnc	next

	cmpl	vgaWriteBottom, %edi
	jc	prev
cXSet2:
	decl	%edx
	jz	done

	cmpl	$2, tmp
	jne	cXSet

	movl	%edx, len
	jmp	*carefulJump
/****************************/
        ALIGNTEXT4
cXMix0:
	movl	AND, %edx
	movb	%al, %dl
	movl	%edx, mix
	movl	$cXMix2, jump
cXMix:
        movb    (%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
        movb    %al, (%edi)

        addl    %ecx, %ebx
        jl      cXMix1

        incl    tmp
        addl    nlwidth, %edi
        addl    e3, %ebx
cXMix1:
        addl    %esi, %edi

        cmpl    vgaWriteTop, %edi
        jnc     next

        cmpl    vgaWriteBottom, %edi
        jc      prev
cXMix2:
        decl    len
        jz      done

        cmpl    $2, tmp
        jne     cXMix

        jmp     *carefulJump
/****************************/
        ALIGNTEXT4
carefulY:
	movl	nlwidth, %esi

	cmpl	GXcopy, rop
	jne	cYMix0

	movl	len, %edx
        movl    $cYSet2, jump
	jmp	cYSet
/****************************/
        ALIGNTEXT4
cYSet:
	movb	%al, (%edi)
	addl	%ecx, %ebx
	jl	cYSet1

	addl	signdx, %edi
	addl	e3, %ebx
cYSet1:
	addl	%esi, %edi
	incl	tmp

	cmpl	vgaWriteTop, %edi
	jnc	next

	cmpl	vgaWriteBottom, %edi
	jc	prev
cYSet2:
	decl	%edx
	jz	done

	cmpl	$2, tmp
	jne	cYSet

	movl	%edx, len
	jmp	*carefulJump
/****************************/
        ALIGNTEXT4
cYMix0:
	movl	AND, %edx
	movb	%al, %dl
	movl	%edx, mix
        movl    $cYMix2, jump
cYMix:
        movb    (%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
        movb    %al, (%edi)

        addl    %ecx, %ebx
        jl      cYMix1

        addl    signdx, %edi
        addl    e3, %ebx
cYMix1:
        addl    %esi, %edi
        incl    tmp

        cmpl    vgaWriteTop, %edi
        jnc     next

        cmpl    vgaWriteBottom, %edi
        jc      prev
cYMix2:
        decl    len
        jz      done

        cmpl    $2, tmp
        jne     cYMix

        jmp     *carefulJump
/****************************/
        ALIGNTEXT4
next:
	movl	%edx, tmp1
        subl    $0x10000, %edi
        movb    segment, %al
        addb    $17, %al
        movb    %al, segment
        movw    $0x3cd, %dx
        OUT_B(%dx)
        movl    XOR, %eax
        movl    tmp1, %edx
        jmp     *jump
/****************************/
        ALIGNTEXT4
prev:
	movl	%edx, tmp1
        addl    $0x10000, %edi
        movb    segment, %al
        subb    $17, %al
        movb    %al, segment
        movw    $0x3cd, %dx
        OUT_B(%dx)
        movl    XOR, %eax
        movl    tmp1, %edx
        jmp     *jump
/****************************/
        ALIGNTEXT4
pixmap:
	cmpl	Y_AXIS, axis
	jne	pixmap1

	movl	nlwidth, %eax
	movl	signdx, %ebx
	movl	%ebx, nlwidth
	movl	%eax, signdx
pixmap1:
	addl	VGABASE, %edi
	movl	len, %ecx
	cmpl	GXcopy, rop
	jne	mmLine2
mmLine1:
	movl	%ecx, %eax
	andl	$3, %eax
	movl	fsTable(,%eax,4), %eax
	movl	%eax, jump 
	movl	XOR, %eax
	movl	e, %ebx
	movl	e1, %edx
	movl	signdx, %esi
	shrl	$2, %ecx
	jz	fs1
fs6:
	movb	%al, (%edi)
	addl	%edx, %ebx
	jl	fs2
	addl	nlwidth, %edi
	addl	e3, %ebx
fs2:
	addl	%esi, %edi
	movb	%al, (%edi)
	addl	%edx, %ebx
	jl	fs3
	addl	nlwidth, %edi
	addl	e3, %ebx
fs3:
	addl	%esi, %edi
	movb	%al, (%edi)
	addl	%edx, %ebx
	jl	fs4
	addl	nlwidth, %edi
	addl	e3, %ebx
fs4:
	addl	%esi, %edi
	movb	%al, (%edi)
	addl	%edx, %ebx
	jl	fs5
	addl	nlwidth, %edi
	addl	e3, %ebx
fs5:
	addl	%esi, %edi
	decl	%ecx
	jnz	fs6
fs1:
	jmp	*jump
	.data
        ALIGNDATA4
fsTable:
	.long	done, fs8, fs9, fs10
	.text
	ALIGNTEXT4
fs10:
	movb	%al, (%edi)
	addl	%edx, %ebx
	jl	fs11
	addl	nlwidth, %edi
	addl	e3, %ebx
fs11:
	addl	%esi, %edi
fs9:
	movb	%al, (%edi)
	addl	%edx, %ebx
	jl	fs12
	addl	nlwidth, %edi
	addl	e3, %ebx
fs12:
	addl	%esi, %edi
fs8:
	movb	%al, (%edi)
	jmp	done
/****************************/
        ALIGNTEXT4
mmLine2:
	movl	%ecx, %eax
	andl	$3, %eax
	movl	fmTable(,%eax,4), %eax
	movl	%eax, jump
	movl	XOR, %eax
	movl	AND, %edx
	movb	%al, %dl
	movl	e, %ebx
	movl	signdx, %esi
	shrl	$2, %ecx
	jz	fm1
fm6:
	movb	(%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
	movb	%al, (%edi)
	addl	e1, %ebx
	jl	fm2
	addl	nlwidth, %edi
	addl	e3, %ebx
fm2:
	addl	%esi, %edi
	movb	(%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
	movb	%al, (%edi)
	addl	e1, %ebx
	jl	fm3
	addl	nlwidth, %edi
	addl	e3, %ebx
fm3:
	addl	%esi, %edi
	movb	(%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
	movb	%al, (%edi)
	addl	e1, %ebx
	jl	fm4
	addl	nlwidth, %edi
	addl	e3, %ebx
fm4:
	addl	%esi, %edi
	movb	(%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
	movb	%al, (%edi)
	addl	e1, %ebx
	jl	fm5
	addl	nlwidth, %edi
	addl	e3, %ebx
fm5:
	addl	%esi, %edi
	decl	%ecx
	jnz	fm6
fm1:
	jmp	*jump
	.data
        ALIGNDATA4
fmTable:
	.long	done, fm8, fm9, fm10
	.text
        ALIGNTEXT4
fm10:
	movb	(%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
	movb	%al, (%edi)
	addl	e1, %ebx
	jl	fm11
	addl	nlwidth, %edi
	addl	e3, %ebx
fm11:
	addl	%esi, %edi
fm9:
	movb	(%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
	movb	%al, (%edi)
	addl	e1, %ebx
	jl	fm12
	addl	nlwidth, %edi
	addl	e3, %ebx
fm12:
	addl	%esi, %edi
fm8:
	movb	(%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
	movb	%al, (%edi)
	jmp	done
