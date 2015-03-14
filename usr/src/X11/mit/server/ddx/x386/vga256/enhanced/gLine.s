/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/gLine.s,v 1.10 1992/08/29 11:10:00 dawes Exp $ */
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
5/20/92
*******************************************************************************/
/*  speedupcfbBresS (rop, and, xor, addrl, nlwidth, signdx, signdy, axis,
		     x1, y1, e, e1, e2, len);
*/

#include "assembler.h"

#include "vgaAsm.h"

	.file "gLine.s"

#if !defined(SYSV) && !defined(SVR4)
#define speedupcfbBresS _speedupcfbBresS
#endif

	.data
.copyright:
	STRING	"Copyright 1992 by Glenn G. Lai" 

#define rop	8(%ebp)
#define and	12(%ebp)
#define xor	16(%ebp)
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

	ALIGNDATA4
e3:
	.long	0
.draw:
	.long	0
.switchBanks:
	.long	0
.jump1:
	.long	0
.segment:
	.byte	0
	
	.text
	ALIGNTEXT4
	.globl	speedupcfbBresS
/****************************************/
speedupcfbBresS:
	cmpl 	$0,56(%esp)
	jz	.return

	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx

	movl	e2, %eax
	subl	e1, %eax
	movl	%eax, e3

	shll	$2, nlwidth
	movl	nlwidth, %eax
	imull	y1, %eax
	addl	addrl, %eax
	addl	x1, %eax
	movl	%eax, %edi

	movl	e, %eax
	subl	e1, %eax
	movl	%eax, e

	cmpl	$0, signdy
	jge	.g1
	negl	nlwidth
.g1:
	cmpl	Y_AXIS, axis
	jne	.g2
	movl	nlwidth, %eax
	movl	signdx, %ebx
	movl	%ebx, nlwidth
	movl	%eax, signdx
/****************************************/
.g2:
	subl	VGABASE, %edi
	jnc	.window

	addl	VGABASE, %edi
	movl	len, %ecx
	cmpl	GXcopy, rop
	jne	.mmLine2
.mmLine1:
	call	.freelySet
	jmp	.done
	ALIGNTEXT4
.mmLine2:
	call	.freelyMix
	jmp	.done
/****************************************/
	ALIGNTEXT4
.window:
	movl	%edi, %eax
	andl	$0x000f0000, %eax
	shrl	$16, %eax
	movb	%al, %ah
	shlb	$4, %ah
	orb	%ah, %al
	movb	%al, .segment
	movl	$0x3cd, %edx
	OUT_B	(%dx)
	andl	$0x0000ffff, %edi
	addl	vgaWriteBottom, %edi

	cmpl	Y_AXIS, axis
	je	.yDominant

	cmpl	$1, signdy
	je	.xxDown
/****************************************/
	ALIGNTEXT4
.xxUp:
	movl	len, %ecx
	movl	$0x3cd, %edx
	movl	e, %esi

	cmpl	GXcopy, rop
	jne	.xxUp2
/****************************************/
.xxUp1:
	movl	%ecx, %eax
	andl	$3, %eax
	shll	$2, %eax
	movl	.xxUp1Table(%eax), %ebx
	movl	%ebx, .jump1
	movl	xor, %eax
	movl	e1, %ebx
	shrl	$2, %ecx
	jz	.xxUp11
.xxUp16:
	movb	%al, (%edi)
	addl	%ebx, %esi
	jl	.xxUp12
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteBottom, %edi
	jnc	.xxUp12
	addl	$0x10000, %edi
	movb	.segment, %al
	decb	%al
	movb	%al, .segment
	OUT_B	(%dx)
	movb	%ah, %al
.xxUp12:
	addl	signdx, %edi
	movb	%al, (%edi)
	addl	%ebx, %esi
	jl	.xxUp13
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteBottom, %edi
	jnc	.xxUp13
	addl	$0x10000, %edi
	movb	.segment, %al
	decb	%al
	movb	%al, .segment
	OUT_B	(%dx)
	movb	%ah, %al
.xxUp13:
	addl	signdx, %edi
	movb	%al, (%edi)
	addl	%ebx, %esi
	jl	.xxUp14
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteBottom, %edi
	jnc	.xxUp14
	addl	$0x10000, %edi
	movb	.segment, %al
	decb	%al
	movb	%al, .segment
	OUT_B	(%dx)
	movb	%ah, %al
.xxUp14:
	addl	signdx, %edi
	movb	%al, (%edi)
	addl	%ebx, %esi
	jl	.xxUp15
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteBottom, %edi
	jnc	.xxUp15
	addl	$0x10000, %edi
	movb	.segment, %al
	decb	%al
	movb	%al, .segment
	OUT_B	(%dx)
	movb	%ah, %al
.xxUp15:
	addl	signdx, %edi
	decl	%ecx
	jnz	.xxUp16
.xxUp11:
	jmp	*.jump1
	ALIGNTEXT4
.xxUp1Table:
	.long	.done, .xxUp101, .xxUp102, .xxUp103
.xxUp103:
	movb	%al, (%edi)
	addl	%ebx, %esi
	jl	.xxUp1031
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteBottom, %edi
	jnc	.xxUp1031
	addl	$0x10000, %edi
	movb	.segment, %al
	decb	%al
	movb	%al, .segment
	OUT_B	(%dx)
	movb	%ah, %al
.xxUp1031:
	addl	signdx, %edi
.xxUp102:
	movb	%al, (%edi)
	addl	%ebx, %esi
	jl	.xxUp1021
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteBottom, %edi
	jnc	.xxUp1021
	addl	$0x10000, %edi
	movb	.segment, %al
	decb	%al
	movb	%al, .segment
	OUT_B	(%dx)
	movb	%ah, %al
.xxUp1021:
	addl	signdx, %edi
.xxUp101:
	movb	%al, (%edi)
	jmp	.done
/****************************************/
	ALIGNTEXT4
.xxUp2:
	movl	%ecx, %eax
	andl	$3, %eax
	shll	$2, %eax
	movl	.xxUp2Table(%eax), %ebx
	movl	%ebx, .jump1
	movl	xor, %eax
	movl	and, %ebx
	movb	%al, %bl
	movb	.segment, %ah
	shrl	$2, %ecx
	jz	.xxUp21
.xxUp26:
	movb	(%edi), %al
	andb	%bh, %al
	xorb	%bl, %al
	movb	%al, (%edi)
	addl	e1, %esi
	jl	.xxUp22
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteBottom, %edi
	jnc	.xxUp22
	addl	$0x10000, %edi
	subb	$0x11, %ah
	movb	%ah, %al
	OUT_B	(%dx)
.xxUp22:
	addl	signdx, %edi
	movb	(%edi), %al
	andb	%bh, %al
	xorb	%bl, %al
	movb	%al, (%edi)
	addl	e1, %esi
	jl	.xxUp23
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteBottom, %edi
	jnc	.xxUp23
	addl	$0x10000, %edi
	subb	$0x11, %ah
	movb	%ah, %al
	OUT_B	(%dx)
.xxUp23:
	addl	signdx, %edi
	movb	(%edi), %al
	andb	%bh, %al
	xorb	%bl, %al
	movb	%al, (%edi)
	addl	e1, %esi
	jl	.xxUp24
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteBottom, %edi
	jnc	.xxUp24
	addl	$0x10000, %edi
	subb	$0x11, %ah
	movb	%ah, %al
	OUT_B	(%dx)
.xxUp24:
	addl	signdx, %edi
	movb	(%edi), %al
	andb	%bh, %al
	xorb	%bl, %al
	movb	%al, (%edi)
	addl	e1, %esi
	jl	.xxUp25
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteBottom, %edi
	jnc	.xxUp25
	addl	$0x10000, %edi
	subb	$0x11, %ah
	movb	%ah, %al
	OUT_B	(%dx)
.xxUp25:
	addl	signdx, %edi
	decl	%ecx
	jnz	.xxUp26
.xxUp21:
	jmp	*.jump1
.xxUp2Table:
	.long	.done, .xxUp201, .xxUp202, .xxUp203
.xxUp203:
	movb	(%edi), %al
	andb	%bh, %al
	xorb	%bl, %al
	movb	%al, (%edi)
	addl	e1, %esi
	jl	.xxUp2031
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteBottom, %edi
	jnc	.xxUp2031
	addl	$0x10000, %edi
	subb	$0x11, %ah
	movb	%ah, %al
	OUT_B	(%dx)
.xxUp2031:
	addl	signdx, %edi
.xxUp202:
	movb	(%edi), %al
	andb	%bh, %al
	xorb	%bl, %al
	movb	%al, (%edi)
	addl	e1, %esi
	jl	.xxUp2021
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteBottom, %edi
	jnc	.xxUp2021
	addl	$0x10000, %edi
	subb	$0x11, %ah
	movb	%ah, %al
	OUT_B	(%dx)
.xxUp2021:
	addl	signdx, %edi
.xxUp201:
	movb	(%edi), %al
	andb	%bh, %al
	xorb	%bl, %al
	movb	%al, (%edi)
	jmp	.done
/****************************************/
	ALIGNTEXT4
.xxDown:
	movl	len, %ecx
	movl	$0x3cd, %edx
	movl	e, %esi

	cmpl	GXcopy, rop
	jne	.xxDown2
/****************************************/
.xxDown1:
	movl	%ecx, %eax
	andl	$3, %eax
	shll	$2, %eax
	movl	.xxDown1Table(%eax), %ebx
	movl	%ebx, .jump1
	movl	xor, %eax
	movl	e1, %ebx
	shrl	$2, %ecx
	jz	.xxDown11
.xxDown16:
	movb	%al, (%edi)
	addl	%ebx, %esi
	jl	.xxDown12
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteTop, %edi
	jc	.xxDown12
	subl	$0x10000, %edi
	movb	.segment, %al
	incb	%al
	movb	%al, .segment
	OUT_B	(%dx)
	movb	%ah, %al
.xxDown12:
	addl	signdx, %edi
	movb	%al, (%edi)
	addl	%ebx, %esi
	jl	.xxDown13
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteTop, %edi
	jc	.xxDown13
	subl	$0x10000, %edi
	movb	.segment, %al
	incb	%al
	movb	%al, .segment
	OUT_B	(%dx)
	movb	%ah, %al
.xxDown13:
	addl	signdx, %edi
	movb	%al, (%edi)
	addl	%ebx, %esi
	jl	.xxDown14
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteTop, %edi
	jc	.xxDown14
	subl	$0x10000, %edi
	movb	.segment, %al
	incb	%al
	movb	%al, .segment
	OUT_B	(%dx)
	movb	%ah, %al
.xxDown14:
	addl	signdx, %edi
	movb	%al, (%edi)
	addl	%ebx, %esi
	jl	.xxDown15
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteTop, %edi
	jc	.xxDown15
	subl	$0x10000, %edi
	movb	.segment, %al
	incb	%al
	movb	%al, .segment
	OUT_B	(%dx)
	movb	%ah, %al
.xxDown15:
	addl	signdx, %edi
	decl	%ecx
	jnz	.xxDown16
.xxDown11:
	jmp	*.jump1
	ALIGNTEXT4
.xxDown1Table:
	.long	.done, .xxDown101, .xxUp102, .xxUp103
.xxDown103:
	movb	%al, (%edi)
	addl	%ebx, %esi
	jl	.xxDown1031
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteTop, %edi
	jc	.xxDown1031
	subl	$0x10000, %edi
	movb	.segment, %al
	incb	%al
	movb	%al, .segment
	OUT_B	(%dx)
	movb	%ah, %al
.xxDown1031:
	addl	signdx, %edi
.xxDown102:
	movb	%al, (%edi)
	addl	%ebx, %esi
	jl	.xxDown1021
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteTop, %edi
	jc	.xxDown1021
	subl	$0x10000, %edi
	movb	.segment, %al
	incb	%al
	movb	%al, .segment
	OUT_B	(%dx)
	movb	%ah, %al
.xxDown1021:
	addl	signdx, %edi
.xxDown101:
	movb	%al, (%edi)
	jmp	.done
/****************************************/
	ALIGNTEXT4
.xxDown2:
	movl	%ecx, %eax
	andl	$3, %eax
	shll	$2, %eax
	movl	.xxDown2Table(%eax), %ebx
	movl	%ebx, .jump1
	movl	xor, %eax
	movl	and, %ebx
	movb	%al, %bl
	movb	.segment, %ah
	shrl	$2, %ecx
	jz	.xxDown21
.xxDown26:
	movb	(%edi), %al
	andb	%bh, %al
	xorb	%bl, %al
	movb	%al, (%edi)
	addl	e1, %esi
	jl	.xxDown22
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteTop, %edi
	jc	.xxDown22
	subl	$0x10000, %edi
	addb	$0x11, %ah
	movb	%ah, %al
	OUT_B	(%dx)
.xxDown22:
	addl	signdx, %edi
	movb	(%edi), %al
	andb	%bh, %al
	xorb	%bl, %al
	movb	%al, (%edi)
	addl	e1, %esi
	jl	.xxDown23
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteTop, %edi
	jc	.xxDown23
	subl	$0x10000, %edi
	addb	$0x11, %ah
	movb	%ah, %al
	OUT_B	(%dx)
.xxDown23:
	addl	signdx, %edi
	movb	(%edi), %al
	andb	%bh, %al
	xorb	%bl, %al
	movb	%al, (%edi)
	addl	e1, %esi
	jl	.xxDown24
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteTop, %edi
	jc	.xxDown24
	subl	$0x10000, %edi
	addb	$0x11, %ah
	movb	%ah, %al
	OUT_B	(%dx)
.xxDown24:
	addl	signdx, %edi
	movb	(%edi), %al
	andb	%bh, %al
	xorb	%bl, %al
	movb	%al, (%edi)
	addl	e1, %esi
	jl	.xxDown25
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteTop, %edi
	jc	.xxDown25
	subl	$0x10000, %edi
	addb	$0x11, %ah
	movb	%ah, %al
	OUT_B	(%dx)
.xxDown25:
	addl	signdx, %edi
	decl	%ecx
	jnz	.xxDown26
.xxDown21:
	jmp	*.jump1
.xxDown2Table:
	.long	.done, .xxDown201, .xxUp202, .xxUp203
.xxDown203:
	movb	(%edi), %al
	andb	%bh, %al
	xorb	%bl, %al
	movb	%al, (%edi)
	addl	e1, %esi
	jl	.xxDown2031
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteTop, %edi
	jc	.xxDown2031
	subl	$0x10000, %edi
	addb	$0x11, %ah
	movb	%ah, %al
	OUT_B	(%dx)
.xxDown2031:
	addl	signdx, %edi
.xxDown202:
	movb	(%edi), %al
	andb	%bh, %al
	xorb	%bl, %al
	movb	%al, (%edi)
	addl	e1, %esi
	jl	.xxDown2021
	addl	nlwidth, %edi
	addl	e3, %esi
	cmpl	vgaWriteTop, %edi
	jc	.xxDown2021
	subl	$0x10000, %edi
	addb	$0x11, %ah
	movb	%ah, %al
	OUT_B	(%dx)
.xxDown2021:
	addl	signdx, %edi
.xxDown201:
	movb	(%edi), %al
	andb	%bh, %al
	xorb	%bl, %al
	movb	%al, (%edi)
	jmp	.done
/****************************************/
	ALIGNTEXT4
.yTable:
	.long	.freelySet, .freelyMix
.yDominant:
	movl	$.yTable, %ebx
	cmpl	$1, signdy 
	je	.yDown
/****************************************/
.yUp:
	movl	$.prevW, .switchBanks
	movl	%edi, %edx
	subl	vgaWriteBottom, %edx
	andl	$0x0000fc00, %edx
	shrl	$10, %edx
	incl	%edx
	jmp	.y0
	ALIGNTEXT4
.yDown:
	movl	$.nextW, .switchBanks
	movl	$64, %edx
	movl	%edi, %eax
	subl	vgaWriteBottom, %eax
	andl	$0x0000fc00, %eax
	shrl	$10, %eax
	subl	%eax, %edx 
.y0:
        cmpl    GXcopy, rop
        je	.y03
	addl	$4, %ebx
.y03:
	movl	(%ebx), %eax
	movl	%eax, .draw
/****************************************/
	movl	len, %ebx
	ALIGNTEXT4
.y12:
	movl	%edx, %ecx
	cmpl	%ebx, %ecx
	jc	.y11
	movl	%ebx, %ecx
.y11:
	subl	%ecx, %ebx
	movl	%ebx, len
	call	*.draw
	movl	len, %ebx
	andl	%ebx, %ebx
	jz	.done
	call	*.switchBanks
	jmp	.y12
/****************************************/
	ALIGNTEXT4	
.freelySet:
	movl	%ecx, %eax
	andl	$3, %eax
	shll	$2, %eax
	movl	.fsTable(%eax), %ebx
	movl	%ebx, .jump1 
	movl	xor, %eax
	movl	e, %ebx
	movl	e1, %edx
	movl	signdx, %esi
	shrl	$2, %ecx
	jz	.fs1
.fs6:
	movb	%al, (%edi)
	addl	%edx, %ebx
	jl	.fs2
	addl	nlwidth, %edi
	addl	e3, %ebx
.fs2:
	addl	%esi, %edi
	movb	%al, (%edi)
	addl	%edx, %ebx
	jl	.fs3
	addl	nlwidth, %edi
	addl	e3, %ebx
.fs3:
	addl	%esi, %edi
	movb	%al, (%edi)
	addl	%edx, %ebx
	jl	.fs4
	addl	nlwidth, %edi
	addl	e3, %ebx
.fs4:
	addl	%esi, %edi
	movb	%al, (%edi)
	addl	%edx, %ebx
	jl	.fs5
	addl	nlwidth, %edi
	addl	e3, %ebx
.fs5:
	addl	%esi, %edi
	decl	%ecx
	jnz	.fs6
.fs1:
	jmp	*.jump1
	ALIGNTEXT4
.fsTable:
	.long	.fs7, .fs8, .fs9, .fs10
.fs10:
	movb	%al, (%edi)
	addl	%edx, %ebx
	jl	.fs11
	addl	nlwidth, %edi
	addl	e3, %ebx
.fs11:
	addl	%esi, %edi
.fs9:
	movb	%al, (%edi)
	addl	%edx, %ebx
	jl	.fs12
	addl	nlwidth, %edi
	addl	e3, %ebx
.fs12:
	addl	%esi, %edi
.fs8:
	movb	%al, (%edi)
	addl	%edx, %ebx
	jl	.fs13
	addl	nlwidth, %edi
	addl	e3, %ebx
.fs13:
	addl	%esi, %edi
.fs7:
	movl	%ebx, e
	ret
/****************************************/
	ALIGNTEXT4
.freelyMix:
	movl	%ecx, %eax
	andl	$3, %eax
	shll	$2, %eax
	movl	.fmTable(%eax), %ebx
	movl	%ebx, .jump1
	movl	xor, %eax
	movl	and, %edx
	movb	%al, %dl
	movl	e, %ebx
	movl	signdx, %esi
	shrl	$2, %ecx
	jz	.fm1
.fm6:
	movb	(%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
	movb	%al, (%edi)
	addl	e1, %ebx
	jl	.fm2
	addl	nlwidth, %edi
	addl	e3, %ebx
.fm2:
	addl	%esi, %edi
	movb	(%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
	movb	%al, (%edi)
	addl	e1, %ebx
	jl	.fm3
	addl	nlwidth, %edi
	addl	e3, %ebx
.fm3:
	addl	%esi, %edi
	movb	(%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
	movb	%al, (%edi)
	addl	e1, %ebx
	jl	.fm4
	addl	nlwidth, %edi
	addl	e3, %ebx
.fm4:
	addl	%esi, %edi
	movb	(%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
	movb	%al, (%edi)
	addl	e1, %ebx
	jl	.fm5
	addl	nlwidth, %edi
	addl	e3, %ebx
.fm5:
	addl	%esi, %edi
	decl	%ecx
	jnz	.fm6
.fm1:
	jmp	*.jump1
	ALIGNTEXT4
.fmTable:
	.long	.fm7, .fm8, .fm9, .fm10
.fm10:
	movb	(%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
	movb	%al, (%edi)
	addl	e1, %ebx
	jl	.fm11
	addl	nlwidth, %edi
	addl	e3, %ebx
.fm11:
	addl	%esi, %edi
.fm9:
	movb	(%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
	movb	%al, (%edi)
	addl	e1, %ebx
	jl	.fm12
	addl	nlwidth, %edi
	addl	e3, %ebx
.fm12:
	addl	%esi, %edi
.fm8:
	movb	(%edi), %al
	andb	%dh, %al
	xorb	%dl, %al
	movb	%al, (%edi)
	addl	e1, %ebx
	jl	.fm13
	addl	nlwidth, %edi
	addl	e3, %ebx
.fm13:
	addl	%esi, %edi
.fm7:
	movl	%ebx, e
	ret
/****************************************/
	ALIGNTEXT4
.nextW:
	subl	$0x10000, %edi
	movb	.segment, %al
	addb	$0x11, %al
	movb	%al, .segment
	movl	$0x3cd, %edx
	OUT_B	(%dx)
	movl	$64, %edx
	ret	
/****************************************/
	ALIGNTEXT4
.prevW:
	addl	$0x10000, %edi
	movb	.segment, %al
	subb	$0x11, %al
	movb	%al, .segment
	movl	$0x3cd, %edx
	OUT_B	(%dx)
	movl	$64, %edx
	ret	
/****************************************/
	ALIGNTEXT4
.done:
	popl	%ebx
	popl	%esi
	popl	%edi
	leave
.return:
	ret
