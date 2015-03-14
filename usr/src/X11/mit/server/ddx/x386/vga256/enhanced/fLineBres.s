/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/fLineBres.s,v 1.8 1992/08/29 11:09:51 dawes Exp $ */
/* Copyright 1992 by James Tsillas, Arlignton, Massachusetts.

		All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation.

JAMES TSILLAS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
EVENT SHALL DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
*/

#include "assembler.h"

#include "vgaAsm.h"

	.file "fLineBres.s"

#define rop	8(%ebp)
#define andv	%dh
#define xorv	%dl
#define addrl   20(%ebp)
#define nlwidth 24(%ebp)
#define signdx	28(%ebp)
#define signdy	32(%ebp)
#define axis	36(%ebp)
#define x1	40(%ebp)
#define y1	44(%ebp)
#define e	%edi
#define e1	%esi
#define e2	56(%ebp)
#define len     %ecx

#define addrb   %ebx
#define e3	-4(%ebp)
#define tmp	%eax

#define GXcopy	$3
#define Y_AXIS	$1

#if !defined(SYSV) && !defined(SVR4)
#define fastcfbBresS _fastcfbBresS
#endif

	.text
	ALIGNTEXT4
.globl	fastcfbBresS

fastcfbBresS:
	pushl %ebp
	movl %esp,%ebp
	subl $4,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	cmpl $0,60(%ebp)		/* check for zero length first */
	jz .allfinish
	movb 12(%ebp),andv
	movb 16(%ebp),xorv
	movl 48(%ebp),e
	movl 52(%ebp),e1
	movl 60(%ebp),len
	movl e2,tmp
	subl e1,tmp
	movl tmp,e3
	shll $2,nlwidth
	movl nlwidth,tmp
	imull y1,tmp
	addl x1,tmp
	addl addrl,tmp
	movl tmp,addrb
	cmpl $0,signdy
	jge .L1
	negl nlwidth
.L1:	subl e1,e
	cmpl Y_AXIS,axis
	jnz .L2
	pushl nlwidth
	pushl signdx
	popl nlwidth
	popl signdx
.L2:	cmpl GXcopy,rop
	jnz  .LSet

.LCopy: cmpl VGABASE,addrl
	jb .nocheckloopC
	pushl addrb
	call vgaSetWrite
	movl tmp,addrb
	addl $4,%esp

	ALIGNTEXT4
.writeloopC:
        movb xorv,(addrb)
        addl e1,e
        js .L3
        addl nlwidth,addrb
        addl e3,e
.L3:	addl signdx,addrb
	cmpl vgaWriteBottom,addrb
	jbe .L4
	cmpl vgaWriteTop,addrb
	jae .L9
	loop .writeloopC
	jmp .allfinish
.L9:	pushl addrb
        call vgaWriteNext
        movl tmp,addrb
        addl $4,%esp
	loop .writeloopC
	jmp .allfinish
.L4:	pushl addrb
        call vgaWritePrev
        movl tmp,addrb
        addl $4,%esp
	loop .writeloopC
	jmp .allfinish

	ALIGNTEXT4
.nocheckloopC:
        movb xorv,(addrb)
        addl e1,e
        js .L5
        addl nlwidth,addrb
        addl e3,e
.L5:	addl signdx,addrb
	loop .nocheckloopC

	ALIGNTEXT4
.allfinish:
	popl %ebx
	popl %esi
	popl %edi
	addl $4,%esp
	leave
	ret

.LSet:	cmpl VGABASE,addrl
	jb .nocheckloopS
	pushl addrb
	call vgaSetReadWrite
	movl tmp,addrb
	addl $4,%esp

	ALIGNTEXT4
.writeloopS:
	movb (addrb),%al	/* Minimize slow memory access */
	andb andv,%al
        xorb xorv,%al
	movb %al,(addrb)
        addl e1,e
        js .L6
        addl nlwidth,addrb
        addl e3,e
.L6:	addl signdx,addrb
	cmpl vgaWriteBottom,addrb
	jbe .L7
	cmpl vgaWriteTop,addrb
	jae .L10
	loop .writeloopS
	jmp .allfinish
.L10:	pushl addrb
        call vgaReadWriteNext
        movl tmp,addrb
        addl $4,%esp
	loop .writeloopS
	jmp .allfinish
.L7:	pushl addrb
        call vgaReadWritePrev
        movl %eax,addrb
        addl $4,%esp
	loop .writeloopS
	jmp .allfinish

	ALIGNTEXT4
.nocheckloopS:
	movb (addrb),%al	/* Minimize slow memory access */
	andb andv,%al
        xorb xorv,%al
	movb %al,(addrb)
        addl e1,e
        js .L8
        addl nlwidth,addrb
        addl e3,e
.L8:	addl signdx,addrb
	loop .nocheckloopS
	jmp .allfinish


