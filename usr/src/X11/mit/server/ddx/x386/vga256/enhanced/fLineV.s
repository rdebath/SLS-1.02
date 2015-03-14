/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/fLineV.s,v 1.8 1992/08/29 11:09:53 dawes Exp $ */
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

	.file "fLineV.s"

#define rop		8(%ebp)
#define andv		%bl
#define xorv		%bh
#define	pdst		%edi
#define	nlwidth		%esi
#define x1		28(%ebp)
#define y1		32(%ebp)
#define len		36(%ebp)

#define	len0		%ecx
#define tmp		%eax

#define	GXcopy		$3
#define GXxor		$6

#if !defined(SYSV) && !defined(SVR4)
#define fastcfbVertS _fastcfbVertS
#endif

	.text
.globl	fastcfbVertS
	ALIGNTEXT4
fastcfbVertS:
	pushl	%ebp
	movl	%esp,%ebp
	pushl	%ebx
	pushl	%esi
	pushl	%edi
	movb	12(%ebp),andv
	movb	16(%ebp),xorv
	movl	20(%ebp),pdst
	movl	24(%ebp),nlwidth
	shll	$2,nlwidth
	movl	y1,tmp
	imull	nlwidth,tmp
	addl	x1,tmp
	addl	tmp,pdst
	movl	pdst,20(%ebp)
	cmpl	GXcopy,rop
	jne	.GXxorloop

.GXcopyloop:
	cmpl	VGABASE,pdst
	jb	.nocheckC

	ALIGNTEXT4
.checkC:
	pushl	20(%ebp)
	call	vgaSetWrite
	movl	tmp,pdst
	addl	$4,%esp
	movl	vgaWriteTop,tmp
	subl	pdst,tmp
	xorl	%edx,%edx
	divl	nlwidth
	orl	tmp,tmp
	jz	.L3
	cmpl	len,tmp
	jae	.nocheckC
	movl	tmp,len0
	subl	len0,len
	imull	nlwidth,tmp
	addl	tmp,20(%ebp)
.L1:	movb	xorv,(pdst)
	addl	nlwidth,pdst
	loop	.L1			/* len0 is %ecx */
	cmpl	vgaWriteTop,pdst
	jae	.checkC
.L3:	movb	xorv,(pdst)
	decl	len
	jz	.allfinish
	addl	nlwidth,20(%ebp)
	jmp	.checkC

	ALIGNTEXT4
.nocheckC:
	cmpl	$0,len
	jz	.allfinish
	movl	len,len0
.L2:	movb	xorv,(pdst)
	addl	nlwidth,pdst
	loop	.L2			/* len0 is %ecx */
.allfinish:
	popl	%edi
	popl	%esi
	popl	%ebx
	leave
	ret

.GXxorloop:
	cmpl	GXxor,rop
	jne	.GXsetloop
	cmpl	VGABASE,pdst
	jb	.nocheckX

	ALIGNTEXT4
.checkX:
	pushl	20(%ebp)
	call	vgaSetReadWrite
	movl	tmp,pdst
	addl	$4,%esp
	movl	vgaWriteTop,tmp
	subl	pdst,tmp
	xorl	%edx,%edx
	divl	nlwidth
	orl	tmp,tmp
	jz	.L6
	cmpl	len,tmp
	jae	.nocheckX
	movl	tmp,len0
	subl	len0,len
	imull	nlwidth,tmp
	addl	tmp,20(%ebp)
.L4:	xorb	xorv,(pdst)
	addl	nlwidth,pdst
	loop	.L4			/* len0 is %ecx */
	cmpl	vgaWriteTop,pdst
	jae	.checkX
.L6:	xorb	xorv,(pdst)
	decl	len
	jz	.allfinish
	addl	nlwidth,20(%ebp)
	jmp	.checkX

	ALIGNTEXT4
.nocheckX:
	cmpl	$0,len
	jz	.allfinish
	movl	len,len0
.L5:	xorb	xorv,(pdst)
	addl	nlwidth,pdst
	loop	.L5			/* len0 is %ecx */
	jmp	.allfinish

.GXsetloop:
	cmpl	VGABASE,pdst
	jb	.nocheckS

	ALIGNTEXT4
.checkS:
	pushl	20(%ebp)
	call	vgaSetReadWrite
	movl	tmp,pdst
	addl	$4,%esp
	movl	vgaWriteTop,tmp
	subl	pdst,tmp
	xorl	%edx,%edx
	divl	nlwidth
	orl	tmp,tmp
	jz	.L9
	cmpl	len,tmp
	jae	.nocheckS
	movl	tmp,len0
	subl	len0,len
	imull	nlwidth,tmp
	addl	tmp,20(%ebp)
.L7:	movb	(pdst),%al
	xorb	xorv,%al
	andb	andv,%al
	movb	%al,(pdst)
	addl	nlwidth,pdst
	loop	.L7			/* len0 is %ecx */
	cmpl	vgaWriteTop,pdst
	jae	.checkS
.L9:	movb	(pdst),%al
	xorb	xorv,%al
	andb	andv,%al
	movb	%al,(pdst)
	decl	len
	jz	.allfinish
	addl	nlwidth,20(%ebp)
	jmp	.checkS

	ALIGNTEXT4
.nocheckS:
	cmpl	$0,len
	jz	.allfinish
	movl	len,len0
.L8:	movb	(pdst),%al
	xorb	xorv,%al
	andb	andv,%al
	movb	%al,(pdst)
	addl	nlwidth,pdst
	loop	.L8			/* len0 is %ecx */
	jmp	.allfinish


