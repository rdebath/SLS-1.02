/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/fLineH.s,v 1.8 1992/08/29 11:09:52 dawes Exp $ */
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

	.file "fLineH.s"

#define rop		8(%ebp)
#define andv		%ebx
#define andvb		%bl
#define	andvw		%bx
#define xorv		%edx
#define xorvb		%dl
#define	xorvw		%dx
#define addrl		%edi
#define nlwidth		24(%ebp)
#define	x1		28(%ebp)
#define	y1		32(%ebp)
#define	len		%esi

#define count		%ecx
#define tmp		%eax

#define GXcopy  	$3
#define GXxor		$6

#if !defined(SYSV) && !defined(SVR4)
#define fastcfbHorzS _fastcfbHorzS
#endif

.text
	ALIGNTEXT4
.globl	fastcfbHorzS

fastcfbHorzS:
	pushl		%ebp
	movl		%esp,%ebp
	pushl		%esi
	pushl		%edi
	pushl		%ebx
	movl		16(%ebp),xorv
	movl		20(%ebp),addrl
	movl		36(%ebp),len
	cld
	movl		nlwidth,tmp
	shll		$2,tmp
	imull		y1,tmp
	addl		x1,tmp
	addl		tmp,addrl
	cmpl		VGABASE,addrl
	jb		.noClongL
	pushl		addrl
	call		vgaSetReadWrite
	movl		tmp,addrl
	addl		$4,%esp
	movl		vgaWriteTop,tmp
	subl		addrl,tmp
	cmpl		len,tmp
	jae		.noClongL

.ClongL:
	cmpl		GXcopy,rop
	jne		.XlongL
	cmpl		$2,len
	je		.L14
	jb		.L15
.L13:	testl		$3,addrl
	jz		.L8
	movb		xorvb,(addrl)
	incl		addrl
	decl		len
.L14:	testl		$3,addrl
	jz		.L8
	movb		xorvb,(addrl)
	incl		addrl
	decl		len
.L15:	testl		$3,addrl
	jz		.L8
	movb		xorvb,(addrl)
	incl		addrl
	decl		len
	cmpl		vgaWriteTop,addrl
	jb		.L8
	pushl		addrl
	call		vgaWriteNext
	movl		tmp,addrl
	addl		$4,%esp
.L8:	movl		len,count
	andl		$0xfffffffc,count
	movl		vgaWriteTop,tmp
	subl		addrl,tmp
	cmpl		tmp,count
	jbe		.L16
	movl		tmp,count
.L16:	subl		count,len
	shrl		$2,count
	jz		.L10
	movl		xorv,tmp
	repz
	stosl
	cmpl		$3,len
	jbe		.L17
	pushl		addrl
	call		vgaWriteNext
	movl		tmp,addrl
	addl		$4,%esp
	movl		len,count
	andl		$0xfffffffc,count
	subl		count,len
	shrl		$2,count
	movl		xorv,tmp
	repz
	stosl
.L17:	cmpl		vgaWriteTop,addrl
	jb		.L10
	pushl		addrl
	call		vgaWriteNext
	movl		tmp,addrl
	addl		$4,%esp
.L10:	cmpl		$2,len
	ja		.L11
	je		.L12
	cmpl		$0,len
	je		.allfinish
	movb		xorvb,(addrl)
	jmp		.allfinish
.L11:	movw		xorvw,(addrl)
	movb		xorvb,2(addrl)		/**/
	jmp		.allfinish
.L12:	movw		xorvw,(addrl)		/**/
.allfinish:
	popl		%ebx
	popl		%edi
	popl		%esi
	leave
	ret

.XlongL:
	movl		12(%ebp),andv
	cmpl		GXxor,rop
	jne		.SlongL
	cmpl		$2,len
	je		.LX14
	jb		.LX15
.LX13:	testl		$3,addrl
	jz		.LX8
	xorb		xorvb,(addrl)		/**/
	incl		addrl
	decl		len
.LX14:	testl		$3,addrl
	jz		.LX8
	xorb		xorvb,(addrl)		/**/
	incl		addrl
	decl		len
.LX15:	testl		$3,addrl
	jz		.LX8
	xorb		xorvb,(addrl)		/**/
	incl		addrl
	decl		len
	cmpl		vgaWriteTop,addrl
	jb		.LX8
	pushl		addrl
	call		vgaReadWriteNext
	movl		tmp,addrl
	addl		$4,%esp
.LX8:	movl		len,count
	andl		$0xfffffffc,count
	movl		vgaWriteTop,tmp
	subl		addrl,tmp
	cmpl		tmp,count
	jbe		.LX16
	movl		tmp,count
.LX16:	subl		count,len
	shrl		$2,count
	jz		.LX10
.LX9:	xorl		xorv,(addrl)		/**/
	addl		$4,addrl
	loop		.LX9
	cmpl		$3,len
	jbe		.LX17
	pushl		addrl
	call		vgaReadWriteNext
	movl		tmp,addrl
	addl		$4,%esp
	movl		len,count
	andl		$0xfffffffc,count
	subl		count,len
	shrl		$2,count
.LX18:	xorl		xorv,(addrl)		/**/
	addl		$4,addrl
	loop		.LX18
.LX17:	cmpl		vgaWriteTop,addrl
	jb		.LX10
	pushl		addrl
	call		vgaReadWriteNext
	movl		tmp,addrl
	addl		$4,%esp
.LX10:	cmpl		$2,len
	ja		.LX11
	je		.LX12
	cmpl		$0,len
	je		.allfinish
	xorb		xorvb,(addrl)		/**/
	jmp		.allfinish
.LX11:	xorw		xorvw,(addrl)		/**/
	xorb		xorvb,2(addrl)		/**/
	jmp		.allfinish
.LX12:	xorw		xorvw,(addrl)		/**/
	jmp		.allfinish

.SlongL:
	cmpl		$2,len
	je		.LS14
	jb		.LS15
.LS13:	testl		$3,addrl
	jz		.LS8
	movb		(addrl),%al
	xorb		xorvb,%al		/**/
	andb		andvb,%al
	stosb
	decl		len
.LS14:	testl		$3,addrl
	jz		.LS8
	movb		(addrl),%al
	xorb		xorvb,%al		/**/
	andb		andvb,%al
	stosb
	decl		len
.LS15:	testl		$3,addrl
	jz		.LS8
	movb		(addrl),%al
	xorb		xorvb,%al		/**/
	andb		andvb,%al
	stosb
	decl		len
	cmpl		vgaWriteTop,addrl
	jb		.LS8
	pushl		addrl
	call		vgaReadWriteNext
	movl		tmp,addrl
	addl		$4,%esp
.LS8:	movl		len,count
	andl		$0xfffffffc,count
	movl		vgaWriteTop,tmp
	subl		addrl,tmp
	cmpl		tmp,count
	jbe		.LS16
	movl		tmp,count
.LS16:	subl		count,len
	shrl		$2,count
	jz		.LS10
.LS9:	movl		(addrl),tmp
	xorl		xorv,tmp		/**/
	andl		andv,tmp
	stosl
	loop		.LS9
	cmpl		$3,len
	jbe		.LS17
	pushl		addrl
	call		vgaReadWriteNext
	movl		tmp,addrl
	addl		$4,%esp
	movl		len,count
	andl		$0xfffffffc,count
	subl		count,len
	shrl		$2,count
.LS18:	movl		(addrl),tmp
	xorl		xorv,tmp		/**/
	andl		andv,tmp
	stosl
	loop		.LS18
.LS17:	cmpl		vgaWriteTop,addrl
	jb		.LS10
	pushl		addrl
	call		vgaReadWriteNext
	movl		tmp,addrl
	addl		$4,%esp
.LS10:	cmpl		$2,len
	ja		.LS11
	je		.LS12
	cmpl		$0,len
	je		.allfinish
	movb		(addrl),%al
	xorb		xorvb,%al		/**/
	andb		andvb,%al
	movb		%al,(addrl)
	jmp		.allfinish
.LS11:	movw		(addrl),%ax
	xorw		xorvw,%ax		/**/
	andw		andvw,%ax
	stosw
	movb		(addrl),%al
	xorb		xorvb,%al		/**/
	andb		andvb,%al
	movb		%al,(addrl)
	jmp		.allfinish
.LS12:	movw		(addrl),%ax
	xorw		xorvw,%ax		/**/
	andw		andvw,%ax
	movw		%ax,(addrl)
	jmp		.allfinish

.noClongL:
	cmpl		GXcopy,rop
	jne		.noXlongL
	cmpl		$2,len
	je		.LC6
	jb		.LC7
.LC5:	testl		$3,addrl
	jz		.LC0
	movb		xorvb,(addrl)
	incl		addrl
	decl		len
.LC6:	testl		$3,addrl
	jz		.LC0
	movb		xorvb,(addrl)
	incl		addrl
	decl		len
.LC7:	testl		$3,addrl
	jz		.LC0
	movb		xorvb,(addrl)
	incl		addrl
	decl		len
.LC0:	movl		len,count
	andl		$0xfffffffc,count
	subl		count,len
	shrl		$2,count
	jz		.LC2
	movl		xorv,tmp
	cld
	repz
	stosl
.LC2:	cmpl		$2,len
	ja		.LC3
	je		.LC4
	cmpl		$0,len
	je		.allfinish
	movb		xorvb,(addrl)		/**/
	jmp		.allfinish
.LC3:	movw		xorvw,(addrl)		/**/
	movb		xorvb,2(addrl)		/**/
	jmp		.allfinish
.LC4:	movw		xorvw,(addrl)		/**/
	jmp		.allfinish


.noXlongL:
	movl		12(%ebp),andv
	cmpl		GXxor,rop
	jne		.noSlongL
	cmpl		$2,len
	je		.LX6
	jb		.LX7
.LX5:	testl		$3,addrl
	jz		.LX0
	xorb		xorvb,(addrl)		/**/
	incl		addrl
	decl		len
.LX6:	testl		$3,addrl
	jz		.LX0
	xorb		xorvb,(addrl)		/**/
	incl		addrl
	decl		len
.LX7:	testl		$3,addrl
	jz		.LX0
	xorb		xorvb,(addrl)		/**/
	incl		addrl
	decl		len
.LX0:	movl		len,count
	andl		$0xfffffffc,count
	subl		count,len
	shrl		$2,count
	jz		.LX2
.LX1:	xorl		xorv,(addrl)		/**/
	addl		$4,addrl
	loop		.LX1
.LX2:	cmpl		$2,len
	ja		.LX3
	je		.LX4
	cmpl		$0,len
	je		.allfinish
	xorb		xorvb,(addrl)		/**/
	jmp		.allfinish
.LX3:	xorw		xorvw,(addrl)		/**/
	xorb		xorvb,2(addrl)		/**/
	jmp		.allfinish
.LX4:	xorw		xorvw,(addrl)		/**/
	jmp		.allfinish

.noSlongL:
	cmpl		$2,len
	je		.LS6
	jb		.LS7
.LS5:	testl		$3,addrl
	jz		.LS0
	movb		(addrl),%al
	xorb		xorvb,%al		/**/
	andb		andvb,%al
	stosb
	decl		len
.LS6:	testl		$3,addrl
	jz		.LS0
	movb		(addrl),%al
	xorb		xorvb,%al		/**/
	andb		andvb,%al
	stosb
	decl		len
.LS7:	testl		$3,addrl
	jz		.LS0
	movb		(addrl),%al
	xorb		xorvb,%al		/**/
	andb		andvb,%al
	stosb
	decl		len
.LS0:	movl		len,count
	andl		$0xfffffffc,count
	subl		count,len
	shrl		$2,count
	jz		.LS2
.LS1:	movl		(addrl),tmp
	xorl		xorv,tmp		/**/
	andl		andv,tmp
	stosl
	loop		.LS1
.LS2:	cmpl		$2,len
	ja		.LS3
	je		.LS4
	cmpl		$0,len
	je		.allfinish
	movb		(addrl),%al
	xorb		xorvb,%al		/**/
	andb		andvb,%al
	movb		%al,(addrl)
	jmp		.allfinish
.LS3:	movw		(addrl),%ax
	xorw		xorvw,%ax		/**/
	andw		andvw,%ax
	stosw
	movb		(addrl),%al
	xorb		xorvb,%al		/**/
	andb		andvb,%al
	movb		%al,(addrl)
	jmp		.allfinish
.LS4:	movw		(addrl),%ax
	xorw		xorvw,%ax		/**/
	andw		andvw,%ax
	movw		%ax,(addrl)
	jmp		.allfinish

