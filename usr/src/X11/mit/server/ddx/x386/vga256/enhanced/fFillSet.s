/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/fFillSet.s,v 1.8 1992/08/29 11:09:50 dawes Exp $ */
/*
 * Copyright 1990,91 by Thomas Roell, Dinkelscherben, Germany.
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Thomas Roell not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  Thomas Roell makes no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as is" without express or implied warranty.
 *
 * THOMAS ROELL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THOMAS ROELL BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Thomas Roell, roell@informatik.tu-muenchen.de
 *
 * This file was derived from a similar X11R4 file (X386 1.1) for X11R5
 * (X386 1.2) by James Tsillas.  This file was further derived to its current
 * form by David Wexelblat (dwex@mtgzfs3.att.com).
 *
 * /usr/X386/mit/server/ddx/x386/cfb.banked/RCS/fFillSet.s,v 1.0 92/05/01 16:44:22 root Exp
 */

#include "assembler.h"

	.file "fFillSet.s"

/*
 *
 * This routine implements a fast Solid Fill in GXset mode.
 * no segment checking is done.
 *
 * SYNTAX:
 * unchar * fastFillSolidGXset(pdst,fill1,fill2,hcount,count,width,widthPitch);
 * 
 *  (7/27/90 TR)
 *  (4/92 JT)
 *  (5/92 DW)
 */

#define pdst        %ebx
#define lp          %ecx
#define count       %edx
#define hcount      %edi
#define width       28(%ebp)
#define widthPitch  %esi
#define tmp         %eax
#define tmpw        %ax
#define tmpb	    %al
#define fill1       12(%ebp)
#define fill2       16(%ebp)

#if !defined(SYSV) && !defined(SVR4)
#define fastFillSolidGXset _fastFillSolidGXset
#endif

	.text
	ALIGNTEXT4
.globl	fastFillSolidGXset

fastFillSolidGXset:
	pushl %ebp
	movl %esp,%ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),pdst
	movl 20(%ebp),hcount
	movl 24(%ebp),count
	movl 32(%ebp),widthPitch
	orl hcount,hcount
	jz .finish
	orl count,count
	jz .finish
	cmpl $3,count
	jg .blockloop
	je .tribbleloop
	cmpl $2,count
	je .wordloop
/*
 * do a fast vertical line
 */
	ALIGNTEXT4
.byteloop:
	movb fill1,tmpb
	andb (pdst),tmpb
	xorb fill2,tmpb
	movb tmpb,(pdst)
	leal 1(widthPitch,pdst),pdst
	decl hcount
	jnz .byteloop
	jmp .finish

	ALIGNTEXT4
.wordloop:
	movw fill1,tmpw
	andw (pdst),tmpw
	xorw fill2,tmpw
	movw tmpw,(pdst)
	leal 2(widthPitch,pdst),pdst
	decl hcount
	jnz .wordloop
	jmp .finish

	ALIGNTEXT4
.tribbleloop:
	movw fill1,tmpw
	andw (pdst),tmpw
	xorw fill2,tmpw
	movw tmpw,(pdst)
	movb fill1,tmpb
	andb 2(pdst),tmpb
	xorb fill2,tmpb
	movb tmpb,2(pdst)
	leal 3(widthPitch,pdst),pdst
	decl hcount
	jnz .tribbleloop
	jmp .finish

	ALIGNTEXT4
.blockloop:
	testl $1,pdst
	jz .alignword
	movb fill1,tmpb
	andb (pdst),tmpb
	xorb fill2,tmpb
	movb tmpb,(pdst)
	incl pdst
	decl count
.alignword:
	testl $2,pdst
	jz .aligneddword
	movw fill1,tmpw
	andw (pdst),tmpw
	xorw fill2,tmpw
	movw tmpw,(pdst)
	leal 2(pdst),pdst
	leal -2(count),count
.aligneddword:
	movl count,lp
	shrl $5,lp
	jz .fixupdword

	ALIGNTEXT4
.dwordloop:
	movl 0(pdst),tmp
	andl fill1,tmp
	xorl fill2,tmp
	movl tmp,0(pdst)
	movl 4(pdst),tmp
	andl fill1,tmp
	xorl fill2,tmp
	movl tmp,4(pdst)
	movl 8(pdst),tmp
	andl fill1,tmp
	xorl fill2,tmp
	movl tmp,8(pdst)
	movl 12(pdst),tmp
	andl fill1,tmp
	xorl fill2,tmp
	movl tmp,12(pdst)
	movl 16(pdst),tmp
	andl fill1,tmp
	xorl fill2,tmp
	movl tmp,16(pdst)
	movl 20(pdst),tmp
	andl fill1,tmp
	xorl fill2,tmp
	movl tmp,20(pdst)
	movl 24(pdst),tmp
	andl fill1,tmp
	xorl fill2,tmp
	movl tmp,24(pdst)
	movl 28(pdst),tmp
	andl fill1,tmp
	xorl fill2,tmp
	movl tmp,28(pdst)
	leal 32(pdst),pdst
	decl lp
	jnz .dwordloop

.fixupdword:
	movl count,lp
	andl $28,lp
	leal (lp,pdst),pdst
	movl .jumptab1(lp),tmp
	jmp *tmp

	ALIGNTEXT4
.jumptab1: .long .Lnoop, .L0, .L1, .L2, .L3, .L4, .L5, .L6

.L6:	movl fill1,tmp
	andl -28(pdst),tmp
	xorl fill2,tmp
	movl tmp,-28(pdst)
.L5:	movl fill1,tmp
	andl -24(pdst),tmp
	xorl fill2,tmp
	movl tmp,-24(pdst)
.L4:	movl fill1,tmp
	andl -20(pdst),tmp
	xorl fill2,tmp
	movl tmp,-20(pdst)
.L3:	movl fill1,tmp
	andl -16(pdst),tmp
	xorl fill2,tmp
	movl tmp,-16(pdst)
.L2:	movl fill1,tmp
	andl -12(pdst),tmp
	xorl fill2,tmp
	movl tmp,-12(pdst)
.L1:	movl fill1,tmp
	andl -8(pdst),tmp
	xorl fill2,tmp
	movl tmp,-8(pdst)
.L0:	movl fill1,tmp
	andl -4(pdst),tmp
	xorl fill2,tmp
	movl tmp,-4(pdst)
.Lnoop:

	test $2,count
	jz .fixupbyte
	movw fill1,tmpw
	andw (pdst),tmpw
	xorw fill2,tmpw
	movw tmpw,(pdst)
	leal 2(pdst),pdst
.fixupbyte:
	test $1,count
	jz .enditeration
	movb fill1,tmpb
	andb (pdst),tmpb
	xorb fill2,tmpb
	movb tmpb,(pdst)
	incl pdst

.enditeration:
	leal (widthPitch,pdst),pdst
	movl width,count
	decl hcount
	jnz .blockloop

.finish:
	movl pdst,%eax
	leal -12(%ebp),%esp
	popl %ebx
	popl %esi
	popl %edi
	leave
	ret

