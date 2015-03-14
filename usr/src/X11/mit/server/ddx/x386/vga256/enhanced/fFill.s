/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/enhanced/fFill.s,v 1.12 1992/09/11 15:44:14 dawes Exp $ */
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
 * /usr/X386/mit/server/ddx/x386/cfb.banked/RCS/fFill.s,v 1.0 92/05/01 16:44:22 root Exp
 */

#include "assembler.h"


#if __STDC__ && !defined(UNIXCPP)
#define RROP_NAME_CAT(prefix,suffix)    prefix##suffix
#else
#define RROP_NAME_CAT(prefix,suffix)    prefix/**/suffix
#endif

#define GXAnd	1
#define GXCopy	2
#define GXOr	3
#define GXXor	4

#if RROP == GXAnd
	.file "fFillAnd.s"
#  define RROPB	andb
#  define RROPW andw
#  define RROPL andl
#  define RROP_NAME(pref)	RROP_NAME_CAT(pref,GXand)
#elif RROP == GXOr
	.file "fFillOr.s"
#  define RROPB	orb
#  define RROPW orw
#  define RROPL orl
#  define RROP_NAME(pref)	RROP_NAME_CAT(pref,GXor)
#elif RROP == GXXor
	.file "fFillXor.s"
#  define RROPB	xorb
#  define RROPW xorw
#  define RROPL xorl
#  define RROP_NAME(pref)	RROP_NAME_CAT(pref,GXxor)
#elif RROP == GXCopy
	.file "fFillCopy.s"
#  define RROPB	movb
#  define RROPW movw
#  define RROPL movl
#  define RROP_NAME(pref)	RROP_NAME_CAT(pref,GXcopy)
#endif


/*
 *
 * This routine implements a fast Solid Fill in GXcopy
 * No segment checking is done.
 *
 * SYNTAX:
 * unchar * fastFillSolid<RROP>(pdst,fill,dummy,hcount,count,width,widthPitch);
 * 
 *  (7/27/90 TR)
 *  (4/92 JT)
 *  (5/92 DW)
 */

#define pdst        %edi
#define fill        %eax
#define fillw       %ax
#define fillb       %al
#define count       %esi
#define hcount      %ecx
#define width       28(%ebp)
#define widthPitch  %ebx
#define tmp         %eax


	.data
	ALIGNDATA4
countt:	
	.long 0
	.text
	ALIGNTEXT4
#if !defined(SYSV) && !defined(SVR4)
.globl	RROP_NAME(_fastFillSolid)

RROP_NAME(_fastFillSolid):
#else
.globl	RROP_NAME(fastFillSolid)

RROP_NAME(fastFillSolid):
#endif
	pushl %ebp
	movl %esp,%ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 8(%ebp),pdst
	movl 12(%ebp),fill
	movl 20(%ebp),hcount
	movl 24(%ebp),count
	movl 32(%ebp),widthPitch
	orl hcount,hcount
	jz .finish
	orl count,count
	jz .finish
	cmpl $3,count
	jg .startblockloop
	je .tribbleloop
	cmpl $2,count
	je .wordloop
/*
 * do a fast vertical line
 */
	ALIGNTEXT4
.byteloop:
	RROPB fillb,(pdst)
	leal 1(widthPitch,pdst),pdst
	loop .byteloop
	jmp .finish

	ALIGNTEXT4
.wordloop:
	RROPW fillw,(pdst)
	leal 2(widthPitch,pdst),pdst
	loop .wordloop
	jmp .finish

	ALIGNTEXT4
.tribbleloop:
	RROPW fillw,(pdst)
	RROPB fillb,2(pdst)
	leal 3(widthPitch,pdst),pdst
	loop .tribbleloop
	jmp .finish

#undef count
#define count %ecx
#undef hcount
#define hcount %esi

.startblockloop:
	xchgl %ecx, %esi
	ALIGNTEXT4
.blockloop:
	testl $1,pdst
	jz .alignword
#if RROP == GXCopy
	stosb
#else
	RROPB fillb,(pdst)
	incl pdst
#endif
	decl count
.alignword:
	testl $2,pdst
	jz .aligneddword
#if RROP == GXCopy
	stosw
#else
	RROPW fillw,(pdst)
	addl $2,pdst
#endif
	leal -2(count),count
.aligneddword:
	movl count, countt
	andl $3, countt
	shrl $2, count
#if RROP == GXCopy
	repz
	stosl
#else
	orl %ecx, %ecx
	jz .endloop
	ALIGNTEXT4
.loop0: RROPL fill,(pdst)
	addl $4,pdst
	loop .loop0
.endloop:
#endif
	testl $2,countt
	jz .fixupbyte
#if RROP == GXCopy
	stosw
#else
	RROPW fillw,(pdst)
	addl $2,pdst
#endif
.fixupbyte:
	testl $1,countt
	jz .enditeration
#if RROP == GXCopy
	stosb
#else
	RROPB fillb,(pdst)
	incl pdst
#endif
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


