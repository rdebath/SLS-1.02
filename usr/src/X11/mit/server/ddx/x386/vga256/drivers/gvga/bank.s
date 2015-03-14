/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/drivers/gvga/bank.s,v 1.10 1992/08/29 11:15:49 dawes Exp $ */
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
 * $Header: /proj/X11/mit/server/ddx/x386/drivers/gvga/RCS/bank.s,v 1.1 1991/06/02 22:37:11 root Exp $
 */


/*
 * These are here the very lowlevel VGA bankswitching routines.
 * The segment to switch to is passed via %eax. Only %eax and %edx my be used
 * without saving the original contents.
 *
 * WHY ASSEMBLY LANGUAGE ???
 *
 * These routines must be callable by other assembly routines. But I don't
 * want to have the overhead of pushing and poping the normal stack-frame.
 */

/*
 * first we have here a mirror for the segment register. That's because a
 * I/O read costs so much more time, that is better to keep the value of it
 * in memory.
 */

#include "assembler.h"

	.file "gvgabank.s"

#if !defined(SYSV) && !defined(SVR4)
#define GVGASetRead _GVGASetRead
#define GVGASetWrite _GVGASetWrite
#define GVGASetReadWrite _GVGASetReadWrite
#endif

	.data
Segment:
	.byte 0x40
 

	.text

	ALIGNTEXT4
	.globl	GVGASetRead
GVGASetRead:
	movb	Segment,%ah
	andb	$0x78,%ah
	orb	%al,%ah
	movb	%ah,Segment
	movb	$6,%al
	movl	$0x3C4,%edx
	OUT_W	(%dx)
	ret

        ALIGNTEXT4
	.globl	GVGASetWrite
GVGASetWrite:
	movb	Segment,%ah
	andb	$0x47,%ah
	shlb	$3,%al
	orb	%al,%ah
	movb	%ah,Segment
	movb	$6,%al
	movl	$0x3C4,%edx
	OUT_W	(%dx)
	ret
	
	ALIGNTEXT4
	.globl	GVGASetReadWrite
GVGASetReadWrite:
	movb	%al,%ah
	shlb	$3,%ah
	orb	%al,%ah
	orb	$0x40,%ah
        movb    %ah,Segment
	movb	$6,%al
	movl	$0x3C4,%edx
	OUT_W	(%dx)
        ret

