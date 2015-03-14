/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/drivers/et4000/bank.s,v 1.10 1992/08/29 11:15:43 dawes Exp $ */
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
 * $Header: /proj/X11/mit/server/ddx/x386/drivers/et4000/RCS/bank.s,v 1.1 1991/06/02 22:37:04 root Exp $
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

 	.file "et4000bank.s"

#if !defined(SYSV) && !defined(SVR4)
#define ET4000SetRead _ET4000SetRead
#define ET4000SetWrite _ET4000SetWrite
#define ET4000SetReadWrite _ET4000SetReadWrite
#endif

	.data
Segment:
	.byte 0
 

	.text

	ALIGNTEXT4
	.globl	ET4000SetRead
ET4000SetRead:
	movb	Segment,%ah
	andb	$0x0f,%ah
	shlb	$4,%al
	orb	%ah,%al
	movb	%al,Segment
	movl	$0x3CD,%edx
	OUT_B	(%dx)
	ret

        ALIGNTEXT4
	.globl	ET4000SetWrite
ET4000SetWrite:
	movb	Segment,%ah
	andb	$0xf0,%ah
	orb	%ah,%al
	movb	%al,Segment
	movl	$0x3CD,%edx
	OUT_B	(%dx)
	ret
	
	ALIGNTEXT4
	.globl	ET4000SetReadWrite
ET4000SetReadWrite:
	movb	%al,%ah
	shlb	$4,%ah
	orb	%ah,%al
        movb    %al,Segment
	movl	$0x3CD,%edx
        OUT_B    (%dx)
        ret

