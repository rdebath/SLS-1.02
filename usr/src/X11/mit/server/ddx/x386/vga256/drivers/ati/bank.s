/* $Header: /home/x_cvs/mit/server/ddx/x386/vga256/drivers/ati/bank.s,v 1.3 1992/08/30 07:43:41 dawes Exp $ */
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
 * These are here the very lowlevel VGA bankswitching routines.
 * The segment to switch to is passed via %eax. Only %eax and %edx my be used
 * without saving the original contents.
 *
 * WHY ASSEMBLY LANGUAGE ???
 *
 * These routines must be callable by other assembly routines. But I don't
 * want to have the overhead of pushing and poping the normal stack-frame.
 *
 * Enhancements to support most VGA Wonder cards (including Plus and XL)
 * by Doug Evans, dje@sspiff.UUCP.
 * ALL DISCLAIMERS APPLY TO MY ADDITIONS AS WELL.
 *
 *	I've come to believe the people who design these cards couldn't
 *	allow for future enhancements if their life depended on it!
 *	The register set architecture is a joke!
 *
 * Boards V4 and V5 have the 18800-1 chip. Boards Plus and XL have the 28800
 * chip. Page selection is done with Extended Register 1CE, Index B2.
 * The format is:
 *
 * D7-D5 = Read page select bits 2-0
 * D4    = Reserved (18800-1)
 * D4    = Read page select bit 3 (28800)	Arrggghhhh!!!!
 * D3-D1 = Page select bits 2-0
 * D0    = Reserved (18800-1)
 * D0    = Page select bit 3 (28800)		Arrggghhhh!!!!
 *
 * Actually, it's even worse than this. The manual can't make up it's mind
 * whether its:
 *		R2 R1 R0 R3 W2 W1 W0 W3
 * or
 *		R2 R1 R0 W3 W2 W1 W0 R3
 *
 * It appears that the format is: R2 R1 R0 W3 W2 W1 W0 R3.
 */

#include "assembler.h"

	.file "atibank.s"

#if !defined(SYSV) && !defined(SVR4)
#define ATISetRead _ATISetRead
#define ATISetWrite _ATISetWrite
#define ATISetReadWrite _ATISetReadWrite
#define ATIExtReg _ATIExtReg
#endif

/**
 ** Please read the notes in driver.c !!!
 **/

	.data

/*
 * We have a mirror for the segment register because an I/O read costs so much
 * more time, that is better to keep the value of it in memory.
 */

Segment:
	.byte 0
 
/*
 * The functions ...
 */

	.text


	ALIGNTEXT4
	.globl	ATISetRead
ATISetRead:
	andl	$0x0F,%eax			/* FIXME: necessary? */
	movb	Segment,%ah
	andb	$0x1E,%ah
	rorb	$3,%al
	orb	%al,%ah
	movb	%ah,Segment
	movw	ATIExtReg,%dx
  	movb	$0xB2,%al
	OUT_W	(%dx)
	ret

        ALIGNTEXT4
	.globl	ATISetWrite
ATISetWrite:
	andl	$0x0F,%eax			/* FIXME: necessary? */
	movb	Segment,%ah
	andb	$0xE1,%ah
	shlb	$1,%al
	orb	%al,%ah
	movb	%ah,Segment
	movw	ATIExtReg,%dx
  	movb	$0xB2,%al
	OUT_W	(%dx)
	ret
	
	ALIGNTEXT4
	.globl	ATISetReadWrite
ATISetReadWrite:
	andl	$0x0F,%eax			/* FIXME: necessary? */
	movb	%al,%ah
	shlb	$1,%ah
	rorb	$3,%al
	orb	%al,%ah
        movb    %ah,Segment
	movw	ATIExtReg,%dx
  	movb	$0xB2,%al
	OUT_W	(%dx)
	ret
