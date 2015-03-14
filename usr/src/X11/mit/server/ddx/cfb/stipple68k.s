/*
 * $XConsortium: stipple68k.s,v 1.1 91/02/23 12:43:38 rws Exp $
 *
 * Copyright 1990 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

/*
 * Stipple code for 68k processors
 */

#ifdef TETEXT
#define _stipplestack _stipplestackte
#endif


#define atemp	a0
#define addr	a1
#define stipple	a2
#define stride	a3
#define case	a4

#define ctemp	d0
#define value	d1
#define count	d2
#define shift	d3
#define c	d4
#define lshift	d5
#define rshift	d6

#define PushMask	#0x3e38
#define PopMask		#0x1c7c
#define NumReg	8
#define arg0	36
#define arg1	40
#define arg2	44
#define arg3	48
#define arg4	52
#define arg5	56
#define arg6	60

#define ForEachLine	L2
#define ForEachBits	L5
#define CASE_SIZE	5

.text
	.even
	.globl _stipplestack
_stipplestack:
	moveml	PushMask,sp@-
	movel	sp@(arg0),addr
	movel	sp@(arg1),stipple
	movel	sp@(arg2),value
	movel	sp@(arg3),stride
	movew	sp@(arg4+2),count
	movel	sp@(arg5),shift
	subqw	#1,count		/* predecrement count */
	lea	CaseBegin,case
	movew	#28,lshift
	addl	shift,lshift
	movew	#28,rshift
	subql	#4,shift
	negl	shift
ForEachLine:
	movel	addr,atemp
	addl	stride,addr
	movel	stipple@+,c
#ifdef TETEXT
	beq	NextLine
#endif
	/* Get first few bits */
	movel	c,ctemp
	lsrl	lshift,ctemp
	lsll	#CASE_SIZE,ctemp
	lsll	shift,c			/* set up for next bits */
	jmp	case@(ctemp:l)

ForEachBits:
	addl	#4,atemp
	movel	c,ctemp
	lsrl	rshift,ctemp		/* better than lsrl, andi */
	lsll	#CASE_SIZE,ctemp
	lsll	#4,c			/* set up for next bits */
	jmp	case@(ctemp:l)

#define Break				\
	andl	c,c		 	; \
	bne	ForEachBits	 	; \
	dbra	count,ForEachLine	; \
	moveml	sp@+,PopMask		; \
	rts				;

CaseBegin:
	bne	ForEachBits		/* 0 */
NextLine:
	dbra	count,ForEachLine
	moveml	sp@+,PopMask
	rts
	
	. = CaseBegin + 0x20

	moveb	value,atemp@(3)		/* 1 */
	Break

	. = CaseBegin + 0x40

	moveb	value,atemp@(2)		/* 2 */
	Break

	. = CaseBegin + 0x60

	movew	value,atemp@(2)		/* 3 */
	Break

	. = CaseBegin + 0x80

	moveb	value,atemp@(1)		/* 4 */
	Break

	. = CaseBegin + 0xa0

	moveb	value,atemp@(3)		/* 5 */
	moveb	value,atemp@(1)
	Break

	. = CaseBegin + 0xc0

	movew	value,atemp@(1)		/* 6 */
	Break

	. = CaseBegin + 0xe0

	movew	value,atemp@(2)		/* 7 */
	moveb	value,atemp@(1)	
	Break

	. = CaseBegin + 0x100

	moveb	value,atemp@(0)		/* 8 */
	Break

	. = CaseBegin + 0x120

	moveb	value,atemp@(3)		/* 9 */
	moveb	value,atemp@(0)
	Break

	. = CaseBegin + 0x140

	moveb	value,atemp@(2)		/* a */
	moveb	value,atemp@(0)
	Break

	. = CaseBegin + 0x160

	movew	value,atemp@(2)		/* b */
	moveb	value,atemp@(0)
	Break

	. = CaseBegin + 0x180

	movew	value,atemp@(0)		/* c */
	Break

	. = CaseBegin + 0x1a0

	moveb	value,atemp@(3)		/* d */
	movew	value,atemp@(0)
	Break

	. = CaseBegin + 0x1c0

	moveb	value,atemp@(2)		/* e */
	movew	value,atemp@(0)
	Break

	. = CaseBegin + 0x1e0

	movel	value,atemp@(0)		/* f */
	Break
