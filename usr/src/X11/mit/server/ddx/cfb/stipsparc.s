/*
 * $XConsortium: stipsparc.s,v 1.6 91/06/12 17:03:24 keith Exp $
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
 * SPARC assembly code for optimized text rendering.
 *
 * Other stippling could be done in assembly, but the payoff is
 * not nearly as large.  Mostly because large areas are heavily
 * optimized already.
 */

/* not that I expect to ever see an LSB SPARC, but ... */
#ifdef LITTLE_ENDIAN
# define BitsR		sll
# define BitsL		srl
# define BO(o)		o
# define HO(o)		o
# define WO(o)		o
# define FourBits(dest,bits)	and	bits, 0xf, dest
#else
# define BitsR		srl
# define BitsL		sll
# define BO(o)		3-o
# define HO(o)		2-o
# define WO(o)		o
# define FourBits(dest,bits)	srl	bits, 28, dest
#endif

/*
 * stipplestack(addr, stipple, value, stride, Count, Shift)
 *               4       5       6      7     16(sp) 20(sp)
 *
 *  Apply successive 32-bit stipples starting at addr, addr+stride, ...
 *
 *  Used for text rendering, but only when no data could be lost
 *  when the stipple is shifted left by Shift bits
 */
/* arguments */
#define addr	%i0
#define stipple	%i1
#define value	%i2
#define stride	%i3
#define count	%i4
#define shift	%i5

/* local variables */
#define atemp	%l0
#define bits	%l1
#define lshift	%l2
#define sbase	%l3
#define stemp	%l4

#define CASE_SIZE	5	/* case blocks are 2^5 bytes each */
#define CASE_MASK	0x1e0	/* first case mask */

#define ForEachLine	LY1
#define NextLine	LY2
#define CaseBegin	LY3
#define ForEachBits	LY4
#define NextBits	LY5

#ifdef TETEXT
#define	_stipplestack	_stipplestackte
#endif

	.seg	"text"
	.proc	16
	.globl	_stipplestack
_stipplestack:
	save	%sp,-64,%sp
	sethi	%hi(CaseBegin),sbase		/* load up switch table */
	or	sbase,%lo(CaseBegin),sbase

	mov	4,lshift			/* compute offset within */
	sub	lshift, shift, lshift		/*  stipple of remaining bits */
#ifdef LITTLE_ENDIAN
	inc	CASE_SIZE, shift		/* first shift for LSB */
#else
	inc	28-CASE_SIZE, shift		/* first shift for MSB */
#endif
	/* do ... while (--count > 0); */
ForEachLine:
	ld	[stipple],bits			/* get stipple bits */
	mov	addr,atemp			/* set up for this line */
#ifdef TETEXT
	/* Terminal emulator fonts are expanded and have many 0 rows */
	tst	bits
	bz	NextLine			/* skip out early on 0 */
#endif
	add	addr, stride, addr		/* step for the loop */
	BitsR	bits, shift, stemp		/* get first bits */
	and	stemp, CASE_MASK, stemp		/* compute first jump */
	BitsL	bits, lshift, bits		/* set remaining bits */
	jmp	sbase+stemp			/*  ... */
	tst	bits

ForEachBits:
	inc	4, atemp
ForEachBits1:
	FourBits(stemp, bits)			/* compute jump for */
	sll	stemp, CASE_SIZE, stemp		/*  these four bits */
	BitsL	bits, 4, bits			/* step for remaining bits */
	jmp	sbase+stemp			/* jump */
	tst	bits
CaseBegin:
	bnz,a	ForEachBits1			/* 0 */
	inc	4, atemp
NextLine:
	deccc	1, count
	bnz,a	ForEachLine
	inc	4, stipple
	ret
	restore
	nop

	bnz	ForEachBits			/* 1 */
	stb	value, [atemp+BO(0)]
	deccc	1, count
	bnz,a	ForEachLine
	inc	4, stipple
	ret
	restore
	nop
					
	bnz	ForEachBits			/* 2 */
	stb	value, [atemp+BO(1)]
	deccc	1, count
	bnz,a	ForEachLine
	inc	4, stipple
	ret
	restore
	nop
					
	bnz	ForEachBits			/* 3 */
	sth	value, [atemp+HO(0)]
	deccc	1, count
	bnz,a	ForEachLine
	inc	4, stipple
	ret
	restore
	nop
					
	bnz	ForEachBits			/* 4 */
	stb	value, [atemp+BO(2)]
	deccc	1, count
	bnz,a	ForEachLine
	inc	4, stipple
	ret
	restore
	nop
					
	stb	value, [atemp+BO(0)]		/* 5 */
	bnz	ForEachBits
	stb	value, [atemp+BO(2)]
	deccc	1, count
	bnz,a	ForEachLine
	inc	4, stipple
	ret
	restore
					
	stb	value, [atemp+BO(1)]		/* 6 */
	bnz	ForEachBits
	stb	value, [atemp+BO(2)]
	deccc	1, count
	bnz,a	ForEachLine
	inc	4, stipple
	ret
	restore
					
	sth	value, [atemp+HO(0)]		/* 7 */
	bnz	ForEachBits
	stb	value, [atemp+BO(2)]
	deccc	1, count
	bnz,a	ForEachLine
	inc	4, stipple
	ret
	restore
					
	bnz	ForEachBits			/* 8 */
	stb	value, [atemp+BO(3)]
	deccc	1, count
	bnz,a	ForEachLine
	inc	4, stipple
	ret
	restore
	nop
					
	stb	value, [atemp+BO(0)]		/* 9 */
	bnz	ForEachBits
	stb	value, [atemp+BO(3)]
	deccc	1, count
	bnz,a	ForEachLine
	inc	4, stipple
	ret
	restore
					
	stb	value, [atemp+BO(1)]		/* a */
	bnz	ForEachBits
	stb	value, [atemp+BO(3)]
	deccc	1, count
	bnz,a	ForEachLine
	inc	4, stipple
	ret
	restore
					
	sth	value, [atemp+HO(0)]		/* b */
	bnz	ForEachBits
	stb	value, [atemp+BO(3)]
	deccc	1, count
	bnz,a	ForEachLine
	inc	4, stipple
	ret
	restore
					
	bnz	ForEachBits			/* c */
	sth	value, [atemp+HO(2)]
	deccc	1, count
	bnz,a	ForEachLine
	inc	4, stipple
	ret
	restore
	nop
					
	stb	value, [atemp+BO(0)]		/* d */
	bnz	ForEachBits
	sth	value, [atemp+HO(2)]
	deccc	1, count
	bnz,a	ForEachLine
	inc	4, stipple
	ret
	restore
					
	stb	value, [atemp+BO(1)]		/* e */
	bnz	ForEachBits
	sth	value, [atemp+HO(2)]
	deccc	1, count
	bnz,a	ForEachLine
	inc	4, stipple
	ret
	restore
					
	bnz	ForEachBits			/* f */
	st	value, [atemp+WO(0)]
	deccc	1, count
	bnz,a	ForEachLine
	inc	4, stipple
	ret
	restore
