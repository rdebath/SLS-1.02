/*
 * Copyright 1989 Chris Torek
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Chris Torek or M.I.T.
 * not be used in advertising or publicity pertaining to distribution of
 * the software without specific, written prior permission.  Chris
 * Torek and M.I.T. make no representations about the suitability of
 * this software for any purpose.  It is provided "as is" without express
 * or implied warranty.
 *
 * CHRIS TOREK AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
 * SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS.  IN NO EVENT SHALL CHRIS TOREK OR M.I.T. BE LIABLE FOR ANY
 * SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
 * CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * Original Author:
 * 	Chris Torek
 * 	Dept. of Computer Science
 * 	Univ. of Maryland
 * 	chris@cs.umd.edu
 */ 

#ifndef lint
static char rcsid[] = "$Header: /home/reed/grunwald/Projects/Iptex/lib/RCS/gfclass.c,v 1.3 89/02/13 14:31:07 grunwald Exp Locker: grunwald $";
#endif

/*
 * gfclass - GF code classification tables.
 */

#include "gfclass.h"

/* shorthand---in lowercase for contrast (read on!) */
#define	three(x)	x, x, x
#define	four(x)		x, x, x, x
#define	five(x)		four(x), x
#define	six(x)		four(x), x, x
#define	eight(x)	four(x), four(x)
#define	sixteen(x)	eight(x), eight(x)
#define	thirty_two(x)	sixteen(x), sixteen(x)
#define	sixty_four(x)	thirty_two(x), thirty_two(x)
#define	one_twenty_eight(x) sixty_four(x), sixty_four(x)
#define	one_sixty_five(x) one_twenty_eight(x), thirty_two(x), five(x)

/*
 * Length of the single (or first) operand, if any.
 */
char gf_oplen[256] = {
	sixty_four(GPL_NONE),	/* GF_PAINT_0 through GF_PAINT_63 */
	GPL_UNS1,		/* GF_PAINT1 */
	GPL_UNS2,		/* GF_PAINT2 */
	GPL_UNS3,		/* GF_PAINT3 */
	GPL_NONE,		/* GF_BOC */
	GPL_NONE,		/* GF_BOC1 */
	GPL_NONE,		/* GF_EOC */
	GPL_NONE,		/* GF_SKIP0 */
	GPL_UNS1,		/* GF_SKIP1 */
	GPL_UNS2,		/* GF_SKIP2 */
	GPL_UNS3,		/* GF_SKIP3 */
	one_sixty_five(GPL_NONE),/* GF_NEW_ROW_0 through GF_NEW_ROW_164 */
	GPL_UNS1,		/* GF_XXX1 */
	GPL_UNS2,		/* GF_XXX2 */
	GPL_UNS3,		/* GF_XXX3 */
	GPL_SGN4,		/* GF_XXX4 */
	GPL_SGN4,		/* GF_YYY */
	GPL_NONE,		/* GF_NOP */
	GPL_NONE,		/* GF_CHAR_LOC */
	GPL_NONE,		/* GF_CHAR_LOC0 */
	GPL_NONE,		/* GF_PRE */
	GPL_NONE,		/* GF_POST */
	GPL_NONE,		/* GF_POSTPOST */
	six(GPL_NONE)		/* 250 through 255 */
};

/*
 * Types of the various opcodes.
 */
char gf_gt[256] = {
	sixty_four(GT_PAINT0),	/* GF_PAINT_0 through GF_PAINT_63 */
	three(GT_PAINT),	/* GF_PAINT1 through GF_PAINT3 */
	GT_BOC,			/* GF_BOC */
	GT_BOC1,		/* GF_BOC1 */
	GT_EOC,			/* GF_EOC */
	GT_SKIP0,		/* GF_SKIP0 */
	three(GT_SKIP),		/* GF_SKIP1 through GF_SKIP3 */
	one_sixty_five(GT_NEW_ROW),/* GF_NEW_ROW_0 throgh GF_NEW_ROW_164 */
	four(GT_XXX),		/* GF_XXX1 through GF_XXX4 */
	GT_YYY,			/* GF_YYY */
	GT_NOP,			/* GF_NOP */
	GT_CHAR_LOC,		/* GF_CHAR_LOC */
	GT_CHAR_LOC0,		/* GF_CHAR_LOC0 */
	GT_PRE,			/* GF_PRE */
	GT_POST,		/* GF_POST */
	GT_POSTPOST,		/* GF_POSTPOST */
	six(GT_UNDEF)		/* 250 through 255 */
};
