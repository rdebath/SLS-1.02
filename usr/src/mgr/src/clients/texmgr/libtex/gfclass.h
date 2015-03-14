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

/*
 * GF classification codes
 */

/*
 * Predicate for simple paint commands.  This is presumably the most
 * common GF operation; it may be profitable to check for this before
 * switching out on the command type.
 */
#define	GF_IsPaint(c)	((c) < 64)

/*
 * Symbolic names for command `types', as returned by the macro
 * GT_TYPE(int c).
 */
#define GT_PAINT0	0	/* paint without parameter */
#define	GT_PAINT	1	/* paint with parameter */
#define GT_BOC		2	/* long BOC */
#define	GT_BOC1		3	/* short BOC */
#define GT_EOC		4	/* EOC */
#define GT_SKIP0	5	/* parameterless SKIP */
#define	GT_SKIP		6	/* parmeterised SKIP */
#define GT_NEW_ROW	7	/* NEW_ROW_n */
#define GT_XXX		8	/* XXXn */
#define GT_YYY		9	/* YYY */
#define	GT_NOP		10	/* no op */
#define GT_CHAR_LOC	11	/* CHAR_LOC */
#define	GT_CHAR_LOC0	12	/* abbreviated CHAR_LOC */
#define	GT_PRE		13
#define	GT_POST		14
#define	GT_POSTPOST	15
#define	GT_UNDEF	16

/*
 * Symbolic names for parameter lengths, obtained via the macro
 * GT_OpLen(int c).
 */
#define	GPL_NONE	0	/* no parameter, or too complex */
#define	GPL_UNS1	1	/* one one-byte parameter in 0..255 */
#define	GPL_UNS2	2	/* one two-byte parameter in 0..65535 */
#define	GPL_UNS3	3	/* one three-byte parameter in 0..16777215 */
#define	GPL_SGN4	4	/* one four-byte signed parameter */
/*
 * there are no unsigned four byte parameters, and no shorter signed
 * parameters
 */

#define GF_OpLen(code)	(gf_oplen[code])
#define GF_TYPE(code)	(gf_gt[code])
extern char gf_oplen[];
extern char gf_gt[];
