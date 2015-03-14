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
 * Macros to convert DVI opcodes to (hopefully) simpler values.
 */

/*
 * Large range types.
 */
#define DVI_IsChar(code) ((code) < 128)
#define DVI_IsFont(code) ((code) >= 171 && (code) < 235)

/*
 * Symbolic names for generic types (for things with parameters).
 * These are obtained via the macro DVI_DT(int c), where 0 <= c <= 255.
 */
#define	DT_CHAR		 0
#define DT_SET		 1
#define	DT_SETRULE	 2
#define DT_PUT		 3
#define	DT_PUTRULE	 4
#define	DT_NOP		 5
#define	DT_BOP		 6
#define	DT_EOP		 7
#define	DT_PUSH		 8
#define	DT_POP		 9
#define DT_RIGHT	10
#define DT_W0		11
#define	DT_W		12
#define	DT_X0		13
#define DT_X		14
#define DT_DOWN		15
#define	DT_Y0		16
#define DT_Y		17
#define	DT_Z0		18
#define DT_Z		19
#define	DT_FNTNUM	20
#define DT_FNT		21
#define DT_XXX		22
#define DT_FNTDEF	23
#define	DT_PRE		24
#define	DT_POST		25
#define	DT_POSTPOST	26
#define	DT_UNDEF	27

/*
 * Symbolic names for parameter lengths, obtained via the macro
 * DVL_OpLen(int c).
 *
 * N.B.: older drivers may assume that 0 => none, 1-4 => 1-4 bytes
 * and 5-7 => unsigned version of 1-4---so DO NOT change these values!
 */
#define	DPL_NONE	0
#define	DPL_SGN1	1
#define	DPL_SGN2	2
#define	DPL_SGN3	3
#define	DPL_SGN4	4
#define	DPL_UNS1	5
#define	DPL_UNS2	6
#define	DPL_UNS3	7
/* there are no unsigned four byte parameters */

#define DVI_OpLen(code)  (dvi_oplen[code])
#define DVI_DT(code)	 (dvi_dt[code])
extern char dvi_oplen[];
extern char dvi_dt[];
