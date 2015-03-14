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
 * File I/O: numbers.
 *
 * We deal in fixed format numbers and (FILE *)s here.
 * For pointer I/O, see pio.h.
 *
 * N.B.: These do the `wrong thing' at EOF.  It is imperative
 * that the caller add appropriate `if (feof(fp))' statements.
 */

/*
 * Get one unsigned byte.  Note that this is a proper expression.
 * The reset have more limited contexts, and are therefore OddLy
 * CapItaliseD.
 */
#define	fgetbyte(fp)	(getc(fp))

/*
 * Get a two-byte unsigned integer, a three-byte unsigned integer,
 * or a four-byte signed integer.
 */
#define fGetWord(fp, r)	((r)  = getc(fp) << 8,  (r) |= getc(fp))
#define fGet3Byte(fp,r) ((r)  = getc(fp) << 16, (r) |= getc(fp) << 8, \
			 (r) |= getc(fp))
#define fGetLong(fp, r)	((r)  = getc(fp) << 24, (r) |= getc(fp) << 16, \
			 (r) |= getc(fp) << 8,  (r) |= getc(fp))

/*
 * Fast I/O write (and regular write) macros.
 */
#define	putbyte(fp, r)	(putc((r), fp))

#define PutWord(fp, r)	(putc((r) >> 8,  fp), putc((r), fp))
#define Put3Byte(fp, r)	(putc((r) >> 16, fp), putc((r) >> 8, fp), \
			 putc((r), fp))
#define PutLong(fp, r)	(putc((r) >> 24, fp), putc((r) >> 16, fp), \
			 putc((r) >> 8, fp),  putc((r), fp))

/*
 * Function types
 */
i32	GetByte(), GetWord(), Get3Byte(), GetLong();
