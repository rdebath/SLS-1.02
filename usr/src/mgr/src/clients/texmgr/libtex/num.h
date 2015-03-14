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
 * Pointer I/O: numbers.
 *
 * We deal in fixed format numbers and pointers here.
 * For file I/O, see fio.h.
 */

/*
 * Get one unsigned byte.  Note that this is a proper expression.
 * The rest have more limited contexts, and are therefore OddLy
 * CapItaliseD.
 */
#define	pgetbyte(p)	UnSign8(*(p)++)

/*
 * Get a two-byte unsigned integer, a three-byte unsigned integer,
 * or a four-byte signed integer.
 */
#define	pGetWord(p, r) ((r)  = UnSign8(*(p)++) << 8, \
			(r) |= UnSign8(*(p)++))
#define	pGet3Byte(p,r) ((r)  = UnSign8(*(p)++) << 16, \
			(r) |= UnSign8(*(p)++) << 8, \
			(r) |= UnSign8(*(p)++))
#define	pGetLong(p, r) ((r)  = UnSign8(*(p)++) << 24, \
			(r) |= UnSign8(*(p)++) << 16, \
			(r) |= UnSign8(*(p)++) << 8, \
			(r) |= UnSign8(*(p)++))

/*
 * ADD pputbyte, pPutWord, pPut3Byte, pPutLong HERE IF THEY PROVE
 * USEFUL.  (But then must consider changing PutWord &c in fio.h.)
 */
