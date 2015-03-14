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
 * C-TeX types (system dependent).
 */

#ifndef _CTEX_TYPES_
#define _CTEX_TYPES_

/* a 16 bit integer (signed) */
typedef short i16;

/* a 32 bit integer (signed) */
typedef long i32;

/* macros to sign extend quantities that are less than 32 bits long */
#if defined(u3b) || defined(u3b2) || defined(u3b5) || defined(ibm032) || defined(sgi)
#define Sign8(n)	((n) & (1 << 7) ? ((n) | 0xffffff00) : (n))
#define Sign16(n)	((i32) (short) (n))
#define Sign24(n)	((n) & (1 << 23) ? ((n) | 0xff000000) : (n))
#else
#if defined(sun) || defined(hp300)
/* Sun mishandles (int)(char)(constant), but this subterfuge works: */
#define Sign8(n)	((i32) (char) (int) (n))
#else
#define Sign8(n)	((i32) (char) (n))
#endif /* sun */
#define Sign16(n)	((i32) (short) (n))
#define Sign24(n)	(((n) << 8) >> 8)
#endif /* u3b || u3b2 || u3b5 */

/* macros to truncate quantites that are signed but shouldn't be */
#define UnSign8(n)	((n) & 0xff)
#define UnSign16(n)	((n) & 0xffff)
#define UnSign24(n)	((n) & 0xffffff)

/* note that we never have unsigned 32 bit integers */

#endif /* _CTEX_TYPES_ */
