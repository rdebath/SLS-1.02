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
 * Convert a magnification factor to floating point.  This is used in
 * conjunction with the FONT_SLOP stuff to try to get the file names
 * right, and may also be used by DVI reading programs to get slightly
 * more accurate values for (mag/1000.0).
 */
double
DMagFactor(mag)
	int mag;
{

	switch (mag) {

	case 1095:		/* stephalf */
		return (1.095445);

	case 1315:		/* stepihalf */
		return (1.314534);

	case 2074:		/* stepiv */
		return (2.0736);

	case 2488:		/* stepv */
		return (2.48832);

	case 2986:		/* stepiv */
		return (2.985984);

	default:		/* remaining mags have been ok */
		return ((double) mag / 1000.);
	}
	/* NOTREACHED */
}
