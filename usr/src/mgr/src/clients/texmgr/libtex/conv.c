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
static char rcsid[] = "$Header: /home/reed/grunwald/Projects/Iptex/lib/RCS/conv.c,v 1.3 89/02/13 14:30:55 grunwald Exp Locker: grunwald $";
#endif

/*
 * Conversions.
 */

#include "types.h"
#include "conv.h"

Conv Conversion;

double	DMagFactor();

/*
 * Set a conversion (possibly the global conversion).
 */
void
CSetConversion(c, dpi, usermag, num, denom, dvimag)
	register struct conversion *c;
	int dpi, usermag;
	i32 num, denom, dvimag;
{
	double ddpi = dpi;

	c->c_mag = DMagFactor((int) dvimag) * DMagFactor(usermag);
	c->c_dpi = ddpi;

	/*
	 * The conversion facture is figured as follows:  there are exactly
	 * num/denom DVI units per decimicron, and 254000 decimicrons per
	 * inch, and dpi pixels per inch.  Then we have to adjust this by
	 * the stated magnification. 
	 */
	c->c_fromsp = (num / 254000.0) * (ddpi / denom) * c->c_mag;

	/*
	 * c->c_tosp is 1/c->c_fromsp, but we will invert the expression
	 * above in the hopes of some extra accuracy.
	 *
	 * IS THIS ANY GOOD?  I NEED A NUMERICAL ANALYST!
	 */
	c->c_tosp = (254000.0 / num) * (denom / ddpi) * (1.0 / c->c_mag);
}
