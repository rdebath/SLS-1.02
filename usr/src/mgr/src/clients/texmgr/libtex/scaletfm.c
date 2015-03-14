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
static char rcsid[] = "$Header: /home/reed/grunwald/Projects/Iptex/lib/RCS/scaletfm.c,v 1.3 89/02/13 14:31:19 grunwald Exp Locker: grunwald $";
#endif

#include "types.h"
#include "font.h"

/*
 * From DVITYPE.WEB:
 *
 * ``The most important part of in_TFM is the width computation, which
 *   involvles multiplying the relative widths in the TFM file by the scaling
 *   factor in the DVI file.  This fixed-point multiplication must be done with
 *   precisely the same accuracy by all DVI-reading programs, in order to
 *   validate the assumptions made by DVI-writing programs like \TeX 82.
 *
 *   Let us therefore summarize what needs to be done.  Each width in a TFM
 *   file appears as a four-byte quantity called a fix_word.  A fix_word whose
 *   respective bytes are (a,b,c,d) represents the number
 *
 *	   {{ b * 2^{-4} + c * 2^{-12} + d * 2^{-20},        if a = 0;
 *    x = {{
 *	   {{ -16 + b * 2^{-4} + c * 2^{-12} + d * 2^{-20},  if a = 255.
 *
 *   (No other choices of a are allowed, since the magnitude of a TFM dimension
 *   must be less than 16.)  We want to multiply this quantity by the integer
 *   z, which is known to be less than 2^{27}.  Let \alpha = 16z.  If z <
 *   2^{23}, the individual multiplications b * z, c * z, d * z cannot
 *   overflow; otherwise we will divide z by 2, 4, 8, or 16, to obtain a
 *   multiplier less than 2^{23}, and we can compensate for this later.  If z
 *   has thereby been replaced by z' = z/2^e, let \beta = 2^{4-e}; we shall
 *   compute
 *
 *	\lfloor (b + c * 2^{-8} + d * 2^{-16})z' / \beta \rfloor
 *
 *   if a = 0, or the same quantity minus \alpha if a = 255.  This calculation
 *   must be done exactly, for the reasons stated above; the following program
 *   does the job in a system-independent way, assuming that arithmetic is
 *   exact on numbers less than 2^{31} in magnitude.''
 */

/*
 * Scale the single TFM width t by z.
 */
i32
ScaleOneWidth(t, z)
	register i32 t, z;
{
	register i32 alpha, log2beta, r;

	/* First compute \alpha, \beta, and z': */
	alpha = 16 * z;
	log2beta = 4;
	while (z >= (1 << 23)) {
		z >>= 1;
		log2beta--;
	}

	/* The four values 'a', 'b', 'c', and 'd' are fields within t: */
#define	a	(UnSign8(t >> 24))
#define	b	(UnSign8(t >> 16))
#define	c	(UnSign8(t >> 8))
#define	d	(UnSign8(t))
	if (t) {
		r = (((((d * z) >> 8) + c * z) >> 8) + b * z) >> log2beta;
		if (a) {
			if (a != 255)
				error(0, 0, "bad TFM width! [ScaleOneWidth]");
			r -= alpha;
		}
		return (r);
	}
	else
		return (0);
}

/*
 * Scale a set of glyphs [l..h) in font f according to f->f_dvimag.
 */
ScaleGlyphs(f, l, h)
	register struct font *f;
	int l, h;
{
	register int i;
	register i32 t, z, alpha, log2beta;

	z = f->f_dvimag;
	alpha = 16 * z;
	log2beta = 4;
	while (z >= (1 << 23)) {
		z >>= 1;
		log2beta--;
	}

	for (i = l; i < h; i++) {
		if ((t = f->f_gly[i]->g_tfmwidth) == 0)
			continue;
		t = (((((d * z) >> 8) + c * z) >> 8) + b * z) >> log2beta;
		if (a) {
			if (a != 255)
				error(0, 0, "\"%s\", glyph %d: bad TFM width",
					f->f_path, i);
			t -= alpha;
		}
		f->f_gly[i]->g_tfmwidth = t;
	}
}
