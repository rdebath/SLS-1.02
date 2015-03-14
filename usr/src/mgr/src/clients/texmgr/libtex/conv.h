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
 * Conversions.  Conversion factors convert between values in scaled
 * points and values in device-dependenent units.  The results of all
 * conversions are rounded to the nearest integral value, of type (i32).
 */

/*
 * This is now done using `double' values, but may be changed to
 * fixed-point or some other `fast' method, as long as the results
 * are consistent and reasonably accurate.  The structure `conversion'
 * holds the conversion-method-dependent quantities; the macros
 * fromSP and toSP apply the conversion to a value.  (Note that
 * fromSP and toSP need not be macros, but should be fast.)
 *
 * SetConversion sets the (single, global) conversion factor.
 * If a driver needs special conversions, there is another routine,
 * CSetConversion that sets a specific conversion, and cfromSP and
 * ctoSP to apply these.
 *
 * IS USING DOTS PER INCH SUFFICIENT?  (Pixels per point might be better.)
 *
 * Note that it is necessary to set the global conversion factor before
 * using any fonts.
 */

typedef struct conversion {
	double	c_fromsp;	/* multiplier to convert from scaled points */
	double	c_tosp;		/* multiplier to convert to scaled points:
				   could divide by c_fromsp, but this should
				   be faster and more accurate */
	double	c_mag;		/* the magnification this conversion
				   represents; mainly for GetFont() */
	double	c_dpi;		/* dpi (should be pixels per point?) */
} Conv;

/*
 * In order to do this, we need to round properly.  The compilers I
 * have tend to generate very poor code for this.  The following is
 * intended to help them out.  Smarter compilers can do better, but
 * if they are smart enough, they will realise that the variables
 * here are not used anywhere else, and discard them.  (For a compiler
 * to do this given separate compliation, `static' is a must.)
 */

#ifdef lint			/* or a smart compiler */

#define	ROUND(f) ((i32) ((f) < 0.0 ? (f) - 0.5 : (f) + 0.5))
#define	CEIL(f)	((double) (i32) (f) < (f) ? (i32) (f) + 1 : (i32) (f))

#else

static double _half = 0.5;
static double _zero = 0.0;
static double _d;

#define	ROUND(f) ((i32) (_d = (f), _d < _zero ? _d - _half : _d + _half))

#ifdef NEGATIVE_FLOAT_ROUNDS_TO_NEGATIVE_INFINITY

#define	CEIL(f)  (-(i32) -(f))

#else /* we will assume that floating to integer truncates */

static i32 _i;

#define	CEIL(f)	 (_i = _d = (f), _i < _d ? _i + 1 : _i)

#endif /* round towards negative infinity */

#endif /* lint */

#define	SetConversion(dpi, usermag, num, denom, dvimag)	\
	CSetConversion(&Conversion, dpi, usermag, num, denom, dvimag)

#define	cfromSP(c, v)	ROUND((c)->c_fromsp * (v))
#define	ctoSP(c, v)	ROUND((c)->c_tosp * (v))

#define	fromSP(v)	cfromSP(&Conversion, v)
#define	toSP(v)		ctoSP(&Conversion, v)

/*
 * Conversions for rules are a bit different: we must round up, rather
 * than off.  ConvRule applies the global conversion value for a rule
 * value (height or width); CConvRule applies a specific conversion.
 */

#define	CConvRule(c, v)	CEIL((c)->c_fromsp * (v))
#define	ConvRule(v)	CConvRule(&Conversion, v)

void	CSetConversion();
