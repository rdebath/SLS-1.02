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

/* DVI file info */

/*
 * Units of distance are stored in scaled points, but we can convert to
 * units of 10^-7 meters by multiplying by the numbers in the preamble.
 */

/* the structure of the stack used to hold the values (h,v,w,x,y,z) */

typedef struct dvi_stack {
	i32	h;		/* the saved h */
	i32	v;		/* the saved v */
	i32	w;		/* etc */
	i32	x;
	i32	y;
	i32	z;
} DviStack;

extern DviStack dvi_current;	/* the current values of h, v, etc */
extern int	dvi_f;			/* the current font */

#define dvi_h dvi_current.h
#define dvi_v dvi_current.v
#define dvi_w dvi_current.w
#define dvi_x dvi_current.x
#define dvi_y dvi_current.y
#define dvi_z dvi_current.z
