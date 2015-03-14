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
static char rcsid[] = "$Header: /home/reed/grunwald/Projects/Iptex/lib/RCS/tfm.c,v 1.4 89/02/13 14:31:26 grunwald Exp Locker: grunwald $";
#endif

/*
 * TFM file reading routines.
 *
 * TODO:
 *	finish
 */

#include <stdio.h>
#include "types.h"
#include "fio.h"
#include "tfm.h"

char	*malloc();

#define	ALLOC(n, type)	((type *) malloc((unsigned) ((n) * sizeof (type))))

static trd_header();
static trd_ci();
static trd_fix();

int
readtfmfile(f, t, stopafterwidth)
	register FILE *f;
	register struct tfmdata *t;
	int stopafterwidth;	/* ??? */
{
	i32 nc;

	if (trd_header(f, &t->t_hdr))
		return (-1);
	nc = t->t_hdr.th_ec - t->t_hdr.th_bc + 1;

	t->t_ci = NULL;
	t->t_width = NULL;
	t->t_height = NULL;
	t->t_depth = NULL;

	(void) fseek(f, t->t_hdr.th_lh * 4L, 1);	/* XXX */

	if ((t->t_ci = ALLOC(nc, struct char_info_word)) == NULL ||
	    trd_ci(f, nc, t->t_ci) ||
	    (t->t_width = ALLOC(t->t_hdr.th_nw, i32)) == NULL ||
	    trd_fix(f, t->t_hdr.th_nw, t->t_width))
		goto bad;
	if (stopafterwidth)
		return (0);
	if ((t->t_height = ALLOC(t->t_hdr.th_nh, i32)) == NULL ||
	    trd_fix(f, t->t_hdr.th_nh, t->t_height) ||
	    (t->t_depth = ALLOC(t->t_hdr.th_nd, i32)) == NULL ||
	    trd_fix(f, t->t_hdr.th_nd, t->t_depth))
		goto bad;
	return (0);
	
      bad:
	if (t->t_ci != NULL) {
	  free((char *) t->t_ci);  t->t_ci = 0;
	}
	if (t->t_width != NULL) {
	  free((char *) t->t_width); t->t_width = 0;
	}
	if (t->t_height != NULL) {
	  free((char *) t->t_height); t->t_height = 0;
	}
	if (t->t_depth != NULL) {
	  free((char *) t->t_depth); t->t_depth = 0;
	}
	return (-1);
      }

static int
trd_header(f, th)
	register FILE *f;
	register struct tfmheader *th;
{
	register i32 *p;

	for (p = &th->th_lf; p <= &th->th_np; p++)
		fGetWord(f, *p);
	if (feof(f))
		return (-1);
	return (0);
}

static int
trd_ci(f, nc, ci)
	register FILE *f;
	register int nc;
	register struct char_info_word *ci;
{

	while (--nc >= 0) {
		ci->ci_width = fgetbyte(f);
		ci->ci_h_d = fgetbyte(f);
		ci->ci_i_t = fgetbyte(f);
		ci->ci_remainder = fgetbyte(f);
		ci++;
	}
	if (feof(f))
		return (-1);
	return (0);
}

static int
trd_fix(f, nf, p)
	register FILE *f;
	register int nf;
	register i32 *p;
{

	while (--nf >= 0) {
		fGetLong(f, *p);
		p++;
	}
	if (feof(f))
		return (-1);
	return (0);
}
