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
static char rcsid[] = "$Header: /home/reed/grunwald/Projects/Iptex/lib/RCS/gffont.c,v 1.4 89/02/13 14:31:08 grunwald Exp Locker: grunwald $";
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "types.h"
#include "font.h"
#include "gfcodes.h"
#include "gfclass.h"
#include "num.h"

/*
 * GF font operations.
 *
 * GF files may be compact, but this code surely is not!
 *
 * TODO:
 *	think about fonts with characters outside [0..255]
 *	find some way to free gf body when done
 */

static gf_read();
static gf_getgly();
static gf_rasterise();
static gf_freefont();

struct	fontops gfops =
	{ "gf", 1.0, gf_read, gf_getgly, gf_rasterise, gf_freefont };

/*
 * Local info.
 */

/*
 * A bounding box.  The names follow those in the GF documentation.
 */
struct bounds {
	i32	min_m, max_m;	/* min and max `m' (colunm) values */
	i32	min_n, max_n;	/* min and max `n' (row) values */
};

/*
 * char_loc is one `character locator' from a GF file, save for the
 * `character residue', which we need not.
 */
struct char_loc {
	i32	cl_dx;		/* x escapement (scaled pixels) */
	i32	cl_dy;		/* y escapement (scaled pixels) */
	i32	cl_w;		/* TFM width */
	i32	cl_p;		/* pointer to BOC (or specials) */
};

/*
 * GF details include:
 *  ->	the main body of the GF file (all bytes save pre- and post-amble);
 *  ->	global box boundaries;
 * and	character locators, addressed by `character residue'.  Empty
 *	slots are indicated by a -1 `pointer'.
 */
struct gf_details {
	char	*gd_body;		/* GF body */
	char	*gd_base;		/* == gd_body - preamble_size */
	struct	bounds gd_gb;		/* global boundaries */
	struct	char_loc gd_cl[256];	/* character locators */
};

/*
 * Get the gf_details from font f.
 */
#define	ftogd(f) ((struct gf_details *) (f)->f_details)

extern	int errno;
char	*malloc(), *copyit();

static int gf_read();
static int gf_read();
static int gf_rasterise();
static int gf_freefont();

/*
 * I am making the assumption that 530 bytes will always be enough
 * to find the end of the GF file.  12 should suffice, as there
 * should be at most seven GF_FILLER bytes, preceded by the GF ID,
 * preceded by the four byte postamble pointer; but at least one
 * VMS TeX pads pads DVI files to a full `sector', so I am assuming
 * it may do the same to GF files.
 */
#ifdef vms
#define	POSTSIZE	530	/* make only VMS pay for its ways; */
#else
#define	POSTSIZE	16	/* others get to use something reasonable */
#endif

/*
 * Find the GF postamble.  Store the offsets of the POST and POSTPOST
 * opcodes through postp and postpostp.
 */
static
findGFpostamble(fd, postp, postpostp)
	int fd;
	long *postp, *postpostp;
{
	register long offset;
	register char *p;
	register int i;
	register i32 n;
	char postbuf[POSTSIZE];

	/*
	 * Avoid lseek()ing beyond beginning of file; it may give odd
	 * results.  Read the last POSTSIZE bytes (or however many we
	 * can get).
	 */
	offset = lseek(fd, 0L, 2) - (long) POSTSIZE;
	if (offset < 0L)
		offset = 0L;
	(void) lseek(fd, offset, 0);
	i = read(fd, postbuf, POSTSIZE);
	if (i <= 0)
		return (-1);
	p = &postbuf[i];
	i -= 4;			/* account for the pointer in advance */

	/*
	 * Now search backwards for the GF_ID byte.  The postamble
	 * pointer will be four bytes behind that.
	 */
	while (--i >= 0) {
		if (UnSign8(*--p) == GF_ID)
			goto foundit;
		if (UnSign8(*p) != GF_FILLER)
			break;
	}
	return (-1);		/* cannot find postamble ptr */

foundit:
	/*
	 * Store the (presumed) position of the POSTPOST byte, which
	 * is i-1 bytes beyond `offset'.
	 */
	*postpostp = offset + i - 1;

	/*
	 * Read out the postamble pointer and seek to the postamble,
	 * also saving the offset.
	 */
	p -= 4;
	pGetLong(p, n);
	*postp = offset = n;
	(void) lseek(fd, offset, 0);
	return (0);		/* made it */
}

/*
 * Read a GF file.
 */
static int
gf_read(f)
	register struct font *f;
{
	register struct gf_details *gd;
	register char *p;
	register struct char_loc *cl;
	register int i;
	int fd, presize, postsize, bodysize, firstc, lastc;
	char *postamble;
	long postaddr, postpostaddr;
	i32 lasteoc;
	char *problem = NULL;
	struct stat st;
	char b[4];
	int saverr;

	if ((fd = open(f->f_path, 0)) < 0)
		return (-1);
	gd = NULL;		/* prepare for failure */
	postamble = NULL;

	/*
	 * The file had best be at least 50 bytes long.  A
	 * `completely empty' GF file might consist of a PRE, a GF_ID,
	 * no comment (one zero byte), then: POST, pointer to last
	 * EOC, design size, checksum, hppp, vppp, min_m, max_m,
	 * min_n, max_n, POSTPOST, pointer to POST, GF_ID, and four
	 * FILLERs.
	 */
	(void) fstat(fd, &st);
	if (st.st_size < 50) {	/* too small to be a GF file */
		problem = "file is too short";
		goto fail;
	}

	/*
	 * Read the very beginning and pick up the preamble size.
	 */
	if (read(fd, b, 4) != 4)
		goto fail;
	if (UnSign8(b[0]) != GF_PRE) {
		problem = "file does not begin with PRE";
		goto fail;
	}
	i = UnSign8(b[1]);
	if (i != GF_ID)
		error(0, 0, "Warning: strange GF id (%d) in \"%s\"", i,
			f->f_path);
	presize = 3 + UnSign8(b[2]);

	/*
	 * Find the postamble, allocate space, and read it in.
	 */
	if (findGFpostamble(fd, &postaddr, &postpostaddr)) {
		problem = "cannot find postamble";
		goto fail;
	}
	postsize = postpostaddr - postaddr + 1;
	if ((p = malloc(postsize)) == NULL)
		goto fail;
	if (read(fd, p, postsize) != postsize)
		goto fail;
	postamble = p;
	
	/*
	 * Make sure we found it.
	 */
	if (pgetbyte(p) != GF_POST) {
		problem = "no GF_POST at postamble";
		goto fail;
	}

	/*
	 * Looks okay.  Allocate detail space and poke through the postamble.
	 */
	if ((gd = (struct gf_details *) malloc(sizeof (*gd))) == NULL)
		goto fail;
	gd->gd_body = NULL;

	pGetLong(p, lasteoc);	/* actually one past last EOC */
	pGetLong(p, f -> f_design_size );
	pGetLong(p, f->f_checksum);
	pGetLong(p, f -> f_hppp);
	pGetLong(p, f -> f_vppp);

	pGetLong(p, gd->gd_gb.min_m);
	pGetLong(p, gd->gd_gb.max_m);
	pGetLong(p, gd->gd_gb.min_n);
	pGetLong(p, gd->gd_gb.max_n);

	/*
	 * Zap all the character locators, then read those that are
	 * defined in the postamble.  Remember the first and last
	 * characters so that we know which glyphs are defined.  Lastc
	 * is actually the last-plus-one'th character.
	 */
	for (cl = gd->gd_cl, i = 256; --i >= 0; cl++)
		cl->cl_p = -1;
	firstc = 256;
	lastc = 0;
	for (;;) {
		i32 dx, dy;

		switch (pgetbyte(p)) {

		case GF_CHAR_LOC:
			i = pgetbyte(p);
			pGetLong(p, dx);
			pGetLong(p, dy);
			goto merge;

		case GF_CHAR_LOC0:
			i = pgetbyte(p);
			dx = ((i32) pgetbyte(p)) << 16;
			dy = 0;
merge:
			if (i < firstc)
				firstc = i;
			if (i >= lastc)
				lastc = i + 1;
			cl = &gd->gd_cl[i];
			cl->cl_dx = dx;
			cl->cl_dy = dy;
			pGetLong(p, cl->cl_w);
			pGetLong(p, cl->cl_p);
			break;

		case GF_POSTPOST:
			goto done;

		default:
			error(0, 0, "I do not understand %d here",
				UnSign8(p[-1]));
			problem = "unexpected opcode in postamble";
			goto fail;
		}
	}
done:
	free(postamble);
	postamble = NULL;	/* all done with it */

	/*
	 * Alas, we need the instructions whether or not we need
	 * the rasters, since the raster bounding box information
	 * can only be properly determined by converting the rasters.
	 * Compute the size of the main body of the GF file, then
	 * read it in.
	 */
	bodysize = lasteoc - presize;
	if ((gd->gd_body = malloc(bodysize + 1)) == NULL)
		goto fail;
	(void) lseek(fd, (long) presize, 0);
	if (read(fd, gd->gd_body, bodysize) != bodysize)
		goto fail;
	/*
	 * The next byte might be a special, so we just
	 * arbitrarily stuff in a POST.
	 */
	gd->gd_body[bodysize] = GF_POST;
	gd->gd_base = gd->gd_body - presize;

	f->f_details = (char *) gd;
	if (FontHasGlyphs(f, firstc, lastc))
		goto fail2;
	(void) close(fd);
	return (0);

fail:
	if (problem == NULL)
		error(0, errno, "trouble reading \"%s\"", f->f_path);
	else
		error(0, 0, "%s\n\t(are you sure \"%s\" is a GF file?)",
			problem, f->f_path);
	errno = 0;
fail2:
	saverr = errno;
	if (postamble != NULL) {
		free(postamble); postamble = 0;
	      }
	if (gd != NULL) {
		if (gd->gd_body != NULL) {
			free(gd->gd_body); gd -> gd_body = 0;
		      }
		free((char *) gd); gd = 0;
	}
	(void) close(fd);
	errno = saverr;
	return (-1);
}

/*
 * Some global variables, used while building rasters.  (These are
 * referenced also in copyit(), so must be global.  Fortunately, no
 * one yet requires the font routines to be re-entrant.)
 */
static char *buildraster;	/* raster being built */
static int buildsize;		/* size of buildraster (bytes) */

static struct bounds tempb;	/* bounds used during buildraster */
static struct bounds ob;	/* observed bounds */

/*
 * Bit tables: `left' and `right' bits.  lbits[b] has all the bits
 * that are to the left of bit b set; rbits[b] has all the bits
 * that are to the right of bit b set, as well as bit b itself.
 */
static char lbits[] = { 0x00, 0x80, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc, 0xfe };
static char rbits[] = { 0xff, 0x7f, 0x3f, 0x1f, 0x0f, 0x07, 0x03, 0x01 };

/*
 * The magic address `nullraster' is known in getgly() as a valid
 * but empty raster, and changed there to NULL.  A NULL return from
 * drawchar indicates failure; we need something to distinguish the
 * empty raster.
 */
static char nullraster[1];

/*
 * `Inline functions':
 *  ->	convert a bit number to a byte number (round down);
 *  ->	convert a number of bits to a number of bytes (round up);
 * and	convert a bit to a bit index.
 */
#define btoby(b) ((b) >> 3)
#define	btonb(b) (((b) + 7) >> 3)
#define	btobi(b) ((b) & 7)

/*
 * Helper function for getgly: build the raster, and compute its
 * minimal bounding box.  Called with `p' pointing past the backpointer
 * field of the BOC command (i.e., at min_m or del_m).  `abbrev' is true
 * iff this was a BOC1.  `globalb' are the global bounds from the GF file,
 * whose name is pointed to by gfname.
 */
static char *
drawchar(p, abbrev, globalb, gfname)
	register char *p;
	int abbrev;
	struct bounds globalb;
	char *gfname;
{
	register i32 m;		/* m register (column) */
	register char *colp;	/* pointer to byte corresponding to m */
	register int c;		/* temporary */
	register i32 i;		/* temporary */
	register int black;	/* true when paint_switch==black */
	register i32 n;		/* n register (row) */
	int stride;		/* multiplier to convert linear to 2d array */
	int wrotethisrow;	/* true iff we wrote in the current row */
	char *virtrast;		/* virtual origin version of buildraster */
	int mustcopy;		/* true if we must copy the built raster */
	struct bounds gb;	/* bounds from the GF file */

	/* get the bounds */
	if (abbrev) {
		c = pgetbyte(p);/* del_m */
		gb.min_m = (gb.max_m = pgetbyte(p)) - c;
		c = pgetbyte(p);/* del_n */
		gb.min_n = (gb.max_n = pgetbyte(p)) - c;
	} else {
		pGetLong(p, gb.min_m);
		pGetLong(p, gb.max_m);
		pGetLong(p, gb.min_n);
		pGetLong(p, gb.max_n);
	}

	/*
	 * Trim the GF bounds according to the global bounds.  We
	 * use the trimmed values to allocate the build space.
	 */
	tempb = gb;
#define	GB_ADJ(field, cmp) \
	if (tempb.field cmp globalb.field) \
		tempb.field = globalb.field
	GB_ADJ(min_m, <);
	GB_ADJ(max_m, >);
	GB_ADJ(min_n, <);
	GB_ADJ(max_n, >);
#undef GB_ADJ

	/*
	 * Compute the distance between rows (the number of bytes per
	 * column), then make sure there is room in the build space
	 * for [min_n..max_n] of these, possibly by allocating a new raster.
	 */
	stride = btonb(tempb.max_m - tempb.min_m + 1);
	c = stride * (tempb.max_n - tempb.min_n + 1);
	if (c <= 0)		/* completely empty character */
		return (nullraster);
	if (c > buildsize) {
		if (buildraster != NULL) {
			free(buildraster); buildraster = 0;
		      }
		if ((buildraster = malloc(c)) == NULL) {
			buildsize = 0;
			return (NULL);
		}
		buildsize = c;
	}

	/*
	 * If we are using an old raster that is too big, remember to
	 * scrunch it down later.
	 */
	mustcopy = buildsize > c;

	/* clear the raster to white */
	bzero(buildraster, c);

	/*
	 * Make a virtual origin raster pointer.  The virtual origin is
	 * where raster[0][0] is, whether or not there is a raster[0][0].
	 * Normally, this would be
	 *	&buildraster[-gb.min_n * stride - btoby(gb.min_m)],
	 * but it is complicated by two things.  Firstly, we would like
	 * n==max_n to be the bottommost point in the raster (low
	 * addresses), and n==min_n to be the topmost (high addresses).
	 * In other words, we need to reflect the n (Y) values about
	 * the X axis: negate them.  Secondly, the raster we create
	 * must be `flush left'.  That is, somewhere along its rows,
	 * bit 0x80 must be set at the left edge of one of its columns.
	 * We need to subtract away the minimum bit index before
	 * calculating bit values.  This cannot really be done in
	 * advance, since we cannot address bits directly.
	 */
	virtrast = &buildraster[gb.max_n * stride];

	/*
	 * Set up the bounds-trimming variables.  The observed m bounds
	 * are kept offset by gb.min_m until we finish drawing the
	 * character.
	 */
	ob.min_m = tempb.max_m - gb.min_m + 1;
	ob.max_m = tempb.min_m - gb.min_m - 1;
	ob.min_n = tempb.max_n + 1;
	ob.max_n = tempb.min_n - 1;
	wrotethisrow = 0;

#define FIX_N_BOUNDS() { \
	if (wrotethisrow) { \
		c = -n; /* recall that n is reflected about X axis */ \
		if (c < ob.min_n) \
			ob.min_n = c; \
		if (c > ob.max_n) \
			ob.max_n = c; \
		wrotethisrow = 0; \
	} \
}

	/*
	 * Initialise state variables: m = min_m, n = max_n,
	 * paint_switch = white.
	 */
	m = 0;			/* gb.min_m - gb.min_m */
	n = -gb.max_n;		/* reflected */
	colp = &virtrast[n * stride];
if (colp != buildraster)
panic("gffont drawchar colp != buildraster");
	black = 0;

	/*
	 * Now interpret the character.
	 * `for (;;)' pushes everything off the right edge, alas.
	 */
more:
	c = pgetbyte(p);
	if (GF_IsPaint(c))	/* faster? */
		goto paint;
	switch (GF_OpLen(c)) {

	case GPL_NONE:
		break;

	case GPL_UNS1:
		i = pgetbyte(p);
		break;

	case GPL_UNS2:
		pGetWord(p, i);
		break;

	case GPL_UNS3:
		pGet3Byte(p, i);
		break;

	case GPL_SGN4:
		pGetLong(p, i);
		break;

	default:
		panic("gffont drawchar GF_OpLen(%d) = %d", c, GF_OpLen(c));
		/* NOTREACHED */
	}

	switch (GF_TYPE(c)) {

	case GT_PAINT0:
paint:
		i = c - GF_PAINT_0;
		/* FALLTHROUGH */

	case GT_PAINT:
		/*
		 * Paint `i' bits in the current row at columns [m..m+i).
		 */
		if (i && black) {
			/* remember to adjust n bounds later */
			wrotethisrow = 1;
			/* adjust minimum m bound */
			if (m < ob.min_m)
				ob.min_m = m;

			/*
			 * Finish the partial byte at colp.  There are 8-k
			 * bits to set to finish it, where k is the bit
			 * index value from m.  If we need to set fewer
			 * than 8-k bits, set them now and skip the rest
			 * of this.
			 */
			c = 8 - btobi(m);
			if (i < c) {	/* cannot finish it off */
				*colp |= UnSign8(lbits[i]) >> btobi(m);
				m += i;
			} else {	/* finish it off */
				*colp++ |= rbits[btobi(m)];
				i -= c;
				m += c;

				/*
				 * Update m to reflect having written the
				 * remaining i bits, then write them.
				 * First write all the full bytes, then
				 * start a partial byte with whatever
				 * is left over, if anything.
				 */
				m += i;
				i >>= 3;
				while (--i >= 0)
					*colp++ = 0xff;
				*colp |= lbits[btobi(m)];
			}

			/* adjust maximum m bound */
			if (m > ob.max_m)
				ob.max_m = m;
		} else {
			/*
			 * Add the bit index so that we round up whenever
			 * this fills the partial byte at colp.
			 */
			colp += (i + btobi(m)) >> 3;
			m += i;
		}
		black = !black;
		break;

	case GT_EOC:		/* all done */
		FIX_N_BOUNDS();
		goto done;

	case GT_SKIP0:		/* skip no rows */
		i = 0;
		/* FALLTHROUGH */

	case GT_SKIP:		/* skip some rows, draw white */
		m = 0;
		black = 0;
		goto skip_or_new_row;

	case GT_NEW_ROW:	/* next row near left col, draw black */
		m = c - GF_NEW_ROW_0;
		black = 1;
		i = 0;			/* n offset is 1: skip no rows */
skip_or_new_row:
		FIX_N_BOUNDS();
		n += i + 1;		/* += because reflected */
		colp = &virtrast[n * stride + btoby(m)];
		break;

	case GT_XXX:		/* special */
		p += i;
		break;

	case GT_YYY:		/* numspecial */
		break;

	case GT_NOP:		/* dull */
		break;

	case GT_BOC:		/* should not appear */
	case GT_BOC1:
	case GT_CHAR_LOC:
	case GT_CHAR_LOC0:
	case GT_PRE:
	case GT_POST:
	case GT_POSTPOST:
	case GT_UNDEF:
		error(0, 0, "unexpected GF opcode %d", c);
		error(1, 0, "bad GF file \"%s\"", gfname);
		/* NOTREACHED */

	default:
		panic("gffont drawchar GF_TYPE(%d) = %d", c, GF_TYPE(c));
		/* NOTREACHED */
	}
	goto more;

done:
	/*
	 * The observed bounds `m' values are both off by gb.min_m, so
	 * fix them now.  CONSIDER ADJUSTING n HERE TOO
	 */
	ob.min_m += gb.min_m;
	ob.max_m += gb.min_m;

	/*
	 * If we used too much memory for the raster, copy it now.
	 */
	if (mustcopy || tempb.min_n != ob.min_n || tempb.max_n != ob.max_n ||
	    btonb(ob.max_m - ob.min_m + 1) != stride)
		return (copyit());

	/*
	 * If the left column bounds match, just move the raster in place.
	 */
	if (tempb.min_m == ob.min_m) {
		p = buildraster;
		buildraster = NULL;
		buildsize = 0;
		return (p);
	}

	/*
	 * The raster must be copied, but only because it is not
	 * `left justified'.
	 *
	 * CONSIDER DEFERRING THIS PHASE UNTIL THE RASTER IS USED
	 */
	return (copyit());
}

/*
 * Copy the built raster to newly allocated space.
 * We may need to shift all the bits left as well.
 */
char *
copyit()
{
	register char *o, *p;
	register int i, oldoff, newoff;
	char *n;

	/*
	 * Compute the observed minimum stride.  If it is zero or negative,
	 * there is no raster at all, else allocate just enough space
	 * to hold the new raster.
	 */
	newoff = btonb(ob.max_m - ob.min_m + 1);
	if (newoff <= 0)
		return (nullraster);
if (ob.max_n < ob.min_n)
panic("gffont copyit max_n < min_n");
	n = malloc((unsigned) (newoff * (ob.max_n - ob.min_n + 1)));
	if ((p = n) == NULL)
		return (NULL);

	/*
	 * Compute the old stride.
	 */
	oldoff = btonb(tempb.max_m - tempb.min_m + 1);
if (oldoff < newoff)
panic("gffont copyit oldoff < newoff");

	/*
	 * Point at the old raster, then add the offset to the first
	 * written row, and then the offset to the first written column.
	 */
	o = buildraster;
	o += (ob.max_n - tempb.max_n) * oldoff;
	i = ob.min_m - tempb.min_m;
	o += btoby(i);

	/*
	 * Now copy each row, doing shifting if required.
	 */
	if ((i = btobi(i)) != 0) {	/* must shift, alas */
		register int r = 8 - i, j, k;

		oldoff -= newoff;
		for (k = ob.max_n; k >= ob.min_n; k--) {
			for (j = newoff; --j >= 0;) {
				*p++ = *o++ << i;
				p[-1] |= UnSign8(*o) >> r;
			}
			o += oldoff;
		}
	} else if (oldoff > newoff) {	/* compressing columns */
		for (i = ob.max_n; i >= ob.min_n; i--) {
			bcopy(o, p, newoff);
			o += oldoff;
			p += newoff;
		}
	} else				/* just squeezing out extra rows */
		bcopy(o, p, newoff * (ob.max_n - ob.min_n + 1));

	/* finally, return the copy */
	return (n);
}

/*
 * Obtain the specified range of glyphs.
 */
static int
gf_getgly(f, l, h)
	register struct font *f;
	int l, h;
{
	register struct glyph *g;
	register struct char_loc *cl;
	register char *p;
	register i32 c;
	register int i;
	register i32 thisboc;
	int abbrev;
	struct gf_details *gd = ftogd(f);

	/*
	 * For each glyph, make sure there exists an instance of that
	 * character residue.  Go find the actual glyph (which may be
	 * arbitrarily far down a chain of pointers) and get its info.
	 */
	for (cl = &gd->gd_cl[i = l]; i < h; i++, cl++) {
		g = f->f_gly[i];
		thisboc = cl->cl_p;

		/*
		 * Screw around locating the character for real.
		 */
		while (thisboc != -1) {
			p = gd->gd_base + thisboc;
skip:
			c = pgetbyte(p);
			switch (GF_TYPE(c)) {

			case GT_XXX:
				switch (GF_OpLen(c)) {

				case GPL_UNS1:
					c = pgetbyte(p);
					break;

				case GPL_UNS2:
					pGetWord(p, c);
					break;

				case GPL_UNS3:
					pGet3Byte(p, c);
					break;

				case GPL_SGN4:
					pGetLong(p, c);
					break;

				default:
					panic("gf_getgly GF_OpLen(%d) = %d",
						c, GF_OpLen(c));
					/* NOTREACHED */
				}
				p += c;
				goto skip;

			case GT_YYY:
				p += 4;
				goto skip;

			case GT_BOC:
				abbrev = 0;
				pGetLong(p, c);
				break;

			case GT_BOC1:
				abbrev = 1;
				c = pgetbyte(p);
				break;

			default:
				error(0, 0, "GF code %d; I expected BOC", c);
				error(1, 0, "bad GF file \"%s\"", f->f_path);
				/* NOTREACHED */
			}
			/*
			 * Found a BOC.  If it is the right character,
			 * go handle it.
			 */
			if (c == i)
				goto handleit;
			if ((c & 255) != i) {
				error(0, 0, "%d != %d mod 256", c, i);
				error(1, 0, "Bad GF file \"%s\"", f->f_path);
			}
			/*
			 * Follow the backpointer.
			 */
			if (abbrev)
				thisboc = -1;
			else
				pGetLong(p, thisboc);
		}

		/*
		 * If we get here, the glyph is not valid after all.
		 */
		continue;
	
		/*
		 * The glyph is okay.  Set it up.
		 */
handleit:
		g->g_flags = GF_VALID;
		g->g_xescapement = cl->cl_dx;
		g->g_yescapement = cl->cl_dy;
		g->g_tfmwidth = cl->cl_w;
		g -> g_rawtfmwidth = g -> g_tfmwidth;
		if (!abbrev)
			p += 4;		/* skip backpointer */
		if ((p = drawchar(p, abbrev, gd->gd_gb, f->f_path)) == NULL)
			return (-1);	/* ??? */
		if (p == nullraster)
			p = NULL;
		/* set height &c based on observed bounds */
		g->g_height = ob.max_n - ob.min_n + 1;
		g->g_width = ob.max_m - ob.min_m + 1;
		g->g_yorigin = ob.max_n;
		g->g_xorigin = -ob.min_m;
		g->g_raster = p;
		g->g_rotation = ROT_NORM;
	}
	return (0);
}

/*
 * Obtain rasters for the specified glyphs.  We did this above, while
 * adjusting the bounding boxes, so this routine should never get called.
 */
static int
gf_rasterise(f, l, h)
	struct font *f;
	int l, h;
{

	panic("gf_rasterise(%s, %d, %d)", f->f_path, l, h);
}

/*
 * Discard the font details.
 */
static
gf_freefont(f)
	struct font *f;
{
	struct gf_details *gd;

	if ((gd = ftogd(f)) != NULL) {
		free(gd->gd_body); gd -> gd_body = 0;
		free((char *) gd); gd = 0;
	}
}
