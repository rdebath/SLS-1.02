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
 * Font file information, readers, etc.
 */

#ifndef _CTEX_TYPES_
#include "types.h"
#endif

/*
 * First, font independent information: per glyph info, and per font
 * info.
 */
struct glyph {
	short	g_flags;	/* see below */
	/*
	 * The following cannot be used with GetRasterlessFont
	 */
	short	g_rotation;	/* see below */
	char	*g_raster;	/* raster, if known */
	/*
	 * These, however, do come with rasterless fonts,
	 * even though they relate only to the raster.
	 */
	i32	g_height;	/* height of bounding box */
	i32	g_width;	/* width of bounding box */
	i32	g_yorigin;	/* y origin (>= 0 -> within box) */
	i32	g_xorigin;	/* x origin (>= 0 -> within box) */
	/*
	 * This of course comes with every font.
	 */
	i32	g_rawtfmwidth;	/* the tfmwidth as read from the file */
	i32	g_tfmwidth;	/* width in scaled points (not FIXes!) */
	i32	g_xescapement;	/* x escapement (`chardx') */
	i32	g_yescapement;	/* y escapement (`chardy') */
	/*
	 * This is provided purely for DVI to device conversion programs.
	 */
	int	g_pixwidth;	/* width in pixels */
	/*
	 * Mainly for internal use, index is the glyph index within the
	 * font.  That is, f->f_gly[i]->g_index == i.
	 */
	int	g_index;	/* character index */
	/*
	 * g_details and g_integer are purely for the font reading
	 * subroutines to use however they will.  g_next makes lists
	 * of glyphs while the glyphs are free.
	 */
	union {			/* various options */
		char	*g_details;	/* details: arbitrary */
		i32	g_integer;	/* 32 bit integer */
		struct	glyph *g_next;	/* linked list */
	} g_un;
};

/*
 * Glyph flags.
 */
#define	GF_VALID	0x0001	/* glyph is `real' */
#define	GF_USR0		0x0100	/* reserved to user code */
#define	GF_USR1		0x0200	/* reserved to user code */
#define	GF_USR2		0x0400	/* reserved to user code */
#define	GF_USR3		0x0800	/* reserved to user code */

/*
 * Rotations are in quarter-pi steps, counterclockwise of course.
 * This order must be maintained; see rotate.c.
 */
#define	ROT_NORM	0		/* no rotation: `normal' position */
#define	ROT_LEFT	1		/* 1/4 turn counterclockwise */
#define	ROT_DOWN	2		/* 1/2 turn, i.e., upside-down */
#define	ROT_RIGHT	3		/* 3/4 turn ccw, or 1/4 turn cw */

struct font {
	int	f_flags;	/* see below */
	struct	fontops *f_ops;	/* operations */
	/*
	 * f_details is provided for font reading subroutines.
	 * It is intended to be cast to a pointer to a structure
	 * that is allocated by those routines, and used to keep
	 * track of whatever information those routines need to
	 * determine glyph boxes and (if asked for) rasters.
	 */
	char	*f_details;	/* type dependent stuff */
	/*
	 * f_path is the full pathname to the font file, filled in
	 * by GetFont and GetRasterlessFont.  Note that since we
	 * hold on to the path until the font is freed, it should be
	 * possible to cache glyph accesses on memory-poor machines.
	 */
	char	*f_path;	/* font file pathname */
	/*
	 * f_dvimag and f_dvidsz are the magnification and design size
	 * values from the DVI file.  f_font and f_scaled correspond to
	 * TeX's idea of the proper name for the font (e.g., `cmr10',
	 * `cmbx10 scaled 1440').  (Note that f_scaled is just the
	 * ratio of f_dvimag and f_dvidsz; you could save a bit of memory
	 * by eliminating it and altering the routine Font_TeXName()).
	 * f_checksum should be set by the font reading routines to
	 * the font checksum.  If the value is nonzero, it will be
	 * compared to the checksum in the DVI file.
	 */
	i32	f_dvimag;	/* magnification from DVI file */
	i32	f_dvidsz;	/* design size from DVI file */
	char	*f_font;	/* TeX's name for the font */
	int	f_scaled;	/* the ratio of dvimag to dvidsz, x1000 */
	i32	f_design_size;
	i32	f_checksum;	/* font checksum, or 0 */
	i32	f_hppp;		/* horizontal pixels per point */
	i32	f_vppp;		/* vertical pixels per point */
	/*
	 * f_lowch and f_highch bound the region in which f_gly
	 * indicies are valid.  Specificially, f_gly[i] may be
	 * read or written if and only if i is in the half-open
	 * interval [f_lowch..f_highch).  f_gly is an array of
	 * pointers to glyph structures.  The structures themselves
	 * are not allocated until requested.
	 *
	 * f_glybase is the actual return from malloc(), since it
	 * is theoretically possible for f_gly-f_lowch to become
	 * NULL.
	 */
	int	f_lowch;	/* first character */
	int	f_highch;	/* last character, plus 1 */
	struct	glyph **f_gly;	/* glyphs */
	struct	glyph **f_glybase;
};

/*
 * Font flags.
 */
#define	FF_RASTERS	0x0001	/* font has rasters */
#define	FF_USR0		0x0100	/* reserved to user code */
#define	FF_USR1		0x0200	/* reserved to user code */
#define	FF_USR2		0x0400	/* reserved to user code */
#define	FF_USR3		0x0800	/* reserved to user code */

/*
 * Operations on fonts.
 *
 * The `fo_dpitomag' field is used as a multiplier for a desired
 * resolution in dots per inch.  The result of the multiplication
 * is converted to a font name by multipying by 1000.0 and rounding.
 * The idea is that PXL files will have a multiplier of 5.0, and
 * others will have a multiplier of 1.0.  This suffices for the
 * present, at any rate; in the future, this field may be replaced
 * with something more general.
 *
 * N.B.: more operations may be added as they are discovered to be
 * useful.
 */
struct	fontops {
	char	*fo_name;		/* name, e.g., "gf" */
	double	fo_dpitomag;		/* multiplier */
	int	(*fo_read)();		/* open and read the font itself */
	int	(*fo_getgly)();		/* obtain specified glyphs (range) */
#ifdef notdef
	int	(*fo_freegly)();	/* release specified glyphs */
#endif
	int	(*fo_rasterise)();	/* rasterise specified glyphs */
	int	(*fo_freefont)();	/* discard font (free details) */
	struct	fontops *fo_next;	/* purely for font.c */
};

/*
 * Return a pointer to the glyph information for character `c' in
 * font `f'.
 */
#define	GLYPH(f, c) \
	((c) < (f)->f_lowch || (c) >= (f)->f_highch ? (struct glyph *) 0 : \
	 ((f)->f_gly[c] ? (f)->f_gly[c] : GetGlyph(f, c)))

/*
 * True iff glyph `g' is valid.  Useful for checking return values
 * from GLYPH().
 */
#define	GVALID(g)	((g) && ((g)->g_flags & GF_VALID))

/*
 * True iff glyph g has a raster.
 */
#define	HASRASTER(g)	((g)->g_height || (g)->g_width)

/*
 * Return a pointer to the raster information for glyph `g' in font
 * `f' at rotation `r'.
 */
#define	RASTER(g, f, r)	((g)->g_rotation == (r) && (g)->g_raster ? \
			 (g)->g_raster : GetRaster(g, f, r))

/*
 * Function types.
 */
struct	font *GetFont(), *GetRasterlessFont();
struct	glyph *GetGlyph();
char	*GetRaster();
void	FreeFont();
void	FreeGlyph();
void	FreeRaster();
char	*Font_TeXName();
double	DMagFactor();		/* from magfactor.c */

/*
 * Normally from stdio.h
 */
#ifndef NULL
#define	NULL	0
#endif

/*
 * The following environment variable overrides the default font
 * configuration file.  That default is used when fontinit() is not
 * called, or is passed a null pointer.
 */
#define	CONFENV	"TEXFONTDESC"
