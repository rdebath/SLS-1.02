/* xdvi.h: global declarations.  config.h must be included before this.
   Originally written by Eric C. Cooper, CMU.  */

#ifndef XDVI_H
#define XDVI_H

#include "lib.h"
#include "types.h"

/* This should be called only after a system call fails.  */
#define FATAL_PERROR(s) do { perror (s); exit (errno); } while (0)


#define START_FATAL() do { fprintf (stderr, "fatal: ")
#define END_FATAL() fprintf (stderr, ".\n"); exit (1); } while (0)

#define FATAL(str) START_FATAL (); fprintf (stderr, "%s", str); END_FATAL ()


/* This is the maximum number of numerals that result when an integer is
   converted to a string, plus one for a trailing null.  */
#define MAX_INT_LENGTH  11

#define STREQ(s1, s2) (strcmp (s1, s2) == 0)

/* Return S with the suffix SUFFIX, removing any suffix already present.
   For example, `make_suffix ("/foo/bar.baz", "karl")' returns
   `/foo.bar.karl'.  This returns a string allocated with malloc.  */
extern string make_suffix (/* string s, string suffix */);



#define	Printf	(void) printf
#define	Fprintf	(void) fprintf
#define	Sprintf	(void) sprintf
#define	Fseek	(void) fseek
#define	Fread	(void) fread
#define	Fputs	(void) fputs
#define	Putc	(void) putc
#define	Putchar	(void) putchar
#define	Fclose	(void) fclose
#define	Strcpy	(void) strcpy

unsigned long num();
long snum();

#define one(fp)		((unsigned long) getc(fp) & 0xff)
#define sone(fp)	((long) getc(fp))
#define two(fp)		num (fp, 2)
#define stwo(fp)	snum(fp, 2)
#define four(fp)	num (fp, 4)
#define sfour(fp)	snum(fp, 4)

typedef	unsigned char ubyte;
#define	MAXDIM		32767

/*
 *	pixel_conv is currently used only for converting absolute positions
 *	to pixel values; although normally it should be
 *		((int) ((x) / shrink_factor + (1 << 15) >> 16)),
 *	the rounding is achieved instead by moving the constant 1 << 15 to
 *	PAGE_OFFSET in dvi_draw.c.
 */
#define	pixel_conv(x)	((int) ((x) / shrink_factor >> 16))
#define	pixel_round(x)	((int) ROUNDUP(x, shrink_factor << 16))
#define	spellfour(f)	((long) (sfour(f) * fraction))
#define	spellnum(f,n)	((long) (snum(f,n) * fraction))

#ifdef X10
#undef	MSBITFIRST
#undef	BMLONG
#define	BMSHORT
#endif

#ifdef	BMLONG
#define	BMUNIT			unsigned long
#define	BITS_PER_BMUNIT		32
#define	BYTES_PER_BMUNIT	4
#else	/* BMLONG */
#ifdef	BMSHORT
#define	BMUNIT			unsigned short
#define	BITS_PER_BMUNIT		16
#define	BYTES_PER_BMUNIT	2
#else	/* BMSHORT */
#define	BMUNIT			unsigned char
#define	BITS_PER_BMUNIT		8
#define	BYTES_PER_BMUNIT	1
#endif	/* BMSHORT */
#endif	/* BMLONG */

#define	ADD(a, b)	((BMUNIT *) (((char *) a) + b))
#define	SUB(a, b)	((BMUNIT *) (((char *) a) - b))

extern	BMUNIT	bit_masks[BITS_PER_BMUNIT + 1];

struct frame {
	long dvi_h, dvi_v, w, x, y, z;
	int pxl_v;
};

extern	struct frame 	*stack;
extern	struct frame 	*stackp;

/* entries below with the characters 'dvi' in them are actually stored in
   scaled pixel units */

#define DVI_H   stackp->dvi_h
#define PXL_H   pixel_conv(stackp->dvi_h)
#define DVI_V   stackp->dvi_v
#define PXL_V   stackp->pxl_v
#define WW      stackp->w
#define XX      stackp->x
#define YY      stackp->y
#define ZZ      stackp->z
#define ROUNDUP(x,y) (((x)+(y)-1)/(y))

extern	int	current_page;
extern	int	total_pages;
extern	double	fraction;
extern	int	maxstack;
extern	int	n_fonts_left;		/* for LRU management of fonts */
extern	time_t	dvi_time;		/* last mod. time for dvi file */
extern	int	page_w, page_h;

/*
 * Table of page offsets in DVI file, indexed by page number - 1.
 * Initialized in prepare_pages().
 */
extern	long	*page_offset;

/*
 * Mechanism for reducing repeated warning about specials, lost characters, etc.
 */
extern	boolean	hush_spec, hush_spec_now;
extern	boolean	hush_chars;

/* Font and character data structures.  */

/* Function type for unpacking the bitmap.  */
typedef	void (*unpack_bitmap_proc_ptr) P1H(ubyte ch);
extern unpack_bitmap_proc_ptr gf_unpack, pk_unpack;


/* Bitmap structure for raster ops.  */
struct bitmap
{
  short w, h;		/* width and height in pixels */
  short bytes_wide;	/* scan-line width in bytes */
  char *bits;		/* pointer to the bits */
};


/* Per-character information. There is one of these for each character
   in each font.  All fields except the bitmaps are filled in at font
   definition time.  The bitmaps are computed from the packed data when
   they are needed.  */

struct glyph
{
  long dvi_adv;		/* DVI units to move reference point */
  char *packed_data;    /* bitmap before unpacking */
  short x, y;		/* x and y offset in pixels */
  struct bitmap bitmap;	/* bitmap for character */
  short x2, y2;		/* x and y offset in pixels (shrunken bitmap) */
  struct bitmap bitmap2;/* shrunken bitmap for character */
};


/* Per-font information. There is one of these for every font in the DVI
   file.  The design size is in units of 1/2^20 points (micropoints),
   and the individual character widths are in the TFM file in 1/2^20 em
   units, i.e., they are relative to the design size.
   We then change the sizes to SPELL units (unshrunk pixel / 2^16).  */

struct font
{
  struct font *next;		/* link to next font info block */
  int TeXnumber;		/* font number (in DVI file) */
  int scale;			/* scaled size in SPELL units */
  char *fontname;		/* from the dvi file */
  char *filename;		/* full name on the filesystem */
  ubyte maxchar;		/* largest character code */
  unpack_bitmap_proc_ptr unpack_bitmap; /* function to unpack bitmap  */
  struct glyph glyph[256];
};

/* While we're at it, let's declare the functions which read a font.  */
typedef void (*read_glyphs_proc_ptr) P2H(FILE *, struct font *);
extern read_glyphs_proc_ptr gf_glyphs, pk_glyphs;

/* We keep the current font and its maximum number of characters around
   in globals.  */
extern struct font *current_font;
extern ubyte maxchar;

/* Free the storage for the shrunken bitmaps.  */
extern void free_shrunken_fonts P1H(void);

/* Command line flags.  */

extern	int	debug;

#define DBG_BITMAP	0x1
#define DBG_DVI		0x2
#define DBG_PK          0x4
#define DBG_BATCH       0x8
#define	DBG_EVENT	0x10
#define	DBG_OPEN	0x20
#define DBG_ALL		(DBG_BITMAP|DBG_DVI|DBG_PK|DBG_EVENT|DBG_OPEN)

extern	boolean	list_fonts;

extern	int	pixels_per_inch;
extern	int	offset_x, offset_y;
extern	int	unshrunk_paper_w, unshrunk_paper_h;
extern	int	unshrunk_page_w, unshrunk_page_h;
extern	int	density;

extern	char	*dvi_name;
extern	FILE	*dvi_file;				/* user's file */
extern	char	*prog;

extern	struct	WindowRec {
	caddr_t	win;		/* type Window is not defined yet */
	int	shrinkfactor;
	int	base_x, base_y;
	int	width, height;
	int	min_x, max_x, min_y, max_y;	/* for pending expose events */
} mane, alt, curr;

#define	WINDOW(wr)	((Window) (wr).win)
#define	shrink_factor	curr.shrinkfactor

#ifndef EXTERN
#define EXTERN extern
#endif
EXTERN jmp_buf	dvi_env;	/* mechanism to communicate dvi file errors */

#endif /* not XDVI_H */
