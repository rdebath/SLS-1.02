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
 * Routines for working with fonts.  In particular, the configuration
 * dependent code is here.
 *
 * Specific fonts (GF, PXL, etc.) have functions in separate files.
 */

#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include "types.h"
#include "conv.h"
#include "font.h"

extern char *strsave();

/*
 * Define the default configuration file.
 * Also define the maximum path name length.
 */
#ifndef FONTDESC
	you need to define FONTDESC in the compile line
#endif

#define	PATHLEN	1024

/*
 * A font configuration.  The font list is ordered.
 *
 * A specifier is typically a particular print engine, since
 * different engines need slightly different fonts.
 */
struct fontconf {
	struct	fontconf *fc_next;
	struct	fontops *fc_ops;
	char	*fc_path;	/* path, with metacharacters */
	char	*fc_spec;	/* specifier */
	int	fc_slop;	/* slop value */
};

/*
 * EQ is a fast way to check for string equivalence.
 */
#define	EQ(a, b) (*(a) == *(b) && strcmp(a, b) == 0)

/*
 * Private variables.
 */
static	int didinit;		/* true => initialised already */
static	char *cfname;		/* config file name, for errors */
static	int cfline;		/* config file line, likewise */
static	struct fontops *fontops;/* font operations code: list head */
static	struct fontconf *fonts;	/* font list */
static	struct fontconf **nextfc;/* used during initialisation */
static	char spec_any[] = "*";	/* the `anything' specifier */

static readconf();
static setfont();
static badcf();
static struct font *getafont();	/* get a font and optional rasters */

/*
 * Here, alas, we know about all the kinds of fonts.
 * This also means that every DVI interpreter pulls in
 * the full set of font manipulation routines.
 *
 * PERHAPS THIS SHOULD BE CONFIGURABLE.
 */
#define	ADDFONT(x) { \
	extern struct fontops x; \
	x.fo_next = fontops; \
	fontops = &x; \
}

fontinit(file)
	char *file;
{

	if (didinit) {
		/*
		 * Could free the old configuration and fire up
		 * a new one, but for now . . .
		 */
		error(1, 0, "attempt to reinit fonts");
		/* NOTREACHED */
	}
	didinit++;
	ADDFONT(boxops);
	ADDFONT(blankops);
	ADDFONT(invisops);
	ADDFONT(pxlops);
	ADDFONT(pkops);
	ADDFONT(gfops);
	nextfc = &fonts;
	if (file == NULL)
		if ((file = getenv(CONFENV)) == NULL)
			file = FONTDESC;
	readconf(file);
}

/*
 * A proto resembles a fontspec (indeed, it is a prototype
 * fontspec) but is not quite the same.  It is used to gather
 * the information needed per fontspec before allocating
 * the fontspec itself.
 */
struct proto {
	char	*p_type;
	char	*p_spec;
	char	*p_slop;
	char	*p_path;
};

/*
 * Read the named configuration file.  The file is split into
 * lines, and lines are split into words; if the first word is
 * "font", this is a fontconf, and we read the remainder of the
 * words and make a fontconf entry.
 */
static
readconf(name)
	char *name;
{
	register FILE *f;	/* config file */
	register char **p;	/* pointer into word vector */
	register int c;
	char line[1024];	/* input line */
	char *v[100];		/* word vector */
	struct proto proto;	/* prototype fontconf */

#define GETWORD(x, ifnone) \
	if (--c <= 0) \
		badcf(ifnone); \
	else \
		(x) = *p++

	if ((f = fopen(name, "r")) == NULL)
		error(1, errno, "cannot read font configuration file \"%s\"",
			name);
	cfname = name;
	cfline = 0;
	while (fgets(line, sizeof (line), f) != NULL) {
		cfline++;
		if ((c = strlen(line)) > 0) {
			if (line[--c] != '\n')
				badcf("line too long");
			line[c] = 0;
		}
		if ((c = split(line, v, sizeof (v) / sizeof (*v))) < 0)
			badcf("too many words");
		p = v;
		/* skip things that are not fonts */
		if (c == 0 || !EQ(*p, "font"))
			continue;
		p++;
		GETWORD(proto.p_type, "missing font typename");
		GETWORD(proto.p_spec, "missing font spec (engine)");
		GETWORD(proto.p_slop, "missing slop value");
		GETWORD(proto.p_path, "need pathname");
		(void) setfont(&proto);
	}
}

/*
 * Find a font's operations, given its name.
 */
static struct fontops *
findops(name)
	register char *name;
{
	register struct fontops *fo;

	for (fo = fontops; fo != NULL; fo = fo->fo_next)
		if (EQ(fo->fo_name, name))
			return (fo);
	return (NULL);
}

/*
 * Turn a prototype fontconf into a real one.
 */
static int
setfont(p)
	register struct proto *p;
{
	register struct fontconf *fc;
	struct fontops *ops = findops(p->p_type);

	if (ops == NULL) {
		error(0, 0,
			"\"%s\", line %d: unknown font type \"%s\" ignored",
			cfname, cfline, p->p_type);
		return (-1);
	}
	if ((fc = (struct fontconf *) malloc(sizeof (*fc))) == NULL)
		error(1, errno,
			"out of memory for font configuration (sorry)");
	fc->fc_ops = ops;
	fc->fc_next = NULL;
	fc->fc_path = strsave(p->p_path);
	fc->fc_spec = EQ(p->p_spec, spec_any) ? NULL : strsave(p->p_spec);
	fc->fc_slop = atoi(p->p_slop);
	if (fc->fc_slop < 1)	/* quietly enforce proper slops */
		fc->fc_slop = 1;
	*nextfc = fc;
	nextfc = &fc->fc_next;
	return (0);
}

/*
 * Complain about a problem in the configuration file.
 */
static
badcf(why)
	char *why;
{

	error(1, 0, "\"%s\", line %d: %s", cfname, cfline, why);
	/* NOTREACHED */
}

/*
 * Turn a prototype path, name, and magnification into a full
 * path.
 */
static
pave(result, proto, name, mag)
	char *result, *proto, *name;
	int mag;
{
	register int c;
	register char *s, *d, *p;
	char num[30];

	d = result;
	p = proto;
	s = NULL;
	num[0] = 0;		/* will need changing for other bases */

	while (p != NULL) {
		/*
		 * If sourcing from s, take its next character, and
		 * insert it directly.  Otherwise take the next path
		 * character and interpret it.
		 */
		if (s != NULL) {
			if ((c = *s++) == 0) {
				s = NULL;
				continue;
			}
			goto put;
		}
		if ((c = *p++) == 0)
			p = NULL;
		if (c != '%')
			goto put;

		switch (c = *p++) {

		case 'f':
		case 'n':
		case 's':
			s = name;
			continue;

		case 'd':
		case 'm':
			if (num[0] == 0)
				(void) sprintf(num, "%d", mag);
			s = num;
			continue;

		case 0:
			c = '%';
			p--;
			/* FALLTHROUGH */
		}
put:
		if (d - result >= PATHLEN)
			error(1, 0, "font path `%s' too long (sorry)", proto);
		*d++ = c;
	}
}


/*
 * Given a font name and size, return the first font that fits, along
 * with its name (via fname).  If we cannot find such a font, we set
 * *fname to point to a `canonical' example font name, unless there are
 * are no fonts for the device, in which case we set *fname to NULL.
 */
struct font *
GetFont(nm, dvimag, dvidsz, dev, fname)
	char *nm;
	i32 dvimag, dvidsz;
	char *dev, **fname;
{

	return (getafont(nm, dvimag, dvidsz, dev, fname, 1));
}

/*
 * Same as GetFont, but caller promises never to ask for rasters.
 */
struct font *
GetRasterlessFont(nm, dvimag, dvidsz, dev, fname)
	char *nm;
	i32 dvimag, dvidsz;
	char *dev, **fname;
{

	return (getafont(nm, dvimag, dvidsz, dev, fname, 0));
}

/*
 * NEED TO THINK ABOUT gf NAMING CONVENTIONS HERE: ARE THEY LIKE pxl?
 * WHAT ABOUT OTHERS?
 */
static struct font *
getafont(nm, dvimag, dvidsz, dev, fname, wantrast)
	char *nm;
	i32 dvimag, dvidsz;
	char *dev, **fname;
	int wantrast;
{
	register int slop, fmag;
	register struct font *f;
	register struct fontconf *fc;
	register char *path;
	static char firstpath[PATHLEN], laterpath[PATHLEN];
	double mag;
	int scaled;

	extern Conv Conversion;

	if (!didinit)
		fontinit((char *) NULL);

	/*
	 * The equation below means, approximately, `the font is
	 * magnified by the ratio of the actual size dvimag to the
	 * design size dvidsz, and then further scaled by the
	 * global magnification.'  We multiply this by the printer's
	 * resolution in dots per inch, then use the per-font
	 * conversion factor to convert a dots-per-inch value to
	 * a font name `%m' magnification (extension).
	 */
	mag = (double) dvimag / (double) dvidsz;
	scaled = mag * 1000.0 + 0.5;
	mag *= Conversion.c_mag * Conversion.c_dpi;

	path = firstpath;
	for (fc = fonts; fc != NULL; fc = fc->fc_next) {
		if (dev != NULL && fc->fc_spec != NULL &&
		    !EQ(dev, fc->fc_spec))
			continue;
		fmag = mag * fc->fc_ops->fo_dpitomag + 0.5;
		for (slop = 0; slop < fc->fc_slop; slop++) {
			pave(path, fc->fc_path, nm, fmag + slop);
			if (access(path, R_OK) == 0)
				goto found;


			/* if someone could explain this, I'd appreicate it.
			   On ultrix 2.2, checking R_OK on a RO filesyste
			   fails, with the following errno. Why? */

			if ( errno == EROFS ) {
			  goto found;
			}

			if (slop) {
				pave(path, fc->fc_path, nm, fmag - slop);
				if (access(path, R_OK) == 0)
					goto found;
				if ( errno == EROFS ) {
				  goto found;
				}
			}
			path = laterpath;
		}
	}

	/* not found */
	if (path == firstpath) {	/* never got to try any paths */
		*fname = NULL;
		errno = ENXIO;
	} else {
		*fname = firstpath;
		errno = ENOENT;
	}
	return (NULL);

found:
	*fname = path;

	/* allocate space for the per-font info, and read it in */
	f = (struct font *) malloc(sizeof (struct font));
	if (f == NULL)
		return (NULL);
	f->f_flags = wantrast ? FF_RASTERS : 0;
	f->f_ops = fc->fc_ops;
	f->f_path = strsave(path);
	f->f_font = strsave(nm);
	f->f_dvimag = dvimag;
	f->f_dvidsz = dvidsz;
	f->f_scaled = scaled;
	f->f_checksum = 0;	/* in case the font reader cannot get one */
	errno = 0;
	if ((*f->f_ops->fo_read)(f)) {
		int e = errno;	/* paranoid */

		free(f->f_path); f -> f_path = 0;
		free(f->f_font); f -> f_font = 0;
		free((char *) f); f = 0;
		errno = e;
		return (NULL);
	}
	return (f);
}
