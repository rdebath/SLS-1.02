/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
/******************************************************************************
 * Description
 *	Display a wire-frame rotating icosahedron, with hidden lines removed
 *
 * Arguments:
 *	-r		display on root window instead of creating a new one
 *	=wxh+x+y	X geometry for new window (default 600x600 centered)
 *	host:display	X display on which to run
 * (plus a host of others, try -help)
 *****************************************************************************/
/* Additions by jimmc@sci:
 *  faces and colors
 *  double buffering on the display
 *  additional polyhedra
 *  sleep switch
 */

#include <stdio.h>
#include <unistd.h>
#define __USE_BSD
#include <string.h>
#include <stdlib.h>
#include <time.h>

#include "term.h"

#define DoRed	0x01
#define DoGreen	0x02
#define DoBlue	0x04

#define DELAY	10  /*0*/

typedef double Transform3D[4][4];

#include "polyinfo.h"	/* define format of one polyhedron */

/* Now include all the files which define the actual polyhedra */
Polyinfo polys[] = {
#include "allobjs.h"
};
int polysize = sizeof(polys)/sizeof(polys[0]);

typedef int Display;
typedef int GC;
typedef int Window;
typedef struct {
	short x,y;
} XPoint;

typedef struct {
	short x1,y1,x2,y2;
} XSegment;

typedef	struct {
	unsigned long pixel;			/* pixel value */
	unsigned short red, green, blue;	/* rgb value */
	char flags;				/* DoRed, DoGreen, DoBlue */
	char pad;
} XColor;

typedef struct {
	int prevX, prevY;
	unsigned long *plane_masks;	/* points into dbpair.plane_masks */
	unsigned long enplanemask;	/* what we enable for drawing */
	XColor *colors;		/* size = 2 ** totalplanes */
	unsigned long *pixels;	/* size = 2 ** planesperbuf */
} DBufInfo;

typedef struct {
	int planesperbuf;
	int pixelsperbuf;	/* = 1<<planesperbuf */
	int totalplanes;	/* = 2*planesperbuf */
	int totalpixels;	/* = 1<<totalplanes */
	unsigned long *plane_masks;	/* size = totalplanes */
	unsigned long pixels[1];
	int dbufnum;
	DBufInfo bufs[2];
	DBufInfo *drawbuf, *dpybuf;
} DBufPair;

DBufPair dbpair;

Display dpy;
GC gc;
XColor bgcolor,fgcolor;


char *ProgramName;
Window win;
int winWidth, winHeight;

int dsync = 0;
int dblbuf = 0;
int sleepcount = 0;
int isleepcount = 5;
int numcolors = 0;
char **colornames;	/* points into argv */
int dofaces = 0;
int doedges = 1;

Polyinfo *findpoly();

static char *help_message[] = {
	"where options include:",
	"    -display host:dpy                X server to use",
	"    -geometry geom                   geometry of window to use",
	"    -r                               draw in the root window",
	"    -d number                        dashed line pattern for wire frames",
	"    -colors color ...                codes to use on sides",
	"    -dbl                             use double buffering",
	"    -noedges                         don't draw wire frame edges",
	"    -faces                           draw faces",
	"    -i                               invert",
	"    -sleep number                    seconds to sleep in between draws",
	"    -obj objname                     type of polyhedral object to draw",
	"    -objhelp                         list polyhedral objects available",
	NULL};

void clean();
void	giveObjHelp();
void 	XDrawSegments();
void 	XSync();
void	icoClearArea();
void 	PartialNonHomTransform();
void	FormatRotateMat();
void 	IdentMat();
void 	ConcatMat();
void 	icoFatal();
void 	initDBufs();
void 	drawPoly();
char *xalloc();


/******************************************************************************
 * Description
 *	Main routine.  Process command-line arguments, then bounce a bounding
 *	box inside the window.  Call DrawIco() to redraw the icosahedron.
 *****************************************************************************/

int
main(argc, argv)
int argc;
char **argv;
{
	char *display = NULL;
	char *geom = NULL;
	int useRoot = 0;
	int fg, bg;
	int invert = 0;
	int dash = 0;
	int winX, winY, winW, winH;
	Polyinfo *poly;		/* the poly to draw */
	int icoX, icoY;
	int icoDeltaX, icoDeltaY;
	int icoW, icoH;

	ckmgrterm( *argv );
	signal(SIGTERM,clean);
	signal(SIGINT,clean);
	signal(SIGHUP,clean); 

	ProgramName = argv[0];

	/* Process arguments: */

	poly = findpoly("icosahedron");	/* default */

	while (*++argv) {
		if (!strcmp (*argv, "-display")) {
			display = *++argv;
		} else if (!strncmp (*argv, "-g", 2)) {
			geom = *++argv;
		} else if (**argv == '=') 		/* obsolete */
			geom = *argv;
		else if (!strcmp(*argv, "-r"))
			useRoot = 1;
		else if (!strcmp (*argv, "-d"))
			dash = atoi(*++argv);
		else if (!strcmp(*argv, "-colors")) {
			colornames = ++argv;
			for ( ; *argv && *argv[0]!='-'; argv++) ;
			numcolors = argv - colornames;
			--argv;
		}
		else if (!strcmp (*argv, "-dbl"))
			dblbuf = 1;
		else if (!strcmp(*argv, "-noedges"))
			doedges = 0;
		else if (!strcmp(*argv, "-faces"))
			dofaces = 1;
		else if (!strcmp(*argv, "-i"))
			invert = 1;
		else if (!strcmp (*argv, "-sleep"))
			sleepcount = atoi(*++argv);
		else if (!strcmp (*argv, "-isleep"))
			isleepcount = atoi(*++argv);
		else if (!strcmp (*argv, "-obj"))
			poly = findpoly(*++argv);
		else if (!strcmp(*argv, "-dsync"))
			dsync = 1;
		else if (!strcmp(*argv, "-objhelp")) {
			giveObjHelp();
			exit(1);
		}
		else {	/* unknown arg */
			char **cpp;

			fprintf (stderr, "usage:  %s [-options]\n\n", ProgramName);
			for (cpp = help_message; *cpp; cpp++) {
				fprintf (stderr, "%s\n", *cpp);
			}
			fprintf (stderr, "\n");
			exit (1);
		}
	}

	if (!dofaces && !doedges) icoFatal("nothing to draw");

	m_setup(0);
	m_ttyset();
	m_clear();
	/* Set up window parameters, create and map window if necessary: */

	winW = 1000;
	winH = 1000;
	winX = 0;
	winY = 0;

	if (dblbuf || dofaces) {
		initDBufs(fg,bg,1);
	}
	if (!numcolors) numcolors=1;

	if (dsync)
		XSync(dpy, 0);

	/* Get the initial position, size, and speed of the bounding-box: */

	icoW = icoH = 150;
	srandom((int) time(0) % 231);
	icoX = ((winW - icoW) * (random() & 0xFF)) >> 8;
	icoY = ((winH - icoH) * (random() & 0xFF)) >> 8;
	icoDeltaX = 13;
	icoDeltaY = 9;


	/* Bounce the box in the window: */

	for (;;)
	{
		int prevX;
		int prevY;
		/*
		if (XPending(dpy))
			XNextEvent(dpy, &xev);
*/
		prevX = icoX;
		prevY = icoY;

		icoX += icoDeltaX;
		if (icoX < 0 || icoX + icoW > winW)
		{
			icoX -= (icoDeltaX << 1);
			icoDeltaX = - icoDeltaX;
		}
		icoY += icoDeltaY;
		if (icoY < 0 || icoY + icoH > winH)
		{
			icoY -= (icoDeltaY << 1);
			icoDeltaY = - icoDeltaY;
		}

		drawPoly(poly, win, gc, icoX, icoY, icoW, icoH, prevX, prevY);
	}
        exit(0);
}

void
giveObjHelp()
{
	int i;
	Polyinfo *poly;
	printf("%-16s%-12s  #Vert.  #Edges  #Faces  %-16s\n",
	    "Name", "ShortName", "Dual");
	for (i=0; i<polysize; i++) {
		poly = polys+i;
		printf("%-16s%-12s%6d%8d%8d    %-16s\n",
		    poly->longname, poly->shortname,
		    poly->numverts, poly->numedges, poly->numfaces,
		    poly->dual);
	}
}

Polyinfo *
findpoly(name)
char *name;
{
	int i;
	Polyinfo *poly;
	for (i=0; i<polysize; i++) {
		poly = polys+i;
		if (strcmp(name,poly->longname)==0 ||
		    strcmp(name,poly->shortname)==0) return poly;
	}
	icoFatal("can't find object %s", name);
	return (Polyinfo *)NULL;
}

void
icoClearArea(x,y,w,h)
int x,y,w,h;
{
#if X
	if (dblbuf || dofaces) {
		XSetForeground(dpy, gc, dbpair.drawbuf->pixels[0]);
		/* use background as foreground color for fill */
		XFillRectangle(dpy,win,gc,x,y,w,h);
	}
	else {
		XClearArea(dpy,win,x,y,w,h,0);
	}
#endif
}

/******************************************************************************
 * Description
 *	Undraw previous polyhedron (by erasing its bounding box).
 *	Rotate and draw the new polyhedron.
 *
 * Input
 *	poly		the polyhedron to draw
 *	win		window on which to draw
 *	gc		X11 graphics context to be used for drawing
 *	icoX, icoY	position of upper left of bounding-box
 *	icoW, icoH	size of bounding-box
 *	prevX, prevY	position of previous bounding-box
 *****************************************************************************/
char drawn[MAXNV][MAXNV];

void
drawPoly(poly, win, gc, icoX, icoY, icoW, icoH, prevX, prevY)
Polyinfo *poly;
Window win;
GC gc;
int icoX, icoY, icoW, icoH;
int prevX, prevY;
{
	static int initialized = 0;

	Point3D *v = poly->v;
	int *f = poly->f;
	int NV = poly->numverts;
	int NF = poly->numfaces;

	static Transform3D xform;
	static Point3D xv[2][MAXNV];
	static int buffer;
	register int p0;
	register int p1;
	register XPoint *pv2;
	XSegment *pe;
	register Point3D *pxv;
	static double wo2, ho2;
	XPoint v2[MAXNV];
	XSegment edges[MAXEDGES];
	static XSegment oedges[MAXEDGES];
	static int onedges=0;
	register int i;
	int j,k;
	register int *pf;
	int facecolor;

	int pcount;
	double pxvz;
	XPoint ppts[MAXEDGESPERPOLY];

	/* Set up points, transforms, etc.:  */

	if (!initialized)
	{
		Transform3D r1;
		Transform3D r2;

		FormatRotateMat('x', 5 * 3.1416 / 180.0, r1);
		FormatRotateMat('y', 5 * 3.1416 / 180.0, r2);
		ConcatMat(r1, r2, xform);

		bcopy((char *) v, (char *) xv[0], NV * sizeof(Point3D));
		buffer = 0;

		wo2 = icoW / 2.0;
		ho2 = icoH / 2.0;

		initialized = 1;
	}


	/* Switch double-buffer and rotate vertices: */

	buffer = !buffer;
	PartialNonHomTransform(NV, xform, xv[!buffer], xv[buffer]);


	/* Convert 3D coordinates to 2D window coordinates: */

	pxv = xv[buffer];
	pv2 = v2;
	for (i = NV - 1; i >= 0; --i)
	{
		pv2->x = (int) ((pxv->x + 1.0) * wo2) + icoX;
		pv2->y = (int) ((pxv->y + 1.0) * ho2) + icoY;
		++pxv;
		++pv2;
	}


	/* Accumulate edges to be drawn, eliminating duplicates for speed: */

	pxv = xv[buffer];
	pv2 = v2;
	pf = f;
	pe = edges;
	bzero(drawn, sizeof(drawn));

	/* switch drawing buffers if double buffered */
	if (dofaces) {	/* for faces, need to clear before FillPoly */
		if (dblbuf)
			icoClearArea(
			    dbpair.drawbuf->prevX, dbpair.drawbuf->prevY,
			    icoW + 1, icoH + 1);
		icoClearArea(prevX, prevY, icoW + 1, icoH + 1);
	}

	for (i = NF - 1; i >= 0; --i, pf += pcount)
	{

		pcount = *pf++;	/* number of edges for this face */
		pxvz = 0.0;
		for (j=0; j<pcount; j++) {
			p0 = pf[j];
			pxvz += pxv[p0].z;
		}

		/* If facet faces away from viewer, don't consider it: */
		if (pxvz<0.0)
			continue;

		if (dofaces) {
			if (numcolors)
				facecolor = i%numcolors + 1;
			else	facecolor = 1;
/*			XSetForeground(dpy, gc,
			    dbpair.drawbuf->pixels[facecolor]);
*/			for (j=0; j<pcount; j++) {
				p0 = pf[j];
				ppts[j].x = pv2[p0].x;
				ppts[j].y = pv2[p0].y;
			}
/*			XFillPolygon(dpy, win, gc, ppts, pcount,
			    Convex, CoordModeOrigin);
*/		}

		if (doedges) {
			for (j=0; j<pcount; j++) {
				if (j<pcount-1) k=j+1;
				else k=0;
				p0 = pf[j];
				p1 = pf[k];
				if (!drawn[p0][p1]) {
					drawn[p0][p1] = 1;
					drawn[p1][p0] = 1;
					pe->x1 = pv2[p0].x;
					pe->y1 = pv2[p0].y;
					pe->x2 = pv2[p1].x;
					pe->y2 = pv2[p1].y;
					++pe;
				}
			}
		}
	}

	/* Erase previous, draw current icosahedrons; sync for smoothness. */

	if (doedges) {
		if (dofaces) {
/*			XSetForeground(dpy, gc, dbpair.drawbuf->pixels[0]);
*/			/* use background as foreground color */
		}
		else {
			if (dblbuf)
				icoClearArea(dbpair.drawbuf->prevX,
				    dbpair.drawbuf->prevY,
				    icoW + 1, icoH + 1);
/*			icoClearArea(prevX, prevY, icoW + 1, icoH + 1); */
			if (onedges != 0)
				XDrawSegments(dpy, win, 0,oedges, onedges);
				
			if (dblbuf || dofaces)
/*				XSetForeground(dpy, gc, dbpair.drawbuf->pixels[
					dbpair.pixelsperbuf-1]);
*/;		}
		XDrawSegments(dpy, win, 1, edges, pe - edges);
		/* nap(DELAY); */
		bcopy(edges, oedges, (pe-edges)*sizeof(XSegment));
		onedges = pe - edges;
	}

	if (dsync)
		XSync(dpy, 0);

	if (dblbuf) {
		dbpair.drawbuf->prevX = icoX;
		dbpair.drawbuf->prevY = icoY;
	}
	XSync(dpy, 0);
	if (dblbuf)
		dbpair.dbufnum = 1 - dbpair.dbufnum;
	if (sleepcount) sleep(sleepcount);
}

char *xalloc(nbytes)
int nbytes;
{
	char *p;

	p = malloc((unsigned int)nbytes);
	if (p) return p;
	fprintf(stderr,"ico: no more memory\n");
	exit(1);
}

void
initDBufs(fg,bg,planesperbuf)
int fg,bg;
int planesperbuf;
{
	int i,j,jj,j0,j1,k,m;
	DBufInfo *b, *otherb;

	dbpair.planesperbuf = planesperbuf;
	dbpair.pixelsperbuf = 1<<planesperbuf;
	dbpair.totalplanes = (dblbuf?2:1)*planesperbuf;
	dbpair.totalpixels = 1<<dbpair.totalplanes;
	dbpair.plane_masks = (unsigned long *)
	    xalloc(dbpair.totalplanes * sizeof(unsigned long));
	dbpair.dbufnum = 0;
	for (i=0; i<(dblbuf?2:1); i++) {
		b = dbpair.bufs+i;
		b->plane_masks = dbpair.plane_masks + (i*planesperbuf);
		b->colors = (XColor *)
		    xalloc(dbpair.totalpixels * sizeof(XColor));
		b->pixels = (unsigned long *)
		    xalloc(dbpair.pixelsperbuf * sizeof(unsigned long));
	}

	fgcolor.pixel = fg;
	bgcolor.pixel = bg;
	for (i=0; i<(dblbuf?2:1); i++) {
		b = dbpair.bufs+i;
		if (dblbuf)
			otherb = dbpair.bufs+(1-i);
		for (j0=0; j0<(dblbuf?dbpair.pixelsperbuf:1); j0++) {
			for (j1=0; j1<dbpair.pixelsperbuf; j1++) {
				j = (j0<<dbpair.planesperbuf)|j1;
				if (i==0) jj=j;
				else jj= (j1<<dbpair.planesperbuf)|j0;
				b->colors[jj].pixel = dbpair.pixels[0];
				for (k=0, m=j; m; k++, m=m>>1) {
					if (m&1)
						b->colors[jj].pixel |= dbpair.plane_masks[k];
				}
				b->colors[jj].flags = DoRed | DoGreen | DoBlue;
			}
		}
		b->prevX = b->prevY = 0;
		b->enplanemask = 0;
		for (j=0; j<planesperbuf; j++) {
			b->enplanemask |= b->plane_masks[j];
		}
		for (j=0; j<dbpair.pixelsperbuf; j++) {
			b->pixels[j] = dbpair.pixels[0];
			for (k=0, m=j; m; k++, m=m>>1) {
				if (m&1)
					b->pixels[j] |= b->plane_masks[k];
			}
		}
	}
#if X
	icoClearArea(0, 0, winWidth, winHeight); /* clear entire window */
	sleep(isleepcount);	/*** doesn't work without this!!! */
#endif
}

void
icoFatal(fmt,a0)
char *fmt;
int a0;
{
	fprintf(stderr,"ico: ");
	fprintf(stderr,fmt,a0);
	fprintf(stderr,"\n");
	exit(1);
}

/******************************************************************************
 * Description
 *	Concatenate two 4-by-4 transformation matrices.
 *
 * Input
 *	l		multiplicand (left operand)
 *	r		multiplier (right operand)
 *
 * Output
 *	*m		Result matrix
 *****************************************************************************/

void
ConcatMat(l, r, m)
register Transform3D l;
register Transform3D r;
register Transform3D m;
{
	register int i;
	register int j;

	for (i = 0; i < 4; ++i)
		for (j = 0; j < 4; ++j)
			m[i][j] = l[i][0] * r[0][j]
			    + l[i][1] * r[1][j]
			    + l[i][2] * r[2][j]
			    + l[i][3] * r[3][j];
}



/******************************************************************************
 * Description
 *	Format a matrix that will perform a rotation transformation
 *	about the specified axis.  The rotation angle is measured
 *	counterclockwise about the specified axis when looking
 *	at the origin from the positive axis.
 *
 * Input
 *	axis		Axis ('x', 'y', 'z') about which to perform rotation
 *	angle		Angle (in radians) of rotation
 *	A		Pointer to rotation matrix
 *
 * Output
 *	*m		Formatted rotation matrix
 *****************************************************************************/

void
FormatRotateMat(axis, angle, m)
char axis;
double angle;
register Transform3D m;
{
	double s, c;
	double sin(), cos();

	IdentMat(m);

	s = sin(angle);
	c = cos(angle);

	switch(axis)
	{
	case 'x':
		m[1][1] = m[2][2] = c;
		m[1][2] = s;
		m[2][1] = -s;
		break;
	case 'y':
		m[0][0] = m[2][2] = c;
		m[2][0] = s;
		m[0][2] = -s;
		break;
	case 'z':
		m[0][0] = m[1][1] = c;
		m[0][1] = s;
		m[1][0] = -s;
		break;
	}
}



/******************************************************************************
 * Description
 *	Format a 4x4 identity matrix.
 *
 * Output
 *	*m		Formatted identity matrix
 *****************************************************************************/

void
IdentMat(m)
register Transform3D m;
{
	register int i;
	register int j;

	for (i = 3; i >= 0; --i)
	{
		for (j = 3; j >= 0; --j)
			m[i][j] = 0.0;
		m[i][i] = 1.0;
	}
}



/******************************************************************************
 * Description
 *	Perform a partial transform on non-homogeneous points.
 *	Given an array of non-homogeneous (3-coordinate) input points,
 *	this routine multiplies them by the 3-by-3 upper left submatrix
 *	of a standard 4-by-4 transform matrix.  The resulting non-homogeneous
 *	points are returned.
 *
 * Input
 *	n		number of points to transform
 *	m		4-by-4 transform matrix
 *	in		array of non-homogeneous input points
 *
 * Output
 *	*out		array of transformed non-homogeneous output points
 *****************************************************************************/

void
PartialNonHomTransform(n, m, in, out)
int n;
register Transform3D m;
register Point3D *in;
register Point3D *out;
{
	for (; n > 0; --n, ++in, ++out)
	{
		out->x = in->x * m[0][0] + in->y * m[1][0] + in->z * m[2][0];
		out->y = in->x * m[0][1] + in->y * m[1][1] + in->z * m[2][1];
		out->z = in->x * m[0][2] + in->y * m[1][2] + in->z * m[2][2];
	}
}

void
XSync()
{
	m_flush();
}

void
XDrawSegments(dpy, d, gc, segs, n)
Display	dpy;
Window	d;
GC	gc;
XSegment *segs;
int	n;
{
	register int i;
	register XSegment *s;
	
	m_func(BIT_INVERT);
	for (i=0; i<n; i++) {
		s = segs+i;
		m_line(s->x1, s->y1, s->x2, s->y2);
	}
	m_flush();
}

void
clean()
{
	m_clear();
	m_flush();
	m_ttyreset();
	exit(0);
} 
