/* $XConsortium: ico.c,v 1.35 91/07/19 23:08:43 rws Exp $ */
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


#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/Xfuncs.h>
#include <stdio.h>
#ifdef MULTIBUFFER
#include <X11/extensions/multibuf.h>
#endif /* MULTIBUFFER */

typedef double Transform3D[4][4];

#define MIN_ICO_WIDTH 5
#define MIN_ICO_HEIGHT 5
#define DEFAULT_ICO_WIDTH 150
#define DEFAULT_ICO_HEIGHT 150
#define DEFAULT_DELTAX 13
#define DEFAULT_DELTAY 9

#include "polyinfo.h"	/* define format of one polyhedron */

/* Now include all the files which define the actual polyhedra */
Polyinfo polys[] = {
#include "allobjs.h"
};
int polysize = sizeof(polys)/sizeof(polys[0]);

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

XColor bgcolor,fgcolor;

extern long time();

extern long rand();

char *ProgramName;
Display *dpy;
Window win, draw_window;
int winX, winY, winW, winH;
int icoW = 0, icoH = 0;
int icoDeltaX = DEFAULT_DELTAX, icoDeltaY = DEFAULT_DELTAY;
Colormap cmap;
GC gc;
int multibuf = 0;
#ifdef MULTIBUFFER
int mbevbase, mberrbase;
Multibuffer multibuffers[2];
Window icowin;
#endif /* MULTIBUFFER */

char *Primaries[] = {"red", "green", "blue", "yellow", "cyan", "magenta"};
#define NumberPrimaries 6

int nplanesets;
int dsync = 0;
int softdbl = 0, dblbuf = 0;
int sleepcount = 0;
int msleepcount = 0;
int numcolors = 0;
char **colornames;	/* points into argv */
int dofaces = 0;
int doedges = 1;

Polyinfo *findpoly();

static char *help_message[] = {
"where options include:",
"    -display host:dpy            X server to use",
"    -geometry geom               geometry of window to use",
"    -size WxH                    size of object to rotate",
"    -delta +X+Y                  amount by which to move object",
"    -r                           draw in the root window",
"    -d number                    dashed line pattern for wire frames",
"    -bg color                    background color",
"    -colors color ...            codes to use on sides",
"    -p#                          use # (1 through 8) primary colors",
"    -dbl                         use double buffering (extension if present)",
"    -softdbl                     use software double buffering",
"    -noedges                     don't draw wire frame edges",
"    -faces                       draw faces",
"    -copy                        copy multibuffer frames instead of clearing",
"    -lw number                   line width to use",
"    -i                           invert",
"    -sleep number                seconds to sleep in between draws",
"    -obj objname                 type of polyhedral object to draw",
"    -objhelp                     list polyhedral objects available",
NULL};

/******************************************************************************
 * Description
 *	Main routine.  Process command-line arguments, then bounce a bounding
 *	box inside the window.  Call DrawIco() to redraw the icosahedron.
 *****************************************************************************/

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
	XSetWindowAttributes xswa;
	XWindowAttributes xwa;
	Polyinfo *poly;		/* the poly to draw */
	int icoX, icoY;
	XEvent xev;
	Bool blocking = False;
	unsigned long vmask;
	XGCValues xgcv;
	int linewidth = 0;
	char *background_colorname = NULL;
	char *ico_geom = NULL;
	char *delta_geom = NULL;
	int icodeltax2, icodeltay2;
	extern int _Xdebug;
	int initcolors = 0;
#ifdef MULTIBUFFER
	int update_action = MultibufferUpdateActionBackground;
#endif

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
		else if (!strcmp (*argv, "-copy")) {
#ifdef MULTIBUFFER
			update_action = MultibufferUpdateActionCopied;
#endif
		} else if (!strcmp (*argv, "-lw"))
			linewidth = atoi(*++argv);
		else if (!strcmp (*argv, "-dbl"))
#ifdef MULTIBUFFER
			multibuf = 1;
#else
			dblbuf = 1;
#endif
		else if (!strcmp(*argv, "-softdbl")) {
			dblbuf = 1;
			multibuf = 0;
		}
		else if (!strncmp(*argv, "-p", 2)) {
			numcolors = atoi(argv[0]+2);
			if (numcolors < 1 || numcolors > NumberPrimaries)
			  numcolors = NumberPrimaries;
			colornames = Primaries;
			dofaces = 1;
		}
		else if (!strcmp(*argv, "-bg"))
			background_colorname = *++argv;
		else if (!strcmp(*argv, "-noedges"))
			doedges = 0;
		else if (!strcmp(*argv, "-faces"))
			dofaces = 1;
		else if (!strcmp(*argv, "-i"))
			invert = 1;
		else if (!strcmp(*argv, "-size"))
			ico_geom = *++argv;
		else if (!strcmp(*argv, "-delta"))
			delta_geom = *++argv;
		else if (!strcmp (*argv, "-sleep")) {
			float f = 0.0;
			sscanf (*++argv, "%f", &f);
			msleepcount = (int) (f * 1000.0);
			sleepcount = msleepcount / 1000;
		} else if (!strcmp (*argv, "-obj"))
			poly = findpoly(*++argv);
		else if (!strcmp(*argv, "-dsync"))
			dsync = 1;
		else if (!strncmp(*argv, "-sync",  5)) 
			_Xdebug = 1;
		else if (!strcmp(*argv, "-objhelp")) {
			giveObjHelp();
			exit(1);
		}
		else {	/* unknown arg */
		    char **cpp;

		  usage:
		    fprintf (stderr, "usage:  %s [-options]\n\n", ProgramName);
		    for (cpp = help_message; *cpp; cpp++) {
			fprintf (stderr, "%s\n", *cpp);
		    }
		    fprintf (stderr, "\n");
		    exit (1);
		}
	}

	if (!dofaces && !doedges) icoFatal("nothing to draw");

	if (!(dpy= XOpenDisplay(display)))
	        {
		perror("Cannot open display\n");
		exit(-1);
	        }

#ifdef MULTIBUFFER
	if (multibuf && !XmbufQueryExtension (dpy, &mbevbase, &mberrbase)) {
	    multibuf = 0;
	    dblbuf = 1;
	}
#endif

	cmap = XDefaultColormap(dpy,DefaultScreen(dpy));
	if (!cmap) {
		icoFatal("no default colormap!");
	}

	fg = WhitePixel(dpy, DefaultScreen(dpy));
	bg = BlackPixel(dpy, DefaultScreen(dpy));
	if (background_colorname) {
	    XColor cdef, igndef;

	    if (XAllocNamedColor (dpy, cmap, background_colorname,
				  &cdef, &igndef))
	      bg = cdef.pixel;
	    else 
	      icoFatal ("no such color \"%s\"", background_colorname);
	}
	if (numcolors && (!dofaces || numcolors == 1)) {
	    XColor cdef, igndef;

	    if (XAllocNamedColor (dpy, cmap, colornames[0], &cdef, &igndef))
	      fg = cdef.pixel;
	    else 
	      icoFatal ("no such color \"%s\"", colornames[0]);
	}

	if (invert) {
	    unsigned long tmp = fg;
	    fg = bg;
	    bg = tmp;
	}


	/* Set up window parameters, create and map window if necessary: */

	if (useRoot)
		{
		draw_window = DefaultRootWindow(dpy);
		winX = 0;
		winY = 0;
		winW = DisplayWidth(dpy, DefaultScreen(dpy));
		winH = DisplayHeight(dpy, DefaultScreen(dpy));
		}
	else {
		winW = winH = (multibuf ? 300 : 600);
		winX = (DisplayWidth(dpy, DefaultScreen(dpy)) - winW) >> 1;
		winY = (DisplayHeight(dpy, DefaultScreen(dpy)) - winH) >> 1;
		if (geom) 
			XParseGeometry(geom, &winX, &winY,
				       (unsigned int *)&winW,
				       (unsigned int *)&winH);

		xswa.event_mask = ExposureMask | StructureNotifyMask;
		xswa.background_pixel = bg;
		xswa.border_pixel = fg;
		draw_window = XCreateWindow(dpy, DefaultRootWindow(dpy), 
		    winX, winY, winW, winH, 0, 
		    DefaultDepth(dpy, DefaultScreen(dpy)), 
		    InputOutput, DefaultVisual(dpy, DefaultScreen(dpy)),
		    CWEventMask | CWBackPixel | CWBorderPixel, &xswa);
		XChangeProperty(dpy, draw_window, XA_WM_NAME, XA_STRING, 8, 
				PropModeReplace, (unsigned char *)"Ico", 3);
		XMapWindow(dpy, draw_window);
		while (1) {
		    XNextEvent(dpy, &xev);
		    if (xev.type == Expose)
			break;
		}
		if (XGetWindowAttributes(dpy,draw_window,&xwa)==0) {
			icoFatal("cant get window attributes (size)");
		}
		winW = xwa.width;
		winH = xwa.height;
		}

	if (ico_geom) 
	  XParseGeometry (ico_geom, &icoX, &icoY,
			  (unsigned int *)&icoW,
			  (unsigned int *)&icoH);
	if (icoW <= 0) icoW = DEFAULT_ICO_WIDTH;
	if (icoH <= 0) icoH = DEFAULT_ICO_HEIGHT;
	if (icoW < MIN_ICO_WIDTH) icoW = MIN_ICO_WIDTH;
	if (icoH < MIN_ICO_HEIGHT) icoH = MIN_ICO_HEIGHT;

	if (delta_geom) {
	    unsigned int junk;

	    XParseGeometry (delta_geom, &icoDeltaX, &icoDeltaY, &junk, &junk);
	    if (icoDeltaX == 0 && icoDeltaY == 0) {
		icoDeltaX = DEFAULT_DELTAX;
		icoDeltaY = DEFAULT_DELTAY;
	    }
	}

	win = None;

#ifdef MULTIBUFFER
	if (multibuf) {
	    if (XmbufCreateBuffers (dpy, draw_window, 2, update_action,
				    MultibufferUpdateHintFrequent,
				    multibuffers) == 2) {
		XCopyArea (dpy, draw_window, multibuffers[1],
			   DefaultGC(dpy, DefaultScreen(dpy)),
			   0, 0, winW, winH, 0, 0);
		win = multibuffers[1];
	    } else 
	      icoFatal ("unable to obtain 2 buffers");
	    dblbuf = 1;
	}
#endif /* MULTIBUFFER */
	if (win == None) win = draw_window;

	/* whether or not we are emulating */
	softdbl = (dblbuf && !multibuf);


	/* Set up a graphics context: */

	vmask = (GCBackground | GCForeground | GCLineWidth);
	xgcv.background = bg;
	xgcv.foreground = fg;
	xgcv.line_width = linewidth;
	if (dash) {
	    xgcv.line_style = LineDoubleDash;
	    xgcv.dashes = dash;
	    vmask |= (GCLineStyle | GCDashList);
	}
	gc = XCreateGC (dpy, draw_window, vmask, &xgcv);

	if (dofaces && numcolors>1) {
	    int i,t,bits;
		bits = 0;
		for (t=numcolors; t; t=t>>1) bits++;
		initDBufs(fg,bg,bits);
		/* don't set the background color */
		for (i=0; i<numcolors; i++) {
			setBufColname(i+1,colornames[i]);
		}
		initcolors = 1;
	}
	else if (dblbuf || dofaces) {
		initDBufs(fg,bg,1);
		initcolors = 1;
	}
	if (initcolors) {
	    setDisplayBuf(dblbuf?1:0);	/* insert new colors */
	}
	if (!numcolors) numcolors=1;

	if (dsync)
		XSync(dpy, 0);

	/* Get the initial position, size, and speed of the bounding-box: */

	srand((int) time(0) % 231);
	icoX = ((winW - icoW) * (rand() & 0xFF)) >> 8;
	icoY = ((winH - icoH) * (rand() & 0xFF)) >> 8;


	/* Bounce the box in the window: */

	icodeltax2 = icoDeltaX * 2;
	icodeltay2 = icoDeltaY * 2;
	for (;;)
		{
		int prevX;
		int prevY;

		if (blocking || XPending(dpy)) {
		    XNextEvent(dpy, &xev);

		    switch (xev.type) {
		      case ConfigureNotify:
			if (xev.xconfigure.width != winW ||
			    xev.xconfigure.height != winH)
			  icoX = icoY = 1;
			winW = xev.xconfigure.width;
			winH = xev.xconfigure.height;
			break;
		      case MapNotify:
			blocking = False;
			break;
		      case UnmapNotify:
			blocking = True;
			continue;
		    }
		}

		prevX = icoX;
		prevY = icoY;

		icoX += icoDeltaX;
		if (icoX < 0 || icoX + icoW > winW)
			{
			icoX -= icodeltax2;
			icoDeltaX = - icoDeltaX;
			icodeltax2 = icoDeltaX * 2;
			}
		icoY += icoDeltaY;
		if (icoY < 0 || icoY + icoH > winH)
			{
			icoY -= icodeltay2;
			icoDeltaY = - icoDeltaY;
			icodeltay2 = icoDeltaY * 2;
			}

		drawPoly(poly, gc, icoX, icoY, icoW, icoH, prevX, prevY);
		}
	}

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
}

icoClearArea(x,y,w,h)
int x,y,w,h;
{
    if (multibuf) return;

	if (dblbuf || dofaces) {
		XSetForeground(dpy, gc, dbpair.drawbuf->pixels[0]);
			/* use background as foreground color for fill */
		XFillRectangle(dpy,win,gc,x,y,w,h);
	}
	else {
		XClearArea(dpy,win,x,y,w,h,0);
	}
}

/******************************************************************************
 * Description
 *	Undraw previous polyhedron (by erasing its bounding box).
 *	Rotate and draw the new polyhedron.
 *
 * Input
 *	poly		the polyhedron to draw
 *	gc		X11 graphics context to be used for drawing
 *	icoX, icoY	position of upper left of bounding-box
 *	icoW, icoH	size of bounding-box
 *	prevX, prevY	position of previous bounding-box
 *****************************************************************************/
char drawn[MAXNV][MAXNV];

drawPoly(poly, gc, icoX, icoY, icoW, icoH, prevX, prevY)
Polyinfo *poly;
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

	if (dblbuf)
		setDrawBuf(dbpair.dbufnum);
			/* switch drawing buffers if double buffered */
	/* for faces, need to clear before FillPoly */
	if (dofaces && !multibuf) {	/* multibuf uses update background */
		if (softdbl)
			icoClearArea(
				dbpair.drawbuf->prevX, dbpair.drawbuf->prevY,
				icoW + 1, icoH + 1);
		icoClearArea(prevX, prevY, icoW + 1, icoH + 1);
	}

	if (dsync)
		XSync(dpy, 0);

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
			XSetForeground(dpy, gc,
				dbpair.drawbuf->pixels[facecolor]);
			for (j=0; j<pcount; j++) {
				p0 = pf[j];
				ppts[j].x = pv2[p0].x;
				ppts[j].y = pv2[p0].y;
			}
			XFillPolygon(dpy, win, gc, ppts, pcount,
				Convex, CoordModeOrigin);
		}

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
			XSetForeground(dpy, gc, dbpair.drawbuf->pixels[0]);
				/* use background as foreground color */
		}
		else {
			if (softdbl)
				icoClearArea(dbpair.drawbuf->prevX,
					dbpair.drawbuf->prevY,
					icoW + 1, icoH + 1);
			if (!multibuf)
			  icoClearArea(prevX, prevY, icoW + 1, icoH + 1);
			if (dblbuf || dofaces) {
				XSetForeground(dpy, gc, dbpair.drawbuf->pixels[
					dbpair.pixelsperbuf-1]);
			}
		}
		XDrawSegments(dpy, win, gc, edges, pe - edges);
	}

	if (dsync)
		XSync(dpy, 0);

	if (dblbuf) {
		dbpair.drawbuf->prevX = icoX;
		dbpair.drawbuf->prevY = icoY;
		setDisplayBuf(dbpair.dbufnum);
	}
	if (dblbuf)
		dbpair.dbufnum = 1 - dbpair.dbufnum;
	if (!multibuf && sleepcount) sleep(sleepcount);
	}

char *xalloc(nbytes)
int nbytes;
{
char *p, *malloc();

	p = malloc((unsigned int)nbytes);
	if (p) return p;
	fprintf(stderr,"ico: no more memory\n");
	exit(1);
}

initDBufs(fg,bg,planesperbuf)
int fg,bg;
int planesperbuf;
{
int i,j,jj,j0,j1,k,m,t;
DBufInfo *b, *otherb;

	nplanesets = (softdbl ? 2 : 1);

	dbpair.planesperbuf = planesperbuf;
	dbpair.pixelsperbuf = 1<<planesperbuf;
	dbpair.totalplanes = nplanesets * planesperbuf;
	dbpair.totalpixels = 1<<dbpair.totalplanes;
	dbpair.plane_masks = (unsigned long *)
		xalloc(dbpair.totalplanes * sizeof(unsigned long));
	dbpair.dbufnum = 0;
	for (i=0; i < nplanesets; i++) {
		b = dbpair.bufs+i;
		b->plane_masks = dbpair.plane_masks + (i*planesperbuf);
		b->colors = (XColor *)
			xalloc(dbpair.totalpixels * sizeof(XColor));
		b->pixels = (unsigned long *)
			xalloc(dbpair.pixelsperbuf * sizeof(unsigned long));
	}

	if (dbpair.totalplanes == 1) {
	    dbpair.pixels[0] = bg;
	    dbpair.plane_masks[0] = fg ^ bg;
	} else {
	    t = XAllocColorCells(dpy,cmap,0,
		    dbpair.plane_masks,dbpair.totalplanes, dbpair.pixels,1);
			    /* allocate color planes */
	    if (t==0) {
		    icoFatal("can't allocate enough color planes");
	    }
	}

	fgcolor.pixel = fg;
	bgcolor.pixel = bg;
	XQueryColor(dpy,cmap,&fgcolor);
	XQueryColor(dpy,cmap,&bgcolor);

	setBufColor(0,&bgcolor);
	setBufColor(1,&fgcolor);
	for (i=0; i<nplanesets; i++) {
		b = dbpair.bufs+i;
		if (dblbuf)
			otherb = dbpair.bufs+(1-i);
		for (j0=0; j0<(softdbl?dbpair.pixelsperbuf:1); j0++) {
		    for (j1=0; j1<dbpair.pixelsperbuf; j1++) {
			j = (j0<<dbpair.planesperbuf)|j1;
			if (i==0) jj=j;
			else jj= (j1<<dbpair.planesperbuf)|j0;
			b->colors[jj].pixel = dbpair.pixels[0];
			for (k=0, m=j; m; k++, m=m>>1) {
				if (m&1)
				   b->colors[jj].pixel ^= dbpair.plane_masks[k];
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
				   b->pixels[j] ^= b->plane_masks[k];
			}
		}
	}

	if (!multibuf) {
	    setDrawBuf(0);
	    XSetBackground(dpy, gc, dbpair.bufs[0].pixels[0]);
	    XSetWindowBackground(dpy, draw_window, dbpair.bufs[0].pixels[0]);
	    XSetPlaneMask(dpy, gc, AllPlanes);
	    icoClearArea(0, 0, winW, winH); /* clear entire window */
	}

/*	if (softdbl) setDisplayBuf(1); */
}

setBufColname(n,colname)
int n;
char *colname;
{
int t;
XColor dcolor, color;

	t = XLookupColor(dpy,cmap,colname,&dcolor,&color);
	if (t==0) {	/* no such color */
		icoFatal("no such color %s",colname);
	}
	setBufColor(n,&color);
}

setBufColor(n,color)
int n;		/* color index */
XColor *color;	/* color to set */
{
int i,j,cx;
DBufInfo *b;
unsigned long pix;

	for (i=0; i<nplanesets; i++) {
		b = dbpair.bufs+i;
		for (j=0; j<(softdbl?dbpair.pixelsperbuf:1); j++) {
			cx = n + j*dbpair.pixelsperbuf;
			pix = b->colors[cx].pixel;
			b->colors[cx] = *color;
			b->colors[cx].pixel = pix;
			b->colors[cx].flags = DoRed | DoGreen | DoBlue;
		}
	}
}

setDrawBuf (n)
    int n;
{
    XGCValues xgcv;
    unsigned long mask;

#ifdef MULTIBUFFER
    if (multibuf) {
	win = multibuffers[n];
	n = 0;
    }
#endif /* MULTIBUFFER */

    dbpair.drawbuf = dbpair.bufs+n;
    xgcv.foreground = dbpair.drawbuf->pixels[dbpair.pixelsperbuf-1];
    xgcv.background = dbpair.drawbuf->pixels[0];
    mask = GCForeground | GCBackground;
    if (softdbl) {
	xgcv.plane_mask = dbpair.drawbuf->enplanemask;
	mask |= GCPlaneMask;
    }
    XChangeGC(dpy, gc, mask, &xgcv);
}

setDisplayBuf(n)
int n;
{
#if MULTIBUFFER
    if (multibuf) {
	static int firsttime = 1;

	XmbufDisplayBuffers (dpy, 1, &multibuffers[n], msleepcount, 0);
	if (firsttime) {
	    firsttime = 0;
	    n = 0;
	    goto storecolors;
	}
    } else
#endif
    {
      storecolors:
	dbpair.dpybuf= dbpair.bufs+n;
	if (dbpair.totalpixels > 2)
	    XStoreColors(dpy,cmap,dbpair.dpybuf->colors,dbpair.totalpixels);
    }
}

icoFatal(fmt,a0)
char *fmt;
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
