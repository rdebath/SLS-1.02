/*****************************************************************************
Copyright 1988, 1989 by Digital Equipment Corporation, Maynard, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Digital not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <signal.h>

#ifndef VMS
#include <X11/Xatom.h>
#include <X11/Xos.h>
#else
#include <decw$include/Xatom.h>
#endif
#include "x11perf.h"

/* Only for working on ``fake'' servers, for hardware that doesn't exist */
static Bool     drawToFakeServer = False;
static Pixmap   tileToQuery     = None;

typedef struct _RopNames { char	*name; int  rop; } RopNameRec, *RopNamePtr;

RopNameRec ropNames[] = {
	"clear",    	GXclear,	/* 0 */
	"and",		GXand,		/* src AND dst */
	"andReverse",	GXandReverse,	/* src AND NOT dst */
	"copy",		GXcopy,		/* src */
	"andInverted",	GXandInverted,	/* NOT src AND dst */
	"noop",		GXnoop,		/* dst */
	"xor",		GXxor,		/* src XOR dst */
	"or",		GXor,		/* src OR dst */
	"nor",		GXnor,		/* NOT src AND NOT dst */
	"equiv",	GXequiv,	/* NOT src XOR dst */
	"invert",	GXinvert,	/* NOT dst */
	"orReverse",	GXorReverse,	/* src OR NOT dst */
	"copyInverted",	GXcopyInverted,	/* NOT src */
	"orInverted",	GXorInverted,	/* NOT src OR dst */
	"nand",		GXnand,		/* NOT src OR NOT dst */
	"set",		GXset,		/* 1 */
};

static Bool     labels		= False;
static int      repeat		= 5;
static int	seconds		= 5;

static Window   status;     /* Status window and GC */
static GC       tgc;

static double syncTime = 0.0;

static int saveargc;
static char **saveargv;

static int  numRops = 1;
static int  rops[16] = { GXcopy };
static int  numPlanemasks = 1;
static unsigned long planemasks[256] = { (unsigned long)~0 };

static char *foreground = NULL;
static char *background = NULL;

static int numSubWindows = 7;
static int subWindows[] = {4, 16, 25, 50, 75, 100, 200, 0};

static int  fixedReps = 0;

static Bool *doit;

static XRectangle ws[] = {  /* Clip rectangles */
    {195, 195, 120, 120},
    { 45, 145, 120, 120},
    {345, 245, 120, 120}

};
#define MAXCLIP     (sizeof(ws) / sizeof(ws[0]))
static Window clipWindows[MAXCLIP];
static Colormap cmap;
static int depth = -1;  /* -1 means use default depth */

/* ScreenSaver state */
static XParmRec    xparms;
static int ssTimeout, ssInterval, ssPreferBlanking, ssAllowExposures;


/************************************************
*	    time related stuff			*
************************************************/

#ifdef VMS

typedef struct _vms_time {
    unsigned long low;
    unsigned long high;
}vms_time;

struct timeval {
    long tv_sec;        /* seconds since Jan. 1, 1970 */
    long tv_usec;  /* and microseconds */
};

struct timezone {
    int  tz_minuteswest;     /* of Greenwich */
    int  tz_dsttime;    /* type of dst correction to apply */
};


static int firsttime = True;
static vms_time basetime;

int gettimeofday(tp, tzp)
    struct timeval *tp;
    struct timezone *tzp;
{
    vms_time current_time, resultant;
    unsigned long mumble, foo;
    int status;

    if (firsttime) {
        sys$gettim(&basetime);
        firsttime = False;
    }
    sys$gettim(&current_time);
    resultant.high = current_time.high - basetime.high;
    resultant.low = current_time.low - basetime.low;
    if (current_time.low < basetime.low) {
        resultant.high -= 1;
    }
    status = lib$ediv( &(10000000), &resultant, &tp->tv_sec, &tp->tv_usec);
    tp->tv_usec /= 10;
    return 0;
}

#endif

static struct  timeval start;

void PrintTime()
{
    time_t t;

    t = time((long *)NULL);
    printf("%s\n", ctime(&t));
}

void InitTimes ()
{
    struct timezone foo;
    gettimeofday(&start, &foo);
}

double ElapsedTime(correction)
    double correction;
{
    struct timeval stop;
    struct timezone foo;
    
    gettimeofday(&stop, &foo);
    if (stop.tv_usec < start.tv_usec) {
        stop.tv_usec += 1000000;
	stop.tv_sec -= 1;
    }
    return  (double)(stop.tv_usec - start.tv_usec) +
            (1000000.0 * (double)(stop.tv_sec - start.tv_sec)) - correction;
}

double RoundTo3Digits(d)
    double d;
{
    /* It's kind of silly to print out things like ``193658.4/sec'' so just
       junk all but 3 most significant digits. */

    double exponent, sign;

    exponent = 1.0;
    /* the code below won't work if d should happen to be non-positive. */
    if (d < 0.0) {
	d = -d;
	sign = -1.0;
    } else
	sign = 1.0;
    if (d >= 1000.0) {
	do {
	    exponent *= 10.0;
	} while (d/exponent >= 1000.0);
	d = (double)((int) (d/exponent + 0.5));
	d *= exponent;
    } else {
	if (d != 0.0) {
	    while (d*exponent < 100.0) {
	        exponent *= 10.0;
	    }
	}
	d = (double)((int) (d*exponent + 0.5));
	d /= exponent;
    }
    return d * sign;
}


void ReportTimes(usecs, n, str, average)
    double  usecs;
    int     n;
    char    *str;
{
    double msecsperobj, objspersec;

    if(usecs != 0.0)
    {
        msecsperobj = usecs / (1000.0 * (double)n);
        objspersec = (double) n * 1000000.0 / usecs;

        /* Round obj/sec to 3 significant digits.  Leave msec untouched, to
	   allow averaging results from several repetitions. */
        objspersec =  RoundTo3Digits(objspersec);

        if (average) {
	    printf("%6d trep @ %7.4f msec (%6.1f/sec): %s\n", 
		    n, msecsperobj, objspersec, str);
	} else {
	    printf("%6d reps @ %7.4f msec (%6.1f/sec): %s\n", 
	        n, msecsperobj, objspersec, str);
	}
    } else {
	printf("%6d %sreps @ 0.0 msec (unmeasurably fast): %s\n",
	    n, average ? "t" : "", str);
    }

}



/************************************************
*		Generic X stuff			*
************************************************/

static char *program_name;
void usage();

/*
 * Get_Display_Name (argc, argv) Look for -display, -d, or host:dpy (obselete)
 * If found, remove it from command line.  Don't go past a lone -.
 */
char *Get_Display_Name(pargc, argv)
    int     *pargc;  /* MODIFIED */
    char    **argv; /* MODIFIED */
{
    int     argc = *pargc;
    char    **pargv = argv+1;
    char    *displayname = NULL;
    int     i;

    for (i = 1; i != argc; i++) {
	char *arg = argv[i];

	if (!strcmp (arg, "-display") || !strcmp (arg, "-d")) {
	    if (++i >= argc) usage ();

	    displayname = argv[i];
	    *pargc -= 2;
	    continue;
	}
	if (!strcmp(arg,"-")) {
	    while (i<argc)  *pargv++ = argv[i++];
	    break;
	}
	*pargv++ = arg;
    }

    *pargv = NULL;
    return (displayname);
}


/*
 * Open_Display: Routine to open a display with correct error handling.
 */
Display *Open_Display(display_name)
    char *display_name;
{
    Display *d;

    d = XOpenDisplay(display_name);
    if (d == NULL) {
	fprintf (stderr, "%s:  unable to open display '%s'\n",
		 program_name, XDisplayName (display_name));
	usage ();
	/* doesn't return */
    }

    return(d);
}


#ifdef SIGNALRETURNSINT
int
#else
void
#endif
Cleanup(sig)
    int sig;
{
    fflush(stdout);
    /* This will screw up if Xlib is in the middle of something */
    XSetScreenSaver(xparms.d, ssTimeout, ssInterval, ssPreferBlanking,
	ssAllowExposures);
    XFlush(xparms.d);
    exit(sig);
}


/************************************************
*		Performance stuff		*
************************************************/


void usage()
{
    char    **cpp;
    int     i = 0;
    static char *help_message[] = {
"where options include:",
"    -display <host:display>   the X server to contact",
"    -sync                     do the tests in synchronous mode",
"    -pack                     pack rectangles right next to each other",
"    -repeat <n>               do tests <n> times (default = 5)",
"    -time <s>                 do tests for <s> seconds each (default = 5)",
/*
"    -draw                     draw after each test -- pmax only",
*/
"    -all                      do all tests",
"    -range <test1>[,<test2>]  like all, but do <test1> to <test2>",
"    -labels                   generate test labels for use by fillblnk",
"    -fg                       the foreground color to use",
"    -bg                       the background color to use",
"    -rop <rop0 rop1 ...>      use the given rops to draw (default = GXcopy)",
"    -pm <pm0 pm1 ...>         use the given planemasks to draw (default = ~0)",
"    -depth <depth>            use a visual with <depth> planes per pixel",
"    -reps <n>                 fix the rep count (default = auto scale)",
"    -subs <s0 s1 ...>         a list of the number of sub-windows to use",
"    -v1.2                     perform only v1.2 tests using old semantics",
NULL};

    fflush(stdout);
    fprintf(stderr, "usage: %s [-options ...]\n", program_name);
    for (cpp = help_message; *cpp; cpp++) {
	fprintf(stderr, "%s\n", *cpp);
    }
    while (test[i].option != NULL) {
        fprintf(stderr, "    %-24s   %s\n",
		test[i].option, test[i].label);
        i++;
    }
    fprintf(stderr, "\n");
    
    /* Print out original command line as the above usage message is so long */
    for (i = 0; i != saveargc; i++) {
	fprintf(stderr, "%s ", saveargv[i]);
    }
    fprintf(stderr, "\n\n");
    exit (1);
}

void NullProc(xp, p)
    XParms  xp;
    Parms   p;
{
}

Bool NullInitProc(xp, p, reps)
    XParms  xp;
    Parms   p;
    int reps;
{
    return reps;
}


void HardwareSync(xp)
    XParms  xp;
{
    /*
     * Some graphics hardware allows the server to claim it is done,
     * while in reality the hardware is busily working away.  So fetch
     * a pixel from the window that was drawn to, which should be
     * enough to make the server wait for the graphics hardware.
     */
    XImage *image;

    image = XGetImage(xp->d, xp->w, WIDTH-1, HEIGHT-1, 1, 1, ~0, ZPixmap);
    XDestroyImage(image);
}

void DoHardwareSync(xp, p, reps)
    XParms  xp;
    Parms   p;
    int     reps;
{
    int i;
    
    for (i = 0; i != reps; i++) {
	HardwareSync(xp);
    }
}

static Test syncTest = {
    "syncTime", "Internal test for finding how long HardwareSync takes",
    NullInitProc, DoHardwareSync, NullProc, NullProc, 
    VALL, NONROP, 0,
    {1}
};


static Window CreatePerfWindow(xp, x, y, width, height)
    XParms  xp;
    int     width, height, x, y;
{
    XSetWindowAttributes xswa;
    Window w;
    Screen *s;
    int su;

/*
    s = DefaultScreenOfDisplay(xp->d);
    su = XDoesBackingStore(s);
    printf("Backing store of screen returns %d\n", su);
    su = XDoesSaveUnders(s);
    printf("Save unders of screen returns %d\n", su);
    su = XPlanesOfScreen(s);
    printf("Planes of screen returns %d\n", su);
*/
    xswa.background_pixel = xp->background;
    xswa.border_pixel = xp->foreground;
    xswa.colormap = cmap;
    xswa.override_redirect = True;
    xswa.backing_store = False;
    xswa.save_under = False;
    w = XCreateWindow(xp->d, DefaultRootWindow(xp->d), x, y, width, height, 1,
        xp->vinfo.depth, CopyFromParent, xp->vinfo.visual,
	CWBackPixel | CWBorderPixel | CWColormap | CWOverrideRedirect 
	| CWBackingStore | CWSaveUnder, &xswa);
    XMapWindow (xp->d, w);
    return w;
}


void CreateClipWindows(xp, clips)
    XParms  xp;
    int     clips;
{
    int j;
    XWindowAttributes    xwa;

    (void) XGetWindowAttributes(xp->d, xp->w, &xwa);
    if (clips > MAXCLIP) clips = MAXCLIP;
    for (j = 0; j != clips; j++) {
	clipWindows[j] = CreatePerfWindow(xp,
	    xwa.x + ws[j].x, xwa.y + ws[j].y, ws[j].width, ws[j].height);
    }
} /* CreateClipWindows */


void DestroyClipWindows(xp, clips)
    XParms  xp;
    int     clips;
{
    int j;

    if (clips > MAXCLIP) clips = MAXCLIP;
    for (j = 0; j != clips; j++) {
	XDestroyWindow(xp->d, clipWindows[j]);
    }
} /* DestroyClipWindows */


double DoTest(xp, test, reps)
    XParms  xp;
    Test    *test;
    int     reps;
{
    double  time;
    unsigned int ret_width, ret_height;

    /* Tell screen-saver to restart counting again.  See comments below for the
       XSetScreenSaver call. */
    XForceScreenSaver(xp->d, ScreenSaverReset);
    HardwareSync (xp); 
    InitTimes ();
    (*test->proc) (xp, &test->parms, reps);
    HardwareSync(xp);

    time = ElapsedTime(syncTime);
    if (drawToFakeServer)
        XQueryBestSize(xp->d, TileShape, tileToQuery,
		       32, 32, &ret_width, &ret_height);
    (*test->passCleanup) (xp, &test->parms);
    return time;
}


int CalibrateTest(xp, test, seconds, usecperobj)
    XParms  xp;
    Test    *test;
    int     seconds;
    double  *usecperobj;
{
#define goal    2500000.0   /* Try to get up to 2.5 seconds		    */
#define enough  2000000.0   /* But settle for 2.0 seconds		    */
#define tick      10000.0   /* Assume clock not faster than .01 seconds     */

    double  usecs;
    int     reps, didreps;  /* Reps desired, reps performed		    */
    int     exponent;

    /* Attempt to get an idea how long each rep lasts by getting enough
       reps to last more tan enough.  Then scale that up to the number of
       seconds desired.

       If init call to test ever fails, return False and test will be skipped.
    */

    if (fixedReps != 0) {
	return fixedReps;
    }
    reps = 1;
    for (;;) {
	XDestroySubwindows(xp->d, xp->w);
	XClearWindow(xp->d, xp->w);
	didreps = (*test->init) (xp, &test->parms, reps);
	if (didreps == 0) {
	    return 0;
	}
	/* Create clip windows if requested */
	CreateClipWindows(xp, test->clips);
	HardwareSync(xp);
	InitTimes();
	(*test->proc) (xp, &test->parms, reps);
	HardwareSync(xp);
	usecs = ElapsedTime(syncTime);
	(*test->cleanup) (xp, &test->parms);
	DestroyClipWindows(xp, test->clips);

	if (didreps != reps) {
	    /* The test can't do the number of reps as we asked for.  
	       Give up */
	    *usecperobj = 
		usecs / (double)(didreps * test->parms.objects);
	    return didreps;
	}
	/* Did we go long enough? */
	if (usecs >= enough) break;

	/* Don't let too short a clock make new reps wildly high */
	if (usecs < tick) usecs = tick;

	/* Try to get up to goal seconds. */
	reps = (int) (goal * (double)reps / usecs) + 1;
    }

    *usecperobj = usecs / (double) (reps * test->parms.objects);
    reps = (int) ((double)seconds * 1000000.0 * (double)reps / usecs) + 1;

    /* Now round reps up to 1 digit accuracy, so we don't get stupid-looking
       numbers of repetitions. */
    reps--;
    exponent = 1;
    while (reps > 9) {
	reps /= 10;
	exponent *= 10;
    }
    reps = (reps + 1) * exponent;
    return reps;
} /* CalibrateTest */

void CreatePerfGCs(xp, func, pm)
    XParms  xp;
    int     func;
    unsigned long   pm;
{
    XGCValues gcvfg, gcvbg;
    unsigned long	fg, bg;

    fg = xp->foreground;
    bg = xp->background;
    gcvfg.graphics_exposures = False;
    gcvbg.graphics_exposures = False;
    gcvfg.plane_mask = pm;
    gcvbg.plane_mask = pm;
    gcvfg.function = func;
    gcvbg.function = func;
    
    if (func == GXxor) {
	/* Make test look good visually if possible */
	gcvbg.foreground = gcvfg.foreground = bg ^ fg;
	gcvbg.background = gcvfg.background = bg;
    } else {
	gcvfg.foreground = fg;
	gcvfg.background = bg;
	gcvbg.foreground = bg;
	gcvbg.background = fg;
    }
    xp->fggc = XCreateGC(xp->d, xp->w,
	GCForeground | GCBackground | GCGraphicsExposures
      | GCFunction | GCPlaneMask, &gcvfg);
    xp->bggc = XCreateGC(xp->d, xp->w, 
	GCForeground | GCBackground | GCGraphicsExposures
      | GCFunction | GCPlaneMask, &gcvbg);
}


void DestroyPerfGCs(xp)
    XParms(xp);
{
    XFreeGC(xp->d, xp->fggc);
    XFreeGC(xp->d, xp->bggc);
}

int AllocateColor(display, name, pixel)
    Display     *display;
    char	*name;
    int		pixel;
{
    XColor      color;

    if (name != NULL) {
	/* Try to parse color name */
	if (XParseColor(display, cmap, name, &color)) {
	    if (XAllocColor(display, cmap, &color)) {
		pixel = color.pixel;
	    } else {
		(void) fprintf(stderr,
		    "Can't allocate colormap entry for color %s\n", name);
	    }
	} else {
	    (void) fprintf(stderr, "Can't parse color name %s\n", name);
	}
    }
    return pixel;
} /* AllocateColor */


void DisplayStatus(d, message, test)
    Display *d;
    char    *message;
    char    *test;
{
    char    s[500];

    XClearWindow(d, status);
    sprintf(s, "%s %s", message, test);
    /* We should really look at the height, descent of the font, etc. but
       who cares.  This works. */
    XDrawString(d, status, tgc, 10, 13, s, strlen(s));
}


void ProcessTest(xp, test, func, pm, label)
    XParms  xp;
    Test    *test;
    int     func;
    unsigned long   pm;
    char    *label;
{
    double  time, totalTime;
    int     reps, fooreps;
    int     j;

    CreatePerfGCs(xp, func, pm);
    DisplayStatus(xp->d, "Calibrating", label);
    reps = CalibrateTest(xp, test, seconds, &time);
    if (reps != 0) {
	DisplayStatus(xp->d, "Testing", label);
	XDestroySubwindows(xp->d, xp->w);
	XClearWindow(xp->d, xp->w);
	reps = (*test->init) (xp, &test->parms, reps);
	/* Create clip windows if requested */
	CreateClipWindows(xp, test->clips);

	totalTime = 0.0;
	for (j = 0; j != repeat; j++) {
	    time = DoTest(xp, test, reps);
	    totalTime += time;
	    ReportTimes (time, reps * test->parms.objects,
		    label, False);
	}
	if (repeat > 1) {
	    ReportTimes(totalTime,
		repeat * reps * test->parms.objects,
		label, True);
	}
	(*test->cleanup) (xp, &test->parms);
	DestroyClipWindows(xp, test->clips);
    } else {
	/* Test failed to initialize properly */
    }
    printf ("\n");
    fflush(stdout);
    DestroyPerfGCs(xp);
} /* ProcessTest */


main(argc, argv)
    int argc;
    char **argv;

{
    int		i, j, n, skip;
    int		numTests;       /* Even though the linker knows, we don't. */
    char	hostname[100];
    char	*displayName;
    Bool	foundOne = False;
    Bool	synchronous = False;
    XGCValues	tgcv;
    int		screen;
    int		rop, pm;
    XVisualInfo *vinfolist, vinfotempl;
    unsigned long vmask;

    /* Save away argv, argc, for usage to print out */
    saveargc = argc;
    saveargv = (char **) malloc(argc * sizeof(char *));
    for (i = 0; i != argc; i++) {
	saveargv[i] = argv[i];
    }

    xparms.pack = False;
    xparms.version = VERSION1_3;

    /* Count number of tests */
    ForEachTest(numTests);
    doit = (Bool *)calloc(numTests, sizeof(Bool));

    /* Parse arguments */
    program_name = argv[0];
    displayName = Get_Display_Name (&argc, argv);
    for (i = 1; i != argc; i++) {
	if (strcmp (argv[i], "-all") == 0) {
	    ForEachTest (j)
		doit[j] = True;
	    foundOne = True;
	} else if (strcmp (argv[i], "-labels") == 0) {
	    labels = True;
	} else if (strcmp(argv[i], "-range") == 0) {
	    char *cp1;
	    char *cp2;
	    
	    if (argc <= ++i)
		usage();
	    cp1 = argv[i];
	    if (*cp1 == '-')
		*cp1++;
	    for (cp2 = cp1; *cp2 != '\0' && *cp2 != ','; cp2++) {};
	    if (*cp2 == ',') {
		*cp2++ = '\0';
		if (*cp2 == '-')
		    *cp2++;
	    } else {
		cp2 = "-";
	    }
	    ForEachTest (j) {
		if (strcmp (cp1, (test[j].option) + 1) == 0 &&
		    (test[j].versions & xparms.version)) {
		    int k = j;
		    do {
			doit[k] = True;
		    } while (strcmp(cp2, (test[k].option + 1)) != 0 &&
			     (test[k].versions & xparms.version) &&
			     test[++k].option != NULL);
		    if (*cp2 != '-' && test[k].option == NULL)
			usage();
		    break;
		}
	    }
	    if (test[j].option == NULL)
		usage();
	    foundOne = True;
	} else if (strcmp (argv[i], "-sync") == 0) {
	    synchronous = True;
	} else if (strcmp(argv[i], "-pack") == 0) {
	    xparms.pack = True;
	} else if (strcmp (argv[i], "-draw") == 0) {
	    drawToFakeServer = True;
	} else if (strcmp (argv[i], "-repeat") == 0) {
	    i++;
	    if (argc <= i)
		usage ();
	    repeat = atoi (argv[i]);
	    if (repeat <= 0)
	       usage ();
	} else if (strcmp (argv[i], "-time") == 0) {
	    i++;
	    if (argc <= i)
		usage ();
	    seconds = atoi (argv[i]);
	    if (seconds <= 0)
	       usage ();
	} else if (strcmp(argv[i], "-fg") == 0) {
	    i++;
	    if (argc <= i)
		usage ();
	    foreground = argv[i];
        } else if (strcmp(argv[i], "-bg") == 0) {
	    i++;
	    if (argc <= i)
		usage ();
	    background = argv[i];
	} else if (strcmp(argv[i], "-rop") == 0) {
	    skip = GetRops (i+1, argc, argv, rops, &numRops);
	    i += skip;
	} else if (strcmp(argv[i], "-pm") == 0) {
	    skip = GetNumbers (i+1, argc, argv, planemasks, &numPlanemasks);
	    i += skip;
	} else if (strcmp(argv[i], "-xor") == 0) {
	    numRops = 1;
	    rops[0] = GXxor;
	} else if (strcmp (argv[i], "-both") == 0) {
	    numRops = 2;
	    rops[0] = GXcopy;
	    rops[1] = GXxor;
	} else if (strcmp(argv[i], "-reps") == 0) {
	    i++;
	    if (argc <= i)
		usage ();
	    fixedReps = atoi (argv[i]);
	    if (fixedReps <= 0)
		usage ();
        } else if (strcmp(argv[i], "-depth") == 0) {
	    i++;
	    if (argc <= i)
                usage ();
            depth = atoi(argv[i]);
            if (depth <= 0)
		usage ();
	} else if (strcmp(argv[i], "-subs") == 0) {
	    skip = GetNumbers (i+1, argc, argv, subWindows, &numSubWindows);
	    i += skip;
	} else if (strcmp(argv[i], "-v1.2") == 0) {
	    xparms.version = VERSION1_2;
	} else {
	    ForEachTest (j) {
		if (strcmp (argv[i], test[j].option) == 0 &&
		    (test[j].versions & xparms.version)) {
		    doit[j] = True;
		    goto LegalOption;
		}
	    }
	    usage ();
	LegalOption: 
		foundOne = True;
	}
    }

    if (labels) {
	/* Just print out list of tests for use with .sh programs that
	   assemble data from different x11perf runs into a nice format */
	ForEachTest (i) {
	    int child;
	    if (doit[i] && (test[i].versions & xparms.version)) {
		switch (test[i].testType) {
		    case NONROP:
			printf ("%s\n", test[i].label);
			break;
    
		    case ROP:
			/* Run it through all specified rops and planemasks */
			for (rop = 0; rop < numRops; rop++) {
			    for (pm = 0; pm < numPlanemasks; pm++) {
				if (planemasks[pm] == ~0) {
				    if (rops[rop] == GXcopy) {
					printf ("%s\n", test[i].label);
				    } else {
					printf ("(%s) %s\n",
					    ropNames[rops[rop]].name,
					    test[i].label);
				    }
				} else {
				    printf ("(%s 0x%x) %s\n",
					    ropNames[rops[rop]].name,
					    planemasks[pm],
					    test[i].label);
				}
			    } /* for pm */
			} /* for rop */
			break;
		    
		    case WINDOW:
			for (child = 0; child != numSubWindows; child++) {
			    printf ("%s (%d kids)\n",
				test[i].label, subWindows[child]);
			}
			break;
		} /* switch */
	    }
	}
	exit(0);
    }

    if (!foundOne)
	usage ();
    xparms.d = Open_Display (displayName);
    screen = DefaultScreen(xparms.d);

    /* get visual info of default visual */
    vmask = VisualIDMask | VisualScreenMask;
    vinfotempl.visualid = XVisualIDFromVisual(XDefaultVisual(xparms.d, screen));
    vinfotempl.screen = screen;
    vinfolist = XGetVisualInfo(xparms.d, vmask, &vinfotempl, &n);
    if (!vinfolist || n != 1) {
	fprintf (stderr, "%s: can't get visual info of default visual\n",
	    program_name);
	exit(1);
    }

    if (depth == -1) {
	/* use the default visual and colormap */
	xparms.vinfo = *vinfolist;
	cmap = XDefaultColormap(xparms.d, screen);
    } else {
	/* find the specified visual */
	vmask = VisualDepthMask | VisualScreenMask;
	vinfotempl.depth = depth;
	vinfotempl.screen = screen;
	vinfolist = XGetVisualInfo(xparms.d, vmask, &vinfotempl, &n);
	if (!vinfolist) {
	    fprintf (stderr, "%s: can't find a visual of depth %d\n",
		program_name, depth);
	    exit(1);
	}
	xparms.vinfo = *vinfolist;  /* use the first one in list */
	if (xparms.vinfo.visualid ==
	    XVisualIDFromVisual(XDefaultVisual(xparms.d, screen))) {
	    /* matched visual is same as default visual */
	    cmap = XDefaultColormap(xparms.d, screen);
	} else {
	    cmap = XCreateColormap(xparms.d, DefaultRootWindow(xparms.d),
		xparms.vinfo.visual, AllocNone);
	    /* since this is not default cmap, must force color allocation */
	    if (!foreground) foreground = "Black";
	    if (!background) background = "White";
	}
    }

    printf("x11perf - X11 performance program, version 1.3\n");
#ifndef VMS
    gethostname (hostname, 100);
    printf ("%s server on %s\nfrom %s\n",
	    ServerVendor (xparms.d), DisplayString (xparms.d), hostname);
#else
    printf ("%s server on %s\n",
	    ServerVendor (xparms.d), DisplayString (xparms.d));
#endif
    PrintTime ();

    /* Force screen out of screen-saver mode, grab current data, and set
       time to blank to 8 hours.  We should just be able to turn the screen-
       saver off, but this causes problems on some servers.  We also reset
       the screen-saver timer each test, as 8 hours is about the maximum time
       we can use, and that isn't long enough for some X terminals using a
       serial protocol to finish all the tests.  As long as the tests run to 
       completion, the old screen-saver values are restored. */
    XForceScreenSaver(xparms.d, ScreenSaverReset);
    XGetScreenSaver(xparms.d, &ssTimeout, &ssInterval, &ssPreferBlanking,
	&ssAllowExposures);
    (void) signal(SIGINT, Cleanup); /* ^C */
    (void) signal(SIGQUIT, Cleanup);
    (void) signal(SIGTERM, Cleanup);
    (void) signal(SIGHUP, Cleanup);
    XSetScreenSaver(xparms.d, 8 * 3600, ssInterval, ssPreferBlanking, 
	ssAllowExposures);

    if (drawToFakeServer) {
        tileToQuery =
	    XCreatePixmap(xparms.d, DefaultRootWindow (xparms.d), 32, 32, 1);
    }


    xparms.foreground =
	AllocateColor(xparms.d, foreground, BlackPixel(xparms.d, screen));
    xparms.background =
	AllocateColor(xparms.d, background, WhitePixel(xparms.d, screen));
    xparms.w = CreatePerfWindow(&xparms, 2, 2, WIDTH, HEIGHT);
    status = CreatePerfWindow(&xparms, 2, HEIGHT+5, WIDTH, 20);
    tgcv.foreground = BlackPixel(xparms.d, screen);
    tgcv.background = WhitePixel(xparms.d, screen);
    tgc = XCreateGC(xparms.d, status, GCForeground | GCBackground, &tgcv);
   

    if (synchronous)
	XSynchronize (xparms.d, True);

    /* Get mouse pointer out of the way of the performance window.  On
       software cursor machines it will slow graphics performance.  On
       all current MIT-derived servers it will slow window 
       creation/configuration performance. */
    XWarpPointer(xparms.d, None, status, 0, 0, 0, 0, WIDTH, 20);

    /* Figure out how long to call HardwareSync, so we can adjust for that
       in our total elapsed time */
    (void) CalibrateTest(&xparms, &syncTest, 1, &syncTime);
    printf("Sync time adjustment is %6.4f msecs.\n\n", syncTime/1000);

    ForEachTest (i) {
	int child;
	char label[200];

	if (doit[i] && (test[i].versions & xparms.version)) {
	    switch (test[i].testType) {
	        case NONROP:
		    /* Simplest...just run it once */
		    strcpy (label, test[i].label);
		    ProcessTest(&xparms, &test[i], GXcopy, ~0, label);
		    break;

		case ROP:
		    /* Run it through all specified rops and planemasks */
		    for (rop = 0; rop < numRops; rop++) {
			for (pm = 0; pm < numPlanemasks; pm++) {
			    if (planemasks[pm] == ~0) {
				if (rops[rop] == GXcopy) {
				    sprintf (label, "%s", test[i].label);
				} else {
				    sprintf (label, "(%s) %s",
					ropNames[rops[rop]].name,
					test[i].label);
				}
			    } else {
				sprintf (label, "(%s 0x%x) %s",
					ropNames[rops[rop]].name,
					planemasks[pm],
					test[i].label);
			    }
			    ProcessTest(&xparms, &test[i], rops[rop],
				        planemasks[pm], label);
			} /* for pm */
		    } /* for rop */
		    break;
		
		case WINDOW:
		    /* Loop through number of children array */
		    for (child = 0; child != numSubWindows; child++) {
			test[i].parms.objects = subWindows[child];
			sprintf(label, "%s (%d kids)",
			    test[i].label, test[i].parms.objects);
			ProcessTest(&xparms, &test[i], GXcopy, ~0, label);
		    }
		    break;
	    } /* switch */
	} /* if doit */
    } /* ForEachTest */

    XDestroyWindow(xparms.d, xparms.w);

    /* Restore ScreenSaver to original state. */
    XSetScreenSaver(xparms.d, ssTimeout, ssInterval, ssPreferBlanking,
	ssAllowExposures);
    XCloseDisplay(xparms.d);
}

int
GetWords (argi, argc, argv, wordsp, nump)
    int     argi;
    int     argc;
    char    **argv;
    char    **wordsp;
    int     *nump;
{
    int	    count;

    if (argc <= argi)
	usage();
    count = 0;
    while (argv[argi] && *(argv[argi]) != '-') {
	*wordsp++ = argv[argi];
	++argi;
	count++;
    }
    *nump = count;
    return count;
}

static int
atox (s)
    char    *s;
{
    int     v, c;

    v = 0;
    while (*s) {
	if ('0' <= *s && *s <= '9')
	    c = *s - '0';
	else if ('a' <= *s && *s <= 'f')
	    c = *s - 'a' + 10;
	else if ('A' <= *s && *s <= 'F')
	    c = *s - 'A' + 10;
	v = v * 16 + c;
	s++;
    }
    return v;
}

int GetNumbers (argi, argc, argv, intsp, nump)
    int     argi;
    int     argc;
    char    **argv;
    int     *intsp;
    int     *nump;
{
    char    *words[256];
    int	    count;
    int	    i;
    int	    flip;

    count = GetWords (argi, argc, argv, words, nump);
    for (i = 0; i < count; i++) {
	flip = 0;
	if (!strncmp (words[i], "~", 1)) {
	    words[i]++;
	    flip = ~0;
	}
	if (!strncmp (words[i], "0x", 2))
	    intsp[i] = atox(words[i] + 2) ^ flip;
	else
	    intsp[i] = atoi (words[i]) ^ flip;
    }
    return count;
}

GetRops (argi, argc, argv, ropsp, nump)
    int     argi;
    int     argc;
    char    **argv;
    int     *ropsp;
    int     *nump;
{
    char    *words[256];
    int	    count;
    int	    i;
    int	    rop;

    count = GetWords (argi, argc, argv, words, nump);
    for (i = 0; i < count; i++) {
	if (!strncmp (words[i], "GX", 2))
	    words[i] += 2;
	if (!strcmp (words[i], "all")) {
	    for (i = 0; i < 16; i++)
		ropsp[i] = ropNames[i].rop;
	    *nump = 16;
	    break;
	}
	for (rop = 0; rop < 16; rop++) {
	    if (!strcmp (words[i], ropNames[rop].name)) {
		ropsp[i] = ropNames[rop].rop;
		break;
	    }
	}
	if (rop == 16) {
	    fprintf (stderr, "unknown rop name %s\n", words[i]);
	    usage ();
	}
    }
    return count;
}
