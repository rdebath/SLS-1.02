/* $XConsortium: TestInit.c,v 1.5 91/07/31 11:40:28 rws Exp $ */

/*
 * (c) Copyright 1988, Tektronix Inc.
 * 	All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Tektronix not be used
 * in advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 *
 * Tektronix disclaims all warranties with regard to this software, including
 * all implied warranties of merchantability and fitness, in no event shall
 * Tektronix be liable for any special, indirect or consequential damages or
 * any damages whatsoever resulting from loss of use, data or profits,
 * whether in an action of contract, negligence or other tortious action,
 * arising out of or in connection with the use or performance of this
 * software.
 *
 *	NAME
 *		testinit.c -- TekCMS LibTest Utilities
 *
 *	DESCRIPTION
 */

/*
 *	EXTERNAL INCLUDES
 *              Include files that must be exported to any package or
 *              program using this package.
 */
#include "LibTest.h"
#include <X11/Xlib.h>


/*
 *	INTERNAL INCLUDES
 *              Include files that need NOT be exported to any package or
 *              program using this package.
 */
#include <stdio.h>
#include <sys/stat.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#ifdef AUTOHEADER
#  include "TekCMS_TCI.ah"
#else
#  include "TekCMS_TCI.h"
#endif

/*
 *	EXTERNALS
 *		Declarations that are needed by calling modules.
 *		When using 'autohdr', these declaration will be placed
 *		in the resulting header file.
 */
#ifndef	GLOBAL
#  define	GLOBAL
#endif

GLOBAL Display	*pDpy;
GLOBAL GC	gc;


/*
 *	INTERNALS
 *		Declarations that are local to this module.
 *		(ignored by 'autohdr').
 */
    /*
     *      DEFINES
     */

#define W_WIDTH	 260
#define W_HEIGHT 450
#define W_BORDERWIDTH 0
#define W_BORDER_COLOR		"black"
#define W_BACKGROUND_COLOR	"black"

#define GC_FOREGROUND_COLOR	"white"
#define GC_BACKGROUND_COLOR	"black"

#define EVENT_MASK (\
	    ExposureMask	|\
	    StructureNotifyMask	|\
	    ButtonPressMask )

/* xColor defines */
#define	DOALL	(DoRed | DoGreen | DoBlue)
#define PAD	0

    /*
     *      EXTERNS
     */
extern int _XDefaultError();
extern int LtErrorHandler();



/* include one entry for every window, add more members if desired */
static
struct _winfo{
    XPoint pt;		/* window origin */
} winfo[] = {
	0,	0
};

#define N_WINDOWS ( sizeof(winfo)/sizeof(XPoint) )
static Window	w[N_WINDOWS];
static XImage   *wImage[N_WINDOWS];

/*
 * Include any special fonts that you will want to use, two examples given
 * Access X font id as font[0..n]
 */
static
char * fontname[] = {
	"vtbold",
	"9x15"
};
#define N_FONTS ( sizeof(fontname)/sizeof(char *) )
static Font	font[N_FONTS];



/*
 *	NAME
 *		TestInit - Connect to X Server and init X globals
 *
 *	SYNOPSIS
 */
void
TestInit()
/*
 *	DESCRIPTION
 *		Set the X Error Handler.
 *		Open the display using command line arguments.
 *
 *		Init all x global variables which includes:
 *
 *		cmap
 *		w[0..n]
 *		gc
 *		font[0..n]
 *
 *	RETURNS
 *		void
 *
 */
{

    int		i;
    XSetWindowAttributes xswa;
    XEvent	event;
    Visual	visual;
    Colormap	cmap;
    XColor	xColor,xColorExact;
    XGCValues	xgcValues;
    unsigned long xgcValueMask;
    Status	status;
    struct stat statbuf;
    char *idir = "./IDIR";
    char *rdir = "./RDIR";
    char *vdir = "./VDIR";
    char *displayname = NULL;

    /*
     * Set up our special error handler
     */

     XSetErrorHandler(LtErrorHandler);

    for (i = 1; i < CommandArgc; i++) {
	if ((strcmp(CommandArgv[i], "-display") == 0)
	  && ((i+1) < CommandArgc) && (*CommandArgv[i+1] != '-')) {
	    displayname = CommandArgv[i+1];
	    break;
	}
    }
    if (!(pDpy = XOpenDisplay(displayname))) {
	perror("Cannot open display\n");
	exit(0);
    }

#ifdef XDEBUG
    XSynchronize(pDpy,1);
#endif /* XDEBUG */

#ifdef NEED_COLORMAP
    /*
     * Color Map Initialization
     */
    cmap = XDefaultColormap(pDpy,XDefaultScreen(pDpy));
#endif /* NEED_COLORMAP */

#ifdef NEED_WINDOW
    /*
     * Window Initialization
     */
    status = XAllocNamedColor(pDpy,cmap,
		W_BACKGROUND_COLOR,&xColor,&xColorExact);
    if(!status){
	printf("Error: XAllocNamedColor status: %d\n",status);
    }
    xswa.background_pixel = xColor.pixel;

    status = XAllocNamedColor(pDpy,cmap,
		W_BORDER_COLOR,&xColor,&xColorExact);
    if(!status){
	printf("Error: XAllocNamedColor status: %d\n",status);
    }
    xswa.border_pixel = xColor.pixel;

    xswa.event_mask= EVENT_MASK;
    xswa.override_redirect = True;
    visual.visualid = CopyFromParent;
    for(i=0; i<N_WINDOWS; i++){
	/*
	 * Create and map the global windows to be used in all tests
	 */
	w[i] = XCreateWindow(pDpy, RootWindow(pDpy, 0),
	    winfo[i].pt.x, winfo[i].pt.y, W_WIDTH, W_HEIGHT, W_BORDERWIDTH,
	    DefaultDepth(pDpy, 0), InputOutput, &visual,
	    (CWEventMask | CWBackPixel | CWBorderPixel | CWOverrideRedirect),
	    &xswa);

	XMapRaised(pDpy, w[i]);

    }
#endif /* NEED_WINDOW */


#ifdef NEED_GC
    /* 
     * Graphics Context Initialization
     */
    status = XAllocNamedColor(pDpy,cmap,
		GC_BACKGROUND_COLOR,&xColor,&xColorExact);
    if(!status){
	printf("Error: XAllocNamedColor status: %d\n",status);
    }
    xgcValues.background = xColor.pixel;

    status = XAllocNamedColor(pDpy,cmap,
		GC_FOREGROUND_COLOR,&xColor,&xColorExact);
    if(!status){
	printf("Error: XAllocNamedColor status: %d\n",status);
    }
    xgcValues.foreground = xColor.pixel;

    xgcValueMask = (GCForeground | GCBackground);
    gc = XCreateGC(pDpy, w[0], xgcValueMask, &xgcValues);
#endif /* NEED_GC */


#ifdef NEED_FONTS
    /*
     * Font Initialization
     */
    for( i = 0; i < N_FONTS; i++) {
	font[i] = XLoadFont(pDpy,fontname[i]);
    }
#endif /* NEED_FONTS */

    /* check for the IDIR, RDIR and VDIR directories */
    if (stat(idir, &statbuf) == 0) {
	if (statbuf.st_mode & S_IFDIR) {
	    strcpy(TekCMS_idir, idir);
	}
    }
    if (stat(rdir, &statbuf) == 0) {
	if (statbuf.st_mode & S_IFDIR) {
	    strcpy(TekCMS_rdir, rdir);
	}
    }
    if (stat(vdir, &statbuf) == 0) {
	if (statbuf.st_mode & S_IFDIR) {
	    strcpy(TekCMS_vdir, vdir);
	}
    }
}


/*
 *	NAME
 *		TestCleanup - Clean up X resources
 *
 *	SYNOPSIS
 */
void
TestCleanup()
/*
 *	DESCRIPTION
 *		Cleanup X resources
 *
 *	RETURNS
 *		void
 *
 */
{
    int i;

    /*
     * Clean up X resources
     */
#ifdef NEED_GC
    XFreeGC(pDpy,gc);
#endif /* NEED_GC */

#ifdef NEED_WINDOW
    for( i = 0; i < N_WINDOWS; i++) {
	XDestroyWindow(pDpy,w[i]);
    }
#endif /* NEED_WINDOW */

#ifdef NEED_FONTS
    for( i = 0; i < N_FONTS; i++) {
	XUnloadFont(pDpy,font[i]);
    }
#endif /* NEED_FONTS */

}
