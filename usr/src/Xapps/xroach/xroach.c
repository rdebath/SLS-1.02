/*
    Xroach - A game of skill.  Try to find the roaches under your windows.
    
    Copyright 1991 by J.T. Anderson

    jta@locus.com
    
    This program may be freely distributed provided that all
    copyright notices are retained.

    To build:
      cc -o xroach roach.c -lX11 [-lsocketorwhatever] [-lm] [-l...]

    Dedicated to Greg McFarlane.   (gregm@otc.otca.oz.au)
    
    Squish option contributed by Rick Petkiewizc (rick@locus.com)
    
    Virtual root code adapted from patch sent by Colin Rafferty who
    borrowed it from Tom LaStrange.  Several other folks sent similar
    fixes.
*/

/* @(#)xroach.c	1.5 4/2/91 11:53:31 */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>

#include <stdio.h>
#include <math.h>
#include <malloc.h>
#include <signal.h>

#if __STDC__
#include <stdlib.h>
#else
long strtol();
double strtod();
char *getenv();
#endif

char Copyright[] = "Xroach\nCopyright 1991 J.T. Anderson";

#include "roachmap.h"

typedef unsigned long Pixel;
typedef int ErrorHandler();

#define SCAMPER_EVENT	(LASTEvent + 1)

#if !defined(GRAB_SERVER)
#define GRAB_SERVER	0
#endif

Display *display;
int screen;
Window rootWin;
unsigned int display_width, display_height;
int center_x, center_y;
GC gc;
GC gutsGC;
char *display_name = NULL;
Pixel black, white;

int done = 0;
int eventBlock = 0;
int errorVal = 0;
Bool squishRoach = False;
Pixmap squishMap;
Bool squishWinUp = False;

typedef struct Roach {
    RoachMap *rp;
    int index;
    float x;
    float y;
    int intX;
    int intY;
    int hidden;
    int turnLeft;
    int steps;
} Roach;

Roach *roaches;
int maxRoaches = 10;
int curRoaches = 0;
float roachSpeed = 20.0;

Region rootVisible = NULL;

void Usage();
void SigHandler();
void AddRoach();
void MoveRoach();
void DrawRoaches();
void CoverRoot();
int CalcRootVisible();
int MarkHiddenRoaches();
Pixel AllocNamedColor();
Window FindRootWindow();

void
main(ac, av)
int ac;
char *av[];
{
    XGCValues xgcv;
    int ax;
    char *arg;
    RoachMap *rp;
    int rx;
    float angle;
    XEvent ev;
    char *roachColor = "black";
    char *gutsColor = NULL;
    int nVis;
    int needCalc;
    Window squishWin;
    XSetWindowAttributes xswa;
    
    /*
       Process command line options.
    */
    for (ax=1; ax<ac; ax++) {
	arg = av[ax];
	if (strcmp(arg, "-display") == 0) {
	    display_name = av[++ax];
	}
	else if (strcmp(arg, "-rc") == 0) {
	    roachColor = av[++ax];
	}
	else if (strcmp(arg, "-speed") == 0) {
	    roachSpeed = strtod(av[++ax], (char **)NULL);
	}
	else if (strcmp(arg, "-roaches") == 0) {
	    maxRoaches = strtol(av[++ax], (char **)NULL, 0);
	}
	else if (strcmp(arg, "-squish") == 0) {
	    squishRoach = True;
	}
	else if (strcmp(arg, "-rgc") == 0) {
	    gutsColor = av[++ax];
	}
	else {
	    Usage();
	}
    }

    srand((int)time((long *)NULL));
    
    /*
       Catch some signals so we can erase any visible roaches.
    */
    signal(SIGKILL, SigHandler);
    signal(SIGINT, SigHandler);
    signal(SIGTERM, SigHandler);
    signal(SIGHUP, SigHandler);

    display = XOpenDisplay(display_name);
    if (display == NULL) {
	if (display_name == NULL) display_name = getenv("DISPLAY");
	(void) fprintf(stderr, "%s: cannot connect to X server %s\n", av[0],
	    display_name ? display_name : "(default)");
	exit(1);
    }

    screen = DefaultScreen(display);
    rootWin = FindRootWindow();
    black = BlackPixel(display, screen);
    white = WhitePixel(display, screen);

    display_width = DisplayWidth(display, screen);
    display_height = DisplayHeight(display, screen);
    center_x = display_width / 2;
    center_y = display_height / 2;
    
    /*
       Create roach pixmaps at several orientations.
    */
    for (ax=0; ax<360; ax+=ROACH_ANGLE) {
	rx = ax / ROACH_ANGLE;
	angle = rx * 0.261799387799;
	rp = &roachPix[rx];
	rp->pixmap = XCreateBitmapFromData(display, rootWin,
	    rp->roachBits, rp->width, rp->height);
	rp->sine = sin(angle);
	rp->cosine = cos(angle);
    }

    /*
      Create the squished pixmap
    */
    if (squishRoach) {
    	squishMap = XCreateBitmapFromData(display, rootWin,
		squish_bits, squish_width, squish_height);
    }

    roaches = (Roach *)malloc(sizeof(Roach) * maxRoaches);

    gc = XCreateGC(display, rootWin, 0L, &xgcv);
    XSetForeground(display, gc, AllocNamedColor(roachColor, black));
    XSetFillStyle(display, gc, FillStippled);

    if (squishRoach && gutsColor != NULL) {
        gutsGC = XCreateGC(display, rootWin, 0L, &xgcv);
        XSetForeground(display, gutsGC, AllocNamedColor(gutsColor, black));
        XSetFillStyle(display, gutsGC, FillStippled);
    }
    else
	gutsGC = gc;
    
    while (curRoaches < maxRoaches)
	AddRoach();
    
    XSelectInput(display, rootWin, ExposureMask | SubstructureNotifyMask);

    if (squishRoach) {
	xswa.event_mask = ButtonPressMask;
	xswa.override_redirect = True;
	squishWin = XCreateWindow(display, rootWin, 0, 0,
			display_width, display_height, 0,
			CopyFromParent, InputOnly, CopyFromParent,
			CWOverrideRedirect | CWEventMask, &xswa);
	XLowerWindow(display, squishWin);
    }
    
    needCalc = 1;
    while (!done) {
	if (XPending(display)) {
	    XNextEvent(display, &ev);
	}
	else {
	    if (needCalc) {
		needCalc = CalcRootVisible();
	    }
	    if (needCalc)
		nVis = 0;
	    else
	    	nVis = MarkHiddenRoaches();
	    if (nVis) {
		ev.type = SCAMPER_EVENT;
		if (!squishWinUp && squishRoach) {
		    XMapWindow(display, squishWin);
		    squishWinUp = True;
		}
	    }
	    else {
		if (squishWinUp && squishRoach) {
		    XUnmapWindow(display, squishWin);
		    squishWinUp = False;
		}
		if (needCalc == 0)
		    DrawRoaches();
		eventBlock = 1;
		XNextEvent(display, &ev);
		eventBlock = 0;
	    }
	}
	
	switch (ev.type) {
	    
	    case SCAMPER_EVENT:
		for (rx=0; rx<curRoaches; rx++) {
		    if (!roaches[rx].hidden)
			MoveRoach(rx);
		}
		DrawRoaches();
		XSync(display, False);
		break;
		
	    case UnmapNotify:
		if (ev.xunmap.window != squishWin)
		    needCalc = 1;
		break;

	    case MapNotify:
	    case Expose:
	    case ConfigureNotify:
		needCalc = 1;
		break;
	
	    case ButtonPress:
		checkSquish(&ev);
		break;
		
	}
    }
    
    CoverRoot();
    
    XCloseDisplay(display);
}

#define USEPRT(msg) fprintf(stderr, msg)

void
Usage()
{
    USEPRT("Usage: xroach [options]\n\n");
    USEPRT("Options:\n");
    USEPRT("       -display displayname\n");
    USEPRT("       -rc      roachcolor\n");
    USEPRT("       -roaches numroaches\n");
    USEPRT("       -speed   roachspeed\n");
    USEPRT("       -squish\n");
    USEPRT("       -rgc     roachgutscolor\n");
    
    exit(1);
}

void
SigHandler()
{
       
    /*
       If we are blocked, no roaches are visible and we can just bail
       out.  If we are not blocked, then let the main procedure clean
       up the root window.
    */
    if (eventBlock) {
	XCloseDisplay(display);
	exit(0);
    }
    else {
	done = 1;
    }
}

/*
   Find the root or virtual root window.
*/
Window
FindRootWindow()
{
    Window rootWin;
    Window realRoot;
    Atom swmVroot;
    Window rootReturn, parentReturn, *children;
    unsigned int numChildren;
    int cx;
    Atom actualType;
    Atom actualFormat;
    unsigned long nItems;
    unsigned long bytesAfter;
    Window *newRoot;
    
    /*
       Get real root window.
    */
    realRoot = rootWin = RootWindow(display, screen);
    
    /*
       Get atom for virtual root property.  If the atom doesn't
       exist, we can assume the corresponding property does not
       exist. 
    */
    swmVroot = XInternAtom(display, "__SWM_VROOT", True);
    
    if (swmVroot == None)
	return rootWin;
    
    /*
       Run children of root, looking for a virtual root.
    */
    XQueryTree(display, rootWin, &rootReturn, &parentReturn, 
		    &children, &numChildren);
    for (cx=0; cx<numChildren; cx++) {
	newRoot = NULL;
	nItems = 0;
	if (XGetWindowProperty(display, children[cx], swmVroot, 0L, 1L, False,
	    XA_WINDOW, &actualType, &actualFormat, &nItems,
	    &bytesAfter, &newRoot) == Success && actualFormat != None) {
		if (nItems >= 1) {
		    rootWin = *newRoot;
		}
		if (newRoot) {
		    XFree(newRoot);
		}
	}
	if (rootWin != realRoot) break;
    }
    XFree(children);
    
    return rootWin;
}

/*
   Generate random integer between 0 and maxVal-1.
*/
int
RandInt(maxVal)
int maxVal;
{
	return rand() % maxVal;
}

/*
   Check for roach completely in specified rectangle.
*/
int
RoachInRect(roach, rx, ry, x, y, width, height)
Roach *roach;
int rx;
int ry;
int x;
int y;
unsigned int width;
unsigned int height;
{
    if (rx < x) return 0;
    if ((rx + roach->rp->width) > (x + width)) return 0;
    if (ry < y) return 0;
    if ((ry + roach->rp->height) > (y + height)) return 0;
    
    return 1;
}

/*
   Check for roach overlapping specified rectangle.
*/
int
RoachOverRect(roach, rx, ry, x, y, width, height)
Roach *roach;
int rx;
int ry;
int x;
int y;
unsigned int width;
unsigned int height;
{
    if (rx >= (x + width)) return 0;
    if ((rx + roach->rp->width) <= x) return 0;
    if (ry >= (y + height)) return 0;
    if ((ry + roach->rp->height) <= y) return 0;
    
    return 1;
}

/*
   Give birth to a roach.
*/
void
AddRoach()
{
    Roach *r;
    
    if (curRoaches < maxRoaches) {
	r = &roaches[curRoaches++];
	r->index = RandInt(ROACH_HEADINGS);
	r->rp = &roachPix[r->index];
	r->x = RandInt(display_width - r->rp->width);
	r->y = RandInt(display_height - r->rp->height);
	r->intX = -1;
	r->intY = -1;
	r->hidden = 0;
	r->steps = RandInt(200);
	r->turnLeft = RandInt(100) >= 50;
    }
}

/*
   Turn a roach.
*/
void
TurnRoach(roach)
Roach *roach;
{
    if (roach->index != (roach->rp - roachPix)) return;

    if (roach->turnLeft) {
	roach->index += (RandInt(30) / 10) + 1;
	if (roach->index >= ROACH_HEADINGS)
	    roach->index -= ROACH_HEADINGS;
    }
    else {
	roach->index -= (RandInt(30) / 10) + 1;
	if (roach->index < 0)
	    roach->index += ROACH_HEADINGS;
    }
}

/*
   Move a roach.
*/
void
MoveRoach(rx)
int rx;
{
    Roach *roach;
    Roach *r2;
    float newX;
    float newY;
    int ii;
    
    roach = &roaches[rx];
    newX = roach->x + (roachSpeed * roach->rp->cosine);
    newY = roach->y - (roachSpeed * roach->rp->sine);
    
    if (RoachInRect(roach, (int)newX, (int)newY, 
			    0, 0, display_width, display_height)) {
	
	roach->x = newX;
	roach->y = newY;

	if (roach->steps-- <= 0) {
	    TurnRoach(roach);
	    roach->steps = RandInt(200);
	}

	for (ii=rx+1; ii<curRoaches; ii++) {
	    r2 = &roaches[ii];
	    if (RoachOverRect(roach, (int)newX, (int)newY,
		r2->intX, r2->intY, r2->rp->width, r2->rp->height)) {
	
		TurnRoach(roach);
	    }
	}
    }
    else {
	TurnRoach(roach);
    }
}
    
/*
   Draw all roaches.
*/
void
DrawRoaches()
{
    Roach *roach;
    int rx;
    
    for (rx=0; rx<curRoaches; rx++) {
	roach = &roaches[rx];
	
	if (roach->intX >= 0 && roach->rp != NULL) {
	    XClearArea(display, rootWin, roach->intX, roach->intY,
		roach->rp->width, roach->rp->height, False);
	}
    }
    
    for (rx=0; rx<curRoaches; rx++) {
	roach = &roaches[rx];
	
	if (!roach->hidden) {
	    roach->intX = roach->x;
	    roach->intY = roach->y;
	    roach->rp = &roachPix[roach->index];
    
	    XSetStipple(display, gc, roach->rp->pixmap);
	    XSetTSOrigin(display, gc, roach->intX, roach->intY);
	    XFillRectangle(display, rootWin, gc,
		roach->intX, roach->intY, roach->rp->width, roach->rp->height);
	}
	else {
	    roach->intX = -1;
	}
    }
}

/*
   Cover root window to erase roaches.
*/
void
CoverRoot()
{
    XSetWindowAttributes xswa;
    long wamask;
    Window roachWin;
    
    xswa.background_pixmap = ParentRelative;
    xswa.override_redirect = True;
    wamask = CWBackPixmap | CWOverrideRedirect;
    roachWin = XCreateWindow(display, rootWin, 0, 0,
		    display_width, display_height, 0, CopyFromParent,
		    InputOutput, CopyFromParent, wamask, &xswa);
    XLowerWindow(display, roachWin);
    XMapWindow(display, roachWin);
    XFlush(display);
}    

#if !GRAB_SERVER

int
RoachErrors(dpy, err)
Display *dpy;
XErrorEvent *err;
{
    errorVal = err->error_code;
    
    return 0;
}

#endif /* GRAB_SERVER */

/*
   Calculate Visible region of root window.
*/
int
CalcRootVisible()
{
    Region covered;
    Region visible;
    Window *children;
    int nChildren;
    Window dummy;
    XWindowAttributes wa;
    int wx;
    XRectangle rect;
    int winX, winY;
    unsigned int winHeight, winWidth;
    unsigned int borderWidth;
    unsigned int depth;
    
    /*
       If we don't grab the server, the XGetWindowAttribute or XGetGeometry
       calls can abort us.  On the other hand, the server grabs can make for
       some annoying delays.
    */
#if GRAB_SERVER
    XGrabServer(display);
#else
    XSetErrorHandler(RoachErrors);
#endif

    /*
       Get children of root.
    */
    XQueryTree(display, rootWin, &dummy, &dummy, &children, &nChildren);
    
    /*
       For each mapped child, add the window rectangle to the covered
       region.
    */
    covered = XCreateRegion();
    for (wx=0; wx<nChildren; wx++) {
	if (XEventsQueued(display, QueuedAlready)) {
	    XDestroyRegion(covered);
	    return 1;
	}
	errorVal = 0;
	XGetWindowAttributes(display, children[wx], &wa);
	if (errorVal) continue;
	if (wa.class == InputOutput && wa.map_state == IsViewable) {
	    XGetGeometry(display, children[wx], &dummy, &winX, &winY,
		&winWidth, &winHeight, &borderWidth, &depth);
	    if (errorVal) continue;
	    rect.x = winX;
	    rect.y = winY;
	    rect.width = winWidth + (borderWidth * 2);
	    rect.height = winHeight + (borderWidth * 2);
	    XUnionRectWithRegion(&rect, covered, covered);
	}
    }
    XFree(children);

#if GRAB_SERVER
    XUngrabServer(display);
#else
    XSetErrorHandler((ErrorHandler *)NULL);
#endif
    
    /*
       Subtract the covered region from the root window region.
    */
    visible = XCreateRegion();
    rect.x = 0;
    rect.y = 0;
    rect.width = display_width;
    rect.height = display_height;
    XUnionRectWithRegion(&rect, visible, visible);
    XSubtractRegion(visible, covered, visible);
    XDestroyRegion(covered);
    
    /*
       Save visible region globally.
    */
    if (rootVisible)
	XDestroyRegion(rootVisible);
    rootVisible = visible;
    
    
    /*
       Mark all roaches visible.
    */
    for (wx=0; wx<curRoaches; wx++) 
	roaches[wx].hidden = 0;

    return 0;
}

/*
   Mark hidden roaches.
*/
int
MarkHiddenRoaches()
{
    int rx;
    Roach *r;
    int nVisible;
    
    nVisible = 0;
    for (rx=0; rx<curRoaches; rx++) {
	r = &roaches[rx];
	
	if (!r->hidden) {
	    if (r->intX > 0 && XRectInRegion(rootVisible, r->intX, r->intY,
			    r->rp->width, r->rp->height) == RectangleOut) {
		r->hidden = 1;
	    }
	    else {
		nVisible++;
	    }
	}
    }
    
    return nVisible;
}

/*
   Allocate a color by name.
*/
Pixel
AllocNamedColor(colorName, dfltPix)
char *colorName;
Pixel dfltPix;
{
	Pixel pix;
	XColor scrncolor;
	XColor exactcolor;

	if (XAllocNamedColor(display, DefaultColormap(display, screen),
		colorName, &scrncolor, &exactcolor)) {
		pix = scrncolor.pixel;
	}
	else {
		pix = dfltPix;
	}

	return pix;
}

/*
 *	squishCheck - Check to see if we have to squish any roaches.
 */
checkSquish(buttonEvent)
XButtonEvent *buttonEvent;
{
    int x, y;
    int i;
    int rx;
    Roach *r;
/* */
    x = buttonEvent->x;
    y = buttonEvent->y;

    for (rx=0; rx<curRoaches; rx++) {
	r = &roaches[rx];
	if (r->rp == NULL) continue;

	if (x > r->intX &&
	    x < (r->intX + r->rp->width) &&
	    y > r->intY &&
	    y < (r->intY + r->rp->height)) {
	    XSetStipple(display, gutsGC, squishMap);
	    XSetTSOrigin(display, gutsGC, r->intX, r->intY);
	    XFillRectangle(display, rootWin, gutsGC,
		r->intX, r->intY, squish_width, squish_height);

	    /*
	     * Delete the roach
	     */
	    for (i = rx; i < curRoaches - 1; i++)
	        roaches[i] = roaches[i + 1];
	
	    curRoaches--;
	    rx--;
	}
    }

    return;
}
