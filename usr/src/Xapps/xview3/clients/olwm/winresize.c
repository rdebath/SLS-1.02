/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)winresize.c	26.22	91/09/14 SMI"

#include <errno.h>
#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>
#include <olgx/olgx.h>

#include "ollocale.h"
#include "i18n.h"
#include "mem.h"
#include "olwm.h"
#include "win.h"
#include "globals.h"
#include "events.h"

/***************************************************************************
* global data
***************************************************************************/

extern int Resize_width, Resize_height;
extern void FrameAllowEvents();

/***************************************************************************
* private data
***************************************************************************/

static int whicholgx[] = {OLGX_UPPER_LEFT, OLGX_UPPER_RIGHT, 
	OLGX_LOWER_LEFT, OLGX_LOWER_RIGHT};
static int whichgrav[] = {NorthWestGravity, NorthEastGravity, 
	SouthWestGravity, SouthEastGravity};

static ClassResize classResize;

/***************************************************************************
* private functions
***************************************************************************/

/*
 * resizeCallback - restore the resize corner to its normal state
 */
static void
resizeCallback(dpy, winInfo)
    Display *dpy;
    WinResize *winInfo;
{
    winInfo->depressed = False;
    (WinFunc(winInfo,core.drawfunc))(dpy,winInfo);
}


/* 
 * eventButtonPress - handle button press events on the resize window 
 */
static int
eventButtonPress(dpy, event, winInfo)
Display	*dpy;
XEvent	*event;
WinResize	*winInfo;
{
	unsigned int ignoremask =
	    ModMaskMap[MOD_CONSTRAIN] | ModMaskMap[MOD_INVERT];

        if (ResolveMouseBinding(dpy, event, ignoremask) != ACTION_SELECT) {
	    FrameAllowEvents(winInfo->core.client, event->xbutton.time);
	    return;
	}

	/* draw depressed corner */
	winInfo->depressed = True;
        (WinFunc(winInfo,core.drawfunc))(dpy,winInfo);

	/* resize function will eat button release */
	ClientResize(winInfo->core.client,event,winInfo->which,
		     resizeCallback,winInfo);
}


/*
 * drawResize -- draw the resize window
 */
/*ARGSUSED*/
static int
drawResize(dpy, winInfo)
Display	*dpy;
WinResize *winInfo;
{
	WhichResize 	which = winInfo->which;
	GC		drawForegroundGC = WinGC(winInfo,FOREGROUND_GC);
	GC		drawWindowGC = WinGC(winInfo,WINDOW_GC);
	GC		drawBusyGC = WinGC(winInfo,BUSY_GC);
	Graphics_info	*gisNormal = WinGI(winInfo,NORMAL_GINFO);

	int		focusLines;	/* two lines or solid bar */

	focusLines = (GRV.FocusFollowsMouse ? 1 : 0) ^
		     (GRV.InvertFocusHighlighting ? 1 : 0);

	if ((which == upright) || (which == upleft))
	{
	    if (winInfo->core.client->isBusy) {
		XFillRectangle(dpy,winInfo->core.self,drawWindowGC,
		    (which==upleft)?(ResizeArm_Width(gisNormal)):(0),
		    ResizeArm_Height(gisNormal), 
		    winInfo->core.width-ResizeArm_Width(gisNormal),
		    winInfo->core.height-ResizeArm_Height(gisNormal));
		XFillRectangle(dpy,winInfo->core.self,drawBusyGC,
		    (which==upleft)?(ResizeArm_Width(gisNormal)):(0),
		    ResizeArm_Height(gisNormal), 
		    winInfo->core.width-ResizeArm_Width(gisNormal),
		    winInfo->core.height-ResizeArm_Height(gisNormal));
	    } else if ((winInfo->core.client->isFocus) && !focusLines) {
		if (Win3D(winInfo)) {
		    olgx_draw_box(gisNormal, winInfo->core.self,
		        (which==upleft)?(ResizeArm_Width(gisNormal)):(-2),
		        ResizeArm_Height(gisNormal), 
		        winInfo->core.width-ResizeArm_Width(gisNormal)+2,
		        winInfo->core.height-ResizeArm_Height(gisNormal)+2,
			OLGX_INVOKED, True);
		} else {
		    XFillRectangle(dpy,winInfo->core.self,drawForegroundGC,
		        (which==upleft)?(ResizeArm_Width(gisNormal)):(0),
		        ResizeArm_Height(gisNormal), 
		        winInfo->core.width-ResizeArm_Width(gisNormal),
		        winInfo->core.height-ResizeArm_Height(gisNormal));
		}
	    } else {
		XFillRectangle(dpy,winInfo->core.self,drawWindowGC,
		    (which==upleft)?(ResizeArm_Width(gisNormal)):(0),
		    ResizeArm_Height(gisNormal), 
		    winInfo->core.width-ResizeArm_Width(gisNormal),
		    winInfo->core.height-ResizeArm_Height(gisNormal));
	    }
	} else {	/* lowleft or lowright */
	    XFillRectangle(dpy, winInfo->core.self, drawWindowGC,
		(which==lowleft) ? (ResizeArm_Width(gisNormal)) : 0, 0,
		winInfo->core.width-ResizeArm_Width(gisNormal),
		winInfo->core.height-ResizeArm_Height(gisNormal));
	}

	if (!Win3D(winInfo) || !GRV.F3dResize)
	    gisNormal = WinGI(winInfo, TEXT_GINFO);

	olgx_draw_resize_corner(gisNormal, winInfo->core.self, 0, 0,
		whicholgx[winInfo->which],
		(winInfo->depressed)?(OLGX_INVOKED):(OLGX_NORMAL));
}


/*
 * DestroyResize -- destroy the resize window resources and free any allocated
 *	data.
 */
static int
destroyResize(dpy, winInfo)
Display	*dpy;
WinGeneric *winInfo;
{
	/* free our data and throw away window */
	XUndefineCursor(dpy, winInfo->core.self);
	XDestroyWindow(dpy, winInfo->core.self);
	WIUninstallInfo(winInfo->core.self);
	MemFree(winInfo);
}

/* 
 * focusselectResize - the focus or selection state has changed
 */
static int
focusResize(dpy, winInfo, selected)
Display *dpy;
WinResize *winInfo;
Bool selected;
{
	(WinFunc(winInfo,core.drawfunc))(dpy, winInfo);
}

/*
 * widthfuncResize - recomputes the width of the resize window
 */
static int 
widthfuncResize(win, pxcre)
WinResize *win;
XConfigureRequestEvent *pxcre;
{
	return Resize_width;
}

/*
 * heightfuncResize - recomputes the width of the resize window
 */
static int 
heightfuncResize(win, pxcre)
WinResize *win;
XConfigureRequestEvent *pxcre;
{
	return Resize_height;
}

/***************************************************************************
* global functions
***************************************************************************/

/*
 * MakeResize  -- create the resize window. Return a WinGeneric structure.
 */
WinResize *
MakeResize(dpy, par, which, x, y)
Display	*dpy;
WinGeneric *par;
WhichResize which;
int x,y;
{
	WinResize *w;
	Window win;
        unsigned long valuemask;
        XSetWindowAttributes attributes;

        attributes.event_mask =
	    ButtonPressMask | ButtonMotionMask | ButtonReleaseMask
		| ExposureMask | OwnerGrabButtonMask;
        attributes.win_gravity = whichgrav[which];
	attributes.cursor = GRV.ResizePointer;
        valuemask = CWEventMask | CWWinGravity | CWCursor;

        win = XCreateWindow(dpy, par->core.self,
                         x, y,
                         Resize_width, Resize_height,
                         0,
                         CopyFromParent,
                         CopyFromParent,
                         CopyFromParent,
                         valuemask,
                         &attributes);

	/* create the associated structure */
	w = MemNew(WinResize);
	w->core.self = win;
	w->class = &classResize;
	w->core.kind = WIN_RESIZE;
	WinAddChild(par,w);
	w->core.children = NULL;
	w->core.client = par->core.client;
	w->core.x = x;	
	w->core.y = y;
	w->core.width = Resize_width;
	w->core.height = Resize_height;
	w->core.dirtyconfig = CWX | CWY | CWWidth | CWHeight;
	w->core.exposures = NULL;
	w->which = which;
	w->core.helpstring = "olwm:ResizeCorner";

	/* register the window */
	WIInstallInfo(w);

        XMapRaised(dpy, win);

	return w;
}


/* ResizeInit -- initialise values for the resize class 
 */
void
ResizeInit(dpy)
Display *dpy;
{
	classResize.core.kind = WIN_RESIZE;
	classResize.core.xevents[Expose] = WinEventExpose;
	classResize.core.xevents[ButtonPress] = eventButtonPress;
	classResize.core.focusfunc= focusResize;
	classResize.core.drawfunc= drawResize;
	classResize.core.destroyfunc = destroyResize;
	classResize.core.selectfunc = NULL;
	classResize.core.newconfigfunc = WinNewConfigFunc;
	classResize.core.newposfunc = WinNewPosFunc;
	classResize.core.setconfigfunc = WinSetConfigFunc;
	classResize.core.createcallback = NULL;
	classResize.core.heightfunc = heightfuncResize;
	classResize.core.widthfunc = widthfuncResize;
}
