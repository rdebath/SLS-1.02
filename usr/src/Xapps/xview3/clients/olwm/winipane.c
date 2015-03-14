/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)winipane.c	26.22	91/09/14 SMI"

#include <errno.h>
#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <olgx/olgx.h>

#include "i18n.h"
#include "ollocale.h"
#include "mem.h"
#include "olwm.h"
#include "win.h"
#include "menu.h"
#include "globals.h"
#include "events.h"

/***************************************************************************
* global data
***************************************************************************/

extern Atom AtomChangeState;
extern Atom AtomColorMapWindows;
extern Atom AtomOlwmTimestamp;
extern Window NoFocusWin;

/***************************************************************************
* private data
***************************************************************************/

/* border width for reparented windows */
#define NORMAL_BORDERWIDTH      0

/* This event maks if for wm-created icon panes. */
#define ICON_EVENT_MASK		(ButtonPressMask | ButtonReleaseMask | \
				 ButtonMotionMask | ExposureMask | \
				 EnterWindowMask)

/* This event mask is for clients who handle their own icons. */
#define ICON_EVENT_MASK_2        (ButtonPressMask | ButtonReleaseMask | \
				  ButtonMotionMask | EnterWindowMask)

static ClassPane classIconPane;

#define IPANE_DEFAULT_PIXMAP(w) (w)->core.client->scrInfo->pixmap[ICON_BITMAP]
#define IPANE_DEFAULT_MASK(w) 	(w)->core.client->scrInfo->pixmap[ICON_MASK]

/***************************************************************************
* private functions
***************************************************************************/

/*
 * drawIPane -- draw the pane window
 */
/*ARGSUSED*/	/* dpy arg will be used when multiple Displays supported */
static int
drawIPane(dpy, winInfo)
Display	*dpy;
WinIconPane *winInfo;
{
    Window 	pane = winInfo->core.self;
    Client 	*cli = winInfo->core.client;
    GC 		gc;
    XGCValues gcv;

    if (winInfo->iconClientWindow)
	return;

    XFillRectangle(dpy, pane, WinGC(winInfo,WORKSPACE_GC),
		0, 0, winInfo->core.width, winInfo->core.height);

    /*
     * REMIND: (1) Need to error-check icon pixmap and mask for being the 
     * proper depth.  (2) Need to handle color changes better.  Should we use 
     * a different GC?
     */
    gc = WinGC(winInfo,ICON_NORMAL_GC);

    if (winInfo->iconMask != None) {
	gc = WinGC(winInfo,ICON_MASK_GC);
	XSetClipMask(dpy, gc, winInfo->iconMask);
    }

    XCopyPlane(dpy, winInfo->iconPixmap, pane, gc,
		0, 0, winInfo->core.width, winInfo->core.height,
		0, 0, (unsigned long)1L);

    if (winInfo->iconMask != None) {
	XSetClipMask(dpy, gc, None);
    }
}


/*
 * focusIPane -- handle focus change
 */
static int
focusIPane(dpy, winInfo, focus)
Display	*dpy;
WinGeneric *winInfo;
Bool focus;
{
	/* REMIND: change background pixel of pane window */
}

/*
 * destroyIPane -- destroy the pane window resources and free any allocated
 *	data.
 */
static int
destroyIPane(dpy, winInfo)
Display	*dpy;
WinIconPane *winInfo;
{
	/* free our data and throw away window */
	WIUninstallInfo(winInfo->core.self);
        if (!winInfo->iconClientWindow)
	{
	      /* REMIND there may be other resources to be freed */
              XDestroyWindow(dpy,winInfo->core.self);
	}
	MemFree(winInfo);
}

/*
 * setconfigIPane -- change configuration of pane window
 */
/*ARGSUSED*/	/* dpy arg will be used when multiple Displays supported */
static int
setconfigIPane(dpy, winInfo)
Display	*dpy;
WinIconPane *winInfo;
{
        XWindowChanges xwc;

        if (winInfo->core.dirtyconfig)
        {
                xwc.x = winInfo->core.x;
                xwc.y = winInfo->core.y;
                xwc.width = winInfo->core.width;
                xwc.height = winInfo->core.height;
                XConfigureWindow(dpy, winInfo->core.self,
                        winInfo->core.dirtyconfig&(CWX|CWY|CWWidth|CWHeight), &xwc);
                winInfo->core.dirtyconfig &= ~(CWX|CWY|CWWidth|CWHeight);
        }
}


/* 
 * newconfigIPane - compute a new configuration given an event
 * Note:  this function must *always* be called with a configure request
 * event.
 */
static int
newconfigIPane(win, pxcre)
WinIconPane *win;
XConfigureRequestEvent *pxcre;
{
    int 	oldWidth, oldHeight;
    int 	oldX, oldY;
    WinIconFrame *winFrame = (WinIconFrame *)(win->core.parent);

    if (pxcre == NULL)
	return win->core.dirtyconfig;

    oldX = win->core.x;
    oldY = win->core.y;
    oldWidth = win->core.width;
    oldHeight = win->core.height;

    if ((pxcre->value_mask & CWHeight) && (pxcre->height != oldHeight))
    {
	win->core.height = pxcre->height;
	win->core.dirtyconfig |= CWHeight;
    }

    if ((pxcre->value_mask & CWWidth) && (pxcre->width != oldWidth))
    {
	win->core.width = pxcre->width;
	win->core.dirtyconfig |= CWWidth;
    }

    if (pxcre->value_mask & CWBorderWidth)
    {
	win->pcore.oldBorderWidth = pxcre->border_width;
    }

    if (pxcre->value_mask & (CWX | CWY)) 
    {
	FrameSetPosFromPane(winFrame, (pxcre->value_mask & CWX)?(pxcre->x):oldX,
		(pxcre->value_mask & CWY)?(pxcre->y):oldY);
    }

    if (pxcre->value_mask & (CWStackMode | CWSibling))
    {
	GFrameSetStack(winFrame, pxcre->value_mask, pxcre->detail, pxcre->above);
    }

    return win->core.dirtyconfig;
}

/* 
 * newposIPane - move to a given position (relative to parent)
 */
static int
newposIPane(win,x,y)
WinIconPane *win;
int x, y;
{
	if (win->core.x != x)
	{
		win->core.x = x;
		win->core.dirtyconfig |= CWX;
	}

	if (win->core.y != y)
	{
		win->core.y = y;
		win->core.dirtyconfig |= CWY;
	}

	return win->core.dirtyconfig;
}

/* 
 * setsizeIPane - set the pane to a particular size, and initiate a reconfigure
 */
static int
setsizeIPane(win,w,h)
WinIconPane *win;
int w, h;
{
	if (win->core.width != w)
	{
		win->core.width = w;
		win->core.dirtyconfig |= CWWidth;
	}

	if (win->core.height != h)
	{
		win->core.height = h;
		win->core.dirtyconfig |= CWHeight;
	}
}

static int
eventEnterNotify(dpy, event, winInfo)
Display		*dpy;
XEvent		*event;
WinIconPane	*winInfo;
{
    if (event->xany.type == EnterNotify)
        ColorWindowCrossing(dpy, event, winInfo);
}


/***************************************************************************
* global functions
***************************************************************************/

/*
 * MakeIconPane  -- create the pane window. Return a WinGeneric structure.
 */
WinIconPane *
MakeIconPane(cli,par,wmHints,fexisting)
Client *cli;
WinGeneric *par;
XWMHints *wmHints;
Bool fexisting;
{
	WinIconPane *w;
	WinIconFrame	*frame = (WinIconFrame *)par;
	XSetWindowAttributes xswa;
	XWindowAttributes attr;
	long valuemask;
	Window iconPane;
	Window winRoot;
	unsigned int borderWidth, depthReturn;
	Display *dpy = cli->dpy;
	int screen = cli->screen;
	Status status;
	WinGeneric *info;

	/* create the associated structure */
	w = MemNew(WinIconPane);
	w->class = &classIconPane;
	w->core.kind = WIN_ICONPANE;
	WinAddChild(par,w);
	w->core.children = NULL;
	w->core.client = cli;
	w->core.x = 0;
	w->core.y = 0;
	w->core.colormap = cli->scrInfo->colormap;
	w->core.dirtyconfig = CWX|CWY|CWWidth|CWHeight;
	w->core.exposures = NULL;
	w->core.helpstring = "olwm:Icon";
	w->iconClientWindow = False;
	w->iconPixmap = None;
	w->iconMask = None;

	frame->fcore.panewin = (WinGenericPane *)w;

	/* first try the client's icon window hint */

	if (wmHints && (wmHints->flags & IconWindowHint)) {
	    iconPane = wmHints->icon_window;
	    info = WIGetInfo(iconPane);
	    if (info != NULL && info->core.kind != WIN_PANE) {
		ErrorWarning(gettext(
		    "An existing window was named as an icon window."));
	    } else {
		if (info != NULL)
		    StateWithdrawn(info->core.client);

		status = XGetWindowAttributes(dpy, iconPane, &attr);
		
		if (status) {
		    w->core.x = attr.x;
		    w->core.y = attr.y;
		    w->core.width = attr.width;
		    w->core.height = attr.height;
		    w->core.colormap = attr.colormap;

		    w->iconClientWindow = True;
		    XSelectInput(dpy, iconPane, ICON_EVENT_MASK_2);
		    if (attr.border_width != NORMAL_BORDERWIDTH)
			XSetWindowBorderWidth(dpy, iconPane,
					      NORMAL_BORDERWIDTH);
		    goto goodicon;
		}
		ErrorWarning(gettext(
		    "An invalid window was named as an icon window."));
	    }
	}

	/* try the client's icon pixmap hint */

	if (wmHints && (wmHints->flags & IconPixmapHint)) {
	    status = XGetGeometry(dpy, wmHints->icon_pixmap, &winRoot,
		&(w->core.x), &(w->core.y),
		&(w->core.width), &(w->core.height),
		&borderWidth, &depthReturn);

	    if (status && depthReturn <= 1) {
		/* build icon pixmap window */
		xswa.border_pixel = 0;
		xswa.colormap = cli->scrInfo->colormap;
		xswa.event_mask = ICON_EVENT_MASK;
		valuemask = CWBorderPixel | CWColormap | CWEventMask;

		iconPane = XCreateWindow(dpy,WinRootID(par),
		    0, 0, w->core.width, w->core.height, 0, 
		    cli->scrInfo->depth, InputOutput, cli->scrInfo->visual,
		    valuemask, &xswa);

		w->iconPixmap = wmHints->icon_pixmap;

		/* check for the icon mask */

		if (wmHints->flags & IconMaskHint) {
		    int junkx, junky;
		    unsigned int junkw, junkh;

		    status = XGetGeometry(dpy, wmHints->icon_mask, &winRoot,
			&junkx, &junky, &junkw, &junkh,
			&borderWidth, &depthReturn);

		    if (status && depthReturn == 1)
			w->iconMask = wmHints->icon_mask;
		    else
			ErrorWarning(gettext(
			    "An invalid pixmap was named as an icon mask"));
		}
		goto goodicon;

	    } else {
		ErrorWarning(gettext(
		    "An invalid pixmap was named as an icon pixmap"));
	    }
	}

	/* use the default icon */

	w->iconClientWindow = False;
	w->iconPixmap = IPANE_DEFAULT_PIXMAP(w);
	w->iconMask   = IPANE_DEFAULT_MASK(w);

	w->core.x = w->core.y = 0;
	w->core.width = cli->scrInfo->dfltIconWidth;
	w->core.height = cli->scrInfo->dfltIconHeight;

	xswa.border_pixel = 0;
	xswa.colormap = cli->scrInfo->colormap;
	xswa.event_mask = ICON_EVENT_MASK;
	valuemask = CWBorderPixel | CWColormap | CWEventMask;
	
	iconPane = XCreateWindow(dpy, WinRootID(par),
	    0, 0, w->core.width, w->core.height, 0,
	    cli->scrInfo->depth, InputOutput, cli->scrInfo->visual,
	    valuemask, &xswa);

goodicon:

	w->core.self = iconPane;

	/* set up icon cursor */
	XDefineCursor(dpy, w->core.self, GRV.IconPointer);

	/* register the window */
	WIInstallInfo(w);

	return w;
}

/*
 * IconPaneInit -- initialise the IconPane class function vector
 */
void
IconPaneInit(dpy)
Display *dpy;
{
	classIconPane.core.kind = WIN_ICONPANE;
	classIconPane.core.xevents[Expose] = WinEventExpose;
	classIconPane.core.xevents[ButtonRelease] = PropagateEventToParent;
	classIconPane.core.xevents[MotionNotify] = PropagateEventToParent;
	classIconPane.core.xevents[ButtonPress] = PropagateEventToParent;
	classIconPane.core.xevents[EnterNotify] = eventEnterNotify;
	classIconPane.core.focusfunc = focusIPane;
	classIconPane.core.drawfunc = drawIPane;	/* NULL */
	classIconPane.core.destroyfunc = destroyIPane;
	classIconPane.core.selectfunc = drawIPane;	/* NULL */
	classIconPane.core.newconfigfunc = newconfigIPane;
	classIconPane.core.newposfunc = newposIPane;
	classIconPane.core.setconfigfunc = setconfigIPane;
	classIconPane.core.createcallback = NULL;
	classIconPane.core.heightfunc = NULL;
	classIconPane.core.widthfunc = NULL;
	classIconPane.pcore.setsizefunc = setsizeIPane;
}

/*
 * Set the icon pane's pixmap.
 */
void
IconPaneSetPixmap(dpy,winInfo,pixmap)
	Display		*dpy;
	WinIconPane	*winInfo;
	Pixmap		pixmap;
{
	if (winInfo->iconClientWindow)
		return;

	if (pixmap == None || pixmap == winInfo->iconPixmap)
		return;

	if (winInfo->iconMask == IPANE_DEFAULT_MASK(winInfo))
		winInfo->iconMask = None;

	winInfo->iconPixmap = pixmap;
}

/*
 * Set the icon pane's mask.
 */
void
IconPaneSetMask(dpy,winInfo,mask)
	Display		*dpy;
	WinIconPane	*winInfo;
	Pixmap		mask;
{
	if (winInfo->iconClientWindow)
		return;

	if (mask == None || mask == winInfo->iconMask)
		return;

	if (winInfo->iconPixmap == IPANE_DEFAULT_PIXMAP(winInfo))
		winInfo->iconPixmap = None;

	winInfo->iconMask = mask;
}


