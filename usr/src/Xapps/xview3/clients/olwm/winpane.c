/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)winpane.c	26.14	91/09/14 SMI"

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
#include "globals.h"

/***************************************************************************
* global data
***************************************************************************/

extern Atom AtomChangeState;
extern Atom AtomColorMapWindows;
extern Atom AtomOlwmTimestamp;
extern Window NoFocusWin;
extern void ColormapChange();

/***************************************************************************
* private data
***************************************************************************/

/* border width for reparented windows */
#define NORMAL_BORDERWIDTH      0

static ClassPane classPane;

/***************************************************************************
* private functions
***************************************************************************/

/* 
 * eventEnterLeaveNotify - The pointer has entered or left the window
 */
static int
eventEnterLeaveNotify(dpy, event, winInfo)
Display	*dpy;
XEvent	*event;
WinPane	*winInfo;
{
    if (event->xany.type == EnterNotify)
        ColorWindowCrossing(dpy, event, winInfo);
}

/* 
 * eventColormapNotify
 *
 * Handle changes to this window's colormap attribute.
 */
static int
eventColormapNotify(dpy, event, winInfo)
    Display	*dpy;
    XEvent	*event;
    WinPane	*winInfo;
{
    ColormapChange(dpy, event, (WinGeneric *)winInfo);
}

/* 
 * eventUnmapNotify - the client is transitioning to withrdrawn
 */
static int
eventUnmapNotify(dpy, event, winInfo)
Display	*dpy;
XEvent	*event;
WinPane	*winInfo;
{
        if (winInfo->pcore.pendingUnmaps > 0)
        {
            --winInfo->pcore.pendingUnmaps;
        }
	else
	{
            /* Mark current state */
	    StateWithdrawn(winInfo->core.client);
	}
}


/* 
 * eventDestroyNotify - the pane window has disappeared
 *	This function can get called either during new state processing,
 *	or while app is iconic
 */
static int
eventDestroyNotify(dpy, event, winInfo)
Display	*dpy;
XEvent	*event;
WinPane	*winInfo;
{
	StateWithdrawn(winInfo->core.client);
}


/* 
 * eventPropertyNotify - handle client messages, in particular iconic requests
 */
static int
eventPropertyNotify(dpy, event, winInfo)
Display	*dpy;
XEvent	*event;
WinPane	*winInfo;
{
	ClientDistributeProperty(winInfo->core.client,event);
}

/* 
 * eventClientMessage - handle client messages, in particular iconic requests
 */
static int
eventClientMessage(dpy, event, winInfo)
Display	*dpy;
XEvent	*event;
WinPane	*winInfo;
{
	Client		*cli = winInfo->core.client;

        if (event->xclient.message_type == AtomChangeState)
        {
                if (event->xclient.data.l[0] == IconicState)
                        StateNormIcon(cli);
        }
}


/*
 * eventExtension - handle extension events
 */
static int
eventExtension(dpy, event, winInfo)
    Display	*dpy;
    XEvent	*event;
    WinPane	*winInfo;
{
#ifdef SHAPE
    XShapeEvent *se;
    Client *cli;

    if (event->xany.type == ShapeEventBase) {
	/* it's a ShapeNotify event */
	se = (XShapeEvent *) event;
	if (se->kind != ShapeBounding)
	    return;
	cli = winInfo->core.client;
	
	FrameUpdateShape(cli, cli->framewin);
    }
#endif /* SHAPE */
}


/*
 * drawPane -- draw the pane window
 */
/*ARGSUSED*/	/* dpy arg will be used when multiple Displays supported */
static int
drawPane(dpy, winInfo)
Display	*dpy;
WinGeneric *winInfo;
{
}


/*
 * focusPane -- handle focus change
 */
static int
focusPane(dpy, winInfo, focus)
Display	*dpy;
WinGeneric *winInfo;
Bool focus;
{
}

/*
 * DestroyPane -- destroy the pane window resources and free any allocated
 *	data.
 */
static int
destroyPane(dpy, winInfo)
Display	*dpy;
WinGeneric *winInfo;
{
	/* free our data and throw away window */
	WIUninstallInfo(winInfo->core.self);
	MemFree(winInfo);
}

/*
 * setconfigPane -- change configuration of pane window
 */
/*ARGSUSED*/	/* dpy arg will be used when multiple Displays supported */
static int
setconfigPane(dpy, winInfo)
Display	*dpy;
WinPane *winInfo;
{
	XConfigureEvent ce;
	XWindowChanges xwc;

	if (winInfo->core.dirtyconfig)
	{
		xwc.x = winInfo->core.x;
		xwc.y = winInfo->core.y;
		xwc.width = winInfo->core.width;
		xwc.height = winInfo->core.height;
		XConfigureWindow(dpy, winInfo->core.self, 
			winInfo->core.dirtyconfig, &xwc);
		winInfo->core.dirtyconfig &= ~(CWX|CWY|CWWidth|CWHeight);
	}

	/* send synthetic configure notify in root coordinates */
	ce.type = ConfigureNotify;
	ce.serial = 0L;
	ce.event = winInfo->core.self;
	ce.window = winInfo->core.self;
	WinRootPos(winInfo,&ce.x,&ce.y);
	ce.x -= winInfo->pcore.oldBorderWidth;
	ce.y -= winInfo->pcore.oldBorderWidth;
	ce.width = winInfo->core.width;
	ce.height = winInfo->core.height;
	ce.border_width = winInfo->pcore.oldBorderWidth;
	ce.above = None;
	ce.override_redirect = False;

	XSendEvent(dpy, winInfo->core.self, False, 
			StructureNotifyMask, (XEvent *)&ce);
}


/* 
 * newconfigPane - compute a new configuration given an event
 * Note:  this function must *always* be called with a configure request
 * event.
 */
static int
newconfigPane(win, pxcre)
WinPane *win;
XConfigureRequestEvent *pxcre;
{
    int oldWidth, oldHeight;
    Client *cli = win->core.client;
    int oldX, oldY;
    WinPaneFrame *winFrame = cli->framewin;
    void FrameMoveRelative();
    int dwidth, dheight;

    if (pxcre == NULL)
	return win->core.dirtyconfig;

    WinRootPos(win, &oldX, &oldY);
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
	FrameSetPosAbsolute(winFrame,
			    (pxcre->value_mask & CWX)?(pxcre->x):oldX,
			    (pxcre->value_mask & CWY)?(pxcre->y):oldY);
    }
    else 
    {
	dwidth = oldWidth - win->core.width;
	dheight = oldHeight - win->core.height;
	if ((dwidth != 0) || (dheight!=0))
	{
	    switch (cli->normHints->win_gravity)
	    {
		case NorthWestGravity:
		    break;
		case NorthGravity:
		    FrameMoveRelative(winFrame,dwidth/2,0);
		    break;
		case NorthEastGravity:
		    FrameMoveRelative(winFrame,dwidth,0);
		    break;
		case WestGravity:
		    FrameMoveRelative(winFrame,0,dheight/2);
		    break;
		case CenterGravity:
		    FrameMoveRelative(winFrame,dwidth/2,dheight/2);
		    break;
		case EastGravity:
		    FrameMoveRelative(winFrame,dwidth,dheight/2);
		    break;
		case SouthWestGravity:
		    FrameMoveRelative(winFrame,0,dheight);
		    break;
		case SouthGravity:
		    FrameMoveRelative(winFrame,dwidth/2,dheight);
		    break;
		case SouthEastGravity:
		    FrameMoveRelative(winFrame,dwidth,dheight);
		    break;
	    }
	}
    }


    if (pxcre->value_mask & (CWStackMode | CWSibling))
    {
	GFrameSetStack(winFrame, pxcre->value_mask, pxcre->detail, pxcre->above);
    }

    return win->core.dirtyconfig;
}

/* 
 * newposPane - move to a given position (relative to parent)
 */
static int
newposPane(win,x,y)
WinPane *win;
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
 * setsizePane - set the pane to a particular size, and initiate a reconfigure
 */
static int
setsizePane(win,w,h)
WinPane *win;
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

/***************************************************************************
* global functions
***************************************************************************/

/*
 * MakePane  -- create the pane window. Return a WinGeneric structure.
 */
WinPane *
MakePane(cli,par,win,paneattrs)
Client *cli;
WinGeneric *par;
Window win;
XWindowAttributes *paneattrs;
{
	WinPane *w;
	XSetWindowAttributes attributes;
	long mask;
	WinColormap *colorwin;

	/* this window may already be mentioned as a colourmap window.
	 * grab its colourmap window structure, and unhook it from the
	 * event dispatching table so we can register a new structure
	 * for the window.  We will call another function at the end
	 * of pane processing to re-establish the relation between this
	 * window and other structures in the system.
	 */
	colorwin = ColormapUnhook(win);

	/* create the associated structure */
	w = MemNew(WinPane);
	w->core.self = win;
	w->class = &classPane;
	w->core.kind = WIN_PANE;
	WinAddChild(par,w);
	w->core.children = NULL;
	w->core.client = cli;
	w->core.x = 0; 		/* gets fixed up later */
	w->core.y = 0;		/* gets fixed up later */
	w->core.width = paneattrs->width;
	w->core.height = paneattrs->height;
	w->core.colormap = paneattrs->colormap;
	w->core.dirtyconfig = CWX|CWY|CWWidth|CWHeight;
	w->core.exposures = NULL;
	w->pcore.oldBorderWidth = paneattrs->border_width;
	w->pcore.oldSaveUnder = paneattrs->save_under;
	w->core.helpstring = (char *)NULL;	/* no help */

	cli->framewin->fcore.panewin = (WinGenericPane *)w;

	/* register the window */
	WIInstallInfo(w);

	/* Put the window in the save set so it doesn't go away */
	XChangeSaveSet(cli->dpy,win,SetModeInsert);

        /*
         * Since the pane is reparented, save-unders are not useful.
         * In the code above the save-under attribute is propogated to
         * the frame, so it is safe to remove it here.  But don't do this for 
	 * InputOnly windows.
         */
	if (paneattrs->class == InputOutput) {
	    attributes.save_under = False;
	    XChangeWindowAttributes(cli->dpy, win,
				    (unsigned long) CWSaveUnder, &attributes);

	    /*
	     * Change the border width if necessary.  The border width of 
	     * InputOnly windows is zero by definition.
	     */
	    if (paneattrs->border_width != NORMAL_BORDERWIDTH)
		XSetWindowBorderWidth(cli->dpy, win, NORMAL_BORDERWIDTH);
	}

        /*
	 * Focus Lenience.  Be lenient about enforcing the requirement that
	 * clients set the input hint in WM_HINTS before they can get keyboard
	 * input.  If this flag is set, and if the focus mode is NoInput, then
	 * force the mode to be passive.  This way, if a client either fails
	 * to set the WM_HINTS.input field, or fails to write WM_HINTS at all,
	 * it can still get keyboard input.
	 * 
	 * REMIND This kind of flag should be implemented on a
	 * client-by-client basis, not on a global basis.
         */
        if (GRV.FocusLenience && cli->focusMode == NoInput ) {
            cli->focusMode = Passive;
        }

        /* Reparent the pane */
        XReparentWindow(cli->dpy, win, par->core.self,
	    w->core.x, w->core.y);

	/* we may have saved colourmap manipulation information at 
	 * the beginning.  Re-establish the connection between this window
	 * and other structures based on the old structure.
	 */
	ColormapTransmogrify(colorwin, w);

	return w;
}

/*
 * PaneInit -- initialise the Pane class function vector
 */
void
PaneInit(dpy)
Display *dpy;
{
	classPane.core.kind = WIN_PANE;
	classPane.core.xevents[EnterNotify] = eventEnterLeaveNotify;
	classPane.core.xevents[LeaveNotify] = eventEnterLeaveNotify;
	classPane.core.xevents[ColormapNotify] = eventColormapNotify;
	classPane.core.xevents[UnmapNotify] = eventUnmapNotify;
	classPane.core.xevents[DestroyNotify] = eventDestroyNotify;
	classPane.core.xevents[PropertyNotify] = eventPropertyNotify;
	classPane.core.xevents[ClientMessage] = eventClientMessage;
	classPane.core.extEventHdlr = eventExtension;
	classPane.core.focusfunc = focusPane;
	classPane.core.drawfunc = NULL;
	classPane.core.destroyfunc = destroyPane;
	classPane.core.selectfunc = NULL;
	classPane.core.newconfigfunc = newconfigPane;
	classPane.core.newposfunc = newposPane;
	classPane.core.setconfigfunc = setconfigPane;
	classPane.core.createcallback = NULL;
	classPane.core.heightfunc = NULL;
	classPane.core.widthfunc = NULL;
	classPane.pcore.setsizefunc = setsizePane;
}
