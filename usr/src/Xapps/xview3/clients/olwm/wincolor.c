/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)wincolor.c	26.19	91/10/04 SMI"

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

extern Atom AtomColorMapWindows;

/*
 * ColorFocusLocked indicates the color focus mode.  If it is false, we are in
 * "color-follows-mouse" mode.  In this mode, colormaps are installed based on
 * the location of the mouse.  The WM_COLORMAP_WINDOWS property determines the
 * list of windows that are eligible to have their colormaps installed, but
 * changes to this property do not necessarily cause colormap installation.
 * If ColorFocusLocked is true, we are in "color-locked" mode.  In this mode,
 * a particular colormap is locked into the hardware, and colormaps do not
 * track the pointer location.  If a particular client has the focus, this
 * client will be named by ColorFocusClient, and changes to its
 * WM_COLORMAP_WINDOWS property will cause colormap changes.  If no client has
 * the focus (e.g. the colormap window is the root) then only user action can
 * cause the colormap to change.  In this case the colorFocusClient will be
 * NULL.
 */

/*
 * colorFocusWindow indicates the window that currently has the color focus.  
 * This is updated regardless of the color focus mode.
 */

extern void WinAddColorClient();
extern void WinRemoveColorClient();
extern Bool PropGetWMColormapWindows();

void	InstallDefaultColormap();
void	ColormapChange();

/***************************************************************************
* private data
***************************************************************************/

static ClassColormap classColormap;

/***************************************************************************
* private functions
***************************************************************************/

/* 
 * eventDestroy - handle destroy events on the colormap window 
 */
static int
eventDestroy(dpy, event, winInfo)
Display	*dpy;
XEvent	*event;
WinColormap *winInfo;
{
    Client *cli;
    List *cli_list = winInfo->core.colormapClients;
    List **win_list;
    WinGeneric *newfocuswin;

    /*
     * For every client in this window's client list, search that client's 
     * window list and remove this window from it.
     */
    for (cli = ListEnum(&cli_list); cli != NULL; cli = ListEnum(&cli_list)) {
	win_list = &(cli->colormapWins);
	while (*win_list != NULL) {
	    if ((*win_list)->value == winInfo) {
		ListDestroyCell(win_list);
		break;
	    }
	    win_list = &((*win_list)->next);
	}
	if (ColorFocusLocked(winInfo) && 
	    ColorFocusWindow(winInfo) == winInfo &&
	    ColorFocusClient(winInfo) == cli) {
	    if (cli->colormapWins)
		newfocuswin = cli->colormapWins->value;
	    else
		newfocuswin = (WinGeneric *) PANEOFCLIENT(cli);
	    InstallColormap(dpy, newfocuswin);
	}
    }

    ListDestroy(winInfo->core.colormapClients);
    winInfo->core.colormapClients = NULL_LIST;
    (WinFunc(winInfo,core.destroyfunc))(dpy, winInfo);
}


/* 
 * eventEnterLeaveNotify - handle enter/leave notify events on the colormap window 
 */
static int
eventEnterLeaveNotify(dpy, event, winInfo)
Display	*dpy;
XEvent	*event;
WinColormap *winInfo;
{
    if (event->xany.type == EnterNotify)
	ColorWindowCrossing(dpy, event, winInfo);
}


/*
 * eventUnmapNotify - handle the unmapping of a colormap window
 */
static int
eventUnmapNotify(dpy, event, winInfo)
Display	*dpy;
XEvent	*event;
WinColormap *winInfo;
{
    /*
     * If this is the window with the color focus, and the color focus is not 
     * locked, then we must install the colormap of the window that is now 
     * under the pointer.  REMIND: there's a race condition here, because 
     * calling InstallPointerColormap with an arg of None ends up calling 
     * QueryPointer to find the pointer's location.
     */
    if (ColorFocusWindow(winInfo) == (WinGeneric *)winInfo &&
	!ColorFocusLocked(winInfo) ) {
	InstallPointerColormap(dpy, None, 0, 0, False);
    }
}


/*
 * eventColormapNotify
 *
 * Handle changes to this window's colormap attribute.  If this window had the 
 * colormap focus, install the new colormap.
 */
static int
eventColormapNotify(dpy, event, winInfo)
    Display *dpy;
    XEvent *event;
    WinColormap *winInfo;
{
    ColormapChange(dpy, event, (WinGeneric *)winInfo);
}
	    

/*
 * destroyColormap -- destroy the colormap window resources and 
 *	              free any allocated data.
 */
static int
destroyColormap(dpy, winInfo)
Display	*dpy;
WinGeneric *winInfo;
{
#ifdef DEBUG
	if (winInfo->core.colormapClients != NULL_LIST)
	    puts("warning: destroying cmap window with non-null client list");
#endif /* DEBUG */

	/* free our data and throw away window */
	/* REMIND this test is here to avoid problems with changing
	 * a colourmap window into a pane
	 */
	if (WIGetInfo(winInfo->core.self) == winInfo)
	    WIUninstallInfo(winInfo->core.self);
	MemFree(winInfo);
}


/***************************************************************************
* global functions
***************************************************************************/


/*
 * Colormap Installation Inhibition.
 *
 * When colormap installation is inhibited, information about colormap 
 * installation is stored in the ColormapInhibitRecord structure.  If multiple 
 * installations are requested while installation is inhibited, information 
 * only from the last request is stored.  When installation is uninhibited, 
 * this information is used to install the colormap for real.  Inhibiting 
 * colormap installation prevents unnecessary colormap installation, thereby 
 * reducing flashing.
 */

static struct ColormapInhibitRecord {
    Bool inhibited;
    WinGeneric *winInfo;
} cir;


/*
 * ColormapInhibit -- inhibit or uninhibit colormap installation.
 */
void
ColormapInhibit(inhibit)
    Bool inhibit;
{
    if (inhibit) {
	cir.inhibited = True;
	cir.winInfo = NULL;
    } else {
	cir.inhibited = False;
	if (cir.winInfo != NULL) {
	    InstallColormap(cir.winInfo->core.client->dpy, cir.winInfo);
	}
	cir.winInfo = NULL;
    }
}


/*
 * InstallColormap
 *
 * Install the colormap for the given window.  If the window's colormap 
 * attribute is None, install the default screen colormap instead.  
 * This can occur if a client creates a window, sets its colormap 
 * attribute to a particular colormap, and then destroys that colormap.
 */
void
InstallColormap(dpy, winInfo)
    Display         *dpy;
    WinGeneric	    *winInfo;
{
    Colormap 	cmap = winInfo->core.colormap;

    if (cmap == None) {
    	cmap = winInfo->core.client->scrInfo->colormap;
    }

    if (cir.inhibited) {
	cir.winInfo = winInfo;
    } else {
	XInstallColormap(dpy, cmap);
	ColorFocusWindow(winInfo) = winInfo;
    }
}


/*
 * InstallDefaultColormap
 *
 * Install the default colormap for the screen on which this window resides.  
 * If `lock' is true, lock it in place as well as installing it.
 */
void
InstallDefaultColormap(dpy,winInfo,lock)
    Display	*dpy;
    WinGeneric  *winInfo;
    Bool	lock;
{
    WinRoot *rootwin = winInfo->core.client->scrInfo->rootwin;

    InstallColormap(dpy, rootwin);
    if (lock) {
	ColorFocusClient(rootwin) = rootwin->core.client;
	ColorFocusLocked(rootwin) = True;
    }
}


/*
 * Give this client the colormap focus, and lock the colormap of winInfo into 
 * the hardware.  Doesn't actually check if winInfo is one of cli's colormap 
 * windows.  Note: cli can be NULL.
 */
void
LockColormap(dpy, cli, winInfo)
    Display	*dpy;
    Client	*cli;
    WinGeneric	*winInfo;
{
    InstallColormap(dpy, winInfo);
    ColorFocusClient(winInfo) = cli;
    ColorFocusLocked(winInfo) = True;
}


/*
 * InstallPointerColormap
 *
 * Install the colormap for the leafmost window that we know about that 
 * encloses the pointer.  The boolean setfocusclient indicates whether to set 
 * the colormap focus client to this window's client.  
 *
 * If root is None, we do a QueryPointer to find out where the pointer is
 * instead of using the rootx and rooty values.
 */
void
InstallPointerColormap(dpy, root, rootx, rooty, setfocusclient)
    Display *dpy;
    Window root;
    int rootx, rooty;
    Bool setfocusclient;
{
    Window src;
    Window dest;
    Window child;
    int srcx;
    int srcy;
    int destx, desty;
    WinGeneric *wi = NULL;
    WinGeneric *t;
    Client *cli;

    if (root == None) {
	Window wjunk;
	int junk;
	unsigned int uijunk;

	/* We want only the root, rootx, and rooty; we throw the rest away */
	(void) XQueryPointer(dpy, DefaultRootWindow(dpy), &root, &wjunk,
			     &rootx, &rooty, &junk, &junk, &uijunk);
    }

    src  = root;
    dest = root;
    srcx = rootx;
    srcy = rooty;

    while (1) {
	(void) XTranslateCoordinates(dpy, src, dest, srcx, srcy,
				     &destx, &desty, &child);
	t = WIGetInfo(dest);
	if (t != NULL)
	    wi = t;
	if (child == None)
	    break;
	src = dest;
	dest = child;
	srcx = destx;
	srcy = desty;
    }
    /*
     * At this point, dest contains the leafmost window that encloses the 
     * pointer, and wi points to the window structure of the leafmost known 
     * window that encloses the pointer.
     */

    /* if we didn't find a window we know about, use the root instead */

    if (wi == NULL) {
	wi = WIGetInfo(root);
	if (wi == NULL)
	    return;
    }

    /*
     * If we are over a frame, its window button, or its resize corners,
     * use the head of the pane's colormap window list, or the pane itself if 
     * it has no list.
     */
    switch (wi->core.kind) {
    case WIN_FRAME:
    case WIN_RESIZE:
    case WIN_PUSHPIN:
    case WIN_WINBUTTON:
	if (wi->core.client->colormapWins)
	    wi = wi->core.client->colormapWins->value;
	else
	    wi = (WinGeneric *) PANEOFCLIENT(wi->core.client);
	break;
    default:
	break;
    }

    if (setfocusclient) {
	if (wi->core.colormapClients)
	    cli = (Client *) wi->core.colormapClients->value;
	else if (wi->core.client)
	    cli = wi->core.client;
	else
	    cli = NULL;
	LockColormap(dpy, cli, wi);
    } else {
	InstallColormap(dpy, wi);
    }
}


/*
 * UnlockColormap
 *
 * Turn off colormap-locked mode.
 */
void
UnlockColormap(dpy, root, rootx, rooty)
    Display *dpy;
{
    WinGeneric *rootinfo = WIGetInfo(root);

    /* REMIND: assert rootinfo != NULL */

    ColorFocusClient(rootinfo) = NULL;
    ColorFocusLocked(rootinfo) = False;
    InstallPointerColormap(dpy, root, rootx, rooty, False);
}


/*
 * ColormapChange
 *
 * Handle a change to a window's colormap attribute.
 */
void
ColormapChange(dpy, event, winInfo)
    Display *dpy;
    XEvent *event;
    WinGeneric *winInfo;
{
    if (event->xcolormap.new) {
	winInfo->core.colormap = event->xcolormap.colormap;
	if (winInfo == ColorFocusWindow(winInfo))
	    InstallColormap(dpy, winInfo);
    }
}


/*
 * ColorWindowCrossing
 *
 * Handle colormap installation on crossing events.  If we are not in
 * colormap-locked mode, install the window's colormap.
 */
void
ColorWindowCrossing(dpy, event, winInfo)
    Display *dpy;
    XEvent  *event;
    WinGeneric *winInfo;
{
    if (!ColorFocusLocked(winInfo))
	InstallColormap(dpy, winInfo);
}


/* values for tag field */
#define TAG_NEITHER 0
#define TAG_OLDLIST 1
#define TAG_NEWLIST 2

/*
 * TrackSubwindows      -- check for the WM_COLORMAP_WINDOWS prop
 *                      on a pane, if it exists, track the subwindows.
 */
void
TrackSubwindows(cli)
    Client *cli;
{
    Display	    *dpy = cli->dpy;
    Window	    pane = PANEWINOFCLIENT(cli);
    unsigned long   nItems, remain;
    Window	    *cmapwindata;
    List	    **last;
    List	    *oldlist;
    List	    *l;
    WinGeneric	    *cmwi;
    int		    i;
    WinGenericPane  *paneinfo = PANEOFCLIENT(cli);

    if (!PropGetWMColormapWindows(dpy,pane,&cmapwindata,&nItems))
   	return;

    /*
     * Register all the windows on the new list, taking care to not touch any 
     * window that was on the old list, while getting rid of windows not on 
     * the new list, and ensuring that the new list has no duplicates.  This
     * is a five-step process.  (Note: the tag field is initialized to TAG_
     * NEITHER during window creation.)
     *
     * (1) Mark all windows on the old list as TAG_OLDLIST.
     *
     * (2) Run through the WM_COLORMAP_WINDOWS property.  For each window ID 
     * in this property, there are four cases: (a) we've never seen this 
     * window ID before; (b) we've seen this ID before but it is on neither 
     * list; (c) this ID is on the old list; (d) this ID already on the new 
     * list.  For case (a), a record for the window is created and this case 
     * is subsumed by case (b).
     *
     * Cases (a) and (b) correspond to TAG_NEITHER.  Add this window to the
     * new list, add this client to the window's client list, and mark the
     * window as TAG_NEWLIST.  Case (c) corresponds to TAG_OLDLIST.  Add this
     * window to the new list and mark it as TAG_NEWLIST.  This client is
     * already on the window's client list.  Case (d) corresponds to
     * TAG_NEWLIST.  This window is already on the new list, so nothing more
     * need be done.
     *
     * (3) If we haven't encountered the pane window in the new property, add
     * it to the front of the list and mark it as TAG_NEWLIST (per ICCCM
     * section 4.1.8).
     *
     * (4) Run through the old list.  Each window marked TAG_OLDLIST is no 
     * longer on the new list, so remove this client from the window's client 
     * list.  Windows marked TAG_NEWLIST are already on the new list, so 
     * nothing need be done.  Reclaim the old list.
     *
     * (5) Reset tags of all windows on the new list to TAG_NEITHER.
     */

    oldlist = cli->colormapWins;
    cli->colormapWins = NULL_LIST;

    /* step (1) */

    l = oldlist;
    for (cmwi = ListEnum(&l); cmwi != NULL; cmwi = ListEnum(&l))
	cmwi->core.tag = TAG_OLDLIST;
    
    /* step (2) */

    last = &cli->colormapWins;
    for (i=0; i<nItems; ++i) {
	cmwi = WIGetInfo(cmapwindata[i]);

	/* Check for case (a), convert to case (b). */
	if (cmwi == NULL) {
	    cmwi = MakeColormap(cli, cmapwindata[i]);
	    if (cmwi == NULL)
		continue;
	}

	switch (cmwi->core.tag) {
	case TAG_NEITHER:				/* case (b) */
	    WinAddColorClient(cmwi, cli);
	    /* FALL THRU */
	case TAG_OLDLIST:				/* case (c) */
	    (*last) = ListCons(cmwi, NULL_LIST);
	    last = &((*last)->next);
	    cmwi->core.tag = TAG_NEWLIST;
	    break;
	case TAG_NEWLIST:				/* case (d) */
	    break;
	}
    }
    XFree((char *)cmapwindata);

    /* step (3) */

    switch (paneinfo->core.tag) {
    case TAG_NEITHER:
	WinAddColorClient(paneinfo, cli);
	/* FALL THRU */
    case TAG_OLDLIST:
	cli->colormapWins = ListCons(paneinfo, cli->colormapWins);
	paneinfo->core.tag = TAG_NEWLIST;
	break;
    case TAG_NEWLIST:
	/* it's on the new list, do nothing */
	break;
    }

    /* step (4) */

    l = oldlist;
    for (cmwi = ListEnum(&l); cmwi != NULL; cmwi = ListEnum(&l)) {
	if (cmwi->core.tag == TAG_OLDLIST)
	    WinRemoveColorClient(dpy, cmwi, cli);
	cmwi->core.tag = TAG_NEITHER;
    }
    ListDestroy(oldlist);

    /* step (5) */

    l = cli->colormapWins;
    for (cmwi = ListEnum(&l); cmwi != NULL; cmwi = ListEnum(&l))
	cmwi->core.tag = TAG_NEITHER;

    /* install colormaps as necessary */

    if (!ColorFocusLocked(paneinfo)) {
	InstallPointerColormap(dpy, None, 0, 0, False);
    } else if (ColorFocusClient(paneinfo) == cli) {
	if (cli->colormapWins)
	    InstallColormap(dpy, (WinGeneric *)cli->colormapWins->value);
	else
	    InstallColormap(dpy, paneinfo);
    }
}


/*
 * UnTrackSubwindows -- stop tracking all subwindows.  The Bool destroyed 
 * indicates that this client is being destroyed.  If so, this client loses 
 * the colormap focus.  If not, the color focus window is transferred to this
 * client's pane.
 */
void
UnTrackSubwindows(cli, destroyed)
    Client *cli;
    Bool destroyed;
{
    WinGeneric	*wi;
    List	*l;
    WinGenericPane  *paneinfo = PANEOFCLIENT(cli);

    l = cli->colormapWins;
    for (wi = ListEnum(&l); wi != NULL; wi = ListEnum(&l))
	WinRemoveColorClient(cli->dpy, wi, cli);
    ListDestroy(cli->colormapWins);
    cli->colormapWins = NULL_LIST;

    if (ColorFocusClient(paneinfo) == cli) {
	if (destroyed) {
	    ColorFocusClient(paneinfo) = NULL;
	    if (GRV.ColorLocked) {
		/* lock in the root's colormap */
		InstallColormap(cli->dpy,cli->scrInfo->rootwin);
	    } else {
		/* revert to follow-mouse */
		ColorFocusLocked(paneinfo) = False;
		InstallPointerColormap(cli->dpy, None, 0, 0, False);
	    }
	} else {
	    InstallColormap(cli->dpy,paneinfo);
	}
    }
}

/*
 * ColorUpdateColorMapWindows - handle the PropertyNotify on WM_COLORMAP_WINDOWS
 */
void
ColorUpdateColorMapWindows(cli,event)
	Client		*cli;
	XPropertyEvent	*event;
{
	if (event->state == PropertyNewValue) {
		TrackSubwindows(cli);
	} else {
		UnTrackSubwindows(cli,False);
	}
}


/*
 * MakeColormap  -- create the colormap window. Return a WinGeneric structure.
 */
WinColormap *
MakeColormap(cli,win)
Client *cli;
Window win;
{
	WinColormap *w;
        XWindowAttributes winAttr;

	/*
	 * Select input before getting window attributes in order to avoid 
	 * race conditions with destruction and colormap changes.
	 */

        XSelectInput(cli->dpy, win,
		     EnterWindowMask | ColormapChangeMask |
		     StructureNotifyMask);

        if (XGetWindowAttributes(cli->dpy, win, &winAttr) == 0)
	    return NULL;

	/* create the associated structure */
	w = MemNew(WinColormap);
	w->class = &classColormap;
	w->core.self = win;
	w->core.kind = WIN_COLORMAP;
	w->core.client = cli;
        w->core.colormap = winAttr.colormap;
	w->core.colormapClients = NULL_LIST;
	w->core.helpstring = (char *)NULL;  /* no help for colormaps */

	/* register the window */
	WIInstallInfo(w);

	return w;
}


void
ColormapInit(dpy)
Display *dpy;
{
        classColormap.core.kind = WIN_COLORMAP;
        classColormap.core.xevents[DestroyNotify] = eventDestroy;
        classColormap.core.xevents[EnterNotify] = eventEnterLeaveNotify;
        classColormap.core.xevents[LeaveNotify] = eventEnterLeaveNotify;
	classColormap.core.xevents[UnmapNotify] = eventUnmapNotify;
	classColormap.core.xevents[ColormapNotify] = eventColormapNotify;
        classColormap.core.focusfunc = NULL;
        classColormap.core.drawfunc = NULL;
        classColormap.core.destroyfunc = destroyColormap;
        classColormap.core.selectfunc = NULL;
        classColormap.core.newconfigfunc = NULL;
        classColormap.core.newposfunc = NULL;
        classColormap.core.setconfigfunc = NULL;
        classColormap.core.createcallback = NULL;
        classColormap.core.heightfunc = NULL;
        classColormap.core.widthfunc = NULL;
}


void
ColorFocusInit(dpy, root)
    Display *dpy;
    WinGeneric *root;
{
    InstallColormap(dpy, root);
    ColorFocusClient(root) = (Client *)NULL;
    ColorFocusLocked(root) = GRV.ColorLocked;
}

/* The following two functions are used when a pane is being mapped, to
 * handle the possibility that a pane has already been named as a
 * colourmap window before it was mapped.
 */

/* ColormapUnhook -- Given a window, if exists as a colourmap 
 *	window, remove it from the event dispatching lookup table
 * 	and return a pointer to the window structure.
 */
WinColormap *
ColormapUnhook(w)
Window w;
{
	WinColormap *win;

	win = WIGetInfo(w);
	if (win != NULL)
	{
	    if (win->core.kind == WIN_COLORMAP)
	    {
	        WIUninstallInfo(w);
	    }
	    else
	    {
		win = NULL;
	    }
	}
	return win;
}

/* ColormapTransmogrify -- Take a previously-saved colourmap window
 *	structure, which has been superceded by a pane window structure,
 *	and patch up client-to-window references to point to the
 *	pane window structure.  When done, destroy the colourmap
 *	window structure.
 */
void
ColormapTransmogrify(winc, winp)
WinColormap *winc;
WinPane *winp;
{
    Client *cli;
    List *cli_list;
    List *win_list;

    if (winc == NULL)
	return;

    /*
     * For every client in the colourmap window's client list, search that 
     * client's window list and change the reference.
     */
    cli_list = winc->core.colormapClients;
    for (cli = ListEnum(&cli_list); cli != NULL; cli = ListEnum(&cli_list)) {
	win_list = cli->colormapWins;
	while (win_list != NULL) {
	    if (win_list->value == winc) {
		win_list->value = winp;
		break;
	    }
	    win_list = win_list->next;
	}
    }

    /* patch up other pointers */
    if (ColorFocusWindow(winc) == winc)
	ColorFocusWindow(winc) = (WinGeneric *)winp;
#ifdef NOTDEF
    /* REMIND check that this next statement is correct */
    if (ColorFocusClient(winc) == winc->core.client)
	ColorFocusClient(winc) = winp->core.client;
#endif

    winp->core.colormapClients = winc->core.colormapClients;
    winc->core.colormapClients = NULL_LIST;

    /* the colourmap window can now be destroyed since all references
     * to it have been removed.
     */
    (WinFunc(winc,core.destroyfunc))(winc->core.client->dpy, winc);
}
