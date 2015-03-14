/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)winpinmenu.c	26.25	91/09/14 SMI"

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

/***************************************************************************
* global data
***************************************************************************/

extern Atom AtomDecorAdd;
extern Atom AtomDecorHeader;
extern Atom AtomDecorPin;
extern Atom AtomDeleteWindow;
extern Atom AtomMenuLimited;
extern Atom AtomProtocols;
extern Atom AtomPushpinState;
extern Atom AtomWinAttr;
extern Atom AtomWTOther;

 
/***************************************************************************
* private data
***************************************************************************/

/* function vector for menu windows */
static ClassPinMenu classPinMenu;

/***************************************************************************
* private functions
***************************************************************************/

/*
 * afterMenuShow - called after MenuShowSync has finished showing
 *		   and possibly executing the menu.
 */
static void
afterMenuShow(win)
    WinPinMenu	*win;
{
    MenuInfo *mInfo = win->menuInfo;
    Display *dpy = win->core.client->dpy;

    if (BUTTON_INDEX_OK(mInfo, mInfo->litButton)) {
	DrawLocCursor(dpy, mInfo, mInfo->litButton, False);
	SetButton(dpy, mInfo, mInfo->litButton, False, False);
    }
    SetButton(dpy, mInfo, mInfo->menu->buttonDefault, True, True);
}

/***************************************************************************
*  private event functions
***************************************************************************/

/* 
 * eventButtonPress  - a button has gone down.
 */
static int
eventButtonPress(dpy, event, winInfo)
    Display		*dpy;
    XEvent		*event;
    WinPinMenu		*winInfo;
{
    if (! StartMenuGrabs(dpy, winInfo))
	return;

    MenuMakeFirst(winInfo->menuInfo, afterMenuShow, winInfo);

    MenuTrack(dpy, event, winInfo, winInfo);
}


static int
eventKeyEvent(dpy, event, winInfo)
    Display		*dpy;
    XEvent		*event;
    WinPinMenu		*winInfo;
{
    MenuMakeFirst(winInfo->menuInfo, afterMenuShow, winInfo);

    if (MenuHandleKeyEvent(dpy, event, winInfo, winInfo)) {
	if (! StartMenuGrabs(dpy, winInfo))
	    return;
    }
}

/* 
 * eventClientMessage  - handle a DELETE_WINDOW message.
 */
/*ARGSUSED*/
static int
eventClientMessage(dpy, event, winInfo)
	Display		*dpy;
	XEvent		*event;
	WinPinMenu	*winInfo;
{
    	if ((event->xclient.message_type == AtomProtocols) &&
            (event->xclient.data.l[0] == AtomDeleteWindow))
    	{
		DestroyClient(winInfo->core.client);
    	}
}


static int
eventEnterNotify(dpy, event, winInfo)
Display		*dpy;
XEvent		*event;
WinPinMenu	*winInfo;
{
    if (event->xany.type == EnterNotify)
        ColorWindowCrossing(dpy, event, winInfo);
}


static int
focusMenuFunc(dpy, winInfo, focus)
    Display *dpy;
    WinPinMenu *winInfo;
    Bool focus;
{
    MenuInfo *mInfo = winInfo->menuInfo;

    if (mInfo->litButton != NOBUTTON && focus) {
	SetButton(dpy, mInfo, mInfo->menu->buttonDefault, False, True);
	SetButton(dpy, mInfo, mInfo->litButton, True, False);
	DrawLocCursor(dpy, mInfo, mInfo->litButton, True);
    } else if (! focus) {
	if (BUTTON_INDEX_OK(mInfo, mInfo->litButton)) {
	    DrawLocCursor(dpy, mInfo, mInfo->litButton, False);
	    SetButton(dpy, mInfo, mInfo->litButton, False, False);
	}
	SetButton(dpy, mInfo, mInfo->menu->buttonDefault, True, True);
    }
}


/*
 * destroyMenu -- destroy the menu window resources and free any allocated
 *	data.
 */
static int
destroyMenu(dpy, winInfo)
	Display		*dpy;
	WinPinMenu 	*winInfo;
{
	MenuInfo	*menuInfo = (MenuInfo *)(winInfo->menuInfo);

	/* tell the original menu that we're gone */
	menuInfo->origmenuInfo->pinnedBrother = NULL;

	/* remove window */
	XUndefineCursor(dpy, winInfo->core.self);
	XDestroyWindow(dpy, winInfo->core.self);
	WIUninstallInfo(winInfo->core.self);

	/* free memory */
	MemFree(menuInfo);
	MemFree(winInfo);
}

/*
 * newconfigMenu - recomputes the size of the menu window
 *	Note that menus don't change size, so this is a no-op.
 */
/*ARGSUSED*/
static int 
newconfigMenu(win, pxcre)
	WinPinMenu 	*win;
	XConfigureRequestEvent *pxcre;
{
	return win->core.dirtyconfig;
}

/* menuSetParent -- callback during creation.  Since menus are internally-
 * 	created windows we must fix up certain fields that are only available
 *	after the window is mapped.
 */
static int
menuSetParent(winInfo,cli,par)
    WinGeneric 	*winInfo;
    Client 		*cli;
    WinGenericFrame *par;
{
    MenuInfo *mInfo;
    /* mark this client as owned by olwm itself */
    cli->flags = CLOlwmOwned;
    winInfo->core.client = cli;
    WinAddChild((WinGeneric *)par, winInfo);
    XReparentWindow(cli->dpy, winInfo->core.self, par->core.self, 
		    winInfo->core.x, winInfo->core.y);
    par->fcore.panewin = (WinGenericPane *)winInfo;
    mInfo = ((WinPinMenu *) winInfo)->menuInfo;
    winInfo->core.helpstring = mInfo->menu->helpstring;
    par->core.helpstring = mInfo->menu->helpstring;
}

/***************************************************************************
* global functions
***************************************************************************/

/*
 * MakePinMenu  -- create the pinned menu's menu window (around which we'll put
 *	a frame).    The window is mapped during the transition to normal
 *	state.
 */
WinPinMenu *
MakePinMenu(dpy, winInfo, origMenuInfo)
	Display		*dpy;
	WinGeneric 	*winInfo;
	MenuInfo 	*origMenuInfo;
{
	WinPinMenu 	*w;
	Window 		win;
	Window		rootWin;
        unsigned long 	valuemask;
        XSetWindowAttributes attributes;
	Atom atomList[3];
	XSizeHints 	sizeHints;
	XWMHints 	wmHints;
	MenuInfo	*newMenuInfo;

	/* Make a copy of the original MenuInfo
	 * the main difference is that a pinned menu does not have a title
 	 * in itself since the frame takes care of the title for us.
 	 * Also adjust our height to remove the title height
	 */
	newMenuInfo = MemNew(MenuInfo);
	*newMenuInfo = *origMenuInfo;
	newMenuInfo->menu = origMenuInfo->menu;
	newMenuInfo->titleWidth = 0;
	newMenuInfo->titleHeight = 0;
	newMenuInfo->menuHeight = origMenuInfo->menuHeight - origMenuInfo->titleHeight;
	newMenuInfo->buttonOffset = newMenuInfo->notitleOffset;

	newMenuInfo->childActive = False;
	newMenuInfo->pinIn = False;
	newMenuInfo->litButton = NOBUTTON;
	newMenuInfo->ringedButton = newMenuInfo->menu->buttonDefault;

	/* save a back pointer to the original and mark it pinned */
	newMenuInfo->origmenuInfo = origMenuInfo;
	newMenuInfo->pinnedBrother = NULL;
	origMenuInfo->pinnedBrother = newMenuInfo;

	/* create the associated structure */
	w = MemNew(WinPinMenu);
	w->class = &classPinMenu;
	w->core.kind = WIN_PINMENU;
	w->core.children = NULL;
	w->core.client = winInfo->core.client;
	w->core.x = newMenuInfo->menuX;	
	w->core.y = newMenuInfo->menuY;
	w->core.width = newMenuInfo->menuWidth;
	w->core.height = newMenuInfo->menuHeight;
	w->core.dirtyconfig = CWX|CWY|CWWidth|CWHeight;
	w->core.exposures = NULL;
	w->core.helpstring = (char *)0;
	w->core.colormap = winInfo->core.client->scrInfo->colormap;

	/* create the actual window */
        attributes.event_mask = ButtonReleaseMask | ButtonPressMask | 
	    ExposureMask | PropertyChangeMask | ButtonMotionMask |
	    KeyPressMask | KeyReleaseMask | EnterWindowMask;
        attributes.background_pixel = 
		winInfo->core.client->scrInfo->colorInfo.bg1Color;
	attributes.border_pixel = 0;
	attributes.colormap = w->core.colormap;
	attributes.cursor = GRV.MenuPointer;
        valuemask = CWEventMask | CWBackPixel | CWBorderPixel |
	    CWColormap | CWCursor;

        win = XCreateWindow(dpy, WinRootID(winInfo),
                        w->core.x, w->core.y,
			w->core.width, w->core.height,
                        0,
                        WinDepth(winInfo),
                        InputOutput,
                        WinVisual(winInfo),
                        valuemask,
                        &attributes);
	w->core.self = win;

	/* register the window */
	WIInstallInfo((WinGeneric *)w);

	/* first we set the properties defining what kind of 
	   OpenLook window it is */
	atomList[0] = AtomWTOther;
	atomList[1] = AtomMenuLimited;
	atomList[2] = (Atom) PIN_IN;
	XChangeProperty(dpy, win, AtomWinAttr, AtomWinAttr,
		32, PropModeReplace, (unsigned char *)atomList, 3);

	/* add a push-pin */
	atomList[0] = AtomDecorPin;
	atomList[1] = AtomDecorHeader;
	XChangeProperty(dpy, win, AtomDecorAdd, XA_ATOM,
		32, PropModeReplace, (unsigned char *)atomList, 2);

	/* set protocols */
	atomList[0] = AtomDeleteWindow;
	XChangeProperty(dpy, win, AtomProtocols, XA_ATOM,
		32, PropModeReplace, (unsigned char *)atomList, 1);

	/* now set the size hints */
	sizeHints.flags = USPosition | USSize;
	XChangeProperty(dpy, win, XA_WM_NORMAL_HINTS, XA_WM_SIZE_HINTS, 
		32, PropModeReplace, (unsigned char *)&sizeHints,
		sizeof(XSizeHints)/sizeof(long));

	/* and the wmHints */
	wmHints.flags = InputHint | StateHint;
	wmHints.initial_state = NormalState;
	wmHints.input = True;

	XChangeProperty(dpy, win, XA_WM_HINTS, XA_WM_HINTS, 
		32, PropModeReplace, (unsigned char *)&wmHints,
		sizeof(XWMHints)/sizeof(long));

	/* put the title into the header */
	XStoreName(dpy, win, newMenuInfo->menu->title);

	newMenuInfo->menuWin = (WinGeneric *)w;
	w->menuInfo = newMenuInfo;

	rootWin = w->core.client->scrInfo->rootid;
	StateNew(dpy, rootWin, win, False, (WinPane *)w);

	return w;
}

/*
 * PinMenuInit - initializes class functions
 */
/*ARGSUSED*/
int
PinMenuInit(dpy)
Display *dpy;
{
	classPinMenu.core.kind = WIN_PINMENU;
	classPinMenu.core.xevents[ButtonPress] = eventButtonPress;
	classPinMenu.core.xevents[ClientMessage] = eventClientMessage;
	classPinMenu.core.xevents[KeyPress] = eventKeyEvent;
	classPinMenu.core.xevents[KeyRelease] = eventKeyEvent;
	classPinMenu.core.xevents[EnterNotify] = eventEnterNotify;
	classPinMenu.core.xevents[Expose] = MenuEventExpose;
	classPinMenu.core.focusfunc = focusMenuFunc;
	classPinMenu.core.drawfunc = MenuEventDrawMenu;
	classPinMenu.core.destroyfunc = destroyMenu;
	classPinMenu.core.selectfunc = NULL;
	classPinMenu.core.newconfigfunc = newconfigMenu;
	classPinMenu.core.newposfunc = WinNewPosFunc;
	classPinMenu.core.setconfigfunc = WinSetConfigFunc;
	classPinMenu.core.createcallback = menuSetParent;
	classPinMenu.core.heightfunc = NULL;
	classPinMenu.core.widthfunc = NULL;
	classPinMenu.pcore.setsizefunc = NULL;
}
