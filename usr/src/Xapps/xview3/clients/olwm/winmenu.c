/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)winmenu.c	26.14	91/09/14 SMI"

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
* private data
***************************************************************************/

/* function vector for menu windows */
static ClassMenu classMenu;

#define	MENU_EVENT_MASK		(PropertyChangeMask|SubstructureNotifyMask)
#define	MENU_ATTR_EVENT_MASK	(ButtonPressMask|ExposureMask)

#define MENU_SHADOW_OFFSET	(10);

/***************************************************************************
* private functions
***************************************************************************/

/* REMIND - none yet */

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
	WinMenu		*winInfo;
{
	/* REMIND - placeholder for future */
}

/* 
 * eventButtonRelease  - a button has gone up
 */
static int
eventButtonRelease(dpy, event, winInfo)
	Display		*dpy;
	XEvent		*event;
	WinMenu		*winInfo;
{
	/* REMIND - placeholder for future */
}

/* 
 * eventKeyPress  - a key has gone down
 */
static int
eventKeyPress(dpy, event, winInfo)
	Display		*dpy;
	XEvent		*event;
	WinMenu		*winInfo;
{
	/* REMIND - mouseless operation */
}

/* 
 * eventKeyRelease  - a key has gone up
 */
static int
eventKeyRelease(dpy, event, winInfo)
	Display		*dpy;
	XEvent		*event;
	WinMenu		*winInfo;
{
	/* REMIND - mouseless operation */
}

/* 
 * eventMotionNotify - mouse moved
 */
static int
eventMotionNotify(dpy, event, winInfo)
	Display		*dpy;
	XEvent		*event;
	WinMenu		*winInfo;
{
	/* REMIND - placeholder for future */
}

/*
 * destroyMenu -- destroy the menu window resources and free any allocated
 *	data.
 */
static int
destroyMenu(dpy, winInfo)
	Display		*dpy;
	WinMenu 	*winInfo;
{
	XUndefineCursor(dpy, winInfo->core.self);
	XDestroyWindow(dpy, winInfo->core.self);
#ifdef SHADOW
	XDestroyWindow(dpy, winInfo->menu.shadow);
#endif /* SHADOW */
	MemFree(winInfo);
}


/***************************************************************************
* global functions
***************************************************************************/

/*
 * MakeMenu  -- create the WinMenu structure and windows but does not
 *		map them.
 */
WinMenu *
MakeMenu(dpy, winInfo)
	Display		*dpy;
	WinGeneric 	*winInfo;
{
	WinMenu 	*w;
	Window 		win;
        unsigned long 	valuemask;
        XSetWindowAttributes attributes;
	Client		*cli = winInfo->core.client;

	/* create the associated structure */
	w = MemNew(WinMenu);
	w->class = &classMenu;
	w->core.kind = WIN_MENU;
	w->core.children = NULL;
	w->core.client = cli;
	w->core.x = 0;
	w->core.y = 0;
	w->core.width = 1;
	w->core.height = 1;
	/* REMIND - is dirtyconfig necessary??? */
	w->core.dirtyconfig = CWX|CWY|CWWidth|CWHeight;
	w->core.exposures = NULL;
	w->core.helpstring = (char *)0;

	/* Menu window. */
	attributes.event_mask = MENU_ATTR_EVENT_MASK;
	attributes.save_under = DoesSaveUnders(
					ScreenOfDisplay(dpy,cli->screen));
	attributes.border_pixel = 0;
	attributes.colormap = cli->scrInfo->colormap;
	valuemask = CWEventMask | CWSaveUnder | CWBorderPixel | CWColormap;

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

	WIInstallInfo(w);

#ifdef SHADOW
	/* REMIND - there is no pixmapGray - what to use really??? */
	attributes.background_pixmap = pixmapGray;
	attributes.save_under = DoesSaveUnders(cli->screen);
	w->shadow = XCreateWindow(dpy, WinRootID(winInfo),
			0, 0, 1, 1,
			0,
			WinDepth(winInfo),
			InputOutput,
			WinVisual(winInfo),
			CWBackPixmap |  CWSaveUnder,
			&attributes);
#endif /* SHADOW */

	XDefineCursor( dpy, win, GRV.MenuPointer );

	return w;
}

/*
 * MapMenuWindow - Configures (sizes) and maps the WinMenu windows
 */
void
MapMenuWindow(dpy,winInfo,menuInfo)
	Display		*dpy;
	WinMenu		*winInfo;
	MenuInfo	*menuInfo;
{
	XWindowChanges	changes;

	/* position, size and map menu window */
	winInfo->core.x = menuInfo->menuX;
	winInfo->core.y = menuInfo->menuY;
	winInfo->core.width = menuInfo->menuWidth;
	winInfo->core.height = menuInfo->menuHeight;
	changes.x = winInfo->core.x;
	changes.y = winInfo->core.y;
	changes.width = winInfo->core.width;
	changes.height = winInfo->core.height;
	XConfigureWindow(dpy,winInfo->core.self,
		CWX|CWY|CWWidth|CWHeight,&changes);
	XMapRaised(dpy,winInfo->core.self);

#ifdef SHADOW

	/* map shadow below menu window */
	changes.x = menuInfo.menuX + MENU_SHADOW_OFFSET;
	changes.y = menuInfo.menuY + MENU_SHADOW_OFFSET;
	changes.sibling = winInfo->core.self;
	change.stack_mode = Below;
	XConfigureWindow(dpy,winInfo->shadow,
		CWX|CWY|CWWidth|CWHeight|CWStackMode|CWSibling,&changes);
#endif /* SHADOW */

	/* save the menuinfo */
	winInfo->menuInfo = menuInfo;

	DrawMenu(dpy,menuInfo);
}

/*
 * UnmapMenuWindow - take down WinMenu windows
 */
void
UnmapMenuWindow(dpy,winInfo)
	Display		*dpy;
	WinMenu		*winInfo;
{
	XUnmapWindow(dpy,winInfo->core.self);
#ifdef SHADOW
	XUnmapWindow(dpy,winInfo->shadow);
#endif /* SHADOW */
	winInfo->menuInfo = (MenuInfo *)NULL;
}


int
MenuEventExpose(dpy, event, winInfo)
    Display *dpy;
    XEvent *event;
    WinGeneric *winInfo;
{
    MenuInfo *mInfo = NULL;

    if (winInfo->core.kind == WIN_MENU)
	mInfo = ((WinMenu *) winInfo)->menuInfo;
    else
	mInfo = ((WinPinMenu *) winInfo)->menuInfo;
	    
    if (mInfo == NULL) /*not yet reparented*/
	WinEventExpose(dpy, event, winInfo);
    else {
	SetMenuRedrawHints(dpy, event, mInfo);

	if (event->xexpose.count == 0)
	    DrawMenuWithHints(dpy, mInfo);
    }
}

/*
 * drawMenu -- draw the menu window
 */
int
MenuEventDrawMenu(dpy, winInfo)
    Display	*dpy;
    WinGeneric 	*winInfo;
{
    MenuInfo *mInfo = NULL;

    if (winInfo->core.kind == WIN_MENU)
	mInfo = ((WinMenu *) winInfo)->menuInfo;
    else
	mInfo = ((WinPinMenu *) winInfo)->menuInfo;

    if (mInfo)
	DrawMenu(dpy, mInfo);
}


/*
 * MenuInit - initialize WinMenu class functions
 */
/*ARGSUSED*/
void
MenuInit(dpy)
Display *dpy;
{
	classMenu.core.kind = WIN_MENU;
	classMenu.core.xevents[ButtonPress] = eventButtonPress;
	classMenu.core.xevents[ButtonRelease] = eventButtonRelease;
	classMenu.core.xevents[MotionNotify] = eventMotionNotify;
	classMenu.core.xevents[KeyPress] = eventKeyPress;
	classMenu.core.xevents[KeyRelease] = eventKeyRelease;
	classMenu.core.xevents[Expose] = MenuEventExpose;
	classMenu.core.drawfunc = MenuEventDrawMenu;
	classMenu.core.destroyfunc = destroyMenu;
	classMenu.core.heightfunc = NULL;
	classMenu.core.widthfunc = NULL;
}
