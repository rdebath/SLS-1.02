/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)winicon.c	26.27	91/09/14 SMI"

#include <errno.h>
#include <stdio.h>
#include <string.h>
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
#include "slots.h"
#include "group.h"

extern 	Bool	PropGetWMName();
extern 	Bool	PropGetWMIconName();

/***************************************************************************
* private data
***************************************************************************/

/* events in the icon window that are interesting */
#define ICON_EVENT_MASK        (ButtonPressMask | ButtonReleaseMask | \
				ExposureMask | ButtonMotionMask | \
				EnterWindowMask | FocusChangeMask)

/* border sizes, in pixels */
#define ICON_HORZBORDER 3
#define ICON_VERTBORDER 3

/* Class function vector */
static ClassIconFrame classIconFrame;

/***************************************************************************
* forward-declared functions
***************************************************************************/

/***************************************************************************
* private event functions
***************************************************************************/

static int
menuPressIcon(dpy,event,iconInfo)
Display *dpy;
XEvent *event;
WinIconFrame *iconInfo;
{
    if (iconInfo->core.client->wmDecors->menu_type != MENU_NONE)
	ShowStandardMenu(iconInfo, event, False);
}

static int
selectDoubleClickIcon(dpy,event,iconInfo)
Display *dpy;
XEvent *event;
WinIconFrame *iconInfo;
{
	StateIconNorm(iconInfo->core.client);
}

static int
adjustClickIcon(dpy,event,iconInfo)
Display *dpy;
XEvent *event;
WinIconFrame *iconInfo;
{
        ToggleSelection(iconInfo->core.client, event->xbutton.time);
}

/***************************************************************************
* private functions
***************************************************************************/

/*
 * iconCalcName - calc position/size of icon name
 */
static void
iconCalcName(winIcon,pane)
	WinIconFrame *winIcon;
	Window 	pane;
{
	Display *dpy = winIcon->core.client->dpy;

	winIcon->nameLength = strlen(winIcon->fcore.name);
	winIcon->nameWidth = XTextWidth(GRV.IconFontInfo, winIcon->fcore.name, 
		winIcon->nameLength);
	winIcon->nameX = (winIcon->core.width - winIcon->nameWidth)/2;

	/* 
	 * Position the text one pixel above the ICON_VERTBORDER and
	 * the descent of the font
	 */
	winIcon->nameY = winIcon->core.height - ICON_VERTBORDER - 1
				- GRV.IconFontInfo->max_bounds.descent;
}

/* 
 * iconSetName -- set the icon name and possibly redraw
 */
static void
iconSetName(winIcon,pane)
	WinIconFrame *winIcon;
	Window pane;
{
	Display *dpy = winIcon->core.client->dpy;

	if (winIcon->fcore.name)
		MemFree(winIcon->fcore.name);

	if (!PropGetWMIconName(dpy,pane,&(winIcon->fcore.name)) &&
	    !PropGetWMName(dpy,pane,&(winIcon->fcore.name))) {
		winIcon->fcore.name = MemNewString(GRV.DefaultWinName);
	}

	iconCalcName(winIcon,pane);

	if (!winIcon->core.dirtyconfig)
		(WinFunc(winIcon,core.drawfunc))(dpy,winIcon);
}

/* selectDragIcon -- the user has held the select button down long enough
 * to initiate a drag.  Unpin the icon slot and start a window-move.
 */
static int
selectDragIcon(dpy, ev, iframe, lastpress)
Display *dpy;
XEvent *ev;
WinIconFrame *iframe;
XButtonEvent *lastpress;
{
	SlotFree(iframe);
	iframe->fManuallyPositioned = True;
	ClientMove(iframe->core.client,lastpress);
}


/*
 * newconfigIcon -- compute a new configuration of icon window
 */
static int
newconfigIcon(winInfo, pxcre)
WinIconFrame *winInfo;
XConfigureRequestEvent *pxcre;
{
        Client 	*cli = winInfo->core.client;
        WinPane *winPane = (WinPane *)winInfo->fcore.panewin;
        int 	neww,newh;

        neww = winInfo->fcore.panewin->core.width + 2*widthBothIcon(winInfo);
        newh = winInfo->fcore.panewin->core.height + heightTopIcon(winInfo) +
            heightBottomIcon(winInfo);

        if (neww != winInfo->core.width)
        {
                winInfo->core.width = neww;
                winInfo->core.dirtyconfig |= CWWidth;
        }

        if (newh != winInfo->core.height)
        {
                winInfo->core.height = newh;
                winInfo->core.dirtyconfig |= CWHeight;
        }

        if (winInfo->core.dirtyconfig)
        {
                (WinFunc(winPane,core.newposfunc))(winPane, 
			widthBothIcon(winInfo), heightTopIcon(winInfo));
        }

	if (winInfo->core.dirtyconfig & (CWWidth | CWHeight))
	{
                iconCalcName(winInfo,PANEWINOFCLIENT(cli));
	}

	return winInfo->core.dirtyconfig;
}

/*
 * The icon is being moved to a new (x,y) location.  If the icon slot has not
 * yet been allocated, do so if appropriate.  Otherwise, blindly accept the
 * (x,y) position.
 */
static int
newposIcon(winInfo,x,y)
WinIconFrame *winInfo;
int x,y;
{
	WinNewPosFunc(winInfo,x,y);
	if (winInfo->iconslot == NULL &&
		!ClientIsPinnable(winInfo->core.client) &&
		winInfo->core.client->transientFor == 0) {
	    SlotAlloc(winInfo, winInfo->fManuallyPositioned, GRV.FSnapToGrid);
	}
	return winInfo->core.dirtyconfig;
}


static void
drawDashedRect(dpy, winInfo, win, x, y, w, h)
    Display *dpy;
    WinIconFrame *winInfo;
    Window win;
    int x, y, w, h;
{
    XPoint pts[5];

    pts[0].x = x;	pts[0].y = y;
    pts[1].x = x;	pts[1].y = y + h;
    pts[2].x = x + w;	pts[2].y = y + h;
    pts[3].x = x + w;	pts[3].y = y;
    pts[4].x = x;	pts[4].y = y;

    /*
     * The following is necessary because IconBorderGC uses the LineOnOffDash
     * line-style, which is faster than LineDoubleDash on some servers.
     */
    XDrawLines(dpy, win, WinGC(winInfo,WORKSPACE_GC),
		pts, 5, CoordModeOrigin);
    XDrawLines(dpy, win, WinGC(winInfo,ICON_BORDER_GC),
		pts, 5, CoordModeOrigin);
}

/*
 * drawIconBorder -- based on the value of select, draw the border for an icon
 */
static void
drawIconBorder(dpy, winInfo, select)
Display *dpy;
WinIconFrame *winInfo;
Bool    select;
{
	int 	x, y;           /* values for use with */
	unsigned int width, height;  /* rectangle drawn for border */
	Window 	w = winInfo->core.self;
	GC	borderGC = WinGC(winInfo,BORDER_GC);
	GC	workspaceGC = WinGC(winInfo,WORKSPACE_GC);
	
	x = y = 0;
	width = winInfo->core.width - 1;
	height = winInfo->core.height - 1;

	/*
	 * If 3D is used, give "borderless" icons.  Otherwise, give black and 
	 * white borders.
	 */
	if (select) {
	    XDrawRectangle(dpy, w, borderGC,
			   x, y, width, height );
	    XDrawRectangle(dpy, w, borderGC,
			   x+1, y+1, width-2, height-2 );
	    XDrawRectangle(dpy, w, borderGC,
			   x+2, y+2, width-4, height-4 );
	} else {
	    XDrawRectangle(dpy, w, workspaceGC,
			   x, y, width, height);
	    if (Win3D(winInfo)) {
		XDrawRectangle(dpy, w, workspaceGC, 
			       x+1, y+1, width-2, height-2);
	    } else {
#ifdef notdef
		XDrawRectangle(dpy, w, IconBorderGC, 
			       x+1, y+1, width-2, height-2);
#endif /* notdef */
		drawDashedRect(dpy, winInfo, w, x+1, y+1, width-2, height-2);
	    }

	    XDrawRectangle(dpy, w, workspaceGC,
			   x+2, y+2, width-4, height-4);
	}

#ifdef notdef
/*
 * This stuff was used for the attempt at 3D-look icons.
 * It has been abandoned in favor of the "borderless" icon look.
 */

        /* initial values for first rectangle */
        x = 0;
        y = 0;
        /* need to subtract one, based on how XDrawRectangle works */
        width = winInfo->core.width - 1;
        height = winInfo->core.height - 1;

        /* draw three rectangles for border */
	for ( rectangle = 0 ; rectangle < 3 ; rectangle++ )
	{
              switch( rectangle )
              {
              case 0:         /* outermost rectangle */
                      if (Win3D(winInfo))
                      {
                              if ( select )
                                      olgxState = OLGX_INVOKED;
                              else
                                      olgxState = OLGX_NORMAL;

                              olgx_draw_box( olgx_gisnormal,
                                             winInfo->core.self,
                                             x, y, width+1, height+1,
                                             olgxState, 0 );
                              drawRectangle = False;
                      }
                      else
                      {
                              highlightGC = select
                                              ? DrawSelectedGC
                                              : DrawBackgroundGC;
                              drawRectangle = True;
                      }
                      break;
              case 1:         /* middle rectangle */
                      if ( select )
                                      highlightGC = DrawSelectedGC;
                      else if (Win3D(winInfo))
                              highlightGC = DrawBackgroundGC;
                      else    /* REMIND eventually need to handle
                               * IconBorder resource when 2d & ColorDisplay
                               */
                              highlightGC = IconBorderGC;
                      drawRectangle = True;
                      break;
              case 2:         /* innermost rectangle */
              default:
                      highlightGC = select ? DrawSelectedGC
                                           : DrawBackgroundGC;
                      drawRectangle = True;
                      break;
              }

              if ( drawRectangle )
                      XDrawRectangle( dpy, winInfo->core.self, highlightGC,
                                      x, y, width, height );
              x++;
              y++;
              width -= 2;
              height -= 2;
      }
#endif /* notdef */
}

/*
 * drawIcon -- draw the icon window
 */
/*ARGSUSED*/  /* dpy arg will be used when multiple Displays supported */
static int
drawIcon(dpy, winInfo)
Display       *dpy;
WinIconFrame *winInfo;
{
	Window		frameWin = winInfo->core.self;

	XFillRectangle(dpy, frameWin, WinGC(winInfo,WORKSPACE_GC),
		0, 0, winInfo->core.width, winInfo->core.height);

	/* draw icon name */
	if (winInfo->core.client->wmDecors->flags & WMDecorationIconName)
	    XDrawString(dpy, frameWin, WinGC(winInfo,ICON_NORMAL_GC),
		winInfo->nameX, winInfo->nameY, 
		winInfo->fcore.name, winInfo->nameLength);

	/* draw border */
	drawIconBorder(dpy, winInfo, winInfo->core.client->isSelected);
}


/*
 * DestroyIcon -- destroy the icon window resources and free any allocated
 *    data.
 */
static int
destroyIcon(dpy, winInfo)
Display       *dpy;
WinIconFrame *winInfo;
{
	/* 
	 * Free our data and throw away window
	 */
	SlotFree(winInfo);
	ListDestroy(winInfo->core.children);
	MemFree(winInfo->fcore.name);
	XUndefineCursor(dpy, winInfo->core.self);
	XDestroyWindow(dpy,winInfo->core.self);
	WIUninstallInfo(winInfo->core.self);
	MemFree(winInfo);
}

/*
 * heightIconName - returns the height of the icon name portion of
 *	the total icon height.
 */
static int
heightIconName(win)
WinIconFrame	*win;
{
	if (win->core.client->wmDecors->flags & WMDecorationIconName)
	{
		return (GRV.IconFontInfo->max_bounds.ascent +
		       	GRV.IconFontInfo->max_bounds.descent + 
			ICON_VERTBORDER);
	} else {
		return 0;
	}
}

/*
 * heightTopIcon - returns the height of the top portion of the icon window.
 *	If the IconPane (image/window) is too small then increase the
 *	the top height to bring total height to the minimal icon
 *	window size of ICON_WIN_HEIGHT.  Otherwise use the default
 *	border size.
 */
static int
heightTopIcon(win)
WinIconFrame	*win;
{
	WinIconPane *winPane = (WinIconPane *)(win->fcore.panewin);
	int availHeight,basicbottom;

	availHeight = ICON_WIN_HEIGHT - heightIconName(win);

	if (winPane->core.height < availHeight) {
		return (availHeight-winPane->core.height)/2;
	} else {
		return ICON_VERTBORDER;
	}
}

/*
 * heightBottomIcon - returns the height of the bottom portion of
 *	the icon window - which includes the icon name string (if any).
 *	If the IconPane (image/window) is too small then increase the
 *	the bottom height to bring total height to the minimal icon
 *	window size of ICON_WIN_HEIGHT.  Otherwise use the default
 *	border size.
 */
static int
heightBottomIcon(win)
WinIconFrame	*win;
{
	WinIconPane *winPane = (WinIconPane *)(win->fcore.panewin);
	int nameHeight,availHeight;

	nameHeight = heightIconName(win);

	availHeight = ICON_WIN_HEIGHT - nameHeight;

	if (winPane->core.height < availHeight) {
		return (availHeight - winPane->core.height)/2 + nameHeight;
	} else {
		return nameHeight + ICON_VERTBORDER;
	}
}

/* The icon pane has the same border width on either side, so this function
 * is used to calculate both border widths.
 */
static int
widthBothIcon(win)
WinIconFrame	*win;
{
	WinIconPane *winPane = (WinIconPane *)(win->fcore.panewin);

	if (winPane->iconClientWindow)
	{
	    return ICON_HORZBORDER;
	}
	else
	{
	    if (winPane->core.width < ICON_WIN_WIDTH - 2*ICON_HORZBORDER)
	    {
		return (ICON_WIN_WIDTH-winPane->core.width)/2;
	    }
	    else
		return ICON_HORZBORDER;
	}
}

/* 
 * fullrestoreIcon
 *	Switch icon menus and if this client is iconic then
 *	open it.
 */
static int
fullrestoreIcon(client)
Client	*client;
{
	WinIconFrame	*iconInfo = client->iconwin;

	if (client->wmState == IconicState)
	    StateIconNorm(client);

	iconInfo->fcore.fullsize = !iconInfo->fcore.fullsize;
}


/***************************************************************************
* global functions
***************************************************************************/

/*
 * MakeIcon  -- create the icon window. Return a WinIconFrame structure.
 *	Note that unlike most Make functions, icons are not mapped right
 *	away.
 */
WinIconFrame *
MakeIcon(cli,panewin,paneattrs)
Client *cli;
Window panewin;		
XWindowAttributes *paneattrs;
{
	Display		*dpy = cli->dpy;
	WinIconFrame	*w;
        XSetWindowAttributes attributes;
        unsigned long   valuemask;
	XWMHints	*wmHints = cli->wmHints;

	/* create the window structure */
	w = MemNew(WinIconFrame);
	w->core.kind = WIN_ICON;
	w->class = &classIconFrame;
	w->core.parent = NULL;
	w->core.children = NULL;
	w->core.client = cli;
	w->core.width = ICON_WIN_WIDTH;
	w->core.height = ICON_WIN_HEIGHT;

	/* fill out  the associated structure */
	w->core.dirtyconfig = CWX|CWY|CWWidth|CWHeight;
	w->core.colormap = None;
	w->core.exposures = NULL;
	w->core.helpstring = "olwm:Icon";

	/* create the icon frame */
	attributes.border_pixel = 0;
	attributes.colormap = cli->scrInfo->colormap;
	attributes.event_mask = ICON_EVENT_MASK;
	valuemask = CWBorderPixel | CWColormap | CWEventMask;
	w->core.self = XCreateWindow(dpy, 
		cli->scrInfo->rootid,
		w->core.x, w->core.y, 1, 1, 0,
		cli->scrInfo->depth,
		InputOutput,
		cli->scrInfo->visual,
		valuemask, &attributes);

	/* install icon frame in client */
	cli->iconwin = w;	/* REMIND: should be called cli->iconframe */
	
	/* set the position - either from position or icon slot */
	if (wmHints && (wmHints->flags & IconPositionHint)) 
	{
		w->core.x = wmHints->icon_x;
		w->core.y = wmHints->icon_y;
		w->fManuallyPositioned = True;
	}
	else
	{
		/* to be fixed up at config time */
		w->core.x = w->core.y = 0;
		w->fManuallyPositioned = False;
	}

	/* register the window */
	WIInstallInfo(w);

	/* set cursor for frame */
	XDefineCursor( dpy, w->core.self, GRV.IconPointer );

	iconSetName(w,panewin);

	w->fcore.fullsize = False;

	return w;
}

void
IconInit(dpy)
Display *dpy;
{
	classIconFrame.core.kind = WIN_ICON;
	classIconFrame.core.xevents[Expose] = WinEventExpose;
	classIconFrame.core.xevents[ButtonRelease] = GFrameEventButtonRelease;
	classIconFrame.core.xevents[MotionNotify] = GFrameEventMotionNotify;
	classIconFrame.core.xevents[ButtonPress] = GFrameEventButtonPress;
	classIconFrame.core.xevents[EnterNotify] = GFrameEventEnterNotify;
	classIconFrame.core.xevents[FocusIn] = GFrameEventFocus;
	classIconFrame.core.xevents[FocusOut] = GFrameEventFocus;
	classIconFrame.core.focusfunc = GFrameFocus;
	classIconFrame.core.drawfunc = drawIcon;
	classIconFrame.core.destroyfunc = destroyIcon;
	classIconFrame.core.selectfunc = GFrameSelect;
	classIconFrame.core.newconfigfunc = newconfigIcon;
	classIconFrame.core.newposfunc = newposIcon;
	classIconFrame.core.setconfigfunc = GFrameSetConfigFunc;
	classIconFrame.core.createcallback = NULL;
	classIconFrame.core.heightfunc = NULL;
	classIconFrame.core.widthfunc =  NULL;
	classIconFrame.fcore.heighttop = heightTopIcon;
	classIconFrame.fcore.heightbottom = heightBottomIcon;
	classIconFrame.fcore.widthleft = widthBothIcon;
	classIconFrame.fcore.widthright = widthBothIcon;
	classIconFrame.fcore.menuPress = menuPressIcon;
	classIconFrame.fcore.adjustPress = NULL;
	classIconFrame.fcore.adjustClick = adjustClickIcon;
	classIconFrame.fcore.selectPress = NULL;
	classIconFrame.fcore.selectClick = NULL;
	classIconFrame.fcore.selectDoubleClick = selectDoubleClickIcon;
	classIconFrame.fcore.selectDrag = selectDragIcon;
	classIconFrame.fcore.fullrestoreToggle = fullrestoreIcon;
}

/*
 * DrawIconToWindowLines -- draw the open (close) lines when a window is
 *      becoming an icon or vice-versa
 */
void
DrawIconToWindowLines(dpy, iconInfo, winInfo)
Display *dpy;
WinPaneFrame *winInfo;
WinIconFrame *iconInfo;
{
        int     ii;
	GC	rootGC = WinGC(winInfo,ROOT_GC);
	Window	root = WinRootID(winInfo);

        XGrabServer(dpy);

        for(ii=0; ii < GRV.IconFlashCount ; ii++)
        {
                /* draw the close lines */
                XDrawLine(dpy, root, rootGC,
			iconInfo->core.x, 
			iconInfo->core.y,
			winInfo->core.x, 
			winInfo->core.y);
                XDrawLine(dpy, root, rootGC,
			iconInfo->core.x,
			(int)(iconInfo->core.y + iconInfo->core.height),
			winInfo->core.x,
			(int)(winInfo->core.y + winInfo->core.height));
                XDrawLine(dpy, root, rootGC,
			(int)(iconInfo->core.x + iconInfo->core.width),
			iconInfo->core.y,
			(int)(winInfo->core.x + winInfo->core.width),
			winInfo->core.y);
                XDrawLine(dpy, root, rootGC,
			(int)(iconInfo->core.x + iconInfo->core.width),
			(int)(iconInfo->core.y + iconInfo->core.height),
			(int)(winInfo->core.x + winInfo->core.width),
			(int)(winInfo->core.y + winInfo->core.height));

                XFlush(dpy);
		olwm_usleep((unsigned) GRV.IconFlashOnTime);

                /* erase the close lines */
                XDrawLine(dpy, root, rootGC,
			iconInfo->core.x,
			iconInfo->core.y,
			winInfo->core.x,
			winInfo->core.y);
                XDrawLine(dpy, root, rootGC,
			iconInfo->core.x,
			(int)(iconInfo->core.y + iconInfo->core.height),
			winInfo->core.x,
			(int)(winInfo->core.y + winInfo->core.height));
                XDrawLine(dpy, root, rootGC,
			(int)(iconInfo->core.x + iconInfo->core.width),
			iconInfo->core.y,
			(int)(winInfo->core.x + winInfo->core.width),
			winInfo->core.y);
                XDrawLine(dpy, root, rootGC,
			(int)(iconInfo->core.x + iconInfo->core.width),
			(int)(iconInfo->core.y + iconInfo->core.height),
			(int)(winInfo->core.x + winInfo->core.width),
			(int)(winInfo->core.y + winInfo->core.height));
                XFlush(dpy);
		olwm_usleep((unsigned) GRV.IconFlashOffTime);
        }

        XUngrabServer(dpy);
}

/* 
 * IconUpdateName -- the icon name property has been changed
 */
void
IconUpdateName(cli,event)
	Client		*cli;
	XPropertyEvent	*event;
{
	iconSetName(cli->iconwin,PANEWINOFCLIENT(cli));
}

/*
 * Set the icon's (x,y) location explicitly.  This information is typically
 * taken from the WM_HINTS structure.  Since the coordinates specify the 
 * absolute position of the icon pane, we must subtract the icon border to get 
 * the position if the icon frame.
 */
void
IconSetPos(win,x,y)
WinIconFrame *win;
int x,y;
{
    (WinFunc(win,core.newposfunc))(win,x-ICON_HORZBORDER,y-ICON_VERTBORDER);
}

/*
 * IconShow -- map an icon onto the screen, handling reparenting and
 * save-sets for icon panes.
 */
void
IconShow(cli, winIcon)
    Client *cli;
    WinIconFrame *winIcon;
{
    WinIconPane *pane = (WinIconPane *)winIcon->fcore.panewin;

    XReparentWindow(cli->dpy, pane->core.self, winIcon->core.self,
			pane->core.x, pane->core.y);
    XMapWindow(cli->dpy, pane->core.self);
    if (pane->iconClientWindow)
        XChangeSaveSet(cli->dpy, pane->core.self, SetModeInsert);
    XMapWindow(cli->dpy, winIcon->core.self);
}


/*
 * IconHide -- remove an icon from the screen, handling reparenting and
 * save-sets for icon panes.
 */
void
IconHide(cli, winIcon)
    Client *cli;
    WinIconFrame *winIcon;
{
    WinIconPane *pane = (WinIconPane *)winIcon->fcore.panewin;

    XUnmapWindow(cli->dpy, winIcon->core.self);
    XUnmapWindow(cli->dpy, pane->core.self);
    XReparentWindow(cli->dpy, pane->core.self, cli->scrInfo->rootid,
			winIcon->core.x + pane->core.x,
			winIcon->core.y + pane->core.y);
    if (pane->iconClientWindow)
        XChangeSaveSet(cli->dpy, pane->core.self, SetModeDelete);
}
