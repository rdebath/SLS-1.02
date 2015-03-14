/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)winbutton.c	26.22	91/09/14 SMI"

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
#include "menu.h"
#include "events.h"

extern void FrameAllowEvents();
extern Bool DoDefaultMenuAction();

/***************************************************************************
* private data
***************************************************************************/

#define in_windowmark(win,x,y) \
	( (x) >= 0 && (y) >= 0 && \
	  (x) <= Abbrev_MenuButton_Width(WinGI((win),NORMAL_GINFO)) && \
	  (y) <= Abbrev_MenuButton_Height(WinGI((win),NORMAL_GINFO)) \
	)
static Bool buttonActive = False;
static ClassButton classButton;
static SemanticAction currentAction = ACTION_NONE;

/***************************************************************************
* private functions
***************************************************************************/

static int drawButton();

static void 
doUnhilite(act, mode, winInfo)
    int act;
    MenuTrackMode mode;
    WinButton *winInfo;
{
    Graphics_info	*gisNormal = WinGI(winInfo, NORMAL_GINFO);
    long flags;

    if (act != SYNC_CHANGECLICK)
	flags = OLGX_NORMAL | OLGX_ERASE;
    else if (mode == MODE_CLICK)
	flags = OLGX_BUSY | OLGX_ERASE | OLGX_NORMAL;
    else {
	/* don't do this; it's unsettling to press it in when you drag again */
	return;
    }

    olgx_draw_abbrev_button(gisNormal, winInfo->core.self, 0, 0, flags);
}

/* 
 * eventButtonPress - handle button press events on the close button window.  
 */
static int
eventButtonPress(dpy, event, winInfo)
Display	*dpy;
XEvent	*event;
WinButton	*winInfo;
{
	Client *cli = winInfo->core.client;
	WinPaneFrame *winFrame = cli->framewin;
	SemanticAction a;
	Graphics_info	*gisNormal = WinGI(winInfo,NORMAL_GINFO);

	a = ResolveMouseBinding(dpy, event, ModMaskMap[MOD_CONSTRAIN]);

        switch (a) {
	case ACTION_SELECT:
	    olgx_draw_abbrev_button(gisNormal, winInfo->core.self, 
				    0, 0, OLGX_INVOKED);
	    /*
	     * REMIND: bad style.  This is grabbing the pointer after
	     * the fact.  We should set up a passive grab instead.
	     */
	    XGrabPointer(dpy, winInfo->core.self, False,
		 (ButtonReleaseMask | ButtonPressMask | PointerMotionMask),
		 GrabModeAsync, GrabModeAsync, None,
		 GRV.MovePointer, CurrentTime);
	    buttonActive = True;
	    currentAction = a;
	    break;

        case ACTION_MENU:
	    olgx_draw_abbrev_button(gisNormal, winInfo->core.self, 
				    0, 0, OLGX_INVOKED);
	    if (winFrame->core.client->wmDecors->menu_type != MENU_NONE)
		ShowStandardMenuSync(winFrame, event, True, doUnhilite, winInfo);
	    return;

	default:
	    FrameAllowEvents(cli, event->xbutton.time);
	    return;
        }
}

/* 
 * eventButtonRelease - handle button release events on the close button window.  
 */
static int
eventButtonRelease(dpy, event, winInfo)
Display	*dpy;
XEvent	*event;
WinButton	*winInfo;
{
	Client *cli = winInfo->core.client;
	int x,y;

	FrameAllowEvents(cli, event->xbutton.time);

	if (!AllButtonsUp(event))
	    return;

        XUngrabPointer(dpy, CurrentTime);

        x = event->xbutton.x;
        y = event->xbutton.y;

	if (buttonActive) {
	    drawButton(dpy, winInfo);
	    buttonActive = False;
	}

        if (!in_windowmark(winInfo,x,y) || currentAction != ACTION_SELECT) {
	    return;
        }

	if (!DoDefaultMenuAction(winInfo)) {
		StateNormIcon(cli);
	}

	currentAction = ACTION_NONE;
}

/* 
 * eventMotionNotify - handle motion notify events on the close button window.  
 */
static int
eventMotionNotify(dpy, event, winInfo)
Display	*dpy;
XEvent	*event;
WinButton	*winInfo;
{
	int 	x,y;
	Graphics_info	*gisNormal = WinGI(winInfo,NORMAL_GINFO);

	if (!event->xmotion.same_screen || currentAction != ACTION_SELECT)
		return;

        x = event->xmotion.x;
        y = event->xmotion.y;
        if ( buttonActive && !in_windowmark(winInfo,x,y) ) {
	    drawButton(dpy, winInfo);
            buttonActive = False;
        } else if ( !buttonActive && in_windowmark(winInfo,x,y) ) {
            olgx_draw_abbrev_button(gisNormal, winInfo->core.self,
	    	0, 0, OLGX_INVOKED);
            buttonActive = True;
        }
}

/*
 * drawButton -- draw the window button
 */
/*ARGSUSED*/
static int
drawButton(dpy, winInfo)
Display	*dpy;
WinButton *winInfo;
{
    Client		*cli = winInfo->core.client;
    GC 			windowGC = WinGC(winInfo,WINDOW_GC);
    XGCValues		gcv;
    Graphics_info	*gisNormal = WinGI(winInfo,NORMAL_GINFO);
    int			focusLines = (GRV.FocusFollowsMouse ? 1 : 0) ^
				     (GRV.InvertFocusHighlighting ? 1 : 0);

    /*
     * Erase the background first.  Unfortunately, we can't depend on
     * OLGX_ERASE to do the right thing, because it (a) erases only in BG1,
     * and (b) erases only in 2D mode.  We need to erase a background color
     * that depends on the state of the frame.  If we're in click-focus and we
     * have the focus, draw in BG2; otherwise, draw in BG1.
     */

    /* Temporarily set background to BG2 if click-to-type */
    if (!focusLines && winInfo->core.client->isFocus && Win3D(winInfo)) {
	XGetGCValues(dpy,windowGC,GCBackground,&gcv);
	XSetBackground(dpy,windowGC,cli->scrInfo->colorInfo.bg2Color);
    }

    XFillRectangle(dpy, winInfo->core.self, windowGC, 0, 0,
		   Abbrev_MenuButton_Width(gisNormal),
		   Abbrev_MenuButton_Height(gisNormal));

    /* Restore background back to BG1 */
    if (!focusLines && winInfo->core.client->isFocus && Win3D(winInfo)) {
	XSetBackground(dpy,windowGC,gcv.background);
    }

    olgx_draw_abbrev_button(gisNormal, winInfo->core.self,
			    0, 0, OLGX_NORMAL | OLGX_ERASE);

    /*
     * REMIND: hack for working around OLGX deficiency.  OLGX erases the
     * "ears" at each corner of the window button to the background color.  
     * They should really be filled in with the foreground color.
     */
    if (!focusLines && winInfo->core.client->isFocus && !Win3D(winInfo)) {
	XDrawRectangle(dpy, winInfo->core.self, WinGC(winInfo,FOREGROUND_GC),
		       0, 0,
		       Abbrev_MenuButton_Width(gisNormal)-1,
		       Abbrev_MenuButton_Height(gisNormal)-1);
	XDrawPoint(dpy, winInfo->core.self, WinGC(winInfo,FOREGROUND_GC),
		   Abbrev_MenuButton_Width(gisNormal)-1,
		   Abbrev_MenuButton_Height(gisNormal)-1);
    }

}


/*
 * DestroyButton -- destroy the close button window resources and free any allocated
 *	data.
 */
static int
destroyButton(dpy, winInfo)
Display	*dpy;
WinButton *winInfo;
{
	/* free our data and throw away window */
	XDestroyWindow(dpy, winInfo->core.self);
	WIUninstallInfo(winInfo->core.self);
	MemFree(winInfo);
}

/* 
 * focusButton - the focus or selection state has changed
 */
static int
focusButton(dpy, winInfo, selected)
Display *dpy;
WinButton *winInfo;
Bool selected;
{
        (WinFunc(winInfo,core.drawfunc))(dpy, winInfo);
}

/*
 * heightfuncButton - recomputes the height of the close button window
 */
static int 
heightfuncButton(win, pxcre)
WinButton *win;
XConfigureRequestEvent *pxcre;
{
	return Abbrev_MenuButton_Width(WinGI(win,NORMAL_GINFO));
}

/*
 * widthfuncButton - recomputes the width of the close button window
 */
static int 
widthfuncButton(win, pxcre)
WinButton *win;
XConfigureRequestEvent *pxcre;
{
	return Abbrev_MenuButton_Height(WinGI(win,NORMAL_GINFO));
}


/***************************************************************************
* global functions
***************************************************************************/

/*
 * MakeButton  -- create the close button window. Return a WinGeneric structure.
 */
WinButton *
MakeButton(dpy, par, x, y)
Display	*dpy;
WinGeneric *par;
int x,y;
{
	WinButton *w;
	Window win;
        unsigned long valuemask;
        XSetWindowAttributes attributes;
	Graphics_info	*gisNormal = WinGI(par,NORMAL_GINFO);

        attributes.event_mask =
	    ButtonReleaseMask | ButtonPressMask | ExposureMask;
	attributes.cursor = GRV.IconPointer;
        valuemask = CWEventMask | CWCursor;

        win = XCreateWindow(dpy, par->core.self,
                        x, y,
			Abbrev_MenuButton_Width(gisNormal), 
			Abbrev_MenuButton_Height(gisNormal),
                        0,
                        CopyFromParent,
                        CopyFromParent,
                        CopyFromParent,
                        valuemask,
                        &attributes);

	/* create the associated structure */
	w = MemNew(WinButton);
	w->core.self = win;
	w->class = &classButton;
	w->core.kind = WIN_WINBUTTON;
	WinAddChild(par,w);
	w->core.children = NULL;
	w->core.client = par->core.client;
	w->core.x = x;	
	w->core.y = y;
	w->core.width = Abbrev_MenuButton_Width(gisNormal);
	w->core.height = Abbrev_MenuButton_Height(gisNormal);
	w->core.dirtyconfig = 0;
	w->core.exposures = NULL;
	w->core.helpstring = "olwm:CloseButton";

	/* register the window */
	WIInstallInfo(w);

        XMapWindow(dpy, win);

	return w;
}

void
ButtonInit(dpy)
Display *dpy;
{
        classButton.core.kind = WIN_WINBUTTON;
        classButton.core.xevents[ButtonPress] = eventButtonPress;
        classButton.core.xevents[ButtonRelease] = eventButtonRelease;
        classButton.core.xevents[MotionNotify] = eventMotionNotify;
        classButton.core.xevents[Expose] = WinEventExpose;
        classButton.core.focusfunc = focusButton;
        classButton.core.drawfunc = drawButton;
        classButton.core.destroyfunc = destroyButton;
        classButton.core.selectfunc = NULL;
        classButton.core.newconfigfunc = WinNewConfigFunc;
        classButton.core.newposfunc = WinNewPosFunc;
        classButton.core.setconfigfunc = WinSetConfigFunc;
        classButton.core.createcallback = NULL;
        classButton.core.heightfunc = heightfuncButton;
        classButton.core.widthfunc = widthfuncButton;
}

