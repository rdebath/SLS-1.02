/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)winpush.c	26.19	91/09/14 SMI"

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

extern Atom AtomDeleteWindow;
extern Atom AtomPushpinState;
extern void FrameAllowEvents();

/***************************************************************************
* private data
***************************************************************************/

static ClassPushPin classPushPin;
static Bool     pushpinStateAfterPress;  /* State of the pushpin
					  * after the user pressed
				   	  * the mouse button. */
static SemanticAction currentAction = ACTION_NONE;
void PushPinChangePinState();

/***************************************************************************
* private functions
***************************************************************************/

/* locallyChangePushPinState -- temporarily change the pushpin state,
 * 	while tracking a button press over the pin.  The permanent change
 *	will be made later.
 */
static void
locallyChangePushPinState(dpy,winInfo,newState)
Display *dpy;
WinPushPin *winInfo;
Bool newState;
{
        if (winInfo->pushpinin != newState)
        {
                winInfo->pushpinin = newState;
                (WinFunc(winInfo,core.drawfunc))(dpy, winInfo);
        }
}


/* 
 * eventButtonPress - handle button press events on the pushpin window 
 */
static int
eventButtonPress(dpy, event, winInfo)
Display	*dpy;
XEvent	*event;
WinPushPin	*winInfo;
{
	SemanticAction a = ResolveMouseBinding(dpy, event, 0L);
	FrameAllowEvents(winInfo->core.client, event->xbutton.time);
	switch (a) {
	case ACTION_SELECT:
	case ACTION_ADJUST:
            locallyChangePushPinState(dpy, winInfo, !(winInfo->pushpinin));
            pushpinStateAfterPress = winInfo->pushpinin;
	    currentAction = a;
	    break;
	}
}

/* 
 * eventButtonRelease - handle button release events on the pushpin window 
 */
static int
eventButtonRelease(dpy, event, winInfo)
Display	*dpy;
XEvent	*event;
WinPushPin	*winInfo;
{
	FrameAllowEvents(winInfo->core.client, event->xbutton.time);
	if (!AllButtonsUp(event))
	    return;

	/*
	 * If pushpinStateAfterPress equals the current pin state, we know 
	 * that the user really wants to change the pin state.  If they are 
	 * not equal, the user has released the button outside the pin, so 
	 * there should be no change of state.
	 */
	if (pushpinStateAfterPress == winInfo->pushpinin) {
	    PushPinChangePinState(dpy, winInfo,
			       currentAction == ACTION_SELECT);
	}
	currentAction = ACTION_NONE;
}

/* 
 * eventMotionNotify - handle pointer moves
 */
static int
eventMotionNotify(dpy, event, winInfo)
Display	*dpy;
XEvent	*event;
WinPushPin	*winInfo;
{
	Bool 	fInWindow;
	Graphics_info	*gisNormal = WinGI(winInfo,NORMAL_GINFO);

	if (!event->xmotion.same_screen)
	    return;
	if (currentAction != ACTION_SELECT && currentAction != ACTION_ADJUST)
	    return;

        /* When the user moves the cursor off the pushpin
         * while s/he has the button down we should pull
         * the button out.  If the user moves back onto the
         * pushpin put the pin back in.  So,
         *
         * if (cursor is off the pushpin) and (pin is in)
         *                  OR
         *    (cursor is on the pushpin) and (pin is out)
         * then
         *      change the state of the pushpin.
         */
        fInWindow = !((event->xmotion.x < 0) ||
             (event->xmotion.y < 0) ||
             (event->xmotion.x >= PushPinOut_Width(gisNormal)) ||
             (event->xmotion.y >= PushPinOut_Height(gisNormal)));
        locallyChangePushPinState(dpy, winInfo,
                fInWindow?pushpinStateAfterPress:!pushpinStateAfterPress);
}


/*
 * drawPushPin -- draw the pushpin window
 */
/*ARGSUSED*/
static int
drawPushPin(dpy, winInfo)
Display	*dpy;
WinPushPin *winInfo;
{
	Client *cli = winInfo->core.client;
	Graphics_info *gis = WinGI(winInfo,NORMAL_GINFO);
	int focusLines = (GRV.FocusFollowsMouse ? 1 : 0) ^
			 (GRV.InvertFocusHighlighting ? 1 : 0);

        /* If the titlebar is in reverse video we need to
         * draw the pushpin in reverse video also.
         */
	if (Win3D(winInfo)) {
		/*
		 * REMIND
		 * We need to erase the background here to BG2.  We can't use
		 * OLGX_ERASE because olgx erases only in BG1.  So, we draw an 
		 * filled, invoked box that is just larger than the pin 
		 * window, so that the border doesn't show.
		 */
		
		olgx_draw_box(gis, winInfo->core.self, -1, -1,
			      winInfo->core.width+2,
			      winInfo->core.height+2,
                              ((cli->isFocus) && !focusLines) ?
			          OLGX_INVOKED : OLGX_NORMAL, 
			      True);
        } else {
		GC gc;

		if (cli->isFocus && !focusLines) {
		    gc = WinGC(winInfo, FOREGROUND_GC);
		    gis = WinGI(winInfo, REVPIN_GINFO);
		} else {
		    gc = WinGC(winInfo, WINDOW_GC);
		}
		XFillRectangle(dpy, winInfo->core.self, gc, 0, 0,
			winInfo->core.width, winInfo->core.height);
        }
	if (winInfo->core.client->isBusy)
	{
	    XFillRectangle(dpy, winInfo->core.self, WinGC(winInfo,BUSY_GC),
		0, 0, winInfo->core.width, winInfo->core.height);
	}
	olgx_draw_pushpin(gis, winInfo->core.self, 0, 0,
              	  ((winInfo->pushpinin) ? OLGX_PUSHPIN_IN : OLGX_PUSHPIN_OUT));
}


/*
 * DestroyPushPin -- destroy the pushpin window resources and free any allocated
 *	data.
 */
static int
destroyPushPin(dpy, winInfo)
Display	*dpy;
WinPushPin *winInfo;
{
	/* free our data and throw away window */
	XDestroyWindow(dpy, winInfo->core.self);
	WIUninstallInfo(winInfo->core.self);
	MemFree(winInfo);
}

/* 
 * focusselectPushPin - the focus or selection state has changed
 */
static int
focusselectPushPin(dpy, winInfo, selected)
Display *dpy;
WinPushPin *winInfo;
Bool selected;
{
        (WinFunc(winInfo,core.drawfunc))(dpy, winInfo);
}

/*
 * heightfuncPushPin - recomputes the correct height of the window
 */
static int 
heightfuncPushPin(win, pxcre)
WinPushPin *win;
XConfigureRequestEvent *pxcre;
{
	return PushPinOut_Width(WinGI(win,NORMAL_GINFO));
}

/*
 * widthfuncPushPin - recomputes the correct width of the window
 */
static int 
widthfuncPushPin(win, pxcre)
WinPushPin *win;
XConfigureRequestEvent *pxcre;
{
	return PushPinOut_Height(WinGI(win,NORMAL_GINFO));
}

/***************************************************************************
* global functions
***************************************************************************/

/*
 * MakePushPin  -- create the pushpin window. Return a WinGeneric structure.
 */
WinPushPin *
MakePushPin(dpy, par, pane, x, y)
Display	*dpy;
WinGeneric *par;
Window pane;
int x,y;
{
	WinPushPin *w;
	Window win;
        unsigned long valuemask;
        XSetWindowAttributes attributes;
	Graphics_info	*gisNormal = WinGI(par,NORMAL_GINFO);

        attributes.event_mask = ButtonMotionMask | ButtonReleaseMask | 
		ButtonPressMask | ExposureMask;
        attributes.win_gravity = NorthWestGravity;
        valuemask = CWEventMask | CWWinGravity;

        win = XCreateWindow(dpy, par->core.self,
                        x, y,
			PushPinOut_Width(gisNormal),
			PushPinOut_Height(gisNormal),
                        0,
                        CopyFromParent,
                        CopyFromParent,
                        CopyFromParent,
                        valuemask,
                        &attributes);

	/* create the associated structure */
	w = MemNew(WinPushPin);
	w->core.self = win;
	w->class = &classPushPin;
	w->core.kind = WIN_PUSHPIN;
	WinAddChild(par,w);
	w->core.children = NULL;
	w->core.client = par->core.client;
	w->core.x = x;	
	w->core.y = y;
	w->core.width = PushPinOut_Width(gisNormal);
	w->core.height = PushPinOut_Height(gisNormal);
	w->core.dirtyconfig = CWX | CWY | CWWidth | CWHeight;
	w->core.exposures = NULL;
	w->core.helpstring = "olwm:PushPin";

        /* Determine initial state of push pin. */
        w->pushpinin =  
		(par->core.client->wmDecors->pushpin_initial_state == PIN_IN);

        /* Register the push-pin state. */
        XChangeProperty(dpy, pane,
                        AtomPushpinState,
                        XA_INTEGER, 32,
                        PropModeReplace,
                        (unsigned char *)&(w->pushpinin), 1);

	/* register the window */
	WIInstallInfo(w);

        XMapRaised(dpy, win);

	return w;
}

void
PushPinInit(dpy)
Display *dpy;
{
	classPushPin.core.kind = WIN_PUSHPIN;
	classPushPin.core.xevents[Expose] = WinEventExpose;
	classPushPin.core.xevents[ButtonPress] = eventButtonPress;
	classPushPin.core.xevents[ButtonRelease] = eventButtonRelease;
	classPushPin.core.xevents[MotionNotify] = eventMotionNotify;
	classPushPin.core.focusfunc = focusselectPushPin;
	classPushPin.core.drawfunc = drawPushPin;
	classPushPin.core.destroyfunc = destroyPushPin;
	classPushPin.core.selectfunc = focusselectPushPin;
	classPushPin.core.newconfigfunc = WinNewConfigFunc;
	classPushPin.core.newposfunc = WinNewPosFunc;
	classPushPin.core.setconfigfunc = WinSetConfigFunc;
	classPushPin.core.createcallback = NULL;
	classPushPin.core.heightfunc = heightfuncPushPin;
	classPushPin.core.widthfunc = widthfuncPushPin;
}


/*
 * Permanently change the push pin state.
 */
void
PushPinChangePinState(dpy, winInfo, sendDelete)
    Display *dpy;
    WinPushPin *winInfo;
    Bool sendDelete;
{
	Client		*cli = winInfo->core.client;

        (WinFunc(winInfo,core.drawfunc))(dpy, winInfo);

        /* Tell the client that the state of its push-pin has changed. */
        XChangeProperty(dpy, PANEWINOFCLIENT(cli),
                        AtomPushpinState,
                        XA_INTEGER, 32,
                        PropModeReplace,
                        (unsigned char *)&(winInfo->pushpinin), 1);

        if (!winInfo->pushpinin && sendDelete)
	    ClientKill(winInfo->core.client,False);
}  

/*
 * Sets the pin state to a new state (if different that current state)
 */
void
PushPinSetPinState(dpy,winInfo,newState,sendDelete)
	Display		*dpy;
	WinPushPin	*winInfo;
	int		newState;
	Bool		sendDelete;
{
	if (newState == winInfo->pushpinin)
		return;

	winInfo->pushpinin = newState;
	PushPinChangePinState(dpy,winInfo,sendDelete);
}

/*
 * Toggles the pin state
 */
void
PushPinTogglePinState(dpy,winInfo,sendDelete)
	Display		*dpy;
	WinPushPin	*winInfo;
	Bool		sendDelete;
{
	winInfo->pushpinin = !winInfo->pushpinin;
	PushPinChangePinState(dpy,winInfo,sendDelete);
}
