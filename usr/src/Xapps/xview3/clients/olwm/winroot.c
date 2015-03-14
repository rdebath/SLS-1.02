/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)winroot.c	26.48	91/09/20 SMI"

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
#include "group.h"
#include "events.h"

/***************************************************************************
* global data
***************************************************************************/

extern Time	SelectionTime;
extern Atom	AtomSunWMProtocols;
extern Atom	AtomSunWindowState;
extern Atom	AtomSunOLWinAttr5;
extern Bool	DoingWindowState;

/***************************************************************************
* private data
***************************************************************************/

static ClassRoot classRoot;

/***************************************************************************
* private functions
***************************************************************************/

static Window
findLeafWindow(dpy,win,srcx,srcy,dstx,dsty)
	Display	*dpy;
	Window	win;
	int	srcx,srcy;
	int	*dstx,*dsty;
{
	Window	childwin,dstwin,srcwin;

	srcwin = dstwin = win;
	while (1) {
		XTranslateCoordinates(dpy,srcwin,dstwin,srcx,srcy,
			dstx,dsty,&childwin);
		if (childwin == None)
			break;
		srcx = *dstx; srcy = *dsty;
		srcwin = dstwin;
		dstwin = childwin;
	}
	return dstwin;
}

static void
redistributeKeystroke(dpy,key,dstwin,dstx,dsty )
	Display *dpy;
	XKeyEvent *key;
	Window	dstwin;
	int	dstx,dsty;
{
	static Bool	pressreceived = False;
	static XKeyEvent pressevent;
	Window		childwin;

	if (key->type == KeyPress) {
		if (pressreceived == False) {
			pressevent = *key;
			pressevent.x = dstx;
			pressevent.y = dsty;
			pressevent.window = dstwin;
			pressevent.subwindow = None;
			XSendEvent( dpy, pressevent.window, True, 
				KeyPressMask, (XEvent *)&pressevent );
			pressreceived = True;
		}
    	} else {
		if (key->window != pressevent.window) {
			XTranslateCoordinates(dpy,key->window,pressevent.window,
				key->x,key->y,&dstx,&dsty,&childwin );
    	    		key->window = pressevent.window;
    	    		key->x = dstx;
    	    		key->y = dsty;
		}
    		key->subwindow = None;
    		XSendEvent(dpy,pressevent.window,True,
				KeyPressMask,(XEvent *)key);
		if (key->type == KeyRelease)
	    		pressreceived = False;
    	}
}


/*
 * HandleHelpKey - Figure out what window should really get the Help key.
 *	If it's not an olwm window or a pane window then send the key event
 *	onto that window.  If it's an olwm window then bring up the help
 *	info window with the window kind specific help.  If it is a
 *	WIN_ROOT window we need to use key->root since it is the root
 *	window that the pointer was on when the event happened while
 *	key->window is the window that the grab was made on.
 */
void
HandleHelpKey(dpy, pEvent)
	Display	*dpy;
	XEvent	*pEvent;
{
	static WinGeneric	*olwmWin = (WinGeneric *)0;
	XKeyEvent		*key = (XKeyEvent *)pEvent;
	static Window		dstwin = None;
	int			dstx,dsty;
    
	if (key->type == KeyPress) {

	    dstwin = findLeafWindow(dpy,key->window,
				    key->x_root,key->y_root,&dstx,&dsty);
	    olwmWin = WIGetInfo(dstwin);

	    /* send the help key to the client window */
	    if (olwmWin == NULL || olwmWin->core.kind == WIN_PANE) {
		redistributeKeystroke(dpy,key,dstwin,dstx,dsty);
		olwmWin = (WinGeneric *)0;

	    /* find out which root window is really happened on */
	    } else if (olwmWin->core.kind == WIN_ROOT) {
		if (dstwin != key->root)
	            olwmWin = WIGetInfo(key->root);
		WinShowHelp(dpy,olwmWin,key->x_root,key->y_root);

	    /* it belongs to a decoration window (frame/resize/whatever) */
	    } else {
		WinShowHelp(dpy,olwmWin,key->x_root,key->y_root);
	    }
    	} else { /* if KeyRelease */
	    if (olwmWin == NULL && dstwin != None)
		redistributeKeystroke(dpy,key,dstwin,0,0);
	    dstwin = None;
    	}
}

static Bool
matchKeystrokeToSpec(event,spec)
	XEvent *event;
	KeySpec *spec;
{
   	 return (spec->keycode == event->xkey.keycode &&
	        (spec->modmask == AnyModifier ||
	         spec->modmask == event->xkey.state));
}

/***************************************************************************
* event functions
***************************************************************************/

/* 
 * eventEnterNotify - the pointer has entered the root window
 */
static int
eventEnterNotify(dpy, pEvent, winInfo)
	Display		*dpy;
	XEvent		*pEvent;
	WinRoot		*winInfo;
{
	ColorWindowCrossing(dpy, pEvent, winInfo);

	if (GRV.FocusFollowsMouse)
	    NoFocusTakeFocus(dpy, pEvent->xcrossing.time,
			     winInfo->core.client->scrInfo);
}

/* 
 * eventConfigureRequest - a client wants to change configuration
 */
static int
eventConfigureRequest(dpy, pEvent, winInfo)
	Display		*dpy;
	XEvent		*pEvent;
	WinRoot		*winInfo;
{
	WinGeneric	*clientInfo;
#define ConfEvent	(pEvent->xconfigurerequest)

	if ((clientInfo = WIGetInfo(ConfEvent.window)) == NULL)
	{
		/* we don't know about this window, so let it go */
		ClientConfigure(NULL,NULL,pEvent);
	}
	else /* OBSOLETE: if (ConfEvent.value_mask & (CWX | CWY | CWWidth | CWHeight)) */
	{
		/* configure the window and its frame */
		ClientConfigure(clientInfo->core.client,clientInfo,pEvent);
	}
	/* REMIND doesn't handle stacking or border width yet */
}

/* 
 * eventMapRequest - a new client is mapping
 */
static int
eventMapRequest(dpy, pEvent, winInfo)
	Display		*dpy;
	XEvent		*pEvent;
	WinRoot		*winInfo;
{
#ifdef GPROF_HOOKS
	moncontrol(1);
#endif /* GPROF_HOOKS */
	StateNew(dpy,winInfo->core.self,pEvent->xmaprequest.window,False,NULL);
#ifdef GPROF_HOOKS
	moncontrol(0);
#endif /* GPROF_HOOKS */
}


static void
selectInBox(dpy, winInfo, boxX, boxY, boxW, boxH, timestamp, closure)
    Display	    *dpy;
    WinRoot	    *winInfo;
    int		    boxX, boxY;
    unsigned int    boxW, boxH;
    Time	    timestamp;
    void	    *closure;
{
    ClientInBoxClosure cibclosure;
    int		fuzz = GRV.SelectionFuzz;

    /* 
     * Apply selectFunc to all clients in the box.
     * Widen the box slightly to make selections easier.
     */
    cibclosure.dpy = dpy;
    cibclosure.screen = WinScreen(winInfo);
    cibclosure.func = (int (*)()) closure;
    cibclosure.bx = boxX - fuzz;
    cibclosure.by = boxY - fuzz;
    cibclosure.bw = boxW + 2 * fuzz;
    cibclosure.bh = boxH + 2 * fuzz;
    cibclosure.timestamp = timestamp;
    ListApply(ActiveClientList, ClientInBox, &cibclosure);
}


/* 
 * eventMotionNotify - the pointer is moving
 */
static int
eventMotionNotify(dpy, pEvent, winInfo)
	Display		*dpy;
	XEvent		*pEvent;
	WinRoot		*winInfo;
{
	short			boxX, boxY, boxW, boxH;
	int			(*selectFunc)();

	if (!pEvent->xmotion.same_screen)
	    return;

	/* If the user hasn't moved more than the threshold
	 * amount, break out of here.  REMIND  Also, if we get a 
	 * MotionNotify event with no buttons down, we ignore it.
	 * Ideally this shouldn't happen, but some areas of the code
	 * still leave the pointer grabbed even after all the buttons
	 * have gone up.
	 */
	if ((ABS(pEvent->xmotion.x - winInfo->buttonPressEvent.xbutton.x) < 
	     GRV.MoveThreshold) &&
	    (ABS(pEvent->xmotion.y - winInfo->buttonPressEvent.xbutton.y) < 
	     GRV.MoveThreshold))
	    return;
	if (pEvent->xmotion.state == 0)
	   return;
	
	/*
	 * On Select: Clear existing selected clients and add new ones
	 * On Adjust: Toggle selections on/off
	 */
	switch(winInfo->currentAction) {
	case ACTION_SELECT:	
		ClearSelections(dpy);
		selectFunc = AddSelection;
		break;
	case ACTION_ADJUST:	
		selectFunc = ToggleSelection;
		break;
	default:
		selectFunc = NULL;
		break;
	}

	if (selectFunc)
	    TraceRootBox(dpy, winInfo, &(winInfo->buttonPressEvent),
			     selectInBox, selectFunc);
}

/* 
 * eventButtonRelease - handle a click in the root.
 *
 * If the user clicks in the window, the focus is set to the no-focus window, 
 * and the PRIMARY selection is acquired and nulled.
 */
static int
eventButtonRelease(dpy, pEvent, winInfo)
	Display		*dpy;
	XEvent		*pEvent;
	WinRoot		*winInfo;
{
	if (!AllButtonsUp(pEvent))
	    return;

	/*
	 * This only happens if we did NOT get a motion notify
	 * after the last button press. 
	 */
	if (winInfo->currentAction == ACTION_SELECT) {
	    NoFocusTakeFocus(dpy,pEvent->xbutton.time,
			     winInfo->core.client->scrInfo);
	    ClearSelections(dpy);
	    XSetSelectionOwner(dpy, XA_PRIMARY, NoFocusWin,
			       pEvent->xbutton.time);
	    SelectionTime = pEvent->xbutton.time;
	}
	winInfo->currentAction = ACTION_NONE;
}
		
/* 
 * eventButtonPress - handle a button press.  If the WMGRAB modifier is down,
 * we've received this event by virtue of a passive, synchronous button grab
 * on the root.  We need to (1) propagate the event to the window underneath,
 * if it's a frame or an icon, (2) unfreeze the pointer either by regrabbing
 * or by issuing an AllowEvents request, and (3) ungrab the pointer if the
 * child's handler didn't issue a grab of its own.
 */
static int
eventButtonPress(dpy, pEvent, winInfo)
	Display		*dpy;
	XEvent		*pEvent;
	WinRoot		*winInfo;
{
	SemanticAction a;
	WinGeneric *child;

	if (pEvent->xbutton.state & ModMaskMap[MOD_WMGRAB]) {
	    /* redistribute to child */
	    if (pEvent->xbutton.subwindow != NULL &&
		(child = WIGetInfo(pEvent->xbutton.subwindow)) != NULL &&
		(child->core.kind == WIN_FRAME ||
		 child->core.kind == WIN_ICON) &&
		(GrabSuccess == XGrabPointer(dpy, child->core.self, False,
		    ButtonPressMask | ButtonMotionMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, None, None,
		    pEvent->xbutton.time)))
	    {
		PropagatePressEventToChild(dpy, pEvent, child);
		return;
	    }

	    /*
	     * If the window under the pointer isn't a frame or icon, or if we 
	     * failed to grab the pointer, simply unfreeze the pointer and try 
	     * to process the event normally.
	     */
	    XAllowEvents(dpy, AsyncBoth, pEvent->xbutton.time);
	}

	a = ResolveMouseBinding(dpy, pEvent, ModMaskMap[MOD_CONSTRAIN]);

	winInfo->buttonPressEvent = *pEvent;

	switch (a) {
	case ACTION_MENU:
	    	RootMenuShow(dpy, winInfo, pEvent);
	    	/* FALL THRU */
	case ACTION_SELECT:
	case ACTION_ADJUST:
	    	winInfo->currentAction = a;
	    	break;
	}
}


/* 
 * eventKeyPressRelease - a keystroke has happened in the root window
 */
static int
eventKeyPressRelease(dpy, pEvent, winInfo)
	Display		*dpy;
	XEvent		*pEvent;
	WinRoot		*winInfo;
{

	Window		child;
	WinGeneric	*childInfo;
	Client		*childClient;
	Bool		samescreen;
	int		destX, destY;
	extern Bool	ExecuteKeyboardFunction();
	Bool		isbound;

	isbound = ExecuteKeyboardFunction(dpy, pEvent);

	if (!isbound && pEvent->type == KeyPress)
	    KeyBeep(dpy,pEvent);

#ifdef notdef
	samescreen = XTranslateCoordinates(dpy,
				pEvent->xkey.root, pEvent->xkey.root,
				pEvent->xkey.x_root, pEvent->xkey.y_root, 
				&destX, &destY, &child );

	if (!samescreen)
		return;

	if (pEvent->type == KeyPress && child != None &&
	    matchKeystrokeToSpec(pEvent, &(GRV.FrontKey))) {

		XWindowChanges xwc;
		xwc.stack_mode = Opposite;
		XConfigureWindow( dpy, child, CWStackMode, &xwc );

	} else if (matchKeystrokeToSpec(pEvent, &(GRV.HelpKey))) {

		handleHelpKey(dpy,pEvent);

	} else if (pEvent->type == KeyPress && child != None &&
		   matchKeystrokeToSpec(pEvent, &(GRV.OpenKey))) {

		childInfo = WIGetInfo(child);
		if ( childInfo ) {
			childClient = childInfo->core.client;
			if (childClient != NULL) {
				ClientOpenCloseToggle(childClient);
			}
		}
	} else if (pEvent->type == KeyPress &&
		   matchKeystrokeToSpec(pEvent, &(GRV.ColorLockKey))) {

		InstallPointerColormap(dpy, pEvent->xkey.root, 
			pEvent->xkey.x_root, pEvent->xkey.y_root, True);

	} else if (pEvent->type == KeyPress &&
		   matchKeystrokeToSpec(pEvent, &(GRV.ColorUnlockKey))) {

		UnlockColormap(dpy,winInfo);
	}
#endif /* notdef */
}

/* 
 * eventPropertyNotify - a root property has changed
 */
static int
eventPropertyNotify(dpy, pEvent, winInfo)
	Display		*dpy;
	XEvent		*pEvent;
	WinRoot		*winInfo;
{
	unsigned long	nitems, remain;
	char 		*resourceString;

	/* make sure that the property was the one we care about and
	 * changed (as opposed to deleted)
	 */
	if ( (pEvent->xproperty.atom != XA_RESOURCE_MANAGER) 
	     || (pEvent->xproperty.state != PropertyNewValue) )
		return;

	resourceString = GetWindowProperty(dpy, WinRootID(winInfo),
		XA_RESOURCE_MANAGER, 0L, 100000000L, 
		/* REMIND use ENTIRE_CONTENTS */
		XA_STRING, 0, &nitems, &remain);
	if (resourceString != NULL) {
		UpdateGlobals(dpy, resourceString);
		XFree( resourceString );
        }
}

/* 
 * eventUnmapNotify - an unreparented pane is going away
 */
static int
eventUnmapNotify(dpy, pEvent, winInfo)
	Display		*dpy;
	XEvent		*pEvent;
	WinRoot		*winInfo;
{
	WinGeneric *wg;

	wg = WIGetInfo(pEvent->xunmap.window);
	if (wg != NULL) {
		StateWithdrawn(wg->core.client);
	}
}

/*
 * destroyRoot -- destroy the root window resources and free any allocated
 *	data.
 */
static int
destroyRoot(dpy, winInfo)
	Display		*dpy;
	WinRoot 	*winInfo;
{
	/* delete the _SUN_WM_PROTOCOLS property */
	XDeleteProperty(dpy,winInfo->core.self,AtomSunWMProtocols);

	/* delete the WM_ICON_SIZE property */
	XDeleteProperty(dpy,winInfo->core.self,XA_WM_ICON_SIZE);

	/* free our data and throw away window */
	WIUninstallInfo(winInfo->core.self);
	MemFree(winInfo);
}

/*
 * writeProtocols - write the _SUN_WM_PROTOCOLS property on the root win,
 *		    which advertises the capabilities of the window manager.
 */
static void
writeProtocols(dpy,rootwin)
	Display		*dpy;
	Window		rootwin;
{
	unsigned int	data[10];
	int		nitems = 0;
	
	/* conditionally support the _SUN_WINDOW_STATE protocol */
	if (DoingWindowState)
		data[nitems++] = AtomSunWindowState;

	/* support 5-word-long _OL_WIN_ATTR property */
	data[nitems++] = AtomSunOLWinAttr5;

	if (nitems == 0)
		return;

	XChangeProperty(dpy,rootwin,AtomSunWMProtocols,XA_ATOM,32,
			PropModeReplace,(unsigned char *)data,nitems);
}

/*
 * writeIconSize - write the WM_ICON_SIZE property on the root window.
 */
static void
writeIconSize(dpy,rootwin)
	Display		*dpy;
	Window		rootwin;
{
	XIconSize	iconSize;

	iconSize.min_width = ICON_MIN_WIDTH;
	iconSize.min_height = ICON_MIN_HEIGHT;
	iconSize.max_width = ICON_MAX_WIDTH;
	iconSize.max_height = ICON_MAX_HEIGHT;
	iconSize.width_inc = ICON_WIDTH_INC;
	iconSize.height_inc = ICON_HEIGHT_INC;

	XSetIconSizes(dpy,rootwin,&iconSize,1);
}

/***************************************************************************
* global functions
***************************************************************************/

/*
 * MakeRoot  -- create the root window. Return a WinGeneric structure.
 */
WinRoot *
MakeRoot(dpy, cli)
	Display	*dpy;
	Client	*cli;
{
	XWindowAttributes attr;
	WinRoot *w;
	Window win;

	win = cli->scrInfo->rootid;

	/*
 	 * Tell the server we need to get mapping requests.
	 * ErrorSensitive will force an exit if this fails
	 * (ie another window manager is running).
	 *
	 * REMIND: instead of exiting, MakeRoot should probably just
	 * return NULL, and callers to MakeRoot should check the return
	 * value.
	 */
	ErrorSensitive(
		gettext("Perhaps there is another window manager running?"));
	XSelectInput(dpy,win,
		KeyPressMask | SubstructureRedirectMask |
		ButtonPressMask | ButtonReleaseMask | ButtonMotionMask |
		EnterWindowMask | PropertyChangeMask | OwnerGrabButtonMask);
	XSync(dpy, False);
	ErrorInsensitive(dpy);

	if (XGetWindowAttributes(dpy, win, &attr) == 0) {
	    ErrorGeneral(gettext("Could not get attributes of root window"));
	    /*NOTREACHED*/
	}

	/* mark the client as olwm owned */
	cli->flags = CLOlwmOwned;

	/* create the associated structure */
	w = MemNew(WinRoot);
	w->core.self = win;
	w->class = &classRoot;
	w->core.kind = WIN_ROOT;
	w->core.parent = NULL;
	w->core.children = NULL;
	w->core.client = cli;
	w->core.x = 0;
	w->core.y = 0;
	w->core.width = attr.width;
	w->core.height = attr.height;
	w->core.dirtyconfig = False;
	w->core.colormap = cli->scrInfo->colormap;
	w->core.exposures = NULL;
	w->core.helpstring = "olwm:Workspace";
	w->currentAction = ACTION_NONE;

	/* Write properties on the root window */
	writeProtocols(dpy,win);
	writeIconSize(dpy,win);

	/* register the window */
	WIInstallInfo((WinGeneric *)w);

	return w;
}

/*
 * RootInit - init the WinRoot class function vector
 */
void
RootInit(dpy)
Display *dpy;
{
	classRoot.core.kind = WIN_ROOT;
	classRoot.core.xevents[ConfigureRequest] = eventConfigureRequest;
	classRoot.core.xevents[EnterNotify] = eventEnterNotify;
	classRoot.core.xevents[MapRequest] = eventMapRequest;
	classRoot.core.xevents[MotionNotify] = eventMotionNotify;
	classRoot.core.xevents[ButtonRelease] = eventButtonRelease;
	classRoot.core.xevents[ButtonPress] = eventButtonPress;
	classRoot.core.xevents[KeyPress] = eventKeyPressRelease;
	classRoot.core.xevents[KeyRelease] = eventKeyPressRelease;
	classRoot.core.xevents[PropertyNotify] = eventPropertyNotify;
	classRoot.core.xevents[UnmapNotify] = eventUnmapNotify;
	classRoot.core.focusfunc = NULL;
	classRoot.core.drawfunc = NULL;
	classRoot.core.destroyfunc = destroyRoot;
	classRoot.core.selectfunc = NULL;
	classRoot.core.newconfigfunc = NULL;
	classRoot.core.newposfunc = NULL;
	classRoot.core.setconfigfunc = NULL;
	classRoot.core.createcallback = NULL;
	classRoot.core.heightfunc = NULL;
	classRoot.core.widthfunc = NULL;
}
