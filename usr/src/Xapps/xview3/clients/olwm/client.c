/* 
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

/* client.c - functions relating to clients as a whole 
 */

#ident	"@(#)client.c	26.42	91/09/14 SMI"

#include <errno.h>
#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>
#include <olgx/olgx.h>

#include "i18n.h"
#include "ollocale.h"
#include "events.h"
#include "mem.h"
#include "olwm.h"
#include "win.h"
#include "group.h"
#include "globals.h"

/***************************************************************************
* global data
***************************************************************************/

/*
 * List of currently active clients.  All frames and icons and the no-focus 
 * window are reachable by traversing this list.
 */
List *ActiveClientList;

/*
 * Pointer to the "current" client.  This is usually the client whose window
 * has the input focus, but not always.  The exceptions include icons and
 * pinned menus, where the icon or menu window may be the "current" client but
 * where the focus really is with the no-focus window.
 */

Client *CurrentClient = NULL;

/*
 * DoingWindowState indicates whether olwm is doing window state.
 * This is set in InitClientState() and used in MakeRoot().
 */
Bool	DoingWindowState = False;

extern Atom AtomColorMapWindows;
extern Atom AtomDecorDel;
extern Atom AtomDecorAdd;
extern Atom AtomDeleteWindow;
extern Atom AtomProtocols;
extern Atom AtomLeftFooter;
extern Atom AtomRightFooter;
extern Atom AtomShowProperties;
extern Atom AtomTakeFocus;
extern Atom AtomWindowBusy;
extern Atom AtomWinAttr;
extern Atom AtomWMState;
extern Atom AtomWMName;
extern Atom AtomWMIconName;
extern Atom AtomWMHints;
extern Atom AtomWMNormalHints;
extern Atom AtomSunLedMap;
extern Atom AtomSunWindowState;
extern Atom AtomSunDragDropInterest;

extern void RecursiveRefresh();
extern void PushPinChangeState();
extern void FrameUpdateHeader();
extern void FrameUpdateFooter();
extern void IconUpdateName();
extern void ColorUpdateColorMapWindows();
extern void StateUpdateWinAttr();
extern void StateUpdateDecorAdd();
extern void StateUpdateDecorDel();
extern void StateUpdateWMNormalHints();
extern void StateUpdateWMHints();
extern void StateUpdateWMProtocols();

/***************************************************************************
* private data
***************************************************************************/

typedef struct _ledstate {
	int	led;
	int	led_mode;
} LedState;

static	LedState	composeLedState;

static	Client		*lastCurrentClient = NULL;

#define	LED_MAP_LENGTH		(33L)

/***************************************************************************
* private functions
***************************************************************************/

static void clientSetBusy();

/*
 * setComposeLed - sets the compose led to the new mode iff different than
 *		   the current mode.
 */
static void
setComposeLed(dpy,mode)
	Display		*dpy;
	int		mode;
{
	XKeyboardControl	kbdValue;

	if (mode == composeLedState.led_mode)
		return;

	composeLedState.led_mode = mode;
	kbdValue.led = composeLedState.led;
	kbdValue.led_mode = composeLedState.led_mode;

	XChangeKeyboardControl(dpy,KBLed|KBLedMode,&kbdValue);
}

/***************************************************************************
* global functions
***************************************************************************/

/*
 * InitClientState - init kbd led state for client use
 */
InitClientState(dpy)
Display	*dpy;
{
	int	*ledMap;
	int	i,numLeds,led;
	unsigned int nitems,nremain;
	XKeyboardControl  kbdvalue;

	DoingWindowState = False;

	/* 
	 * Get the _SUN_LED_MAP property from the default root window
	 */
	ledMap = GetWindowProperty(dpy,DefaultRootWindow(dpy),AtomSunLedMap,
			0L,LED_MAP_LENGTH,XA_INTEGER,32,&nitems,&nremain);

	if (ledMap == NULL || nitems != LED_MAP_LENGTH || nremain != 0) {
		return;
	}
	numLeds = ledMap[0];	/* first entry is the number of leds */

	/*
	 * Find out which led number is marked Compose
	 */
	for (i = 1; i <= numLeds; i++) {
		if (ledMap[i] == XK_Multi_key) {
			composeLedState.led = i;
			composeLedState.led_mode = -1; /* for first time */
			DoingWindowState = True;
		}
	}

	/* 
	 * Turn off the compose led 
	 */
	if (DoingWindowState)
		setComposeLed(dpy,LedModeOff);

	XFree((char *)ledMap);
}

/*
 * ClientDefaultWindowState - applies the default window state
 */
ClientDefaultWindowState(dpy)
	Display	*dpy;
{
	setComposeLed(dpy,LedModeOff);
}

/*
 * ClientSetWindowState - applies the client's window state
 */
ClientSetWindowState(cli)
	Client	*cli;
{
	if (!DoingWindowState)
		return;

	/*
 	 * If the client has specified the window state then interpret it.
 	 */
	if (cli->windowState) {
		if (cli->windowState->flags & WSSemanticState) {
			if (cli->windowState->state & WSSemanticCompose)
				setComposeLed(cli->dpy,LedModeOn);
			else
				setComposeLed(cli->dpy,LedModeOff);
		}
	/*
	 * Else just apply the default state
	 */
	} else {
		ClientDefaultWindowState(cli->dpy);
	}
}

/*
 * ClientGetWindowState - gets the _SUN_WINDOW_STATE property and if the
 *			  client has the focus then apply that new state.
 */
ClientGetWindowState(cli)
	Client	*cli;
{
	Window		pane;
	OLWindowState	winState;

	if (!DoingWindowState)
		return;

	if (cli->framewin == NULL)
		return;

	pane = PANEWINOFCLIENT(cli);

	if (!PropGetOLWindowState(cli->dpy,pane,&winState))
		return;

	if (cli->windowState == NULL)
		cli->windowState = MemNew(OLWindowState);

	*cli->windowState = winState;

	if (cli->isFocus)
		ClientSetWindowState(cli);
}

/*
 * ClientUpdateWindowState - handle PropertyNotify on _SUN_WINDOW_STATE
 */
void
ClientUpdateWindowState(cli,event)
	Client		*cli;
	XPropertyEvent	*event;
{
	if (!DoingWindowState)
		return;

	if (event->state == PropertyNewValue) {
		ClientGetWindowState(cli);
	} else {
		if (cli->windowState)
			MemFree((char *)cli->windowState);
		cli->windowState = (OLWindowState *)NULL;
		ClientDefaultWindowState(cli->dpy);
	}
}

/* ClientSendProtocol - send a protocol message to a client
 */
void *
ClientSendProtocol(cli,proto,evtime)
Client *cli;
Atom proto;
Time evtime;
{
	XEvent          clientEvent;

	clientEvent.xclient.type = ClientMessage;
	clientEvent.xclient.message_type = AtomProtocols;
	clientEvent.xclient.format = 32;
	clientEvent.xclient.display = cli->dpy;
	clientEvent.xclient.window = ClientPane(cli);
	clientEvent.xclient.data.l[0] = proto;
	clientEvent.xclient.data.l[1] = evtime;

	XSendEvent(cli->dpy, clientEvent.xclient.window, False,
		NoEventMask, &clientEvent);
	return NULL;
}

/* ClientShowProps - send a ClientMessage of type WM_SHOW_PROPERTIES
 */
void
ClientShowProps(cli)
Client *cli;
{
	XEvent clientEvent;
	
	clientEvent.xclient.type = ClientMessage;
	clientEvent.xclient.message_type = AtomShowProperties;
	clientEvent.xclient.format = 32;
	clientEvent.xclient.display = cli->dpy;
	clientEvent.xclient.window = PANEWINOFCLIENT(cli);

	XSendEvent(cli->dpy, clientEvent.xclient.window, False,
		NoEventMask, &clientEvent);
}

/* ClientKill - a client must be killed.  If it can handle the DELETE_WINDOW
 *	protocol, use it; otherwise, if we are forcing the client to go
 *	away, kill it.
 */
void *
ClientKill(cli,pforce)
Client *cli;
Bool pforce;
{
	if (cli->framewin == NULL)
		return NULL;

	if (cli->protocols & DELETE_WINDOW) {
		ClientSendProtocol(cli,AtomDeleteWindow,LastEventTime);
	} else {
		if (pforce && ! cli->flags & CLOlwmOwned)
			XKillClient(cli->dpy,ClientPane(cli));
		else
			if (GRV.Beep == BeepAlways)
				XBell(cli->dpy, 100);
	}
	return NULL;	
}

/* ClientShutdown - a client must be shut down.  Force the client
 * the go away without any further user interaction.
 */
void *
ClientShutdown(cli,junk)
Client	*cli;
void	*junk;
{
	/* Only kill non-olwm owned clients */
	if (! cli->flags & CLOlwmOwned)
		XKillClient(cli->dpy,ClientPane(cli));
	return NULL;
}


/* UnparentClient - while exiting OLWM: unmap all icons that are on the
 * screen.  Reparent all windows back to the root, suitably offset
 * according to their window-gravities.  Also remap all non-withdrawn
 * windows, and remove all Withdrawn windows from the save-set (so
 * they don't get remapped.  REMIND: We have to do this because
 * Withdrawn windows are still left reparented inside the frame; this
 * shouldn't be the case.
 */
/*ARGSUSED*/
void *
UnparentClient(cli,junk)
Client *cli;
void *junk;
{
	WinPaneFrame *frameInfo;
	WinPane *paneInfo;
	Window pane;

	/* if no framewin then it's probably a root window */
	if (cli->framewin == NULL)
		return NULL;

	if (cli->wmState == IconicState)
	{
		IconHide(cli, cli->iconwin);
	}

	frameInfo = cli->framewin;
	paneInfo = (WinPane*)(frameInfo->fcore.panewin);
	pane = paneInfo->core.self;
	FrameUnparentPane(cli, frameInfo, paneInfo);

	if (cli->wmState == DontCareState) 
	{ 
		XChangeSaveSet(cli->dpy, pane, SetModeDelete);
	} 
	else 
	{
		XMapWindow(cli->dpy,pane);
	}

	return NULL;
}

/*
 * addClient -- add this client structure to the list of active clients
 */
static void
addClient(cli)
Client *cli;
{
	List *l = ActiveClientList;
	Client *tc;

        /* First look to see if window is already listed.  */
        for(tc = ListEnum(&l); tc != NULL; tc = ListEnum(&l))
        {
                if (tc == cli)
                        return;
        }

        /* Wasn't present, add to list. */
	ActiveClientList = ListCons(cli, ActiveClientList);
}

/*
 * removeClient -- remove this client structure from the list of active
 *      clients.
 */
static void
removeClient(cli)
Client *cli;
{
	List **l;

	for (l = &ActiveClientList ; *l != NULL; l = &((*l)->next))
	{
		if ((*l)->value == cli)
		{
			ListDestroyCell(l);
			return;
		}
	}
}

/* 
 * DestroyClient -- destroy all resources associated with this client, and 
 * remove external references to this client.  If this is the current client, 
 * and we are in click-to-type, set the focus to the topmost client *after* 
 * having destroyed this client.
 */
void 
DestroyClient(cli)
Client *cli;
{
	Bool	    setfocus = False;
	Display	    *dpy = cli->dpy;
	ScreenInfo  *scrInfo = cli->scrInfo;
	List	    *l;
	Client	    *tcli;

	UnTrackSubwindows(cli, True);
	if (IsSelected(cli))
		RemoveSelection(cli);

	if (cli == CurrentClient) {
	    ClientSetCurrent(NoFocusWinInfo->core.client);
	    /* purge the old client from lastCurrentClient */
	    /* REMIND need a better interface */
	    lastCurrentClient = NULL;

	    setfocus = !GRV.FocusFollowsMouse;
	}
	
	removeClient(cli);

	/*
	 * Run through the remaining clients and remove any references to this 
	 * client from their warp-back records.
	 */
	l = ActiveClientList;
	while (tcli = ListEnum(&l)) {
	    if (tcli->framewin != NULL &&
		tcli->warpInfo.warpBackClient == cli)
	    {
		tcli->framewin->pointerIsWarped = False;
	    }
	}

	/* destroy the window resources associated with the client */
	WinCallDestroy(cli);

	GroupRemove(cli->groupid, cli);

	/* free up the client structure resources */
	if (cli->wmDecors)
		MemFree(cli->wmDecors);
	if (cli->normHints)
		MemFree(cli->normHints);
	if (cli->wmHints)
		MemFree(cli->wmHints);
	if (cli->wmInstance)
		MemFree(cli->wmInstance);
	if (cli->wmClass)
		MemFree(cli->wmClass);
	if (cli->windowState)
		MemFree(cli->windowState);

	/* REMIND  what's to be done with followers here? */

#ifdef DEBUG
	memset(cli, 0, sizeof(Client));
#endif /* DEBUG */

	MemFree(cli);

	if (setfocus)
	    ClientFocusTopmost(dpy, scrInfo, CurrentTime);
}

/*
 * ClientConfigure - a configure request event has been received on the
 * pane.  Configure the windows accordingly.
 */
void
ClientConfigure(cli,win,pxcre)
Client *cli;
WinGeneric *win;
XConfigureRequestEvent *pxcre;
{
        XWindowChanges          winChange;

	if ((cli == NULL) || (win == NULL))
	{
                /* We don't know about this window, or it's withdrawn
                 * convert the request into an XConfigureWindow
                 * call. We do not look at the hints to see if
                 * the resize is in the proper increments, but since
                 * the app is asking for the reconfigure this seems
                 * right.
                 */
                winChange.x = pxcre->x;
                winChange.y = pxcre->y;
                winChange.width = pxcre->width;
                winChange.height = pxcre->height;
                winChange.border_width = pxcre->border_width;
                winChange.stack_mode = pxcre->detail;
                winChange.sibling = pxcre->above;

                XConfigureWindow(pxcre->display,
                         pxcre->window,
                         /* lint will warn: this is a long, not int */
                         pxcre->value_mask,
                         &winChange );
	}
	else /* cli->wmState == NormalState or IconicState */
	{
		WinCallConfig(cli->dpy,win,pxcre);
	}
}

/*
 * ClientSetWMState     -- set the contents of the WM_STATE property,
 *                         given the information in the WinInfo struct.
 */
void
ClientSetWMState( cli )
	Client *cli;
{
	WinIconFrame *iconWinInfo = cli->iconwin;
	Window 	pane = PANEWINOFCLIENT(cli);
	Window	icon;

	if (iconWinInfo) {
        	if ( iconWinInfo->fcore.panewin )
        		icon = iconWinInfo->fcore.panewin->core.self;
        	else
            		icon = iconWinInfo->core.self;
    	} else {
        	icon = None;
    	}

	PropSetWMState(cli->dpy,pane,cli->wmState,icon);
}


/* ===== Drag-and-Drop Interest Property ================================== */


#define DRAGDROP_VERSION	0
#define INTEREST_RECT		0
#define INTEREST_WINDOW		1
#define SITE_DEFAULT		(1<<2)
#define SITE_FORWARD		(1<<3)

/*
 * Get the next word from the `data' array, indexed by `cur'.  If this causes 
 * us to go beyond `nitems', return silently.
 */
#define NEXTWORD(dest) do {						\
	    if (++cur >= nitems) {					\
		XFree((char *)data);					\
		return;							\
	    }								\
	    (dest) = data[cur];						\
	} while (0)

#define INCR(by) do {							\
	    cur += (by);						\
	    if (cur >= nitems)						\
		return;							\
	} while (0)


static struct {
    unsigned long
	version,
	nsites,
	wid,
	sid,
	flags,
	areatype,
	nelts,
	rx, ry, rw, rh;
} forwardingInterest = {
    0,			/* version */
    1,			/* nsites */
    0,			/* window id -- to be filled in */
    0,			/* site id -- to be filled in */
    0,			/* flags -- to be filled in */
    INTEREST_RECT,	/* areatype */
    1,			/* number of rects */
    0, 0, 0, 0		/* rectangle -- to be filled in */
};
#define FI_LENGTH \
	(sizeof(forwardingInterest)/sizeof(unsigned long))


/*
 * ClientProcessDragDropInterest - read the clients drag'n'drop interest
 * property, and put an interest containing the default site onto the frame
 * window and the icon window.  If we encounter an error reading the property,
 * do nothing and return silently.
 */
void
ClientProcessDragDropInterest(cli, state)
    Client *cli;
    int state;		/* PropertyNewValue or PropertyDelete */
{
    unsigned long *data;
    int nitems, remain, nsites, i, areatype, nelts;
    int cur = 0;
    Window wid;
    unsigned long sid, flags;

    if (state == PropertyDelete) {
	XDeleteProperty(cli->dpy, cli->framewin->core.self,
	    AtomSunDragDropInterest);
	if (cli->iconwin != NULL)
	    XDeleteProperty(cli->dpy, cli->iconwin->core.self,
		AtomSunDragDropInterest);
	return;
    }

    data = GetWindowProperty(cli->dpy, PANEWINOFCLIENT(cli),
	AtomSunDragDropInterest, 0L, 1000000L, AtomSunDragDropInterest,
	32, &nitems, &remain);

    if (data == NULL)
	return;

    if (data[cur] != DRAGDROP_VERSION) {
	XFree((char *)data);
	return;
    }

    NEXTWORD(nsites);
    for (i=0; i<nsites; ++i) {
	NEXTWORD(wid);
	NEXTWORD(sid);
	NEXTWORD(flags);
	NEXTWORD(areatype);
	switch (areatype) {
	case INTEREST_RECT:
	    NEXTWORD(nelts);
	    INCR(4*nelts);
	    break;
	case INTEREST_WINDOW:
	    NEXTWORD(nelts);
	    INCR(nelts);
	    break;
	default:
	    /* unknown area type; just return */
	    XFree((char *)data);
	    return;
	}
	if (flags & SITE_DEFAULT) {
	    forwardingInterest.wid = wid;
	    forwardingInterest.sid = sid;
	    forwardingInterest.flags = flags & ~SITE_DEFAULT;
	    forwardingInterest.flags |= SITE_FORWARD;

	    /* write the property on the frame */

	    forwardingInterest.rx = 0;
	    forwardingInterest.ry = 0;
	    forwardingInterest.rw = cli->framewin->core.width;
	    forwardingInterest.rh = cli->framewin->core.height;
	    XChangeProperty(cli->dpy, cli->framewin->core.self,
		AtomSunDragDropInterest, AtomSunDragDropInterest,
		32, PropModeReplace,
		(unsigned char *) &forwardingInterest, FI_LENGTH);

	    /* write the property on the icon */
	    if (cli->iconwin != NULL) {
		forwardingInterest.rx = 0;
		forwardingInterest.ry = 0;
		forwardingInterest.rw = cli->iconwin->core.width;
		forwardingInterest.rh = cli->iconwin->core.height;
		XChangeProperty(cli->dpy, cli->iconwin->core.self,
		    AtomSunDragDropInterest, AtomSunDragDropInterest,
		    32, PropModeReplace,
		    (unsigned char *) &forwardingInterest, FI_LENGTH);
	    }
	    break;
	}
    }
    XFree((char *)data);
}

/*
 * ClientUpdateDragDropInterest - handle PropertyNotify on DragDropInterest
 */
void
ClientUpdateDragDropInterest(cli,event)
	Client		*cli;
	XPropertyEvent	*event;
{
	ClientProcessDragDropInterest(cli,event->state);
}


/* ClientCreate -- allocate and initialise a client structure 
 */
Client *
ClientCreate(dpy,screen)
	Display *dpy;
	int 	screen;
{
	Client *cli = MemNew(Client);

	cli->wmState = DontCareState;
	cli->dpy = dpy;
	cli->screen = screen;
	cli->scrInfo = GetScrInfoOfScreen(screen);
	/* all other fields set to zero by allocation function */
	addClient(cli);
	return cli;
}

/* ClientPane - return the pane window of a client
 */
Window
ClientPane(cli)
Client *cli;
{
	WinPaneFrame *wf;
	WinPane *wp;

	if ((wf = cli->framewin) == NULL)
		return NULL;
	if ((wp = (WinPane *)(wf->fcore.panewin)) == NULL)
		return NULL;
	return wp->core.self;
}


/*
 * ClientOpenCloseToggle
 *	Perform the appropriate open/close action.
 */
void
ClientOpenCloseToggle(cli)
Client	*cli;
{
	/*
	 * If we are a pop-up (have a pushpin) or are transient,
	 * then dismiss.
	 */
	if ((cli->wmDecors->flags & WMDecorationPushPin)
		|| cli->transientFor != 0) {
	    ClientKill(cli,False);

	/* else switch the state from/to normal/iconic */
	} else {
		switch (cli->wmState) {
		case NormalState:
			StateNormIcon(cli);
			break;
		case IconicState:
			StateIconNorm(cli);
			break;
		default:
			break;
		}
	}
}

/*
 * ClientFullRestoreSizeToggle
 *	Call both frame and icon full/restore toggle functions.
 */
void
ClientFullRestoreSizeToggle(cli)
Client	*cli;
{
	if (cli->wmDecors->flags & WMDecorationResizeable) {
		(WinFunc(cli->framewin,fcore.fullrestoreToggle))(cli);
		(WinFunc(cli->iconwin,fcore.fullrestoreToggle))(cli);
	}
}

/*
 * ClientMove
 *	Moves the client to a new x,y position.
 */
void
ClientMove(cli,trigger)
    	Client 	*cli;
	XEvent	*trigger;
{
	UserMoveWindows(cli, trigger);
}

/*
 * ClientResize
 *	Resizes the client to a new w,h size.
 */
void
ClientResize(cli,trigger,which,callback,cbarg)
    	Client 	*cli;
	XEvent	*trigger;
	WhichResize which;
	void	(*callback)();
	void	*cbarg;
{
	if (cli->wmDecors->flags & WMDecorationResizeable)
	    UserResizeWin(cli, trigger, which, callback, cbarg);
}


/*
 * ClientRaiseTransients
 *
 * Raise any transient windows associated with this client.  Return the 
 * client for the bottommost transient window.  This is useful so that the 
 * parent window can be restacked just below the bottommost transient.  If 
 * there are no transient windows, returns NULL.
 *
 * REMIND raises all transient windows in the order they are encountered in 
 * the active client list, and returns the first one found.  Ideally, this 
 * should preserve the stacking order of the transients.
 */
Client *
ClientRaiseTransients(cli)
    Client *cli;
{
    List *l = ActiveClientList;
    Client *tc;
    Client *first = NULL;

    if (!GRV.KeepTransientsAbove)
	return NULL;

    while (tc = ListEnum(&l)) {
	if (tc->transientFor == PANEWINOFCLIENT(cli)) {
	    XRaiseWindow(tc->dpy, tc->framewin->core.self);
	    if (first == NULL)
		first = tc;
	}
    }
    return first;
}


/*
 * ClientLowerTransients
 *
 * Restack any transient windows associated with this client to be just above 
 * this client's frame.
 *
 * REMIND this lowers all transient windows in the order they are encountered 
 * in the active client list.  Ideally, this should preserve the stacking 
 * order of the transients.
 */
void
ClientLowerTransients(cli)
    Client *cli;
{
    List *l = ActiveClientList;
    Client *tc;
    XWindowChanges xwc;

    if (!GRV.KeepTransientsAbove)
	return;

    xwc.stack_mode = Above;
    xwc.sibling = cli->framewin->core.self;
    while (tc = ListEnum(&l)) {
	if (tc->transientFor == PANEWINOFCLIENT(cli)) {
	    XConfigureWindow(tc->dpy, tc->framewin->core.self,
			     CWSibling | CWStackMode, &xwc);
	}
    }
}


/*
 * ClientFront
 * 
 * Moves the appropriate client window to the front of the window hierarchy.
 * If this window has any transient windows, move them in front first.
 */
void
ClientFront(cli)
Client	*cli;
{
	Client *firsttransient;
	XWindowChanges xwc;

	switch (cli->wmState) {
	case NormalState:
		firsttransient = ClientRaiseTransients(cli);
		if (firsttransient != NULL) {
		    xwc.sibling = firsttransient->framewin->core.self;
		    xwc.stack_mode = Below;
		    XConfigureWindow(cli->dpy, cli->framewin->core.self,
				     CWSibling | CWStackMode, &xwc);
		} else {
		    XRaiseWindow(cli->dpy,cli->framewin->core.self);
		}
		break;
	case IconicState:
		XRaiseWindow(cli->dpy,cli->iconwin->core.self);
		break;
	default:
		break;
	}
}


/*
 * ClientBack
 * 
 * Moves the appropriate client window to the back of the window hierarchy.
 * If this is a transient window, move its parent window to the back first, 
 * and then move this window just in front of it.
 *
 * REMIND this isn't optimal behavior for transient windows, but it does 
 * ensure that transient windows always remain in front of their parents.
 */
void
ClientBack(cli)
Client	*cli;
{
	WinGeneric *owner;
	Window ownerwin;
	XWindowChanges xwc;

	switch (cli->wmState) {
	case NormalState:
		if (cli->transientFor != 0 && GRV.KeepTransientsAbove) {
		    owner = WIGetInfo(cli->transientFor);
		    if (owner == NULL ||
			    owner->core.kind != WIN_PANE) {
			XLowerWindow(cli->dpy,cli->framewin->core.self);
		    } else {
			ownerwin = owner->core.client->framewin->core.self;
			XLowerWindow(cli->dpy, ownerwin);
			xwc.sibling = ownerwin;
			xwc.stack_mode = Above;
			XConfigureWindow(cli->dpy, cli->framewin->core.self,
					 CWSibling | CWStackMode, &xwc);
		    }
		} else {
		    XLowerWindow(cli->dpy,cli->framewin->core.self);
		}
		break;
	case IconicState:
		XLowerWindow(cli->dpy,cli->iconwin->core.self);
		break;
	default:
		break;
	}
}


/*
 * ClientToggleStacking
 *
 * Moves the appropriate client window to the front of the window hierarchy if 
 * it is obscured, otherwise move it to the back of the hierarchy if it 
 * obscures any other window.  REMIND: doesn't deal with transient windows at 
 * all.
 */
void
ClientToggleStacking(cli)
Client	*cli;
{
	XWindowChanges xwc;
	Window win;

	switch (cli->wmState) {
	case NormalState:
		win = cli->framewin->core.self;
		break;
	case IconicState:
		win = cli->iconwin->core.self;
		break;
	}

	xwc.stack_mode = Opposite;
	XConfigureWindow(cli->dpy, win, CWStackMode, &xwc);
}


/*
 * ClientRefresh
 *  	Refresh the window.  We do this by creating a window on top
 * 	of the window to refresh and then immediately destroy it.
 *	Refresh either icon or frame windows.
 */
void
ClientRefresh(cli)
Client *cli;
{
	Window 	w,cliwin;
	int	cliwidth,cliheight;
	XSetWindowAttributes xswa;

	/* 	Chose which window to refresh */
	switch (cli->wmState) {
	case NormalState:
		cliwin = cli->framewin->core.self;
		cliwidth = cli->framewin->core.width;
		cliheight = cli->framewin->core.height;
		break;
	case IconicState:
		cliwin = cli->iconwin->core.self;
		cliwidth = cli->iconwin->core.width;
		cliheight = cli->iconwin->core.height;
		break;
	default:
		return;
		/*NOTREACHED*/
		break;
	}

	/*	if we should refresh all windows */
    	if (GRV.RefreshRecursively) {
		RecursiveRefresh(cli->dpy,cliwin);

	/* 	or just the top/main window */
    	} else {
		w = XCreateWindow(cli->dpy,
			cliwin, 0, 0, cliwidth, cliheight, 0,
	    		CopyFromParent, InputOutput, CopyFromParent,
	    		0, &xswa);
		XMapRaised(cli->dpy,w);
		XDestroyWindow(cli->dpy,w);
    }
}

/*
 * ClientFlashOwner
 *	Find group leader frame, bring it to the top and then
 *	flash its title bar.
 * 
 *	REMIND:  make sure transient windows get treated properly
 *		 before or after (which? not sure) the leader is raised.
 */
void
ClientFlashOwner(cli)
Client	*cli;
{
	Client  *cliLead = GroupLeader(cli->groupid);
	if (cliLead && cliLead->framewin) {
		XRaiseWindow(cliLead->dpy,cliLead->framewin->core.self);
		WinCallDraw((WinGeneric *)cliLead->framewin);
		FrameFlashTitleBar(cliLead->framewin);
	}
}

/*
 * Toggle the pushpin of this client's window.  Returns False if this client's 
 * window has no pin, otherwise, returns True.
 */
Bool
ClientTogglePin(cli)
    Client *cli;
{
    WinPushPin *pin;

    if (cli->framewin == NULL || !ClientIsPinnable(cli))
	return False;

    pin = (WinPushPin *)cli->framewin->winDeco;
    PushPinTogglePinState(cli->dpy,pin,True);
}


/* ClientInBox -- given a bounding box, apply a function to all clients
 *      which fall inside the rectangle
 */
void *
ClientInBox(cli, close)
Client *cli;
ClientInBoxClosure *close;
{
	int x, y, w, h;

	if (cli->screen != close->screen)
		return NULL;

	if (cli->framewin == NULL)
		return NULL;

	if (cli->wmState == IconicState)
	{
		x = cli->iconwin->core.x;
		y = cli->iconwin->core.y;
		w = cli->iconwin->core.width;
		h = cli->iconwin->core.height;
	}
	else
	{
		x = cli->framewin->core.x;
		y = cli->framewin->core.y;
		w = cli->framewin->core.width;
		h = cli->framewin->core.height;
	}

	if ((x >= close->bx) &&
	    (y >= close->by) &&
	    ((x + w) <= (close->bx + close->bw)) &&
	    ((y + h) <= (close->by + close->bh)))
		(close->func)(cli, close->timestamp);

	return NULL;
}

/*
 * ClientSetBusy -- the busy state has (possibly) been changed for a client.
 *	if the client is going from normal to busy:
 *          mark the client as busy
 *          put up a busy window
 *      if the client is going from busy to normal:
 *          mark the client as unbusy
 *          take down a busy window (if it exists)
 */
void
ClientUpdateBusy(cli,event)
	Client 		*cli;
	XPropertyEvent	*event;
{
        int *newBusyPtr;
	int newBusy;
        unsigned long nItems, remain;

	if (event->state == PropertyNewValue) {
	    newBusyPtr = GetWindowProperty(cli->dpy, PANEWINOFCLIENT(cli),
		AtomWindowBusy, 0L, LONG_LENGTH(*newBusyPtr), 
		XA_INTEGER, 32, &nItems, &remain);

	    if (newBusyPtr == NULL) {
		/* property not found or has the wrong type */
		newBusy = 0;
	    } else {
		if (nItems != LONG_LENGTH(newBusy) || remain != 0) {
		    /* got a property, but it is invalid */
		    newBusy = 0;
		} else {
		    /* valid property */
		    newBusy = *newBusyPtr;
		}
		XFree((char *)newBusyPtr);
	    }
	} else {
	    /* property was deleted */
	    newBusy = 0;
	}

	/* 
	 * Losing busy
	 */
        if (cli->isBusy && (newBusy == 0)) {
            cli->isBusy = False;
	    if (cli->isFocus)
		ClientSetFocus(cli,True,event->time);
	    FrameSetBusy(cli->framewin, False);
        }
	/*
	 * Else Gaining busy
	 */
        else if (!cli->isBusy && (newBusy == 1)) {
            cli->isBusy = True;
	    FrameSetBusy(cli->framewin, True);
	    if (cli->isFocus)
	    	    ClientSetFocus(cli,True,event->time);
        }
}


/*
 *	REMIND	this function should be removed when menu/menuinfos
 *	are reorged.
 */
void
DestroyPinnedMenuClients()
{
	List	*l = ActiveClientList;
	Client	*cli;


	for (cli = ListEnum(&l); cli != NULL; cli = ListEnum(&l)) {
		if (cli->framewin && cli->framewin->fcore.panewin &&
		    cli->framewin->fcore.panewin->core.kind == WIN_PINMENU) {
#ifdef DEBUG
			printf("DestroyPinnedMenuClients: destroyed %x\n",cli);
#endif
			DestroyClient(cli);
		}
	}
}


/* ===== focus stuff ====================================================== */


/*
 * Focus Change Inhibition.
 *
 * If focus changing is inhibited, information about focus changing is stored
 * in the FocusInhibitRecord instead of being used to set the focus.  If
 * several focus changes occur while focus changing is inhibited, only
 * information for the latest change is stored.  When focus changing becomes
 * uninhibited, the information is used to set the focus for real.  This 
 * prevents unnecessary focus changing.  REMIND: focus inhibition doesn't 
 * occur when the focus is set to the NoFocus window.  This isn't too bad, as 
 * no highlighting occurs when this happens.
 */

static struct FocusInhibitRecord {
    Bool inhibited;
    Client *cli;
    Bool sendTF;
    Time evtime;
} fir;


/*
 * ClientInhibitFocus -- inhibit or uninhibit focus changing.
 */
void
ClientInhibitFocus(inhibit)
    Bool inhibit;
{
    if (inhibit) {
	fir.inhibited = True;
	fir.cli = NULL;
    } else {
	fir.inhibited = False;
	if (fir.cli != NULL)
	    ClientSetFocus(fir.cli, fir.sendTF, fir.evtime);
	fir.cli = NULL;
    }
}


/*
 * ClientSetFocus -- possibly set the focus to this client.  If focus changing
 * is inhibited, store information in the inhibit record.  Otherwise, set the
 * focus normally.  If the client is GloballyActive, we only send TakeFocus
 * messages if sendTF is true.  If the client has a different focus mode
 * sendTF is ignored.  If the client is a NoInput client, set the focus to the
 * frame itself.  REMIND: this works, if a little bizarre.  The frame doesn't
 * select for keystrokes, so they fall to the root.  The root event handler
 * ends up beeping, which is OK.
 */
void
ClientSetFocus(cli,sendTF,evtime)
Client *cli;
Bool sendTF;
Time evtime;
{
	if (fir.inhibited) {
	    fir.cli = cli;
	    fir.sendTF = sendTF;
	    fir.evtime = evtime;
	    return;
	}

	if (cli->wmState == IconicState) {
	    if (cli->iconwin != NULL)
		XSetInputFocus(cli->dpy, cli->iconwin->core.self,
			       RevertToParent, evtime);
	} else {
	    switch (cli->focusMode)
	    {
	    case NoInput:
		XSetInputFocus(cli->dpy, cli->framewin->core.self,
			       RevertToParent, evtime);
	        break;

	    case Passive:
	    case LocallyActive:
	        XSetInputFocus(cli->dpy, PANEWINOFCLIENT(cli),
		    RevertToParent, evtime);
	        break;

	    case GloballyActive:
	        if (sendTF)
	        {
		    ClientSendProtocol(cli, AtomTakeFocus, evtime);
	        }
	        break;
	    }
	}
}


/*
 * Current Client.
 *
 * The current client is used by the mouseless functions.  These functions
 * maintain the current and previously-current client.  If cli is already the
 * current client, don't do anything.  This is necessary because this may be
 * called when a client is activated explicitly (from ClientActivate) or
 * implicitly (when a globally active client takes the focus).  Therefore, if
 * ClientActivate activates a globally active client, this function will be
 * called twice.
 * 
 * Eventually, this may change to be a "ring-buffer" history of clients.
 *
 * REMIND: the notion of the current client may be a vestige from the time
 * when some objects (like icons and pinned menus) didn't take the focus.
 * Activating one of these items would set the focus to the NoFocus window,
 * but set the current client to that particular client.  Thus, having the
 * focus was not the same as being the current client.  This distinction may
 * no longer be true, in which case it is sufficient to keep track of the
 * client that has the focus, without the separate notion of a current client.
 * One possible problem may occur with buggy Globally Active clients that
 * don't take the focus when requested.  If we rely on the focus to keep track
 * of the current client, we may get "stuck" if the next client fails to take
 * the focus when requested.
 */
void
ClientSetCurrent(cli)
    Client *cli;
{
    if (cli != CurrentClient) {
	lastCurrentClient = CurrentClient;
	CurrentClient = cli;
    }
}


Client *
ClientGetLastCurrent()
{
    return lastCurrentClient;
}


/*
 * Client Activation.
 * 
 * Activate the named client.  The difference between this and ClientSetFocus
 * is that this function selects and raises the client's window in addition to
 * setting the focus.  Further, this function works on iconic clients as well
 * as open ones.  This function sets the current client explicitly.  We would
 * rely on the resulting focus change to set the current client, except that
 * a globally active client may decline the focus when it is asked to take it.
 *
 * REMIND this shouldn't have to deal with selections at all.  However, 
 * icons and headerless windows currently have no way to indicate that they 
 * have the focus.  Therefore, select them.
 */
void
ClientActivate(dpy, cli, time)
    Display *dpy;
    Client *cli;
    Time time;
{
    /*
     * If the current client is selected, assume it was selected because it 
     * was made the active client, and deselect it.  (See REMIND above.)
     */
    if (CurrentClient != NULL && CurrentClient->isSelected)
	ClearSelections(dpy);

    /*
     * If we are being asked to activate a NULL client, or a client without a 
     * frame (i.e. a root client), activate the NoFocus client.
     */
    if (cli == NULL) {
	NoFocusTakeFocus(dpy, time, NULL);
    } else if (cli->framewin == NULL) {
	NoFocusTakeFocus(dpy, time, cli->scrInfo);
    } else {
	ClientSetFocus(cli, True, time);

	/*
	 * If the client is iconic or has no header, select it to show that it 
	 * is the active client.  (See REMIND above.)
	 */
	if (cli->wmState == IconicState ||
		!(cli->wmDecors->flags & WMDecorationHeader)) {
	    ClearSelections(dpy);
	    AddSelection(cli, time);
	}

	if (GRV.RaiseOnActivate)
	    ClientFront(cli);

	ClientSetCurrent(cli);
    }
}


/*
 * Set the focus to the topmost window on the given screen.
 */
void
ClientFocusTopmost(dpy, scrinfo, time)
    Display *dpy;
    ScreenInfo *scrinfo;
    Time time;
{
    Window wjunk;
    Window *children;
    unsigned int nchildren;
    int i;
    Client *topframeclient = NULL;
    Client *topiconclient = NULL;
    WinGeneric *win;

    if (0 == XQueryTree(dpy, scrinfo->rootid, &wjunk, &wjunk,
			&children, &nchildren))
    {
	return;
    }

    /*
     * QueryTree returns children on bottom-to-top order, so search backward, 
     * looking for the topmost frame and icon.
     */
    for (i = nchildren-1; i >= 0; --i) {
	win = WIGetInfo(children[i]);
	if (win != NULL) {
	    if (win->core.kind == WIN_FRAME &&
		win->core.client->wmState == NormalState &&
		topframeclient == NULL)
	    {
		topframeclient = win->core.client;
	    }

	    if (win->core.kind == WIN_ICON &&
		win->core.client->wmState == IconicState &&
		topiconclient == NULL)
	    {
		topiconclient = win->core.client;
	    }

	    if (topframeclient != NULL && topiconclient != NULL)
		break;
	}
    }

    if (topframeclient != NULL)
	ClientSetFocus(topframeclient, True, time);
    else if (topiconclient != NULL)
	ClientSetFocus(topiconclient, True, time);
    else
	NoFocusTakeFocus(dpy, time, scrinfo);

    XFree((char *) children);
}

/* ===== Client Property Changes ================================== */

typedef struct {
	Atom	*propAtom;
	void	(*updateFunc)();
} ClientPropUpdate; 

static ClientPropUpdate propUpdateTable[] =  {
	&AtomWMName,			FrameUpdateHeader,
	&AtomLeftFooter,		FrameUpdateFooter,
	&AtomRightFooter,		FrameUpdateFooter,
	&AtomWMIconName,		IconUpdateName,
	&AtomColorMapWindows,		ColorUpdateColorMapWindows,
	&AtomProtocols,			StateUpdateWMProtocols,
	&AtomWMNormalHints,		StateUpdateWMNormalHints,
	&AtomWMHints,			StateUpdateWMHints,
	&AtomWindowBusy,		ClientUpdateBusy,
	&AtomSunWindowState,		ClientUpdateWindowState,
	&AtomSunDragDropInterest,	ClientUpdateDragDropInterest,
	&AtomWinAttr,			StateUpdateWinAttr,
	&AtomDecorAdd,			StateUpdateDecorAdd,
	&AtomDecorDel,			StateUpdateDecorDel,
};
#define NPROPUPDATETABLE (sizeof(propUpdateTable)/sizeof(ClientPropUpdate))

/* ClientDistributeProperty -- a property of the client has changed.
 *	Forward the change notification to the appropriate handler.
 */
void
ClientDistributeProperty(cli, event)
	Client		*cli;
	XPropertyEvent	*event;
{
	int		i;

	for (i=0; i<NPROPUPDATETABLE; i++) {
		if (event->atom == *propUpdateTable[i].propAtom) {
			(propUpdateTable[i].updateFunc)(cli,event);
			break;
		}
	}
}
