/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)kbdfuncs.c	1.14	91/09/14 SMI"

#include <stdio.h>
#ifdef SYSV
#include <string.h>
#else
#include <strings.h>
#endif

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "i18n.h"
#include "ollocale.h"
#include "olwm.h"
#include "win.h"
#include "menu.h"
#include "globals.h"
#include "group.h"
#include "resources.h"

/*
 * Externals
 */
extern Client *CurrentClient;

extern void ClientActivate();
extern Client *ClientGetLastCurrent();
extern void ClientLowerTransients();
extern Client *ClientRaiseTransients();
extern void ClientSetCurrent();

/* ===== private functions ================================================ */


typedef struct {
    Bool prev;
    Bool seencur;
    Client *cur;
    Client *next;
} FindClosure;


/*
 * Given a keystroke event, find the window (and its client structure) that is
 * under the >pointer<.  If the pointer is on the same screen as the event
 * window, then this is simply the subwindow field of the event.  If the
 * pointer is on another screen, we must do a TranslateCoordinates to find it.
 * Returns True if a window with a client structure was found, otherwise
 * returns False.
 */
static Bool
childUnderPointer(dpy, ke, wid, info, cli)
    Display *dpy;
    XKeyEvent *ke;
    Window *wid;
    WinGeneric **info;
    Client **cli;
{
    int junk;

    if (ke->same_screen) {
	*wid = ke->subwindow;
    } else {
    	(void) XTranslateCoordinates(dpy, ke->root, ke->root,
	    ke->x_root, ke->y_root, &junk, &junk, wid);
    }
    if (*wid == None)
	return False;
    *info = WIGetInfo(*wid);
    if (*info == NULL)
	return False;
    *cli = (*info)->core.client;
    if (*cli == NULL)
	return False;
    return True;
}


/*
 * Derive a value related to an angle without using trigonometry or floating 
 * point.  Derived from Sedgewick, Algorithms, p. 315.  Returns a value in the 
 * range [0..360), scaled by 2^16.  Since Y is positive-downwards in the X 
 * window system, angles increase in the clockwise direction.  This is what we 
 * want.  However, we rotate by -90 to get zero at the top instead of at the 
 * right.
 */

#define SCALE(i)	((i)<<16)
#define UNSCALE(i)	((i)>>16)
#define REMSCALE(i)	(((i)&0xffff)*10000/65536*10)


long
itheta(p1x, p1y, p2x, p2y)
    int p1x, p1y, p2x, p2y;
{
    long ax, ay, dx, dy;
    long t;

    /*
     * You might think that the code below should be:
     * 
     *	    dx = p2x - p1x;
     *	    dy = p2y - p1y;
     *
     * In the X window system, the x-axis is positive right and the y-axis is 
     * positive down.  This puts the zero angle at the right, increasing 
     * clockwise.  We want zero at the top, increasing clockwise.  To do this, 
     * we swap the x and y values and negate the x delta.  This is why the x's 
     * and y's and 1's and 2's are apparently jumbled.
     */

    dx = p1y - p2y;
    dy = p2x - p1x;

    ax = ABS(dx);
    ay = ABS(dy);

    if (dx == 0 && dy == 0)
	t = 0;
    else
	t = SCALE(dy) / (ax+ay);
	    
    if (dx < 0)
	t = SCALE(2)-t;
    else if (dy < 0)
	t = SCALE(4)+t;
    t *= 90;


#ifdef notdef
    /*
     * Rotate so that zero is at top.  We could do this by swapping and
     * negating the dx and dy values above, but this would reduce readability.
     */
    t += SCALE(90);
    if (t >= SCALE(360))
	t -= SCALE(360);
#endif /* notdef */

    return t;
}


/*
 * Compare two client structures for ordering within the global window order.
 * Sort ordering is based first on screen number, then window state (open
 * considered to be before closed), then window position.  Return value if -1
 * if c1 is before c2, 1 if c1 is after c2, and 0 if they are tied.  The bool
 * prev reverse the sense sort order.  The NoFocus client is considered to
 * come before all other clients, regardless of whether the prev bool is set.
 *
 * A static boolean value AngularTraversal controls whether the tranversal 
 * policy should be based on the angular position of the window.  If false, 
 * the old policy of upper-left to lower-right is used.
 */
static int
compareClients(c1, c2, prev)
    Client *c1;
    Client *c2;
    Bool prev;
{
    int pos1, pos2;
    int x1, x2, y1, y2;
    int LESS, GREATER;
    static Bool AngularTraversal = True;

    /* First check if both or either are the NoFocus client. */

    if (c1->framewin == NULL && c2->framewin == NULL)
	return 0;
    if (c1->framewin == NULL)
	return -1;
    if (c2->framewin == NULL)
	return 1;

    if (prev) {
	LESS = 1;
	GREATER = -1;
    } else {
	LESS = -1;
	GREATER = 1;
    }

    /* Compare screens. */

    if (c1->screen < c2->screen)
	return LESS;
    if (c1->screen > c2->screen)
	return GREATER;

    /*
     * Compare states.  Note: this depends on the state values defined in
     * Xutil.h.  Notably, NormalState < IconicState.  This will need to be 
     * changed if olwm adds any new state values.
     */
    if (c1->wmState < c2->wmState)
	return LESS;
    if (c1->wmState > c2->wmState)
	return GREATER;

    /* They are both the same state. */

    if (AngularTraversal) {

	/* calculate angle to window center */

	long theta1, theta2;
	WinRoot *root = c1->scrInfo->rootwin;
	int rootcx = root->core.width / 2;
	int rootcy = root->core.height / 2;

	if (c1->wmState == NormalState) {
	    x1 = c1->framewin->core.x + c1->framewin->core.width / 2;
	    y1 = c1->framewin->core.y + c1->framewin->core.height / 2;
	    x2 = c2->framewin->core.x + c2->framewin->core.width / 2;
	    y2 = c2->framewin->core.y + c2->framewin->core.height / 2;
	} else {
	    x1 = c1->iconwin->core.x + c1->iconwin->core.width / 2;
	    y1 = c1->iconwin->core.y + c1->iconwin->core.height / 2;
	    x2 = c2->iconwin->core.x + c2->iconwin->core.width / 2;
	    y2 = c2->iconwin->core.y + c2->iconwin->core.height / 2;
	}

	theta1 = itheta(rootcx, rootcy, x1, y1);
	theta2 = itheta(rootcx, rootcy, x2, y2);

	if (theta1 < theta2)
	    return LESS;
	else if (theta1 > theta2)
	    return GREATER;

    } else {

	/* Sort from upper-left to lower-right. */

	if (c1->wmState == NormalState) {
	    x1 = c1->framewin->core.x;
	    y1 = c1->framewin->core.y;
	    x2 = c2->framewin->core.x;
	    y2 = c2->framewin->core.y;
	} else {
	    x1 = c1->iconwin->core.x;
	    y1 = c1->iconwin->core.y;
	    x2 = c2->iconwin->core.x;
	    y2 = c2->iconwin->core.y;
	}
	pos1 = MAX(x1,y1);
	pos2 = MAX(x2,y2);

	if (pos1 < pos2)
	    return LESS;
	if (pos1 > pos2)
	    return GREATER;

	/* Positions are tied.  Compare scanline order. */
	if (y1 < y2)
	    return LESS;
	if (y1 > y2)
	    return GREATER;
	if (x1 < x2)
	    return LESS;
	if (x1 > x2)
	    return GREATER;
    }
    return 0;
}


static void *
findClient(cli, closure)
    Client *cli;
    FindClosure *closure;
{
    switch (compareClients(cli, closure->cur, closure->prev)) {
    case -1:
	/* this is earlier in the sort order; ignore */
	break;
    case 0:
	/*
	 * equal: if we've just seen the current client, advance to next; 
	 * otherwise, ignore.
	 */
	if (closure->seencur) {
	    closure->next = cli;
	    closure->seencur = False;
	}
	closure->seencur = (cli == closure->cur);
	break;
    case 1:
	/*
	 * later in sort order; advance if earlier than later ones
	 * already seen.
	 */
	if (closure->next == NULL)
	    closure->next = cli;
	else if (compareClients(cli, closure->next, closure->prev) < 0)
	    closure->next = cli;
	break;
    }
    return NULL;
}


/*
 * Activate the next or previous popup window or pinned menu of the current 
 * application.
 */
static void
selectNextPrevWindow(dpy, ke, prev)
    Display *dpy;
    XKeyEvent *ke;
    Bool prev;
{
    GroupID grpid;
    Group *group;
    FindClosure closure;

    if (ke->type != KeyPress)
	return;

    closure.prev = prev;
    closure.cur  = CurrentClient;
    closure.next = NULL;
    closure.seencur = False;

    (void) GroupApply(CurrentClient->groupid, findClient, &closure,
		      GROUP_LEADER | GROUP_DEPENDENT);
    if (closure.next == NULL) {
	/*
	 * We didn't find a suitable client.  Wrap around by resetting the 
	 * current client to the NoFocus client and searching again.
	 */
	closure.cur = NoFocusWinInfo->core.client;
	closure.next = NULL;
	closure.seencur = False;
	(void) GroupApply(CurrentClient->groupid, findClient, &closure,
			  GROUP_LEADER | GROUP_DEPENDENT);
    }

    if (CurrentClient != closure.next)
	ClientActivate(dpy, closure.next, ke->time);
}


/*
 * Activate the next or previous application's base window.
 */
static void
selectNextPrevApp(dpy, ke, prev)
    Display *dpy;
    XKeyEvent *ke;
    Bool prev;
{
    Client *cli;
    Client *next = NULL;
    Client *first = NULL;
    List *l = ActiveClientList;
    int cmp;
    Bool seencur = False;

    if (ke->type != KeyPress)
	return;

    if (CurrentClient == NULL) {
	/* Consider the current client to be the NoFocus client. */
	ClientSetCurrent(NoFocusWinInfo->core.client);
    }

    l = ActiveClientList;
    for (cli = ListEnum(&l); cli != NULL; cli = ListEnum(&l)) {

	/* skip special screen clients (including NoFocus client) */
	if (cli->framewin == NULL)
	    continue;

	/* skip dependent followers */
	if (cli->groupmask == GROUP_DEPENDENT)
	    continue;

	switch (compareClients(cli, CurrentClient, prev)) {
	case -1:
	    /* earlier in the sort order: see if it's the first client */
	    if (first == NULL || compareClients(cli, first, prev) == -1)
		first = cli;
	    break;
	case 0:
	    /*
	     * equal: if we've just seen the current client, advance to next; 
	     * otherwise, ignore.
	     */
	    if (seencur) {
		next = cli;
		seencur = False;
	    }
	    seencur = (cli == CurrentClient);
	    if (first == NULL)
		first = cli;
	    break;
	case 1:
	    /*
	     * later in sort order; advance if earlier than later ones
	     * already seen.
	     */
	    if (next == NULL)
		next = cli;
	    else if (compareClients(cli, next, prev) < 0)
		next = cli;
	    break;
	}
    }

    if (next != NULL)
	ClientActivate(dpy, next, ke->time);
    else
	ClientActivate(dpy, first, ke->time);
}


/* ===== global functions ================================================= */


/*
 * Beep on keystrokes, but only if the key isn't a modifier, and if beeping is 
 * enabled.
 */
void
KeyBeep(dpy,ke)
    Display *dpy;
    XKeyEvent *ke;
{
    /*
     * REMIND: we should use xkeyevent->time to make sure that we don't send 
     * to many beep requests within a small period of time.
     */
    if ((FindModifierMask(ke->keycode) == 0) && (GRV.Beep == BeepAlways))
        XBell(dpy,100);
}


/*
 * Lock the colormap of the window under the pointer into the hardware.
 */
void
KeyLockColormap(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    if (ke->type != KeyPress)
	return;
    InstallPointerColormap(dpy, ke->root, ke->x_root, ke->y_root, True);
}


/*
 * Unlock the colormap; revert to color-follows-mouse mode.
 */
void
KeyUnlockColormap(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    if (ke->type != KeyPress)
	return;
    UnlockColormap(dpy, ke->root, ke->x_root, ke->y_root);
}


/*
 * KeyRaiseLowerPointer - raise or lower the window under the pointer
 *
 * Restack the window under the pointer, similar to the SunView Front key.  If
 * this window is at all occluded, raise it.  If other windows are occluded by
 * this window, lower it.  (This is the behavior of the value Opposite for the
 * stack-mode parameter of a ConfigureWindow request.)
 *
 * The behavior is different if this window is a transient window, or if this
 * window has transient windows associated with it.  If this window is a
 * transient window, the parent and all transients are pushed to the back,
 * with the transients remaining above the parent.  If this window is a parent
 * of any transient windows, all are raised to the front, with the parent
 * remaining behind the transients.
 *
 * REMIND this is probably suboptimal behavior for transients and parents of 
 * transients, as the occlusion rule is not used for them.  This may result in 
 * apparent misbehavior.
 */
void
KeyRaiseLowerPointer(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    Window child;
    WinGeneric *childinfo;
    Client *cli;
    XWindowChanges xwc;
    WinGeneric *owner;
    Client *transient;

    if (ke->type != KeyPress)
	return;

    /*
     * If childUnderPointer returns a valid window structure, we use it.  If 
     * childUnderPointer returns False, we go ahead and reconfigure any window 
     * found, even if we don't know about it.  This is useful for restacking
     * override-redirect windows.
     */
    if (childUnderPointer(dpy, ke, &child, &childinfo, &cli)) {
	if (cli->transientFor != 0 &&
		GRV.KeepTransientsAbove &&
		(owner = WIGetInfo(cli->transientFor)) != NULL &&
		owner->core.kind == WIN_PANE &&
		owner->core.client != NULL) {
	    XLowerWindow(cli->dpy,
			 owner->core.client->framewin->core.self);
	    ClientLowerTransients(owner->core.client);
	} else {
	    transient = ClientRaiseTransients(cli);
	    if (transient != NULL) {
		xwc.stack_mode = Below;
		xwc.sibling = transient->framewin->core.self;
		XConfigureWindow(dpy, child, CWStackMode | CWSibling, &xwc);
	    } else {
		xwc.stack_mode = Opposite;
		XConfigureWindow(dpy, child, CWStackMode, &xwc);
	    }
	}
    } else {
	if (child != None) {
	    xwc.stack_mode = Opposite;
	    XConfigureWindow(dpy, child, CWStackMode, &xwc);
	}
    }
}


/*
 * Toggle the open/closed state of the window under the pointer.
 */
void
KeyOpenClosePointer(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    Window child;
    WinGeneric *childinfo;
    Client *cli;

    if (ke->type != KeyPress)
	return;

    if (childUnderPointer(dpy, ke, &child, &childinfo, &cli))
	ClientOpenCloseToggle(cli);
}


/*
 * Set the focus to the window under the pointer.  If there is no window under 
 * the pointer, set the focus to the no-focus window.
 */
void
KeyFocusToPointer(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    Window child;
    WinGeneric *childinfo;
    Client *cli;

    if (ke->type != KeyPress)
	return;

    if (childUnderPointer(dpy, ke, &child, &childinfo, &cli))
	ClientSetFocus(cli, True, ke->time);
    else
	NoFocusTakeFocus(dpy, ke->time, GetScrInfoOfRoot(ke->root));
}


/*
 * Activate the base window of the next application.
 */
void
KeyNextApp(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    selectNextPrevApp(dpy, ke, False);
}


/*
 * Activate the base window of the previous application.
 */
void
KeyPrevApp(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    selectNextPrevApp(dpy, ke, True);
}


/*
 * Activate the next popup window or pinned menu of the current application.
 */
void
KeyNextWindow(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    selectNextPrevWindow(dpy, ke, False);
}


/*
 * Activate the previous popup window or pinned menu of the current
 * application.
 */
void
KeyPrevWindow(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    selectNextPrevWindow(dpy, ke, True);
}


/*
 * Toggle between this and the previous active window.
 */
void
KeyToggleInput(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    Client *next;

    if (ke->type != KeyPress)
	return;

    next = ClientGetLastCurrent();
    ClientActivate(dpy, next, ke->time);
}


/*
 * Toggle the pin if the active window, if any.
 */
void
KeyTogglePin(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    if (ke->type != KeyPress)
	return;

    if (!ClientTogglePin(CurrentClient))
	KeyBeep(dpy,ke);
}


/*
 * Refresh the active window.
 */
void
KeyRefresh(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    if (ke->type != KeyPress)
	return;

    if (CurrentClient->framewin != NULL)
	ClientRefresh(CurrentClient);
}


/*
 * Raise the active window to the front.
 */
void
KeyFrontFocus(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    if (ke->type != KeyPress)
	return;

    if (CurrentClient->framewin != NULL)
	ClientFront(CurrentClient);
}


/*
 * Send the active window to the back.
 */
void
KeyBackFocus(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    if (ke->type != KeyPress)
	return;

    if (CurrentClient->framewin != NULL)
	ClientBack(CurrentClient);
}


/*
 * Toggle the open/close state of the active window.
 */
void
KeyOpenCloseFocus(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    if (ke->type != KeyPress)
	return;

    if (CurrentClient->framewin != NULL)
	ClientOpenCloseToggle(CurrentClient);
}


/*
 * Toggle the full/restored size of the active window.
 */
void
KeyFullRestore(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    if (ke->type != KeyPress)
	return;

    if (CurrentClient->framewin != NULL)
	ClientFullRestoreSizeToggle(CurrentClient);
}


/*
 * Quit the active window.
 */
void
KeyQuit(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    if (ke->type != KeyPress)
	return;

    if (CurrentClient->framewin != NULL)
	ClientKill(CurrentClient, True);
}


/*
 * Flash the owner window of the active window.
 */
void
KeyOwner(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    if (ke->type != KeyPress)
	return;

    if (CurrentClient->framewin != NULL)
	ClientFlashOwner(CurrentClient);
}


/*
 * Move the active window.
 */
void
KeyMove(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    if (ke->type != KeyPress)
	return;

    if (CurrentClient->framewin != NULL)
	ClientMove(CurrentClient,ke);
}


/*
 * Resize the active window.
 */
void
KeyResize(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    if (ke->type != KeyPress)
	return;

    if (CurrentClient->framewin != NULL)
	ClientResize(CurrentClient, ke, keyevent, NULL, NULL);
}


/*
 * Bring up the Properties window for the active window.
 */
void
KeyProperties(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
	/* REMIND - fill in when window properties implemented */
}


/*
 * Bring up the workspace menu.
 */
void
KeyWorkspaceMenu(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    if (ke->type == KeyPress)
	RootMenuShow(dpy, WIGetInfo(ke->root), ke);
}


/*
 * Bring up the window menu for the active menu.
 */
void
KeyWindowMenu(dpy, ke)
    Display *dpy;
    XKeyEvent *ke;
{
    WinGenericFrame *frameInfo = (WinGenericFrame *)CurrentClient->framewin;

    if (ke->type != KeyPress)
	return;

    if (frameInfo == NULL || (frameInfo->core.client->wmDecors->menu_type == MENU_NONE))
	KeyBeep(dpy, ke);
    else  {
	ShowStandardMenu(frameInfo, ke, False);
    }
}
