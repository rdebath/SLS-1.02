/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)win.c	26.12	91/09/14 SMI"

#include <stdio.h>
#include <memory.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#include "i18n.h"
#include "olwm.h"
#include "win.h"
#include "mem.h"
#include "st.h"
#include "notice.h"

/***************************************************************************
* local functions
***************************************************************************/

/*ARGSUSED*/
static enum st_retval
doRedrawOneWindow(w,win,c)
Window w;
WinGeneric *win;
void *c;
{
    if ((WinFunc(win,core.drawfunc) != NULL) && (win->core.client != NULL))
	(WinFunc(win,core.drawfunc))(win->core.client->dpy,win);
    return ST_CONTINUE;
}

static void
callSelectTree(dpy,win,sel)
Display *dpy;
WinGeneric *win;
Bool sel;
{
	List *l = win->core.children;

	if (WinFunc(win,core.selectfunc) != NULL)
		(*(WinFunc(win,core.selectfunc)))(dpy,win,sel);
	for (win = ListEnum(&l); win != NULL; win = ListEnum(&l))
		callSelectTree(dpy, win, sel);
}

/* Destroying a window tree:  walk the trees associated with a client, 
 * invoking the destroyfuncs on each.  The order that the windows are
 * destroyed is significant; it works from the root upwards.  The
 * client structure is not affected; it is assumed that this function
 * is only called from routines which take responsibility for removing
 * the client structure as well.
 */
static void
callDestroyTree(dpy,win)
Display *dpy;
WinGeneric *win;
{
	List *l;
	WinGeneric *cwin;

	if (win == NULL)
		return;
	l = win->core.children;
	for (cwin = ListEnum(&l); cwin != NULL; cwin = ListEnum(&l))
		callDestroyTree(dpy, cwin);
	if (WinFunc(win,core.destroyfunc) != NULL)
		(*(WinFunc(win,core.destroyfunc)))(dpy,win);
}

static void
setTreeConfig(dpy,win)
Display *dpy;
WinGeneric *win;
{
	List *l = win->core.children;
	WinGeneric *wc;

	(*(WinFunc(win,core.setconfigfunc)))(dpy,win);
	for (wc = ListEnum(&l); wc != NULL; wc = ListEnum(&l))
	{
		setTreeConfig(dpy,wc);
	}
}

static void
callFocusTree(win, focus)
WinGeneric *win;
Bool focus;
{
	List *l;
	WinGeneric *cwin;

	if (win == NULL)
		return;
	if (WinFunc(win,core.focusfunc) != NULL)
		(WinFunc(win,core.focusfunc))(win->core.client->dpy,win,focus);
	l = win->core.children;
	for (cwin = ListEnum(&l); cwin != NULL; cwin = ListEnum(&l))
		callFocusTree(cwin, focus);
}


static void
callDrawTree(win)
    WinGeneric *win;
{
    List *l;
    WinGeneric *cwin;

    if (WinFunc(win,core.drawfunc) != NULL)
	(WinFunc(win,core.drawfunc))(win->core.client->dpy,win);

    l = win->core.children;
    for (cwin = ListEnum(&l); cwin != NULL; cwin = ListEnum(&l))
	callDrawTree(cwin);
}


/***************************************************************************
* global functions
***************************************************************************/

/* WinCallSelect - call a client's select functions for either the icon or
 *   frame trees, depending on which is visible.  Passes along sel,
 *   which is True iff the client is being selected.
 */
void
WinCallSelect(cli, sel)
Client *cli;
Bool sel;
{
	if (cli->wmState == NormalState)
		callSelectTree(cli->dpy, cli->framewin, sel);
	else if (cli->wmState == IconicState)
		callSelectTree(cli->dpy, cli->iconwin, sel);
}

/* WinCallFocus - call a client's focus functions for the frame tree.
 *   Passes along focus, which is True iff the client is gaining focus
 */
void
WinCallFocus(win,focus)
WinGeneric *win;
Bool focus;
{
	if (win == NULL)
		return;
	win->core.client->isFocus = focus;
	callFocusTree(win, focus);
}
	
/* WinCallDestroy - call a client's destroy functions for both the icon and
 *   frame trees
 */
void
WinCallDestroy(cli)
Client *cli;
{
	Display *dpy = cli->dpy;
	WinPaneFrame *framewin = cli->framewin;
	WinIconFrame *iconwin = cli->iconwin;

	callDestroyTree(dpy, framewin);
	callDestroyTree(dpy, iconwin);
}

/* WinCallConfig - initiate a configuration change, starting at some
 *	particular window.
 *	Configuration change works as follows:  a window is the initiator
 *	of the change.  If the children of this window need to be sized,
 *	then they are called to do so; then the window sizes itself and
 *	sets the position of each of its children.  This process is
 *	repeated on the parent of the initiating window, and so on up
 *	to the top window in the hierarchy (presumably a frame).
 *	A second pass then occurs, doing a depth-first preorder 
 *	traversal of the window tree, performing the window's set
 *	configuration function to make the computed change.
 *	In the first pass, any window which changes its configuration 
 *	should return this fact to be propagated back to this routine;
 *	if no window has changed configuration then the second pass
 *	won't be performed.
 *	The initiator window is passed (in some cases) the configure
 *	request event which cause the action to begin.
 */
void
WinCallConfig(dpy,win,pxcre)
Display *dpy;
WinGeneric *win;
XConfigureRequestEvent *pxcre;
{
	Bool fDirty = False;
	WinGeneric *w;
	
	do
	{
		fDirty = (WinFunc(win,core.newconfigfunc))(win,pxcre) || fDirty;
		pxcre = NULL;
		w = win;
	}
	while ((win = win->core.parent) != NULL);
	if (fDirty)
	{
		setTreeConfig(dpy,w);
	}
}

/*
 * WinCallDraw
 * Call all child windows' draw functions.
 */
void
WinCallDraw(win)
    WinGeneric *win;
{
    callDrawTree(win);
}


/* WinAddChild -- add a child to a parent's list of children
 */
void
WinAddChild(parent,child)
WinGeneric *parent;
WinGeneric *child;
{
	parent->core.children = ListCons(child,parent->core.children);
	child->core.parent = parent;
}

/* WinRemoveChild -- remove a child from a parent's list of children
 */
void
WinRemoveChild(parent,child)
WinGeneric *parent;
WinGeneric *child;
{
	List **l;

	for (l = &(parent->core.children); *l != NULL; l = &((*l)->next))
	{
	    if ((WinGeneric *)((*l)->value) == child)
	    {
		ListDestroyCell(l);
		return;
	    }
	}
#ifdef DEBUG
	printf("Warning:  tried to remove child %x from parent %x, but it wasn't there\n",child,parent);
#endif
}

/* WinRootPos -- figure the root coordinates of a window's position
 */
void
WinRootPos(win,px,py)
WinGeneric *win;
int *px, *py;
{
	*px = 0;
	*py = 0;
	for ( ; win != NULL; win = win->core.parent)
	{
		*px += win->core.x;
		*py += win->core.y;
	}
}


/* WinRedrawAllWindows -- call every window's draw function (if provided)
 */
void
WinRedrawAllWindows()
{
	WIApply(doRedrawOneWindow, NULL);
}

/* WinShowHelp -- show help for that window if available
 */
Bool
WinShowHelp(dpy,win,mousex,mousey)
Display		*dpy;
WinGeneric	*win;
int		mousex,mousey;
{
	int	screen;
	char	*buttons[1];
	char	*msg;
	NoticeBox noticeBox;

	if (win->core.helpstring) {
		if (win->core.client)
			screen = win->core.client->screen;
		else
			screen = DefaultScreen(dpy);

		if (!ShowHelpWindow(screen,mousex,mousey,
		        win->core.helpstring)) {

			buttons[0] = gettext("Ok");
			msg = 
	gettext("Couldn't write to olwmslave\nNo Help Available for olwm");

			noticeBox.numButtons = NOTICE_BUTTON_COUNT(buttons);
			noticeBox.defaultButton = 0;
			noticeBox.buttonText = buttons;
			noticeBox.msgText = msg;
			noticeBox.boxX = -1;	/* centered */
			noticeBox.boxY = -1;
			(void)UseNoticeBox(dpy,screen,&noticeBox);
			return False;
		}
		return True;
	}
	return False;
}

/***************************************************************************
* general event/class functions
***************************************************************************/

int
WinDrawFunc(win)
WinGeneric *win;
{
	if ((WinFunc(win,core.drawfunc) != NULL) && (win->core.client != NULL))
		(WinFunc(win,core.drawfunc))(win->core.client->dpy,win);
}

int
WinEventExpose(dpy, event, win)
Display *dpy;
XEvent *event;
WinGeneric *win;
{
	if (event->xexpose.count == 0)
	    (WinFunc(win,core.drawfunc))(dpy, win);
}


int 
WinNewPosFunc(win,x,y)
WinGeneric *win;
int x,y;
{
    if (x != win->core.x)
    {
	win->core.x = x;
	win->core.dirtyconfig |= CWX;
    }

    if (y != win->core.y)
    {
	win->core.y = y;
	win->core.dirtyconfig |= CWY;
    }

    return win->core.dirtyconfig;
}


int
WinNewConfigFunc(win, pxcre)
WinGeneric *win;
XConfigureRequestEvent *pxcre;
{
	int neww = WinFunc(win,core.widthfunc)(win, pxcre);
	int newh = WinFunc(win,core.heightfunc)(win, pxcre);

	if (neww != win->core.width)
	{
	    win->core.width = neww;
	    win->core.dirtyconfig |= CWWidth;
	}
	if (newh != win->core.height)
	{
	    win->core.height = newh;
	    win->core.dirtyconfig |= CWHeight;
	}
	return win->core.dirtyconfig;
}


int
WinSetConfigFunc(dpy, win)
Display *dpy;
WinGeneric *win;
{
	XWindowChanges xwc;

	if (win->core.dirtyconfig)
	{
	    xwc.x = win->core.x;
	    xwc.y = win->core.y;
	    xwc.width = win->core.width;
	    xwc.height = win->core.height;
	    /* generic windows never change border or stacking */
	    XConfigureWindow(dpy,win->core.self,win->core.dirtyconfig,&xwc);
	}
	win->core.dirtyconfig &= ~(CWX|CWY|CWWidth|CWHeight);
}


/*
 * WinAddColorClient
 *
 * Add cli to this win's list of colormap clients.  Assumes that cli isn't 
 * already in the list.
 */
void
WinAddColorClient(win, cli)
    WinGeneric *win;
    Client *cli;
{
    win->core.colormapClients = ListCons(cli, win->core.colormapClients);
}


/*
 * WinRemoveColorClient
 *
 * Remove cli from this win's list of colormap clients.  If there are no more
 * clients, and this window is a WIN_COLORMAP, destroy the window.  Assumes
 * that cli appears in win's list zero or one times.
 */
void
WinRemoveColorClient(dpy, win, cli)
    Display *dpy;
    WinGeneric *win;
    Client *cli;
{
    List **l;

    l = &win->core.colormapClients;
    while (*l != NULL) {
	if ((*l)->value == cli) {
	    ListDestroyCell(l);
	    break;
	}
	l = &((*l)->next);
    }
    if (win->core.colormapClients == NULL_LIST
	&& win->core.kind == WIN_COLORMAP)
	(WinFunc(win, core.destroyfunc))(dpy, win);
}
