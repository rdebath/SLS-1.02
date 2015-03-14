/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)moveresize.c	26.37	91/09/14 SMI"

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

#include "i18n.h"
#include "ollocale.h"
#include "mem.h"
#include "olwm.h"
#include "win.h"
#include "events.h"
#include "list.h"
#include "globals.h"

/* REMIND - find out how to get rid of this */
extern int Resize_width, Resize_height;

#ifdef ALLPLANES
extern int allplanes;
#endif


typedef enum {
    Unconstrained,		/* resizing not constrained */
    EitherConstrained,		/* constrained, but no direction yet */
    HorizConstrained,		/* constrained horizontally */
    VertConstrained,		/* constrained vertically */
} Constraint;


#define	DELTA_INCREASE		(1)
#define	DELTA_DECREASE		(-1)
#define	JUMP_INCREASE		(10)
#define	JUMP_DECREASE		(-10)

#define REDUCE_ROUNDER		5
#define REDUCE_DIVIDER		10

/*
 * drawDouble
 *
 * Draw a thick box on the given window, using the given GC.  The box is drawn
 * using four rectangles.  This technique is used instead of wide lines
 * because this routine is used during animation, and the wide line code of
 * some servers is too slow.
 */

#define defrect(r, X, Y, W, H) \
	(r).x = X, (r).y = Y, (r).width = W, (r).height = H

static void
drawDouble(dpy, win, gc, x, y, w, h)
    Display    *dpy;
    Window      win;
    GC          gc;
    int         x, y, w, h;
{
    register int thick = GRV.RubberBandThickness;
    XRectangle  rects[4];
    int		nrects,doublethick;

    if (w == 0 && h == 0)
	return;

    doublethick = 2 * thick;

    /* if too small for box just draw one solid rect */
    if (w <= doublethick || h <= doublethick) {
	defrect(rects[0], x, y, w, h);
	nrects = 1;
    /* else draw all 4 rects for the box */
    } else {
    	defrect(rects[0], x, y, w, thick);
    	defrect(rects[1], x, y + h - thick, w, thick);
    	defrect(rects[2], x, y + thick, thick, h - doublethick);
    	defrect(rects[3], x + w - thick, y + thick, thick, h - doublethick);
    	nrects = 4;
    }
#ifdef ALLPLANES
    if (allplanes)
	XAllPlanesFillRectangles(dpy, win, rects, nrects);
    else
#endif /* ALLPLANES */
	XFillRectangles(dpy, win, gc, rects, nrects);
}

#undef defrect


/* ===== status window ==================================================== */


#define HMARGIN 5
#define VMARGIN 3


typedef struct {
    Display	*dpy;
    Window	win;
    int		ypos;
    int		width;
    int		height;
    XFontStruct	*font;
    ScreenInfo	*scrinfo;
} StatusWinInfo;


static StatusWinInfo *
createStatusWindow(dpy, scrinfo, proto)
    Display *dpy;
    ScreenInfo *scrinfo;
    char *proto;
{
    int width, height;
    XSetWindowAttributes attr;
    StatusWinInfo *sw;

    sw = MemNew(StatusWinInfo);
    sw->dpy = dpy;
    sw->font = GRV.TitleFontInfo;
    sw->height = sw->font->ascent + sw->font->descent + 2*VMARGIN;
    sw->width = XTextWidth(sw->font, proto, strlen(proto)) + 2*HMARGIN;
    sw->scrinfo = scrinfo;

    attr.border_pixel = 0;
    attr.colormap = scrinfo->colormap;
    attr.save_under = True;
    sw->win = XCreateWindow(dpy, scrinfo->rootid, 
			    0, 0, sw->width, sw->height, 0,
			    scrinfo->depth, InputOutput, scrinfo->visual,
			    CWColormap | CWBorderPixel | CWSaveUnder, &attr);
    XMapRaised(dpy, sw->win);
    sw->ypos = VMARGIN + sw->font->ascent;
    return sw;
}


static void
paintStatusWindow(sw, string)
    StatusWinInfo *sw;
    char *string;
{
    if (sw == NULL)
	return;

    olgx_draw_box(sw->scrinfo->gi[NORMAL_GINFO], sw->win, 0, 0,
		  sw->width, sw->height, OLGX_NORMAL | OLGX_ERASE, True);
    
    XDrawString(sw->dpy, sw->win, sw->scrinfo->gc[FOREGROUND_GC], 
		(sw->width - XTextWidth(sw->font, string, strlen(string))) / 2,
		sw->ypos, string, strlen(string));
}


static void
destroyStatusWindow(sw)
    StatusWinInfo *sw;
{
    if (sw == NULL)
	return;

    XDestroyWindow(sw->dpy, sw->win);
    MemFree(sw);
}


/* ===== mouse-based window moving ======================================== */


typedef struct {
    Display		*dpy;
    int			initX, initY;
    int			offX, offY;
    int			curX, curY;
    int			rounder, divider;
    List		*winlist;
    WinGenericFrame	*frame;
    StatusWinInfo	*statuswindow;
    Constraint		constraint;
    Bool		dragwin;	    /* true=dragwin, false=dragframe */
    Bool		mouse;
} MoveClosure;


static Bool movewinInterposer();
static void *moveOneWindow();
static void *configOneWindow();
static void *drawOneBox();
static void moveDone();


static void
mouseMovePaintStatus(mstuff, x, y)
    MoveClosure *mstuff;
    int x, y;
{
    char buf[50];

    sprintf(buf, gettext("location: %d , %d"), x, y);
    paintStatusWindow(mstuff->statuswindow, buf);
}



/*
 * UserMoveWindows
 *
 * Allow the user to move a window or the set of selected windows.  The
 * "first" parameter must be the button event that initiated the interaction.  
 * The "winInfo" parameter must be the frame or icon on which the action was 
 * initiated.  The external boolean DragWindow controls whether the whole 
 * window is moved or whether the outline is moved.
 *
 * TODO:
 * (1) clean up coordinate systems;
 * (2) implement hysteresis against other windows' edges.
 */
void
UserMoveWindows(cli, first)
    Client *cli;
    XEvent *first;
{
    Display *dpy = cli->dpy;
    List *winlist = NULL_LIST;
    static MoveClosure mstuff;
    Time timestamp;

    mstuff.dpy = dpy;
    mstuff.offX = 0;
    mstuff.offY = 0;
    mstuff.constraint = Unconstrained;
    mstuff.rounder = 0;
    mstuff.divider = 1;

    if (cli->wmState == IconicState)
	mstuff.frame = (WinGenericFrame *) cli->iconwin;
    else
	mstuff.frame = (WinGenericFrame *) cli->framewin;

    if (first == NULL || first->type == KeyPress) {
	timestamp = (first == NULL) ? CurrentTime : first->xkey.time;
	mstuff.dragwin = False;
	mstuff.curX = mstuff.initX = mstuff.frame->core.x;
	mstuff.curY = mstuff.initY = mstuff.frame->core.y;
	mstuff.mouse = False;
    } else {
	/* it's a ButtonPress */

	mstuff.curX = mstuff.initX = first->xbutton.x_root;
	mstuff.curY = mstuff.initY = first->xbutton.y_root;
	mstuff.dragwin = GRV.DragWindow;
	mstuff.mouse = True;
	timestamp = first->xbutton.time;

	if (first->xbutton.state & ModMaskMap[MOD_INVERT])
	    mstuff.dragwin = !mstuff.dragwin;

	if (first->xbutton.state & ModMaskMap[MOD_REDUCE]) {
	    mstuff.rounder = REDUCE_ROUNDER;
	    mstuff.divider = REDUCE_DIVIDER;
	}

	if (first->xbutton.state & ModMaskMap[MOD_CONSTRAIN])
	    mstuff.constraint = EitherConstrained;
    }

    /*
     * Generate the list of windows to be moved.  If the initial window is 
     * selected, we're moving the selection; otherwise, we're moving just this 
     * window.
     */
    if (IsSelected(cli)) {
	Client *c = (Client *) 0;
	while (c = EnumSelections(c)) {
	    if (c->wmState == IconicState)
		winlist = ListCons(c->iconwin, winlist);
	    else
		winlist = ListCons(c->framewin, winlist);
	}
    } else {
	winlist = ListCons(mstuff.frame, NULL_LIST);
    }
    mstuff.winlist = winlist;

    /* Grab the pointer to change the cursor and confine to the root window. */

    if (XGrabPointer(dpy, cli->scrInfo->rootid, True,
		     ButtonPressMask | ButtonReleaseMask | PointerMotionMask,
		     GrabModeAsync, GrabModeAsync, cli->scrInfo->rootid,
		     GRV.MovePointer, timestamp) != GrabSuccess)
    {
	ErrorWarning(gettext("failed to grab pointer"));
	return;
    }

    if (XGrabKeyboard(dpy, cli->scrInfo->rootid, False,
		      GrabModeAsync, GrabModeAsync,
		      timestamp) != GrabSuccess)
    {
	/* note: not fatal */
	ErrorWarning(gettext("failed to grab keyboard"));
    }

    InstallInterposer(movewinInterposer, &mstuff);

    if (GRV.ShowMoveGeometry)
	mstuff.statuswindow = createStatusWindow(dpy, cli->scrInfo,
				gettext("location: 0000 , 0000"));
    else
	mstuff.statuswindow = NULL;

    /*
     * If we're dragging the outlines, we must also grab the server and draw 
     * the initial set of bounding boxes.
     */
    if (!mstuff.dragwin) {
	XGrabServer(dpy);
	(void) ListApply(mstuff.winlist, drawOneBox, &mstuff);
    }
}


void
moveUpdate(mstuff)
    MoveClosure *mstuff;
{
    if (!mstuff->dragwin)
	(void) ListApply(mstuff->winlist, drawOneBox, mstuff);

    mstuff->offX =
	(mstuff->curX - mstuff->initX + mstuff->rounder) / mstuff->divider;
    mstuff->offY =
	(mstuff->curY - mstuff->initY + mstuff->rounder) / mstuff->divider;

    if (mstuff->constraint == EitherConstrained) {
	if (ABS(mstuff->offX) > ABS(mstuff->offY))
	    mstuff->constraint = HorizConstrained;
	else
	    mstuff->constraint = VertConstrained;
    }

    if (mstuff->constraint == HorizConstrained) {
	mstuff->offY = 0;
    } else if (mstuff->constraint == VertConstrained) {
	mstuff->offX = 0;
    }

    if (mstuff->dragwin)
	(void) ListApply(mstuff->winlist, moveOneWindow, mstuff);
    else
	(void) ListApply(mstuff->winlist, drawOneBox, mstuff);
}


void
moveKeyDelta(mstuff, dx, dy)
    MoveClosure *mstuff;
    int dx, dy;
{
    mstuff->constraint = Unconstrained;

    if (mstuff->mouse) {
	mstuff->initX -= dx;
	mstuff->initY -= dy;
    } else {
	mstuff->curX += dx;
	mstuff->curY += dy;
    }
    moveUpdate(mstuff);
}


/*
 * movewinInterposer
 *
 * Interposer function for moving windows.  Moves the list of windows on each 
 * MotionNotify; releases interposition on ButtonRelease.
 */
/*ARGSUSED*/
static int
movewinInterposer(dpy, event, w, mstuff)
    Display *dpy;
    XEvent *event;
    WinGeneric *w;
    MoveClosure *mstuff;
{
    XEvent nextevent;
    SemanticAction action;

    switch (event->type) {
    case ButtonPress:
	/* ignore if buttons are already down */
	if (!FirstButtonDown(event))
	    break;

	switch (ResolveMouseBinding(dpy, event,
		    ModMaskMap[MOD_REDUCE] | ModMaskMap[MOD_CONSTRAIN]))
	{
	case ACTION_SELECT:
	    mstuff->mouse = True;
	    mstuff->curX = event->xbutton.x_root;
	    mstuff->curY = event->xbutton.y_root;
	    mstuff->initX = mstuff->curX - mstuff->offX;
	    mstuff->initY = mstuff->curY - mstuff->offY;

	    if (event->xbutton.state & ModMaskMap[MOD_REDUCE]) {
		mstuff->rounder = REDUCE_ROUNDER;
		mstuff->divider = REDUCE_DIVIDER;
	    }

	    if (event->xbutton.state & ModMaskMap[MOD_CONSTRAIN])
		mstuff->constraint = EitherConstrained;
	    break;

	default:	/* otherwise, abort the move operation */
	    mstuff->curX = mstuff->initX;
	    mstuff->curY = mstuff->initY;
	    moveUpdate(mstuff);
	    moveDone(mstuff);
	}
	break;

    case ButtonRelease:
	if (AllButtonsUp(event))
	    moveDone(mstuff);
	break;

    case MotionNotify:
	/* if the event is off the screen, ignore it */
        if (!event->xmotion.same_screen)
	    break;

	if (!mstuff->mouse)
	    break;

	/*
	 * Motion compression.  If the next event is a MotionNotify,
	 * ignore this one.
	 */
	if (XEventsQueued(dpy, QueuedAfterReading) > 0 &&
	    (XPeekEvent(dpy,&nextevent), nextevent.type == MotionNotify))
	    break;

	mstuff->curX = event->xmotion.x_root;
	mstuff->curY = event->xmotion.y_root;
	moveUpdate(mstuff);
	break;

    case KeyPress:
	action = FindKeyboardAction(dpy, event);

	switch (action) {
	case ACTION_UP:
	    moveKeyDelta(mstuff,0,DELTA_DECREASE);
	    break;
	case ACTION_DOWN:
	    moveKeyDelta(mstuff,0,DELTA_INCREASE);
	    break;
	case ACTION_LEFT:
	    moveKeyDelta(mstuff,DELTA_DECREASE,0);
	    break;
	case ACTION_RIGHT:
	    moveKeyDelta(mstuff,DELTA_INCREASE,0);
	    break;
	case ACTION_JUMP_UP:
	    moveKeyDelta(mstuff,0,JUMP_DECREASE);
	    break;
	case ACTION_JUMP_DOWN:
	    moveKeyDelta(mstuff,0,JUMP_INCREASE);
	    break;
	case ACTION_JUMP_LEFT:
	    moveKeyDelta(mstuff,JUMP_DECREASE,0);
	    break;
	case ACTION_JUMP_RIGHT:
	    moveKeyDelta(mstuff,JUMP_INCREASE,0);
	    break;
	case ACTION_EXEC_DEFAULT:
	    moveDone(mstuff);
	    break;
	case ACTION_STOP:
	    mstuff->curX = mstuff->initX;
	    mstuff->curY = mstuff->initY;
	    moveUpdate(mstuff);
	    moveDone(mstuff);
	    break;

	default:
	    if (FindModifierMask(event->xkey.keycode) ==
		ModMaskMap[MOD_CONSTRAIN])
	    {
		if (mstuff->mouse) {
		    mstuff->constraint = EitherConstrained;
		    moveUpdate(mstuff);
		}
	    } else {
		KeyBeep(dpy, event);
	    }
	    break;
	}
	break;

    case KeyRelease:
	if (FindModifierMask(event->xkey.keycode) ==
	    ModMaskMap[MOD_CONSTRAIN])
	{
	    mstuff->constraint = Unconstrained;
	    moveUpdate(mstuff, event->xkey.x_root, event->xkey.y_root);
	}
	break;

    /* Send out expose's immediately */
    case Expose:
	return DISPOSE_DISPATCH;

    default:
	return DISPOSE_DEFER;
    }

    return DISPOSE_USED;
}

/* windowOff -- function to determine how far a window should be offset
 * given a pointer offset.  Returns both X and Y, by reference.
 */
static void
windowOff(win, mstuff, pox, poy)
WinGenericFrame *win;
MoveClosure *mstuff;
int *pox, *poy;
{
    int pixw = DisplayWidth(win->core.client->dpy, win->core.client->screen);
    int hpoint;
    int pixy = DisplayHeight(win->core.client->dpy, win->core.client->screen);

    if (mstuff->offX == 0)
    {
	*pox = 0;
    }
    else if (mstuff->offX > 0)
    {
        hpoint = pixw - win->core.x - win->core.width;
    	if ((mstuff->offX >= hpoint) && (mstuff->offX <= hpoint + GRV.EdgeThreshold))
	    *pox = hpoint;
	else if (mstuff->offX >= hpoint+win->core.width-Resize_width)
	    *pox = hpoint+win->core.width-Resize_width;
	else
	    *pox = mstuff->offX;
    }
    else if (mstuff->offX < 0)
    {
    	if ((mstuff->offX <= -win->core.x) && (mstuff->offX >= (-win->core.x - GRV.EdgeThreshold)))
	    *pox = -win->core.x;
	else if (mstuff->offX <= (-win->core.x-win->core.width+Resize_width))
	    *pox = (-win->core.x-win->core.width+Resize_width);
	else
	    *pox = mstuff->offX;
    }

    if (mstuff->offY == 0)
    {
	*poy = 0;
    }
    else if (mstuff->offY > 0)
    {
        hpoint = pixy - win->core.y - win->core.height;
    	if ((mstuff->offY >= hpoint) && (mstuff->offY <= hpoint + GRV.EdgeThreshold))
	    *poy = hpoint;
	else if (mstuff->offY >= hpoint+win->core.height-Resize_height)
	    *poy = hpoint+win->core.height-Resize_height;
	else
	    *poy = mstuff->offY;
    }
    else if (mstuff->offY < 0)
    {
    	if ((mstuff->offY <= -win->core.y) && (mstuff->offY >= (-win->core.y - GRV.EdgeThreshold)))
	    *poy = -win->core.y;
	else if (mstuff->offY <= (-win->core.y-win->core.height+Resize_height))
	    *poy = (-win->core.y-win->core.height+Resize_height);
	else
	    *poy = mstuff->offY;
    }
}

/*
 * moveOneWindow
 *
 * Apply function for window moving animation.  Draws a window outline or 
 * actually moves the window, depending on DragWindow.
 */
static void *
moveOneWindow(win, mstuff)
    WinGenericFrame *win;
    MoveClosure *mstuff;
{
    int offX, offY;

    windowOff(win, mstuff, &offX, &offY);
    XMoveWindow(mstuff->dpy, win->core.self,
		win->core.x + offX,
		win->core.y + offY);
    if (win == mstuff->frame)
	mouseMovePaintStatus(mstuff, win->core.x + offX, win->core.y + offY);
    return (void *) 0;
}


/*
 * drawOneBox
 *
 * Apply function for drawing XOR boxes.  Draws a double-width rectangle 
 * around the outline of a single window.
 */
static void *
drawOneBox(w, mstuff)
    WinGenericFrame *w;
    MoveClosure *mstuff;
{
    int offX, offY;

    windowOff(w, mstuff, &offX, &offY);
    drawDouble(mstuff->dpy, w->core.client->scrInfo->rootid, WinGC(w,ROOT_GC),
	       w->core.x + offX, w->core.y + offY,
	       w->core.width, w->core.height);

    if (w == mstuff->frame)
	mouseMovePaintStatus(mstuff, w->core.x + offX, w->core.y + offY);

    return (void *) 0;
}    


/*
 * configOneWindow
 *
 * Apply function for calling a moved window's configfunc.
 */
static void *
configOneWindow(win, mstuff)
    WinGenericFrame *win;
    MoveClosure *mstuff;
{
    int offX, offY;

    windowOff(win, mstuff, &offX, &offY);
    GFrameSetConfig(win, win->core.x + offX, win->core.y + offY,
	win->core.width, win->core.height);
    return (void *) 0;
}


/*
 * moveDone
 *
 * Cleanup function for window moving.  Releases grabs, uninstalls 
 * interposition, cleans up.
 */
static void
moveDone(mstuff)
    MoveClosure *mstuff;
{
    /*
     * If we're dragging the outlines, we must ungrab the server and undraw 
     * the last set of boxes.
     */
    if (!mstuff->dragwin) {
	(void) ListApply(mstuff->winlist, drawOneBox, mstuff);
	XUngrabServer(mstuff->dpy);
    }
    UninstallInterposer();
    XUngrabPointer(mstuff->dpy, CurrentTime);
    XUngrabKeyboard(mstuff->dpy, CurrentTime);

    (void) ListApply(mstuff->winlist, configOneWindow, mstuff);
    ListDestroy(mstuff->winlist);

    destroyStatusWindow(mstuff->statuswindow);
}


/* ===== mouse-based resizing ============================================= */


/*
 * Note on use of gravity values: in this section, the gravity field is used 
 * to denote the window edge or corner that is being moved.  It's not
 * being used as "gravity" in the usual sense, which is the location that is 
 * being held constant.
 *
 * TODO:
 * (1) implement screen edge hysteresis for resize+move mode (meta key);
 * (2) implement window edge hysteresis.
 */

/*
 * The following enum is arranged specifically so that the values can be 
 * tested with bit operations.  The one-bit indicates down or right if one, up 
 * or left if zero.  The two-bit indicates vertical if one, horizontal if zero.
 * The four-bit indicates a jump if one, normal if zero.
 */
typedef enum {
    RS_LEFT = 0,
    RS_RIGHT,
    RS_UP,
    RS_DOWN,
    RS_J_LEFT,
    RS_J_RIGHT,
    RS_J_UP,
    RS_J_DOWN
} ResizeAction;

#define RS_ISRIGHT	    (1<<0)
#define RS_ISDOWN	    (1<<0)
#define RS_ISVERT	    (1<<1)
#define RS_ISJUMP	    (1<<2)

#define RS_JUMPMULT	    10			/* jump multiplier */

struct {
    int x, y;
} ResizeDeltas[] = {
    {           -1,            0 },	/* left */
    {            1,            0 },	/* right */
    {            0,           -1 },	/* up */
    {            0,            1 },	/* down */
    { -RS_JUMPMULT,            0 },	/* jump left */
    {  RS_JUMPMULT,            0 },	/* jump right */
    {            0, -RS_JUMPMULT },	/* jump up */
    {            0,  RS_JUMPMULT }	/* jump down */
};


typedef struct {
    Client		*cli;
    Constraint		constraint;
    Bool		drawn;
    Bool		moving;
    Bool		minAspect, maxAspect;
    Bool		baseProvided;
    int			origX, origY;
    int			curX, curY;	/* current mouse position */
    int			winX, winY;	/* current window position */
    int			winW, winH;	/* current window height */
    int			minW, minH;
    int			maxW, maxH;
    int			incW, incH;
    int			minAspectX, minAspectY;
    int			maxAspectX, maxAspectY;
    int			baseW, baseH;
    int			borderW, borderH;   /* size of frame border */
    void		(*callback)();
    void		*cbarg;
    StatusWinInfo	*statuswindow;
    int			gravity;		/* see note above */
    Bool		mouse;			/* using mouse? */
} ResizeClosure;


/*
 * Macro for adjusting the size of a window to its resize increment.  First,
 * assigns diff to be the difference between the size and the next lesser
 * incremental size.  If diff is greater than half the incremental
 * size, adjust the size upward to the next greater increment, otherwise 
 * adjust downward.  THIS MACRO ALTERS ITS FIRST ARGUMENT.
 *
 * size is the window size to be adjusted
 * base is base size, to be subtracted off before modulo is done
 * i is the incremental size
 */
#define INCADJ(size, base, i)						\
    {									\
	int diff;							\
	diff = ((size) - (base)) % (i);					\
	(size) += (diff > (i)/2) ? (i)-diff : -diff;			\
    }


static void
resizeDraw(rstuff)
    ResizeClosure *rstuff;
{
    drawDouble(rstuff->cli->dpy, WinRootID(rstuff->cli->framewin),
	       WinGC(rstuff->cli->framewin, ROOT_GC),
	       rstuff->winX, rstuff->winY, rstuff->winW, rstuff->winH);
}


static void
resizePaintStatus(rstuff)
    ResizeClosure *rstuff;
{
    char buf[30];
    int w, h;

    w = rstuff->winW - rstuff->borderW;
    h = rstuff->winH - rstuff->borderH;

    if (rstuff->baseProvided) {
	w -= rstuff->baseW;
	h -= rstuff->baseH;
    }
	
    sprintf(buf, gettext("size: %d x %d"), w / rstuff->incW, h / rstuff->incH);
    paintStatusWindow(rstuff->statuswindow, buf);
}


static void
resizeDone(dpy, e, w, rstuff, doit)
    Display *dpy;
    XEvent *e;
    WinGeneric *w;
    ResizeClosure *rstuff;
    Bool doit;
{
    if (rstuff->drawn)
	resizeDraw(rstuff);

    UninstallInterposer();
    XUngrabPointer(dpy, e->xbutton.time);
    XUngrabKeyboard(dpy, e->xbutton.time);
    XUngrabServer(dpy);

    if (doit)
	GFrameSetConfig(rstuff->cli->framewin, rstuff->winX, rstuff->winY,
			rstuff->winW, rstuff->winH);

    if (rstuff->callback != NULL)
	(*rstuff->callback)(dpy, rstuff->cbarg);

    destroyStatusWindow(rstuff->statuswindow);
}


static void
resizeMotion(rstuff, x, y)
    ResizeClosure *rstuff;
    int x, y;
{
    int dx, dy, dW, dH, tmpW, tmpH, newW, newH, idiff;
    unsigned long aspect;

    dx = x - rstuff->curX;
    dy = y - rstuff->curY;

    if (ABS(dx) <= GRV.MoveThreshold && ABS(dy) <= GRV.MoveThreshold &&
	!rstuff->drawn)
	return;
    
    if (rstuff->mouse) {
	if (rstuff->constraint == EitherConstrained) {
	    if (ABS(rstuff->origX - x) > ABS(rstuff->origY - y))
		rstuff->constraint = HorizConstrained;
	    else
		rstuff->constraint = VertConstrained;
	}

	if (rstuff->constraint == HorizConstrained) {
	    y = rstuff->origY;
	    dy = y - rstuff->curY;
	} else if (rstuff->constraint == VertConstrained) {
	    x = rstuff->origX;
	    dx = x - rstuff->curX;
	}
    }

    newW = rstuff->winW;
    newH = rstuff->winH;

    if (rstuff->moving) {
	if (dx == 0 && dy == 0)
	    return;
	if (rstuff->drawn)
	    resizeDraw(rstuff);
	rstuff->winX += dx;
	rstuff->winY += dy;
	rstuff->curX += dx;
	rstuff->curY += dy;
	resizeDraw(rstuff);
	rstuff->drawn = True;
	return;
    }

    switch (rstuff->gravity) {
    case NorthWestGravity:
	newW -= dx;
	newH -= dy;
	break;
    case NorthGravity:
	newH -= dy;
	break;
    case NorthEastGravity:
	newW += dx;
	newH -= dy;
	break;
    case WestGravity:
	newW -= dx;
	break;
    case CenterGravity:
	break;
    case EastGravity:
	newW += dx;
	break;
    case SouthWestGravity:
	newW -= dx;
	newH += dy;
	break;
    case SouthGravity:
	newH += dy;
	break;
    case SouthEastGravity:
	newW += dx;
	newH += dy;
	break;
    }

    /*
     * Convert from frame size to pane size, apply the constraints, then 
     * convert back to frame size.
     */

    newW -= rstuff->borderW;
    newH -= rstuff->borderH;

    INCADJ(newW, rstuff->baseW, rstuff->incW);
    INCADJ(newH, rstuff->baseH, rstuff->incH);

    newW = MAX(rstuff->minW, MIN(rstuff->maxW, newW));
    newH = MAX(rstuff->minH, MIN(rstuff->maxH, newH));

    if (rstuff->maxAspect &&
	newW * rstuff->maxAspectY > newH * rstuff->maxAspectX)
    {
	if (rstuff->maxAspectX > rstuff->maxAspectY) {
	    /* max aspect is wider than tall; increase height. */
	    newH = (newW * rstuff->maxAspectY) / rstuff->maxAspectX;
	    if (newH > rstuff->maxH) {
		newH = rstuff->maxH;
		newW = (newH * rstuff->maxAspectX) / rstuff->maxAspectY;
	    }
	} else {
	    /* max aspect is taller than wide; decrease width. */
	    newW = (newH * rstuff->maxAspectX) / rstuff->maxAspectY;
	    if (newW < rstuff->minW) {
		newW = rstuff->minW;
		newH = (newW * rstuff->maxAspectY) / rstuff->maxAspectX;
	    }
	}
    }

    if (rstuff->minAspect &&
	newW * rstuff->minAspectY < newH * rstuff->minAspectX)
    {
	if (rstuff->minAspectX > rstuff->minAspectY) {
	    /* min aspect is wider than tall; decrease height. */
	    newH = (newW * rstuff->minAspectY) / rstuff->minAspectX;
	    if (newH < rstuff->minH) {
		newH = rstuff->minH;
		newW = (newH * rstuff->minAspectX) / rstuff->minAspectY;
	    }
	} else {
	    /* min aspect is taller than wide; increase width. */
	    newW = (newH * rstuff->minAspectX) / rstuff->minAspectY;
	    if (newW > rstuff->maxW) {
		newW = rstuff->maxW;
		newH = (newW * rstuff->minAspectY) / rstuff->minAspectX;
	    }
	}
    }

    newW += rstuff->borderW;
    newH += rstuff->borderH;

    /*
     * Calculate the change in size (if any) and update the window's origin
     * (winX, winY) depending on which corner is being moved.  Also, update
     * the virtual pointer location (curX, curY).  Don't draw anything if the
     * size hasn't changed.
     */

    dW = newW - rstuff->winW;
    dH = newH - rstuff->winH;

    if (dW == 0 && dH == 0)
	return;

    if (rstuff->drawn)
	resizeDraw(rstuff);

    switch (rstuff->gravity) {
    case NorthWestGravity:
	rstuff->winX -= dW;
	rstuff->winY -= dH;
	rstuff->curX -= dW;
	rstuff->curY -= dH;
	break;
    case NorthGravity:
	rstuff->winX -= dW / 2;
	rstuff->winY -= dH;
	rstuff->curY -= dH;
	break;
    case NorthEastGravity:
	rstuff->winY -= dH;
	rstuff->curX += dW;
	rstuff->curY -= dH;
	break;
    case WestGravity:
	rstuff->winX -= dW;
	rstuff->winY -= dH / 2;
	rstuff->curX -= dW;
	break;
    case CenterGravity:
	rstuff->winX -= dW / 2;
	rstuff->winY -= dH / 2;
	break;
    case EastGravity:
	rstuff->winY -= dH / 2;
	rstuff->curX += dW;
	break;
    case SouthWestGravity:
	rstuff->winX -= dW;
	rstuff->curX -= dW;
	rstuff->curY += dH;
	break;
    case SouthGravity:
	rstuff->winX -= dW / 2;
	rstuff->curY += dH;
	break;
    case SouthEastGravity:
	rstuff->curX += dW;
	rstuff->curY += dH;
	break;
    }

    rstuff->winW = newW;
    rstuff->winH = newH;

    resizeDraw(rstuff);
    resizePaintStatus(rstuff);
    rstuff->drawn = True;
}


void
resizeDelta(rstuff, action)
    ResizeClosure *rstuff;
    ResizeAction action;
{
    int dx, dy;

    if (rstuff->mouse)
	return;

    if (action & RS_ISVERT) {
	switch (rstuff->gravity) {
	case WestGravity:
	case CenterGravity:
	case EastGravity:
	    rstuff->gravity += (action & RS_ISDOWN) ? 3 : -3;
	    break;
	}
    } else {
	switch (rstuff->gravity) {
	case NorthGravity:
	case CenterGravity:
	case SouthGravity:
	    rstuff->gravity += (action & RS_ISRIGHT) ? 1 : -1;
	    break;
	}
    }

    /* generate a delta vector based on which edge is being moved */

    dx = ResizeDeltas[action].x;
    dy = ResizeDeltas[action].y;

    dx *= rstuff->incW;
    dy *= rstuff->incH;

    resizeMotion(rstuff, rstuff->curX + dx, rstuff->curY + dy);
}


static int
resizeInterposer(dpy, e, w, rstuff)
    Display *dpy;
    XEvent *e;
    WinGeneric *w;
    ResizeClosure *rstuff;
{
    int dx, dy;
    unsigned int mask;
    SemanticAction action;

    switch (e->type) {
    case ButtonPress:
	if (!FirstButtonDown(e))
	    break;
	switch (ResolveMouseBinding(dpy, e,
		    ModMaskMap[MOD_CONSTRAIN] | ModMaskMap[MOD_INVERT]))
	{
	case ACTION_SELECT:
	    rstuff->mouse = True;
	    rstuff->curX = rstuff->origX = e->xbutton.x_root;
	    rstuff->curY = rstuff->origY = e->xbutton.y_root;
	    if (e->xbutton.state & ModMaskMap[MOD_CONSTRAIN])
		rstuff->constraint = EitherConstrained;
	    if (e->xbutton.state & ModMaskMap[MOD_INVERT])
		rstuff->moving = True;

	    rstuff->gravity = NorthWestGravity;
	    if (e->xbutton.y_root > rstuff->winY + (rstuff->winH / 2))
		rstuff->gravity = SouthWestGravity;
	    if (e->xbutton.x_root > rstuff->winX + (rstuff->winW / 2))
		rstuff->gravity += 2;	/* turns any west into any east */

	    resizeMotion(rstuff, e->xbutton.x_root, e->xbutton.y_root);
	    break;
	default:
	    resizeDone(dpy, e, w, rstuff, False);
	    break;
	}
	break;
	
    case ButtonRelease:
	if (AllButtonsUp(e))
	    resizeDone(dpy, e, w, rstuff, True);
	break;

    case MotionNotify:
	if (e->xmotion.same_screen && rstuff->mouse)
	    resizeMotion(rstuff, e->xmotion.x_root, e->xmotion.y_root);
	break;

    case KeyPress:
	action = FindKeyboardAction(dpy, e);

	switch (action) {

	case ACTION_STOP:
	    resizeDone(dpy, e, w, rstuff, False);
	    break;

	case ACTION_UP:		resizeDelta(rstuff, RS_UP);	    break;
	case ACTION_DOWN:	resizeDelta(rstuff, RS_DOWN);	    break;
	case ACTION_LEFT:	resizeDelta(rstuff, RS_LEFT);	    break;
	case ACTION_RIGHT:	resizeDelta(rstuff, RS_RIGHT);	    break;
	case ACTION_JUMP_UP:	resizeDelta(rstuff, RS_J_UP);	    break;
	case ACTION_JUMP_DOWN:	resizeDelta(rstuff, RS_J_DOWN);	    break;
	case ACTION_JUMP_LEFT:	resizeDelta(rstuff, RS_J_LEFT);	    break;
	case ACTION_JUMP_RIGHT:	resizeDelta(rstuff, RS_J_RIGHT);    break;

	case ACTION_EXEC_DEFAULT:
	    resizeDone(dpy, e, w, rstuff, True);
	    break;

	default:
	    mask = FindModifierMask(e->xkey.keycode);
	    if (mask == ModMaskMap[MOD_CONSTRAIN] && rstuff->mouse) {
		rstuff->constraint = EitherConstrained;
		resizeMotion(rstuff, e->xkey.x_root, e->xkey.y_root);
	    } else if (mask == ModMaskMap[MOD_INVERT]) {
		rstuff->moving = True;
	    } else {
		KeyBeep(dpy, e);
	    }
	    break;
	}
	break;

    case KeyRelease:
	mask = FindModifierMask(e->xkey.keycode);
	if (mask == ModMaskMap[MOD_CONSTRAIN] && rstuff->mouse) {
	    rstuff->constraint = Unconstrained;
	    resizeMotion(rstuff, e->xkey.x_root, e->xkey.y_root);
	} else if (mask == ModMaskMap[MOD_INVERT])
	    rstuff->moving = False;
	break;

    case Expose:
	return DISPOSE_DISPATCH;

    default:
	return DISPOSE_DEFER;
    }

    return DISPOSE_USED;
}



/*
 * Install an interposer for resizing with the mouse.
 */
void
UserResizeWin(cli, trigger, corner, callback, cbarg)
    Client *cli;
    XEvent *trigger;
    WhichResize corner;
    void (*callback)();
    void *cbarg;
{
    static ResizeClosure rstuff;
    XSizeHints *sh = cli->normHints;
    Window root = WinRootID(cli->framewin);
    Time timestamp;
    int frameMinWidth, frameMinHeight;

    if (trigger == NULL || trigger->type == KeyPress) {
	if (trigger == NULL)
	    timestamp = CurrentTime;
	else
	    timestamp = trigger->xkey.time;
	rstuff.mouse = False;
	rstuff.origX = rstuff.curX = cli->framewin->core.x;
	rstuff.origY = rstuff.curY = cli->framewin->core.y;
    } else {
	/* it's a button press */
	timestamp = trigger->xbutton.time;
	rstuff.mouse = True;
	if (trigger->xbutton.state & ModMaskMap[MOD_CONSTRAIN])
	    rstuff.constraint = EitherConstrained;
	else
	    rstuff.constraint = Unconstrained;

	if (trigger->xbutton.state & ModMaskMap[MOD_INVERT])
	    rstuff.moving = True;
	else
	    rstuff.moving = False;
	rstuff.origX = rstuff.curX = trigger->xbutton.x_root;
	rstuff.origY = rstuff.curY = trigger->xbutton.y_root;
    }

    switch (corner) {
    case upleft:	rstuff.gravity = NorthWestGravity;	break;
    case upright:	rstuff.gravity = NorthEastGravity;	break;
    case lowleft:	rstuff.gravity = SouthWestGravity;	break;
    case lowright:	rstuff.gravity = SouthEastGravity;	break;
    case keyevent:	rstuff.gravity = CenterGravity;		break;
    }

    /* Grab the pointer to change the cursor and confine to the root window. */
    if (XGrabPointer(cli->dpy, root, True,
		     ButtonPressMask | ButtonReleaseMask | PointerMotionMask,
		     GrabModeAsync, GrabModeAsync, root,
		     GRV.ResizePointer, timestamp) != GrabSuccess)
    {
	ErrorWarning(gettext("failed to grab pointer"));
	return;
    }

    if (XGrabKeyboard(cli->dpy, root, False,
		      GrabModeAsync, GrabModeAsync,
		      timestamp) != GrabSuccess)
    {
	/* note: not fatal */
	ErrorWarning(gettext("failed to grab keyboard"));
    }

    XGrabServer(cli->dpy);

    /* Fill in the closure for the interposer. */

    rstuff.drawn = False;
    rstuff.cli = cli;
    rstuff.winX = cli->framewin->core.x;
    rstuff.winY = cli->framewin->core.y;
    rstuff.winW = cli->framewin->core.width;
    rstuff.winH = cli->framewin->core.height;

    rstuff.callback = callback;
    rstuff.cbarg = cbarg;

    /*
     * Look at the client's size hints and update the closure appropriately.
     */

    rstuff.minW = rstuff.minH = 1;
    rstuff.maxW = rstuff.maxH = 32767; /* REMIND: max value of signed short */
    rstuff.incW = rstuff.incH = 1;
    rstuff.minAspect = rstuff.maxAspect = False;
    rstuff.baseW = rstuff.baseH = 0;

    if (sh != NULL) {
	if (sh->flags & PMinSize) {
	    rstuff.minW = sh->min_width;
	    rstuff.minH = sh->min_height;
	} else if (sh->flags & PBaseSize) {
	    rstuff.minW = MAX(1,sh->base_width);
	    rstuff.minH = MAX(1,sh->base_height);
	}

	if (sh->flags & PMaxSize) {
	    rstuff.maxW = sh->max_width;
	    rstuff.maxH = sh->max_height;
	}

	if ((sh->flags & PResizeInc) &&
	    sh->width_inc > 0 && sh->height_inc > 0)
	{
	    rstuff.incW = sh->width_inc;
	    rstuff.incH = sh->height_inc;
	}

	if (sh->flags & PAspect) {
	    if (sh->min_aspect.x > 0 && sh->min_aspect.y > 0) {
		rstuff.minAspect = True;
		rstuff.minAspectX = sh->min_aspect.x;
		rstuff.minAspectY = sh->min_aspect.y;
	    }
	    if (sh->max_aspect.x > 0 && sh->max_aspect.y > 0) {
		rstuff.maxAspect = True;
		rstuff.maxAspectX = sh->max_aspect.x;
		rstuff.maxAspectY = sh->max_aspect.y;
	    }
	}

	if (sh->flags & PBaseSize) {
	    rstuff.baseW = sh->base_width;
	    rstuff.baseH = sh->base_height;
	    rstuff.baseProvided = True;
	} else if (sh->flags & PMinSize) {
	    rstuff.baseW = sh->min_width;
	    rstuff.baseH = sh->min_height;
	    rstuff.baseProvided = False;
	}
    }

    /* figure in the size of the frame decorations */

    rstuff.borderW
	= FrameWidthLeft(cli->framewin) + FrameWidthRight(cli->framewin);
    rstuff.borderH
	= FrameHeightTop(cli->framewin) + FrameHeightBottom(cli->framewin);

    FrameMinSize(cli->framewin, &frameMinWidth, &frameMinHeight);
    frameMinWidth -= rstuff.borderW;
    frameMinHeight -= rstuff.borderH;
    rstuff.minW = MAX(rstuff.minW, frameMinWidth);
    rstuff.minH = MAX(rstuff.minH, frameMinHeight);

    /* map the geom window and draw the initial outline, if necessary */

    if (GRV.ShowResizeGeometry)
	rstuff.statuswindow = createStatusWindow(cli->dpy, cli->scrInfo,
						 gettext("size: 0000 x 0000"));
    else
	rstuff.statuswindow = NULL;

    if (trigger == NULL || trigger->type == KeyPress) {
	resizeDraw(rstuff);
	rstuff.drawn = True;
    }

    resizePaintStatus(&rstuff);

    InstallInterposer(resizeInterposer, &rstuff);
}


/* ===== root bounding box ================================================ */


typedef struct _rootboxclosure {
    int x0, y0;
    int x, y;
    unsigned int w, h;
    WinRoot *rootWin;
    Window rootID;
    GC rootGC;
    void *closure;
    void (*callback)();
} RootBoxClosure;


static int
rootBoxInterposer(dpy, event, w, rbc)
    Display *dpy;
    XEvent *event;
    WinGeneric *w;
    RootBoxClosure *rbc;
{
    XEvent nextevent;
    register int ex, ey;

    switch (event->type) {

    case ButtonPress:
	return DISPOSE_USED;

    case MotionNotify:
	/* if the event is off the screen, ignore it */
	if (event->xmotion.root != rbc->rootID)
	    return DISPOSE_USED;

	/*
	 * Motion compression.  If the next event is a MotionNotify,
	 * ignore this one.
	 */
	if (XEventsQueued(dpy, QueuedAfterReading) > 0 &&
	    (XPeekEvent(dpy, &nextevent), nextevent.type == MotionNotify))
	{
	    return DISPOSE_USED;
	}

	/* erase old box */
	drawDouble(dpy, rbc->rootID, rbc->rootGC,
		   rbc->x, rbc->y, rbc->w, rbc->h);

	/* update closure with new position */

	ex = event->xmotion.x_root;
	ey = event->xmotion.y_root;

	if (ex > rbc->x0) {
	    rbc->x = rbc->x0;
	    rbc->w = ex - rbc->x;
	} else {
	    rbc->x = ex;
	    rbc->w = rbc->x0 - rbc->x;
	}

	if (ey > rbc->y0) {
	    rbc->y = rbc->y0;
	    rbc->h = ey - rbc->y;
	} else {
	    rbc->y = ey;
	    rbc->h = rbc->y0 - rbc->y;
	}

	/* draw new box */

	drawDouble(dpy, rbc->rootID, rbc->rootGC,
		   rbc->x, rbc->y, rbc->w, rbc->h);
	return DISPOSE_USED;

    case ButtonRelease:
	if (!AllButtonsUp(event))
	    return DISPOSE_USED;
	break;

    case KeyPress:
	if (FindKeyboardAction(dpy, event) != ACTION_STOP) {
	    KeyBeep(dpy,event);
	    return DISPOSE_USED;
	}
	break;

    default:
	return DISPOSE_DEFER;
    }

    /*
     * erase outline, let go of server, call the callback
     */
    drawDouble(dpy, rbc->rootID, rbc->rootGC,
	       rbc->x, rbc->y, rbc->w, rbc->h);

    XUngrabServer(dpy);
    XUngrabPointer(dpy, CurrentTime);
    XUngrabKeyboard(dpy, CurrentTime);

    UninstallInterposer();

    (*rbc->callback)(dpy, rbc->rootWin, rbc->x, rbc->y, rbc->w, rbc->h,
		event->xbutton.time, rbc->closure);

    return DISPOSE_USED;
}


/*
 * TraceRootBox -- trace an XOR box with the initial point specified
 *	by pEvent, which is assumed to be a ButtonPress event.  Call the 
 *	callback function when done.
 */
void
TraceRootBox(dpy, winInfo, pEvent, callback, closure)
Display	*dpy;
WinRoot *winInfo;
XEvent	*pEvent;
void	(*callback)();
void	*closure;
{
	static RootBoxClosure rbc;
	Window rootID = WinRootID(winInfo);

	/* Change and confine the cursor. */
	if (XGrabPointer(dpy, rootID, True,
		ButtonReleaseMask | PointerMotionMask,
		GrabModeAsync, GrabModeAsync, rootID, GRV.BasicPointer,
		pEvent->xbutton.time) != GrabSuccess)
	{
	    ErrorWarning(gettext("failed to grab pointer"));
	    return;
	}

	if (XGrabKeyboard(dpy, rootID, False, GrabModeAsync, GrabModeAsync,
			  pEvent->xbutton.time) != GrabSuccess)
	{
	    /* note: not fatal */
	    ErrorWarning(gettext("failed to grab keyboard"));
	}

	rbc.x = rbc.x0 = pEvent->xbutton.x_root;
	rbc.y = rbc.y0 = pEvent->xbutton.y_root;
	rbc.w = rbc.h = 0;
	rbc.rootWin = winInfo;
	rbc.rootID = rootID;
	rbc.rootGC = WinGC(winInfo, ROOT_GC);
	rbc.callback = callback;
	rbc.closure = closure;

	/* Grab the server, then draw the initial outline. */
	XGrabServer(dpy);
	drawDouble(dpy, rootID, WinGC(winInfo, ROOT_GC),
		   rbc.x, rbc.y, 0, 0);

	InstallInterposer(rootBoxInterposer, &rbc);
	return;
}
