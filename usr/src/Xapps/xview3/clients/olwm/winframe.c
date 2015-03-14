/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)winframe.c	26.48	91/09/14 SMI"

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
#include "group.h"
#include "globals.h"

/***************************************************************************
* global data
***************************************************************************/

extern Atom AtomLeftFooter;
extern Atom AtomRightFooter;
extern Atom AtomTakeFocus;
extern Atom AtomDfltBtn;

/* REMIND - figure out how to get rid of this */
extern int Resize_height, Resize_width;

/***************************************************************************
* private data
***************************************************************************/

#define FRAME_EVENT_MASK        (ButtonPressMask | ButtonReleaseMask | \
                                 ExposureMask | ButtonMotionMask | \
                                 EnterWindowMask | LeaveWindowMask | \
                                 SubstructureRedirectMask | \
                                 FocusChangeMask | PropertyChangeMask)

/* REMIND rework this stuff so it can handle different point sizes */
#define FRAME_OUTLINE_WIDTH     2
#define FRAME_SELECTED_WIDTH    3  

static ClassPaneFrame classPaneFrame;

/***************************************************************************
* forward-declared functions
***************************************************************************/

static void setTitleText();
static void setFooterText();
void FrameUpdateShape();

/***************************************************************************
* sizing and decoration positioning functions
***************************************************************************/

/* decoration positioning */
/* ptSize - determine the point size we're working with.
 * REMIND this function is a hack which should be replaced with a
 * glyph font property.
 */
static int
ptSize(gis)
Graphics_info *gis;
{
#ifdef OBSOLETE
	switch (Resize_height)
	{
	case 10:
		return 10;
	case 11:
		return 12;
	case 12:
		return 14;
	case 14:
		return 19;
	default:
		return 10+((int)(0.5 * Resize_height));
	}
#endif
	return Pointsize_Glyph(gis);
}

static int
xposCloseButton(gis)
Graphics_info *gis;
{
	return ptSize(gis)+FRAME_OUTLINE_WIDTH;
}

static int
yposCloseButton(cli,gis)
Client *cli;
Graphics_info *gis;
{
    if (Abbrev_MenuButton_Height(gis) < headerHeight(cli,gis))
	return(ResizeArm_Height(gis)+(headerHeight(cli,gis)-Abbrev_MenuButton_Height(gis))/2);
    else
	return ResizeArm_Height(gis)+(ptSize(gis)>>4+2);
}

static int
xposPushPin(gis)
Graphics_info *gis;
{
	return xposCloseButton(gis);
}

static int
yposPushPin(cli,gis)
Client *cli;
Graphics_info *gis;
{
    if (PushPinOut_Height(gis) < headerHeight(cli,gis))
	return(ResizeArm_Height(gis)+(headerHeight(cli,gis)-PushPinOut_Height(gis))/2);
    else
	return ResizeArm_Height(gis)+(ptSize(gis)>>4);
}

static int
decoToTitle(gis)
Graphics_info *gis;
{
#ifdef NOTDEF
	return (2*xposCloseButton(gis))/3;
#endif
	return (ptSize(gis)>>2);
}

/* REMIND change this function to use olgx macros to extract
 * font size 
 */
static int
headerHeight(cli,gis)
Client *cli;
Graphics_info *gis;
{
	int fontht = GRV.TitleFontInfo->ascent 
		     + GRV.TitleFontInfo->descent;
	return MAX(Abbrev_MenuButton_Height(gis),fontht+3);
}

/* REMIND this function should also be changed to use olgx macros */
static int
footerHeight(cli,gis)
Client *cli;
Graphics_info *gis;
{
	return GRV.TitleFontInfo->ascent + GRV.TitleFontInfo->descent
	    + ResizeArm_Height(gis);
}


/* height/width functions */
static int
heightTopFrame(win)
WinPaneFrame *win;
{
	Client *cli = win->core.client;
	Graphics_info	*gisNormal = WinGI(win,NORMAL_GINFO);

	if (cli->wmDecors->flags & WMDecorationHeader)
		return 	headerHeight(cli,gisNormal) +
			2*ResizeArm_Height(gisNormal);
	else
		return ResizeArm_Height(gisNormal);
}

static int
heightBottomFrame(win)
WinPaneFrame *win;
{
	Client *cli = win->core.client;
	Graphics_info	*gisNormal = WinGI(win,NORMAL_GINFO);

	if (cli->wmDecors->flags & WMDecorationFooter)
		return footerHeight(cli,gisNormal) +
			ResizeArm_Height(gisNormal);
	else
		return ResizeArm_Height(gisNormal);
}

static int 
widthRightFrame(win)
WinPaneFrame *win;
{
	return ResizeArm_Width(WinGI(win,NORMAL_GINFO));
}

static int 
widthLeftFrame(win)
WinPaneFrame *win;
{
	return ResizeArm_Width(WinGI(win,NORMAL_GINFO));
}

/***************************************************************************
* event-handling functions
***************************************************************************/

/*
 * handle events for the frame
 *
 * The reader should be aware of the fact that both the titlebar
 * window and the frame window are affected when the window's
 * focus and select state is changed.  The window manager was
 * being written before the OpenLook spec. was completed so it is
 * not the most efficient design and is rather awkward in places.
 *
 * The way focusing is handled deserves some attention.  For a
 * detailed description of how focusing should be handled see
 * the Inter-Client Communication Conventions Manual.  I'll give
 * a rough overview below.
 *
 * A client can use one of four input models:  No Input, Passive,
 * Locally Active, Globally Active.  When OLWM is in focus-follows-
 * mouse mode, focus is handled in a fairly straightforward manner.
 * When the cursor enters a window, signaled by the frame getting
 * an EnterNotify event, OLWM sets the focus like this:
 *
 *      No Input        - Do nothing.
 *      Passive         - Set the focus using XSetInputFocus.
 *      Locally Active  - Set the focus using XSetInputFocus.
 *      Globally Active - Set the focus by sending a message
 *                        to the client.
 *
 * When OLWM is in click-to-focus mode, focus is a bit more complicated.
 * This is due to the fact that the user can press down in the decoration
 * around the client window and drag the window, and NOT set the focus.
 * If the user just clicks, without moving, in the decoration then we
 * set the focus.  So, we don't know whether to set the focus until
 * the button release event.  But, if the user presses down in the
 * client window we must set the focus immediately.  This is so the user
 * can go to an xterm which does not have the focus, press down, (which
 * sets the focus), move the cursor, and release the button to select
 * some text, for example.  If the client is Passive or Locally Active,
 * we have a passive grab on the SELECT button.  This is how we handle
 * setting the focus upon button press and release for the four input
 * modes:
 *
 *      ButtonPress
 *              No Input        - Do Nothing.
 *              Passive         - If the press was in the client,
 *                                set the focus
 *              Locally Active  - If the press was in the client,
 *                                set the focus
 *              Globally Active - Do Nothing.
 *
 *      ButtonRelease
 *              No Input        - Do Nothing.
 *              Passive         - Set the focus.  We only get here
 *                                if the button press was NOT in
 *                                the client.
 *              Locally Active  - Set the focus.  We only get here
 *                                if the button press was NOT in
 *                                the client.
 *              Globally Active - Send a message to the client.
 */

/*
 * eventMapRequest -- the pane is go from iconic to normal states
 */
static int
eventMapRequest(dpy, event, frameInfo)
Display	*dpy;
XEvent	*event;
WinPaneFrame *frameInfo;
{
	Client *cli = frameInfo->core.client;

        /* transition from Iconic or Withdrawn */
        if (cli->wmState == IconicState ) {
            StateIconNorm(cli);
        } 
#ifdef DEBUG
	else {
            ErrorWarning("ignoring MapRequest event on frame.");
        }
#endif /* DEBUG */
}

/*
 * eventConfigureRequest -- the pane is trying to change configuration
 */
static int
eventConfigureRequest(dpy, event, frameInfo)
Display	*dpy;
XEvent	*event;
WinPaneFrame *frameInfo;
{
	Client *cli = frameInfo->core.client;
	WinPane		*winPane = (WinPane*)frameInfo->fcore.panewin;

	ClientConfigure(cli,winPane,event);
}

/*
 * selectClickFrame -- the select button has been clicked
 */
static int
selectClickFrame(dpy, event, frameInfo)
Display	*dpy;
XEvent	*event;
WinPaneFrame *frameInfo;
{
	Client 	*cli = frameInfo->core.client;

        if (!GRV.FocusFollowsMouse)
        {
		ClientSetFocus(cli,True,event->xbutton.time);
        }  
}

/*
 * selectDoubleClickFrame -- the select button has been double-clicked
 */
static int
selectDoubleClickFrame(dpy, event, frameInfo)
Display	*dpy;
XEvent	*event;
WinPaneFrame *frameInfo;
{
    if (GRV.SelectToggleStacking)
	ClientBack(frameInfo->core.client);
    else
	ClientFullRestoreSizeToggle(frameInfo->core.client);
}

/*
 * selectDragFrame -- the select button has been pressed and moved enough
 * 	to trigger a drag.
 */
static int
selectDragFrame(dpy, event, frameInfo, lastpress)
Display *dpy;
XEvent *event;
WinPaneFrame *frameInfo;
XButtonEvent *lastpress;
{
	ClientMove(frameInfo->core.client,lastpress);
}

/*
 * menuPressFrame -- the menu button has been pressed
 */
static int
menuPressFrame(dpy,event,frameInfo)
Display *dpy;
XEvent *event;
WinPaneFrame *frameInfo;
{
    if (frameInfo->core.client->wmDecors->menu_type != MENU_NONE)
	ShowStandardMenu(frameInfo, event, False);
}

/*
 * selectPressFrame -- the select or adjust button has been pressed
 */
static int
selectAdjustPressFrame(dpy, event, frameInfo)
Display	*dpy;
XEvent	*event;
WinPaneFrame *frameInfo;
{
	Client *cli = frameInfo->core.client;

        /* If the button press was in the
         * client, set the input focus.
         */
	ClientSetFocus(cli,False,event->xbutton.time);
}


/*
 * adjustClickFrame -- the adjust button has been pressed
 */
static int
adjustClickFrame(dpy, event, frameInfo)
Display	*dpy;
XEvent	*event;
WinPaneFrame *frameInfo;
{
	Client *cli = frameInfo->core.client;

        ToggleSelection(cli, event->xbutton.time);
        if (!GRV.FocusFollowsMouse)
        {
		ClientSetFocus(cli,True,event->xbutton.time);
        }  
}

/*
 * eventEnterNotify
 *
 * If the pointer enters from the pane, turn off the warp-back flag.  Do this 
 * in addition to all the normal frame stuff done by GFrameEventEnterNotify.
 */
static int
eventEnterNotify(dpy, event, frameInfo)
Display	*dpy;
XEvent	*event;
WinPaneFrame *frameInfo;
{
    if (event->xcrossing.detail == NotifyInferior)
	frameInfo->pointerIsWarped = False;

    (void) GFrameEventEnterNotify(dpy, event, frameInfo);
}

/*
 * eventLeaveNotify
 * 
 * If we really left the window, the detail will be something other than
 * NotifyInferior.  If we are in focus-follows-mouse mode, this window loses 
 * the focus.  Also, unset the warp-back flag if the pointer has gone outside 
 * the frame.
 */
static int
eventLeaveNotify(dpy, event, frameInfo)
Display	*dpy;
XEvent	*event;
WinPaneFrame *frameInfo;
{
    /* ignore events caused by grabs */
    if (event->xcrossing.mode != NotifyNormal)
	return;

    if (event->xcrossing.detail != NotifyInferior)
	frameInfo->pointerIsWarped = False;
}


/*
 * drawHeaderBusy3D - draw header in busy state (3D mode)
 */
static void
drawHeaderBusy3D(dpy, win, cli, sel)
    Display *dpy;
    WinPaneFrame *win;
    Client *cli;
    Bool sel;
{
	Window self = win->core.self;
	int w = win->core.width;
	Graphics_info	*gisNormal = WinGI(win,NORMAL_GINFO);
	int armh = ResizeArm_Height(gisNormal);
	int armw = ResizeArm_Width(gisNormal);

	XFillRectangle(dpy, self, WinGC(win,WINDOW_GC), armw, armh, 
		w-2*armw, heightTopFrame(win)-armh);
	XFillRectangle(dpy, self, WinGC(win,BUSY_GC), armw, armh, 
		w-2*armw, heightTopFrame(win)-armh);
	olgx_draw_text(gisNormal, self, win->fcore.name, win->titlex, 
		win->titley, 0, OLGX_NORMAL);
}


/*
 * drawHeaderBusy2D - draw header in busy state (2D mode)
 */
static void
drawHeaderBusy2D(dpy, win, cli, sel)
    Display *dpy;
    WinPaneFrame *win;
    Client *cli;
    Bool sel;
{
	Window self = win->core.self;
	int w = win->core.width;
	int armh = ResizeArm_Height(WinGI(win,NORMAL_GINFO));
	int armw = ResizeArm_Width(WinGI(win,NORMAL_GINFO));

	/* fill in frame-colored area below titlebar */
	XFillRectangle(dpy, self, WinGC(win,WINDOW_GC), armw, armh, 
		w-2*armw, heightTopFrame(win)-armh);
	XFillRectangle(dpy, self, WinGC(win,BUSY_GC), armw, armh, 
		w-2*armw, heightTopFrame(win)-armh);

	/* fill in window name in titlebar */
	XDrawString(dpy, self, WinGC(win,FOREGROUND_GC), 
		win->titlex, win->titley,
		win->fcore.name, win->nameLength);
}


/*
 * drawHeaderBar3D - draw the header, with indented focus bar (3D mode)
 */
static void
drawHeaderBar3D(dpy, win, cli, sel)
Display *dpy;
WinPaneFrame *win;
Client *cli;
Bool sel;
{
	Window self = win->core.self;
	int w = win->core.width;
	Graphics_info	*gisNormal = WinGI(win,NORMAL_GINFO);
	int armh = ResizeArm_Height(gisNormal);

	XFillRectangle(dpy, self, WinGC(win,WINDOW_GC), 
		widthLeftFrame(win), heightTopFrame(win)-armh, 
		w-widthLeftFrame(win)-widthRightFrame(win), armh);

	olgx_draw_box(gisNormal, self, widthLeftFrame(win), armh, 
		w-widthLeftFrame(win)-widthRightFrame(win), 
		heightTopFrame(win)-(2*armh),
		OLGX_INVOKED, True);

	olgx_draw_text(gisNormal, self, win->fcore.name, win->titlex, 
		win->titley, 0, OLGX_INVOKED);
}


/*
 * drawHeaderLines3D - draw the header, with two focus lines (3D mode)
 */
static void
drawHeaderLines3D(dpy, win, cli, sel)
Display *dpy;
WinPaneFrame *win;
Client *cli;
Bool sel;
{
	Window self = win->core.self;
	int outlinewidth = sel?FRAME_SELECTED_WIDTH:FRAME_OUTLINE_WIDTH;
	int w = win->core.width;
	Graphics_info	*gisNormal = WinGI(win,NORMAL_GINFO);
	int armh = ResizeArm_Height(gisNormal);
	int armw = ResizeArm_Width(gisNormal);
	int lineleft, linelen;
static	Bool chiseledFocusLines = False;

	XFillRectangle(dpy, self, WinGC(win,WINDOW_GC), armw, armh, 
		w-2*armw, heightTopFrame(win)-armh);

	olgx_draw_text(gisNormal, self, win->fcore.name, win->titlex, 
		win->titley, 0, OLGX_NORMAL);

	if (cli->wmDecors->flags & WMDecorationResizeable) {
	    lineleft = Resize_width + 1;
	    linelen = w - (2*Resize_width) - 2;
	} else {
	    lineleft = widthLeftFrame(win);
	    linelen = w - widthLeftFrame(win) - widthRightFrame(win);
	}


	if (chiseledFocusLines) {
	    olgx_draw_box(gisNormal, self, lineleft, outlinewidth+1,
			  linelen, 2, OLGX_INVOKED, 0);

	    olgx_draw_box(gisNormal, self, widthLeftFrame(win),
			  heightTopFrame(win)-4,
			  w-widthLeftFrame(win)-widthRightFrame(win),
			  2, OLGX_INVOKED, 0);
	} else {
	    olgx_draw_text_ledge(gisNormal, self, lineleft, outlinewidth+1,
				 linelen);
	    olgx_draw_text_ledge(gisNormal, self, widthLeftFrame(win),
		    heightTopFrame(win)-3, 
		    w-widthLeftFrame(win)-widthRightFrame(win));
	}
}


/*
 * drawHeaderNoFocus3D - draw the header, without focus (3D mode)
 */
static void
drawHeaderNoFocus3D(dpy, win, cli, sel)
Display *dpy;
WinPaneFrame *win;
Client *cli;
Bool sel;
{
	Window self = win->core.self;
	int w = win->core.width;
	Graphics_info	*gisNormal = WinGI(win,NORMAL_GINFO);
	int armh = ResizeArm_Height(gisNormal);
	int armw = ResizeArm_Width(gisNormal);

	XFillRectangle(dpy, self, WinGC(win,WINDOW_GC), armw, armh, 
		w-2*armw, heightTopFrame(win)-armh);

	olgx_draw_text(gisNormal, self, win->fcore.name, win->titlex, 
		win->titley, 0, OLGX_NORMAL);
}


/* 
 * drawHeaderBar2D - draw the header, with inverted focus bar (2D mode)
 */
static void
drawHeaderBar2D(dpy, win, cli, sel)
Display *dpy;
WinPaneFrame *win;
Client *cli;
Bool sel;
{
	Window self = win->core.self;
	int w = win->core.width;
	int armh = ResizeArm_Height(WinGI(win,NORMAL_GINFO));

	/* draw frame-colored rectangle below titlebar box */
	XFillRectangle(dpy, self, WinGC(win,WINDOW_GC), 
		widthLeftFrame(win), heightTopFrame(win)-armh, 
		w-widthLeftFrame(win)-widthRightFrame(win), armh);

	/* draw black titlebar to indicate 2d focus (XFillRectangle uses
	 * foreground color for fill)
	 */
	XFillRectangle(dpy, self, WinGC(win,FOREGROUND_GC), 
		widthLeftFrame(win), armh-1, 
		w-widthLeftFrame(win)-widthRightFrame(win), 
		heightTopFrame(win)-(2*armh)+1);

	/* fill in window name */
	XDrawString(dpy, self, WinGC(win,WINDOW_GC), 
		win->titlex, win->titley,
		win->fcore.name, win->nameLength);
}


/*
 * drawHeaderLines2D - draw the header, with focus lines (2D mode)
 */
static void
drawHeaderLines2D(dpy, win, cli, sel)
Display *dpy;
WinPaneFrame *win;
Client *cli;
Bool sel;
{
	Window self = win->core.self;
	int outlinewidth = sel?FRAME_SELECTED_WIDTH:FRAME_OUTLINE_WIDTH;
	int w = win->core.width;
	Graphics_info	*gisNormal = WinGI(win,NORMAL_GINFO);
	int armh = ResizeArm_Height(gisNormal);
	int armw = ResizeArm_Width(gisNormal);
	GC	foregroundGC = WinGC(win,FOREGROUND_GC);

	/* fill in frame-colored area below titlebar area */
	XFillRectangle(dpy, self, WinGC(win,WINDOW_GC), armw, armh, 
		w-2*armw, heightTopFrame(win)-armh);

	/* fill in window name */
	XDrawString(dpy, self, foregroundGC, win->titlex, win->titley,
		win->fcore.name, win->nameLength);

	/* draw 2 pixel tall black focus indicator line above titlebar area 
	 * (without overwriting the resize corners)
	 */
	if (cli->wmDecors->flags & WMDecorationResizeable)
		XFillRectangle(dpy, self, foregroundGC, 
			Resize_width+1, outlinewidth+1, 
			w-(2*Resize_width)-2, 2);
	else
		XFillRectangle(dpy, self, foregroundGC, 
			widthLeftFrame(win), outlinewidth+1, 
			w-widthLeftFrame(win)-widthRightFrame(win), 2);

	/* draw 2 pixel tall black focus indicator line below titlebar area */
	XFillRectangle(dpy, self, foregroundGC, 
		widthLeftFrame(win), heightTopFrame(win)-3,
		w-widthLeftFrame(win)-widthRightFrame(win), 2);
}


/*
 * drawHeaderNoFocus2D - draw the header, without focus (2D mode)
 */
static void
drawHeaderNoFocus2D(dpy, win, cli, sel)
Display *dpy;
WinPaneFrame *win;
Client *cli;
Bool sel;
{
	Window self = win->core.self;
	int w = win->core.width;
	int armh = ResizeArm_Height(WinGI(win,NORMAL_GINFO));
	int armw = ResizeArm_Width(WinGI(win,NORMAL_GINFO));

	/* fill in frame-colored area below titlebar */
	XFillRectangle(dpy, self, WinGC(win,WINDOW_GC), armw, armh, 
		w-2*armw, heightTopFrame(win)-armh);

	/* fill in window name in titlebar */
	XDrawString(dpy, self, WinGC(win,FOREGROUND_GC), 
		win->titlex, win->titley,
		win->fcore.name, win->nameLength);
}


/* drawFooter - draw the footer
 */
static void
drawFooter(dpy, win, cli)
Display *dpy;
WinPaneFrame *win;
Client *cli;
{
	Window self = win->core.self;
	int w = win->core.width;
	int h = win->core.height;
	int fy = h-heightBottomFrame(win);
	Graphics_info	*gisNormal = WinGI(win,NORMAL_GINFO);
	int baseline = fy + GRV.TitleFontInfo->ascent +
	    ResizeArm_Height(gisNormal);
	int margin = FRAME_OUTLINE_WIDTH + ptSize(gisNormal);
	int footwidth = w - 2*margin;
	int qfootwidth = footwidth / 4;
	int gutter = ptSize(gisNormal);
	int rstart, lmaxwidth, rmaxwidth;

	/* fill in frame-colored area above footer */
	XFillRectangle(dpy, self, WinGC(win,WINDOW_GC), 
		widthLeftFrame(win), fy,
		w - widthLeftFrame(win) - widthRightFrame(win),
		footerHeight(cli, gisNormal));

	/* REMIND we don't paint the "more arrow" if text is truncated */

	if ((win->leftFooter.width + win->rightFooter.width + gutter)
	    <= footwidth) {
	    /* room for both: no clipping */
	    lmaxwidth = win->leftFooter.width;
	    rmaxwidth = win->rightFooter.width;
	} else if (win->rightFooter.width < qfootwidth) {
	    /* right footer takes less than 1/4 of the footer */
	    rmaxwidth = win->rightFooter.width;
	    lmaxwidth = footwidth - rmaxwidth - gutter;
	} else if
	    ((win->leftFooter.width) < (footwidth - qfootwidth - gutter)) {
	    /* left footer takes less than 3/4 of the footer */
	    lmaxwidth = win->leftFooter.width;
	    rmaxwidth = footwidth - lmaxwidth - gutter;
	} else {
	    /* must truncate both */
	    rmaxwidth = qfootwidth;
	    lmaxwidth = footwidth - qfootwidth - gutter;
	}
	rstart = w - margin - rmaxwidth;

	if (win->leftFooter.string)
	    olgx_draw_text(gisNormal, self, win->leftFooter.string,
			   margin, baseline, lmaxwidth, OLGX_NORMAL);

	if (win->rightFooter.string)
	    olgx_draw_text(gisNormal, self, win->rightFooter.string,
			   rstart, baseline, rmaxwidth, OLGX_NORMAL);
}


/* drawBase2D - draw the outer border of the window (2D mode)
 */
static void
drawBase2D(dpy, win, cli, sel)
Display *dpy;
WinPaneFrame *win;
Client *cli;
Bool sel;
{
	Window self = win->core.self;
	int lwidth = sel ? FRAME_SELECTED_WIDTH : FRAME_OUTLINE_WIDTH;
	int w = win->core.width;
	int h = win->core.height;
	int armh = ResizeArm_Height(WinGI(win,NORMAL_GINFO));
	int armw = ResizeArm_Width(WinGI(win,NORMAL_GINFO));
	GC	borderGC = WinGC(win,BORDER_GC);

	/* fill background with window color */

	XFillRectangle(dpy, self, WinGC(win,WINDOW_GC), lwidth, lwidth,
		       w-(2*lwidth), h-(2*lwidth));

	/* draw frame outline: top, bottom, left, right */

	XFillRectangle(dpy,self,borderGC,0,0,w,lwidth);
	XFillRectangle(dpy,self,borderGC,0,h-lwidth,w,lwidth);
	XFillRectangle(dpy,self,borderGC,0,lwidth,lwidth,h-(2*lwidth));
	XFillRectangle(dpy,self,borderGC,w-lwidth,lwidth,lwidth,h-(2*lwidth));
}

/* drawBase3D - draw the outer border of the window (3D mode)
 */
static void
drawBase3D(dpy, win, cli, sel)
Display *dpy;
WinPaneFrame *win;
Client *cli;
Bool sel;
{
	Graphics_info	*gisNormal = WinGI(win,NORMAL_GINFO);

	olgx_draw_box(gisNormal, win->core.self, 0, 0, 
			win->core.width, win->core.height, 
			OLGX_NORMAL, True);
	if (sel) {
	    olgx_draw_box(gisNormal, win->core.self, 1, 1,
			  win->core.width-2, win->core.height-2,
			  OLGX_NORMAL, False);
	}
}


/*
 * drawHeader - draw header appropriately, taking into consideration the 2D/3D 
 * style, busy state, selected state, and focus state.
 */
static void
drawHeader(dpy, winInfo, cli, sel)
    Display *dpy;
    WinPaneFrame *winInfo;
    Client *cli;
    Bool sel;
{
    void (*func)();

    if (cli->isBusy) {

	func = Win3D(winInfo) ? drawHeaderBusy3D : drawHeaderBusy2D;

    } else if (cli->isFocus) {

	/*
	 * Switch based on a value whose 1-bit indicates focus bar (if 
	 * zero) or focus lines (if one), and whose 2-bit indicates 2D (if 
	 * zero) or 3D (if one).
	 */
	switch (((GRV.FocusFollowsMouse ? 1 : 0) ^
		 (GRV.InvertFocusHighlighting ? 1 : 0)) |
		(Win3D(winInfo) ? 2 : 0))
	{
	    case 0:		func = drawHeaderBar2D;		break;
	    case 1:		func = drawHeaderLines2D;	break;
	    case 2:		func = drawHeaderBar3D;		break;
	    case 3:		func = drawHeaderLines3D;	break;
	}

    } else {
	func = Win3D(winInfo) ? drawHeaderNoFocus3D : drawHeaderNoFocus2D;
    }

    (*func)(dpy, winInfo, cli, sel);
}


/*
 * drawFrame -- draw the frame window
 */
/*ARGSUSED*/	/* dpy arg will be used when multiple Displays supported */
static int
drawFrame(dpy, winInfo)
Display	*dpy;
WinPaneFrame *winInfo;
{
    Client *cli = winInfo->core.client;

    if (Win3D(winInfo) && GRV.F3dFrames)
        drawBase3D(dpy, winInfo, cli, cli->isSelected);
    else
        drawBase2D(dpy, winInfo, cli, cli->isSelected);

    if (cli->wmDecors->flags & WMDecorationHeader)
	drawHeader(dpy, winInfo, cli, cli->isSelected);

    if (cli->wmDecors->flags & WMDecorationFooter)
        drawFooter(dpy, winInfo, cli); /* no difference between 2D and 3D */

}

/*
 * focusFrame - call global focus handler and redraw frame
 */
static int
focusFrame(dpy,winInfo,focus)
Display		*dpy;
WinPaneFrame	*winInfo;
Bool		focus;
{
	GFrameFocus(dpy,winInfo,focus);
	(WinFunc(winInfo,core.drawfunc))(dpy,winInfo);
}

/*
 * DestroyFrame -- destroy the frame window resources and free any allocated
 *	data.
 */
static int
destroyFrame(dpy, winInfo)
Display	*dpy;
WinPaneFrame *winInfo;
{
	/* free our data and throw away window */
	ListDestroy(winInfo->core.children);
	MemFree(winInfo->fcore.name);
	if (winInfo->leftFooter.string)
		XFree(winInfo->leftFooter.string);
	if (winInfo->rightFooter.string)
		XFree(winInfo->rightFooter.string);
	XUndefineCursor(dpy, winInfo->core.self);
	XDestroyWindow(dpy,winInfo->core.self);
	WIUninstallInfo(winInfo->core.self);
	MemFree(winInfo);
}

/*
 * newconfigFrame -- compute a new configuration of frame window
 */
static int
newconfigFrame(winInfo, pxcre)
WinPaneFrame *winInfo;
XConfigureRequestEvent *pxcre;
{
	Client *cli = winInfo->core.client;
	Display *dpy = cli->dpy;
	WinPane *winPane = (WinPane *)winInfo->fcore.panewin;
	int neww;
	int newh;
	WinGeneric *winDeco;
	Graphics_info	*gisNormal = WinGI(winPane,NORMAL_GINFO);

	neww = winInfo->fcore.panewin->core.width + widthLeftFrame(winInfo) + 
	    widthRightFrame(winInfo);
	newh = winInfo->fcore.panewin->core.height + heightTopFrame(winInfo) + 
	    heightBottomFrame(winInfo);

	if (neww != winInfo->core.width)
	{
		winInfo->core.width = neww;
		winInfo->core.dirtyconfig |= CWWidth;
		setTitleText(dpy,winInfo,winPane->core.self);
		setFooterText(dpy,winInfo,winPane->core.self);
	}

	if (newh != winInfo->core.height)
	{
		winInfo->core.height = newh;
		winInfo->core.dirtyconfig |= CWHeight;
	}
	
	if (winInfo->core.dirtyconfig)
	{
		(WinFunc(winPane,core.newposfunc))(winPane, widthLeftFrame(winInfo), 
			heightTopFrame(winInfo));
		winDeco = winInfo->winDeco;
	        if (cli->wmDecors->flags & WMDecorationPushPin)
       		{
		    (WinFunc(winDeco,core.newposfunc))(winDeco,
			xposPushPin(gisNormal),
			yposPushPin(cli,gisNormal));
        	}
        	if (cli->wmDecors->flags & WMDecorationCloseButton)
        	{
		    (WinFunc(winDeco,core.newposfunc))(winDeco,
			xposCloseButton(gisNormal),
			yposCloseButton(cli,gisNormal));
        	}
		if (cli->isBusy && winInfo->winBusy != NULL)
		    (WinFunc(winInfo->winBusy,core.newposfunc))(winInfo->winBusy, 
			widthLeftFrame(winInfo), heightTopFrame(winInfo));
	}

	return winInfo->core.dirtyconfig;
}


/*
 * setconfigFrame -- set new configuration for frame window
 */
static int
setconfigFrame(dpy, winInfo)
    Display *dpy;
    WinPaneFrame *winInfo;
{
    Client *cli = winInfo->core.client;

    FrameUpdateShape(cli, winInfo);
    GFrameSetConfigFunc(dpy, winInfo);
}


static int
selectFrame(dpy, winInfo, selected)
    Display	    *dpy;
    WinPaneFrame    *winInfo;
    Bool	    selected;
{
    FrameUpdateShape(winInfo->core.client, winInfo);
    GFrameSelect(dpy, winInfo, selected);
}


/* 
 * makeSpecials -- make any special mark windows (pushpin, close button)
 */
static void
makeSpecials(cli,dpy,wf,panewin,wid,high)
Client *cli;
Display *dpy;
WinPaneFrame *wf;
Window panewin;
int wid,high;
{
	int decorWidth = 0;
	Graphics_info	*gisNormal = WinGI(wf,NORMAL_GINFO);

	/* Make resize children */
	if (cli->wmDecors->flags & WMDecorationResizeable)
	{
		MakeResize(dpy, wf, upleft, 0, 0);
		MakeResize(dpy, wf, upright, wid-Resize_width, 0);
		MakeResize(dpy, wf, lowleft, 0, high-Resize_height);
		MakeResize(dpy, wf, lowright, wid-Resize_width, high-Resize_height);
	}

        /* Here we figure out, among other things, how much space
         * the decorations will take up in the title bar.  Also, we
         * set the leftmost point at which the title string can be
         * drawn without interfering with the decoration, if any,
         * on the left hand side of the title bar.  'frame->titleOff'
         *
	 * A window cannot have both a close button and a pushpin.  So, if
	 * they ask for both, they only get the pushpin.  This mutual
	 * exclusion was taken care of in GetOLWinDecors in states.c
         */
        if (cli->wmDecors->flags & WMDecorationPushPin)
        {
		wf->winDeco = (WinGeneric *)
			MakePushPin(dpy,wf,panewin,xposPushPin(gisNormal),
				    yposPushPin(cli,gisNormal));
                decorWidth = xposPushPin(gisNormal) + 
			PushPinOut_Width(gisNormal);
        }

        if (cli->wmDecors->flags & WMDecorationCloseButton)
        {
		wf->winDeco = (WinGeneric *)
			MakeButton(dpy,wf,xposCloseButton(gisNormal),
				   yposCloseButton(cli,gisNormal));
                decorWidth = xposCloseButton(gisNormal) +
			Abbrev_MenuButton_Width(gisNormal) ;
        }

	wf->titleOff = decorWidth + decoToTitle(gisNormal);
}

/* setTitleText - extract the name of the window and set up the titlebar
 * 	area
 */
static void
setTitleText(dpy,w,panewin)
Display *dpy;
WinPaneFrame *w;
Window panewin;
{
	int availwidth;
	char *ptr,*tmp;

        /* 	
	 * Get window name
	 */

	if (w->fcore.name)
		MemFree(w->fcore.name);

	if (!PropGetWMName(dpy,panewin,&(w->fcore.name))) {
	    w->fcore.name = MemNewString(GRV.DefaultWinName);
	}

	w->nameLength = strlen(w->fcore.name);
	w->nameWidth = XTextWidth(GRV.TitleFontInfo, w->fcore.name, w->nameLength);

#ifdef NOTDEF
	availwidth = w->core.width - widthLeftFrame(w) - widthRightFrame(w) - 
		w->titleOff;
#endif
	availwidth = w->core.width - widthRightFrame(w) - w->titleOff;
	availwidth = MAX(0,availwidth);

        if (availwidth < w->nameWidth)
        {
                /* Must truncate the title.
                 * First we see if there is a colon and truncate
                 * all the chars up to the colon.
                 */
                if (ptr = strchr(w->fcore.name, ':'))
                {
                        ptr++; /* after ':' */
                        w->nameLength -= ptr - w->fcore.name;
			tmp = w->fcore.name;
                        w->fcore.name = MemNewString(ptr);
			MemFree(tmp);
                        w->nameWidth = XTextWidth( GRV.TitleFontInfo, w->fcore.name, 
				w->nameLength);
                }
        }

        while (availwidth < w->nameWidth)
        {
                /* Truncate the title from the right. */
                w->fcore.name[strlen(w->fcore.name) - 1] = '\0';
                w->nameLength--;
                w->nameWidth = XTextWidth( GRV.TitleFontInfo, w->fcore.name, 
					   w->nameLength);
        }


        /* Center that title. */
        w->titlex = w->titleOff + (availwidth - w->nameWidth)/2;
        w->titley = GRV.TitleFontInfo->max_bounds.ascent + 2 + 
		ResizeArm_Height(WinGI(w,NORMAL_GINFO));
}

/*
 * calcFooterSize - figure footer size from string
 */
static void
calcFooterSize(footer)
	Footer	*footer;
{
	if (footer->string == NULL) {
		footer->width = footer->length = 0;
	} else {
		footer->length = strlen(footer->string);
		footer->width = XTextWidth(GRV.TitleFontInfo,
						footer->string,footer->length);
	}
}

/*
 * setLeftFooter - sets the left footer from the AtomLeftFooter property
 */
static void
setLeftFooter(dpy,winInfo,panewin)
	Display		*dpy;
	WinPaneFrame	*winInfo;
	Window		panewin;
{
	if (winInfo->leftFooter.string)
		MemFree(winInfo->leftFooter.string);

	if (!PropGetOLLeftFooter(dpy,panewin,&(winInfo->leftFooter.string)))
		winInfo->leftFooter.string = NULL;

	calcFooterSize(&(winInfo->leftFooter));
}

/*
 * setRightFooter - sets the left footer from the AtomRightFooter property
 */
static void
setRightFooter(dpy,winInfo,panewin)
	Display		*dpy;
	WinPaneFrame	*winInfo;
	Window		panewin;
{
	if (winInfo->rightFooter.string)
		MemFree(winInfo->rightFooter.string);

	if (!PropGetOLRightFooter(dpy,panewin,&(winInfo->rightFooter.string)))
		winInfo->rightFooter.string = NULL;

	calcFooterSize(&(winInfo->rightFooter));
}

/* 
 * setFooterText - set both left and right footers
 */
static void
setFooterText(dpy,w,panewin)
	Display 	*dpy;
	WinPaneFrame 	*w;
	Window 		panewin;
{
	setLeftFooter(dpy,w,panewin);
	setRightFooter(dpy,w,panewin);
}

/*
 * fullrestoreFrame
 */
static int
fullrestoreFrame(client)
Client	*client;
{
	WinPaneFrame	*frameInfo = client->framewin;
	WinPane		*paneInfo = (WinPane *)frameInfo->fcore.panewin;
	int		width,height;
	
	if (frameInfo->fcore.fullsize) {	/* going to restore/normal */
		/* restore from saved values */
		frameInfo->core.y = frameInfo->restoreY;
		frameInfo->core.dirtyconfig |= CWY;
		frameInfo->restoreSet = False;
		width = frameInfo->restoreWidth;
		height = frameInfo->restoreHeight;

	} else {				/* going to fullsize */
		/* save current size attributes */
		if (!frameInfo->restoreSet) {
			frameInfo->restoreWidth = paneInfo->core.width;
			frameInfo->restoreHeight = paneInfo->core.height;
			frameInfo->restoreY = frameInfo->core.y;
			frameInfo->restoreSet = True;
		}
	
		/* move to the top of the screen */
		frameInfo->core.y = 0;
		frameInfo->core.dirtyconfig |= CWY;

		/* if has specified a max size then use it */
		if ((client->normHints) &&
		    (client->normHints->flags & PMaxSize)) {
			width = client->normHints->max_width;
			height = client->normHints->max_height;

		/* else if has specified a resize increment then use it */
		} else if ((client->normHints) &&
		           (client->normHints->flags & PResizeInc)) {
			int	availHt,incr,incrHt,baseHt;

			if (client->normHints->flags & PBaseSize)
				baseHt = client->normHints->base_height;
			else
				baseHt = 0;
			incrHt = client->normHints->height_inc;
			availHt = DisplayHeight(client->dpy,client->screen) -
				heightTopFrame(frameInfo) - 
				heightBottomFrame(frameInfo) -
				baseHt;
			incr = availHt / incrHt;

			width = paneInfo->core.width;
			height = baseHt + ( incr * incrHt);

		/* else make it full screen height */
		} else {
			width = paneInfo->core.width;
			height = DisplayHeight(client->dpy,client->screen) -
				heightTopFrame(frameInfo) - 
				heightBottomFrame(frameInfo);
		}
	}

	/* Set the new pos/height */
	(WinFunc(paneInfo,pcore.setsizefunc))(paneInfo,width,height);
	WinCallConfig(client->dpy,paneInfo,NULL);

	frameInfo->fcore.fullsize = !frameInfo->fcore.fullsize;

}

/***************************************************************************
* global functions
***************************************************************************/

/*
 * FrameSetupGrabs
 *
 * Set up any pointer grabs for this window, as appropriate for the focus mode
 * (follow-mouse or click) and for the focus model (Passive, Globally Active,
 * etc.) of this window.  This is important for ClickFocus mode for Passive
 * and Locally Active clients.  If the user clicks over the pane window, we
 * get the event, set the focus, and replay the event, thus passing the event
 * through.
 *
 * The buttons are grabbed with owner-events True so that olwm's own 
 * subwindows (e.g. window buttons, resize corners) will get the event instead 
 * of the frame.
 * 
 * REMIND we need to remove explicit reference to Buttons 1 and 2.
 */
void
FrameSetupGrabs(cli, win, activate)
    Client *cli;
    Window win;
    Bool activate;
{
    if (!GRV.FocusFollowsMouse) {
	switch (cli->focusMode) {
	case Passive:
	case LocallyActive:
	    if (activate) {
		XGrabButton(cli->dpy, Button1, AnyModifier, win, True,
		    ButtonPressMask | ButtonReleaseMask | ButtonMotionMask,
		    GrabModeSync, GrabModeSync, None, None);
		XGrabButton(cli->dpy, Button2, AnyModifier, win, True,
		    ButtonPressMask | ButtonReleaseMask | ButtonMotionMask,
		    GrabModeSync, GrabModeSync, None, None);
	    } else {
		XUngrabButton(cli->dpy, Button1, AnyModifier, win);
		XUngrabButton(cli->dpy, Button2, AnyModifier, win);
	    }
	    break;
	}
    }
}


/*
 * FrameAllowEvents
 *
 * Issue an AllowEvents request if necessary.  If the client is Passive or 
 * Locally Active, and the focus mode is click-to-type, there is a passive, 
 * synchronous grab on the frame window.  If a subwindow receives an event but 
 * wants to ignore it, it must call this function in order to unfreeze the 
 * devices.
 */
void
FrameAllowEvents(cli, time)
    Client *cli;
    Time time;
{
    if (!GRV.FocusFollowsMouse) {
	switch (cli->focusMode) {
	case Passive:
	case LocallyActive:
	    XAllowEvents(cli->dpy, AsyncBoth, time);
	    break;
	}
    }
}


/*
 * MakeFrame  -- create the frame window. Return a WinPaneFrame structure.
 *	Note that unlike most Make functions, frames are not mapped right
 *	away.
 */
WinPaneFrame *
MakeFrame(cli,panewin,paneattrs)
Client *cli;
Window panewin;		
XWindowAttributes *paneattrs;
{
	Display *dpy = cli->dpy;
	WinPaneFrame *w;
	Window win;
        XSetWindowAttributes attributes;
        unsigned long   valuemask;
	int wid, high;

	/* create the frame window */
        attributes.event_mask = FRAME_EVENT_MASK;
        attributes.save_under =
	    paneattrs->save_under ||
	    (cli->transientFor != 0 && GRV.TransientsSaveUnder);
	attributes.background_pixmap = None;
	attributes.cursor = GRV.TargetPointer;
	attributes.border_pixel = 0;
	attributes.colormap = cli->scrInfo->colormap;
        valuemask = CWEventMask | CWSaveUnder | CWBackPixmap | CWCursor |
	    CWBorderPixel | CWColormap;

        win = XCreateWindow(dpy, cli->scrInfo->rootid,
                         0, 0, 1, 1,
                         0,
			 cli->scrInfo->depth,
                         InputOutput,
                         cli->scrInfo->visual,
                         valuemask,
                         &attributes);

	/* create the associated structure */
	w = MemNew(WinPaneFrame);
	w->class = &classPaneFrame;
	w->core.self = win;
	w->core.kind = WIN_FRAME;
	w->core.parent = NULL;
	w->core.children = NULL;
	w->core.client = cli;
	/* x and y set later */

	/* compute size of frame from pane */
	wid = paneattrs->width + widthLeftFrame(w) + widthRightFrame(w);
	high = paneattrs->height + heightTopFrame(w) + heightBottomFrame(w);

	w->core.width = wid;	/* these get fixed up at config time */
	w->core.height = high;
	w->core.stack_mode = Above;
	w->core.dirtyconfig = CWX | CWY | CWHeight | CWWidth | CWStackMode;
	w->core.colormap = None;
	w->core.exposures = NULL;
	w->core.helpstring = "olwm:Frame";

	/* REMIND this call appears to be redundant */
	FrameSetPosFromPane(w, paneattrs->x, paneattrs->y);

	cli->framewin = w;

	/* register the window */
	WIInstallInfo(w);

	/* if there's any special marks, make them */
	makeSpecials(cli,dpy,w,panewin,wid,high);

	/* set up the titlebar */
	if (cli->wmDecors->flags & WMDecorationHeader)
		setTitleText(dpy,w,panewin);

	/* set up the footer */
	if (cli->wmDecors->flags & WMDecorationFooter)
		setFooterText(dpy,w,panewin);


        /* Determine which menu should come up when menus are requested
         * for this frame. */

	FrameSetupGrabs(cli, win, True);

	/* set the full/normal size to transition to full size 
	 * on first activation */
	w->fcore.fullsize = False;;
	w->restoreSet = False;

	return w;
}

void
FrameInit(dpy)
Display *dpy;
{
	classPaneFrame.core.kind = WIN_FRAME;

	classPaneFrame.core.xevents[MapRequest] = eventMapRequest;
	classPaneFrame.core.xevents[ConfigureRequest] = eventConfigureRequest;
	classPaneFrame.core.xevents[Expose] = WinEventExpose;
	classPaneFrame.core.xevents[ButtonRelease] = GFrameEventButtonRelease;
	classPaneFrame.core.xevents[MotionNotify] = GFrameEventMotionNotify;
	classPaneFrame.core.xevents[ButtonPress] = GFrameEventButtonPress;
	classPaneFrame.core.xevents[EnterNotify] = eventEnterNotify;
	classPaneFrame.core.xevents[LeaveNotify] = eventLeaveNotify;
	classPaneFrame.core.xevents[FocusIn] = GFrameEventFocus;
	classPaneFrame.core.xevents[FocusOut] = GFrameEventFocus;

	classPaneFrame.core.focusfunc = focusFrame;
	classPaneFrame.core.drawfunc = drawFrame;
	classPaneFrame.core.destroyfunc = destroyFrame;
	classPaneFrame.core.selectfunc = selectFrame;
	classPaneFrame.core.newconfigfunc = newconfigFrame;
	classPaneFrame.core.newposfunc = WinNewPosFunc;
	classPaneFrame.core.setconfigfunc = setconfigFrame;
	classPaneFrame.core.createcallback = NULL;
	classPaneFrame.core.heightfunc = NULL;
	classPaneFrame.core.widthfunc = NULL;
	classPaneFrame.fcore.heighttop = heightTopFrame;
	classPaneFrame.fcore.heightbottom = heightBottomFrame;
	classPaneFrame.fcore.widthleft = widthRightFrame;
	classPaneFrame.fcore.widthright = widthLeftFrame;
	classPaneFrame.fcore.menuPress = menuPressFrame;
	classPaneFrame.fcore.adjustPress = selectAdjustPressFrame;
	classPaneFrame.fcore.adjustClick = adjustClickFrame;
	classPaneFrame.fcore.selectPress = selectAdjustPressFrame;
	classPaneFrame.fcore.selectClick = selectClickFrame;
	classPaneFrame.fcore.selectDoubleClick = selectDoubleClickFrame;
	classPaneFrame.fcore.selectDrag = selectDragFrame;
	classPaneFrame.fcore.fullrestoreToggle = fullrestoreFrame;
}

#ifdef NOTDEF
/* FrameSetStack -- set the frame's stacking position.   Does not initiate
 *	a configuration change.
 */
void
FrameSetStack(win, mask, mode, sib)
WinPaneFrame *win;
int mask;
int mode;
Window sib;
{
	WinGeneric *wsib;

	if ((mask & CWSibling) && (mask & CWStackMode))
	{
		wsib = WIGetInfo(sib);
		if (wsib != NULL)
		{
			win->core.stack_sib = wsib->core.client->framewin->core.self;
			win->core.dirtyconfig |= CWSibling;
		}
	}
	if (mask & CWStackMode)
	{
		win->core.stack_mode = mode;
		win->core.dirtyconfig |= CWStackMode;
	}
}
#endif /* NOTDEF */


/* FrameMoveRelative
 * Moves a frame by a delta in x and y
 */
void 
FrameMoveRelative(win,dx,dy)
WinPaneFrame *win;
int dx,dy;
{
	(WinFunc(win,core.newposfunc))(win,win->core.x+dx,win->core.y+dy);
}


/*
 * FrameSetPosAbsolute
 * The client is moving the pane to an absolute location on the screen, so we 
 * must move the frame accordingly.
 */
void
FrameSetPosAbsolute(win, x, y)
    WinPaneFrame *win;
    int x, y;
{
        int fx, fy;
	WinGenericPane *pane = win->fcore.panewin;
	int panebord = (pane == NULL)?(0):(pane->pcore.oldBorderWidth);

        fx = x - widthLeftFrame(win)+panebord;
        fy = y - heightTopFrame(win)+panebord;
        (WinFunc(win,core.newposfunc))(win, fx, fy);
}


/* FrameSetPosFromPane -- the client has specified a position for the pane.  
 * 	Using the window gravity, the frame's position should be adjusted
 * 	so that the point on the frame named by the window gravity is at the
 *	corresponding point in the requested pane.
 *	REMIND we aren't accounting for the window's border width here 
 */
void
FrameSetPosFromPane(win,x,y)
WinPaneFrame *win;
int x,y;
{
	int bw = widthLeftFrame(win)+widthRightFrame(win);
	int bh = heightTopFrame(win)+heightBottomFrame(win);
	WinGenericPane *pane = win->fcore.panewin;
	int panebord = (pane == NULL)?(0):(pane->pcore.oldBorderWidth);

	switch (win->core.client->normHints->win_gravity)
	{
	case NorthWestGravity:
		break;

	case NorthGravity:
		x -= bw/2-panebord;
		break;

	case NorthEastGravity:
		x -= bw-2*panebord;
		break;

	case WestGravity:
		y -= bh/2-panebord;
		break;

	case CenterGravity:
		y -= bh/2-panebord;
		x -= bw/2-panebord;
		break;

	case EastGravity:
		y -= bh/2-panebord;
		x -= bw-2*panebord;
		break;

	case SouthWestGravity:
		y -= bh-2*panebord;
		break;

	case SouthGravity:
		y -= bh-2*panebord;
		x -= bw/2-panebord;
		break;

	case SouthEastGravity:
		y -= bh-2*panebord;
		x -= bw-2*panebord;
		break;

	}
	(WinFunc(win,core.newposfunc))(win, x, y);
}

/* FrameUnparentPane
 *
 * Reparent the pane back to the root, moving the pane's position according to
 * the window gravity.  Also, restack the pane so that it has the same 
 * stacking order as the frame had.  This is important, so that windows on top 
 * remain on top after exiting the window manager.
 */
void
FrameUnparentPane(cli, winFrame, winPane)
Client *cli;
WinPaneFrame *winFrame;
WinPane *winPane;
{
	int x = winFrame->core.x;
	int y = winFrame->core.y;
	int bw = widthLeftFrame(winFrame)+widthRightFrame(winFrame);
	int bh = heightTopFrame(winFrame)+heightBottomFrame(winFrame);
	int panebord = winPane->pcore.oldBorderWidth;
	XWindowChanges xwc;

	switch (winFrame->core.client->normHints->win_gravity)
	{
	case NorthWestGravity:
		break;

	case NorthGravity:
		x += bw/2-panebord;
		break;

	case NorthEastGravity:
		x += bw-2*panebord;
		break;

	case WestGravity:
		y += bh/2-panebord;
		break;

	case CenterGravity:
		y += bh/2-panebord;
		x += bw/2-panebord;
		break;

	case EastGravity:
		y += bh/2-panebord;
		x += bw-2*panebord;
		break;

	case SouthWestGravity:
		y += bh-2*panebord;
		break;

	case SouthGravity:
		y += bh-2*panebord;
		x += bw/2-panebord;
		break;

	case SouthEastGravity:
		y += bh-2*panebord;
		x += bw-2*panebord;
		break;
	}

	XSetWindowBorderWidth(cli->dpy,winPane->core.self,winPane->pcore.oldBorderWidth);
        if (winPane->pcore.oldSaveUnder)
        {
            XSetWindowAttributes xwa;
            xwa.save_under = True;
            XChangeWindowAttributes(cli->dpy,winPane->core.self,CWSaveUnder,&xwa);
        }

	if (winPane->core.kind != WIN_MENU) {
	    XReparentWindow(cli->dpy, winPane->core.self, 
		cli->scrInfo->rootid, x, y);
	    XChangeSaveSet(cli->dpy, winPane->core.self, SetModeDelete);
	    if (GRV.RestackWhenWithdraw) {
		xwc.sibling = winFrame->core.self;
		xwc.stack_mode = Above;
		XConfigureWindow(cli->dpy, winPane->core.self,
			     	CWSibling|CWStackMode, &xwc);
	    }
	}
}

/*
 *  FrameUpdateHeader -- the header text has changed; update as appropriate
 */
/* ARGSUSED */
void
FrameUpdateHeader(cli,event)
	Client 		*cli;
	XPropertyEvent	*event;
{
	setTitleText(cli->dpy,cli->framewin,PANEWINOFCLIENT(cli));
	(WinFunc(cli->framewin,core.drawfunc))(cli->dpy, cli->framewin);
}

/* 
 * FrameUpdateFooter -- the footer text has changed
 */
void
FrameUpdateFooter(cli,event)
	Client 		*cli;
	XPropertyEvent	*event;
{
	if (event->atom == AtomLeftFooter) {
		setLeftFooter(cli->dpy,cli->framewin,PANEWINOFCLIENT(cli));
	} else if (event->atom == AtomRightFooter) {
		setRightFooter(cli->dpy,cli->framewin,PANEWINOFCLIENT(cli));
	}

	(WinFunc(cli->framewin,core.drawfunc))(cli->dpy, cli->framewin);
}

/* 
 * FrameFlashTitleBar -- flash the title bar 
 */
void
FrameFlashTitleBar(winInfo)
WinPaneFrame *winInfo;
{
	Client *cli = winInfo->core.client;
	Display *dpy = cli->dpy;
	int ii;
	void (*draw)(), (*undraw)();

	/* if no header, don't draw anything */
	if ((cli->wmDecors->flags & WMDecorationHeader) == 0)
	    return;

	/* set up draw/undraw procs */

	if (Win3D(winInfo)) {
	    undraw = drawHeaderNoFocus3D;
	    draw = drawHeaderBar3D;
	} else {
	    undraw = drawHeaderNoFocus2D;
	    draw = drawHeaderBar2D;
	}

	(*undraw)(dpy, winInfo, cli, cli->isSelected);

	for (ii=0; ii<GRV.FlashCount; ii++) {
		(*draw)(dpy, winInfo, cli, cli->isSelected);
		XFlush(dpy);
		olwm_usleep((unsigned)(GRV.FlashTime));
		(*undraw)(dpy, winInfo, cli, cli->isSelected);
		XFlush(dpy);
		olwm_usleep((unsigned)(GRV.FlashTime));
	}

	/* now redraw the header */
	drawHeader(dpy, winInfo, cli, cli->isSelected);
}

/* FrameSetBusy - change the frame's busy state.  The client's overall 
 *  	indication has already been set; create a busy window and manipulate
 *	the focus (if necessary).
 */
void
FrameSetBusy(win, newBusy)
WinPaneFrame *win;
Bool newBusy;
{
	if (newBusy)
	{
	    win->winBusy = MakeBusy(win->core.client->dpy, win);
	}
	else
	{
	    (WinFunc(win->winBusy,core.destroyfunc))(win->core.client->dpy, win->winBusy);
	}
	WinCallDraw(win);
}

/*
 * FrameWarpPointer - warp to pane windows' default button position if a
 * property is given; otherwise, if the window has a pushpin, warp it there.
 * This function can only be called AFTER the frame & pane are mapped.
 */
#define WARPINFO_LEN		6
void
FrameWarpPointer(cli)
Client		*cli;
{
	WinPaneFrame	*frameInfo = cli->framewin;
	WinPane		*paneInfo = (WinPane *) frameInfo->fcore.panewin;
	int		*warpParam;
        unsigned long 	nItems, remain;
	Window		root, child, dest;
	int		root_x, root_y, win_x;
	unsigned int	keys_buttons;
	int		offsetX, offsetY;
	Graphics_info	*gis = WinGI(frameInfo, NORMAL_GINFO);
	extern Client   *CurrentClient;

	frameInfo->pointerIsWarped = False;

	if (!GRV.PopupJumpCursor)
	    return;

	/* see if window pane has any warp info */
	warpParam = GetWindowProperty(cli->dpy, paneInfo->core.self,
	    AtomDfltBtn, 0L, WARPINFO_LEN, XA_INTEGER, 0, &nItems, &remain);
	
	if (warpParam != NULL && nItems != WARPINFO_LEN) {
	    XFree((char *) warpParam);
	    warpParam = NULL;
	}

	/*
	 * Warp to pin if warpParam non-NULL or WarpToPin is set.
	 */
	if (warpParam == NULL &&
	    !(cli->wmDecors->flags & WMDecorationWarpToPin))
	{
	    return;
	}

	if (warpParam == NULL) {

	    WinRootPos(frameInfo, &offsetX, &offsetY);

	    /* warp to the pushpin */
	    cli->warpInfo.dflButtonX = xposPushPin(gis);
	    cli->warpInfo.dflButtonY = yposPushPin(cli, gis);
	    cli->warpInfo.dflButtonW = PushPinOut_Width(gis);
	    cli->warpInfo.dflButtonH = PushPinOut_Height(gis);

	    cli->warpInfo.warpToX =
		cli->warpInfo.dflButtonX + PushPinOut_Width(gis) / 2;
	    cli->warpInfo.warpToY =
		cli->warpInfo.dflButtonY + PushPinOut_Height(gis) / 2;

	    dest = frameInfo->core.self;

	} else {

	    WinRootPos(paneInfo, &offsetX, &offsetY);

	    /* save warp destination information */
	    cli->warpInfo.warpToX = warpParam[0];
	    cli->warpInfo.warpToY = warpParam[1];
	    cli->warpInfo.dflButtonX = warpParam[2];
	    cli->warpInfo.dflButtonY = warpParam[3];
	    cli->warpInfo.dflButtonW = warpParam[4];
	    cli->warpInfo.dflButtonH = warpParam[5];
	    XFree((char *)warpParam);
	    dest = paneInfo->core.self;
	}

	/* translate to root coordinates */

	cli->warpInfo.dflButtonX += offsetX;
	cli->warpInfo.dflButtonY += offsetY;

	/* save warp return information */
	if (!XQueryPointer(cli->dpy, dest, &root, &child, &root_x, &root_y,
			   &win_x, &win_x, &keys_buttons))
	{
	    /* refuse to warp off the screen */
	    return;
	}

	cli->warpInfo.warpBackClient = CurrentClient;
	cli->warpInfo.warpBackX = root_x;
	cli->warpInfo.warpBackY = root_y;

	/* warp the pointer */
	XWarpPointer(cli->dpy,
		     None, dest,
		     0, 0, 0, 0,
		     cli->warpInfo.warpToX,
		     cli->warpInfo.warpToY);

	frameInfo->pointerIsWarped = True;

	/* finally, set the focus if necessary */
	if (!GRV.AutoInputFocus)
	    ClientSetFocus(cli, True, CurrentTime);
}

/*
 * FrameUnwarpPointer - called when a pane is unmapping, and the pointer
 * needs to be restored to its original position (if it was warped when the
 * window was initially mapped).
 */
void
FrameUnwarpPointer(cli)
Client		*cli;
{
	WinPaneFrame	*frameInfo = cli->framewin;

	if (frameInfo->pointerIsWarped) {
	    XWarpPointer(cli->dpy, cli->scrInfo->rootid, cli->scrInfo->rootid,
			 cli->warpInfo.dflButtonX, cli->warpInfo.dflButtonY,
			 cli->warpInfo.dflButtonW, cli->warpInfo.dflButtonH,
			 cli->warpInfo.warpBackX, cli->warpInfo.warpBackY);

	    if (cli->warpInfo.warpBackClient != NULL) {
		/*
		 * REMIND: set current client in order to prevent
		 * DestroyClient from setting focus again.  Should we use 
		 * ClientActivate instead?
		 */
		Client *warpcli = cli->warpInfo.warpBackClient;

		if (warpcli->framewin != NULL) {
		    ClientSetCurrent(cli->warpInfo.warpBackClient);
		    ClientSetFocus(cli->warpInfo.warpBackClient,
				   True, CurrentTime);
		}
	    }

	    /* invalidate the pointer warp info */
	    frameInfo->pointerIsWarped = False;
	}
}


/*
 * Update the frame's bounding shape, based on the client's bounding shape and
 * (if the window is selected) the resize corners.
 */
void
FrameUpdateShape(cli, winInfo)
    Client *cli;
    WinPaneFrame *winInfo;
{
#ifdef SHAPE
    if (cli->isShaped) {
	XShapeCombineShape(cli->dpy, winInfo->core.self, ShapeBounding,
			   widthLeftFrame(winInfo), heightTopFrame(winInfo),
			   winInfo->fcore.panewin->core.self,
			   ShapeBounding, ShapeSet);
	if (cli->isSelected &&
	    (cli->wmDecors->flags & WMDecorationResizeable))
	{
	    XRectangle rects[8];
	    Graphics_info *gi = WinGI(winInfo, NORMAL_GINFO);
	    int tw = Resize_width;		/* total width & height */
	    int th = Resize_height;
	    int aw = ResizeArm_Width(gi);	/* arm width & height */
	    int ah = ResizeArm_Height(gi);

	    /* Add in shapes for the resize corners, two rectangles each. */

	    /* upper left */

	    rects[0].x = 0;
	    rects[0].y = 0;
	    rects[0].width = tw;
	    rects[0].height = ah;

	    rects[1].x = 0;
	    rects[1].y = ah;
	    rects[1].width = aw;
	    rects[1].height = th - ah;

	    /* upper right */

	    rects[2].x = winInfo->core.width - tw;
	    rects[2].y = 0;
	    rects[2].width = tw;
	    rects[2].height = ah;

	    rects[3].x = winInfo->core.width - aw;
	    rects[3].y = ah;
	    rects[3].width = aw;
	    rects[3].height = th - ah;

	    /* lower left */

	    rects[4].x = 0;
	    rects[4].y = winInfo->core.height - th;
	    rects[4].width = aw;
	    rects[4].height = th - ah;

	    rects[5].x = 0;
	    rects[5].y = winInfo->core.height - ah;
	    rects[5].width = tw;
	    rects[5].height = ah;

	    /* lower right */

	    rects[6].x = winInfo->core.width - aw;
	    rects[6].y = winInfo->core.height - th;
	    rects[6].width = aw;
	    rects[6].height = th - ah;

	    rects[7].x = winInfo->core.width - tw;
	    rects[7].y = winInfo->core.height - ah;
	    rects[7].width = tw;
	    rects[7].height = ah;

	    XShapeCombineRectangles(cli->dpy, winInfo->core.self,
		ShapeBounding, 0, 0, rects, 8, ShapeUnion, Unsorted);
	}
    }
#endif
}


/*
 * Return the minimum width and height of this frame.
 */
void
FrameMinSize(frame, width, height)
    WinPaneFrame    *frame;
    int		    *width;		/* RETURN */
    int		    *height;		/* RETURN */
{
    int		    decors = frame->core.client->wmDecors->flags;
    Graphics_info   *gi = WinGI(frame, NORMAL_GINFO);
    int		    temp;

    /* calculate minimum height */

    temp = 2*ResizeArm_Height(gi);

    if (decors & WMDecorationResizeable)
	temp = MAX(temp, 2*Resize_height);

    if (decors & WMDecorationHeader)
	temp = MAX(temp, heightTopFrame(frame) + heightBottomFrame(frame));

    *height = temp;

    /* calculate minimum width */

    temp = 2*ResizeArm_Width(gi);

    if (decors & WMDecorationResizeable)
	temp = MAX(temp, 2*Resize_width);

    if (decors & WMDecorationCloseButton)
	temp = MAX(temp, 2*xposCloseButton(gi) + Abbrev_MenuButton_Width(gi));

    if (decors & WMDecorationPushPin)
	temp = MAX(temp, 2*xposPushPin(gi) + PushPinOut_Width(gi));

    *width = temp;
}
