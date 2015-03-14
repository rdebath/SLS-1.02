/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ifndef _OLWM_WIN_H
#define _OLWM_WIN_H

#ident	"@(#)win.h	26.34	91/09/14 SMI"

#include "list.h"
#include "events.h"
#include "screen.h"

#ifdef SHAPE
#include <X11/extensions/shape.h>
#endif

/***************************************************************************
* Client state structures
***************************************************************************/

/* a type for the four possible focus modes */
typedef enum { Passive, NoInput, LocallyActive, GloballyActive } FocusMode;

typedef enum {
    MENU_FULL,     /* Close, Zoom, Props, Scale, Back, Refresh, Quit */
    MENU_LIMITED,  /* Dismiss, Scale, Refresh */
    MENU_NONE,
    MENU_ROOT,	   /* used only by usermenu.c */
    NUM_MENUS
} MenuIndex;

typedef struct {
        long flags;
	MenuIndex menu_type;
	int def_item;			/* default menu item */
        int pushpin_initial_state;
	int cancel;			/* 1 means Cancel instead of Dismiss */
	/* numbers assigned as per OLXCI 3/20/89 */
} WMDecorations;

/* pin states numbers, as per OLXCI 3/20/89 */
#define PIN_OUT 0
#define PIN_IN 1

/* value for flags */
#define WMDecorationHeader      (1L<<0)
#define WMDecorationFooter      (1L<<1)
#define WMDecorationPushPin     (1L<<2)
#define WMDecorationCloseButton (1L<<3)
#define WMDecorationHeaderDeco (WMDecorationPushPin | WMDecorationCloseButton)
#define WMDecorationResizeable  (1L<<4)
#define WMDecorationIconName	(1L<<5)
#define WMDecorationWarpToPin	(1L<<6)

typedef int WMState;	/* use state codes in Xutil.h */

typedef struct {	/* pointer warp state data */
	int		warpToX, warpToY;	/* where to pointer goes on warp */
	int		dflButtonX, dflButtonY;	/* location of default button */
	int		dflButtonW, dflButtonH;	/* size of default button */
	struct _client	*warpBackClient;	/* where to return focus */
	int		warpBackX, warpBackY;	/* where to put it back */
} WarpInfo;

/*
 *	Semantic window state
 */
typedef struct _windowstate {
	unsigned long	flags;
	unsigned long	state;
} OLWindowState;

/* value for OLWindowState flags */
#define WSSemanticState		(1L<<0)

/* value for OLWindowState state */
#define WSSemanticCompose	(1L<<0)
#define WSSemanticCapsLock	(1L<<1)
#define WSSemanticNumLock	(1L<<2)
#define WSSemanticScrollLock	(1L<<3)

/* client flags */
#define CLOlwmOwned		(1L<<0)

/***************************************************************************
* Client structures
***************************************************************************/

/* a top-level client window */

typedef struct _client
{
	int		flags;
	WMDecorations	*wmDecors;
	WMState		wmState;
	XSizeHints	*normHints;	/* WM_NORMAL_HINTS */
	XWMHints	*wmHints;	/* WM_HINTS */
	char		*wmInstance;	/* WM_CLASS instance name */
	char		*wmClass;	/* WM_CLASS class name */
	int 		protocols;
	FocusMode	focusMode;
	Bool		isFocus;
	Bool		isSelected;
	Bool		isBusy;
	Display		*dpy;
	int		screen;
	struct _winpaneframe *framewin;	
	struct _winiconframe *iconwin;
	WarpInfo	warpInfo;
	Window		groupid;	/* Actually GroupID */
	unsigned int	groupmask;	/* role that client is playing in group */
	long		busyState;
	List		*colormapWins;
	struct _screeninfo *scrInfo;
	OLWindowState	*windowState;
	Window		transientFor;
#ifdef SHAPE
	Bool		isShaped;
#endif
} Client;

#define ClientIsPinnable(cli) ((cli)->wmDecors->flags & WMDecorationPushPin)

/***************************************************************************
* Window class structures
***************************************************************************/

/* classes of window which we can deal with */
typedef enum { WIN_FRAME, WIN_ICON, WIN_RESIZE, WIN_PUSHPIN, WIN_WINBUTTON,
	WIN_PANE, WIN_ICONPANE, WIN_COLORMAP, WIN_MENU, WIN_PINMENU, 
	WIN_NOFOCUS, WIN_ROOT, WIN_BUSY } WinKind;

typedef int (*EvFunc)();
typedef int (*IntFunc)();
typedef struct _genericclasscore {
	WinKind kind;
	EvFunc xevents[LASTEvent];	/* function for each X event */
	EvFunc extEventHdlr;	/* handler for extension events */
	EvFunc focusfunc;	/* focus state has changed */
	EvFunc drawfunc;	/* draw window */
	EvFunc destroyfunc;	/* destroy window and structures */
	EvFunc selectfunc;	/* selecte state has changed */
	EvFunc newconfigfunc;  	/* compute configuration */
	EvFunc newposfunc;     	/* set position */
	EvFunc setconfigfunc;  	/* set configuration */
	EvFunc createcallback; 	/* used in menu creation */
	IntFunc heightfunc;	/* compute correct height */
	IntFunc widthfunc;	/* compute correct width */
} GenericClassCore;

typedef struct _classgeneric {
	GenericClassCore core;
} ClassGeneric;

typedef struct _genericframeclasscore {
	IntFunc heighttop, heightbottom, widthleft, widthright;
	IntFunc menuPress;
	IntFunc adjustPress, adjustClick;
	IntFunc selectPress, selectClick, selectDoubleClick, selectDrag;
	IntFunc fullrestoreToggle;
} GenericFrameClassCore;

typedef struct _classgenericframe {
	GenericClassCore core;
	GenericFrameClassCore fcore;
} ClassGenericFrame;

typedef struct _genericpaneclasscore {
	EvFunc setsizefunc;
} GenericPaneClassCore;

typedef struct _classpane {
	GenericClassCore core;
	GenericPaneClassCore pcore;
} ClassGenericPane;

typedef ClassGenericFrame ClassPaneFrame;
typedef ClassGenericFrame ClassIconFrame;
typedef ClassGeneric ClassPushPin;
typedef ClassGenericPane ClassPane;
typedef ClassGeneric ClassMenu;
typedef ClassGenericPane ClassPinMenu;
typedef ClassGeneric ClassResize;
typedef ClassGeneric ClassButton;
typedef ClassGeneric ClassBusy;
typedef ClassGenericPane ClassIconPane;
typedef ClassGeneric ClassRoot;
typedef ClassGeneric ClassNoFocus;
typedef ClassGeneric ClassColormap;

/* Core functions:
 *
 * destroyfunc - int (*destroyfunc)(Display *dpy, WinGeneric *win)
 *   Called when the window is being destroyed; should destroy any private
 *   resources associated with the window (including possibly destroying
 *   the X window) and destroy the window information structure.
 *
 * selectfunc - int (*selectfunc)(Display *dpy, WinGeneric *win, Bool selected)
 *   Called whenever the window is selected/deselected.  The window should
 *   update its private state -- most particularly, its screen appearance --
 *   to match the new state.  This function is only called when the selection
 *   state has changed.
 *
 * focusfunc - int (*focusfunc)(Display *dpy, WinGeneric *win, Bool focus)
 *   Called whenever the window gains/loses focus.  The window should update
 *   its private state -- most particularly, its screen appearance --
 *   to match the new state.  The window does _not_ need to take focus if
 *   it is active; this has already been taken care of.  This function is
 *   only called when the focus has changed.
 *
 * newconfigfunc - int (*newconfigfunc)(WinGeneric *win, 
 *			XConfigureRequestEvent *xcre)
 *   Called when the configuration of a window should be recomputed.  It
 *   can be assumed that all child windows will have already computed their
 *   sizes.  This function should compute the size of this window, and call
 *   the newposfunc of each child that needs to be moved.  If the configuration
 *   of the window is changed by this function, the dirtyconfig flag should be
 *   set.  The xcre parameter will be be set to a configure request event
 *   structure only if a configure request was received for this
 *   particular window; otherwise the parameter will be null.
 *   Returns True if any dirtyconfig flag has been set.
 *
 * newposfunc - int (*newposfunc)(WinGeneric *win, int x, y)
 *   Instructs a child window to move to a position (x,y).  If this position
 *   is different from the old position, then the dirtyconfig flag
 *   should be set.  Returns True if any dirtyconfig flag has been set.
 *
 * setconfigfunc - int (*setconfigfunc)(Display *dpy, WinGeneric *win)
 *   The window's configuration may have changed; if so, call XConfigureWindow
 *   to make the changes.  Clear the dirtyconfig bits in the process.
 *   The pane window will need to send a synthetic configure notify for
 *   any configuration call.
 *
 * createcallback - int (*createcallback)(WinGeneric *self, Client *cli, 
 *			WinGeneric *parent)
 *   For internally-created panes, the client and parent are filled out
 *   when the StateNew process would normally create a pane.
 *
 * heightfunc - int (*heightfunc)(WinGeneric *self, XConfigureRequestEvent *pxcre)
 *   should return the window's correct height, given a configure request
 *   event and the current environment.  Used to see if a window needs to 
 *   be resized.
 * 
 * widthfunc - same as heightfunc, except that it returns correct width
 */

/***************************************************************************
* Window instance structures
***************************************************************************/

/*
 * The wincore structure contains common information about each window.
 *
 * stack_mode, stack_sib, and dirtyconfig contain information pending window 
 * configuration changes that have not yet been sent to the server.
 *
 * exposures is a list of exposed rectangles that have not yet been repainted.
 *
 * colormapClients is a list of clients that have this window in their 
 * colormapWins list.  tag is used only while processing changes to a client's 
 * colormapWins list.
 */
typedef struct _wincore {
	Window			self;
	WinKind			kind;
	struct _wingeneric 	*parent;
	List			*children;
	Client			*client;
	int			x, y;
	unsigned int		width, height;
	int			stack_mode;
	Window			stack_sib;
	unsigned int		dirtyconfig;
	Colormap		colormap;
	List			*exposures;
	List			*colormapClients;
	Bool			tag;
	char			*helpstring;
} WinCore;
/* REMIND maybe add: cursor */

/* macros associated with a window core */
#define WinIsKind(w,k) 	((w)->core.kind == (k))
#define WinClass(w) 	((w)->class)
#define WinFunc(w,f) 	((w)->class->f)
#define	WinScreen(w)	((w)->core.client->screen);
#define WinRootID(w)	((w)->core.client->scrInfo->rootid)
#define WinDepth(w)	((w)->core.client->scrInfo->depth)
#define WinVisual(w)	((w)->core.client->scrInfo->visual)
#define WinGI(w,idx)	((w)->core.client->scrInfo->gi[(idx)])
#define WinGC(w,idx)	((w)->core.client->scrInfo->gc[(idx)])
#define Win3D(w)	((w)->core.client->scrInfo->use3D)

/*
 * These macros access the fields of the ColorMapFocus struct inside
 * the ScreenInfo struct of a Client.
 */
#define ColorFocusWindow(win) ((win)->core.client->scrInfo->cmapfocus.window)
#define ColorFocusClient(win) ((win)->core.client->scrInfo->cmapfocus.client)
#define	ColorFocusLocked(win) ((win)->core.client->scrInfo->cmapfocus.locked)


/* FrameCore defines fields common to all types of frame */
typedef struct _winframecore {
	struct _wingenericpane *panewin;/* pane inside frame */
	char 	*name;			/* name to be displayed on frame */
	Bool	fullsize;		/* if frame is full size */
} WinFrameCore;

/* PaneCore defines fields common to all types of panes */
typedef struct _winpanecore {
	int		oldBorderWidth;
	int		oldSaveUnder;
	int		pendingUnmaps;
} WinPaneCore;

/* Specific window types */

typedef struct _wingeneric {
	ClassGeneric	*class;
	WinCore		core;
} WinGeneric;

typedef struct _winroot {
	ClassGeneric	*class;
	WinCore		core;
	SemanticAction	currentAction;
	XEvent		buttonPressEvent;
} WinRoot;

typedef struct _wingenericframe {
	ClassGenericFrame *class;
	WinCore 	core;
	WinFrameCore	fcore;
} WinGenericFrame;

/* macros for generic frames */
#define FrameHeightTop(w) (WinFunc((w),fcore.heighttop))((w))
#define FrameHeightBottom(w) (WinFunc((w),fcore.heightbottom))((w))
#define FrameWidthLeft(w) (WinFunc((w),fcore.widthleft))((w))
#define FrameWidthRight(w) (WinFunc((w),fcore.widthright))((w))


typedef struct _wingenericpane {
	ClassGenericPane *class;
	WinCore core;
	WinPaneCore pcore;
} WinGenericPane;

typedef struct {
	char			*string;
	int			length,width;
} Footer;

typedef struct _winpaneframe {
	ClassPaneFrame 		*class;
	WinCore			core;
	WinFrameCore		fcore;

	/* resize corners */
	struct _winresize	*resizeCorner[4];

	/* footers */
	Footer			leftFooter;
	Footer			rightFooter;

	/* title fields */
	int			titleOff;	/* x offset of title area in pix */
	int			nameLength;	/* length of name in chars */
	int			nameWidth;	/* width of name in pix */
	int			titlex, titley;	/* title position */

	/* pointer warp status */
	Bool			pointerIsWarped;

	/* sizing functions */
	void 	(*normfullsizefunc)();	/* normal/full size function */
	Bool 	restoreSet;		/* True => restore values valid */
	int	restoreX, restoreY, 
		restoreWidth, restoreHeight;
					/* context for full/norm size 
					 * note that x/y is frame; w/h are pane
					 */

	/* other decorations */
	WinGeneric *winDeco;	/* window button or pushpin */
	struct _winbusy *winBusy; /* special window when busy */
} WinPaneFrame;

typedef struct _winiconframe {
	ClassIconFrame *class;
	WinCore		core;
	WinFrameCore	fcore;
	struct _iconSlot *iconslot;
	Bool		fManuallyPositioned;
	int		nameX, nameY;		/* name of icon */
	int		nameWidth;
	int		nameLength;
} WinIconFrame;

typedef struct _winiconpane {
	ClassIconPane 	*class;
	WinCore 	core;
	WinPaneCore 	pcore;
	Bool		iconClientWindow;	/* true iff client owns pane */
	Pixmap		iconPixmap;		/* None if icon has client window */
	Pixmap		iconMask;
} WinIconPane;

typedef struct _winpushpin {
	ClassPushPin *class;
	WinCore		core;
	Bool		pushpinin;
} WinPushPin;

typedef struct _winpane {
	ClassPane *class;
	WinCore 	core;
	WinPaneCore	pcore;
} WinPane;

typedef struct _winmenu {
	ClassMenu	*class;
	WinCore		core;
	struct _menuInfo *menuInfo;
	Bool		ignoreNextExpose;
} WinMenu;

typedef struct _winpinmenu {	/* pinned menus are a subclass of panes */
	ClassPinMenu 	*class;
	WinCore		core;
	WinPaneCore	pcore;
	struct _menuInfo *menuInfo;
} WinPinMenu;

typedef enum {upleft, upright, lowleft, lowright, keyevent} WhichResize;

typedef struct _winresize {
	ClassResize *class;
	WinCore		core;
	WhichResize	which;
	Bool		depressed;
} WinResize;

typedef struct _winbutton {
	ClassButton *class;
	WinCore		core;
} WinButton;

typedef struct _winbusy {
	ClassBusy *class;
	WinCore core;
	Bool isFocus;
} WinBusy;

typedef WinGeneric WinNoFocus;

/* colormap windows aren't linked into the ordinary window tree since they
 * are presumed under client control.  We create info structures for them 
 * only so appropriate events can be dispatched on them.  Colormap windows
 * are in a list on the client structure.
 */
typedef WinGeneric WinColormap;

/* functions mapping windows to infos */

extern void WIInstallInfo();
extern Bool WIUninstallInfo();
extern WinGeneric *WIGetInfo();
extern void WIApply();

/* window functions */
extern void WinAddChild();
extern void WinRemoveChild();

/* Useful client macros */
#define PANEOFCLIENT(cli) ((cli)->framewin->fcore.panewin)
#define PANEWINOFCLIENT(cli) (PANEOFCLIENT(cli)->core.self)

/* globals */
#ifdef SHAPE
extern Bool ShapeSupported;
extern int  ShapeEventBase;
extern int  ShapeErrorBase;
#endif

#endif /* _OLWM_WIN_H */
