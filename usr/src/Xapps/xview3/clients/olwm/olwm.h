/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ifndef _OLWM_OLWM_H
#define _OLWM_OLWM_H

#ident	"@(#)olwm.h	26.19	91/09/14 SMI"

#ifndef ABS
#define ABS(a)		(((a) < 0) ? -(a) : (a))
#endif

#ifndef MAX
#define	MAX(a,b)	(((a) > (b)) ? (a) : (b))
#endif

#ifndef MIN
#define MIN(a,b)        ((a) < (b) ? (a) : (b))
#endif

/* Determine the size of an object type in 32bit multiples.
 * Rounds up to make sure the result is large enough to hold the object. */
#define LONG_LENGTH(a)	((long)(( sizeof(a) + 3 ) / 4))

#define	FOOTLEN	50L

/* protocols bits */
#define		TAKE_FOCUS		(1<<0)
#define		SAVE_YOURSELF		(1<<1)
#define		DELETE_WINDOW		(1<<2)

/* Icon positioning modes */
typedef enum { AlongTop, AlongBottom, AlongRight, AlongLeft,
	       AlongTopRL, AlongBottomRL, AlongRightBT, AlongLeftBT
	     } IconPreference;

/* size of icon window */
#define ICON_WIN_WIDTH 60
#define ICON_WIN_HEIGHT 60
#define ICON_GRID_WIDTH 13
#define ICON_GRID_HEIGHT 13

/* min/max/inc icon sizes */
#define ICON_MIN_WIDTH 		1
#define ICON_MIN_HEIGHT 	1
#define ICON_MAX_WIDTH		128
#define ICON_MAX_HEIGHT		128
#define ICON_WIDTH_INC		1
#define ICON_HEIGHT_INC		1

/* minimum window size */
#define MINSIZE 5

/* adornment pixmaps */
extern	Pixmap	pixIcon;
extern	Pixmap	pixmapGray;
extern	Pixmap	pixGray;

/* miscellaneous functions */
extern int ExitOLWM();
extern void *GetWindowProperty();

/* state functions */
extern struct _client *StateNew();
extern void ReparentTree();
extern void StateNormIcon();
extern void StateIconNorm();
extern void StateWithdrawn();

/* root window functions */
extern struct _winroot *MakeRoot();

/* no-focus window information and functions */
extern Window NoFocusWin;
extern struct _wingeneric *NoFocusWinInfo;

extern struct _wingeneric *MakeNoFocus();
extern void NoFocusTakeFocus();
extern void NoFocusInit();
extern int NoFocusEventBeep();

/* client information and functions */
extern struct _List *ActiveClientList;

extern struct _client *ClientCreate();
extern Window ClientPane();
typedef struct _clientinboxclose {
	Display *dpy;
	int 	screen;
	int 	(*func)();
	short 	bx, by, bw, bh;
	Time 	timestamp;
} ClientInBoxClosure;
extern void *ClientInBox();
extern void ClientInhibitFocus();
extern void ClientSetFocus();
extern void ClientSetCurrent();
extern struct _client *ClientGetLastCurrent();
extern void ClientActivate();
extern void ClientFocusTopmost();

/* frame functions */
extern struct _winpaneframe *MakeFrame();
extern void FrameSetPosFromPane();
extern void FrameFullSize();
extern void FrameNormSize();
extern void FrameNewFooter();
extern void FrameNewHeader();
extern void FrameSetBusy();
extern void FrameWarpPointer();
extern void FrameUnwarpPointer();

/* generic frame functions */
extern int GFrameFocus();
extern int GFrameSelect();
extern int GFrameSetConfigFunc();
extern void GFrameSetStack();
extern void GFrameSetConfig();
extern int GFrameEventButtonPress();
extern int GFrameEventMotionNotify();
extern int GFrameEventButtonRelease();
extern int GFrameEventFocus();
extern int GFrameEventEnterNotify();

/* icon functions */
extern void IconInit();
extern struct _winiconframe *MakeIcon();
extern void IconChangeName();
extern void DrawIconToWindowLines();
extern void IconShow();
extern void IconHide();
extern void IconSetPos();

/* icon pane functions */
extern struct _winiconpane *MakeIconPane();

/* pane functions */
extern struct _winpane *MakePane();

/* pinned menu functions */
extern struct _winmenu *MakeMenu();

/* colormap functions */
extern struct _wingeneric *MakeColormap();
extern void TrackSubwindows();
extern void UnTrackSubwindows();
extern void ColormapInhibit();
extern void InstallColormap();
extern void InstallPointerColormap();
extern void UnlockColormap();
extern void ColorWindowCrossing();
extern struct _wingeneric *ColormapUnhook();
extern void ColormapTransmogrify();

/* selection functions */
extern Bool IsSelected();
extern struct _client *EnumSelections();
extern Time TimeFresh();
extern int AddSelection();
extern Bool RemoveSelection();
extern Bool ToggleSelection();
extern void ClearSelections();
extern void SelectionResponse();

/* decoration window functions */
extern struct _winpushpin *MakePushPin();
extern struct _winbutton *MakeButton();

/* general window functions */
extern void WinCallFocus();
extern void WinRedrawAllWindows();
extern Bool WinShowHelp();

/* general window event functions */
extern int WinEventExpose();
extern int WinNewPosFunc();
extern int WinNewConfigFunc();
extern int WinSetConfigFunc();

/* rubber-banding functions */
extern void UserMoveWindows();
extern void UserResizeWin();
extern void TraceRootBox();

/* busy windows */
extern struct _winbusy *MakeBusy();

#endif /* _OLWM_OLWM_H */
