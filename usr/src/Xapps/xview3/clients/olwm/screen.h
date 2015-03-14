/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)screen.h	26.17	91/09/14 SMI"

#ifndef _OLWM_SCREEN_H
#define _OLWM_SCREEN_H

#include <olgx/olgx.h>

/*
 *	Index's into GC array of ScreenInfo struct
 */
typedef enum {	ROOT_GC,
		FOREGROUND_GC,
		BORDER_GC,
		WINDOW_GC,
		WORKSPACE_GC,
		BUSY_GC,
		ICON_NORMAL_GC,
		ICON_MASK_GC,
		ICON_BORDER_GC,
		NUM_GCS 
} ScreenGCIndex;

/*
 *	Index's into Graphics_info array of ScreenInfo struct
 */
typedef enum { NORMAL_GINFO, 
	       BUTTON_GINFO, 
	       TEXT_GINFO,
	       REVPIN_GINFO,
	       NUM_GINFOS 
} ScreenGinfoIndex;

/*
 *	Index's into Pixmap array of ScreenInfo struct
 */
typedef enum {
    BUSY_STIPPLE,
    ICON_BITMAP,
    ICON_MASK,
    PROTO_DRAWABLE,
    GRAY50_BITMAP,
    NUM_PIXMAPS
} ScreenPixmapIndex;

/*
 * 	ColorMapFocus	- client/window which has colormap focus
 */
typedef struct _colormapfocus {
	struct _client		*client;
	struct _wingeneric	*window;
	Bool			locked;
} ColorMapFocus;

/*
 *	BackgroundType	- type of workspace background
 */
typedef enum { BG_None, BG_Color, BG_Pixmap } BackgroundType;

/*
 *	ColorInfo	- window/workspace/etc colors
 */
typedef struct _colorinfo {
	unsigned long		flags;
	Bool			reverseVideo;
	unsigned long		black, white;
	unsigned long		fgColor, bgColor;
	unsigned long		bg0Color,bg1Color,bg2Color,bg3Color;
	unsigned long		borderColor;
	BackgroundType		workspaceType;
	unsigned long		workspaceColor;
	unsigned long		workspaceRootPixel;
} ColorInfo;

#define CIWorkspaceColorAlloced		(1L<<0)
#define CIWindowColorAlloced		(1L<<1)
#define CIForegroundColorAlloced	(1L<<2)
#define CIBackgroundColorAlloced	(1L<<3)
#define CIBorderColorAlloced		(1L<<4)

/*
 *	ScreenInfo	- Per screen info
 */
typedef struct _screeninfo {
	int			screen;
	Window			rootid;
	struct _winroot		*rootwin;
	int			depth;
	Visual			*visual;
	Colormap		colormap;
	Bool			iscolor;
	Bool			use3D;
	ColorInfo		colorInfo;
	GC			gc[NUM_GCS];
	Graphics_info		*gi[NUM_GINFOS];
	Pixmap			pixmap[NUM_PIXMAPS];
	struct _menuCache	*menuCache;
	ColorMapFocus		cmapfocus;
	int			framepos;
	struct _iconGrid	*iconGrid;
	char			**environment;
	int			instanceQ;	/* quark for this screen's
						   instance name */
	int			dfltIconWidth, dfltIconHeight;
} ScreenInfo;

/*
 *	Global functions from screen.c
 */
extern	void		InitScreens(/* Display *dpy */);
extern	void		DestroyScreens(/* Display *dpy */);
extern	ScreenInfo	*GetFirstScrInfo();
extern	ScreenInfo	*GetScrInfoOfScreen(/* int screen */);
extern	ScreenInfo	*GetScrInfoOfRoot(/* Window root */);
extern	void		SetWorkspaceColor(/* Display *dpy */);
extern	void		SetWindowColor(/* Display *dpy */);
extern	void		SetForegroundColor(/* Display *dpy */);
extern	void		SetBackgroundColor(/* Display *dpy */);
extern	void		SetBorderColor(/* Display *dpy */);
extern	void		SetTitleFont(/* Display *dpy */);
extern	void		SetTextFont(/* Display *dpy */);
extern	void		SetButtonFont(/* Display *dpy */);
extern	void		SetIconFont(/* Display *dpy */);
extern	void		SetGlyphFont(/* Display *dpy */);
extern	void		SetIconLocation(/*Display *dpy */);
extern	void		UpdateScreenResources();

#endif	/* _OLWM_SCREEN_H */
