/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 *
 *	Written for Sun Microsystems by Crucible, Santa Cruz, CA.
 */

static	char	sccsid[] = "@(#) InitGraphics.c 26.4 91/09/14 Crucible";
#include <errno.h>
#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>

#include <olgx/olgx.h>

#include "olwm.h"
#include "globals.h"
#include "resources.h"


/* Externs. */
extern Display	*DefDpy;
extern int	DefScreen;
extern Bool	WorkspaceColorUsed;
extern unsigned long WorkspaceColorPixel;

/* Globals. */
GC	RootGC;
GC	DrawBackgroundGC, DrawSelectedGC;
GC	DrawNormalGC, DrawReverseGC;
#ifdef STILL_NEEDED?
GC	DrawLinesGC, DrawRevLinesGC;
#endif /* STILL_NEEDED? */
GC	IconNormalGC, IconBorderGC, IconSelectedGC, IconUnselectedGC;
GC	DrawBusyGC;
GC	DrawWhiteGC, DrawBlackGC;

Bool	ColorDisplay;
Pixmap	Gray50;
int	Gray50width;
int	Gray50height;

/* bitmaps */

#define gray50_width 8
#define gray50_height 8
static char gray50_bits[] = {
    0x55, 0xaa, 0x55, 0xaa, 0x55, 0xaa, 0x55, 0xaa
};

#define busy_gray_width 8
#define busy_gray_height 8
static char busy_gray_bits[] = {
    0x88, 0x00, 0x22, 0x00, 0x88, 0x00, 0x22, 0x00
};

/* OLGX context */
Graphics_info *olgx_gisbutton;		/* buttons have special font */
Graphics_info *olgx_gistext;		/* notice box text has special font */
Graphics_info *olgx_gisnormal;
Graphics_info *olgx_gisreverse;
Graphics_info *olgx_gisrevpin;		/* drawing reverse pushpins */

int Resize_height, Resize_width;

/*
 * initGCs - initialize all the GCs used by olwm
 *
 *	Creating all of these in one place will hopefully prevent the
 *	creation of redundant GCs.  (There is some motivation for creating
 *	GCs as they are needed, so they can also be free'd when not used,
 *	but so far it's resulted in more GCs than really needed.  If we change
 *	back to that scheme, some Upd* routines for dynamically changing
 *	resources will need to be reorganized.)
 */
static void
initGCs(dpy)
Display	*dpy;
{
	XGCValues       values;
static	char		dashList[2] = { 1, 1 };

        /* Create a GC for drawing move window outlines in the root window. */
	values.function = GXxor;
	values.foreground = ~((~0L) << DisplayPlanes(dpy,DefaultScreen(dpy)));
	values.subwindow_mode = IncludeInferiors;
	RootGC = XCreateGC( dpy, RootWindow(dpy, DefaultScreen(dpy)),
			    ( GCFunction | GCForeground |
			      GCBackground | GCSubwindowMode ),
			    &values );

        /* Create a GC for drawing strictly using the background color 
	 * (frame-color), for example, in the frame.
	 * Used for drawing filled frame-colored rectangles for frame 
	 * edges around titlebar/header and footer.
	 * Should only be used for XFillRectangle, since both foreground and
	 * background are background colored (Bg1) and no font is specified.
	 */
        values.function = GXcopy;
        values.foreground = values.background = GRV.Bg1Color;
        values.line_width = 1;
        values.graphics_exposures = False;
        DrawBackgroundGC = XCreateGC( dpy, RootWindow(dpy, DefaultScreen(dpy)),
                        	      ( GCFunction | GCForeground 
					| GCBackground | GCGraphicsExposures 
					| GCLineWidth ), 
				      &values );

        /* Create a GC for text and line graphics (forecolor on backcolor). */
        values.function = GXcopy;
        values.foreground = GRV.Fg1Color;
        values.background = GRV.Bg1Color;
        values.font = GRV.TitleFontInfo->fid;
        values.graphics_exposures = False;
        DrawNormalGC = XCreateGC( dpy, RootWindow(dpy, DefaultScreen(dpy)),
                        (GCFont | GCFunction | GCForeground | GCBackground |
                         GCGraphicsExposures),
                        &values );

        /* Create a GC the opposite of the previous one. */
        values.function = GXcopy;
        values.foreground = GRV.Bg1Color;
        values.background = GRV.Fg1Color;
        values.font = GRV.TitleFontInfo->fid;
        values.graphics_exposures = False;
        DrawReverseGC = XCreateGC(dpy, RootWindow(dpy, DefaultScreen(dpy)),
                        (GCFont | GCFunction | GCForeground | GCBackground |
                         GCGraphicsExposures),
                        &values);

#ifdef STILL_NEEDED?
        /* This GC is used for the frame borders and titlebar
         * lines in focus follows mouse mode, so it's black on background.
         */
        values.function = GXcopy;
        values.foreground = BlackPixel(dpy, DefaultScreen(dpy));
        values.background = GRV.Bg1Color;
        values.font = GRV.TitleFontInfo->fid;
        values.graphics_exposures = False;
        DrawLinesGC = XCreateGC( dpy, RootWindow(dpy, DefaultScreen(dpy)),
                        (GCFont | GCFunction | GCForeground | GCBackground |
                         GCGraphicsExposures),
                        &values );
        /* Create a GC the opposite of the previous one. */
        values.function = GXcopy;
        values.foreground = GRV.Bg1Color;
        values.background = BlackPixel(dpy, DefaultScreen(dpy));
        values.font = GRV.TitleFontInfo->fid;
        values.graphics_exposures = False;
        DrawRevLinesGC = XCreateGC(dpy, RootWindow(dpy, DefaultScreen(dpy)),
                        (GCFont | GCFunction | GCForeground | GCBackground |
                         GCGraphicsExposures),
                        &values);
#endif /* STILL_NEEDED? */

	/* Create a GC for drawing the icon name (just like DrawNormal, but
	 * using IconFont).  Is also used for the icon pixmap.
	 */
        values.function = GXcopy;
        values.foreground = GRV.Fg1Color;
	if (WorkspaceColorUsed)
	    values.background = WorkspaceColorPixel;
	else
	    values.background = GRV.Bg1Color;
	values.font = GRV.IconFontInfo->fid;
	values.line_width = 1;
	values.graphics_exposures = False;
        IconNormalGC = XCreateGC( dpy, RootWindow(dpy, DefaultScreen(dpy)),
                        (GCFont | GCFunction | GCForeground | GCBackground |
			 GCGraphicsExposures | GCLineWidth),
                        &values );

	/* Create a GC for drawing the icon name and pixmap when selected.  
	 * (used only in 3D)
	 */
        values.function = GXcopy;
        values.foreground = GRV.Fg1Color;
	if (WorkspaceColorUsed)
	    values.background = WorkspaceColorPixel;
	else
	    values.background = GRV.Bg1Color;
	values.font = GRV.IconFontInfo->fid;
	values.line_width = 1;
	values.graphics_exposures = False;
        IconSelectedGC = XCreateGC( dpy, RootWindow(dpy, DefaultScreen(dpy)),
                          (GCFont | GCFunction | GCForeground | GCBackground |
			   GCGraphicsExposures | GCLineWidth),
                          &values );

	/* GC for unselected icons */
	values.function = GXcopy;
	if (WorkspaceColorUsed)
	    values.foreground = WorkspaceColorPixel;
	else
	    values.foreground = GRV.Bg1Color;
	values.line_width = 0;
	IconUnselectedGC = XCreateGC(dpy, DefaultRootWindow(dpy),
				     GCFunction | GCForeground | GCLineWidth,
				     &values);

	/* Create a GC for icon border. */
        values.function = GXcopy;
#ifndef COLOR2D
        values.foreground = GRV.Fg1Color;
#else
        values.foreground = GRV.BorderColor;
#endif /* COLOR2D */
        values.background = GRV.Bg1Color;
	values.line_width = 0;
	values.line_style = LineOnOffDash;
	values.graphics_exposures = False;
        IconBorderGC = XCreateGC( dpy, RootWindow(dpy, DefaultScreen(dpy)),
                        ( GCFunction | GCForeground | GCBackground |
			  GCGraphicsExposures | GCLineWidth | GCLineStyle ),
                        &values );
	XSetDashes( dpy, IconBorderGC, 1, dashList, 2 );

        /* Create a GC for drawing strictly using the selected color 
	 * (slightly dark background for 3d and border color for 2d). 
	 * Used for drawing icon selection borders.
	 * Should only be used for XFillRectangle, since both foreground and
	 * background are same color and no font is specified.
	 */
        values.function = GXcopy;
#ifndef COLOR2D
        values.foreground = values.background = GRV.Bg2Color;
#else
	if ( GRV.F3dUsed )
        	values.foreground = values.background = GRV.Bg2Color;
	else
        	values.foreground = values.background = GRV.BorderColor;
#endif /* COLOR2D */
	values.line_width = 1;
	values.graphics_exposures = False;
        DrawSelectedGC = XCreateGC( dpy, RootWindow(dpy, DefaultScreen(dpy)),
                        (GCFunction | GCForeground | GCBackground |
			 GCGraphicsExposures | GCLineWidth),
                        &values );

	{
	Pixmap		busyStipple;

	busyStipple = XCreatePixmapFromBitmapData(dpy, DefaultRootWindow(dpy),
	    busy_gray_bits, busy_gray_width, busy_gray_height, 1, 0, 1);

        /* Create a GC for text and line graphics (forecolor on backcolor). */
        values.function = GXcopy;
        values.foreground = GRV.Fg1Color;
        values.graphics_exposures = False;
	values.fill_style = FillStippled;
	values.stipple = busyStipple;
        DrawBusyGC = XCreateGC( dpy, RootWindow(dpy, DefaultScreen(dpy)),
                        (GCStipple | GCFunction | GCForeground | 
                         GCGraphicsExposures |  GCFillStyle),
                        &values );


	}

	/* Create a GC for drawing black lines/rectangles (black on black). */
        values.function = GXcopy;
        values.foreground = values.background = BlackPixel(dpy, 
							   DefaultScreen(dpy));
	values.line_width = 1;
	values.graphics_exposures = False;
        DrawBlackGC = XCreateGC( dpy, RootWindow(dpy, DefaultScreen(dpy)),
                        	 ( GCFunction | GCForeground | GCBackground |
			 	   GCGraphicsExposures | GCLineWidth ),
                        	 &values );

	/* Create a GC for drawing white lines/rectangles (white on white).  */
        values.function = GXcopy;
        values.foreground = values.background = WhitePixel(dpy, 
							   DefaultScreen(dpy));
	values.line_width = 1;
	values.graphics_exposures = False;
        DrawWhiteGC = XCreateGC( dpy, RootWindow(dpy, DefaultScreen(dpy)),
                        	 ( GCFunction | GCForeground | GCBackground |
			 	   GCGraphicsExposures | GCLineWidth ),
                        	 &values );
}


/*
 * initOLGX - initialize all the olgx Graphics_info structures used by olwm
 *
 *	Creating all of these in one place will hopefully prevent the
 *	creation of redundant gis variables.  (There is some motivation for 
 *	creating olgx_gis* as they are needed, so they can also be better
 *	managed, but so far it's resulted in more gis variables than really 
 *	needed.  If we change back to that scheme, some Upd* routines for 
 *	dynamically changing resources will need to be reorganized.)
 *
 *	Notice that only the colors are set up here, since setting fonts
 *	is all done in the Upd*Font() routines.	
 */
static void
initOLGX(dpy)
Display	*dpy;
{
	unsigned long pixvals[5];
	int dflag = GRV.F3dUsed ? OLGX_3D_COLOR : OLGX_2D;

	/*
	 * REMIND
	 * We will probably want to add support for OLGX_3D_MONO at
	 * some point.  This will require replacing all of the NULL's
	 * in the olgx_initialize() calls with an array of pixmaps.
	 */

	/* gis for drawing buttons */

	pixvals[OLGX_WHITE] = GRV.Bg0Color;
	pixvals[OLGX_BG1] = GRV.Bg1Color;
	pixvals[OLGX_BG2] = GRV.Bg2Color;
	pixvals[OLGX_BG3] = GRV.Bg3Color;
	pixvals[OLGX_BLACK] = GRV.Fg1Color;

	olgx_gisbutton = olgx_initialize(dpy, DefaultScreen(dpy), dflag,
					 GRV.GlyphFontInfo,
					 GRV.ButtonFontInfo,
					 pixvals, NULL);

	/* gis for drawing descriptive text */

	pixvals[OLGX_WHITE] = GRV.Bg0Color;
	pixvals[OLGX_BG1] = GRV.Bg1Color;
	pixvals[OLGX_BG2] = GRV.Bg2Color;
	pixvals[OLGX_BG3] = GRV.Bg3Color;
	pixvals[OLGX_BLACK] = GRV.Fg1Color;

	olgx_gistext = olgx_initialize(dpy, DefaultScreen(dpy), dflag,
				       GRV.GlyphFontInfo,
				       GRV.TextFontInfo,
				       pixvals, NULL);

	/* gis for drawing everything else */

	pixvals[OLGX_WHITE] = GRV.Bg0Color;
	pixvals[OLGX_BG1] = GRV.Bg1Color;
	pixvals[OLGX_BG2] = GRV.Bg2Color;
	pixvals[OLGX_BG3] = GRV.Bg3Color;
	pixvals[OLGX_BLACK] = GRV.Fg1Color;

	olgx_gisnormal = olgx_initialize(dpy, DefaultScreen(dpy), dflag,
					 GRV.GlyphFontInfo,
					 GRV.TitleFontInfo,
					 pixvals, NULL);

	/* gis for drawing in reverse */

	pixvals[OLGX_WHITE] = GRV.Bg2Color;
	pixvals[OLGX_BG1] = GRV.Fg1Color;
	pixvals[OLGX_BG2] = GRV.Bg0Color;
	pixvals[OLGX_BG3] = GRV.Fg1Color;
	pixvals[OLGX_BLACK] = GRV.Bg1Color;

	olgx_gisreverse = olgx_initialize(dpy, DefaultScreen(dpy), dflag,
					  GRV.GlyphFontInfo,
					  GRV.TitleFontInfo,
					  pixvals, NULL);
					  

	/* gis for drawing pushpin in reverse - useful only in 2D */

	pixvals[OLGX_WHITE] = GRV.Fg1Color;
	pixvals[OLGX_BG1] = GRV.Bg1Color;
	pixvals[OLGX_BG2] = GRV.Bg2Color;
	pixvals[OLGX_BG3] = GRV.Bg3Color;
	pixvals[OLGX_BLACK] = GRV.Bg0Color;

	olgx_gisrevpin = olgx_initialize(dpy, DefaultScreen(dpy), dflag,
					 GRV.GlyphFontInfo,
					 GRV.TitleFontInfo,
					 pixvals, NULL);
}


/*
 * isColorDisplay -- check to see if the display supports color.
 *	This should only need to be called once (which olwm first invoked)
 */
static Bool
isColorDisplay( dpy )
Display	*dpy;
{
	XVisualInfo	*vis;
	XVisualInfo	visTemplate;
	int		nvis;
	int		ii;
	Bool		colorDisplay = False;

	/* determine whether on a color display */
	vis = XGetVisualInfo( dpy, VisualNoMask, &visTemplate, &nvis );
	for ( ii = 0; ii < nvis; ++ii ) 
	{
	    if (vis[ii].screen == DefaultScreen(dpy)) 
	    {
		if ( vis[ii].class == StaticColor
		     || vis[ii].class == PseudoColor
		     || vis[ii].class == DirectColor ) 
		{
		    colorDisplay = True;
		    break;
		}
	    }
	}
	XFree( vis );

	return( colorDisplay );
}


/*
 * PreInitGraphics
 * This is a little strange.  PreInitGraphics initializes graphical data that 
 * might need to be used while the resource database is being read in.  
 * InitGraphics is set up based on values read from the resource database.
 */
void
PreInitGraphics(dpy)
    Display *dpy;
{
    ColorDisplay = isColorDisplay(dpy);
    
    Gray50 = XCreateBitmapFromData(dpy, DefaultRootWindow(dpy),
				   gray50_bits, gray50_width, gray50_height );
    Gray50width = gray50_width;
    Gray50height = gray50_height;
}


InitGraphics(dpy)
Display	*dpy;
{
	initGCs(dpy);
	initOLGX(dpy);
	
	/* Global cursor variables are set in resources.c:SetCursors()
	 * [created from cursorFont resource].  They need to be assigned
	 * to windows as appropriate.
	 */
	XDefineCursor( dpy, RootWindow(dpy, DefaultScreen(dpy)), 
		       GRV.BasicPointer );

	/* Apply the TitleFontInfo settings everywhere applicable */
	UpdTitleFont( dpy, RM_TITLE_FONT, True );

	/* Apply TextFontInfo information as appropriate. */
	UpdTextFont( dpy, RM_TEXT_FONT, True );

	/* Apply the ButtonFontInfo settings everywhere applicable */
	UpdButtonFont( dpy, RM_BUTTON_FONT, True );

	/* Apply the GlyphFontInfo settings everywhere applicable */
	UpdGlyphFont( dpy, RM_GLYPHFONT, True );
}

/* UninitGraphics	- free up any server resources that have been consumed,
 *			  and restore root cursor.
 *
 *	REMIND Currently unused.
 */
UninitGraphics()
{
	XUndefineCursor(DefDpy, RootWindow(DefDpy, DefScreen));
	olgx_destroy(olgx_gisbutton);
	olgx_destroy(olgx_gistext);
	olgx_destroy(olgx_gisnormal);
	olgx_destroy(olgx_gisreverse);
	olgx_destroy(olgx_gisrevpin);
}

/* HACK - get size of resize corners.  Should be 
 * replaced by a function/macro in olgx.  Assumes
 * all resize corners are the same size.
 *
 *	REMIND - this is called from UpdGlyphFont(), since this
 *	routine uses the size of that font.
 */
setResizeSizes()
{
	char s[2];
	XCharStruct xcs;
	int i1, i2;

	s[0]=UL_RESIZE_OUTLINE;
	s[1]='\0';
	XTextExtents(GRV.GlyphFontInfo,s,1,&i1,&i2,&i2,&xcs);
	Resize_height = xcs.ascent + xcs.descent;
	Resize_width = xcs.width;
}
