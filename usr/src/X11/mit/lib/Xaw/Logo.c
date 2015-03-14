/* $XConsortium: Logo.c,v 1.25 91/05/22 16:56:51 converse Exp $ */

/*
Copyright 1988 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.
M.I.T. makes no representations about the suitability of
this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/Xaw/XawInit.h>
#include <X11/Xaw/LogoP.h>
#include <X11/extensions/shape.h>

static XtResource resources[] = {
    {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
        XtOffsetOf(LogoRec,logo.fgpixel), XtRString,
       (XtPointer) XtDefaultForeground},
    {XtNshapeWindow, XtCShapeWindow, XtRBoolean, sizeof (Boolean),
       XtOffsetOf(LogoRec,logo.shape_window), XtRImmediate, 
       (XtPointer) FALSE},
};

static void Initialize(), Realize(), Destroy(), Redisplay(), Resize();
static Boolean SetValues();

LogoClassRec logoClassRec = {
    { /* core fields */
    /* superclass		*/	(WidgetClass) &simpleClassRec,
    /* class_name		*/	"Logo",
    /* widget_size		*/	sizeof(LogoRec),
    /* class_initialize		*/	XawInitializeWidgetSet,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	Realize,
    /* actions			*/	NULL,
    /* num_actions		*/	0,
    /* resources		*/	resources,
    /* resource_count		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	Destroy,
    /* resize			*/	Resize,
    /* expose			*/	Redisplay,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	NULL,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
    },
    { /* simple fields */
    /* change_sensitive         */      XtInheritChangeSensitive
    },
    { /* logo fields */
    /* ignore                   */      0
    }
};

WidgetClass logoWidgetClass = (WidgetClass) &logoClassRec;


/*****************************************************************************
 *									     *
 *			   private utility routines			     *
 *									     *
 *****************************************************************************/

static void create_gcs (w)
    LogoWidget w;
{
    XGCValues v;

    v.foreground = w->logo.fgpixel;
    w->logo.foreGC = XtGetGC ((Widget) w, GCForeground, &v);
    v.foreground = w->core.background_pixel;
    w->logo.backGC = XtGetGC ((Widget) w, GCForeground, &v);
}

static void check_shape (w)
    LogoWidget w;
{
    if (w->logo.shape_window) {
	int event_base, error_base;

	if (!XShapeQueryExtension (XtDisplay (w), &event_base, &error_base))
	  w->logo.shape_window = FALSE;
    }
}

/* ARGSUSED */
static void unset_shape (w)
    LogoWidget w;
{
    XSetWindowAttributes attr;
    unsigned long mask;
    Display *dpy = XtDisplay ((Widget) w);
    Window win = XtWindow ((Widget) w);

    if (win == None) return;

    if (w->core.background_pixmap != None && 
	w->core.background_pixmap != XtUnspecifiedPixmap) {
	attr.background_pixmap = w->core.background_pixmap;
	mask = CWBackPixmap;
    } else {
	attr.background_pixel = w->core.background_pixel;
	mask = CWBackPixel;
    }
    XChangeWindowAttributes (dpy, win, mask, &attr);
    XShapeCombineMask (dpy, win, ShapeBounding, 0, 0, None, ShapeSet);
    if (!w->logo.foreGC) create_gcs (w);
    w->logo.need_shaping = w->logo.shape_window;
}

static void set_shape (w)
    LogoWidget w;
{
    GC ones, zeros;
    Display *dpy = XtDisplay ((Widget) w);
    Window win = XtWindow ((Widget) w);
    unsigned int width = (unsigned int) w->core.width;
    unsigned int height = (unsigned int) w->core.height;
    Pixmap pm = XCreatePixmap (dpy, win, width, height, (unsigned int) 1);
    XGCValues v;

    v.foreground = (Pixel) 1;
    v.background = (Pixel) 0;
    ones = XCreateGC (dpy, pm, (GCForeground | GCBackground), &v);
    v.foreground = (Pixel) 0;
    v.background = (Pixel) 1;
    zeros = XCreateGC (dpy, pm, (GCForeground | GCBackground), &v);

    if (pm && ones && zeros) {
	int x = 0, y = 0;
	Widget parent;

	XmuDrawLogo (dpy, pm, ones, zeros, 0, 0, width, height);
	for (parent = (Widget) w; XtParent(parent);
	     parent = XtParent(parent)) {
	    x += parent->core.x + parent->core.border_width;
	    y += parent->core.y + parent->core.border_width;
	}
	XShapeCombineMask (dpy, XtWindow (parent), ShapeBounding,
			   x, y, pm, ShapeSet);
	w->logo.need_shaping = FALSE;
    } else {
	unset_shape (w);
    }
    if (ones) XFreeGC (dpy, ones);
    if (zeros) XFreeGC (dpy, zeros);
    if (pm) XFreePixmap (dpy, pm);
}


/*****************************************************************************
 *									     *
 *				 class methods				     *
 *									     *
 *****************************************************************************/

/* ARGSUSED */
static void Initialize (request, new)
    Widget request, new;
{
    LogoWidget w = (LogoWidget)new;

    if (w->core.width < 1) w->core.width = 100;
    if (w->core.height < 1) w->core.height = 100;

    w->logo.foreGC = (GC) NULL;
    w->logo.backGC = (GC) NULL;
    check_shape (w);
    w->logo.need_shaping = w->logo.shape_window;
}

static void Destroy (gw)
    Widget gw;
{
    LogoWidget w = (LogoWidget) gw;
    if (w->logo.foreGC) {
	XtReleaseGC (gw, w->logo.foreGC);
	w->logo.foreGC = (GC) NULL;
    }
    if (w->logo.backGC) {
	XtReleaseGC (gw, w->logo.backGC);
	w->logo.backGC = (GC) NULL;
    }
}

static void Realize (gw, valuemaskp, attr)
    Widget gw;
    XtValueMask *valuemaskp;
    XSetWindowAttributes *attr;
{
    LogoWidget w = (LogoWidget) gw;

    if (w->logo.shape_window) {
	attr->background_pixel = w->logo.fgpixel;  /* going to shape */
	*valuemaskp |= CWBackPixel;
    } else
      create_gcs (w);
    (*logoWidgetClass->core_class.superclass->core_class.realize)
	(gw, valuemaskp, attr);
}

static void Resize (gw)
    Widget gw;
{
    LogoWidget w = (LogoWidget) gw;

    if (w->logo.shape_window) set_shape (w);
}

/* ARGSUSED */
static void Redisplay (gw, event, region)
    Widget gw;
    XEvent *event;		/* unused */
    Region region;		/* unused */
{
    LogoWidget w = (LogoWidget) gw;

    if (w->logo.shape_window) {
	if (w->logo.need_shaping) set_shape (w);  /* may change shape flag */
    }
    if (!w->logo.shape_window)
      XmuDrawLogo(XtDisplay(w), XtWindow(w), w->logo.foreGC, w->logo.backGC,
		  0, 0, (unsigned int) w->core.width,
		  (unsigned int) w->core.height);
}

/* ARGSUSED */
static Boolean SetValues (gcurrent, grequest, gnew)
    Widget gcurrent, grequest, gnew;
{
    LogoWidget current = (LogoWidget) gcurrent;
    LogoWidget new = (LogoWidget) gnew;
    Boolean redisplay = FALSE;

   check_shape (new);			/* validate shape_window */

    if ((new->logo.fgpixel != current->logo.fgpixel) ||
	(new->core.background_pixel != current->core.background_pixel)) {
	Destroy (gnew);
	if (!new->logo.shape_window) create_gcs (new);
	redisplay = TRUE;
    }
   
   if (new->logo.shape_window != current->logo.shape_window) {
       if (new->logo.shape_window) {
	   Destroy (gnew);
	   set_shape (new);
	   redisplay = FALSE;
       } else {
	   unset_shape (new);		/* creates new GCs */
	   redisplay = TRUE;
       }
   }

   return (redisplay);
}
