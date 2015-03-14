/*
 * BarGauge: Widget to periodically display values in form of a horizontal bar.
 * loosely based on Athena StripChart Widget.
 *
 * Author: Gabor Herr		herr@iti.informatik.th-darmstadt.de
 *
 * $Id: BarGauge.c,v 1.2 1993/01/24 17:27:57 gabor Exp $
 */

/***********************************************************
Copyright 1992, 1993 by Gabor Herr
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Gabor Herr or Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

GABOR HERR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
GABOR HERR BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/XawInit.h>
#include "BarGaugeP.h"
#include <X11/Xfuncs.h>

#define MS_PER_SEC 1000

#define min(a,b)	((a) < (b) ? (a) : (b))

/* Private Data */

#define offset(field) XtOffsetOf(BarGaugeRec, field)

static XtResource resources[] = {
    {XtNwidth, XtCWidth, XtRDimension, sizeof(Dimension),
	offset(core.width), XtRImmediate, (XtPointer) 120},
    {XtNheight, XtCHeight, XtRDimension, sizeof(Dimension),
	offset(core.height), XtRImmediate, (XtPointer) 12},
    {XtNsegments, XtCSegments, XtRInt, sizeof(int),
        offset(bar_gauge.segments), XtRImmediate, (XtPointer) 1},
    {XtNsegmentGap, XtCSegmentGap, XtRInt, sizeof(int),
        offset(bar_gauge.segment_gap), XtRImmediate, (XtPointer) 2},
    {XtNupdate, XtCInterval, XtRInt, sizeof(int),
        offset(bar_gauge.update), XtRImmediate, (XtPointer) 10},
    {XtNgetValue, XtCCallback, XtRCallback, sizeof(XtPointer),
        offset(bar_gauge.get_value), XtRImmediate, (XtPointer) NULL},
    {XtNmaxValue, XtCMaxValue, XtRFloat, sizeof(float),
        offset(bar_gauge.max_value), XtRString, (XtPointer) "1.0"},
    {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(bar_gauge.fgpixel[0]), XtRString, XtDefaultForeground},
    {XtNforeground1, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(bar_gauge.fgpixel[1]), XtRString, XtDefaultForeground},
    {XtNforeground2, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(bar_gauge.fgpixel[2]), XtRString, XtDefaultForeground},
    {XtNforeground3, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(bar_gauge.fgpixel[3]), XtRString, XtDefaultForeground},
    {XtNforeground4, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(bar_gauge.fgpixel[4]), XtRString, XtDefaultForeground},
    {XtNforeground5, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(bar_gauge.fgpixel[5]), XtRString, XtDefaultForeground},
    {XtNforeground6, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(bar_gauge.fgpixel[6]), XtRString, XtDefaultForeground},
    {XtNforeground7, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(bar_gauge.fgpixel[7]), XtRString, XtDefaultForeground},
};

#undef offset

static void Initialize(), Destroy(), Redisplay();
static Boolean SetValues();
static void repaint_window();

BarGaugeClassRec barGaugeClassRec = {
    { /* core fields */
    /* superclass		*/	(WidgetClass) &simpleClassRec,
    /* class_name		*/	"BarGauge",
    /* size			*/	sizeof(BarGaugeRec),
    /* class_initialize		*/	XawInitializeWidgetSet,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	XtInheritRealize,
    /* actions			*/	NULL,
    /* num_actions		*/	0,
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	XtExposeCompressMultiple |
	                                XtExposeGraphicsExposeMerged,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	Destroy,
    /* resize			*/	NULL,
    /* expose			*/	Redisplay,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	NULL,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	NULL,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
    },
    { /* Simple class fields */
    /* change_sensitive		*/	XtInheritChangeSensitive
    }
};

WidgetClass barGaugeWidgetClass = (WidgetClass) &barGaugeClassRec;

/****************************************************************
 *
 * Private Procedures
 *
 ****************************************************************/

static void draw_it();

/* ARGSUSED */
static void Initialize (greq, gnew)
    Widget greq, gnew;
{
    BarGaugeWidget w = (BarGaugeWidget)gnew;
    XGCValues gcval;
    int i;

    if (w->bar_gauge.update > 0)
        w->bar_gauge.interval_id = XtAppAddTimeOut( 
					XtWidgetToApplicationContext(gnew),
					w->bar_gauge.update * MS_PER_SEC, 
					draw_it, (XtPointer) gnew);

    if (w->bar_gauge.segments > XG_NUM_SEGMENTS)
      w->bar_gauge.segments = XG_NUM_SEGMENTS;

    /* reset values */
    for ( i = 0; i < XG_NUM_SEGMENTS; i++ )
      w->bar_gauge.last_values[i] = 0.0;
					
    /* create GCs for each active segment */
    for ( i = 0; i < w->bar_gauge.segments; i++ ) {
    	gcval.foreground = w->bar_gauge.fgpixel[i];
    	w->bar_gauge.fgGC[i] = XtGetGC( (Widget) w, GCForeground, &gcval );
    }
}
 
static void Destroy (gw)
     Widget gw;
{
     BarGaugeWidget w = (BarGaugeWidget)gw;
     int i;

     if (w->bar_gauge.update > 0)
         XtRemoveTimeOut (w->bar_gauge.interval_id);

     for ( i = 0; i < w->bar_gauge.segments; i++ )
       XtReleaseGC( (Widget) w, w->bar_gauge.fgGC[i] );
}

/*
 * NOTE: This function really needs to recieve graphics exposure 
 *       events, but since this is not easily supported until R4 I am
 *       going to hold off until then.
 */

/* ARGSUSED */
static void Redisplay(w, event, region)
     Widget w;
     XEvent *event;
     Region region;
{
	(void) repaint_window ((BarGaugeWidget)w);
}

/* ARGSUSED */
static void 
draw_it(client_data, id)
XtPointer client_data;
XtIntervalId *id;		/* unused */
{
   BarGaugeWidget w = (BarGaugeWidget)client_data;
   XgBarGaugeValues vals;
   
   if (w->bar_gauge.update > 0)
       w->bar_gauge.interval_id =
       XtAppAddTimeOut(XtWidgetToApplicationContext( (Widget) w),
		       w->bar_gauge.update * MS_PER_SEC,draw_it,client_data);

   if (w->bar_gauge.get_value == NULL)
       return;

   vals.num_values = w->bar_gauge.segments;
   vals.values     = w->bar_gauge.last_values;
   XtCallCallbacks( (Widget)w, XtNgetValue, (XtPointer)&vals );
   
   if (XtIsRealized((Widget)w))
       repaint_window(w);
} /* draw_it */

static void 
repaint_window(w)
BarGaugeWidget w;
{
  int lastx;
  int i;
  Dimension valuewidth = w->core.width -
	w->bar_gauge.segments * (w->bar_gauge.segment_gap + 1);
  
  if (XtIsRealized((Widget)w)) {

    XClearWindow( XtDisplay (w), XtWindow (w));

    for ( lastx = i = 0; i < w->bar_gauge.segments; i++ ) {

       int x = (int)(valuewidth * (w->bar_gauge.last_values[i] / w->bar_gauge.max_value));

       /* don't let segment disappear, make it at least 1 pixel wide */
       if (x <= 0) x = 1;
       
       XFillRectangle(XtDisplay(w), XtWindow(w), w->bar_gauge.fgGC[i],
		      lastx, 0, x, w->core.height);

       lastx += (x + w->bar_gauge.segment_gap);
    }

    XFlush(XtDisplay(w));		    /* Flush output buffers */

  }
}

/* ARGSUSED */
static Boolean SetValues (current, request, new)
    Widget current, request, new;
{
    BarGaugeWidget old = (BarGaugeWidget)current;
    BarGaugeWidget w = (BarGaugeWidget)new;
    Boolean ret_val = FALSE;
    XGCValues gcval;
    int i;
    int checkgc;

#define has_changed(resource) \
	(old->bar_gauge.resource != w->bar_gauge.resource)
    
    if (has_changed(update)) {
	if (old->bar_gauge.update > 0)
	    XtRemoveTimeOut (old->bar_gauge.interval_id);
	if (w->bar_gauge.update > 0)
	    w->bar_gauge.interval_id =
		XtAppAddTimeOut(XtWidgetToApplicationContext(new),
				w->bar_gauge.update * MS_PER_SEC,
				draw_it, (XtPointer)w);
    }

    if (has_changed(segments)) {
      if (old->bar_gauge.segments > w->bar_gauge.segments) {
      	 /* free GC's, that we don't need any more */
      	 for ( i = w->bar_gauge.segments; i < old->bar_gauge.segments; i++ )
      	   XtReleaseGC( (Widget) old, old->bar_gauge.fgGC[i] );
      }
      else {
      	 /* allocate GC's, for the new segments */
      	 for ( i = old->bar_gauge.segments; i < w->bar_gauge.segments; i++ ) {
      	   gcval.foreground = w->bar_gauge.fgpixel[i];
      	   w->bar_gauge.fgGC[i] = XtGetGC( (Widget) w, GCForeground, &gcval );
      	 }
      }

      ret_val = True;
    }

    checkgc = min( w->bar_gauge.segments, old->bar_gauge.segments );

    for ( i = 0; i < checkgc; i++ )
      if ( has_changed( fgpixel[i] ) ) {
      	XtReleaseGC( (Widget) old, old->bar_gauge.fgGC[i] );
      	gcval.foreground = w->bar_gauge.fgpixel[i];
      	w->bar_gauge.fgGC[i] = XtGetGC( (Widget) w, GCForeground, &gcval );

      	ret_val = True;
      }
    
    return( ret_val );
}
