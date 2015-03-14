/*
 * BarGauge: Widget to periodically display a value in form of a horizontal bar.
 * based on Athena StripChart Widget.
 *
 * Author: Gabor Herr		herr@iti.informatik.th-darmstadt.de
 *
 * $Id: BarGaugeP.h,v 1.2 1993/01/24 17:27:57 gabor Exp $
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

#ifndef _XgBarGaugeP_h
#define _XgBarGaugeP_h

#include "BarGauge.h"
#include <X11/Xaw/SimpleP.h>

/*
 * max. # of different segments within the bar. Change this only, if you also
 * add new (foreground) resources.
 * FIXME: This should better be a variable or a resource, and the foreground
 * values should be better in a list.
 */
#define XG_NUM_SEGMENTS	8

/* New fields for the barGauge widget instance record */

typedef struct {
  Pixel fgpixel[XG_NUM_SEGMENTS];	/* color index for each bar segments */
  GC	fgGC[XG_NUM_SEGMENTS];		/* graphics context for fgpixel */
    
  float max_value;			/* Max Value in bar */
  int	segments;			/* # of segments */
  int	segment_gap;			/* spacing between segments */
  
  double last_values[XG_NUM_SEGMENTS];	/* last displayed values */

  int	update;				/* update frequence */
  XtCallbackList get_value; 		/* proc to call to fetch load pt */
  XtIntervalId interval_id;
} BarGaugePart;

/* Full instance record declaration */
typedef struct _BarGaugeRec {
   CorePart core;
   SimplePart simple;
   BarGaugePart bar_gauge;
} BarGaugeRec;

/* New fields for the BarGauge widget class record */
typedef struct {int dummy;} BarGaugeClassPart;

/* Full class record declaration. */
typedef struct _BarGaugeClassRec {
   CoreClassPart core_class;
   SimpleClassPart simple_class;
   BarGaugeClassPart bar_gauge_class;
} BarGaugeClassRec;

/* Class pointer. */
extern BarGaugeClassRec barGaugeClassRec;

#endif /* _XgBarGaugeP_h */
