/*
 * $XConsortium: ClockP.h,v 1.8 90/12/01 13:03:54 rws Exp $
 */

#ifndef _ClockP_h
#define _ClockP_h

#include "Clock.h"
#include <X11/CoreP.h>
#include "transform.h"

#define POLY_SIZE	6

/* New fields for the clock widget instance record */
typedef struct {
	Pixel		minute;
	Pixel		hour;
	Pixel		jewel;
	GC		minuteGC;
	GC		hourGC;
	GC		jewelGC;
	GC		eraseGC;
	GC		shapeGC;	/* pointer to GraphicsContext */
/* start of graph stuff */
	int		backing_store;	/* backing store variety */
	Boolean		shape_window;	/* use SetWindowShapeMask */
	Boolean		transparent;	/* make window transparent */
	float		border_size;
	float		jewel_size;
	XtIntervalId	interval_id;
	Transform	t;
	Transform	maskt;
	Pixmap		shape_mask;	/* window shape */
	int		shape_width;	/* window width when shape last made */
	int		shape_height;	/* window height when shape last made */
	double		hour_angle;	/* hour hand position */
	double		minute_angle;	/* minute hand position */
	int		polys_valid;	/* polygons contain good data */
	TPoint		minute_poly[POLY_SIZE];	/* polygon for minute hand */
	TPoint		hour_poly[POLY_SIZE];	/* polygon for hour hand */
} ClockPart;

/* Full instance record declaration */
typedef struct _ClockRec {
	CorePart core;
	ClockPart clock;
} ClockRec;

/* New fields for the Clock widget class record */
typedef struct {int dummy;} ClockClassPart;

/* Full class record declaration. */
typedef struct _ClockClassRec {
	CoreClassPart core_class;
	ClockClassPart clock_class;
} ClockClassRec;

/* Class pointer. */
extern ClockClassRec clockClassRec;

#endif /* _ClockP_h */
