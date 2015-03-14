/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "suit.h"
#include <math.h>

#define PI                      3.14159

#define OLD_HOURS "old hours"
#define OLD_MINUTES "old minutes"
#define OLD_SECONDS "old seconds"


PRIVATE void OptimizePaint (SUIT_object o, char *propName, char *propType, Pointer new, Pointer old)
{
    if ( !(SUIT_stringsMatch (propName, OLD_HOURS) ||
	   SUIT_stringsMatch (propName, OLD_MINUTES) ||
	   SUIT_stringsMatch (propName, OLD_SECONDS)) )
	OBJECT_OPTIMIZED(o) = FALSE;
}


PRIVATE void PaintSecondHand (int sec)
{
    double angle = PI*((double)(60-sec)/30 + 0.5);
    GP_lineCoord (0.0, 0.0, 0.8*cos(angle), 0.8*sin(angle));
}


PRIVATE void PaintMinuteHand (int min)
{
    double px[3], py[3];
    double angle = PI*((double)(60-min)/30 + 0.5);
    px[0] = 0.05*cos(angle-PI/2);    py[0] = 0.05*sin(angle-PI/2);
    px[1] = 0.05*cos(angle+PI/2);    py[1] = 0.05*sin(angle+PI/2);
    px[2] = 0.8*cos(angle);          py[2] = 0.8*sin(angle);
    GP_fillPolygonCoord (3, px, py);
}


PRIVATE void PaintHourHand (int hour, int min)
{
    double px[3], py[3];
    double angle = PI*((12-((double)hour+(double)min/60))/6 + 0.5);
    px[0] = 0.05*cos(angle-PI/2);    py[0] = 0.05*sin(angle-PI/2);
    px[1] = 0.05*cos(angle+PI/2);    py[1] = 0.05*sin(angle+PI/2);
    px[2] = 0.5*cos(angle);          py[2] = 0.5*sin(angle);
    GP_fillPolygonCoord (3, px, py);
}


PRIVATE void PaintAnalogClock (SUIT_object o)
{
    int hour,min,sec,milli,i;
    int oldhour, oldmin, oldsec;
    double angle, len;

    oldhour = SUIT_getInteger (o, OLD_HOURS);
    oldmin = SUIT_getInteger (o, OLD_MINUTES);
    oldsec = SUIT_getInteger (o, OLD_SECONDS);
    GP_convertTime (GP_getCurrentTime(), &hour, &min, &sec, &milli);

    GP_setWindow (GP_defRectangle(-1.0, -1.0, 1.0, 1.0));
    if (OBJECT_OPTIMIZED(o)) {
	if (hour == oldhour && min == oldmin &&
	    (sec == oldsec || !SUIT_getBoolean (o, HAS_SECOND_HAND)))
	    return;
	GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
	if (SUIT_getBoolean (o, HAS_SECOND_HAND))
	    PaintSecondHand (oldsec);
	PaintMinuteHand (oldmin);
	PaintHourHand (oldhour, oldmin);
	GP_setColor (SUIT_getColor (o, FOREGROUND_COLOR));
	GP_fillEllipseCoord (-0.05, -0.05, 0.05, 0.05);
	if (SUIT_getBoolean (o, HAS_SECOND_HAND))
	    PaintSecondHand (sec);
	PaintMinuteHand (min);
	PaintHourHand (hour, min);
    } else {
	GP_setColor (SUIT_getColor (o, FOREGROUND_COLOR));
	if (SUIT_getBoolean (o, HAS_RIM))
	    GP_ellipseCoord (-1.0, -1.0, 1.0, 1.0);
	GP_fillEllipseCoord (-0.05, -0.05, 0.05, 0.05);
	for (i=0; i < 60; i++) {
	    angle = PI*((double)i/30 - 0.5);
	    len = (i%5==0)? 0.1 : 0.05;
	    GP_lineCoord ((1.0-len)*cos(angle),(1.0-len)*sin(angle),cos(angle),sin(angle));
	}
	if (SUIT_getBoolean (o, HAS_SECOND_HAND))
	    PaintSecondHand (sec);
	PaintMinuteHand (min);
	PaintHourHand (hour, min);
    }	

    SUIT_deluxeSetInteger (o, OLD_HOURS, hour, OBJECT);
    SUIT_makePropertyTemporary (o, OLD_HOURS, OBJECT);
    SUIT_deluxeSetInteger (o, OLD_MINUTES, min, OBJECT);
    SUIT_makePropertyTemporary (o, OLD_MINUTES, OBJECT);
    SUIT_deluxeSetInteger (o, OLD_SECONDS, sec, OBJECT);
    SUIT_makePropertyTemporary (o, OLD_SECONDS, OBJECT);
    OBJECT_OPTIMIZED(o) = TRUE;
}


PRIVATE void PaintTimeAsString (int hour, int min, int sec)
{
    char buf[50];
    sprintf (buf, "%d:%.2d:%.2d",hour,min,sec);
    GP_justifyText (buf, JUSTIFY_CENTER);
}


PRIVATE void PaintDigitalClock (SUIT_object o)
{
    int hour, min, sec, milli;
    int oldhour, oldmin, oldsec;
    GP_time t;

    t = GP_getCurrentTime();
    oldhour = SUIT_getInteger (o, OLD_HOURS);
    oldmin = SUIT_getInteger (o, OLD_MINUTES);
    oldsec = SUIT_getInteger (o, OLD_SECONDS);
    GP_convertTime (t, &hour, &min, &sec, &milli);
    if (!SUIT_getBoolean(o, MILITARY_TIME) && hour > 12)
	hour -= 12;
    if (OBJECT_OPTIMIZED(o)) {
	if (hour == oldhour && min == oldmin &&
	    (sec == oldsec || !SUIT_getBoolean (o, HAS_SECOND_HAND)))
	    return;
	GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
	PaintTimeAsString (oldhour, oldmin, oldsec);
	GP_setColor (SUIT_getColor (o, FOREGROUND_COLOR));
    }
    PaintTimeAsString (hour, min, sec);
    SUIT_deluxeSetInteger (o, OLD_HOURS, hour, OBJECT);
    SUIT_makePropertyTemporary (o, OLD_HOURS, OBJECT);
    SUIT_deluxeSetInteger (o, OLD_MINUTES, min, OBJECT);
    SUIT_makePropertyTemporary (o, OLD_MINUTES, OBJECT);
    SUIT_deluxeSetInteger (o, OLD_SECONDS, sec, OBJECT);
    SUIT_makePropertyTemporary (o, OLD_SECONDS, OBJECT);
    OBJECT_OPTIMIZED(o) = TRUE;
}


PRIVATE void HitDigitalClock (SUIT_object o, SUIT_event e)
{
    SUIT_setBoolean (o, HAS_SECOND_HAND, !SUIT_getBoolean (o, HAS_SECOND_HAND));
}


SUIT_object SUIT_createClock (char *name)
{
    static boolean firsttime = TRUE;
    SUIT_object clockWidget;

    clockWidget = SUIT_createObject (name, "clock");
    SUIT_addDisplayToObject (clockWidget, "digital", HitDigitalClock, PaintDigitalClock);
    SUIT_addDisplayToObject (clockWidget, "analog", HitDigitalClock, PaintAnalogClock);

    SUIT_deluxeSetInteger (clockWidget, OLD_HOURS, 0, OBJECT);
    SUIT_makePropertyTemporary (clockWidget, OLD_HOURS, OBJECT);
    SUIT_deluxeSetInteger (clockWidget, OLD_MINUTES, 0, OBJECT);
    SUIT_makePropertyTemporary (clockWidget, OLD_MINUTES, OBJECT);
    SUIT_deluxeSetInteger (clockWidget, OLD_SECONDS, 0, OBJECT);
    SUIT_makePropertyTemporary (clockWidget, OLD_SECONDS, OBJECT);
    if (firsttime) {
	SUIT_deluxeSetBoolean (clockWidget, ANIMATED, TRUE, CLASS);
	firsttime = FALSE;
    }
    SUIT_registerInterest (clockWidget, OptimizePaint);
    return clockWidget;
}
