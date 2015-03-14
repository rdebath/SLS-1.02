/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "suit.h"
#include <math.h>

enum bouncingShape {
    circle, square
};


PRIVATE void OptimizePaint (SUIT_object o, char *propName, char *propType, Pointer p1, Pointer p2)
{
    if (!
	(SUIT_stringsMatch (propName, BALL_X) ||
	 SUIT_stringsMatch (propName, BALL_Y) ||
	 SUIT_stringsMatch (propName, PREVIOUS_TIME) ||
	 SUIT_stringsMatch (propName, VECTOR_X) ||
	 SUIT_stringsMatch (propName, VECTOR_Y) 
	 )
	)
      {
	OBJECT_OPTIMIZED(o) = FALSE;
      }
}


static void DrawThing (SUIT_object o, enum bouncingShape shape)
{
    double size = SUIT_getDouble (o, BALL_SIZE);
    double vectorX = SUIT_getDouble (o, VECTOR_X);
    double vectorY = SUIT_getDouble (o, VECTOR_Y);
    double oldX = SUIT_getDouble (o, BALL_X);
    double oldY = SUIT_getDouble (o, BALL_Y);
    double x, y;
    double half = size / 2.0;

    double unitVecX, unitVecY;
    double prev_time = SUIT_getDouble (o, PREVIOUS_TIME);
    double curr_time = 0.0, elapsed_time;
    int speed = SUIT_getInteger (o, PIXELS_PER_SECOND);
    int hr, min, sec, msec;
    SUIT_viewport vp;
    double wrldCrds_per_sec;

    vp = SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o));
    wrldCrds_per_sec = (double) speed / (double) (vp.top_right.x - vp.bottom_left.x);

    if (SUIT_getBoolean (o,ANIMATED)) {

       GP_convertTime (GP_getCurrentTime (), &hr, &min, &sec, &msec);
       curr_time = 60000.0*(double)min + 1000.0*(double)sec + msec;

       if (prev_time == 0.0) {
	   prev_time = curr_time;
           GP_convertTime (GP_getCurrentTime (), &hr, &min, &sec, &msec);
           curr_time = 60000.0*(double)min + 1000.0*(double)sec + msec;
       }
       elapsed_time = curr_time - prev_time;
    } else
    	elapsed_time = 0.0;

    unitVecX = vectorX / (sqrt (vectorX * vectorX + vectorY * vectorY));
    unitVecY = vectorY / (sqrt (vectorX * vectorX + vectorY * vectorY));

    x = oldX + wrldCrds_per_sec / 1000.0 * elapsed_time *unitVecX;
    y = oldY + wrldCrds_per_sec / 1000.0 * elapsed_time *unitVecY;

    if (OBJECT_OPTIMIZED(o)) {
	GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
	GP_fillRectangleCoord (oldX - half, oldY - half, oldX + half, oldY + half);
    }
    else 
      SUIT_backgroundAndBorderObject(o);

    GP_setColor (SUIT_getColor (o, FOREGROUND_COLOR));
    if (SUIT_getBoolean (o, FILLED)) {
	if (shape == circle)
	    GP_fillEllipseCoord (x - half, y - half, x + half, y + half);
	else
	    GP_fillRectangleCoord (x - half, y - half, x + half, y + half);
    } else {
	if (shape == circle)
	    GP_ellipseCoord (x - half, y - half, x + half, y + half);
	else
	    GP_rectangleCoord (x - half, y - half, x + half, y + half);
    }

    if ((x > (1.0-half)) || (x < half))
	SUIT_setDouble (o, VECTOR_X, -vectorX);
    if ((y > 1.0-half) || (y < half))
	SUIT_setDouble (o, VECTOR_Y, -vectorY);

    if (x > 1.0 - half)
	x = 1.0 - half;
    else if (x < 0.0 + half)
	x = 0.0 + half;
    if (y > 1.0 - half)
	y = 1.0 - half;
    else if (y < 0.0 + half)
	y = 0.0 + half;


    SUIT_suspendMarkingRedisplay (o);
    SUIT_setDouble (o, PREVIOUS_TIME, curr_time);
    SUIT_makePropertyTemporary (o, PREVIOUS_TIME, OBJECT);

    SUIT_setDouble (o, BALL_X, x);
    SUIT_setDouble (o, BALL_Y, y);
    SUIT_resumeMarkingRedisplay (o);
    OBJECT_OPTIMIZED(o) = TRUE;
}



static void DrawCircle (SUIT_object o)
{
    DrawThing (o, circle);
}


static void DrawSquare (SUIT_object o)
{
    DrawThing (o, square);
}


static void HitBall (SUIT_object o, SUIT_event e)
{
    SUIT_setBoolean (o, ANIMATED, !SUIT_getBoolean (o, ANIMATED));
}



SUIT_object SUIT_createBouncingBall (char *name)
{
    SUIT_object retval;
    static boolean firsttime = TRUE;

    retval = SUIT_createObject (name, "bouncing ball");
    SUIT_addDisplayToObject (retval, "square", HitBall, DrawSquare);
    SUIT_addDisplayToObject (retval, "standard", HitBall, DrawCircle);
    SUIT_deluxeSetBoolean (retval, ANIMATED, TRUE, OBJECT);
    SUIT_deluxeSetDouble (retval, VECTOR_X, 1.0, OBJECT);
    SUIT_makePropertyTemporary (retval, VECTOR_X, OBJECT);
    SUIT_deluxeSetDouble (retval, VECTOR_Y, 0.9, OBJECT);
    SUIT_makePropertyTemporary (retval, VECTOR_Y, OBJECT);
    SUIT_deluxeSetDouble (retval, PREVIOUS_TIME, 0.0, OBJECT);
    SUIT_makePropertyTemporary (retval, PREVIOUS_TIME, OBJECT);
    if (firsttime) {
	SUIT_deluxeSetDouble (retval, BALL_SIZE, 0.10, CLASS);
	SUIT_deluxeSetDouble (retval, BALL_X, 0.0, CLASS);
	SUIT_deluxeSetDouble (retval, BALL_Y, 0.0, CLASS);
	SUIT_deluxeSetInteger (retval, PIXELS_PER_SECOND, 100, CLASS);
	SUIT_deluxeSetBoolean (retval, FILLED, TRUE, CLASS);
	SUIT_deluxeSetColor (retval, BACKGROUND_COLOR, GP_defColor ("black", WHITE_ON_MONO), CLASS);
	SUIT_deluxeSetColor (retval, FOREGROUND_COLOR, GP_defColor ("red", BLACK_ON_MONO), CLASS);
    }
    SUIT_registerInterest (retval, OptimizePaint);

    return (retval);
}

