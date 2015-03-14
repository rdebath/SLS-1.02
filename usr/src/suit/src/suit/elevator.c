/* (C) Copyright 1990, 1991, 1992 the University of Virginia */

#include "suit.h"


PRIVATE void OptimizePaint (SUIT_object o, char *propName, char *propType, Pointer new, Pointer old)
{
    if (!(SUIT_stringsMatch (propName, CURRENT_VALUE) ||
	  SUIT_stringsMatch (propName, LAST_DRAWN_CURRENT_VALUE) ||
	  SUIT_stringsMatch (propName, SLIDING) ||
	  SUIT_stringsMatch (propName, "offset")))
	OBJECT_OPTIMIZED(o) = FALSE;
}


PRIVATE void DrawMotifElevator (SUIT_object o)
{
    double percent = SUIT_getDouble (o, PERCENT_FULL);
    double current;
    SUIT_viewport vp;
    GP_color back;
    boolean vert;

    back = SUIT_getColor (o, BACKGROUND_COLOR);
    vp = SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o));
    if (BILEVEL_DISPLAY)
	back.blackOnMonochrome = !back.blackOnMonochrome;
    if (SUIT_stringsMatch (OBJECT_CLASS(SUIT_getParent(o)), "bounded value"))
	vp = SUIT_mapViewportToScreen(SUIT_getParent(o), OBJECT_VIEWPORT(SUIT_getParent(o)));
    vert = vp.top_right.x - vp.bottom_left.x <= vp.top_right.y - vp.bottom_left.y;

    if (vert)
	current = (1.0-SUIT_getDouble (o, CURRENT_VALUE)) * (1.0 - percent);
    else
	current = SUIT_getDouble (o, CURRENT_VALUE) * (1.0 - percent);

    GP_setColor (GP_getDepthColor(back));
    if (OBJECT_OPTIMIZED(o)) {
	double last = SUIT_getDouble (o, LAST_DRAWN_CURRENT_VALUE);
	if (vert)
	    GP_fillRectangleCoord (0.0, last, 1.0, last+percent);
	else
	    GP_fillRectangleCoord (last, 0.0, last+percent, 1.0);
    } else
	GP_fillRectangleCoord (0.0, 0.0, 1.0, 1.0);
    GP_setColor (SUIT_getColor (o, FOREGROUND_COLOR));
    if (vert)
	GP_beveledBox (GP_mapRectangle(GP_defRectangle(0.0, current, 1.0, current+percent)), back, TRUE, 3);
    else
	GP_beveledBox (GP_mapRectangle(GP_defRectangle(current, 0.0, current+percent, 1.0)), back, TRUE, 3);

    SUIT_suspendMarkingRedisplay (o);
    SUIT_deluxeSetDouble (o, LAST_DRAWN_CURRENT_VALUE, current, OBJECT);
    SUIT_makePropertyTemporary (o, LAST_DRAWN_CURRENT_VALUE, OBJECT);
    SUIT_resumeMarkingRedisplay (o);
    OBJECT_OPTIMIZED(o) = TRUE;
}



PRIVATE void HitElevator (SUIT_object o, SUIT_event e)
{
    SUIT_viewport vp;
    boolean vert;
    void (*func)(SUIT_object);
    double percent = SUIT_getDouble (o, PERCENT_FULL);
    double current = SUIT_getDouble(o, CURRENT_VALUE);
    double lower = current * (1.0 - percent), upper = lower + percent;    
    double where;

    vp = SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o));
    if (SUIT_stringsMatch (OBJECT_CLASS(SUIT_getParent(o)), "bounded value"))
	vp = SUIT_mapViewportToScreen(SUIT_getParent(o), OBJECT_VIEWPORT(SUIT_getParent(o)));
    vert = vp.top_right.x - vp.bottom_left.x < vp.top_right.y - vp.bottom_left.y;
    where = vert? (1.0 - e.worldLocation.y) : e.worldLocation.x;

    if (where < 0.0)
	where = 0.0;
    else if (where > 1.0)
	where = 1.0;
    if ((where >= lower && where <= upper) || SUIT_getBoolean (o, SLIDING)) {
	if (e.type == MOUSE_DOWN) {
	    SUIT_reportMouseMotion (o, UNTIL_MOUSE_UP);
	    SUIT_deluxeSetDouble (o, "offset", where-lower, OBJECT);
	    SUIT_makePropertyTemporary (o, "offset", OBJECT);
	    SUIT_deluxeSetBoolean (o, SLIDING, TRUE, OBJECT);
	    SUIT_makePropertyTemporary (o, SLIDING, OBJECT);
	    return;
	} else if (e.type == MOUSE_MOTION || e.type == MOUSE_UP) {
	    current = (where - SUIT_getDouble(o, "offset"))/(1.0-percent);
	    if (current > 1.0)
		current = 1.0;
	    else if (current < 0.0)
		current = 0.0;
	    SUIT_setDouble (o, CURRENT_VALUE, current);
	    if (e.type == MOUSE_UP) {
		SUIT_deluxeSetBoolean (o, SLIDING, FALSE, OBJECT);
		SUIT_makePropertyTemporary (o, SLIDING, OBJECT);
	    }
	}
    } else if (where > current) {
	current += percent/(1.0-percent);
	if (current > 1.0)
	    current = 1.0;
	SUIT_setDouble (o, CURRENT_VALUE, current);
    } else {
	current -= percent/(1.0-percent);
	if (current < 0.0)
	    current = 0.0;
	SUIT_setDouble (o, CURRENT_VALUE, current);
    }
    if ((func = (SUIT_callbackFunctionPtr) SUIT_getFunctionPointer (o, CALLBACK_FUNCTION)) != NULL)
	func (o);
}



SUIT_object SUIT_createElevator (char * name, void (*callback)(SUIT_object))
{
    SUIT_object o;
    static boolean firsttime = TRUE;

    o = SUIT_createObject (name, "elevator");
    /*SUIT_addDisplayToObject(o, "standard", HitElevator, DrawElevator);*/
    SUIT_addDisplayToObject(o, "motif", HitElevator, DrawMotifElevator);
    SUIT_deluxeSetFunctionPointer (o, CALLBACK_FUNCTION, (SUIT_functionPointer)callback, OBJECT);
    SUIT_registerInterest (o, OptimizePaint);
    if (firsttime) {
	SUIT_deluxeSetBoolean (o, HAS_BACKGROUND, FALSE, CLASS);
	SUIT_deluxeSetBoolean (o, BORDER_RAISED, FALSE, CLASS);
	firsttime = FALSE;
    }
    return o;
}
