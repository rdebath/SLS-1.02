/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "suit.h"


PRIVATE void HitArrow(SUIT_object o, SUIT_event input)
{
    void (*func)(SUIT_object) = (SUIT_callbackFunctionPtr) SUIT_getFunctionPointer (o, CALLBACK_FUNCTION);
    if (input.type == MOUSE_DOWN) {
	SUIT_deluxeSetBoolean (o, DRAW_FILLED, TRUE, OBJECT);
	SUIT_makePropertyTemporary (o, DRAW_FILLED, OBJECT);
	SUIT_reportMouseMotion (o, UNTIL_MOUSE_UP);
    } else if (SUIT_getBoolean (o, DRAW_FILLED) && input.type == MOUSE_UP) {
	SUIT_deluxeSetBoolean (o, DRAW_FILLED, FALSE, OBJECT);
	SUIT_makePropertyTemporary (o, DRAW_FILLED, OBJECT);
    }
    if (func != NULL)
	func (o);
}



PRIVATE void PaintSimpleArrow(SUIT_object o)
{
    GP_point corners[7];
    char *dir = SUIT_getEnumString(o, DIRECTION);

    if (SUIT_stringsMatch (dir, "up"))
	{
	corners[0] = GP_defPoint(0.5, (1 - (0.1)));
	corners[1] = GP_defPoint(0.9, (1 - (0.5)));
	corners[2] = GP_defPoint(0.7, (1 - (0.5)));
	corners[3] = GP_defPoint(0.7, (1 - (0.9)));
	corners[4] = GP_defPoint(0.3, (1 - (0.9)));
	corners[5] = GP_defPoint(0.3, (1 - (0.5)));
	corners[6] = GP_defPoint(0.1, (1 - (0.5)));
	}
    else if (SUIT_stringsMatch (dir, "down"))
	{
	corners[0] = GP_defPoint (0.5, 0.1);
	corners[1] = GP_defPoint (0.9, 0.5);
	corners[2] = GP_defPoint (0.7, 0.5);
	corners[3] = GP_defPoint (0.7, 0.9);
	corners[4] = GP_defPoint (0.3, 0.9);
	corners[5] = GP_defPoint (0.3, 0.5);
	corners[6] = GP_defPoint (0.1, 0.5);
	}
    else if (SUIT_stringsMatch (dir, "left"))
	{
	corners[0] = GP_defPoint (0.1, 0.5);
	corners[1] = GP_defPoint (0.5, 0.9);
	corners[2] = GP_defPoint (0.5, 0.7);
	corners[3] = GP_defPoint (0.9, 0.7);
	corners[4] = GP_defPoint (0.9, 0.3);
	corners[5] = GP_defPoint (0.5, 0.3);
	corners[6] = GP_defPoint (0.5, 0.1);
	}
    else /* draw it right by default */
	{
	corners[0] = GP_defPoint (1-0.1, 0.5);
	corners[1] = GP_defPoint (1-0.5, 0.9);
	corners[2] = GP_defPoint (1-0.5, 0.7);
	corners[3] = GP_defPoint (1-0.9, 0.7);
	corners[4] = GP_defPoint (1-0.9, 0.3);
	corners[5] = GP_defPoint (1-0.5, 0.3);
	corners[6] = GP_defPoint (1-0.5, 0.1);
	}

    if (SUIT_getBoolean (o, DRAW_FILLED))
	GP_fillPolygon(7, corners);
    else
	GP_polygon (7, corners);

}


PRIVATE void PaintMotifArrow (SUIT_object o)
{
    int df;
    char *dir = SUIT_getEnumString(o, DIRECTION);
    GP_color bg;
    
    bg = SUIT_getColor(o,BACKGROUND_COLOR);
    if (BILEVEL_DISPLAY)
	bg.blackOnMonochrome = !bg.blackOnMonochrome;
    if (SUIT_getBoolean (o, DARKEN_BACKGROUND))
	GP_setColor (GP_getDepthColor(bg));
    else
	GP_setColor (bg);
    GP_fillRectangleCoord (0.0, 0.0, 1.0, 1.0);
    if (SUIT_getBoolean (o, INTERMEDIATE_FEEDBACK))
	df=SUIT_getBoolean(o,DRAW_FILLED);
    else
	df=!SUIT_getBoolean(o,DRAW_FILLED);
    if (SUIT_stringsMatch (dir, "up"))
	GP_beveledTriangleNorth (GP_defPoint(0.5,1.0), GP_defPoint(0.0,0.0), GP_defPoint(1.0,0.0),
				 bg, df,
				 SUIT_getInteger(o,SHADOW_THICKNESS));
    else if (SUIT_stringsMatch (dir, "down"))
	GP_beveledTriangleSouth (GP_defPoint(0.5,0.0), GP_defPoint(1.0,1.0), GP_defPoint(0.0,1.0),
				 bg,
				 !SUIT_getBoolean(o,DRAW_FILLED),
				 SUIT_getInteger(o,SHADOW_THICKNESS));
    else if (SUIT_stringsMatch (dir, "left"))
	GP_beveledTriangleWest (GP_defPoint(0.0,0.5), GP_defPoint(1.0,0.0), GP_defPoint(1.0,1.0),
				bg,
				!SUIT_getBoolean(o,DRAW_FILLED),
				SUIT_getInteger(o,SHADOW_THICKNESS));
    else          /* draw it right by default */
	GP_beveledTriangleEast (GP_defPoint(1.0,0.5), GP_defPoint(0.0,1.0), GP_defPoint(0.0,0.0),
				bg,
				!SUIT_getBoolean(o,DRAW_FILLED),
				SUIT_getInteger(o,SHADOW_THICKNESS));
}


PRIVATE char *directions[] = { "up", "down", "left", "right" };

SUIT_object SUIT_createArrowButton(char *name, void (*callback)(SUIT_object))
{
    SUIT_object o;
    static boolean firsttime = TRUE;

    o = SUIT_createObject(name, "arrow button");
    SUIT_addDisplayToObject(o, "simple arrow", HitArrow, PaintSimpleArrow);
    SUIT_addDisplayToObject(o, "motif", HitArrow, PaintMotifArrow);
    SUIT_setFunctionPointer (o, CALLBACK_FUNCTION, (SUIT_functionPointer)callback);
    if (firsttime) {
	SUIT_deluxeSetEnum (o, DIRECTION, SUIT_defEnum("up", 4, directions), CLASS);
	SUIT_deluxeSetBoolean (o, BORDER_RAISED, FALSE, CLASS);
	SUIT_deluxeSetInteger (o, SHADOW_THICKNESS, 3, CLASS);
	SUIT_deluxeSetBoolean (o, HAS_BACKGROUND, FALSE, CLASS);
	SUIT_deluxeSetBoolean (o, DARKEN_BACKGROUND, TRUE, CLASS);
	firsttime = FALSE;
    }
    return o;
}
