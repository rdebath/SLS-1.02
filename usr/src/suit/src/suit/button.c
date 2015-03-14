/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "suit.h"

#define ABORT_CALLBACK_FUNCTION "abort callback function"




PRIVATE void ShrinkToFit (SUIT_object o)
{
    static boolean canEnter = TRUE; /* prevents some needless recursion */

    if (canEnter && SUIT_getBoolean (o, SHRINK_TO_FIT)) {
	int w, w2, a, d;
	char *display = SUIT_getEnumString (o, ACTIVE_DISPLAY);
	int border = SUIT_getInteger (o, MARGIN);

	GP_setFont (SUIT_getFont (o, FONT));
	GP_setViewport(SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o)));
	GP_setWindow (OBJECT_WINDOW(o));

	GP_inquireTextExtentWithoutMapping (SUIT_getText (o, LABEL), &w, &a, &d);
	if (SUIT_stringsMatch (display, "button with hotkey")) {
	    GP_inquireTextExtentWithoutMapping (SUIT_getText (o, HOTKEY), &w2, &a, &d);
	    w += w2 + 2*border;
	}
	canEnter = FALSE;
	SUIT_changeObjectSize (o, w+2*border, a+d+2*border);
	canEnter = TRUE;
    }
}


PRIVATE void ShrinkToFitCallback (SUIT_object o, char *propName, char *propType, Pointer new, Pointer old)
{
    if (SUIT_stringsMatch (propName, VIEWPORT) ||
	SUIT_stringsMatch (propName, LABEL) ||
	SUIT_stringsMatch (propName, FONT) ||
	SUIT_stringsMatch (propName, MARGIN) ||
	SUIT_stringsMatch (propName, ACTIVE_DISPLAY) ||
	SUIT_stringsMatch (propName, SHRINK_TO_FIT))
	ShrinkToFit (o);
}


#define BOOL(S)  (SUIT_getBoolean(o,S))?"TRUE":"FALSE"
void DrawLabel (SUIT_object);

PRIVATE void DrawButton (SUIT_object o)
{
    /* pausch hack: */
    if (SUIT_getBoolean (o, INTERMEDIATE_FEEDBACK)) {
	GP_setColor (GP_getDepthColor(SUIT_getColor (o, BACKGROUND_COLOR)));
	GP_fillRectangleCoord (0.0,0.0,1.0,1.0);
	SUIT_borderObject (o);
    }

    if (BILEVEL_DISPLAY) {
	GP_color fg;
	fg = SUIT_getColor(o,FOREGROUND_COLOR);
	if (SUIT_getBoolean (o,INTERMEDIATE_FEEDBACK)) 
	    GP_setColor (GP_defColor(fg.colorName, !fg.blackOnMonochrome));
	else
	    GP_setColor (fg);
    } else {
	if (SUIT_getBoolean (o,DISABLED))  {
	    GP_setColor (SUIT_getColor(o,DISABLED_COLOR));
	    GP_setFillBitmapPattern (4); /* a nice stipple */
	    GP_setFillStyle (BITMAP_PATTERN_OPAQUE);
	} else
	    GP_setColor (SUIT_getColor(o,FOREGROUND_COLOR));
    }
    DrawLabel(o);
}


#ifdef IS_THIS_REALLY_NEEDED_ANYMORE
PRIVATE void DrawRoundtangle (SUIT_object o)
{
    SUIT_viewport vp;
    int short_side, radius, diameter, x1, y1, x2, y2;
    double scale_factor;
    
    vp = SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o));
    x1 = vp.bottom_left.x;
    y1 = vp.bottom_left.y;
    x2 = vp.top_right.x;
    y2 = vp.top_right.y;
    
    if ((x2 - x1) < (y2 - y1))
	short_side = x2 - x1;
    else
	short_side = y2 - y1;
    
    
    scale_factor = SUIT_getDouble (o, ROUNDING_FACTOR);
    
    /* ensure that the rounding stays "normal" */
    if (scale_factor > 0.5)
    {
	scale_factor = 0.5;
	SUIT_deluxeSetDouble (o, ROUNDING_FACTOR, scale_factor, CLASS);
    }
    if (scale_factor < 0)
    {
	scale_factor = 0;
	SUIT_deluxeSetDouble (o, ROUNDING_FACTOR, scale_factor, CLASS);
    }
    
    radius = (int) (short_side * scale_factor);
    diameter = radius * 2;
    
      /*
      SRGP_ellipseArc (SRGP_defRectangle (x1, y2 - diameter, x1 + diameter,  y2), 270, 360);
      SRGP_ellipseArc (SRGP_defRectangle (x2 - diameter, y2 - diameter, x2, y2), 0, 90);
      SRGP_ellipseArc (SRGP_defRectangle (x1, y1, x1 + diameter, y1 + diameter), 180, 270);
      SRGP_ellipseArc (SRGP_defRectangle (x2 - diameter, y1, x2, y1 + diameter), 90, 180); 
      */
    
    GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
    SRGP_fillEllipseArc (SRGP_defRectangle (x2 - diameter, y2 - diameter, x2, y2), 0, 90);
    SRGP_fillEllipseArc (SRGP_defRectangle (x1, y2 - diameter, x1 + diameter,  y2), 90, 180);
    SRGP_fillEllipseArc (SRGP_defRectangle (x1, y1, x1 + diameter, y1 + diameter), 180, 270);
    SRGP_fillEllipseArc (SRGP_defRectangle (x2 - diameter, y1, x2, y1 + diameter), 270, 360);
    SRGP_fillRectangleCoord (x1+radius,y1,x2-radius,y2);
    SRGP_fillRectangleCoord (x1,y1+radius,x2,y2-radius);

    GP_setColor (SUIT_getColor (o,BORDER_COLOR));
    SRGP_ellipseArc (SRGP_defRectangle (x2 - diameter, y2 - diameter, x2, y2), 0, 90);
    SRGP_ellipseArc (SRGP_defRectangle (x1, y2 - diameter, x1 + diameter,  y2), 90, 180);
    SRGP_ellipseArc (SRGP_defRectangle (x1, y1, x1 + diameter, y1 + diameter), 180, 270);
    SRGP_ellipseArc (SRGP_defRectangle (x2 - diameter, y1, x2, y1 + diameter), 270, 360);

    GP_setColor (SUIT_getColor (o, BORDER_COLOR));
    GP_setLineWidth (SUIT_getInteger (o, BORDER_WIDTH));
    
    SRGP_lineCoord (x1 + radius, y1, x2 - radius, y1);
    SRGP_lineCoord (x1 + radius, y2, x2 - radius, y2);
    SRGP_lineCoord (x1, y1 + radius, x1, y2 - radius);
    SRGP_lineCoord (x2, y1 + radius, x2, y2 - radius);

    DrawButton (o);
}
#endif



PRIVATE void DrawButtonWithHotKey (SUIT_object o)
{
    if (SUIT_getBoolean (o, INTERMEDIATE_FEEDBACK)) {
	GP_setLineWidth (5);
	GP_rectangleCoord (0.0, 0.0, 1.0, 1.0);
    }
    GP_setColor (SUIT_getColor(o,BACKGROUND_COLOR));
    GP_fillRectangleCoord(0.0,0.0,1.0,1.0);
    SUIT_borderObject (o);
    
    if (SUIT_getBoolean(o,DISABLED))
      GP_setColor (SUIT_getColor(o,DISABLED_COLOR));
    else
      GP_setColor (SUIT_getColor(o,FOREGROUND_COLOR));
    
    GP_setViewport (GP_mapRectangle(GP_defRectangle(0.05,0.05,0.95,0.95)));
    GP_justifyText (SUIT_getText (o, LABEL), JUSTIFY_CENTER_LEFT);
    GP_justifyText (SUIT_getText (o, HOTKEY), JUSTIFY_CENTER_RIGHT);
}



PRIVATE void HitButton (SUIT_object o, SUIT_event input)
{
    void (*func)(SUIT_object);
    boolean pushed = input.worldLocation.x >= 0.0 && input.worldLocation.x <= 1.0 &&
	             input.worldLocation.y >= 0.0 && input.worldLocation.y <= 1.0;

    if (SUIT_getBoolean (o, DISABLED)) 
      return;

    if (input.type == CLICK || input.type == KEYSTROKE) {
	/* This is a bit of animation to make it look like the button has been depressed. */
	GP_time start,now;
	SUIT_deluxeSetBoolean (o, INTERMEDIATE_FEEDBACK, TRUE, OBJECT);
	SUIT_setBoolean (o, BORDER_RAISED, FALSE);
	SUIT_setBoolean (o, HAS_BACKGROUND, FALSE);
	SUIT_paintObject (o);  /* Surely I'm going to Hell for this one! */
	now = start = GP_getCurrentTime();
	while (GP_timeDifference(now,start) < 100)
	    now = GP_getCurrentTime();
    }

    if ((input.type == MOUSE_MOTION || input.type == MOUSE_UP)
	&& !SUIT_getBoolean(o, GOT_A_MOUSE_DOWN))
	return;

    if (input.type == MOUSE_DOWN || input.type == MOUSE_MOTION) {
	SUIT_deluxeSetBoolean (o, INTERMEDIATE_FEEDBACK, pushed, OBJECT);
	if (input.type == MOUSE_DOWN) {
	    SUIT_deluxeSetBoolean (o, GOT_A_MOUSE_DOWN, TRUE, OBJECT);
	    SUIT_makePropertyTemporary (o, GOT_A_MOUSE_DOWN, OBJECT);
	}
	SUIT_setBoolean(o, BORDER_RAISED, !pushed); 
	SUIT_setBoolean(o, HAS_BACKGROUND, !pushed); 
	if (input.type == MOUSE_DOWN)
	    SUIT_reportMouseMotion (o, UNTIL_MOUSE_UP);
    } else {
	SUIT_deluxeSetBoolean (o, INTERMEDIATE_FEEDBACK, FALSE, OBJECT);
	SUIT_setBoolean(o, BORDER_RAISED, TRUE); 
	SUIT_setBoolean(o, HAS_BACKGROUND, TRUE); 
	SUIT_deluxeSetBoolean (o, GOT_A_MOUSE_DOWN, FALSE, OBJECT);
	SUIT_makePropertyTemporary (o, GOT_A_MOUSE_DOWN, OBJECT);
	if (pushed) {
	    func = (SUIT_callbackFunctionPtr) SUIT_getFunctionPointer (o, CALLBACK_FUNCTION);
	    if (func != NULL)
		func (o);
	}
    }
}




PRIVATE char *just[] = { "left", "center", "right" };


SUIT_object SUIT_createButton (char *name, SUIT_callbackFunctionPtr callback)
{
    SUIT_object o;
    static boolean firstTime = TRUE;
    
    o = SUIT_createObject (name, "button");

    SUIT_addDisplayToObject (o, "button with hotkey", HitButton, DrawButtonWithHotKey);
    SUIT_addDisplayToObject (o, "standard", HitButton, DrawButton);

    SUIT_registerInterest (o, ShrinkToFitCallback);
    SUIT_setText(o, LABEL, name);

    SUIT_deluxeSetBoolean (o, INTERMEDIATE_FEEDBACK, FALSE, OBJECT);
    SUIT_makePropertyTemporary (o, INTERMEDIATE_FEEDBACK, OBJECT);
    SUIT_deluxeSetBoolean (o, GOT_A_MOUSE_DOWN, FALSE, OBJECT);
    SUIT_makePropertyTemporary (o, GOT_A_MOUSE_DOWN, OBJECT);
    SUIT_deluxeSetBoolean(o, DISABLED, FALSE, OBJECT);
    SUIT_setFunctionPointer (o, CALLBACK_FUNCTION, (SUIT_functionPointer) callback);

    if ( firstTime ) {
	SUIT_deluxeSetBoolean (o, SHRINK_TO_FIT, TRUE, CLASS);
	SUIT_deluxeSetEnum (o, JUSTIFICATION, SUIT_defEnum("center", 3, just), CLASS);
	/*SUIT_deluxeSetDouble (o, ROUNDING_FACTOR, 0.5, CLASS);*/
	SUIT_deluxeSetColor  (o, DISABLED_COLOR, GP_defColor ("white", WHITE_ON_MONO), CLASS);
        firstTime = FALSE;
    }
    return (o);

}



PRIVATE void simpleDoneFunction (SUIT_object o)
{
    void (*func)(SUIT_object);
    if ((func = (SUIT_callbackFunctionPtr) SUIT_getFunctionPointer (o, DONE_CALLBACK_FUNCTION)) != NULL)
	func (o);
    SUIT_done(SAVE_SUI_FILE, EXIT_APPLICATION);
}

SUIT_object SUIT_createDoneButton (SUIT_callbackFunctionPtr callback)
{
    SUIT_object retval = SUIT_createButton("Done", simpleDoneFunction);
    SUIT_setFunctionPointer (retval, DONE_CALLBACK_FUNCTION, (SUIT_functionPointer) callback);
    return retval;
}


PRIVATE void simpleAbortFunction (SUIT_object o)
{
    void (*func)(SUIT_object);
    if ((func = (SUIT_callbackFunctionPtr) SUIT_getFunctionPointer (o, ABORT_CALLBACK_FUNCTION)) != NULL)
	func (o);
    SUIT_done(DO_NOT_SAVE_SUI_FILE, EXIT_APPLICATION);
}

SUIT_object SUIT_createAbortButton (SUIT_callbackFunctionPtr callback)
{
    SUIT_object retval = SUIT_createButton("Abort", simpleAbortFunction);
    SUIT_setFunctionPointer (retval, ABORT_CALLBACK_FUNCTION, (SUIT_functionPointer) callback);
    return retval;
}


/**************************************************************************/
/* LABEL WIDGET ... */

void DrawLabel (SUIT_object o)
{
    char *j = SUIT_getEnumString (o, JUSTIFICATION);
    if (SUIT_stringsMatch(j,"left"))
	GP_justifyText (SUIT_getText (o, LABEL), JUSTIFY_CENTER_LEFT);
    else if (SUIT_stringsMatch(j,"center"))
	GP_justifyText (SUIT_getText (o, LABEL), JUSTIFY_CENTER);
    else if (SUIT_stringsMatch(j,"right"))
	GP_justifyText (SUIT_getText (o, LABEL), JUSTIFY_CENTER_RIGHT);
}



SUIT_object SUIT_createLabel(char *name)
{
    SUIT_object o = SUIT_createObject (name, LABEL);
    static boolean firstTime = TRUE;
    
    SUIT_addDisplayToObject (o, "standard", NULL, DrawLabel);

    SUIT_registerInterest (o, ShrinkToFitCallback);
    SUIT_setText (o, LABEL, name);

    if ( firstTime )
    {
	SUIT_deluxeSetEnum (o, JUSTIFICATION, SUIT_defEnum("center", 3, just), CLASS);
	SUIT_deluxeSetBoolean (o, SHRINK_TO_FIT, TRUE, CLASS);
	/*SUIT_deluxeSetDouble (o, ROUNDING_FACTOR, 0.5, CLASS);*/
	SUIT_deluxeSetBoolean(o, HAS_BORDER, FALSE, CLASS);
	firstTime = FALSE;
    }
    return (o);
}
