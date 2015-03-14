/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "suit.h"

#define OLD_CHOICE "old choice"
#define INTERMEDIATE_CHOICE "intermediate choice"
#define DRAW_INTER_PUSHED "draw inter pushed"


PRIVATE void OptimizePaint (SUIT_object o, char *propName, char *propType, Pointer new, Pointer old)
{
    if ( !(SUIT_stringsMatch (propName, CURRENT_VALUE) ||
	   SUIT_stringsMatch (propName, OLD_CHOICE) ||
	   SUIT_stringsMatch (propName, DRAW_INTER_PUSHED) ||
	   SUIT_stringsMatch (propName, INTERMEDIATE_CHOICE)) )
	OBJECT_OPTIMIZED(o) = FALSE;
/*
    if (SUIT_stringsMatch (propName, CURRENT_VALUE) &&
	SUIT_stringsMatch (propType, "SUIT_enum") && old != NULL) {
	SUIT_enum olde;
	olde = * (SUIT_enum *) old;
	SUIT_setInteger (o, OLD_CHOICE, olde.currentChoice);
    }
*/
}



void SUIT_pressThisRadioButton (SUIT_object o, char *name)
{
    SUIT_setEnumString (o, CURRENT_VALUE, name);
}



void SUIT_addButtonToRadioButtons (SUIT_object o, char *name)
{
    SUIT_enum current;
    boolean new;
    SUIT_enum newenum;
    DynArray newarray = DynCreate(sizeof(char*), 1);

    current = SUIT_getEnum (o, CURRENT_VALUE);
    new = SUIT_stringsMatch (SUIT_getEnumSelection(current), "default value");
    if (!new) {
	int i;
	for (i=0; i < DynSize(current.choices); i++) {
	    char *str = * (char **) DynGet (current.choices, i);
	    DynAdd (newarray, (void *)&str);
	}
    }
    DynAdd (newarray, (void *)&name);
    newenum.choices = newarray;
    newenum.currentChoice = DynHigh(newarray);
    SUIT_setEnum (o, CURRENT_VALUE, newenum);
}



PRIVATE void PaintButton (int i, SUIT_object o, SUIT_enum current, rectangle vp, boolean pushedIn)
{
    int j = DynSize(current.choices) - i - 1;
    int x2 = vp.top_right.x, x1 = vp.bottom_left.x;
    int avg_height = (vp.top_right.y - vp.bottom_left.y)/DynSize(current.choices);
    int y1 = j*avg_height + vp.bottom_left.y;
    int y2 = (j+1)*avg_height + vp.bottom_left.y;
    int border = SUIT_getInteger (o, MARGIN);
    int w, a, d, size, gap;
    char *label;
    
    if (i < 0 || i > DynHigh(current.choices))
	return;
    label = * (char **) DynGet (current.choices, i);
    SRGP_inquireTextExtent ("Arbitrary", &w, &a, &d);
    size = a+d;
    gap = (avg_height - (a+d))/2;

    GP_beveledDiamond (SRGP_defRectangle(x1+border, y1+gap, x1+border+size, y2-gap),
		       SUIT_getColor(o,BACKGROUND_COLOR), !pushedIn, 2);
    GP_setViewport (SRGP_defRectangle(x1+size+3*border,y1,x2,y2));
    label = * (char **) DynGet (current.choices, i);
    GP_justifyText (label, JUSTIFY_CENTER_LEFT);
}



PRIVATE void PaintRadioButtons (SUIT_object o)
{
    SUIT_enum current;
    rectangle vp;
    int i;

    current = SUIT_getEnum (o, CURRENT_VALUE);
    vp = SUIT_mapViewportToScreen (o, OBJECT_VIEWPORT(o));
    if (OBJECT_OPTIMIZED(o)) {
	int inter = SUIT_getInteger (o, INTERMEDIATE_CHOICE);
	int old = SUIT_getInteger (o, OLD_CHOICE);
	PaintButton (current.currentChoice, o, current, vp, TRUE);
	if (old >= 0 && old != current.currentChoice)
	    PaintButton (old, o, current, vp, FALSE);
	if (inter >= 0)
	    PaintButton (inter, o, current, vp, SUIT_getBoolean(o, DRAW_INTER_PUSHED));
    } else
	for (i=0; i < DynSize(current.choices); i++)
	    PaintButton (i, o, current, vp, (i == current.currentChoice));

    SUIT_suspendMarkingRedisplay (o);
    SUIT_setInteger (o, OLD_CHOICE, current.currentChoice);
    SUIT_resumeMarkingRedisplay (o);

    OBJECT_OPTIMIZED(o) = TRUE;
}



PRIVATE void HitRadioButtons (SUIT_object o, SUIT_event e)
{
    boolean inside = (e.worldLocation.x >= 0.0 && e.worldLocation.x <= 1.0 && 
		      e.worldLocation.y >= 0.0 && e.worldLocation.y <= 1.0);
    SUIT_enum current;
    int which;
    int inter = SUIT_getInteger (o, INTERMEDIATE_CHOICE);

    current = SUIT_getEnum (o, CURRENT_VALUE);
    which = MIN((1.0 - e.worldLocation.y) * DynSize(current.choices), DynHigh(current.choices));
    if (e.type == MOUSE_DOWN) {
	SUIT_setInteger (o, INTERMEDIATE_CHOICE, inter = which);
	SUIT_reportMouseMotion (o, UNTIL_MOUSE_UP);
    } else if (e.type == CLICK || e.type == KEYSTROKE || 
	       (e.type == MOUSE_UP && inside && which == inter)) {
	SUIT_callbackFunctionPtr func;
	SUIT_setInteger (o, INTERMEDIATE_CHOICE, -1);
	/* SUIT_setInteger (o, OLD_CHOICE, current.currentChoice); */
	current.currentChoice = which;
	SUIT_setEnum (o, CURRENT_VALUE, current);
	if ((func = (SUIT_callbackFunctionPtr) SUIT_getFunctionPointer (o, CALLBACK_FUNCTION)) != NULL)
	    func (o);
    }
    if (inter == current.currentChoice)
	SUIT_setBoolean (o, DRAW_INTER_PUSHED, (!inside || which != inter));
    else
	SUIT_setBoolean (o, DRAW_INTER_PUSHED, (inside && which == inter));
}



SUIT_object SUIT_createRadioButtons (char *name, SUIT_callbackFunctionPtr funct)
{
    SUIT_object o = SUIT_createObject (name, "radio buttons");
    SUIT_addDisplayToObject (o, "standard", HitRadioButtons, PaintRadioButtons);

    SUIT_setBoolean (o, SHRINK_TO_FIT, FALSE);
    SUIT_setFunctionPointer (o, CALLBACK_FUNCTION, (SUIT_functionPointer) funct);

    SUIT_setInteger (o, INTERMEDIATE_CHOICE, -1);
      SUIT_makePropertyTemporary (o, INTERMEDIATE_CHOICE, OBJECT);
      SUIT_lockProperty (o, INTERMEDIATE_CHOICE, OBJECT);
    SUIT_setInteger (o, OLD_CHOICE, -1);
      SUIT_makePropertyTemporary (o, OLD_CHOICE, OBJECT);
      SUIT_lockProperty (o, OLD_CHOICE, OBJECT);
    SUIT_setBoolean (o, DRAW_INTER_PUSHED, FALSE);
      SUIT_makePropertyTemporary (o, DRAW_INTER_PUSHED, OBJECT);
      SUIT_lockProperty (o, DRAW_INTER_PUSHED, OBJECT);

    SUIT_registerInterest (o, OptimizePaint);

    return o;
}
