/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "suit.h"


/**************************************************************************/
/* SWITCH WIDGET .... */


PRIVATE void ShrinkToFit (SUIT_object o)
{
    if (SUIT_getBoolean (o, SHRINK_TO_FIT)) {
	int w, a, d, border;
	GP_setFont (SUIT_getFont (o, FONT));
	GP_setViewport(SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o)));
	GP_setWindow (OBJECT_WINDOW(o));
	GP_inquireTextExtentWithoutMapping (SUIT_getText (o, LABEL), &w, &a, &d);
	border = SUIT_deluxeGetInteger (NULL, MARGIN, GLOBAL);
	SUIT_changeObjectSize (o, a+d+w+4*border, a+d+2*border);
    }
}



PRIVATE void ShrinkToFitCallback (SUIT_object o, char *propName, char *propType, Pointer new, Pointer old)
{
    if (SUIT_stringsMatch (propName, VIEWPORT) ||
	SUIT_stringsMatch (propName, FONT) ||
	SUIT_stringsMatch (propName, LABEL) ||
	SUIT_stringsMatch (propName, SHRINK_TO_FIT))
	ShrinkToFit (o);
}


static void DrawMotif (SUIT_object o)
{
    int	w, a, d, border, size, gap;
    int height, x1, y1, x2, y2;
    rectangle vp;

    vp = SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o));
    height = vp.top_right.y - vp.bottom_left.y;
    x1 = vp.bottom_left.x;
    y1 = vp.bottom_left.y;
    x2 = vp.top_right.x;
    y2 = vp.top_right.y;
    
    SRGP_inquireTextExtent(SUIT_getText(o,LABEL),&w,&a,&d);
    size=a+d;
    border=SUIT_deluxeGetInteger(NULL,MARGIN, GLOBAL);
    gap=(height-size)/2;
    GP_pushGraphicsState();	
    if (SUIT_getBoolean(o,INTERMEDIATE_FEEDBACK))
	GP_beveledBox (SRGP_defRectangle(x1+border,y1+gap,
					 x1+border+size, y2-gap), 
		       SUIT_getColor(o,BACKGROUND_COLOR),
		       SUIT_getBoolean(o,CURRENT_VALUE), 2);
    else
	GP_beveledBox (SRGP_defRectangle(x1+border,y1+gap,
					 x1+border+size, y2-gap), 
		       SUIT_getColor(o,BACKGROUND_COLOR),
		       !SUIT_getBoolean(o,CURRENT_VALUE), 2);
    GP_popGraphicsState();
    GP_setViewport(SRGP_defRectangle(x1+size+3*border,y1,x2,y2));
    if (SUIT_getBoolean (o,DISABLED))
	GP_setColor(SUIT_getColor(o,DISABLED_COLOR));
    GP_justifyText(SUIT_getText(o,LABEL),JUSTIFY_CENTER_LEFT);
}



PRIVATE void DrawCheckBox (SUIT_object o)
{
    int w, a, d, border,size,gap;
    SUIT_viewport vp;
    int height;

    vp = SUIT_mapViewportToScreen (o, OBJECT_VIEWPORT(o));
    height = vp.top_right.y-vp.bottom_left.y;
    
    SRGP_inquireTextExtent (SUIT_getText (o, LABEL), &w, &a, &d);
    size = a+d;
    border = SUIT_deluxeGetInteger (NULL, MARGIN, GLOBAL);
    gap = (height-size)/2;
    if (SUIT_getBoolean (o, INTERMEDIATE_FEEDBACK)) {
	SRGP_setLineWidth (2);
	SRGP_rectangleCoord (vp.bottom_left.x+border, vp.bottom_left.y+gap,
			     vp.bottom_left.x+border+size, vp.top_right.y-gap);
	SRGP_setLineWidth (1);
    } else
	SRGP_rectangleCoord (vp.bottom_left.x+border, vp.bottom_left.y+gap,
			     vp.bottom_left.x+border+size, vp.top_right.y-gap);
    if (SUIT_getBoolean (o, CURRENT_VALUE)) {
	SRGP_lineCoord (vp.bottom_left.x+border, vp.bottom_left.y+gap, vp.bottom_left.x+border+size, vp.top_right.y-gap);
	SRGP_lineCoord (vp.bottom_left.x+border+size, vp.bottom_left.y+gap, vp.bottom_left.x+border, vp.top_right.y-gap);
    }
    GP_setViewport (SRGP_defRectangle(vp.bottom_left.x+size+3*border,vp.bottom_left.y,vp.top_right.x,vp.top_right.y));
    if (SUIT_getBoolean (o,DISABLED))
	GP_setColor(SUIT_getColor(o,DISABLED_COLOR));
    GP_justifyText (SUIT_getText(o, LABEL), JUSTIFY_CENTER_LEFT);
}



PRIVATE void DrawRadioButtonSwitch (SUIT_object o)
{
    int w, a, d, border,size,gap;
    SUIT_viewport vp;
    int height;
    
    vp = SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o));
    height = vp.top_right.y-vp.bottom_left.y;
    SRGP_inquireTextExtent (SUIT_getText (o, LABEL), &w, &a, &d);
    size = a+d;
    border = SUIT_deluxeGetInteger (NULL, MARGIN, GLOBAL);
    gap = (height-size)/2;
    if (SUIT_getBoolean (o, INTERMEDIATE_FEEDBACK)) {
	SRGP_setLineWidth (2);
	SRGP_ellipse (SRGP_defRectangle(vp.bottom_left.x+border, vp.bottom_left.y+gap,
					vp.bottom_left.x+border+size, vp.top_right.y-gap));
	SRGP_setLineWidth (1);
    } else
	SRGP_ellipse (SRGP_defRectangle(vp.bottom_left.x+border, vp.bottom_left.y+gap,
					vp.bottom_left.x+border+size, vp.top_right.y-gap));
    if (SUIT_getBoolean (o, CURRENT_VALUE))
	SRGP_fillEllipse (SRGP_defRectangle(vp.bottom_left.x+border+size/4, vp.bottom_left.y+gap+size/4,
					    vp.bottom_left.x+border+size-size/4, vp.top_right.y-gap-size/4));
    GP_setViewport (SRGP_defRectangle(vp.bottom_left.x+size+3*border,vp.bottom_left.y,vp.top_right.x,vp.top_right.y));
    if (SUIT_getBoolean (o,DISABLED))
	GP_setColor(SUIT_getColor(o,DISABLED_COLOR));
    GP_justifyText (SUIT_getText(o, LABEL), JUSTIFY_CENTER_LEFT);
}



PRIVATE void DrawMotifRadioButtonSwitch (SUIT_object o)
{
    int w, a, d, border,size,gap;
    char *label = SUIT_getText (o, LABEL);
    int height, x1, y1, x2, y2;
    rectangle vp;
    
    vp = SUIT_mapViewportToScreen(o, OBJECT_VIEWPORT(o));
    height = vp.top_right.y - vp.bottom_left.y;
    x1 = vp.bottom_left.x;
    y1 = vp.bottom_left.y;
    x2 = vp.top_right.x;
    y2 = vp.top_right.y;

    SRGP_inquireTextExtent (label, &w, &a, &d);
    size = a+d;
    border = SUIT_deluxeGetInteger (NULL, MARGIN, GLOBAL);
    gap = (height-size)/2;
    if (SUIT_getBoolean (o, INTERMEDIATE_FEEDBACK))
	GP_beveledDiamond (SRGP_defRectangle(x1+border, y1+gap, x1+border+size, y2-gap),
			   SUIT_getColor(o,BACKGROUND_COLOR),
			   SUIT_getBoolean(o,CURRENT_VALUE), 2);
    else
	GP_beveledDiamond (SRGP_defRectangle(x1+border, y1+gap, x1+border+size, y2-gap),
			   SUIT_getColor(o,BACKGROUND_COLOR),
			   !SUIT_getBoolean(o,CURRENT_VALUE), 2);
    GP_setViewport (SRGP_defRectangle(x1+size+3*border,y1,x2,y2));
    if (SUIT_getBoolean (o,DISABLED))
	GP_setColor(SUIT_getColor(o,DISABLED_COLOR));
    GP_justifyText (label, JUSTIFY_CENTER_LEFT);
}



PRIVATE void HitOnOffSwitch (SUIT_object o, SUIT_event e)
{
    boolean inside = e.worldLocation.x >= 0.0 && e.worldLocation.x <= 1.0 &&
	e.worldLocation.y >= 0.0 && e.worldLocation.y <= 1.0;
    void (*funct)(SUIT_object);
    
    if (SUIT_getBoolean(o,DISABLED)) 
	return;
    
    if (e.type == MOUSE_DOWN) {
	SUIT_deluxeSetBoolean (o, INTERMEDIATE_FEEDBACK, TRUE, OBJECT);
	SUIT_makePropertyTemporary (o, INTERMEDIATE_FEEDBACK, OBJECT);
	SUIT_reportMouseMotion (o, UNTIL_MOUSE_UP);
    } else if (e.type == MOUSE_MOTION) {
	SUIT_deluxeSetBoolean (o, INTERMEDIATE_FEEDBACK, inside, OBJECT);
	SUIT_makePropertyTemporary (o, INTERMEDIATE_FEEDBACK, OBJECT);
    } else {
	SUIT_deluxeSetBoolean (o, INTERMEDIATE_FEEDBACK, FALSE, OBJECT);
	SUIT_makePropertyTemporary (o, INTERMEDIATE_FEEDBACK, OBJECT);
	if (inside) {
	    SUIT_setBoolean (o, CURRENT_VALUE, !(SUIT_getBoolean (o, CURRENT_VALUE)));
	    funct = (SUIT_callbackFunctionPtr) SUIT_getFunctionPointer (o, CALLBACK_FUNCTION);
	    if (funct != NULL) 
		funct (o);
	}
    }
}


SUIT_object SUIT_createOnOffSwitch (char *name, void (*hitProc)())
{
    SUIT_object o;
    static boolean firsttime = TRUE;
    
    o = SUIT_createObject(name, "on/off switch");
    SUIT_addDisplayToObject(o, "dot in circle", HitOnOffSwitch, DrawRadioButtonSwitch);
    SUIT_addDisplayToObject(o, "check box", HitOnOffSwitch, DrawCheckBox);
    SUIT_addDisplayToObject(o, "motif diamond", HitOnOffSwitch,DrawMotifRadioButtonSwitch);
    SUIT_addDisplayToObject(o, "motif square", HitOnOffSwitch, DrawMotif);
    
    SUIT_registerInterest (o, ShrinkToFitCallback);
    
    SUIT_setText (o, LABEL, name);
    SUIT_deluxeSetBoolean (o, INTERMEDIATE_FEEDBACK, FALSE, OBJECT);
    SUIT_makePropertyTemporary (o, INTERMEDIATE_FEEDBACK, OBJECT);
    SUIT_deluxeSetFunctionPointer (o, CALLBACK_FUNCTION, hitProc, OBJECT);
    SUIT_setBoolean (o, CURRENT_VALUE, FALSE);
    SUIT_setBoolean (o, DISABLED, FALSE);
    if (firsttime) {
	SUIT_deluxeSetColor  (o, DISABLED_COLOR, GP_defColor ("white", WHITE_ON_MONO), CLASS);
	SUIT_deluxeSetBoolean (o, SHRINK_TO_FIT, TRUE, CLASS);
	firsttime = FALSE;
    }
    ShrinkToFit (o);
    
    return(o);
}
