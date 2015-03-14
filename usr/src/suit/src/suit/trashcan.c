/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "suit.h"

/*----------------------------------------------------------*/
/*    BILLBOARD WIDGET    */

PRIVATE void findBestFontSize(SUIT_object o, char *message)
     /*  Finds the largest font size that keeps message in o's window */
{
#if defined (X_WINDOWS)
#    define MAXSIZE 25
#elif defined (IBM_PC)
#    define MAXSIZE 25
#else
#    define MAXSIZE 500
#endif
    
    double  messWidth, messHeight, messdHi;
    int  size = 10;
    boolean  windowBigger = TRUE; 
    GP_font theFont;
    
    theFont = SUIT_getFont(o, FONT);
    GP_setFont (GP_defFont (theFont.family, "bold", (double)size));
    GP_inquireTextExtent(message, &messWidth, &messHeight, &messdHi);
    if (messWidth > 0.74) windowBigger = 0;
    while (windowBigger) {
	GP_setFont (GP_defFont (theFont.family, theFont.style, (double)(++size)));
	GP_inquireTextExtent(message, &messWidth, &messHeight, &messdHi);
	if ((size == MAXSIZE) || (messWidth > 0.70) || (messHeight > 0.7)) windowBigger = FALSE;
    }
    GP_setFont (GP_defFont (theFont.family, theFont.style, (double)(--size)));
}

PRIVATE void PaintTruck (SUIT_object o)
{
    GP_setWindow (GP_defRectangle(0.0, 0.0, 1.0, 1.0));

    if (!BILEVEL_DISPLAY){
	GP_setColor (SUIT_getColor(o, BODY_COLOR));
	GP_fillRectangle (GP_defRectangle(0.09, 0.3, 0.85, 0.8));
    }

    GP_setColor (SUIT_getColor(o, OUTLINE_COLOR));
    GP_drawRectangle (GP_defRectangle(0.09, 0.3, 0.85, 0.8));

    GP_setColor (SUIT_getColor(o, CAB_COLOR));
    GP_fillRectangle (GP_defRectangle(0.85, 0.3, 0.98, 0.5));

    GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
    GP_fillRectangle (GP_defRectangle(0.93, 0.4, 0.98, 0.5));

    GP_setColor (SUIT_getColor(o, OUTLINE_COLOR));
    GP_drawRectangle (GP_defRectangle(0.93, 0.4, 0.98, 0.5));
    GP_drawRectangle (GP_defRectangle(0.85, 0.3, 0.98, 0.5));

    GP_setColor (SUIT_getColor(o, WHEEL_COLOR));
#define FUDGE 0.025
    GP_fillEllipse (GP_defRectangle(0.19-FUDGE, 0.2-FUDGE, 0.29+FUDGE, 0.3+FUDGE));
    GP_fillEllipse (GP_defRectangle(0.65-FUDGE, 0.2-FUDGE, 0.75+FUDGE, 0.3+FUDGE));
    GP_fillEllipse (GP_defRectangle(0.865-FUDGE, 0.2-FUDGE, 0.965+FUDGE, 0.3+FUDGE));
#undef FUDGE
    
    GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
    GP_fillEllipse (GP_defRectangle(0.225, 0.235, 0.255, 0.265));
    GP_fillEllipse (GP_defRectangle(0.685, 0.235, 0.715, 0.265));
    GP_fillEllipse (GP_defRectangle(0.9 , 0.235, 0.93, 0.265));

    findBestFontSize(o, "EXPORT");
    GP_setColor (SUIT_getColor(o, TEXT_COLOR));
    GP_setViewport (GP_mapRectangle (GP_defRectangle(0.09, 0.3, 0.85, 0.8)));
    GP_justifyText ("EXPORT", JUSTIFY_CENTER);
}



PRIVATE void PaintQuestionMark (SUIT_object o)
{
    double w, h, d;

    GP_setWindow(GP_defRectangle(0.0, 0.0, 1.0, 1.0));

    GP_pushGraphicsState();
    findBestFontSize(o,"EXPORT");
    GP_inquireTextExtent("INFO", &w, &h, &d);
    w /= 2.0;
    h /= 2.0;
    if (BILEVEL_DISPLAY) {
	GP_setColor (SUIT_getColor(o, MARK_COLOR));
	GP_fillRectangle (GP_defRectangle((0.5 - w), (0.5 - h), (0.5 + w), (0.5 + h))); 
    }
    GP_setColor (SUIT_getColor(o, TEXT_COLOR));
    GP_setViewport (GP_mapRectangle(GP_defRectangle (0.0, 0.65-h, 1.0, 0.65+h)));
    GP_justifyText ("INFO", JUSTIFY_CENTER); 
    GP_popGraphicsState();

    GP_setLineWidth(5);
    if (!BILEVEL_DISPLAY)
	GP_setColor (SUIT_getColor(o, MARK_COLOR));
    GP_ellipseArc (GP_defRectangle(0.2, 0.4, 0.8, 0.9), -90.0, 180.0);
    GP_line(GP_defPoint(0.5, 0.25), GP_defPoint(0.5, 0.4));
    GP_line(GP_defPoint(0.5, 0.1), GP_defPoint(0.5, 0.2));
}



PRIVATE void paintTrashCan (SUIT_object o)
{
    GP_setWindow (GP_defRectangle(0.0, 0.0, 1.0, 1.0));
    if (!BILEVEL_DISPLAY) 
	GP_setColor (SUIT_getColor(o, CAN_COLOR));
    GP_fillEllipse (GP_defRectangle (0.1, 0.1, 0.9, 0.3));
    if (!BILEVEL_DISPLAY) {
	GP_setColor (SUIT_getColor(o, OUTLINE_COLOR));
	GP_ellipse (GP_defRectangle (0.1, 0.1, 0.9, 0.3));}
    if (!BILEVEL_DISPLAY)
	GP_setColor (SUIT_getColor(o, CAN_COLOR));
    GP_fillRectangleCoord(0.1, 0.2, 0.9, 0.8); 
    if (BILEVEL_DISPLAY)
	GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
    else GP_setColor (SUIT_getColor(o, INSIDE_COLOR));
    GP_fillEllipse (GP_defRectangle (0.1, 0.7, 0.9, 0.9));
    if (BILEVEL_DISPLAY)
	GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
    else GP_setColor (SUIT_getColor(o, TEXT_COLOR));
    findBestFontSize(o, "EXPORT");
    GP_justifyText ("TRASH", JUSTIFY_CENTER);
    if (!BILEVEL_DISPLAY)
	GP_setColor (SUIT_getColor (o, OUTLINE_COLOR));
    else GP_setColor (SUIT_getColor (o, FOREGROUND_COLOR));
    GP_ellipse (GP_defRectangle (0.1, 0.7, 0.9, 0.9));
    GP_line(GP_defPoint(0.1, 0.2), GP_defPoint(0.1, 0.8));
    GP_line(GP_defPoint(0.9, 0.2), GP_defPoint(0.9, 0.8));
    
}


PRIVATE void HitBillBoard (SUIT_object o, SUIT_event e)
{
    SUIT_inform(SUIT_getText(o,HELP_MESSAGE));
}





SUIT_object SUIT_createTrashCan (char *name)
{
    SUIT_object retval = SUIT_createObject (name, "billboard");
    SUIT_setColor (retval, CAN_COLOR, GP_defColor("white", WHITE_ON_MONO));
    SUIT_setColor (retval, INSIDE_COLOR, GP_defColor("blue", WHITE_ON_MONO));
    SUIT_setColor (retval, OUTLINE_COLOR, GP_defColor("blue", WHITE_ON_MONO));
    SUIT_setColor (retval, TEXT_COLOR, GP_defColor("black", BLACK_ON_MONO));
    SUIT_setText (retval, HELP_MESSAGE, "You can drag a property or widget in here to delete it.");
    SUIT_addDisplayToObject (retval, "standard", HitBillBoard, paintTrashCan);
    SUIT_deluxeSetBoolean (retval, BORDER_RAISED, FALSE, CLASS);
    return retval;
}



SUIT_object SUIT_createInfoButton (char *name)
{
    SUIT_object retval = SUIT_createObject (name, "billboard");
    SUIT_setColor (retval, MARK_COLOR, GP_defColor("blue", WHITE_ON_MONO));
    SUIT_setColor (retval, TEXT_COLOR, GP_defColor("black", BLACK_ON_MONO));
    SUIT_setColor (retval, OUTLINE_COLOR, GP_defColor("blue", WHITE_ON_MONO));
    SUIT_setText (retval, HELP_MESSAGE, "You can drag a property or widget in here to find out information about it.");
    SUIT_addDisplayToObject(retval, "standard", HitBillBoard, PaintQuestionMark);
    SUIT_deluxeSetBoolean (retval, BORDER_RAISED, FALSE, CLASS);
    return retval;
}



SUIT_object SUIT_createExportButton (char *name)
{
    SUIT_object retval = SUIT_createObject (name, "billboard");
    SUIT_setColor (retval, BODY_COLOR, GP_defColor("white", WHITE_ON_MONO));
    SUIT_setColor (retval, CAB_COLOR, GP_defColor("red", BLACK_ON_MONO));
    SUIT_setColor (retval, OUTLINE_COLOR, GP_defColor("blue", BLACK_ON_MONO));
    SUIT_setColor (retval, WHEEL_COLOR, GP_defColor("black", BLACK_ON_MONO));
    SUIT_setColor (retval, TEXT_COLOR, GP_defColor("black", BLACK_ON_MONO));
    SUIT_setText (retval, HELP_MESSAGE, "You can drag a property in here to create a widget which controls it.");
    SUIT_addDisplayToObject(retval, "standard", HitBillBoard, PaintTruck);
    SUIT_deluxeSetBoolean (retval, BORDER_RAISED, FALSE, CLASS);
    return retval;
}
