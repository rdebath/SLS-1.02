/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "suit.h"

/*--------------------------------------------------------*/
/*   UVA LOGO WIDGET   */

PRIVATE void PaintRotunda (SUIT_object o)
{
    double x;

    GP_setWindow (GP_defRectangle(-0.1, -0.1, 1.1, 1.1));
    GP_setLineWidth (SUIT_getInteger(o, LINE_WIDTH));

    GP_rectangleCoord(0.0, 0.0, 1.0, 0.75);
    GP_rectangleCoord(0.25, 0.0, 0.75, 0.6);
    GP_ellipse(GP_defRectangle(0.0, 0.0, 1.0, 1.0));
    GP_line(GP_defPoint(0.0, 0.21), GP_defPoint(1.0, 0.21));
    GP_line(GP_defPoint(0.25, 0.6), GP_defPoint(0.5, 0.75));
    GP_line(GP_defPoint(0.75, 0.6), GP_defPoint(0.5, 0.75));
    GP_line(GP_defPoint(0.0, 0.6), GP_defPoint(1.0, 0.6));
    for (x = 0.35; x <= 0.75; x += 0.1) {
      GP_line(GP_defPoint(x, 0.6), GP_defPoint(x, 0.21)); }
}


PRIVATE void PaintBigV (SUIT_object o)
{

    GP_point corners[15];

    GP_setWindow (GP_defRectangle(-0.1, -0.1, 1.1, 1.1));
    corners[0] = GP_defPoint (0.12, 0.8);
    corners[1] = GP_defPoint (0.43, 0.0);
    corners[2] = GP_defPoint (0.57, 0.0);
    corners[3] = GP_defPoint (0.88, 0.8);
    corners[4] = GP_defPoint (0.91, 0.8);
    corners[5] = GP_defPoint (0.91, 1.0);
    corners[6] = GP_defPoint (0.71, 1.0);
    corners[7] = GP_defPoint (0.71, 0.8);
    corners[8] = GP_defPoint (0.74, 0.8);
    corners[9] = GP_defPoint (0.5, 0.18);
    corners[10] = GP_defPoint (0.25, 0.8);
    corners[11] = GP_defPoint (0.29, 0.8);
    corners[12] = GP_defPoint (0.29, 1.0);
    corners[13] = GP_defPoint (0.09, 1.0);
    corners[14] = GP_defPoint (0.09, 0.8);
    GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
    GP_fillRectangleCoord(-0.1, -0.1, 1.1, 1.1);
    GP_setColor (SUIT_getColor (o, FOREGROUND_COLOR));
    GP_fillPolygon (15, corners);
}


PRIVATE void PaintUVa (SUIT_object o)
{
    GP_point corners[8];

    GP_setWindow (GP_defRectangle(-0.1, -0.1, 1.1, 1.1));
    GP_fillEllipse (GP_defRectangle (0.0, 0.0, 0.33, 0.4));
    GP_fillRectangleCoord(0.0, 0.2, 0.1, 1.0);
    GP_fillRectangleCoord(0.23, 0.2, 0.33, 1.0);
    GP_setColor(SUIT_getColor (o, BACKGROUND_COLOR));
    GP_fillEllipse (GP_defRectangle (0.1, 0.1, 0.23, 0.3));
    GP_fillRectangleCoord(0.1, 0.2, 0.23, 1.0);
    GP_setColor(SUIT_getColor (o, FOREGROUND_COLOR));
    corners[0] = GP_defPoint (0.43, 1.0);
    corners[1] = GP_defPoint (0.34, 1.0);
    corners[2] = GP_defPoint (0.45, 0.0);
    corners[3] = GP_defPoint (0.55, 0.0);
    corners[4] = GP_defPoint (0.67, 1.0);
    corners[5] = GP_defPoint (0.58, 1.0);
    corners[6] = GP_defPoint (0.50, 0.2);
    GP_fillPolygon (7, corners);
    corners[0] = GP_defPoint (0.64, 0.0);
    corners[1] = GP_defPoint (0.78, 1.0);
    corners[2] = GP_defPoint (0.86, 1.0);
    corners[3] = GP_defPoint (1.0, 0.0);
    corners[4] = GP_defPoint (0.91, 0.0);
    corners[5] = GP_defPoint (0.89, 0.2);
    corners[6] = GP_defPoint (0.75, 0.2);
    corners[7] = GP_defPoint (0.73, 0.0);
    GP_fillPolygon (8, corners);
    corners[0] = GP_defPoint (0.76, 0.35);
    corners[1] = GP_defPoint (0.82, 0.83);
    corners[2] = GP_defPoint (0.88, 0.35);
    GP_setColor (SUIT_getColor (o, BACKGROUND_COLOR));
    GP_fillPolygon (3, corners);
}
  
  
  
PRIVATE void HitLogo (SUIT_object o, SUIT_event input)
{
    GP_beep ();
}



SUIT_object SUIT_createUVALogo (char *name)
{
    SUIT_object o;

    o = SUIT_createObject (name, "uva logo");
    SUIT_addDisplayToObject(o, "big v", HitLogo, PaintBigV);
    SUIT_addDisplayToObject(o, "rotunda", HitLogo, PaintRotunda);
    SUIT_addDisplayToObject(o, "uva", HitLogo, PaintUVa);
    return (o);
}
