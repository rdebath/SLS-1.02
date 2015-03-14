/***************************************
*  coords.c --
*      A demonstration program illustrating 
*      the use of pixel and world coordinates
*      
*      The program uses a custom made widget:
*      A Coordinate Tester.
*      You click on it, a cross hair appears 
*      and prints out your current position.
*      
***************************************** */

#include "suit.h"

SUIT_object CoordTester;

void HitCoordTester (SUIT_object coord_tester, SUIT_event myEvent)
{
    SUIT_setDouble (coord_tester, "current x", myEvent.worldLocation.x);	    
    SUIT_setDouble (coord_tester, "current y", myEvent.worldLocation.y); 

    printf ("Last world coord hit: (%f, %f)\n", 
	    (float) myEvent.worldLocation.x, 
	    (float) myEvent.worldLocation.y);

    printf ("    Which is relative pixel location (%d, %d)\n\n", 
	     myEvent.relativePixelLocation.x, 
	     myEvent.relativePixelLocation.y);
}


void PaintCoordTester (SUIT_object coord_tester)
{
    double crossHairSize;
    double hitx, hity;
    
    crossHairSize = SUIT_getDouble (coord_tester, "cross hair size");
    hitx = SUIT_getDouble (coord_tester, "current x");
    hity = SUIT_getDouble (coord_tester, "current y");

    /* Paint Cross Hairs */
    GP_lineCoord(hitx, hity-crossHairSize, hitx, hity+crossHairSize);
    GP_lineCoord(hitx-crossHairSize, hity, hitx+crossHairSize, hity);
}


SUIT_object CreateCoordTester(char *name)
{
    SUIT_object coord_tester;
    coord_tester = SUIT_createObject(name, "CoordTester class");
    SUIT_addDisplayToObject (coord_tester, "standard", HitCoordTester, PaintCoordTester);
    SUIT_setDouble(coord_tester, "cross hair size", 0.05);
    return (coord_tester);
}


void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);
    CoordTester = CreateCoordTester("tester widget");
    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}



