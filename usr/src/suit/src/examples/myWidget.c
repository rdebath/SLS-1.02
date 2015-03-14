/***************************************
*  myWidget.c --
*      A demonstration program illustrating 
*      the creation of a new widget.
*      
*      The program uses a custom made widget:
*      A Coordinate Tester.
*      You click on it, a cross hair appears 
*      and a label changes to show you 
*      your current position.
***************************************** */

#include "suit.h"
SUIT_object CoordTester;

void HitCoordTester (SUIT_object tester, SUIT_event myEvent)
{
    SUIT_setDouble (tester, "current X", myEvent.worldLocation.x);
    SUIT_setDouble (tester, "current Y", myEvent.worldLocation.y);
}


void PaintCoordTester (SUIT_object tester)
{
    double crossHairSize;
    double hitx, hity;
    
    crossHairSize = SUIT_getDouble (tester, "cross hair size");
    hitx = SUIT_getDouble (tester, "current X");
    hity = SUIT_getDouble (tester, "current Y");
    /* Cross hairs */
    GP_lineCoord(hitx, hity-crossHairSize, hitx, hity+crossHairSize);
    GP_lineCoord(hitx-crossHairSize, hity, hitx+crossHairSize, hity);
}



SUIT_object CreateCoordTester(char *name)
{
							      
    SUIT_object tester;					         /* 2 */

    
    tester = SUIT_createObject(name, "CoordTester class");	 /* 3 */
    SUIT_addDisplayToObject (tester, "standard", 
			     HitCoordTester, PaintCoordTester);	 /* 4 */
    SUIT_registerClass ("CoordTester class", CreateCoordTester, 
        "This class of widgets displays a crosshair that moves under mouse control"); /* 5 */
    return (tester);					         /* 6 */
}



void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);
    CoordTester = CreateCoordTester("thing1");		         /* 1 */    
    SUIT_createDoneButton (NULL); 
    SUIT_beginStandardApplication();
}


/* 
 * Here we create a new CoordTester widget. 
 * 
 * 1.) Notice that the Creation Routine takes a string which is the name of the
 *     widget. This convention is followed in all SUIT widgets, and you 
 *     should follow it too. All widget names in SUIT must be unique. 
 *     Failure to keep names unique will cause a runtime error as soon as
 *     the offending widget is created.
 *      
 * 2.) Create a local variable here. We need to return a SUIT_object to the 
 *     caller.
 * 
 * 3.) Call SUIT_createObject(). Here, you supply a unique name and the
 *     the name of a class that the widget should belong to. If SUIT 
 *     hasn't seen the class name before, it assumes that you are 
 *     creating a new class (as we are here, a class called 
 *     "CoordTester class".
 *     
 * 4.) Add a display style to this widget. You may call this function as 
 *     many times as you want, once for each display style you 
 *     require. This widget has only one display style. Each call of 
 *     this function requires two functions, a hit procedure that 
 *     handles mouse and keyboard events and a paint proc that paints 
 *     that widget to the screen. 
 *     
 * 5.) OPTIONAL : Call SUIT_registerClass() to register the new class with SUIT
 *     so that you can create new widgets of 
 *     this type interactively with SUIT-n. There is no need to do 
 *     this if the widget has any kind of fucntionality assoaciated 
 *     with it -- best done for widgets like labels and placemats that 
 *     exist only for show. We do it here for demonstrative purposes.
 *     Notice that the parameter to this function is the name of the 
 *     function that creates the new class. 
 *     
 * 6.) Return the newly created SUIT_object.
 */


