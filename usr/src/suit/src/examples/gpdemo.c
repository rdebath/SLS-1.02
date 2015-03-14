/* gpdemo.c --
 * illustrates the use of some of the GP calls.
 * It might be helpful to compile and run this program before 
 * examining the code
 *
 */

#include "suit.h"

void demoLines(SUIT_object me)
{
    GP_point pt1, pt2;

    pt1 = GP_defPoint (0.3, 0.5);
    pt2 = GP_defPoint (0.9, 0.7);
    
    /* you can specify lines either 
     * by listing coords explicitly 
     * or by giving points
     */
    GP_lineCoord(0.1, 0.1, 0.1, 0.9);
    GP_line(pt1, pt2);
}


void demoArcs(SUIT_object me)
{

    /* This sweeps out an arc that starts at 
     *  0.0 degrees (3 O'clock position) and goes 
     * COUNTER CLOCKWISE to 135.0 degrees (about 10 o`clock)
     */
    GP_ellipseArc(GP_defRectangle(0.1, 0.1, 0.3, 0.5), 300.0, 160.0);
    GP_ellipse(GP_defRectangle(0.7, 0.1, 0.9, 0.5));
    GP_ellipse(GP_defRectangle(0.6, 0.6, 0.9, 0.9));
}


void demoPoly(SUIT_object me)
{
    int num_vertices = 5;
    double xlist[5];
    double ylist[5];
    
    /* Load the arrays of points so that we can make a polygon */
    xlist[0] = 0.2;
    xlist[1] = 0.5;
    xlist[2] = 0.4;
    xlist[3] = 0.9;
    xlist[4] = 0.1;

    ylist[0] = 0.1;
    ylist[1] = 0.2;
    ylist[2] = 0.5;
    ylist[3] = 0.5;
    ylist[4] = 0.9;
    
    GP_polygonCoord(num_vertices, xlist, ylist);
}


void demoRect(SUIT_object me)
{
    GP_rectangle rect;

    rect = GP_defRectangle(0.1, 0.8, 0.9, 0.9);

    GP_rectangleCoord(0.5, 0.1, 0.9, 0.7);
    GP_drawRectangle(rect);
    GP_fillRectangleCoord(0.1, 0.1, 0.3, 0.3);
}


void demoColor(SUIT_object me)
{
    /* you can get a SUIT property ....*/
    GP_setColor(SUIT_getColor(me, "demo color"));
    GP_fillRectangleCoord(0.2, 0.1, 0.5, 0.2);

   /* ... OR  */

    /* define a new color yourself. Look under 
    *  "color names" in the index for a list of legal names
    * ... */
    GP_setColor(GP_defColor("salmon", FALSE));
    GP_fillRectangleCoord(0.1, 0.5, 0.3, 0.8);
    
    /* ... OR  */

    /* define a new color by specifying the color by 
     * Red, Green, and Blue (RGB) components. 
     * Here, we specify something with a fair bit of green in it.
     */
    GP_setColor(GP_defColorRGB(100, 50213, 120, TRUE));
    GP_fillRectangleCoord(0.4, 0.1, 0.9, 0.9);
}

void demoLineWidth(SUIT_object me)
{
    GP_setLineWidth(2);
    GP_lineCoord(0.0, 0.1, 0.0, 0.9);

    GP_setLineWidth(3);
    GP_lineCoord(0.2, 0.1, 0.2, 0.9);

    GP_setLineWidth(4);
    GP_lineCoord(0.4, 0.1, 0.4, 0.9);

    GP_setLineWidth(5);
    GP_lineCoord(0.6, 0.1, 0.6, 0.9);

    GP_setLineWidth(6);
    GP_lineCoord(0.8, 0.1, 0.8, 0.9);
}


void demoText(SUIT_object me)
{
    GP_rectangle rect;

    GP_text(GP_defPoint(0.7, 0.7), "Hello, World"); 
    GP_justifyText("Centered at the top", JUSTIFY_TOP_CENTER);
    GP_justifyText("Left and center", JUSTIFY_CENTER_LEFT);

    rect = GP_defRectangle(0.0, 0.0, 1.0, 0.3);
    GP_drawRectangle(rect);
    GP_justifyTextInRectangle("top, right of this rectangle", 
			      JUSTIFY_TOP_RIGHT, rect);

    /* show off the special codes that GP can use */
    GP_text(GP_defPoint(0.5, 0.5),
	"@italic(Fancy) stuff: @underline(really) fancy. @(copyright)1992");
}


SUIT_object CreateDemoChip(char *name, void (*PaintProc)(SUIT_object o)) 
{
    SUIT_object obj;
    obj = SUIT_createObject(name, "GP demo pad");
    SUIT_addDisplayToObject(obj, "standard", NULL, PaintProc);
    return obj;
}

void main (int argc, char *argv[])
{
    SUIT_object pad;

    SUIT_init(argv[0]);

    CreateDemoChip("lines", demoLines);
    CreateDemoChip("arc",   demoArcs);
    CreateDemoChip("poly", demoPoly);
    CreateDemoChip("rect", demoRect);
    CreateDemoChip("words", demoText);
    
    CreateDemoChip("color", demoColor);
    CreateDemoChip("width", demoLineWidth);
    
    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}
