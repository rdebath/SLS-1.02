/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "suit.h"

#define SPRING_WIDTH	0.15
#define HALF_SPRING_WIDTH	(SPRING_WIDTH/2.0)

#define MIDDLE		0.5

#define   BOX_MIN 	0.25
#define   BOX_MAX       0.75


PRIVATE boolean inHorizontalRegion (GP_point p, double min, double max)
{
    return( (p.x >= min) && (p.x <= max) && (ABS(p.y-MIDDLE) <= HALF_SPRING_WIDTH) );
}



PRIVATE boolean inVerticalRegion (GP_point p, double min, double max)
{
    return( (p.y >= min) && (p.y <= max) && (ABS(p.x-MIDDLE) <= HALF_SPRING_WIDTH) );
}



PRIVATE void PaintVerticalSpring (double starty, double endy, boolean spring)
{
    point p1, p2;

    p1 = GP_mapPoint(GP_defPoint(MIDDLE, starty));
    p2 = GP_mapPoint(GP_defPoint(MIDDLE, endy));

    if (spring) {
	int height = p2.y - p1.y;
	int span = height/10;
	int radius = height/5;
	int midy = (p1.y + p2.y)/2;
	int fudge = radius/3;

	SRGP_lineCoord (p1.x, p1.y, p1.x, p1.y+span+1);
	SRGP_lineCoord (p1.x, p2.y-span-1, p1.x, p2.y);
	p1.y += span;   p2.y -= span;
	p1.x -= radius/2;
	SRGP_ellipseArc (SRGP_defRectangle (p1.x-radius, p1.y, p1.x+radius, p1.y+2*radius-2), 
			 300.0, 90.0);
	SRGP_ellipseArc (SRGP_defRectangle (p1.x-radius, midy-radius, p1.x+radius, midy+radius),
			 270.0, 90.0);
	SRGP_ellipseArc (SRGP_defRectangle (p1.x-radius, p2.y-2*radius+2, p1.x+radius, p2.y),
			 270.0, 60.0);
	SRGP_ellipseArc (SRGP_defRectangle (p1.x-radius+fudge, midy-radius, p1.x+radius-fudge, 
					    p1.y+2*radius-2), 90.0, 270.0);
	SRGP_ellipseArc (SRGP_defRectangle (p1.x-radius+fudge, p2.y-2*radius+2, p1.x+radius-fudge, 
					    midy+radius), 90.0, 270.0);
    } else
	SRGP_line (p1, p2);
}


PRIVATE void PaintHorizontalSpring (double startx, double endx, boolean spring)
{
    point p1, p2;

    p1 = GP_mapPoint(GP_defPoint(startx, MIDDLE));
    p2 = GP_mapPoint(GP_defPoint(endx, MIDDLE));

    if (spring) {
	int width = p2.x - p1.x;
	int span = width/10;
	int radius = width/5;
	int midx = (p1.x + p2.x)/2;
	int fudge = radius/3;

	SRGP_lineCoord (p1.x, p1.y, p1.x+span, p1.y);
	SRGP_lineCoord (p2.x-span, p1.y, p2.x, p1.y);
	p1.x += span;   p2.x -= span;
	p1.y += radius/2;
	SRGP_ellipseArc (SRGP_defRectangle (p1.x, p1.y-radius, p1.x+2*radius-2, p1.y+radius), 
			 210.0, 360.0);
	SRGP_ellipseArc (SRGP_defRectangle (midx-radius, p1.y-radius, midx+radius, p1.y+radius), 
			 180.0, 360.0);
	SRGP_ellipseArc (SRGP_defRectangle (p2.x-2*radius+2, p1.y-radius, p2.x, p1.y+radius), 
			 180.0, 330.0);
	SRGP_ellipseArc (SRGP_defRectangle (midx-radius, p1.y-radius+fudge, p1.x+2*radius-2, 
					    p1.y+radius-fudge), 0.0, 180.0);
	SRGP_ellipseArc (SRGP_defRectangle (p2.x-2*radius+2, p1.y-radius+fudge, midx+radius, 
					    p1.y+radius-fudge), 0.0, 180.0);
    } else
	SRGP_line (p1, p2);
}



PRIVATE void PaintSpringiness (SUIT_object o)
{
    SUIT_springiness spring = SUIT_getSpringiness (o, CURRENT_VALUE);
    
    PaintVerticalSpring (BOX_MIN, MIDDLE, spring & VERTICAL_SPRINGINESS);
    PaintVerticalSpring (MIDDLE, BOX_MAX, spring & VERTICAL_SPRINGINESS);
    
    PaintHorizontalSpring (BOX_MIN, MIDDLE, spring & HORIZONTAL_SPRINGINESS);
    PaintHorizontalSpring (MIDDLE, BOX_MAX, spring & HORIZONTAL_SPRINGINESS);
    
    PaintVerticalSpring (0.0, BOX_MIN, spring & BELOW_SPRINGINESS);
    PaintVerticalSpring (BOX_MAX, 1.0, spring & ABOVE_SPRINGINESS);
    
    PaintHorizontalSpring (0.0, BOX_MIN, spring & LEFT_SPRINGINESS);
    PaintHorizontalSpring (BOX_MAX, 1.0, spring & RIGHT_SPRINGINESS);
    
    GP_beveledBorder (GP_mapRectangle(GP_defRectangle(BOX_MIN,BOX_MIN,BOX_MAX,BOX_MAX)),
		       SUIT_getColor(o, BORDER_COLOR), TRUE, SUIT_getInteger(o, BORDER_WIDTH));
}



PRIVATE int SetVerticalSpringiness (SUIT_springiness springiness, int spring)
{
    if (spring & springiness) {
	spring &= ~springiness;
	if (!((spring & ABOVE_SPRINGINESS) ||
	      (spring & VERTICAL_SPRINGINESS) ||
	      (spring & BELOW_SPRINGINESS))) {
	    spring = spring | springiness;
	    SUIT_inform ("Sorry, there must be at least one vertical spring.");
	}
    } else
	spring = spring | springiness;
    return spring;
}



PRIVATE int SetHorizontalSpringiness (SUIT_springiness springiness, int spring)
{
    if (spring & springiness) {
	spring &= ~springiness;
	if (!((spring & RIGHT_SPRINGINESS) ||
	      (spring & HORIZONTAL_SPRINGINESS) ||
	      (spring & LEFT_SPRINGINESS))) {
	    spring = spring | springiness;
	    SUIT_inform ("Sorry, there must be at least one horizontal spring.");
	}
    } else
	spring = spring | springiness;
    return spring;
}


PRIVATE void HitSpringiness (SUIT_object o, SUIT_event e)
{
    SUIT_springiness spring = SUIT_getSpringiness (o, CURRENT_VALUE);
    void (*funct)(SUIT_object);
    
    if ( inVerticalRegion(e.worldLocation, BOX_MAX, 1.0) )
	spring = SetVerticalSpringiness (ABOVE_SPRINGINESS, spring);
    else if ( inVerticalRegion(e.worldLocation, BOX_MIN, BOX_MAX) )
	spring = SetVerticalSpringiness (VERTICAL_SPRINGINESS, spring);
    else if ( inVerticalRegion(e.worldLocation, 0.0, BOX_MIN) )
	spring = SetVerticalSpringiness (BELOW_SPRINGINESS, spring);
    else if ( inHorizontalRegion(e.worldLocation, 0.0, BOX_MIN) )
	spring = SetHorizontalSpringiness (LEFT_SPRINGINESS, spring);
    else if ( inHorizontalRegion(e.worldLocation, BOX_MIN, BOX_MAX) )
	spring =SetHorizontalSpringiness (HORIZONTAL_SPRINGINESS, spring);
    else if ( inHorizontalRegion(e.worldLocation, BOX_MAX, 1.0) )
	spring = SetHorizontalSpringiness (RIGHT_SPRINGINESS, spring);
    SUIT_setSpringiness (o, CURRENT_VALUE, spring);
    if ((funct = (SUIT_callbackFunctionPtr) SUIT_getFunctionPointer (o, CALLBACK_FUNCTION)) != NULL)
	funct (o);
}



SUIT_object SUIT_createSpringPanel (char *name)
{
    SUIT_object o;
    
    o = SUIT_createObject (name, "spring panel");
    SUIT_addDisplayToObject (o, "standard", HitSpringiness, PaintSpringiness);
    SUIT_setBoolean (o, BORDER_RAISED, FALSE);
    
    SUIT_changeObjectSize (o, 200, 200);
    return o;
}

