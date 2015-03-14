/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include <math.h>
#include "suit.h"

#ifndef PI
#define PI           3.141592654
#endif
#define MAX_SIDES    50

/*--------------------------------------------------------*/
/*   N-SIDED POLYGON WIDGET   */

PRIVATE void PaintPolygon (SUIT_object o)
{
    double theta, angle, sine, cosine;
    int sides, i;
    point vertices[MAX_SIDES];
    
    /* always be a valid polygon */
    if ((sides = SUIT_getInteger (o, NUMBER_OF_SIDES)) < 3) 
	sides = 3;
    if (sides > MAX_SIDES)
	sides = MAX_SIDES;
    
    GP_setWindow (GP_defRectangle(-1.0, -1.0, 1.0, 1.0));
    angle = 2*PI/sides;
    for (theta = 0, i = 0;  i < sides;  theta += angle, i++)
    {
	sine = sin(theta);
	cosine = cos(theta);
	vertices[i] = GP_mapPoint(GP_defPoint(cosine, sine));
    }
    if ( SUIT_getBoolean(o, FILLED) )
	SRGP_fillPolygon (sides, vertices);
    else
	SRGP_polygon (sides, vertices);
}

SUIT_object SUIT_createPolygon (char *name)
{
    SUIT_object o;
    
    o = SUIT_createObject (name, "polygon");
    SUIT_addDisplayToObject(o, "standard", NULL, PaintPolygon);
    SUIT_setInteger (o, NUMBER_OF_SIDES, 3);
    return (o);
}
