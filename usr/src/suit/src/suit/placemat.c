/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "suit.h"

/*--------------------------------------------------------*/
/*   PLACE MAT WIDGET   */

PRIVATE
void Dummy(SUIT_object o)
{
/* by having a dummy procedure, we get suit to draw our background & border*/
}

SUIT_object SUIT_createPlaceMat (char *name)
{
    SUIT_object o;
    static boolean firsttime = TRUE;

    o = SUIT_createObject (name, "place mat");
    SUIT_addDisplayToObject(o, "standard", NULL, Dummy);
    if (firsttime) {
	SUIT_deluxeSetBoolean (o, BORDER_RAISED, FALSE, CLASS);
	firsttime = FALSE;
    }
    return (o);
}
