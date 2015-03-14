#include "suit.h"

void HitDragger(SUIT_object dragger, SUIT_event ev)
{
    GP_point newp;

    GP_setFont(SUIT_getFont(dragger, FONT));
    newp = GP_unMapPoint(SUIT_dragText("Here is text"));
    SUIT_setDouble(dragger, "drag point x", newp.x);
    SUIT_setDouble(dragger, "drag point y", newp.y);
}


void PaintDragger(SUIT_object dragger)
{
    GP_point pt;
    pt = GP_defPoint(SUIT_getDouble(dragger, "drag point x"), 
		     SUIT_getDouble(dragger, "drag point y"));
    GP_text(pt, "Here is text");
}



SUIT_object createDragTester(char* name)
{
    SUIT_object dragger;
    
    dragger = SUIT_createObject(name, "drag tester");
    SUIT_addDisplayToObject(dragger, "standard", HitDragger, PaintDragger);
    return dragger;
}

void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);
    SUIT_createDoneButton (NULL);

    createDragTester("fred");

    SUIT_beginStandardApplication();
}
