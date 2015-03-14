/***************************************
*  resize.c --
*      a demonstration program illustrating 
*      the use of  SUIT_resizeRectangle()
*      
***************************************** */

#include "suit.h"


void HitResizer (SUIT_object me,  SUIT_event e)
{
    SUIT_viewport oldvp, newvp;

    oldvp = SUIT_getViewport(me, VIEWPORT);
    newvp = SUIT_resizeRectangle (oldvp);
    SUIT_setViewport (me, VIEWPORT, newvp);
    OBJECT_SELECTED(me) = 0;
}  


void PaintResizer  (SUIT_object me)
{
    GP_justifyText (SUIT_getText(me, "label"), JUSTIFY_CENTER);
}


SUIT_object CreateResizer (char *name) 
{
    SUIT_object obj;

    obj = SUIT_createObject(name, "ResizeWidget");
    SUIT_addDisplayToObject(obj, "standard", HitResizer, PaintResizer);
    SUIT_setText(obj, LABEL, "Click here to show handles, then resize");
    return obj;
}


void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);

    SUIT_createLabel("Resize Rectangle Demo Program");
    SUIT_createLabel("You can resize this widget without the SUIT keys");
    CreateResizer("my resizer");

    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}
