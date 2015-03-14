/****************************************
*  DragToTrash.c --
*      A demonstration program illustrating
*      how to make a new widget which:
*       1. If you hold the mouse down over it, drags itself, and
*       2. Checks to see if it is dropped over the trashcan,
*          whereupon it brings up a SUIT inform box telling you
*          so. If not, it relocates itself.
*****************************************/


#include "suit.h"

SUIT_object draggee, trashcan;


/*
 * This is the hit procedure for the dragging widget.
 * When it receives a MOUSE_DOWN event (in other words,
 * when you are holding the mouse button over it),
 * it calls SUIT_moveRectangle(), giving it its viewport,
 * the current location of the mouse, and whether or not
 * it wants to be dragged off-screen. SUIT_moveRectangle() 
 * returns a viewport. We then check to
 * see if it overlaps with the trashcan.
 * If it does, we do a SUIT_inform.
 * If not, we move the widget by setting its viewport to the
 * new location.
 */

void HitMyDraggingWidget (SUIT_object dragger, SUIT_event e)
{
    SUIT_viewport oldvp,      
                  newvp,      
                  trashvp;     /* the viewport of the trashcan */

    if (e.type == MOUSE_DOWN) {
	oldvp = SUIT_getViewport (dragger, VIEWPORT);
	newvp = SUIT_moveRectangle (oldvp, e.locator.position, FALSE);
	trashvp = SUIT_getViewport (trashcan, VIEWPORT);
	if (SUIT_viewportsOverlap (newvp, trashvp))
	    SUIT_inform ("I've been dropped over the trash can.");
	else
	    SUIT_setViewport (dragger, VIEWPORT, newvp);
    }
}


void PaintMyDraggingWidget(SUIT_object o)
{
    GP_lineCoord (0.0, 0.0, 1.0, 1.0);
}


SUIT_object CreateMyDraggingWidget(char *name)
{
    SUIT_object dragger;

    dragger = SUIT_createObject (name,"my dragging widget");
    SUIT_addDisplayToObject (dragger, "standard", 
			     HitMyDraggingWidget, PaintMyDraggingWidget);
    return dragger;
}


main(int argc, char *argv[])
{
    SUIT_init (argv[0]);
    draggee=CreateMyDraggingWidget ("waldo");
    trashcan=SUIT_createTrashCan ("floyd");
    SUIT_createDoneButton(NULL);
    SUIT_beginStandardApplication ();
}


