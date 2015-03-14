/***************************************
*  move.c
*      A demonstration program illustrating 
*      the use of SUIT_moveRectangle()
*      
***************************************** */

#include "suit.h"

void HitMover (SUIT_object mover, SUIT_event e)
 /* the third parameter to SUIT_moveRectangle() 
  * dictates whether the rectangle will be allowed 
  * to move off the screen. Here we set it to FALSE, 
  * but you can set it to TRUE to let the rectangle
  * boundaries go off screen.
  */
{
    point pos;
    SUIT_viewport oldvp, newvp;

    pos = e.locator.position;
    oldvp = SUIT_getViewport(mover, VIEWPORT);
    newvp = SUIT_moveRectangle (oldvp, pos, FALSE);
    SUIT_setViewport (mover, VIEWPORT, newvp);
}


void PaintMover  (SUIT_object mover)
{
    GP_justifyText (SUIT_getText(mover, LABEL), JUSTIFY_CENTER);
}



SUIT_object CreateMover (char *name) 
{
    SUIT_object mover;

    mover = SUIT_createObject(name, "Moving Widget");
    SUIT_addDisplayToObject(mover, "standard", HitMover, PaintMover);
    SUIT_setText(mover, LABEL, name);
    return mover;
}

 
void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);

    SUIT_createLabel("Move Rectangle Demo Program");
    SUIT_createLabel("You can move this widget without using the SUIT keys");
    CreateMover("Drag this widget");
    SUIT_createDoneButton (NULL);

    SUIT_beginStandardApplication();
}
