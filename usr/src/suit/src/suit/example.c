#include "privsuit.h"

#ifdef sdkjfjsdkfsdkjl
void MoveMe (SUIT_object obj, SUIT_event event)
{
    if (event.type == MOUSE_DOWN) {
	rectangle new = SUIT_moveRectangle (SUIT_mapViewportToScreen(obj, OBJECT_VIEWPORT(obj)),
					    event.locator.position, FALSE);
	SUIT_object p = SUIT_mapPointToObject (new.bottom_left);
	SUIT_addChildToObject (p, obj);
	SUIT_setViewport (obj, VIEWPORT, SUIT_mapScreenToViewport(obj, new));
    }
}


void PaintMe (SUIT_object obj)  
{
    GP_justifyText("Move Me", JUSTIFY_CENTER);
} 



void main (int argc, char *argv[])
{
    SUIT_deluxeInit(&argc, argv);

    SUIT_createObject ("mover", "movable object");
    SUIT_addDisplayToObject (SUIT_name("mover"), "standard", MoveMe, PaintMe);

    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}
#endif

void Echo (SUIT_object typebox)
{
    printf ("text is \"%s\"\n",SUIT_getText(typebox,CURRENT_VALUE));
    SUIT_setBoolean (typebox, VISIBLE, FALSE);
}

SUIT_object Trapper (SUIT_object obj, SUIT_event *evt)
{
    if (evt->type == KEYSTROKE) {
	SUIT_setBoolean (SUIT_name("typebox"), VISIBLE, TRUE);
	return SUIT_name("typebox");
    }
    
    return obj;
}


void main (int argc, char *argv[])
{
    SUIT_deluxeInit(&argc, argv);

    SUIT_createTypeInBox ("typebox", Echo);
    SUIT_setBoolean (SUIT_name("typebox"), VISIBLE, FALSE);
    
    SUIT_registerTrapper (Trapper);

    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}
