#include "suit.h"

#define OLD_CURSOR_COLOR  "old cursor color"

SUIT_object button;

void typeinCallback(SUIT_object typeinBox)
{
    printf("hit the box %s\n", OBJECT_NAME(typeinBox));
}


void TurnCursorOff (SUIT_object typebox)
{
    SUIT_setColor (typebox, OLD_CURSOR_COLOR, SUIT_getColor(typebox, CURSOR_COLOR));
    SUIT_setColor (typebox, CURSOR_COLOR, SUIT_getColor(typebox, BACKGROUND_COLOR));
}


void TurnCursorOn (SUIT_object typebox)
{
    SUIT_setColor (typebox, CURSOR_COLOR, SUIT_getColor(typebox, OLD_CURSOR_COLOR));
}

SUIT_object MyTypeInTrapper (SUIT_object widgetThatWasHit, SUIT_event *ev)
{
    static SUIT_object beingHit = NULL;
    
    if (ev->type == KEYSTROKE && ev->keyboard == '\r') {
	
        if (beingHit == NULL)  /* done only the first time */
	    beingHit = SUIT_name("field 1");
	
	if (SUIT_name("field 1") == beingHit) {
	    TurnCursorOff(beingHit);
	    beingHit = SUIT_name("field 2");
	    TurnCursorOn(beingHit);
	} else if (SUIT_name("field 2") == beingHit) {
	    TurnCursorOff(beingHit);
	    beingHit = SUIT_name("field 3");
	    TurnCursorOn(beingHit);
	} else if (SUIT_name("field 3") == beingHit) {
	    TurnCursorOff(beingHit);
	    beingHit = SUIT_name("field 1");
	    TurnCursorOn(beingHit);
	}
	return beingHit;
    }
    else
	return widgetThatWasHit;
}


void CreateObjects()
{
    SUIT_createTypeInBox("field 1", typeinCallback);

    SUIT_createTypeInBox("field 2", typeinCallback);
    TurnCursorOff(SUIT_name("field 2"));

    SUIT_createTypeInBox("field 3", typeinCallback);
    TurnCursorOff(SUIT_name("field 3"));
    SUIT_registerTrapper(MyTypeInTrapper);
}


void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);
    CreateObjects();
    SUIT_createDoneButton(NULL);
    SUIT_beginStandardApplication();
}	



