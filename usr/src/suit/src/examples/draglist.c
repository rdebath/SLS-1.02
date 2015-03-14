/***************************************************
 * This demonstrates a slick hack that will let you 
 * drag items from one text list to another
 * FOR ADVANCED SUIT USERS ONLY
 *
 **************************************************/

#include "suit.h"

SUIT_object sl0, sl1;

SUIT_object dragAndDropList (SUIT_object drag, SUIT_event *e) {
    
    char *itemToDrag;
    SUIT_textList dropList;
    SUIT_object drop;
    point pt;
    
    if (e->type == MOUSE_DOWN) {
	if (SUIT_stringsMatch (OBJECT_CLASS (drag), "scrollable list")) {
	    SUIT_hitObject (drag, *e);
	    SUIT_checkAndProcessInput (1);
	    itemToDrag = SUIT_getText (drag, CURRENT_VALUE);
	    if (!SUIT_stringsMatch (itemToDrag, "")) {
		pt = SUIT_dragText (itemToDrag);
		drop = SUIT_mapPointToObject (pt);
		if (SUIT_stringsMatch (OBJECT_CLASS (SUIT_getParent (drop)), "scrollable list")) {
		    if (drag != SUIT_getParent(drop)) {
			dropList = SUIT_getTextList (SUIT_getParent (drop), LIST);
			SUIT_appendToTextList (dropList, itemToDrag);
			SUIT_setTextList (SUIT_getParent (drop), LIST, dropList);
			return NULL;
		    }
		}
	    }		
	}
    }
    return drag;
}

void 	main (int argc, char *argv[]) {
    
    SUIT_deluxeInit(&argc, argv);
    
    sl0 = SUIT_createScrollableList ("sl 0", NULL);
    sl1 = SUIT_createScrollableList ("sl 1", NULL);
    
    SUIT_registerTrapper (dragAndDropList);
    
    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}

