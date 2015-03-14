#include "suit.h"
/************************************
 * 
 * This example program demonstrates how you can 
 * handle double-click type "events"
 * 
 ************************************/


#define MILLISECONDS_ALLOWED_BETWEEN_CLICKS "milliseconds allowed between clicks"

SUIT_object doubleClickMonitor;

void copyTime (GP_time *src, GP_time *dest) {

    (*dest).secs = (*src).secs;
    (*dest).millisecs = (*src).millisecs;

    /* SAME AS:
       
    dest->secs = src->secs;
    dest->millisecs = src->millisecs;

    */
}

void hitDoubleClickMonitor (SUIT_object o, SUIT_event e) {
    /* this routine just measures the time between sucessive 
       click events and reports the occurence of two clicks 
       if they are sufficiently close together in time 
       */

    static boolean firstTime = TRUE;
    static GP_time previousTime;
    GP_time currentTime;
    time_t timeDifference;
    
    if (e.type == CLICK) {
	if (firstTime) {
	    firstTime = FALSE;
	    previousTime = GP_getCurrentTime ();
	}
	else {
	    currentTime = GP_getCurrentTime ();
	    timeDifference = GP_timeDifference (currentTime, previousTime);
	    if (timeDifference <= SUIT_getInteger (o, MILLISECONDS_ALLOWED_BETWEEN_CLICKS)) {
		SUIT_inform ("double click detected!");
	    }
	    copyTime (&currentTime, &previousTime);
	}
    }
}

void paintDoubleClickMonitor (SUIT_object o) {

    GP_justifyText ("Double click on me", JUSTIFY_CENTER);
}


SUIT_object createDoubleClickMonitor (char *name) {

    SUIT_object o;
    
    o = SUIT_createObject (name, "double click monitor");
    SUIT_addDisplayToObject (o, "standard", hitDoubleClickMonitor, paintDoubleClickMonitor);

    SUIT_deluxeSetInteger (o, MILLISECONDS_ALLOWED_BETWEEN_CLICKS, 500, OBJECT);
    return o;
}

void main (int argc, char *argv[]) {

    SUIT_deluxeInit(&argc, argv);

    doubleClickMonitor = createDoubleClickMonitor ("chuck");
    
    SUIT_createDoneButton(NULL);
    SUIT_beginStandardApplication();
}


