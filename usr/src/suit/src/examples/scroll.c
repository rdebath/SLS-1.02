/**********************
 * Scroll.c ---
 *     Demonstration of using SUIT's scrollable lists
 *     This program shows how to add and remove items 
 *     from a scrollable list.
 * 
 *********************/


#include "suit.h"

SUIT_object ListWidget;
char *Disney[] = { "Mickey", "Minnie", "Goofy", "Pluto", "Donald" };


void ListWidgetCallback (SUIT_object scrollBox)
{
    printf ("User just selected %s\n", 
	    SUIT_getText (scrollBox, CURRENT_VALUE));
}


void main (int argc, char *argv[])
{
    SUIT_init (argv[0]);

    ListWidget = SUIT_createScrollableList ("my scrollable list", 
					    ListWidgetCallback);
    SUIT_setTextList (ListWidget, LIST, SUIT_defTextList (Disney, 5));    
    SUIT_createDoneButton (NULL);

    SUIT_beginStandardApplication ();
}


