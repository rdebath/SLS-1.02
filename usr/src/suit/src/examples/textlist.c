/**********************
 * textlist.c ---
 *     Demonstration of manipulating textLists.
 *     There is a scrollable list (which uses a textList for the
 *     underlying data structure) that allows the user to add 
 *     and delete items from the textlist.
 *   
 *     In addition to the scrollable list which will display the
 *     textlist, there are two other widgets in this application: a 
 *     button and a type in box.
 * 
 *     The button's callback will remove the currently selected 
 *     item from the text list using the SUIT_deleteFromTextList() call.
 * 
 *     The type in box's callback will add the given string as another 
 *     entry into the text list.
 *********************/


#include "suit.h"

SUIT_object ListWidget, DeleteButton, AddTypeBox;
char *Disney[] = { "Mickey", "Minnie", "Goofy", "Pluto", "Donald" };


void DeleteButtonCallback (SUIT_object button)
{
    int row = SUIT_getInteger (ListWidget, CURRENT_ROW);
    SUIT_textList list = SUIT_getTextList (ListWidget, LIST);

    if (row >= 0) {
	SUIT_deleteFromTextList (list, row);
	SUIT_setTextList (ListWidget, LIST, list);
    } else 
	SUIT_inform ("Please choose an item in the list first.");
}

void TypeBoxCallback (SUIT_object typeInBox)
/* users can type in this box and the string will be added to the text box. */
{
    SUIT_textList list = SUIT_getTextList (ListWidget, LIST);

    SUIT_appendToTextList (list, SUIT_getText (typeInBox, CURRENT_VALUE));
    SUIT_setTextList (ListWidget, LIST, list);
    SUIT_setText (typeInBox, CURRENT_VALUE, "");
}


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

    /* Create the list. Notice that we use an array of char*'s to do this */
    SUIT_setTextList (ListWidget, LIST, SUIT_defTextList (Disney, 5)); 

    /* Create a type in box -- allows addition of of strings to the list */
    AddTypeBox = SUIT_createTypeInBox ("add item", TypeBoxCallback);
    
    /* Create a button that will delete the current item from the list */
    DeleteButton = SUIT_createButton ("delete button", DeleteButtonCallback);

    SUIT_setText (DeleteButton, LABEL, "Delete Current Item");
    SUIT_createDoneButton (NULL);
    SUIT_createLabel("Add item:");
    SUIT_createLabel("Text List Demo Program");

    SUIT_beginStandardApplication ();
}


