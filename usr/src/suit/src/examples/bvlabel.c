/* bvlabel.c ---
 *   This program creates a bounded value 
 *   with a label that tracks the current 
 *   value. In other words, the label will
 *   always display the textual representation
 *   of the CURRENT_VALUE number of the
 *   bounded value. Note that the functionality is 
 *   attached to the bounded value through the
 *   bounded value's callback.
 * 
 *   FOR ADVANCED USERS:
 *   Look at the example file 
 * 
 *            interest.c
 *          
 *   for a way to do something similar in an even better way.
 */


#include "suit.h"

void DisplayValue (SUIT_object scroller)
{
    char buffer[100];
    sprintf (buffer, "%.2f", SUIT_getDouble(scroller, CURRENT_VALUE));
    SUIT_setText (SUIT_name("the label"), LABEL, buffer);
}


void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);
    SUIT_createBoundedValue ("the bvalue", DisplayValue);
    SUIT_createLabel ("the label");
    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}
