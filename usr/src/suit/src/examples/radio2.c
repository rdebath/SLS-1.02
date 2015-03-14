/****************************************
* RadioButtons.c --
* A demonstration program illustrating 
* the use of SUIT's Radio Button Widgets
* using SUIT enums.
* 
* THIS IS NOT THE EASIEST WAY TO MAKE
* RADIO BUTTONS. USE THIS FILE AS AN 
* EXAMPLE ONLY IF YOU HAVE A SUIT ENUM
* THAT YOU WANT TO USE. OTHERWISE, LOOK 
* AT THE FILE 
*
*              Radio1.c 
*
* Which shows the "usual" way to make Radio Buttons.
*
*****************************************/

#include "suit.h"

#define NUMBER_OF_FRUITS     4

char *fruit_array[] = {"kiwi", "elderberries", "breadfruit", "mango"};

void ChooseRadioButton(SUIT_object radio_buttons)
{
    char *curr_choice;
    curr_choice = SUIT_getEnumString(radio_buttons, CURRENT_VALUE);
    printf("User just selected %s\n", curr_choice);
}

void main (int argc, char *argv[])
{
    SUIT_object buttons;

    SUIT_init(argv[0]);

    buttons = SUIT_createRadioButtons ("fruits", ChooseRadioButton); /* 1 */
    SUIT_setEnum (buttons, CURRENT_VALUE, 
	  SUIT_defEnum ("mango", NUMBER_OF_FRUITS, fruit_array)); /* 2 */


    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}


/* ------------------------------
 * NOTES
 * ------------------------------
 * 1.) CREATE THE RADIO BUTTONS themselves. 
 *     This gives you a container where the choices are to go.
 *     The buttons always have a single callback function 
 *     that is called for you when the radio buttons collection
 *     is hit. Like all callbacks, the radio button callback 
 *     takes a single SUIT_object as a parameter. The SUIT_object 
 *     passed in is the radio button being hit.
 * 
 * 2.) ADD BUTTONS to the radio button by passing in the radio button
 *     itself and a SUIT enum. Notice that we define the enumeration 
 *     out of an array of strings.
 *     
 */
