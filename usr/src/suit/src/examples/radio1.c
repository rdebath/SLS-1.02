/****************************************
* radio1.c --
* A demonstration program illustrating 
* the use of SUIT's Radio Button Widgets.
* 
* 
* This is the easiest way to make them. 
* If you have a SUIT_enum, look at 
*
*              Radio2.c 
*
* Which shows a cleaner way to make Radio buttons
* out of a SUIT_enum. 
*                     
*****************************************/

#include "suit.h"
SUIT_object buttons;


void ChooseRadioButton(SUIT_object radio_buttons)
{
    char *curr_choice;
    curr_choice = SUIT_getEnumString(radio_buttons, CURRENT_VALUE);
    printf("User just selected %s\n", curr_choice);
}



void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);
    SUIT_createLabel("Radio Button Demo Program");

    buttons = SUIT_createRadioButtons("my buttons", ChooseRadioButton);/* 1 */
    SUIT_addButtonToRadioButtons (buttons, "Speed Racer");
    SUIT_addButtonToRadioButtons (buttons, "Johnny Quest");            /* 2 */
    SUIT_addButtonToRadioButtons (buttons, "Jetsons");			 

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
 *     itself and a name for the label. This name will become a member of 
 *     of the SUIT_emumeration that the radio button widget is creating for you.
 *     The last radio button added will be the one selected when 
 *     the radio buttons come up for the first time. 
 *     
 * 3.) TO SELECT A BUTTON under program control, you could say, for example,
 *
 *     SUIT_pressThisRadioButton(buttons, "Jetsons");                           
 *
 *     
 */
