/***************************************
*  Trappers.c --
*      A demonstration program illustrating 
*      the use of Trappers.
*      
*      This program contains a single button.
*      Press "META-A" and the button is pressed, regardless
*      of the location of the mouse. ("META-A" is a "hot key").
*      
* OUTPUT:
*      Messages are sent to a SUIT label.
***************************************** */

#include "suit.h"
SUIT_object fred;

char *Notes ="@bold(About The Trapper Demo:)\n\nThis is a demonstration of the use of trapper functions in SUIT. Trappers are used to capture input events before they are seen by SUIT, thus allowing you to implement \"hot keys\", or keyboard accelerators.\n\nIn this demo, the button called Fred can be pressed directly, or it can be activated by pressing META-\"A\". The callback for the button is the same in either case: it beeps and changes the label.";

void HitFred (SUIT_object button)
{
    printf ("User pressed the Fred Button\n");
}


/* 
 * 1.) Test for the type of the event. This might have been a CLICK or
 *     some other event type as listed in the reference manual.
 *
 *     FOR AN EXAMPLE OF TESTING FOR OTHER EVENT TYPES, SEE THE
 *     EXAMPLE FILE "Events.c". 
 *     
 *     The function "MyHotKeyTrapper" 
 *     contains the tests necessary to test for all hotkeys in the 
 *     application. If there were more hot keys than just the one for 
 *     Fred, we  would test for all of them here.
 * 2.) META-A is the hotkey for the button we call "fred"
 * 3.) We call Fred's hit proc here, though we could really have
 *     done anything here we wanted.
 * 4.) We return NULL so that SUIT knows that the event was consumed and
 *     handled properly. If we wanted SUIT to pass the event on to some other 
 *     object, we could return that object instead. Whatever object 
 *     this function returns, SUIT will behave as if that object was the 
 *     one hit.
 */
SUIT_object MyHotKeyTrapper(SUIT_object widgetThatWasHit, SUIT_event *ev)
{
    if (ev->type == KEYSTROKE){					    /* 1 */
	if ((ev->locator.modifier_chord[META] == DOWN) && 
	    (ev->keyboard == 'a')) {				    /* 2 */
	    SUIT_hitObject(fred, *ev);				    /* 3 */
	    return NULL;					    /* 4 */
	}
    }
    return widgetThatWasHit;
}


void CreateObjects()
{
    SUIT_createLabel("SUIT trapper function Demo");
    SUIT_createLabel("Hotkey for Fred is META-\"A\"");
    fred = SUIT_createButton("Fred", HitFred);
    SUIT_createTextBox("explain", Notes);

    /* 
     * Now we make sure that the front of the button reads correctly:
     *              --------------------
     *              |                   |
     *              | Fred       Meta-A |
     *              |                   |
     *              ---------------------
     *        
     * By setting the HOTKEY property and cycling the button to the proper 
     * display style (using SUIT-c or by setting the ACTIVE_DISPLAY property), 
     * we only alter the appearence of the button. The real work of activating the 
     * button when the hotkey is pressed is done by the trapper function, defined above.
     * Notice that the HOTKEY property is merely a text string that appears on the 
     * right side of the button, along with the label for the button.
     */
    SUIT_setText(fred, HOTKEY, "Meta-a");
    SUIT_setEnumString (fred, ACTIVE_DISPLAY, "button with hotkey");
    
    /* 
     * Now we register a trapper function that SUIT will call before 
     * processing any input events. This is what allows us to use a hotkey 
     * rather than a button press to activate the Fred Button.
     */
    SUIT_registerTrapper(MyHotKeyTrapper);

}


void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);
    CreateObjects();
    SUIT_createDoneButton(NULL);
    SUIT_beginStandardApplication();
}	
