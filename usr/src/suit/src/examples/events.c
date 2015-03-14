/***************************************
*  Events.c --
*
*      NOTE: This example program is probably most
*      useful if you compile & run it first, and then
*      examine the source code.
*
*      This demonstration program illustrates
*      the different kinds of events that can be
*      detected by SUIT. To do this, we create a new
*      Widget whose paint proc does nothing more than 
*      report the kind of event that occured. This same
*      sort of testing routine can be used in trapper 
*      functions as well for determining the kind of event 
*      that occured.
*      
* OUTPUT:
*      Output is printed on the face of a special widget 
*      created for this demo.
***************************************** */

#include "suit.h"
SUIT_object fred;

char *Notes ="@bold(About The Events Demo:)\n\nThis demonstration shows the different kinds of mouse events that SUIT can generate. The possible events are:\n\n@bold(click) : a quick press and release of the mouse button\n@bold(shift-click) : mouse click with shift key held down\n@bold(control-click) : mouse click with control key down\n@bold(meta-click) : mouse click with meta key down. Under some window managers (e.g. mwm), it may not be possible to trap some of the mouse and keyboard events like shift-click\n@bold(mouse motion) : mouse button held down while mouse moves\n@bold(mouse down) : this event is usually turned into mouse motion.\n@bold(mouse up) : Generate this event by pressing the mouse button down outside, and dragging the button into the widget\n@bold(keystroke) :any press of the keyboard keys";


/* 
 * This is the hit proc for the tester widget.
 * The events tested for are:
 * 1.) click      : a quick press and release of the mouse button
 * 2.) shift click : mouse click with shift key held down
 * 3.) control click: mouse click with control key down
 * 4.) meta click  : mouse click with meta key down. On some 
 *                    platforms, it may not be possible to trap this 
 *                    key combination, as some window managers (e.g. 
 *                    mwm) will intercept this.
 * 5.) mouse motion: mouse button held down while mouse moves
 * 6.) mouse down
 * 7.) mouse up
 * 8.) modifed keystroke : a keypress with a SHIFT or META or CONTROL
 * 9.) keystroke : any press of the keyboard keys
 * 
 */
     
void HitTester (SUIT_object tester, SUIT_event ev)
{
    char buf[100];
    
    switch (ev.type) {
      case CLICK:					     /* 1 */
	if (ev.locator.modifier_chord[SHIFT] == DOWN)	     /* 2 */
	    sprintf (buf, "Event was a SHIFT click");
	else if (ev.locator.modifier_chord[CONTROL] == DOWN) /* 3 */
	    sprintf (buf, "Event was a CONTROL click");
	else if (ev.locator.modifier_chord[META] == DOWN)    /* 4 */
	    sprintf (buf, "Event was a META click");
	else
	    sprintf (buf, "Event was a click");
	break;
      case MOUSE_MOTION:				    /* 5 */
	sprintf (buf, "Event was mouse motion");
	break;
      case MOUSE_DOWN:					    /* 6 */
	sprintf (buf, "Event was a mouse down");
	break;
      case MOUSE_UP:					    /* 7 */
	sprintf (buf, "Event was a mouse up");
	break;
      case KEYSTROKE:					    /* 8 */
	if (ev.locator.modifier_chord[SHIFT] == DOWN)	     
	    sprintf (buf, "Event was a SHIFT - %c", ev.keyboard);
	else if (ev.locator.modifier_chord[CONTROL] == DOWN)
	    /* note: for control characters events, 
	       ev.locator.modifier_chord[CONTROL] is true, and
	       ev.keyboard contains the control character itself,
	       so that 'control-a' comes back as 'control-a' (ASCII value
	       1), not as 'a' (ASCII value 97)
	       */
	    sprintf (buf, "Event was a CONTROL  - %c", ev.keyboard);
	else if (ev.locator.modifier_chord[META] == DOWN)
	    sprintf (buf, "Event was a META - %c", ev.keyboard);
	else                                                /* 9 */
	    sprintf (buf, "Event was a plain keyboard event. User typed %c", ev.keyboard);
	break;
    }
    SUIT_setBoolean(tester, "border raised", !SUIT_getBoolean(tester,"border raised"));
    SUIT_setText(tester , LABEL, buf);
}


void PaintTester (SUIT_object tester)
{
    GP_justifyText(SUIT_getText(tester, LABEL), JUSTIFY_CENTER);
}



void CreateObjects(void)
{
    SUIT_object tester;

    SUIT_createLabel("Event Tester Demo Program");
    SUIT_createTextBox("explain", Notes);

    tester = SUIT_createObject("event tester", "event tester class");
    SUIT_addDisplayToObject(tester, "standard", HitTester, PaintTester);
}



void main (int argc, char *argv[])
{
    SUIT_init(argv[0]);
    CreateObjects();
    SUIT_createDoneButton(NULL);
    SUIT_beginStandardApplication();
}	
