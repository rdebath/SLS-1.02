#include "HEADERS.h"
#include "srgplocal.h"
#include <ctype.h>

/** LOW-LEVEL "DRIVERS"

   SRGP__activateDevice (deviceID)
      Must be called when device is going from INACTIVE to active, OR
         when device is changed from one active mode to the other.

   SRGP__deactivateDevice (deviceID)
      Must be called only when device is going from active to inactive.


   SRGP__handleRawEvents (boolean in_waitEvent_call)
      This function nevers enters a wait state.
      It examines all the events on the "raw"
         queue: the queue of the underlying graphics package
	 (e.g., X11, Mac).
      Exception: it may not handle all the raw events.
         It exits as soon as it sees a valid trigger situation.
      It returns a device ID IF AND ONLY IF...
	 1) the appl. is in a call to SRGP_waitEvent(), AND
	 2) a valid trigger for a device currently in Event mode
	    has been encountered.
      IF it does return a device ID, THEN...
         It automatically sets the proper value for either
	     srgp__get_locator_measure or
	     srgp__get_keyboard_measure
	 in preparation for the application's ensuing call to 
	     SRGP_get...()
      Another exception: it may "pass over" some raw events and
         just leave them in the raw queue.
      It will pass over a raw event IF AND ONLY IF...
	 1) the appl. is not in a call to SRGP_waitEvent(), AND
	 2) the event is a valid trigger for a device
	    currently in Event mode.
      Another possibility is that it will discard a raw event
         without processing it at all.
      It will discard a raw event IF AND ONLY IF...
         The event is for a device that is currently inactive.
**/

#define BACKSPACE_KEY	  8
#define CARRIAGE_RETURN  13   /* hex 0D */


static RgnHandle cursorRgn;

static EventRecord macevent;
static boolean gotEvent;

static int which_button;

static Handle transData;

void
SRGP__initInputDrivers()
{
   cursorRgn = NewRgn();
   
   transData = RGetResource ('KCHR', 0);
}


static void AllowUserToGrowConsole (void)
{
   long gresult;
   int newheight, newwidth;
   Rect growlimit = {100,100,  3000,3000};

   /* This loop ignores all events until a mouse-down occurs.  
      No matter where the
      mouse-down occurs, it initiates a grow interaction!
    */
   DrawGrowIcon (srgpmac__cwindow);
   while (1) {
      gotEvent = WaitNextEvent (everyEvent, &macevent, 1, cursorRgn);
      if ( ! gotEvent)
         continue;
      if (macevent.what == mouseDown) {
	 gresult = GrowWindow (srgpmac__cwindow, macevent.where, &growlimit);
	 HiliteMenu (0);
	 newheight = (gresult>>16);
	 newwidth = (gresult&0xffff);
	 SizeWindow (srgpmac__cwindow, newwidth, newheight, FALSE);
	 SRGP__reactToScreenResize (newwidth, newheight);
	 break;
      }
   }
}




/** RAW-LEVEL DEACTIVATION OF A DEVICE
Responsible for erasing echo, and resetting device's measure to the
   hardwired default.
Upon entry, the device's cur_mode is its old value (has not been
   changed yet)!  And this procedure does not change it!
**/

void
SRGP__deactivateDevice (int device)
{
   switch (device) {
      
    case LOCATOR:
      SRGP__disableLocatorRubberEcho();
      SRGP__disableLocatorCursorEcho();
      srgp__cur_locator_measure.position = 
         SRGP_defPoint(srgp__canvasTable[0].max_xcoord>>1,
		       srgp__canvasTable[0].max_ycoord>>1);
      /* Delete all currently queued locator-related raw events. */
      /* ANY WAY TO DO THIS ON MAC? */
      break;
       
    case KEYBOARD:
      SRGP__disableKeyboardEcho();
      srgp__cur_keyboard_measure.buffer[0] = '\0';
      bzero (srgp__cur_keyboard_measure.modifier_chord,
	     sizeof(srgp__cur_keyboard_measure.modifier_chord));
      break;
   }
}





/** RAW-LEVEL ACTIVATION OF A DEVICE
Called whenever:
  a device is placed into EVENT or SAMPLE mode...
     a) when previously inactive
     b) when previously active but in a different mode
Responsible for initiating echo and setting X's selection mask.
Upon entry, the device's echo info and mode has already been set
  to their new values.
**/

void
SRGP__activateDevice (int device)
{
   switch (device) {
    case LOCATOR:
      SRGP__disableLocatorCursorEcho();
      SRGP__disableLocatorRubberEcho();
      SRGP__enableLocatorCursorEcho();
      SRGP__enableLocatorRubberEcho();
      break;
       
    case KEYBOARD:
      SRGP__enableKeyboardEcho();
      break;
   }
}





void
SRGP__updateRawCursorPosition ()
{
   srgp__cur_Xcursor_x = srgp__cur_locator_measure.position.x;
   srgp__cur_Xcursor_y = 
      SCREENFIXED(srgp__cur_locator_measure.position.y);
   /* SORRY, CAN'T "warp the cursor to new position" ON THE Mac */
   SRGP__updateLocatorRubberEcho();
}




static void DetermineModifiers (int modifiers, buttonStatus chord[3])
{
   chord[SHIFT] = (modifiers & shiftKey)?DOWN:UP; 
   chord[CONTROL] = (modifiers & controlKey)?DOWN:UP; 
   chord[META] = (modifiers & optionKey)?DOWN:UP; 
}






static inputDevice HandleXButtonEvent(buttonStatus TRANSITION_TYPE)
{
   GrafPtr savedport;

   if (srgp__curActiveCanvasId != 0) {
      savedport = thePort;
      SetPort (srgp__canvasTable[0].drawable.win);
      GlobalToLocal (&macevent.where);
      SetPort (savedport);
   }
   else
      GlobalToLocal (&macevent.where);

   srgpx__cur_time = TickCount();
   which_button = LEFT_BUTTON; 
   srgp__cur_locator_measure.button_chord[which_button] = TRANSITION_TYPE; 
   srgp__cur_locator_measure.button_of_last_transition = which_button; 
   srgp__cur_locator_measure.position.x = macevent.where.h;
   srgp__cur_locator_measure.position.y = SCREENFIXED(macevent.where.v);
   DetermineModifiers (macevent.modifiers, srgp__cur_locator_measure.modifier_chord); 
   if ((srgp__cur_mode[LOCATOR] == EVENT) && 
       ((srgp__cur_locator_button_mask >> which_button) & 1)) { 
		srgp__get_locator_measure = srgp__cur_locator_measure; 
         	return LOCATOR; 
   }
   return NO_DEVICE;
}



static void CheckForMouseMotion (void)
{
   Point p;
   GrafPtr savedport;
   
   if (srgp__curActiveCanvasId != 0) {
      savedport = thePort;
      SetPort (srgp__canvasTable[0].drawable.win);
      GetMouse (&p);
      SetPort (savedport);
   }
   else
      GetMouse (&p);
   
   if ((p.h == srgp__cur_Xcursor_x) &&
       (p.v == srgp__cur_Xcursor_y))
	     return;  /* no movement of mouse since we last checked */
	     
   srgpx__cur_time = TickCount();
      
   srgp__cur_Xcursor_x = p.h;
   srgp__cur_Xcursor_y = p.v;
   if (srgp__cur_mode[LOCATOR] != INACTIVE)
      SRGP__updateLocatorRubberEcho();
                  
   srgp__cur_locator_measure.position.x = srgp__cur_Xcursor_x;
   srgp__cur_locator_measure.position.y = SCREENFIXED(srgp__cur_Xcursor_y);
}



/*!*/
/** SRGP_interpret_X_event
      This function nevers enters a wait state.
      It examines all the events on the "raw"
         queue: the queue of the underlying graphics package
	 (e.g., X11, Mac).
      Exception: it may not handle all the raw events.
         It exits as soon as it sees a valid trigger situation.
      It returns a device ID IF AND ONLY IF...
	 1) the appl. is in a call to SRGP_waitEvent(), AND
	 2) a valid trigger for a device currently in Event mode
	    has been encountered.
      IF it does return a device ID, THEN...
         It automatically sets the proper value for either
	     srgp__get_locator_measure or
	     srgp__get_keyboard_measure
	 in preparation for the application's ensuing call to 
	     SRGP_get...()
      Another exception: it may "pass over" some raw events and
         just leave them in the raw queue.
      It will pass over a raw event IF AND ONLY IF...
	 1) the appl. is not in a call to SRGP_waitEvent(), AND
	 2) the event is a valid trigger for a device
	    currently in Event mode.
      Another possibility is that it will discard a raw event
         without processing it at all.
      It will discard a raw event IF AND ONLY IF...
         The event is for a device that is currently inactive.

TABLE OF SITUATIONS:
   LOCATOR  KEYBOARD  in_wait_event                Masks
   -------  --------  -------------   ------------------------------------
   Inactive DONTCARE  DONTCARE		KEYPRESS
   Sample   DONTCARE 	NO		KEYPRESS, MOTION, BUTTONS
   Sample   DONTCARE 	YES		KEYPRESS, MOTION, BUTTONS
   Event    DONTCARE    NO		KEYPRESS, MOTION
   Event    DONTCARE    YES		KEYPRESS, MOTION, BUTTONS

**/
int
SRGP__handleRawEvents (boolean in_wait_event, boolean unused)
{
   int xr, yr, xstrcount;
   char buffer[10];
   int mask;
   WindowPtr winptr;
   inputDevice id;
   long gresult;
   int whichmenu, whichitem;
   int newheight, newwidth;
   Rect boundrect = {0,40,3000,3000};
   unsigned int keycode;
   static long keyTransState=0;


 while (1) {
   gotEvent = WaitNextEvent (everyEvent, &macevent, 1, cursorRgn);
   
   CheckForMouseMotion();
   if ( ! gotEvent)
      return NO_DEVICE;
   
   switch (macevent.what) {
            
    case app4Evt:  /* my empirical observation is that this is the new "null" code */
    case nullEvent:
      return NO_DEVICE;
      
    case mouseUp:
      if (id = HandleXButtonEvent (UP))
         return id;
      break;
      
    case mouseDown:
      switch (FindWindow(macevent.where, &winptr)) {
       case inMenuBar:
	 gresult = MenuSelect (macevent.where);
         whichmenu = (gresult>>16);
         whichitem = (gresult&0xffff);
	 switch (whichmenu) {
	  case 200:
	    switch (whichitem) {
	     case 1:
	       AllowUserToGrowConsole();
	    }
	 }
	 break;
      case inDrag:
         if (winptr == srgpmac__cwindow) {
            /****** SRGP__deactivateApplColorTable(); */
            DragWindow (winptr, macevent.where, &screenBits.bounds);
            /****** SRGP__activateApplColorTable(); */
         }
         break;
       case inGoAway:
         if (TrackGoAway (winptr, macevent.where)) {
            SRGP_end();
            ExitToShell();
         }
         break;
       case inSysWindow:
	 SystemClick (&macevent, winptr);
	 break;
       case inContent:
         if (id = HandleXButtonEvent (DOWN))
            return id;
	 break;
      }
      break;
      
    case updateEvt: 
      if (winptr == srgpmac__cwindow) {
         BeginUpdate (winptr);
         EndUpdate (winptr);
      }
      return NO_DEVICE;
      
    case activateEvt:
      if ((macevent.modifiers & activeFlag) != 0) 
         /* THIS IS AN ACTIVATION OF THE MAC WINDOW */
         SRGP__activateApplColorTable();
      else
         SRGP__deactivateApplColorTable();
      break;
    
    case keyDown:
      /* buffer[0] = macevent.message & charCodeMask; */
      keycode = ((macevent.message & keyCodeMask)>>8) | (macevent.modifiers&0xff00);
      buffer[0] = (char) KeyTrans (*transData, keycode, &keyTransState);
      if (srgp__cur_mode[KEYBOARD] == INACTIVE) 
	 break;
      
      srgpx__cur_time = macevent.when;

      /* ONLY IF KEYBOARD IS ACTIVE */
      if (srgp__cur_keyboard_processing_mode == RAW) {
	 srgp__cur_keyboard_measure.buffer[0] = *buffer;
	 srgp__cur_keyboard_measure.buffer[1] = '\0';
         DetermineModifiers (macevent.modifiers, srgp__cur_keyboard_measure.modifier_chord);
	 if (srgp__cur_mode[KEYBOARD] == EVENT) {
	    strcpy (srgp__get_keyboard_measure.buffer, 
		    srgp__cur_keyboard_measure.buffer);
	    bcopy (srgp__cur_keyboard_measure.modifier_chord,
		   srgp__get_keyboard_measure.modifier_chord,
		   sizeof(srgp__get_keyboard_measure.modifier_chord));
	    return KEYBOARD;
	 }
      }
      else /* EDIT processing mode */
	 switch (buffer[0]) {
	  case CARRIAGE_RETURN:
	    if (srgp__cur_mode[KEYBOARD] == EVENT)
	       strcpy (srgp__get_keyboard_measure.buffer, 
		       srgp__cur_keyboard_measure.buffer);
	    srgp__cur_keyboard_measure.buffer[0] = '\0';
	    srgp__cur_keyboard_measure_length = 0;
	    SRGP__updateKeyboardEcho();
	    if (srgp__cur_mode[KEYBOARD] == EVENT)
	       return KEYBOARD;
	    break;
	    
	  case BACKSPACE_KEY:
	    if (srgp__cur_keyboard_measure_length > 0) {
	       srgp__cur_keyboard_measure_length =
		  srgp__cur_keyboard_measure_length - 1;
	       srgp__cur_keyboard_measure.buffer
		  [srgp__cur_keyboard_measure_length] =
		     '\0';
	       SRGP__updateKeyboardEcho();
	    }
	    break;
	    
	  default:
            /* CHECK: IS THE KEY PRINTABLE ASCII? */
	    if ((isprint(*buffer)) &&
		(srgp__cur_keyboard_measure_length < MAX_STRING_SIZE)) {
	       srgp__cur_keyboard_measure.buffer
		  [srgp__cur_keyboard_measure_length] = 
		     *buffer;
	       srgp__cur_keyboard_measure_length++;
	       srgp__cur_keyboard_measure.buffer
		  [srgp__cur_keyboard_measure_length] = 
		     '\0';
	       SRGP__updateKeyboardEcho();
	    }
	    break;
	 }
   }
 }
}
