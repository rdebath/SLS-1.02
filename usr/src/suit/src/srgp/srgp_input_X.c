#include "HEADERS.h"
#include "srgplocal.h"
#include <ctype.h>
#include <X11/keysym.h>

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

#define BACKSPACE_KEY	 127
#define CARRIAGE_RETURN  13

#define KEYS		(KeyPressMask)
#define MOTION_HINT	(PointerMotionHintMask|PointerMotionMask)
#define MOTION_ALL	(PointerMotionMask)
#define BUTTONS (ButtonPressMask|ButtonReleaseMask|OwnerGrabButtonMask)

/* pausch hack: added ExposureMask */
#define DEFAULT	(StructureNotifyMask|EnterWindowMask|ExposureMask)

/** X SELECT INPUT MASKS
  I am always looking for keyboard presses (X events) because
  some keys are always active, allowing the user to press F6
  (for example) to turn on tracing at run-time, etc.
  
  When the locator is in sample mode w/o rubber echo, I need
      pointer-motion *hints* only.
  When the locator is using rubber echo, I need all pointer motion events.
  When the locator is in event mode and not echoed rubberly,
      I only need button-event reports.
**/

static unsigned long 
selectinputmask 
 [3] /*inputMode*/
 [2] /*boolean:rubberon?*/ = {
   /* input mode: INACTIVE */   {0L, 0L},
   /* input mode: SAMPLE */   	{MOTION_ALL|BUTTONS, MOTION_ALL|BUTTONS},
   /* input mode: EVENT */   	{BUTTONS, MOTION_ALL|BUTTONS}
};

#define SelectInput(LOCMODE,RUBBERON)   \
   XSelectInput (srgpx__display, srgpx__screenwin, \
		 selectinputmask[LOCMODE][RUBBERON?1:0]|KEYS|DEFAULT)


void
SRGP__initInputDrivers()
{
   /* INITIALIZE X INPUT */
   SelectInput (INACTIVE,FALSE);
}



void
SRGP__updateInputSelectionMask()
{
   SelectInput (srgp__cur_mode[LOCATOR], 
		srgp__cur_locator_echo_type > CURSOR);
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
   XEvent xev;

   switch (device) {
      
    case LOCATOR:
      SRGP__disableLocatorRubberEcho();
      SRGP__disableLocatorCursorEcho();
      srgp__cur_locator_measure.position = 
         SRGP_defPoint(srgp__canvasTable[0].max_xcoord>>1,
		       srgp__canvasTable[0].max_ycoord>>1);
      SelectInput(INACTIVE,FALSE);
      /* Delete all currently queued locator-related raw events. */
      while (XCheckMaskEvent(srgpx__display, MOTION_ALL|BUTTONS, &xev));
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
      SRGP__updateInputSelectionMask();
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
   XWarpPointer
      (srgpx__display,
       None, srgpx__screenwin,
       0,0,0,0,
       srgp__cur_Xcursor_x, srgp__cur_Xcursor_y);
   SRGP__updateLocatorRubberEcho();
}




static   XEvent xevent;
static   int xstrcount;
static   char buffer[50];
static   KeySym keysym;
static   unsigned long mask;
static   boolean in_wait_event;



void
SRGP__updateLocationKnowledge ()
{
   Window rw, cw;
   int xr, yr;
   unsigned int keys_buttons;

   XQueryPointer (srgpx__display, srgpx__screenwin,
		  &rw, &cw, &xr, &yr,
		  &srgp__cur_Xcursor_x, &srgp__cur_Xcursor_y, 
		  &keys_buttons);
   srgp__cur_locator_measure.position.x = srgp__cur_Xcursor_x;
   srgp__cur_locator_measure.position.y = SCREENFIXED(srgp__cur_Xcursor_y);
   
   srgp__dirty_location = FALSE;
}




static inputDevice HandleXButtonEvent (buttonStatus TRANSITION_TYPE) 
{
   int which_button;
   boolean do_return_event_notice = FALSE;


   which_button = xevent.xbutton.button - 1;

   if (srgp__cur_mode[LOCATOR] != EVENT)
      goto change_cur_measure;

   if (((srgp__cur_locator_button_mask >> which_button) & 1) == 0)
      goto change_cur_measure;

   if ( ! in_wait_event) {
      XPutBackEvent (srgpx__display, &xevent);
      mask &=  ~(BUTTONS);
      return NO_DEVICE;
   }

   do_return_event_notice = TRUE;

 change_cur_measure:
   srgpx__cur_time = xevent.xbutton.time;
   srgp__cur_locator_measure.button_chord[which_button] = TRANSITION_TYPE;
   srgp__cur_locator_measure.button_of_last_transition = which_button;
   srgp__cur_locator_measure.modifier_chord[SHIFT] =
      (xevent.xbutton.state&ShiftMask?TRUE:FALSE); 
   srgp__cur_locator_measure.modifier_chord[CONTROL] =  
      (xevent.xbutton.state&ControlMask?TRUE:FALSE); 
   srgp__cur_locator_measure.modifier_chord[META] =  
      (xevent.xbutton.state&Mod1Mask?TRUE:FALSE); 
   srgp__cur_locator_measure.position.x = 
      xevent.xbutton.x; 
   srgp__cur_locator_measure.position.y = 
      srgp__canvasTable[0].max_ycoord - xevent.xbutton.y; 
   if (do_return_event_notice) {
      srgp__get_locator_measure = srgp__cur_locator_measure; 
      return LOCATOR; 
   }
   return NO_DEVICE;
}



static inputDevice HandleRawModeKeyEvent (void)
{
   boolean do_return_event_notice = FALSE;
   
   if (srgp__cur_mode[KEYBOARD] != EVENT)
      goto change_cur_measure;
   
   if ( ! in_wait_event) {
      XPutBackEvent (srgpx__display, &xevent);
      mask &=  ~(KEYS);
      return NO_DEVICE;
   }

   do_return_event_notice = TRUE;
   
 change_cur_measure:
   srgp__cur_keyboard_measure.buffer[0] = *buffer;
   srgp__cur_keyboard_measure.buffer[1] = '\0';
   srgp__cur_keyboard_measure.modifier_chord[SHIFT] =
      (xevent.xkey.state&ShiftMask?TRUE:FALSE);
   srgp__cur_keyboard_measure.modifier_chord[CONTROL] =
      (xevent.xkey.state&ControlMask?TRUE:FALSE);
   srgp__cur_keyboard_measure.modifier_chord[META] =
      (xevent.xkey.state&Mod1Mask?TRUE:FALSE);
   if (do_return_event_notice) {
      strcpy (srgp__get_keyboard_measure.buffer, 
	      srgp__cur_keyboard_measure.buffer);
      bcopy (srgp__cur_keyboard_measure.modifier_chord,
	     srgp__get_keyboard_measure.modifier_chord,
	     sizeof(srgp__get_keyboard_measure.modifier_chord));
      srgp__get_keyboard_measure.position =
	 srgp__cur_keyboard_measure.position;
      return KEYBOARD;
   }
   return NO_DEVICE;
}


static inputDevice HandleProcModeKeyEvent (void)
{
   boolean do_return_event_notice = FALSE;
   
   switch (buffer[0]) {
    case CARRIAGE_RETURN:
      if (srgp__cur_mode[KEYBOARD] != EVENT)
	 goto erase_cur_measure;
      if (in_wait_event)
	 do_return_event_notice = TRUE;
      else {
	 XPutBackEvent (srgpx__display, &xevent);
	 mask &=  ~KEYS;
	 return NO_DEVICE;
      }
      if (do_return_event_notice) {
	 strcpy (srgp__get_keyboard_measure.buffer, 
		 srgp__cur_keyboard_measure.buffer);
	 srgp__get_keyboard_measure.position =
	    srgp__cur_keyboard_measure.position;
      }
    erase_cur_measure:
      srgp__cur_keyboard_measure.buffer[0] = '\0';
      srgp__cur_keyboard_measure_length = 0;
      SRGP__updateKeyboardEcho();
      if (do_return_event_notice)
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
   return NO_DEVICE;
}
   





/** SRGP__handleRawEvents
      This function nevers enters a wait state, unless it has been
         called as a result of SRGP_waitEvent(FOREVER).
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

int
   SRGP__handleRawEvents (boolean inwaitevent, boolean forever)
{
   inputDevice id;
   XEvent   unusedXEvent; /* pausch hack; used to flush expose events */   
   in_wait_event = inwaitevent;

   mask = (KEYS|MOTION_ALL|DEFAULT|BUTTONS);

   /* pausch hack: we need to do this to get the exposure events */
   XSelectInput(srgpx__display, srgpx__screenwin, mask);


   while (1) {
      
      if (forever && inwaitevent) {
         XMaskEvent (srgpx__display, mask, &xevent);
      }
      else {
         if ( ! XCheckMaskEvent (srgpx__display, mask, &xevent))
	 {
	    return NO_DEVICE;
	}
      }
      
      switch (xevent.type) {
	 
       case EnterNotify:
	 /* IF ON COLOR SYSTEM:
	    When cursor enters screen canvas, install SRGP colormap. */
	 if ((srgp__available_depth > 1) && 
	     (srgp__available_depth == srgp__application_depth))
	    XInstallColormap (srgpx__display, srgpx__colormap);
	 break;
	 
       case MotionNotify:
	 /* WE CAN ASSUME LOCATOR IS ACTIVE IF WE GET HERE. */
	 srgpx__cur_time = xevent.xmotion.time;
	 if (xevent.xmotion.is_hint)
	    /* WE CAN ASSUME RUBBER-ECHO IS OFF IF WE GET HERE. */
	    /* WE CAN ALSO ASSUME LOCATOR IS IN SAMPLE MODE IF WE GET HERE. */
	    srgp__dirty_location = TRUE;
	 else {
	    srgp__cur_Xcursor_x = xevent.xmotion.x;
	    srgp__cur_Xcursor_y = xevent.xmotion.y;
	    srgp__cur_locator_measure.position.x = srgp__cur_Xcursor_x;
	    srgp__cur_locator_measure.position.y = 
	       SCREENFIXED(srgp__cur_Xcursor_y);
	    SRGP__updateLocatorRubberEcho();
	 }
	 break;
	 
       case ButtonPress:
	 if (id = HandleXButtonEvent (DOWN)) {
	    return id;
	 }
	 break;
	 
       case ButtonRelease:
	 if (id = HandleXButtonEvent (UP)) {
	    return id;
	 }
	 break;
	 
       case MappingNotify:
	 XRefreshKeyboardMapping (&xevent.xmapping);
	 break;
	 
       case Expose:
	 /* a pausch hack: argubly, we really should have a separate 'expose'
	    SRGP callback to the application, but we're just going to
	    assume that he'll do a complete repaint when he's resized
	    */

	 SRGP__reactToScreenResize(srgp__canvasTable[0].max_xcoord+1,
				   srgp__canvasTable[0].max_ycoord+1);

	 break;

       case ConfigureNotify:
	 /* WE ONLY WISH TO REACT IF TRULY INVOLVED RESIZING (not just move) */
	 if (xevent.xconfigure.width == srgp__canvasTable[0].max_xcoord+1) 
	    if (xevent.xconfigure.height == srgp__canvasTable[0].max_ycoord+1) 
	       break;
	 
	 /* IF WE GET HERE, we wish to change the dimensions as recorded in
	    the canvas table, being careful if the screen canvas is currently
	    active. */
	 SRGP__reactToScreenResize
	    (xevent.xconfigure.width, xevent.xconfigure.height);
	 /* at this point, any exposes are unnecessary, so flush them */
	 while ( XCheckTypedEvent(srgpx__display, Expose, &unusedXEvent));
	 break;
	 
       case KeyPress:
	 xstrcount = XLookupString (&xevent.xkey, buffer, 50, &keysym, 0);
	 /* HERE, CHECK FOR keysym==FKEY for interesting FKEYS. */

	 /* Rob hack for arrow keys */
	 switch (keysym) {
	   case XK_Up:		xstrcount = 1;	buffer[0] = 16;	buffer[1] = '\0';	break;
	   case XK_Down:	xstrcount = 1;	buffer[0] = 14;	buffer[1] = '\0';	break;
	   case XK_Left:	xstrcount = 1;	buffer[0] = 2;	buffer[1] = '\0';	break;
	   case XK_Right:	xstrcount = 1;	buffer[0] = 6;	buffer[1] = '\0';	break;
	 }

	 if (srgp__cur_mode[KEYBOARD] == INACTIVE) 
	    break;
	 if (xstrcount != 1)
	    break;
	 
	 srgpx__cur_time = xevent.xkey.time;
	 srgp__cur_keyboard_measure.position.x = xevent.xkey.x;
	 srgp__cur_keyboard_measure.position.y = SCREENFIXED(xevent.xkey.y);
	 
	 if (srgp__cur_keyboard_processing_mode == RAW) {
	    if (id = HandleRawModeKeyEvent()) {
	       return id;
	    }
	 }
	 else {
	    if (id = HandleProcModeKeyEvent()) {
	       return id;
	    }
	 }
      }
   }
}
