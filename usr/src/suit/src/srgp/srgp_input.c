#include "HEADERS.h"
#include "srgplocal.h"

#ifdef RS6000
#define NBBY 8
#include <sys/time.h>
#include <sys/select.h>
#elif UNIX
#include <sys/types.h>
#include <sys/time.h>
#endif



/** DEVICE INDEPENDENCE
This file contains functions that are device-independent,
   except for the "sleeping" arrangements for SRGP_waitEvent.
Refer to inputraw.c and echo.c for the low-level "drivers" 
   for each type of implementation.

**/


static void
ComputeTimestamp (srgp_timestamp *ts)
{
   Time tdiff =  srgpx__cur_time - srgpx__starttime;

   ts->seconds = tdiff / rawgranularity;
   ts->ticks = (tdiff % rawgranularity) / ((double)rawgranularity) * 60;
}



/** SRGP __ initInputModule **/
void
SRGP__initInputModule ()
{
   /* DEFAULT KEYBOARD ECHO POSITION IS MIDDLE OF SCREEN WINDOW. */
   srgp__cur_keyboard_echo_origin = 
      SRGP_defPoint(srgp__canvasTable[0].max_xcoord >> 1,
		    srgp__canvasTable[0].max_ycoord >> 1);
   srgp__cur_locator_echo_anchor = srgp__cur_keyboard_echo_origin;

   /* DEFAULT KEYBOARD ATTRIBUTES */
   srgp__cur_keyboard_echo_font = 0;
   srgp__cur_keyboard_echo_color = SRGP_BLACK;

   SRGP__disableLocatorCursorEcho();

   /* INITIALIZE CURSOR TABLE. */
   PUSH_TRACE;
   /* SRGP_loadCursor (0, XC_arrow);    "device" dependent!!! */
   POP_TRACE;
   srgp__cur_cursor = 0;

   /* INITIALIZE ACTIVITY INFORMATION. */
   srgp__cur_mode[LOCATOR]=srgp__cur_mode[KEYBOARD]=INACTIVE;
   srgp__cur_locator_echo_type = CURSOR;
   srgp__cur_keyboard_processing_mode = EDIT;

   /* INITIALIZE MEASURES AND MASKS */
   srgp__cur_locator_measure.button_chord[LEFT_BUTTON] = UP;
   srgp__cur_locator_measure.button_chord[MIDDLE_BUTTON] = UP;
   srgp__cur_locator_measure.button_chord[RIGHT_BUTTON] = UP;
   srgp__cur_locator_measure.position = srgp__cur_locator_echo_anchor;

   srgp__cur_keyboard_measure.buffer = malloc(MAX_STRING_SIZE+1);
   srgp__cur_keyboard_measure.buffer_length = MAX_STRING_SIZE+1;
   srgp__cur_keyboard_measure.buffer[0] = '\0';
   srgp__get_keyboard_measure.buffer = malloc(MAX_STRING_SIZE+1);
   srgp__get_keyboard_measure.buffer_length = MAX_STRING_SIZE+1;
   srgp__get_keyboard_measure.buffer[0] = '\0';

   srgp__cur_locator_button_mask = LEFT_BUTTON_MASK;

   SRGP__initInputDrivers ();
   SRGP__initEchoModule ();
}
   



/** SRGP set input mode
The device is first deactivated, then its cur_mode status is updated,
then it is activated
**/
void
SRGP_setInputMode (inputDevice device, inputMode mode)
{
   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setInputMode  %d into %d\n", 
		  device, mode);
      srgp_check_system_state();
      srgp_check_device (device);
      srgp_check_mode (mode);
      LeaveIfNonFatalErr();
   }

   if (mode == srgp__cur_mode[device])
      /* NOTHING IS TO BE DONE. */
      return;

   if (mode == INACTIVE) {
      /* WE ARE BRINGING AN ACTIVE DEVICE TO INACTIVITY. */
      SRGP__deactivateDevice (device);
      srgp__cur_mode[device] = INACTIVE;
   }

   else {
       /* 
      if ((device==LOCATOR)&&(srgp__cur_mode[LOCATOR]==INACTIVE))
	 SRGP__updateRawCursorPosition();
	 */
      srgp__cur_mode[device] = mode;
      SRGP__activateDevice (device);
   }
}






/** SRGP wait event
The event in the queue which satisfies the wait is removed
and placed at the tail of the free-list.  It is there that
the "get" routines look.

Returns the identifier of the device which issued the event.
   (NO_DEVICE if none).
**/

inputDevice
SRGP_waitEvent (maximum_wait_time)
int maximum_wait_time;
{
#ifdef X11
   struct timeval tp, expiretime, timeout, *timeout_ptr;
   struct timezone tzp;
   int fd;
   fd_set readfds;
#endif
#ifdef THINK_C   
   Time curtime, expiretime=0;
#endif
   int return_value;
   boolean do_continue_wait, do_continue_search;
   boolean forever = (maximum_wait_time < 0);

   DEBUG_AIDS{
      srgp_check_system_state();
      LeaveIfNonFatalErr();
   }


#ifdef UNIX
#define MILLION 1000000
   FD_ZERO (&readfds);
   FD_SET (fd=ConnectionNumber(srgpx__display), &readfds);

   /* INITIALIZE (if necessary) TIMING INFORMATION */
   if (maximum_wait_time > 0) {
      long maxwait_sec, maxwait_ticks;
      gettimeofday (&tp,&tzp);
      maxwait_sec = maximum_wait_time / 60;
      maxwait_ticks = maximum_wait_time % 60;
      expiretime.tv_usec = tp.tv_usec + (maxwait_ticks*MILLION/60);
      expiretime.tv_sec = tp.tv_sec + maxwait_sec + 
	                  (expiretime.tv_usec / MILLION);
      expiretime.tv_usec %= MILLION;
   }
#endif

#ifdef THINK_C
   expiretime = TickCount() + maximum_wait_time;
#endif

   do_continue_wait = TRUE;

   return_value = NO_DEVICE;  /* timeout is what we assume will happen */


   /**** LOOP *****/
   do {
      return_value = SRGP__handleRawEvents (TRUE,forever);

      if ((return_value != NO_DEVICE) || (maximum_wait_time == 0))
	 do_continue_wait = FALSE;

#ifdef UNIX
      /* Otherwise, perform a wait state */
      else {
	 if (!forever) {
	    gettimeofday (&tp,&tzp);
	    /* Perform subtraction: expiretime - tp */
	    if (expiretime.tv_usec < tp.tv_usec) {
	       /* perform a borrow */
	       expiretime.tv_sec--;
	       expiretime.tv_usec += 1000000;
	    }
	    timeout_ptr = &timeout;
	    timeout.tv_usec = expiretime.tv_usec - tp.tv_usec;
	    timeout.tv_sec  = expiretime.tv_sec  - tp.tv_sec;
	    if ((timeout.tv_sec < 0) || (timeout.tv_usec < 0))
	       /* Whoops!  We've already expired! */
	       do_continue_wait = FALSE;
	 }
	 else /* (maximum_wait_time is negative representing infinity) */
	    timeout_ptr = NULL;

	 if (do_continue_wait)
	    if (XPending(srgpx__display) == 0) {
	       if (select (fd+1, &readfds, NULL, NULL, timeout_ptr))
		  /* An event occurred before the timeout! */
		  ;  /* so do nothing */
	       else 
		  do_continue_wait = FALSE;
	    }
      }
#endif

#ifdef THINK_C
      else
         if (maximum_wait_time > 0) {
            do_continue_wait =  (TickCount() < expiretime);
         }
#endif

   }
   while (do_continue_wait);

   srgp__device_at_head_of_queue = return_value;

   return return_value;
}



void
SRGP_getLocator (srgp__locator_measure *measure)
{
   DEBUG_AIDS{
      srgp_check_system_state();
      srgp_check_event_type (LOCATOR);
      LeaveIfNonFatalErr();
   }

   /* this assignment statement is very risky!  God help me! */
   *measure = *((srgp__locator_measure*)(&srgp__get_locator_measure));
}


void
SRGP_getDeluxeLocator (srgp__deluxe_locator_measure *measure)
{
   DEBUG_AIDS{
      srgp_check_system_state();
      srgp_check_event_type (LOCATOR);
      LeaveIfNonFatalErr();
   }

   *measure = srgp__get_locator_measure;
   ComputeTimestamp (&(measure->timestamp));
}



void
SRGP_getKeyboard (char *measure, int bufsize)
{
   DEBUG_AIDS{
      srgp_check_system_state();
      srgp_check_event_type (KEYBOARD);
      LeaveIfNonFatalErr();
   }
   strncpy (measure, srgp__get_keyboard_measure.buffer, bufsize-1);
   *(measure+bufsize-1) = '\0';
}


void
SRGP_getDeluxeKeyboard (srgp__deluxe_keyboard_measure *measure)
{
   DEBUG_AIDS{
      srgp_check_system_state();
      srgp_check_event_type (KEYBOARD);
      LeaveIfNonFatalErr();
   }
   strncpy (measure->buffer, srgp__get_keyboard_measure.buffer, 
	    measure->buffer_length-1);
   *(measure->buffer+measure->buffer_length-1) = '\0';
   bcopy (srgp__get_keyboard_measure.modifier_chord,
	  measure->modifier_chord,
	  sizeof(measure->modifier_chord));
   measure->position = srgp__get_keyboard_measure.position;
   
   ComputeTimestamp (&(measure->timestamp));
}




/** MEASURE SETTING **/

void
SRGP_setLocatorMeasure (point position)
{
   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setLocatorMeasure %d,%d\n", 
		  position.x, position.y);
      srgp_check_system_state();
      LeaveIfNonFatalErr();
   }

   SRGP__handleRawEvents (FALSE,FALSE);

   srgp__cur_locator_measure.position = position;

   SRGP__updateRawCursorPosition ();
}


/*!*/
void
SRGP_setKeyboardMeasure (str)
char *str;
{
   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setKeyboardMeasure %s\n", str);
      srgp_check_system_state();
      LeaveIfNonFatalErr();
   }
   SRGP__handleRawEvents (FALSE,FALSE);

   strncpy (srgp__cur_keyboard_measure.buffer, str, MAX_STRING_SIZE);
   *(srgp__cur_keyboard_measure.buffer+MAX_STRING_SIZE) = '\0';
   srgp__cur_keyboard_measure_length = strlen(srgp__cur_keyboard_measure.buffer);

   SRGP__updateKeyboardEcho ();
}




/** ATTRIBUTES
The routines allow the application to control the echoing
associated with the keyboard and the locator devices.
**/

/*!*/

void
SRGP_setLocatorEchoType (echoType value)
{
   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setLocatorEchoType %d\n", value);
      srgp_check_system_state();
      srgp_check_locator_echo_type(value);
      LeaveIfNonFatalErr();
   }

   SRGP__handleRawEvents (FALSE,FALSE);

   SRGP__disableLocatorRubberEcho();
   SRGP__disableLocatorCursorEcho();

   srgp__cur_locator_echo_type = value;

   SRGP__enableLocatorRubberEcho();
   SRGP__enableLocatorCursorEcho();

   SRGP__updateInputSelectionMask();
}

   
/*!*/

void
SRGP_setLocatorEchoCursorShape (int id)
{
   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setLocatorEchoCursorShape %d\n", id);
      srgp_check_system_state();
      LeaveIfNonFatalErr();
   }
   SRGP__handleRawEvents (FALSE,FALSE);

   srgp__cur_cursor = id;

   SRGP__updateLocatorCursorShape ();
}


/*!*/

void
SRGP_setLocatorEchoRubberAnchor (point position)
{
   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setLocatorEchoRubberAnchor %d,%d\n",
		  position.x, position.y);
      srgp_check_system_state();
      LeaveIfNonFatalErr();
   }

   SRGP__handleRawEvents (FALSE,FALSE);

   srgp__cur_locator_echo_anchor = position;

   SRGP__updateLocatorRubberAnchor ();
}


/*!*/


void
SRGP_setLocatorButtonMask (int value)
{
   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setLocatorButtonMask %d\n", value);
   }

   srgp__cur_locator_button_mask = value;
}


/*!*/


void
SRGP_setKeyboardProcessingMode (keyboardMode value)
{
   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setKeyboardProcessingMode %d\n", 
		  value);
      srgp_check_system_state();
      LeaveIfNonFatalErr();
   }
   SRGP__handleRawEvents (FALSE,FALSE);

   if (srgp__cur_mode[KEYBOARD] != INACTIVE)
      SRGP__deactivateDevice (KEYBOARD);

   srgp__cur_keyboard_processing_mode = value;

   if (srgp__cur_mode[KEYBOARD] != INACTIVE)
      SRGP__activateDevice (KEYBOARD);
}


/*!*/


void
SRGP_setKeyboardEchoColor (int value)
{
   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setKeyboardEchoColor %d\n", value);
      srgp_check_system_state();
      LeaveIfNonFatalErr();
   }
   SRGP__handleRawEvents (FALSE,FALSE);

   srgp__cur_keyboard_echo_color = value;
   SRGP__updateKeyboardEchoAttributes();
}


/*!*/


void
SRGP_setKeyboardEchoOrigin (point position)
{
   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setKeyboardEchoOrigin %d,%d\n", 
		  position.x, position.y);
      srgp_check_system_state();
      LeaveIfNonFatalErr();
   }
   SRGP__handleRawEvents (FALSE,FALSE);

   srgp__cur_keyboard_echo_origin = position;
   SRGP__updateKeyboardEchoAttributes();
}


/*!*/


void
SRGP_setKeyboardEchoFont (int fontindex)
{
   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setKeyboardEchoFont %d\n", fontindex);
      srgp_check_system_state();
      LeaveIfNonFatalErr();
   }
   SRGP__handleRawEvents (FALSE,FALSE);

   srgp__cur_keyboard_echo_font = fontindex;
   SRGP__updateKeyboardEchoAttributes ();
}




/** SAMPLERS **/

void
SRGP_sampleLocator (srgp__locator_measure *measure)
{
   DEBUG_AIDS{
      srgp_check_system_state();
      LeaveIfNonFatalErr();
   }
   SRGP__handleRawEvents (FALSE,FALSE);
   if (srgp__dirty_location) 
      SRGP__updateLocationKnowledge();
   *measure = *((srgp__locator_measure*)(&srgp__cur_locator_measure));
}


void
SRGP_sampleDeluxeLocator (srgp__deluxe_locator_measure *measure)
{
   DEBUG_AIDS{
      srgp_check_system_state();
      LeaveIfNonFatalErr();
   }
   SRGP__handleRawEvents (FALSE,FALSE);
   if (srgp__dirty_location) 
      SRGP__updateLocationKnowledge();
   *measure = srgp__cur_locator_measure;
   ComputeTimestamp (&(measure->timestamp));
}

/*!*/

void
SRGP_sampleKeyboard (char *measure, int bufsize)
{
   DEBUG_AIDS{
      srgp_check_system_state();
      LeaveIfNonFatalErr();
   }
   SRGP__handleRawEvents (FALSE,FALSE);
   strncpy (measure, srgp__cur_keyboard_measure.buffer, bufsize-1);
   *(measure+bufsize-1) = '\0';
}

void
SRGP_sampleDeluxeKeyboard (srgp__deluxe_keyboard_measure *measure)
{
   DEBUG_AIDS{
      srgp_check_system_state();
      LeaveIfNonFatalErr();
   }
   SRGP__handleRawEvents (FALSE,FALSE);
   strncpy (measure->buffer, srgp__cur_keyboard_measure.buffer, 
	    measure->buffer_length-1);
   *(measure->buffer+measure->buffer_length-1) = '\0';
   bcopy (srgp__cur_keyboard_measure.modifier_chord,
	  measure->modifier_chord,
	  sizeof(measure->modifier_chord));
   measure->position = srgp__get_keyboard_measure.position;
   ComputeTimestamp (&(measure->timestamp));
}
