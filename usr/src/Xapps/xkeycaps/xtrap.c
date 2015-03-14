/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation.  No representations are made about the suitability of this
 * software for any purpose.  It is provided "as is" without express or 
 * implied warranty.
 */

#define NEED_EVENTS
#define NEED_REPLIES
#include <X11/Xproto.h>
#include <X11/extensions/xtrap/xtraplib.h>
#include <X11/extensions/xtrap/xtraplibp.h>

#include <stdio.h>
#include "KbdWidgetP.h"


extern void xkeycaps_DispatchEvent_hook ();
static void xkeycaps_xtrap_key_event_callback ();

void
xkeycaps_xtrap_open_connection (widget)
     KeyboardWidget widget;
{
  XETC *tc = 0;
  if (((KeyboardWidget) widget)->keyboard.use_xtrap)
    {
      tc = XECreateTC (XtDisplay (widget), 0, 0);
      if (! tc)
	fprintf (stderr, "%s: XTrap initialization failed for display %s\n",
		 progname, DisplayString (XtDisplay (widget)));
    }
  if (tc)
    {
      EventFlags events;
      XETrapSetTimestamps (tc, True, False);
      memset (events, 0, sizeof (events));
      BitTrue (events, KeyPress);
      BitTrue (events, KeyRelease);
      XETrapSetEvents (tc, True, events);
      XEAddEventCBs (tc, events, xkeycaps_xtrap_key_event_callback,
		     (XtPointer) XtWindow (widget));
      XEStartTrapRequest (tc);
    }
  widget->keyboard.trap_data = (char *) tc;
}

void
xkeycaps_xtrap_main_loop (keyboard, app)
     KeyboardWidget keyboard;
     XtAppContext app;
{
  XEvent event;
  XETC *tc = (XETC *) keyboard->keyboard.trap_data;
  XtInputMask imask;
  while (1)
    {
      imask = XETrapAppPending (app);
      
      /* Check to see what's going on so that we don't block in either
	 NextEvent or ProcessEvent since neither of these routines can
	 correctly deal with XTrap Events.
       */
      if (imask & XtIMXEvent)
	{
	  XtAppNextEvent (app, &event);
	  xkeycaps_DispatchEvent_hook (&event);
	}
      else if (imask & (XtIMTimer | XtIMAlternateInput))
	XtAppProcessEvent (app, (XtIMTimer | XtIMAlternateInput));
      else
	/* Nothing going on, so we need to block */
	XETrapWaitForSomething (app);
      
      XETrapDispatchEvent (&event, tc);
    }
}


void
xkeycaps_xtrap_simulate_event (keyboard, event)
     KeyboardWidget keyboard;
     XEvent *event;
{
#if 0
  if (XESimulateXEventRequest ((XETC *) keyboard->keyboard.trap_data,
			       event->xkey.type, event->xkey.keycode,
			       /* event->xkey.x, event->xkey.y, */
			       0, 0,
			       0))
    fprintf (stderr, "%s: XTrap: KeyPress simulation failed\n", progname);
#endif
}


static void
xkeycaps_xtrap_key_event_callback (tc, data, window)
     XETC *tc;
     XETrapDatum *data;
     Window window;
{
  XEvent event;
  if (data->u.event.u.u.type != KeyPress &&
      data->u.event.u.u.type != KeyRelease)
    return;
  event.xkey.serial    = 0;
  event.xkey.display   = tc->dpy;
  event.xkey.window    = window;
  event.xkey.type      = data->u.event.u.u.type;
  event.xkey.keycode   = data->u.event.u.u.detail;
  event.xkey.root      = data->u.event.u.keyButtonPointer.root;
  event.xkey.subwindow = data->u.event.u.keyButtonPointer.child;
  event.xkey.time      = data->u.event.u.keyButtonPointer.time;
  event.xkey.state     = data->u.event.u.keyButtonPointer.state;
  event.xkey.x_root    = data->u.event.u.keyButtonPointer.rootX;
  event.xkey.y_root    = data->u.event.u.keyButtonPointer.rootY;
  event.xkey.x         = data->u.event.u.keyButtonPointer.eventX;
  event.xkey.y         = data->u.event.u.keyButtonPointer.eventY;
  event.xkey.send_event= -1;

/*  fprintf (stderr, "simulating %s (%d, %d)\n",
	   (event.xkey.type == KeyPress ? "KeyPress" :
	    (event.xkey.type == KeyRelease ? "KeyRelease" : "???")),
	   event.xkey.type, event.xkey.keycode);
*/
  XtDispatchEvent (&event);
}
