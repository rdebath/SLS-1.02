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

#if __STDC__
#include <stdlib.h>
#endif

#include <stdio.h>
#include "KbdWidgetP.h"
#include "KeyWidgetP.h"
#include "xkeycaps.h"

extern int string_equal ();

void highlight_key_action ();
void unhighlight_key_action ();
void toggle_key_action ();
void simulate_KeyPress_action ();
void simulate_KeyRelease_action ();
void track_key_action ();
void untrack_key_action ();
void describe_key_action ();

static void highlight_key ();
static void dehighlight_key ();
static void mouse_highlight_key ();
static void mouse_dehighlight_key ();

static void simulate_key_event ();
void key_to_event ();

KeyWidget keycode_to_key ();
KeyWidget window_to_key ();

void init_modifier_mapping ();



/* The goal of the mouse-tracking code in the following functions is for the
   mouse with a button down to be just like your finger on the keyboard: so
   long as a button is down, the key under it is depressed.  If the mouse
   moves to another key without the button going up first, the previous key
   is released and the new key is depressed.

   When a ButtonDown event comes in, the server gives you an implicit grab
   of the mouse so that you will get the corresponding ButtonUp event no 
   matter what happens to focus or mouse-position in the meantime.  This
   is good, and totally necessary to our purposes.  But, we want to be able
   to track the mouse while the button is down.  Initially, I did this by
   selecting EnterNotify/LeaveNotify events on the child windows, but those
   aren't generated while the parent window has a grab.  So instead, the 
   parent window gets PointerMotion events, and looks at the "child" window
   that the mouse is over to notice the crossings.

   This loses slightly in that if you click down on the A key and move
   quickly over to the L key, we won't necessarily notice that *all* of the
   intervening keys went down; only the ones that we happened to get motion
   events for.  Others down/up pairs may be compressed out of existence.
 */


KeyWidget
parse_key_action_args (name, event, argv, argc, widget, mouse_p_ret)
     char *name;
     XEvent *event;
     char **argv;
     int argc;
     KeyboardWidget widget;
     int *mouse_p_ret;
{
  struct keyboard *kbd = widget->keyboard.kbd;
  KeyWidget key = 0;
  int mouse_p = 0;
  int mod_p = 0;
  int track_p = 0;
  int highlighted_p = 0;
  char *key_desc = 0;
  int orig_argc = argc;

  for (; argc > 0; argc--)
    {
      if (string_equal (argv[argc-1], "ifmod"))
	mod_p = 1;
      else if (string_equal (argv[argc-1], "unlessmod"))
	mod_p = -1;
      else if (string_equal (argv[argc-1], "iftracking"))
	track_p = 1;
      else if (string_equal (argv[argc-1], "unlesstracking"))
	track_p = -1;
      else if (string_equal (argv[argc-1], "ifhighlighted"))
	highlighted_p = 1;
      else if (string_equal (argv[argc-1], "unlesshighlighted"))
	highlighted_p = -1;
      else if (key_desc)
	fprintf (stderr, "%s: too many args (%d) passed to %s action\n",
		 progname, orig_argc, name);
      else
	key_desc = argv[argc-1];
    }

  if (key_desc && string_equal (key_desc, "mouse"))
    key = widget->keyboard.key_under_mouse, mouse_p = 1;
  else if (key_desc && string_equal (key_desc, "highlighted"))
    key = widget->keyboard.mouse_highlighted_key, mouse_p = 1;
  else if (key_desc && string_equal (key_desc, "displayed"))
    key = widget->keyboard.documented_key, mouse_p = 1;
  else if (key_desc)
    fprintf (stderr, "%s: unrecognised arg to %s: %s\n", progname,
	     name, key_desc);
  else if (event->xany.type == ButtonPress ||
	   event->xany.type == ButtonRelease ||
	   event->xany.type == MotionNotify)
    key = widget->keyboard.key_under_mouse, mouse_p = 1;
  else if (event->xany.type == KeyPress ||
	   event->xany.type == KeyRelease)
    key = keycode_to_key (widget, event->xkey.keycode);
  else
    fprintf (stderr, "%s: %d isn't a key or button press or release in %s\n",
	     progname, event->xany.type, name);

  if (mod_p != 0) {
    if (!key) return 0;
    if (mod_p ==  1 && !key->key.modifier_bits) return 0;
    if (mod_p == -1 &&  key->key.modifier_bits) return 0;
  }

  if (track_p ==  1 && !widget->keyboard.tracking_key) return 0;
  if (track_p == -1 &&  widget->keyboard.tracking_key) return 0;

  if (highlighted_p ==  1 && key != widget->keyboard.mouse_highlighted_key)
    return 0;
  if (highlighted_p == -1 && key == widget->keyboard.mouse_highlighted_key)
    return 0;

  if (mouse_p_ret) *mouse_p_ret = mouse_p;
  return key;
}


void
highlight_key_action (widget, event, argv, argc)
     KeyboardWidget widget;
     XEvent *event;
     char *argv[];
     int *argc;
{
  struct keyboard *kbd = widget->keyboard.kbd;
  Display *dpy = XtDisplay (widget);
  int mouse_p;
  KeyWidget key = parse_key_action_args ("HighlightKey", event,
					 argv, *argc, widget, &mouse_p);
  if (! key) return;
  if (mouse_p)
    mouse_highlight_key (key);
  else
    highlight_key (key);
}


void
unhighlight_key_action (widget, event, argv, argc)
     KeyboardWidget widget;
     XEvent *event;
     char *argv[];
     int *argc;
{
  struct keyboard *kbd = widget->keyboard.kbd;
  Display *dpy = XtDisplay (widget);
  int mouse_p;
  char events_queued_p = 0;
  KeyWidget key = parse_key_action_args ("UnhighlightKey", event,
					 argv, *argc, widget, &mouse_p);
  if (! key) return;
  if (mouse_p)
    mouse_dehighlight_key (key);
  else
    dehighlight_key (key);
}


void
toggle_key_action (widget, event, argv, argc)
     KeyboardWidget widget;
     XEvent *event;
     char *argv[];
     int *argc;
{
  struct keyboard *kbd = widget->keyboard.kbd;
  Display *dpy = XtDisplay (widget);
  int mouse_p;
  KeyWidget key = parse_key_action_args ("ToggleKey", event, argv, *argc,
					 widget, &mouse_p);
  if (! key) return;
  if (mouse_p)
    {
      if (key->key.mouse_highlighted)
	mouse_dehighlight_key (key);
      else
	mouse_highlight_key (key);
    }
  else
    {
      if (key->key.key_highlighted)
	dehighlight_key (key);
      else
	highlight_key (key);
    }
}



void
simulate_KeyPress_action (widget, event, argv, argc)
     KeyboardWidget widget;
     XEvent *event;
     char *argv[];
     int *argc;
{
  struct keyboard *kbd = widget->keyboard.kbd;
  Display *dpy = XtDisplay (widget);
  KeyWidget key = parse_key_action_args ("SimulateKeyPress", event, argv,
					 *argc, widget, 0);
  if (key) simulate_key_event (key, 1);
}


void
simulate_KeyRelease_action (widget, event, argv, argc)
     KeyboardWidget widget;
     XEvent *event;
     char *argv[];
     int *argc;
{
  struct keyboard *kbd = widget->keyboard.kbd;
  Display *dpy = XtDisplay (widget);
  KeyWidget key = parse_key_action_args ("SimulateKeyRelease", event,
					 argv, *argc, widget, 0);
  if (key) simulate_key_event (key, 0);
}


void
describe_key_action (widget, event, argv, argc)
     KeyboardWidget widget;
     XEvent *event;
     char *argv[];
     int *argc;
{
  struct keyboard *kbd = widget->keyboard.kbd;
  Display *dpy = XtDisplay (widget);
  KeyWidget key = parse_key_action_args ("DescribeKey", event,
					 argv, *argc, widget, 0);
  if (! key) return;

  /* If the event invoking this is a KeyPress or KeyRelease, and the
     key in question is a modifier key, don't do the optimization of
     not redisplaying because we're already displaying that key.  This
     is so that the "DescribeKey(displayed)" action can be bound to
     KeyDown and KeyUp, and will change the display when modifier keys
     are pressed and released.
   */
  if ((event->xany.type == KeyPress || event->xany.type == KeyRelease)
      && (widget->keyboard.modifier_vector [event->xkey.keycode / 8]
	  & (1 << (event->xkey.keycode % 8))))
    widget->keyboard.documented_key = 0;

  if (key != widget->keyboard.documented_key)
    {
      describe_key (key);
      widget->keyboard.documented_key = key;
    }
}


void
track_key_action (widget, event, argv, argc)
     KeyboardWidget widget;
     XEvent *event;
     char *argv[];
     int *argc;
{
  struct keyboard *kbd = widget->keyboard.kbd;
  Display *dpy = XtDisplay (widget);
  KeyWidget key = parse_key_action_args ("TrackKey", event,
					 argv, *argc, widget, 0);
  if (!key) return;
  if (event->xany.type != ButtonPress && event->xany.type != ButtonRelease)
    {
      fprintf (stderr, "%s: TrackKey action invoked on a non-button event\n",
	       progname);
      return;
    }
  widget->keyboard.tracking_key |= (1 << (event->xbutton.button - 1));
}

void
untrack_key_action (widget, event, argv, argc)
     KeyboardWidget widget;
     XEvent *event;
     char *argv[];
     int *argc;
{
  struct keyboard *kbd = widget->keyboard.kbd;
  Display *dpy = XtDisplay (widget);
  KeyWidget key = parse_key_action_args ("UntrackKey", event,
					 argv, *argc, widget, 0);
  if (event->xany.type != ButtonPress && event->xany.type != ButtonRelease)
    {
      fprintf (stderr,
	       "%s: UntrackKey action invoked on a non-button event\n",
	       progname);
      return;
    }
/*  widget->keyboard.tracking_key &= ~(1 << (event->xbutton.button - 1)); */
  widget->keyboard.tracking_key = 0;
}

#if __STDC__
extern void sensitize_menu (KeyboardWidget, Widget, Bool);
#endif

void
key_menu_pre_popup_hook (menu, client_data, call_data)
     Widget menu;
     XtPointer client_data, call_data;

{
  KeyboardWidget widget = (KeyboardWidget) client_data;
  KeyWidget key = widget->keyboard.key_under_mouse;
  Arg av[10];
  int ac = 0;
  char buf1 [255];	/* so that the "!=" test in SimpleMenu succeeds... */
  char buf2 [255];
  static int toggle = 0;
  char *b = (toggle ? buf1 : buf2);
  message (widget, ""); /* clear the message area */
  if (key)
    {
      toggle = !toggle;
      sprintf (b, " Key %s (0x%02X) ", key->key.key_name,
	       key->key.key->keycode);
      XtSetArg (av[ac], XtNlabel, b); ac++;
    }
  else
    XtSetArg (av[ac], XtNlabel, "--  No Key  --"), ac++;
  XtSetValues (menu, av, ac);
  sensitize_menu (widget, menu, (key ? True : False));
}



/* This is called before every action is invoked.  It does two things:
   it keeps the variable key_under_mouse up to date, and, if in TrackKey
   mode, it simulates ButtonPress and ButtonRelease events when the mouse
   moves from one key to another.
 */

void
keyboard_track_motion_hook (action_widget, client_data, action_name,
			    event, argv, argc)
     Widget action_widget;
     XtPointer client_data;
     char *action_name;
     XEvent *event;
     char **argv;
     int *argc;
{
  if (action_widget != (Widget) client_data)	/* not the KeyboardWidget */
    return;
  if (event->xany.type != MotionNotify)
    return;
  {
    KeyboardWidget widget = (KeyboardWidget) action_widget;
    struct keyboard *kbd = widget->keyboard.kbd;
    Display *dpy = XtDisplay (widget);
    KeyWidget last_key_under_mouse = widget->keyboard.key_under_mouse;
    Window root, child;
    int root_x, root_y, x, y;
    unsigned int mask;

    /* This call to XQueryPointer is just to tell the server that it's
       ok to start sending motion events again.  When MotionHintMask
       is used, we only get one MotionNotify event until something
       interesting happens again.
     */
    XQueryPointer (dpy, XtWindow (widget), &root, &child,
		   &root_x, &root_y, &x, &y, &mask);
  
    widget->keyboard.key_under_mouse = window_to_key (dpy, child);

    if (widget->keyboard.tracking_key &&
	widget->keyboard.key_under_mouse
	&& widget->keyboard.key_under_mouse != last_key_under_mouse
	&& !widget->keyboard.key_under_mouse->key.modifier_bits)
      /* If we're in tracking-mode and the mouse has entered a key that it
	 wasn't within before, then simulate a ButtonUp event on the key
	 the mouse was last over, and then simulate a ButtonDown event on
	 the key the mouse is over now.  But don't do that if the key the
	 mouse is over now is a modifier key, because that breaks the
	 "ifmod" arguments to actions.
	 
	 We simulate these events so that the correct sets of actions get
	 invoked.  Remember that the user may have changed the actions.
       */
      {
	XEvent sim_event;
	
	/* Put back a ButtonDown event for the current key first.
	 */
	sim_event.xany.type = ButtonPress;
	sim_event.xbutton.send_event = 1;
	sim_event.xbutton.display = dpy;
	sim_event.xbutton.window = XtWindow (widget); /* the KeyboardWidget */
	sim_event.xbutton.root = event->xmotion.root;
	sim_event.xbutton.subwindow =
	  (widget->keyboard.key_under_mouse
	   ? XtWindow (widget->keyboard.key_under_mouse)
	   : 0);
	sim_event.xbutton.time = event->xmotion.time;
	sim_event.xbutton.x = event->xmotion.x;
	sim_event.xbutton.y = event->xmotion.y;
	sim_event.xbutton.x_root = event->xmotion.x_root;
	sim_event.xbutton.y_root = event->xmotion.y_root;
	sim_event.xbutton.state = event->xmotion.state;
	sim_event.xbutton.button = widget->keyboard.tracking_key;
	sim_event.xbutton.same_screen = 1;
	XPutBackEvent (dpy, &sim_event);

	/* Now put back a ButtonUp event for the previous key.
	   We do it in this order because we're pushing on the front.
	 */
	sim_event.xany.type = ButtonRelease;
	sim_event.xbutton.subwindow =
	  (last_key_under_mouse ? XtWindow (last_key_under_mouse) : 0);
	sim_event.xbutton.x = (last_key_under_mouse
			       ? last_key_under_mouse->core.x : 0);
	sim_event.xbutton.y = (last_key_under_mouse
			       ? last_key_under_mouse->core.y : 0);
	sim_event.xbutton.x_root = -1; /* This is hard, blow it off. */
	sim_event.xbutton.y_root = -1;
	XPutBackEvent (dpy, &sim_event);
      }
  }
}



static unsigned long simulated_modifier_state = 0;
static unsigned long real_modifier_state = 0;


#define key_highlighted_p(KEY) \
  (((KEY)->key.mouse_highlighted > 0) || ((KEY)->key.key_highlighted > 0))

static void
mouse_highlight_key (widget)
     KeyWidget widget;
{
  KeyboardWidget keyboard = (KeyboardWidget) widget->core.parent;
  if (!key_highlighted_p (widget)) {
    KeyHighlight (widget);
#if 0
    printf (" simulated: 0x%02x | 0x%02x = 0x%02x\n",
	    simulated_modifier_state, widget->key.modifier_bits,
	    simulated_modifier_state | widget->key.modifier_bits);
#endif
    simulated_modifier_state |= widget->key.modifier_bits;
    widget->key.mouse_highlighted++;
    keyboard->keyboard.mouse_highlighted_key = widget;
  }
}


static void
mouse_dehighlight_key (widget)
     KeyWidget widget;
{
  KeyboardWidget keyboard = (KeyboardWidget) widget->core.parent;
  struct key *key = widget->key.key;
  Screen *screen = DefaultScreenOfDisplay (XtDisplay (widget));

  widget->key.mouse_highlighted--;
  if (key_highlighted_p (widget)) return;
  if (widget->key.mouse_highlighted < 0)
    {
      printf ("%s: ERROR: more ButtonUps than ButtonDowns on \"%s\"\n",
	      progname, widget->key.key_name);
      widget->key.mouse_highlighted = 0;
    }
  KeyDehighlight (widget);

  keyboard->keyboard.mouse_highlighted_key = 0;
#if 0
  printf (" simulated: 0x%02x & ~0x%02x = 0x%02x\n",
	  simulated_modifier_state, widget->key.modifier_bits,
	  simulated_modifier_state & ~widget->key.modifier_bits);
#endif
  simulated_modifier_state &= ~widget->key.modifier_bits;
}


static void
highlight_key (widget)
     KeyWidget widget;
{
  KeyboardWidget keyboard = (KeyboardWidget) widget->core.parent;
  struct key *key = widget->key.key;

/*  printf ("%s: KeyPress %s\n", progname, widget->key.key_name);*/

  if (!key_highlighted_p (widget)) {
    KeyHighlight (widget);
#if 0
    printf (" real     : 0x%02x | 0x%02x = 0x%02x\n",
	    real_modifier_state, widget->key.modifier_bits,
	    real_modifier_state | widget->key.modifier_bits);
#endif
    real_modifier_state |= widget->key.modifier_bits;
  }
  if (widget->key.key_highlighted)
    /* We really shouldn't ever see more than one KeyPress on a given keycode
       without an intervening KeyRelease, but we can if the keyboard focus
       moves away from our window between the KeyPress and KeyRelease.  Cope
       with this by not incrementing the key-press-count to >1.  This could
       also happen if two keys on the keyboard had the same keycode, which
       would make for a terribly bogus keyboard.
     */
    printf ("%s: ERROR: more KeyPresses than KeyReleases on \"%s\"\n",
	    progname, widget->key.key_name);
  else
    widget->key.key_highlighted++;

  keyboard->keyboard.key_state_vector [key->keycode / 8]
    |= (1 << (key->keycode % 8));
}

static void
dehighlight_key (widget)
     KeyWidget widget;
{
  KeyboardWidget keyboard = (KeyboardWidget) widget->core.parent;
  struct key *key = widget->key.key;
  Screen *screen = DefaultScreenOfDisplay (XtDisplay (widget));
  widget->key.key_highlighted--;

/*  printf ("%s: KeyRelease %s\n", progname, widget->key.key_name); */

  keyboard->keyboard.key_state_vector [key->keycode / 8] &=
    ~(1 << (key->keycode % 8));

  if (widget->key.key_highlighted < 0)
    {
      printf ("%s: ERROR: more KeyReleases than KeyPresses on \"%s\"\n",
	      progname, widget->key.key_name);
      widget->key.key_highlighted = 0;
    }
  if (key_highlighted_p (widget)) return;

  KeyDehighlight (widget);
#if 0
  printf (" real     : 0x%02x & ~0x%02x = 0x%02x\n",
	  real_modifier_state, widget->key.modifier_bits,
	  real_modifier_state & ~widget->key.modifier_bits);
#endif
  real_modifier_state &= ~widget->key.modifier_bits;
}



void
key_to_event (key, event, down_p)
     KeyWidget key;
     XEvent *event;
     int down_p;
{
  event->xkey.type = down_p ? KeyPress : KeyRelease;
  event->xkey.display = XtDisplay (key);
  event->xkey.time = CurrentTime;
  event->xkey.x = event->xkey.y = 0;
  event->xkey.x_root = event->xkey.y_root = 0;
  event->xkey.state = (simulated_modifier_state | real_modifier_state);
  event->xkey.keycode = key->key.key->keycode;
}

extern Window XSendEvent_BadWindow;

static void
simulate_key_event (widget, down_p)
     KeyWidget widget;
     int down_p;
{
  KeyboardWidget keyboard = (KeyboardWidget) widget->core.parent;
  struct keyboard *kbd = keyboard->keyboard.kbd;
  struct key *key = widget->key.key;
  XEvent event;
  Window window;
  int revert_to;
  if (! key->keycode) return;
  XGetInputFocus (XtDisplay (widget), &window, &revert_to);

  /* If this widget (or any parent of it) has the keyboard focus, then send
     the events to the window the user has picked.
   */
  if (window == PointerRoot)
    window = keyboard->keyboard.target_window;
  else
    {
      Widget parent;
      for (parent = (Widget) widget; parent; parent = XtParent (parent))
	if (XtWindow (parent) == window)
	  {
	    window = keyboard->keyboard.target_window;
	    break;
	  }
    }
  if (!window) return;
  key_to_event (widget, &event, down_p);
  event.xkey.window = window;
#ifdef HAVE_XTRAP
  if (keyboard->keyboard.trap_data)
    xkeycaps_xtrap_simulate_event (keyboard, &event);
  else
#endif /* HAVE_XTRAP */
  if (! XSendEvent (XtDisplay (widget), window, True,
		    down_p ? KeyPressMask : KeyReleaseMask,
		    &event))
    {
      fprintf (stderr, "%s: Couldn't simulate a keypress: ", progname);
      if (window == None) fprintf (stderr, "No focus window\n");
      else
	fprintf (stderr, "focus window doesn't accept XSendEvent input\n");
    }
  XSync (XtDisplay (widget), 0);
  if (XSendEvent_BadWindow &&
      keyboard->keyboard.target_window == XSendEvent_BadWindow)
    {
      char buf [255];
      sprintf (buf,
	       "Window 0x%X seems to have gone away.  Keyboard focus cleared.",
	       XSendEvent_BadWindow);
      message (keyboard, buf);
      XBell (XtDisplay ((Widget) widget), 0);
      XSendEvent_BadWindow = 0;
      keyboard->keyboard.target_window = 0;
    }
}


int mappingNotify_event_expected = 0;

static Bool
mapping_event_p (dpy, event, closure)
     Display *dpy;
     XEvent *event;
     char *closure;
{
  if (event->xany.type == MappingNotify) *closure = 1;
  return False;
}

void
keyboard_handle_mapping_notify (widget, client_data, event, cont)
     Widget widget;
     XtPointer client_data;
     XEvent *event;
     Boolean *cont;
{
  struct keyboard *kbd = ((KeyboardWidget) widget)->keyboard.kbd;
  char events_queued_p = 0;
  XRefreshKeyboardMapping (&event->xmapping);
  init_modifier_mapping (widget);
  XSync (XtDisplay (widget), 0);
  if (XCheckIfEvent (XtDisplay (widget), event,
		     mapping_event_p, &events_queued_p))
    exit (-69); /* can't happen. */

  if (!events_queued_p && ((KeyboardWidget) widget)->keyboard.documented_key)
    /* refresh, in case the documented key has been modified */
    {
      describe_key (((KeyboardWidget) widget)->keyboard.documented_key);
      if (! mappingNotify_event_expected)
	message ((KeyboardWidget) widget,
		 "MappingNotify event received; keyboard state refreshed.");
      else
	mappingNotify_event_expected = 0;
    }
}

void
keyboard_handle_keymap_notify (widget, client_data, event, cont)
     Widget widget;
     XtPointer client_data;
     XEvent *event;
     Boolean *cont;
{
  /* To prevent getting out of sync with the real state of the
     keyboard during those unfortunate moments when we don't have
     the keyboard focus, we select KeymapNotify, which gives us
     the same data as XQueryKeymap would, but sends it just after
     any FocusIn or EnterNotify event would have been generated.
     Unfortunately, this means that this event will be generated
     every time the cursor move from inside of a key to the top-
     level keyboard window (since a particular kind of EnterNotify
     event would be generated there.)  This shouldn't be too big
     a deal: it's just another 40 or so bytes, and a call to bcmp.
   */
  struct keyboard *kbd = ((KeyboardWidget) widget)->keyboard.kbd;
  int i, j;
  
  if (! kbd) return; /* in the process of switching displayed keyboards? */

#ifdef HAVE_XTRAP
  if (((KeyboardWidget) widget)->keyboard.trap_data)
    return;	/* if we're in XTrap mode, this function is redundant */
#endif

  if (! bcmp (event->xkeymap.key_vector,
	      ((KeyboardWidget) widget)->keyboard.key_state_vector, 32))
    return;	/* Nothing's changed, bug out. */
  
  for (i = 0; i < kbd->nrows; i++)
    for (j = 0; j < kbd->rows[i].nkeys; j++)
      {
	struct key *key = &kbd->rows[i].keys[j];
	int down = ((event->xkeymap.key_vector [key->keycode / 8])
		    & (1 << (key->keycode % 8)));
	if (! key->widget) continue;
	if (! key->keycode) continue;
	if (down && !key->widget->key.key_highlighted)
	  /* Down now, wasn't before */
	  highlight_key (key->widget);
	else if (!down && key->widget->key.key_highlighted)
	  /* Not down now, was before */
	  dehighlight_key (key->widget);
      }
#if 0
  /* This sometimes fails because the event contains random noise in the
     bits which correspond to a keycode for which there is no physical key.
   */
  if (bcmp (event->xkeymap.key_vector, kbd->key_state_vector, 32))
    fprintf (stderr,
	     "%s: KeymapNotify handling is way broken...\n",
	     progname);
#endif
}




KeyWidget
window_to_key (dpy, window)
     Display *dpy;
     Window window;
{
  KeyWidget key = (KeyWidget) XtWindowToWidget (dpy, window);
  if (! key) return 0;
  if (! XtIsSubclass ((Widget) key, keyWidgetClass)) return 0;
  return key;
}


KeyWidget
keycode_to_key (keyboard, code)
     KeyboardWidget keyboard;
     KeyCode code;
{
  int i, j;
  struct keyboard *kbd = keyboard->keyboard.kbd;
  if (! code) return 0;
  for (i = 0; i < kbd->nrows; i++)
    for (j = 0; j < kbd->rows[i].nkeys; j++)
      if (kbd->rows[i].keys[j].keycode == code)
	return (KeyWidget) kbd->rows[i].keys[j].widget;
  return 0;
}


static void
clear_modifier_flags (widget)
     KeyboardWidget widget;
{
  struct keyboard *kbd = widget->keyboard.kbd;
  int i, j;
  bzero (widget->keyboard.modifier_vector, 32);
  for (i = 0; i < kbd->nrows; i++)
    for (j = 0; j < kbd->rows[i].nkeys; j++)
      if (kbd->rows[i].keys[j].widget)
	kbd->rows[i].keys[j].widget->key.modifier_bits = 0;
}


void
init_modifier_mapping (widget)
     KeyboardWidget widget;
{
  Display *dpy = XtDisplay (widget);
  struct keyboard *kbd = widget->keyboard.kbd;
  XModifierKeymap *modifier_map;
  int modifier_index, modifier_key, column, i, j;
  XKeyboardState kbd_state;
  XKeyboardControl kbd_control;
  int mkpm;

  clear_modifier_flags (widget);
  modifier_map = XGetModifierMapping (dpy);
  /* Look at the defined modifier keys, and annotate our key structures
     with which modifier bits they set.  This will be done each time the
     user runs xmodmap as well.
   */
  mkpm = modifier_map->max_keypermod;
  for (modifier_index = 0; modifier_index < 8; modifier_index++)
    for (modifier_key = 0; modifier_key < mkpm; modifier_key++)
      {
	int index = modifier_index * mkpm + modifier_key;
	int keycode = modifier_map->modifiermap [index];
	KeyWidget key = keycode_to_key (widget, keycode);
	if (! key) continue;
	key->key.modifier_bits |= (1 << modifier_index);
	widget->keyboard.modifier_vector [keycode / 8]
	  |= (1 << (keycode % 8));
      }
  XFreeModifiermap (modifier_map);

# if 0
  kbd_control.auto_repeat_mode = 0;
  XChangeKeyboardControl (dpy, KBAutoRepeatMode, &kbd_control);
# endif

  XGetKeyboardControl (dpy, &kbd_state);
  for (i = 0; i < kbd->nrows; i++)
    for (j = 0; j < kbd->rows[i].nkeys; j++)
      {
	struct key *key = &kbd->rows[i].keys[j];
	if (key->widget)
	  key->widget->key.auto_repeat_p
	    = !!((kbd_state.auto_repeats [key->keycode / 8])
		 & (1 << (key->keycode % 8)));
      }
}
