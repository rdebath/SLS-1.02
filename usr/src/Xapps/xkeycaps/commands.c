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
#include <X11/X.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xproto.h>

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <X11/Xmu/WinUtil.h>
#include <X11/Xmu/Error.h>

#include <X11/Xaw/Simple.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/Scrollbar.h>

/* We need all this bullshit because the Toggle widget (really, the Command
   widget) doesn't provide a GetValues-hook to access the "state" property.
   Fuck me harder.
 */
#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include <X11/Xaw/SimpleP.h>
#include <X11/Xaw/CommandP.h>

#include "KbdWidgetP.h"
#include "KeyWidgetP.h"

#include "xkeycaps.h"

#include "vroot.h"	/* This is standard in R5 but not R4 */

#ifndef NO_PWD
#include <pwd.h>
#endif

/* We can only do Vendor keysyms in X11r5, because earlier versions 
   didn't provide any way to map over the contents of an xrm database.
 */
#ifdef XtSpecificationRelease
# if XtSpecificationRelease >= 5
#  define DO_VENDOR_KEYSYMS
# endif
#endif

extern int mappingNotify_event_expected;

struct key_menus {
  Widget popup;
  Widget popup_kids [10];
  struct edit_key_box *edit_key_box;
};

#ifndef isupper
# define isupper(c)  ((c) >= 'A' && (c) <= 'Z')
#endif
#ifndef _tolower
# define _tolower(c)  ((c) - 'A' + 'a')
#endif

int
string_equal (s1, s2)
     char *s1, *s2;
{
  if (s1 == s2) return 1;
  if (!s1 || !s2) return 0;
  while (*s1 && *s2)
    {
      if ((isupper (*s1) ? _tolower (*s1) : *s1) !=
	   (isupper (*s2) ? _tolower (*s2) : *s2))
	 return 0;
       s1++, s2++;
     }
  return ((*s1 || *s2) ? 0 : 1);
}



int y_or_n_p ();

extern Widget make_label (), make_label_1 ();

Widget
make_button (parent, name, string, left, top, callback, data, menu_name)
     Widget parent;
     char *name, *string;
     Widget left, top;
     void (*callback) ();
     XtPointer data;
     char *menu_name;
{
  Widget w = make_label_1 (parent, name, string, left, top,
			   (menu_name
			    ? menuButtonWidgetClass
			    : commandWidgetClass),
			   callback, data);
  if (menu_name)
    {
      Arg av [10];
      int ac = 0;
      XtSetArg (av [ac], XtNmenuName, menu_name); ac++;
      XtSetValues (w, av, ac);
    }
  return w;
}


Widget
make_toggle (parent, name, left, top, state, callback, data, label,
	     radio_group, radio_data)
     Widget parent;
     char *name;
     Widget left, top;
     int state;
     void (*callback) ();
     XtPointer data;
     char *label;
     Widget radio_group;
     XtPointer radio_data;
{
  Arg av [20];
  int ac = 0;
  Widget w;
  XtSetArg (av[ac], XtNleft, XtChainLeft); ac++;
  XtSetArg (av[ac], XtNright, XtChainLeft); ac++;
  XtSetArg (av[ac], XtNtop, XtChainTop); ac++;
  XtSetArg (av[ac], XtNbottom, XtChainTop); ac++;
  XtSetArg (av[ac], XtNjustify, XtJustifyLeft); ac++;
  XtSetArg (av[ac], XtNstate, (state ? True : False)); ac++;
  if (left) XtSetArg (av[ac], XtNfromHoriz, left),  ac++;
  if (top) XtSetArg (av[ac], XtNfromVert, top),  ac++;
  if (label) XtSetArg (av[ac], XtNlabel, label),  ac++;
  if (radio_group) XtSetArg (av[ac], XtNradioGroup, radio_group),  ac++;
  if (radio_data) XtSetArg (av[ac], XtNradioData, radio_data),  ac++;
  w = XtCreateManagedWidget (name, toggleWidgetClass, parent, av, ac);
  if (callback) XtAddCallback (w, XtNcallback, callback, data);
  return w;
}




static void
button_quit (widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
  exit (0);
}


static int modify_keyboard_modifiers (), modify_keyboard ();

static void
button_restore (button, client_data, call_data)
     Widget button;
     XtPointer client_data, call_data;
{
  KeyboardWidget widget = *((KeyboardWidget *) client_data);
  KeySym *keysyms;
  KeyCode lowest = 255, highest = 0;
  XModifierKeymap *modmap;
  struct keyboard *kbd = widget->keyboard.kbd;
  int per_code = widget->keyboard.default_keysyms_per_code;
  int i, j, k;

  if (0 != y_or_n_p (widget, "restoreQuery", "yes", "no", 0))
    {
      message (widget, "Aborted.");
      return;
    }
  keysyms = (KeySym *) calloc (sizeof (KeySym), per_code * 256);
  modmap = XNewModifiermap (2); /* It'll grow */
  bzero (modmap->modifiermap, modmap->max_keypermod * 8);

  for (i = 0; i < kbd->nrows; i++)
    {
      struct row *row = &kbd->rows [i];
      for (j = 0; j < row->nkeys; j++)
	{
	  struct key *key = &row->keys [j];
	  if (key->keycode)
	    {
	      if (key->keycode < lowest)
		lowest = key->keycode;
	      if (key->keycode > highest)
		highest = key->keycode;
	      for (k = 0; k < per_code; k++)
		keysyms [key->keycode * per_code + k] =
		  key->default_keysyms [k];
	      if (key->default_mods)
		for (k = 0; k < 8; k++)
		  if (key->default_mods & (1<<k))
		    modmap = XInsertModifiermapEntry (modmap, key->keycode, k);
	    }
	}
    }
  if (highest <= lowest) exit (-69); /* can't happen */

  if (! modify_keyboard (widget, lowest, per_code,
			 keysyms + (lowest * per_code),
			 highest - lowest, modmap))
    message (widget, "Keyboard restored to default state.");
  XFreeModifiermap (modmap);
  free (keysyms);
}

extern char *version, *short_version;

extern KeyWidget keycode_to_key ();

static char *bit_names[] = { "Shift", "Lock", "Control", "Mod1",
			      "Mod2", "Mod3", "Mod4", "Mod5" };

static void
button_write (button, client_data, call_data)
     Widget button;
     XtPointer client_data, call_data;
{
  KeyboardWidget widget = *((KeyboardWidget *) client_data);
  XModifierKeymap *current_modmap, *default_modmap;
  struct keyboard *kbd = widget->keyboard.kbd;
  int per_code = widget->keyboard.default_keysyms_per_code;
  struct { struct key *key; int count; char *names[8]; }
     all [256], differences [256];
  int all_count = 0, diff_count = 0;
  KeySym *keysyms;
  int count, i, j, k;
  int partial = y_or_n_p (widget, "writeQuery", "full", "partial", "abort");
  long now = time ((long *) 0);
#ifdef NO_PWD
  char *userid = 0;
#else
  struct passwd *pw = (struct passwd *) getpwuid (getuid ());
  char *userid = (pw ? pw->pw_name : 0);
#endif
  KeyCode added [8][255];
  KeyCode subtracted [8][255];
  int added_count, subtracted_count;
  int cmkpm, dmkpm;
  int any_mod_changes = 0;

  if (partial >= 2)
    {
      message (widget, "Aborted.");
      return;
    }

  current_modmap = XGetModifierMapping (XtDisplay (widget));
  default_modmap = XNewModifiermap (2); /* It'll grow */
  bzero (default_modmap->modifiermap, default_modmap->max_keypermod * 8);

  for (i = 0; i < kbd->nrows; i++)
    {
      struct row *row = &kbd->rows [i];
      for (j = 0; j < row->nkeys; j++)
	{
	  struct key *key = &row->keys [j];
	  if (key->keycode)
	    {
	      unsigned long bits = key->widget->key.modifier_bits;
	      keysyms = XGetKeyboardMapping (XtDisplay (widget), key->keycode,
					     1, &count);
	      if (! keysyms) count = 0;
	      all [all_count].key = key;
	      for (; count > 0; count--)
		if (keysyms [count-1]) break;
	      if (count == 0)
		{
		  all [all_count].names [0] = "NoSymbol";
		  count = 1;
		}
	      else
		for (k = 0; k < count; k++)
		  {
		    char *str = "NoSymbol";
		    if (keysyms [k])
		      {
			str = XKeysymToString (keysyms [k]);
			if (! str)
			  {
			    /* core leak, but this shouldn't ever happen
			       unless there's a bug in libX11.a, or the
			       user did something stupid with xmodmap.
			     */
			    str = (char *) malloc (255);
			    sprintf (str, "0x%02X", keysyms [k]);
			  }
		      }
		    all [all_count].names [k] = str;
		  }
	      all [all_count].count = count;
	      if (! keysyms) count = 0;
	      if (count > per_code ||
		  ((!partial) && (bits != key->default_mods)) ||
		  (count > 0 && keysyms [0] != key->default_keysyms [0]) ||
		  (count > 1 && keysyms [1] != key->default_keysyms [1]) ||
		  (count > 2 && keysyms [2] != key->default_keysyms [2]) ||
		  (count > 3 && keysyms [3] != key->default_keysyms [3]) ||
		  (count > 4 && keysyms [4] != key->default_keysyms [4]) ||
		  (count > 5 && keysyms [5] != key->default_keysyms [5]) ||
		  (count > 6 && keysyms [6] != key->default_keysyms [6]) ||
		  (count > 7 && keysyms [7] != key->default_keysyms [7]))
		differences [diff_count++] = all [all_count];
	      all_count++;
	      for (k = 0; k < 8; k++)
		{
		  if (key->default_mods & (1<<k))
		    default_modmap = XInsertModifiermapEntry (default_modmap,
							      key->keycode, k);
		  if (bits & (1<<k))
		    current_modmap = XInsertModifiermapEntry (current_modmap,
							      key->keycode, k);
		}
	      if (keysyms) XFree ((char *) keysyms);
	    }
	}
    }

  /* I'd just like to take this moment to point out that C has all
     the expressive power of two dixie cups and a string.
   */
  cmkpm = current_modmap->max_keypermod;
  dmkpm = default_modmap->max_keypermod;
  bzero (added, sizeof (added));
  bzero (subtracted, sizeof (subtracted));

  for (i = 0; i < 8; i++)
    {
      KeyCode kc1, kc2;
      added_count = subtracted_count = 0;
      if (partial)
	{
	  for (j = 0; j < cmkpm; j++)
	    {
	      kc1 = current_modmap->modifiermap [i * cmkpm + j];
	      if (!kc1) continue;
	      for (k = 0; k < dmkpm; k++)
		{
		  kc2 = default_modmap->modifiermap [i * dmkpm + k];
		  if (kc1 == kc2) break;
		}
	      if (kc1 != kc2) added [i][added_count++] = kc1;
	    }
	  for (j = 0; j < dmkpm; j++)
	    {
	      kc1 = default_modmap->modifiermap [i * dmkpm + j];
	      if (!kc1) continue;
	      for (k = 0; k < cmkpm; k++)
		{
		  kc2 = current_modmap->modifiermap [i * cmkpm + k];
		  if (kc1 == kc2) break;
		}
	      if (kc1 != kc2) subtracted [i][subtracted_count++] = kc1;
	    }
	  if (added_count || subtracted_count) any_mod_changes = 1;
	}
      else
	{
	  for (j = 0; j < cmkpm; j++)
	    {
	      kc1 = current_modmap->modifiermap [i * cmkpm + j];
	      if (kc1)
		added [i][added_count++] = kc1;
	    }
	}
    }

  printf ("!\n! This is an `xmodmap' input file for the %s keyboard.\n",
	  kbd->long_name);
  printf ("! This file was automatically generated on %s", ctime (&now));
  if (userid)
    printf ("! by %s with %s.\n!\n", userid, short_version);
  else
    printf ("! with %s.\n", version);
  if (partial && any_mod_changes)
    {
      printf ("! This file presupposes that the keyboard is in the ");
      printf ("default state, and\n! may malfunction if it is not.\n!\n");
    }
  else if (! partial)
    {
      if (diff_count)
	printf ("! This file makes the following changes:\n!\n");
      else
	printf ("! This file encodes the default state.\n!\n");
    }

  /* If we're going to write out "remove" commands, do it before writing
     out any "keycode" statements, since xmodmap is so lame.  We only
     generate "remove" statements in "partial" mode.
   */
  any_mod_changes = 0;
  if (partial)
    for (i = 0; i < 8; i++)
      {
	char *str;
	KeySym ks;
	if (subtracted [i][0])
	  {
	    printf ("remove %-8s=", bit_names [i]);
	    for (j = 0; j < sizeof (subtracted [0]); j++)
	      {
		KeyWidget kw;
		if (! subtracted [i][j]) break;
		any_mod_changes = 1;
		/* note: we don't use the *current* keysym corresponding to the
		   keycode being removed here, but the *default* one...
		 */
		kw = keycode_to_key (widget, subtracted [i][j]);
		ks = kw->key.key->default_keysyms [0];
		str = XKeysymToString (ks);
		if (str)
		  printf (" %s", str);
		else
		  printf (" 0x04X", subtracted [i][j]);
	      }
	    printf ("\n");
	  }
      }
  if (any_mod_changes) printf ("\n");

  /* Write out the differences.  This happens in both "partial" and "full"
     modes, but in "full" mode it's in the form of a descriptive comment
     instead of xmodmap commands.
   */
  for (i = 0; i < diff_count; i++)
    {
      if (partial)
	printf ("keycode 0x%02X =\t", differences[i].key->keycode);
      else
	printf ("! The \"%s\" key generates ",
		differences[i].key->widget->key.key_name);
      for (j = 0; j < differences[i].count; j++)
	{
	  printf ("%s", differences[i].names[j]);
	  if (j+1 == differences[i].count) continue;
	  if (partial)
	    {
	      putchar ('\t');
	      if (strlen (differences[i].names[j]) < 8) putchar ('\t');
	    }
	  else
	    {
	      if (j+1 == differences[i].count-1)
		printf ((j == 0) ? " and " : ", and ");
	      else
		printf (", ");
	    }
	}
      /* write the diffs of the modifier bits in the full-mode comment. */
      if (!partial &&
	  differences[i].key->default_mods !=
	  differences[i].key->widget->key.modifier_bits)
	{
	  unsigned long bits = differences[i].key->widget->key.modifier_bits;
	  int this_mod_count = 0;
	  if (! bits)
	    printf (", and has no modifiers\n");
	  else
	    {
	      printf (", and the ");
	      for (k = 0; k < 8; k++)
		{
		  if (bits & (1<<k))
		    {
		      if (this_mod_count++) printf ("/");
		      printf (bit_names [k]);
		    }
		}
	      printf (" modifier%s\n", (this_mod_count>1 ? "s" : ""));
	    }
	}
      else
	printf ("\n");
    }

  /* In "full" mode, write out all the "keycode" commands.  This is the
     first actual xmodmap command text in full mode.
   */
  if (!partial)
    {
      printf ("\n");
      for (i = 0; i < all_count; i++)
	{
	  printf ("keycode 0x%02X =\t", all [i].key->keycode);
	  for (j = 0; j < all[i].count; j++)
	    {
	      printf ("%s", all[i].names[j]);
	      if (j == all[i].count - 1) continue;
	      putchar ('\t');
	      if (strlen (all[i].names[j]) < 8) putchar ('\t');
	    }
	  printf ("\n");
	}
      printf ("\n");
      printf ("clear Shift\nclear Lock\nclear Control\nclear Mod1\n");
      printf ("clear Mod2\nclear Mod3\nclear Mod4\nclear Mod5\n");
    }

  /* In partial mode, write out any "add" commands.  This is after any
     "keycode" commands have already been output (and any "remove" commands
     before them.)  In full mode, the "add" commands are the whole story.
   */
  printf ("\n");
  for (i = 0; i < 8; i++)
    {
      char *str;
      KeySym ks;
      if (added [i][0])
	{
	  printf ("add    %-8s=", bit_names [i]);
	  for (j = 0; j < sizeof (added [0]); j++)
	    {
	      if (! added [i][j]) break;
	      ks = XKeycodeToKeysym (XtDisplay (widget), added [i][j], 0);
	      str = XKeysymToString (ks);
	      if (str)
		printf (" %s", str);
	      else
		printf (" 0x04X", added [i][j]);
	    }
	  printf ("\n");
	}
    }
  fflush (stdout);
}


static void
button_pick_window (button, client_data, call_data)
     Widget button;
     XtPointer client_data, call_data;
{
  KeyboardWidget keyboard = *((KeyboardWidget *) client_data);
  XtAppContext app = XtWidgetToApplicationContext ((Widget) keyboard);
  Widget topmost, target;
  Display *dpy = XtDisplay (keyboard);
  Window root = RootWindowOfScreen (keyboard->core.screen);
  Window window = 0;
  int buttons = 0;
  int once = 0;
  XEvent event;

  message (keyboard, "Please select the window to type at:");

  if (XGrabPointer (dpy, root, False, ButtonPressMask | ButtonReleaseMask,
		    GrabModeSync, GrabModeAsync, root,
		    keyboard->keyboard.select_cursor, CurrentTime))
    {
      XBell (dpy, 0);
      message (keyboard, "Grab failed.");
      return;
    }
  
  while (window == 0 || buttons != 0)
    {
      /* allow one more event */
      XAllowEvents (dpy, SyncPointer, CurrentTime);
      XWindowEvent (dpy, root, ButtonPressMask|ButtonReleaseMask, &event);
      switch (event.type)
	{
	case ButtonPress:
	  once = 1;
	  if (window == 0)
	    window = event.xbutton.subwindow; /* window selected */
	  if (window == 0)
	    window = root;
	  buttons++;
	  break;
	case ButtonRelease:
	  if (buttons > 0) buttons--;
	  break;
	}
    }
  XUngrabPointer(dpy, CurrentTime);
  XSync (dpy, 0);

  if (window && window != root) window = XmuClientWindow (dpy, window);
  topmost = (Widget) keyboard;
  while (XtParent (topmost)) topmost = XtParent (topmost);
  target = XtWindowToWidget (XtDisplay (topmost), window);
  if (window == XtWindow (topmost)) target = topmost;
  if (target || window == root) window = 0;
  if (window)
    {
      char buf [255];
      char buf2 [100];
      char *string1 = 0, *string2 = 0, *string3 = 0;
      char *name;
      XClassHint classhint;
      classhint.res_name = classhint.res_class = 0;
      if (! XGetClassHint (dpy, window, &classhint))
	classhint.res_name = classhint.res_class = 0;

      string1 = classhint.res_name;
      string2 = classhint.res_class;
      XFetchName (dpy, window, &string3);
      name = string3;
      if (string2 && string3 && string_equal (string2, string3)) string3 = 0;
      if (string1 && string3 && string_equal (string1, string3)) string3 = 0;
      if (string1 && string2 && string_equal (string1, string2)) string2 = 0;
      if (!string2) string2 = string3, string3 = 0;
      if (!string1) string1 = string2, string2 = 0;
      if (string1 && string2 && string3)
	sprintf (buf2, "%s / %s / %s", string1, string2, string3);
      else if (string1 && string2)
	sprintf (buf2, "%s / %s", string1, string2);
      else if (string1)
	sprintf (buf2, "%s", string1);
      else
	sprintf (buf2, "unnamed");

      sprintf (buf, "Keyboard focus locked on to window 0x%X (%s)",
	       window, buf2);
      message (keyboard, buf);
      if ((classhint.res_name && string_equal (classhint.res_name, "xterm")) ||
	  (classhint.res_class && string_equal (classhint.res_class, "xterm")))
	message2 (keyboard,
	    "Remember to select \"Allow SendEvents\" from the XTerm menu.");
      if (classhint.res_name) XFree (classhint.res_name);
      if (classhint.res_class) XFree (classhint.res_class);
      if (name) XFree (name);
    }
  else
    message (keyboard, "Keyboard-focus cleared.");
  
  keyboard->keyboard.target_window = window;
}



static void
select_keyboard_cb (button, client_data, call_data)
     Widget button;
     XtPointer client_data, call_data;
{
  KeyboardWidget *keyboard = (KeyboardWidget *) client_data;
  char *name = 0;
  Arg av [10];
  int ac = 0;
  XtSetArg (av [ac], XtNlabel, &name); ac++;
  XtGetValues (button, av, ac);
  replace_keyboard (keyboard, name);
}



static int
modify_keyboard_modifiers (widget, modmap)
     KeyboardWidget widget;
     XModifierKeymap *modmap;
{
  Display *display = XtDisplay ((Widget) widget);
  int retries, timeout;
  char buf [255];
  for (retries = 4, timeout = 2; retries > 0; retries--, timeout *= 2)
    {
      int result;
      result = XSetModifierMapping (display, modmap);
      switch (result)
	{
	case MappingSuccess:
	  mappingNotify_event_expected = 1;
	  return 0;
	case MappingBusy:
	  sprintf (buf, "please release all keys withing %d seconds", timeout);
	  /* Calling message() doesn't work because exposes don't get
	     processed while we're sleeping.
	   */
	  message (widget, buf);
	  fprintf (stderr, "%s: %s\n", progname, buf);
	  XSync (display, 0);
	  sleep (timeout);
	  continue;
	case MappingFailed:
	  message (widget, "XSetModifierMapping() failed");
	  XBell (display, 0);
	  return -1;
	default:
	  sprintf (buf, "unknown return code %d from XSetModifierMapping()",
		   result);
	  message (widget, buf);
	  XBell (display, 0);
	  return -1;
	}
    }
  sprintf (buf, "XSetModifierMapping() failed", timeout);
  message (widget, buf);
  XBell (display, 0);
  return -1;
}


/* We install this as an error handler around the call to
   XChangeKeyboardMapping(), so that we can trap errors that that
   operation generates.  Gotta love this 1960's condition system...
 */
static int XChangeKeyboardMapping_error = 0;

static int
modify_keyboard_error_handler (dpy, error)
     Display *dpy;
     XErrorEvent *error;
{
  switch (error->request_code)
    {
    case X_ChangeKeyboardMapping:
      XChangeKeyboardMapping_error = 1;
      return 0;
    default:
      XmuPrintDefaultErrorMessage (dpy, error, stderr);
      exit (-1);
    }
}

static int
modify_keyboard (widget, first_keycode, keysyms_per_keycode, keysyms,
		 num_codes, modmap)
     KeyboardWidget widget;
     int first_keycode;
     int keysyms_per_keycode;
     KeySym *keysyms;
     int num_codes;
     XModifierKeymap *modmap;
{
  Display *display = XtDisplay ((Widget) widget);
  int (*old_handler) ();
  if (keysyms_per_keycode == 0) keysyms_per_keycode = 1;
  XSync (display, 0);
  mappingNotify_event_expected = 1;
  XChangeKeyboardMapping_error = 0;
  old_handler = XSetErrorHandler (modify_keyboard_error_handler);
  XChangeKeyboardMapping (display, first_keycode, keysyms_per_keycode,
			  keysyms, num_codes);
  /* Is there a race condition here?  Are we guarenteed that by calling
     XSync() we will get back an error generated by the previously-sent
     request?
   */
  XSync (XtDisplay ((Widget) widget), 0);
  XSetErrorHandler (old_handler);
  if (XChangeKeyboardMapping_error)
    {
      mappingNotify_event_expected = 0;
      message (widget, "XChangeKeyboardMapping() failed.");
      XBell (display, 0);
      return -1;
    }
  if (modmap)
    return modify_keyboard_modifiers (widget, modmap);
  return 0;
}


static void
restore_key_default (parent, client_data, call_data)
     Widget parent;
     XtPointer client_data, call_data;
{
  KeyboardWidget widget = (KeyboardWidget) client_data;
  KeyWidget key = widget->keyboard.key_under_mouse;
  KeySym *keysyms = key->key.key->default_keysyms;
  int per_code = widget->keyboard.default_keysyms_per_code;
  KeyCode code = key->key.key->keycode;
  unsigned long bits = key->key.key->default_mods;
  XModifierKeymap *modmap;
  int i, j, error;

  if (! code)
    {
      XBell (XtDisplay (widget), 0);
      message (widget, "That key generates no KeyCode.");
      return;
    }

  modmap = XGetModifierMapping (XtDisplay (widget));
  for (i = 0; i < 8; i++)
    if (bits & (1<<i))
      modmap = XInsertModifiermapEntry (modmap, code, i);
    else
      modmap = XDeleteModifiermapEntry (modmap, code, i);

  XSync (XtDisplay (widget), 0);
  error = modify_keyboard (widget, code, per_code, keysyms, 1, modmap);
  XFreeModifiermap (modmap);

  if (! error)
    {
      char *k, buf [255], buf2 [255], *b = buf;
      for (i = 0; i < per_code; i++)
	{
	  if (i) *b++ = ',', *b++ = ' ';
	  k = XKeysymToString (keysyms [i]);
	  if (keysyms [i] && !k)
	    {
	      sprintf (b, "0x%04X", keysyms [i]);
	      k = b;
	    }
	  else
	    {
	      if (! k) k = "NoSymbol";
	      strcpy (b, k);
	    }
	  b += strlen (k);
	}
      sprintf (buf2, "KeyCode 0x%02X restored to default state (%s)", 
	       key->key.key->keycode, buf);
      message (widget, buf2);
    }
  XSync (XtDisplay (widget), 0);
}


KeyWidget prompt_for_key ();


static void
swap_key (parent, client_data, call_data)
     Widget parent;
     XtPointer client_data, call_data;
{
  KeyboardWidget widget = (KeyboardWidget) client_data;
  XModifierKeymap *modmap = XGetModifierMapping (XtDisplay (widget));
  KeyWidget source_key = widget->keyboard.key_under_mouse;
  KeyWidget target_key;
  KeySym *source_keysyms;
  KeySym *target_keysyms;
  int source_count, target_count;
  unsigned long source_bits = source_key->key.modifier_bits;
  unsigned long target_bits;
  KeyCode source_code = source_key->key.key->keycode;
  KeyCode target_code;
  char buf [255];
  int i, error;

  sprintf (buf, "Click on the key to swap with 0x%02X (%s)",
	   source_key->key.key->keycode,
	   source_key->key.key_name);
  target_key = prompt_for_key (widget, buf);
  if (! target_key) return;

  target_bits = target_key->key.modifier_bits;
  target_code = target_key->key.key->keycode;
  
  if (source_code == target_code)
    {
      XBell (XtDisplay (widget), 0);
      message (widget, "Those keys are the same!");
      return;
    }

  for (i = 0; i < 8; i++)
    {
      if (source_bits & (1<<i))
	modmap = XInsertModifiermapEntry (modmap, target_code, i);
      else
	modmap = XDeleteModifiermapEntry (modmap, target_code, i);

      if (target_bits & (1<<i))
	modmap = XInsertModifiermapEntry (modmap, source_code, i);
      else
	modmap = XDeleteModifiermapEntry (modmap, source_code, i);
    }

  source_keysyms = XGetKeyboardMapping (XtDisplay (widget), source_code,
					1, &source_count);
  target_keysyms = XGetKeyboardMapping (XtDisplay (widget), target_code,
					1, &target_count);

  error = modify_keyboard ((Widget) widget, target_code,
			   source_count, source_keysyms, 1, modmap);
  if (error) return;
  error = modify_keyboard ((Widget) widget, source_code,
			   target_count, target_keysyms, 1, 0);
  if (error) return;

  sprintf (buf, "Keys 0x%02x (%s) and 0x%02x (%s) swapped.",
	   source_key->key.key->keycode, source_key->key.key_name,
	   target_key->key.key->keycode, target_key->key.key_name);
  message (widget, buf);

  if (source_keysyms) XFree ((char *) source_keysyms);
  if (target_keysyms) XFree ((char *) target_keysyms);
  if (modmap) XFreeModifiermap (modmap);
}


static void
clone_key (parent, client_data, call_data)
     Widget parent;
     XtPointer client_data, call_data;
{
  KeyboardWidget widget = (KeyboardWidget) client_data;
  XModifierKeymap *modmap = XGetModifierMapping (XtDisplay (widget));
  KeyWidget source_key = widget->keyboard.key_under_mouse;
  KeyWidget target_key;
  KeySym *source_keysyms;
  int source_count;
  unsigned long source_bits = source_key->key.modifier_bits;
  KeyCode source_code = source_key->key.key->keycode;
  KeyCode target_code;
  char buf [255];
  int i, error;

  sprintf (buf, "Click on the key to turn into a copy of 0x%02X (%s)",
	   source_key->key.key->keycode,
	   source_key->key.key_name);
  target_key = prompt_for_key (widget, buf);
  if (! target_key) return;

  target_code = target_key->key.key->keycode;
  
  if (source_code == target_code)
    {
      XBell (XtDisplay (widget), 0);
      message (widget, "Those keys are the same!");
      return;
    }
  for (i = 0; i < 8; i++)
    {
      if (source_bits & (1<<i))
	modmap = XInsertModifiermapEntry (modmap, target_code, i);
      else
	modmap = XDeleteModifiermapEntry (modmap, target_code, i);
    }

  source_keysyms = XGetKeyboardMapping (XtDisplay (widget), source_code,
					1, &source_count);
  error = modify_keyboard ((Widget) widget, target_code,
			   source_count, source_keysyms, 1, modmap);
  if (source_keysyms) XFree ((char *) source_keysyms);
  if (modmap) XFreeModifiermap (modmap);
  if (error) return;

  sprintf (buf, "Keys 0x%02x (%s) and 0x%02x (%s) are now the same.",
	   source_key->key.key->keycode, source_key->key.key_name,
	   target_key->key.key->keycode, target_key->key.key_name);
  message (widget, buf);
}



static void
disable_key (parent, client_data, call_data)
     Widget parent;
     XtPointer client_data, call_data;
{
  KeyboardWidget widget = (KeyboardWidget) client_data;
  KeyWidget key = widget->keyboard.key_under_mouse;
  KeyCode code = key->key.key->keycode;
  KeySym keysym = 0;
  int i, error;
  char buf [255];
  XModifierKeymap *modmap = XGetModifierMapping (XtDisplay (widget));
  for (i = 0; i < 8; i++)
    modmap = XDeleteModifiermapEntry (modmap, code, i);
  error = modify_keyboard ((Widget) widget, code, 1, &keysym, 1, modmap);
  XFreeModifiermap (modmap);
  if (! error)
    {
      sprintf (buf, "KeyCode 0x%02X (%s) disabled.", key->key.key->keycode,
	       key->key.key_name);
      message (widget, buf);
    }
  XSync (XtDisplay (widget), 0);
}



extern void key_menu_pre_popup_hook ();


void pop_up_key_dbox ();
static struct edit_key_box * make_edit_key_dbox ();

struct key_menus *
make_key_menus (widget)
     KeyboardWidget widget;
{
  Arg av [20];
  int ac = 0, i = 0;
  Widget menu, item;
  struct key_menus *key_menus = (struct key_menus *)
    malloc (sizeof (struct key_menus));
  bzero (key_menus->popup_kids, sizeof (key_menus->popup_kids));

  XawSimpleMenuAddGlobalActions (XtWidgetToApplicationContext((Widget)widget));

  XtSetArg (av[ac], XtNlabel, "keyMenu"); ac++;
  menu = XtCreatePopupShell ("keyMenu", simpleMenuWidgetClass,
			     (Widget) widget, av, ac);
  XtAddCallback (menu, XtNpopupCallback, key_menu_pre_popup_hook,
		 (XtPointer) widget);
  key_menus->popup = menu;
  ac = 0;
  item = XtCreateManagedWidget ("editKeysyms", smeBSBObjectClass, menu, av,ac);
  XtAddCallback (item, XtNcallback, pop_up_key_dbox, (XtPointer) widget);
  key_menus->popup_kids [i++] = item;

  item = XtCreateManagedWidget ("swapKey", smeBSBObjectClass, menu, av, ac);
  XtAddCallback (item, XtNcallback, swap_key, (XtPointer) widget);
  key_menus->popup_kids [i++] = item;

  item = XtCreateManagedWidget ("cloneKey", smeBSBObjectClass, menu, av, ac);
  XtAddCallback (item, XtNcallback, clone_key, (XtPointer) widget);
  key_menus->popup_kids [i++] = item;

  item = XtCreateManagedWidget ("disableKey", smeBSBObjectClass, menu, av, ac);
  XtAddCallback (item, XtNcallback, disable_key, (XtPointer) widget);
  key_menus->popup_kids [i++] = item;

  item = XtCreateManagedWidget ("restoreKey", smeBSBObjectClass, menu, av, ac);
  XtAddCallback (item, XtNcallback, restore_key_default, (XtPointer) widget);
  key_menus->popup_kids [i++] = item;

  key_menus->edit_key_box = make_edit_key_dbox (widget);
  return key_menus;
}

void
sensitize_menu (widget, menu, sensitive)
     KeyboardWidget widget;
     Widget menu;
     Bool sensitive;
{
  Arg av [10];
  int ac = 0, i = 0;
  struct key_menus *key_menus = widget->keyboard.key_menus;
  if (menu != key_menus->popup) return;
  XtSetArg (av [ac], XtNsensitive, sensitive); ac++;
  for (i = 0; i < sizeof (key_menus->popup_kids); i++)
    if (! key_menus->popup_kids [i]) return;
    else XtSetValues (key_menus->popup_kids [i], av, ac);
}



Widget
make_command_widgets (parent, kbd)
     Widget parent;
     Widget *kbd;
{
  Widget box, top, kmenu, item;
  Arg av [20];
  int i, ac = 0;
  XtSetArg (av[ac], XtNleft, XtChainLeft); ac++;
  XtSetArg (av[ac], XtNright, XtChainLeft); ac++;
  XtSetArg (av[ac], XtNtop, XtChainTop); ac++;
  XtSetArg (av[ac], XtNbottom, XtChainTop); ac++;
  box = XtCreateManagedWidget ("buttons", formWidgetClass, parent, av, ac);
  top = 0;
  top = make_button (box, "quit", NULL, 0, top, button_quit, kbd, 0);
  top = make_button (box, "keyboard", NULL, 0, top, 0, kbd, "keyboardMenu");
  top = make_button (box, "focus", NULL, 0, top, button_pick_window, kbd, 0);
  top = make_button (box, "restore", NULL, 0, top, button_restore, kbd, 0);
  top = make_button (box, "write", NULL, 0, top, button_write, kbd, 0);

  kmenu = XtCreatePopupShell ("keyboardMenu", simpleMenuWidgetClass, parent,
			      0, 0);
  ac = 0;
  for (i = 0; all_kbds [i]; i++)
    {
      XtSetArg (av[0], XtNlabel, all_kbds[i]->long_name);
      item = XtCreateManagedWidget ("selectKeyboard", smeBSBObjectClass,
				    kmenu, av, 1);
      XtAddCallback (item, XtNcallback, select_keyboard_cb, (XtPointer) kbd);
    }

  return box;
}



/* These are used to compute the default sizes of the windows.  Hack hack.
 */
#define LONGEST_KEYSYM_NAME "Greek_upsilonaccentdieresis"
#define MEDIUM_LENGTH_KEYSYM_NAME "Greek_IOTAdiaresis"

static char *all_keyset_names [] = {
  "Latin1",
  "Latin2",
  "Latin3",
  "Latin4",
  "Kana",
  "Arabic",
  "Cyrillic",
  "Greek",
  "Technical",
  "Special",
  "Publishing",
  "APL",
  "Hebrew",
  "Keyboard",
#ifdef DO_VENDOR_KEYSYMS
  "Vendor",	/* man, is this hairy... */
#endif
  0
};

#define KEYBOARD_CHARSET_INDEX 13
#ifdef DO_VENDOR_KEYSYMS
#define VENDOR_CHARSET_INDEX 14
#endif

static char **keysym_name_buffer = 0;
static int keysym_name_buffer_size = 0;

static void
ensure_keysym_name_buffer (size)
     int size;
{
  if (keysym_name_buffer_size >= size)
    return;
  if (keysym_name_buffer) free ((char *) keysym_name_buffer);
  keysym_name_buffer_size = size;
  keysym_name_buffer = (char **) malloc (sizeof (char *) * size);
}


#ifdef DO_VENDOR_KEYSYMS
struct vendor_keysym {
  char *name;
  KeySym keysym;
  int index;
};

static struct vendor_keysym *vendor_keysyms = 0;
static int n_vendor_keysyms = 0;

static XrmDatabase vendor_keysym_db = 0;

static Bool
init_vendor_keysyms_mapper (db, bindings, quarks, type, value, closure)
     XrmDatabase *db;
     XrmBindingList bindings;
     XrmQuarkList quarks;
     XrmRepresentation *type;
     XrmValue *value;
     XPointer closure;
{
  KeySym keysym;
  char c;
  int i;
  int *count = (int *) closure;
  if (quarks [1]) /* should only be one level deep. */
    abort ();

  if (! value->addr || value->size <= 1)
    return False;

  keysym = 0;
  for (i = 0; i < value->size - 1; i++)
    {
      c = ((char *) value->addr) [i];
      if ('0' <= c && c <= '9') keysym = (keysym<<4)+c-'0';
      else if ('a' <= c && c <= 'z') keysym = (keysym<<4)+c-'a'+10;
      else if ('A' <= c && c <= 'Z') keysym = (keysym<<4)+c-'A'+10;
      else
	{
	  fprintf (stderr, "%s: unparsable entry in XKeysymDB: \"%s: %s\"\n",
		   progname, XrmQuarkToString (quarks [0]),
		   (char *) value->addr);
	  return False;
	}
    }
  
  if (n_vendor_keysyms <= *count)
    {
      n_vendor_keysyms *= 2;
      vendor_keysyms = (struct vendor_keysym *)
	realloc (vendor_keysyms, sizeof (struct vendor_keysym) *
		 n_vendor_keysyms);
    }
  vendor_keysyms [*count].index = -1; /* we fill this in later */
  vendor_keysyms [*count].keysym = keysym;
  vendor_keysyms [*count].name = (char *) XrmQuarkToString (quarks [0]);
  (*count)++;
  return False;
}


static int
sort_vendor_keysyms_1 (left, right)
     int left, right;
{
  int L = left, R = right, middle;
  struct vendor_keysym swap;
  KeySym pivot = vendor_keysyms [left].keysym;
  while (L < R)
    {
      while (vendor_keysyms [L].keysym <= pivot && L <= right)
	L++;
      while (vendor_keysyms [R].keysym > pivot && R >= left)
	R--;
      if (L < R)
	{
	  swap = vendor_keysyms [L];
	  vendor_keysyms [L] = vendor_keysyms [R];
	  vendor_keysyms [R] = swap;
	}
    }
  middle = R;
  swap = vendor_keysyms [left];
  vendor_keysyms [left] = vendor_keysyms [middle];
  vendor_keysyms [middle] = swap;
  if (left < middle - 1)
    middle = sort_vendor_keysyms_1 (left, middle - 1);
  if (middle + 1 < right)
    middle = sort_vendor_keysyms_1 (middle + 1, right);
  return middle;
}


static void
sort_vendor_keysyms ()
{
  int i;
  sort_vendor_keysyms_1 (0, n_vendor_keysyms - 1);
  for (i = 0; i < n_vendor_keysyms; i++)
    {
      if (i > 0 && vendor_keysyms [i].keysym < vendor_keysyms [i-1].keysym)
	abort ();
      vendor_keysyms [i].index = i;
    }
}


#ifndef NO_XInitKeysymDB
extern XrmDatabase _XInitKeysymDB ();
#endif

static void
init_vendor_keysyms ()
{
  static int done = 0;
  int count = 0;
  XrmName name = { 0 };
  XrmClass class = { 0 };

  if (done) return;
  done = 1;

  if (! vendor_keysym_db)
#ifdef NO_XInitKeysymDB
    {
      char *dbname = (char *) getenv ("XKEYSYMDB");
# ifdef KEYSYMDB
      if (! dbname) dbname = KEYSYMDB;
# endif
      if (! dbname)
	{
	  fprintf (stderr,
		   "%s: don't know where to look for XKeysymDB.\n\
	Consider setting $XKEYSYMDB to point to it.\n", progname);
	  return;
	}
      keysymdb = XrmGetFileDatabase (dbname);
    }
#else /* we have _XInitKeysymDB(), so let Xlib be clever for us. */
    vendor_keysym_db = (XrmDatabase) _XInitKeysymDB ();
#endif

  n_vendor_keysyms = 255; /* probably not more than this; it's realloc'd. */
  vendor_keysyms = (struct vendor_keysym *)
    malloc (n_vendor_keysyms * sizeof (struct vendor_keysym));

  XrmEnumerateDatabase (vendor_keysym_db, &name, &class,
			XrmEnumOneLevel, init_vendor_keysyms_mapper,
			(XtPointer) &count);

  if (count < n_vendor_keysyms) /* might as well shrink it */
    {
      n_vendor_keysyms = count;
      vendor_keysyms = (struct vendor_keysym *)
	realloc (vendor_keysyms, count * sizeof (struct vendor_keysym));
    }
  else if (! count)
    {
      free ((char *) vendor_keysyms);
      n_vendor_keysyms = 0;
      vendor_keysyms = 0;
      return;
    }
  /* Hash order isn't very pretty; sort them by keysym numeric value. */
  sort_vendor_keysyms ();
}

static void
fill_keysym_name_buffer_with_vendor_keysyms ()
{
  int i;
  if (! vendor_keysyms) init_vendor_keysyms ();
  ensure_keysym_name_buffer (n_vendor_keysyms + 1);
  for (i = 0; i < n_vendor_keysyms; i++)
    keysym_name_buffer [i] = vendor_keysyms [i].name;
  keysym_name_buffer [i] = 0;
}

static int
vendor_keysym_to_small_index (keyboard, keysym)
     KeyboardWidget keyboard;
     KeySym keysym;
{
  int i;
  char buf [255];
  if (! vendor_keysyms) init_vendor_keysyms ();
  for (i = 0; i < n_vendor_keysyms; i++)
    if (keysym == vendor_keysyms [i].keysym)
      return vendor_keysyms [i].index;
  sprintf (buf, "Unrecognised vendor keysym 0x%x.", keysym);
  XBell (XtDisplay ((Widget) keyboard), 0);
  message (keyboard, buf);
  fprintf (stderr, "%s: %s\n", progname, buf);
  return 0;
}

#endif /* DO_VENDOR_KEYSYMS */


struct edit_key_box {
  KeyboardWidget keyboard;
  Widget shell;
  Widget label;
  Widget keysym_buttons [8];
  Widget mod_buttons [8];
  Widget keyset_list, keysym_list;
  Widget autorepeat_widget;
  int autorepeat;
};


static void
keyset_list_cb (widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
  XawListReturnStruct *lr = (XawListReturnStruct *) call_data;
  struct edit_key_box *box = (struct edit_key_box *) client_data;
  int set = lr->list_index;
  int i, j = 0;
  int set_start = 0;

  if (set == KEYBOARD_CHARSET_INDEX) set = 255;

  if (set != 0) set_start = 1; /* Latin1 is the only set that has NoSymbol */

#ifdef DO_VENDOR_KEYSYMS
  if (set == VENDOR_CHARSET_INDEX)
    fill_keysym_name_buffer_with_vendor_keysyms ();
  else
#endif
    {
      ensure_keysym_name_buffer (256);
      for (i = set_start; i < 256; i++)
	{
	  char *name = XKeysymToString ((set << 8) | i);
	  if (! name && i == 0) name = "NoSymbol";
	  if (name)
	    keysym_name_buffer [j++] = name;
	}
      keysym_name_buffer [j++] = 0;
    }
  XawListChange (box->keysym_list, keysym_name_buffer, 0, 0, True);
}


static void
keysym_list_cb (widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
  XawListReturnStruct *lr = (XawListReturnStruct *) call_data;
  struct edit_key_box *box = (struct edit_key_box *) client_data;
  int set = lr->list_index;
  int i = (int) XawToggleGetCurrent (box->keysym_buttons [0]);
  if (i > 0)
    {
      Arg av [10];
      int ac = 0;
      XtSetArg (av [ac], XtNlabel, lr->string); ac++;
      XtSetValues (box->keysym_buttons [i-1], av, ac);
    }
}


static void
autorepeat_button_cb (widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
  struct edit_key_box *box = (struct edit_key_box *) client_data;
  Arg av [10];
  int ac = 0;
  box->autorepeat = !box->autorepeat;
  XtSetArg (av [ac], XtNlabel, (box->autorepeat ? "Yes" : "No")); ac++;
  XtSetValues (widget, av, ac);
}


static void
abort_button_cb (widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
  struct edit_key_box *box = (struct edit_key_box *) client_data;
  XtPopdown (box->shell);
  message (box->keyboard, "Aborted.");
}


static void
ok_button_cb (button, client_data, call_data)
     Widget button;
     XtPointer client_data, call_data;
{
  struct edit_key_box *box = (struct edit_key_box *) client_data;
  KeyboardWidget widget = box->keyboard;
  KeyWidget key = widget->keyboard.key_under_mouse;
  KeyCode code = key->key.key->keycode;
  KeySym keysyms [8];
  int keysym_count;
  XModifierKeymap *modmap = XGetModifierMapping (XtDisplay (widget));
  int i, error;
  Arg av [10];
  int ac = 0;
  char *str;

  ac = 0;
  XtSetArg (av [ac], XtNlabel, &str); ac++;
  for (i = 0; i < 8; i++)
    {
      XtGetValues (box->keysym_buttons [i], av, ac);
      if (str && !strcmp (str, "NoSymbol")) str = 0;
      if (! str)
	keysyms [i] = 0;
      else
	{
	  keysyms [i] = XStringToKeysym (str);
	  if (! keysyms [i])
	    fprintf (stderr,
		     "%s: ERROR: XStringToKeysym(\"%s\") returned 0\n",
		     progname, str);
	}
    }
  for (keysym_count = 8; keysym_count > 0; keysym_count--)
    if (keysyms [keysym_count-1]) break;

  for (i = 0; i < 8; i++)
    {
      /* We should be able to do this with XtGetValues, but nooooo... */
      int state = ((CommandWidget) (box->mod_buttons [i]))->command.set;
      if (state)
	modmap = XInsertModifiermapEntry (modmap, code, i);
      else
	modmap = XDeleteModifiermapEntry (modmap, code, i);
    }

  XSync (XtDisplay (widget), 0);
  error = modify_keyboard ((Widget) widget, code, keysym_count,
			   keysyms, 1, modmap);
  XFreeModifiermap (modmap);

  if (!error && box->autorepeat != key->key.auto_repeat_p)
    {
      XKeyboardControl values;
      values.key = key->key.key->keycode;
      values.auto_repeat_mode =
	(box->autorepeat ? AutoRepeatModeOn : AutoRepeatModeOff);
      XChangeKeyboardControl (XtDisplay (widget), KBKey | KBAutoRepeatMode,
			      &values);
    }

  XtPopdown (box->shell);

  if (! error) message (box->keyboard, "Modified.");
}


static void
move_scrollbar (list, percent)
     Widget list;
     float percent;
{
  Widget vp = XtParent (list);
  Widget scrollbar = XtNameToWidget (vp, "vertical");
  float visible_fraction = (((float) vp->core.height) /
			    ((float) list->core.height));
  XEvent event;
  Arg av [10];
  int ac = 0;
  if (visible_fraction < 1.0)
    percent -= (visible_fraction / 2.0);
  XawScrollbarSetThumb (scrollbar, 0.0, -1.0);
  XtCallCallbacks (scrollbar, XtNjumpProc, (XtPointer) &percent);
}


static void
keysym_button_cb (widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
  struct edit_key_box *box = (struct edit_key_box *) client_data;
  KeyboardWidget keyboard = box->keyboard;
  KeyWidget key = keyboard->keyboard.key_under_mouse;
  char *keysym_name;
  KeySym real_keysym;
  int keyset, keysym, index, list_size;
  Arg av[10];
  int ac = 0;

  if (call_data == 0)	/* we're being toggled off */
    return;

  XtSetArg (av[ac], XtNlabel, &keysym_name); ac++;
  XtGetValues (widget, av, ac);
  real_keysym = XStringToKeysym (keysym_name);
  /* Get the one that's in the list */
  keysym_name = XKeysymToString (real_keysym);

  if (real_keysym > 0xFFFF)
    {
#ifdef DO_VENDOR_KEYSYMS
      keyset = VENDOR_CHARSET_INDEX;
      keysym = vendor_keysym_to_small_index (keyboard, real_keysym);
#else
      XBell (XtDisplay (keyboard), 0);
      message (keyboard,
	       "XKeyCaps was compiled without support for Vendor keysyms.");
      message2 (keyboard, "Consider upgrading to X11r5...");
      keyset = 0;
      keysym = 0;
#endif
    }
  else
    {
      keyset = (real_keysym >> 8);
      keysym = (real_keysym & 0xff);
      if (keyset == 255) keyset = KEYBOARD_CHARSET_INDEX;
    }

  list_size = (sizeof (all_keyset_names) / sizeof (all_keyset_names[0]));
  XawListHighlight (box->keyset_list, keyset);
  move_scrollbar (box->keyset_list, (((float) keyset) / ((float) list_size)));
  keyset_list_cb (box->keyset_list, box,
		  XawListShowCurrent (box->keyset_list));

  index = 256;
  for (list_size = 0; list_size < 256; list_size++)
    if (keysym_name_buffer [list_size] == keysym_name)
      index = list_size;
    else if (! keysym_name_buffer [list_size])
      break;
  if (! keysym_name) index = 0;
  if (index < 256)
    XawListHighlight (box->keysym_list, index);
  else
    index = 0;
  move_scrollbar (box->keysym_list, (((float) index) / ((float) list_size)));
}


static void
undo_button_cb (widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
  struct edit_key_box *box = (struct edit_key_box *) client_data;
  KeyboardWidget keyboard = box->keyboard;
  KeyWidget key = keyboard->keyboard.key_under_mouse;
  KeySym *keysyms;
  int syms_per_code;
  char buf [255];
  Arg av[20];
  int ac = 0;
  int i, j;

  keysyms = XGetKeyboardMapping (XtDisplay (widget), key->key.key->keycode,
				 1, &syms_per_code);

  sprintf (buf, "Definition of key 0x%02X (%s)", key->key.key->keycode,
	   key->key.key_name);
  XtSetArg (av[ac], XtNlabel, buf); ac++;
  XtSetValues (box->label, av, ac);
  ac = 0;

  for (i = 0; i < syms_per_code; i++)
    {
      char *sym;
      char buf [255];
      if (! keysyms [i])
	sym = "NoSymbol";
      else
	sym = XKeysymToString (keysyms [i]);
      if (! sym)
	{
	  sprintf (buf, "0x%04X", keysyms [i]);
	  sym = buf;
	}
      ac = 0;
      XtSetArg (av[ac], XtNlabel, sym); ac++;
      XtSetValues (box->keysym_buttons [i], av, ac);
    }
  if (keysyms) XFree ((char *) keysyms);
  ac = 0;
  XtSetArg (av[ac], XtNlabel, "NoSymbol"); ac++;
  for (; i < 8; i++)
    XtSetValues (box->keysym_buttons [i], av, ac);

  for (i = 0; i < 8; i++)
    {
      ac = 0;
      XtSetArg (av[ac], XtNstate,
		((key->key.modifier_bits & 1<<i) ? True : False)); ac++;
      XtSetValues (box->mod_buttons [i], av, ac);
    }

  XawToggleSetCurrent (box->keysym_buttons [0], (XtPointer) 1);
  keysym_button_cb (box->keysym_buttons [0], box, (void *) 1);
  box->autorepeat = !key->key.auto_repeat_p;
  autorepeat_button_cb (box->autorepeat_widget, box, 0);
}


static struct edit_key_box *
make_edit_key_dbox (widget)
     KeyboardWidget widget;
{
  struct edit_key_box *box = (struct edit_key_box *)
    malloc (sizeof (struct edit_key_box));
  Arg av [20];
  int ac = 0;
  Widget toplevel, box1, box2, box3;
  Widget keysym_box, button_box, keyset_box, keyset_syms_box, mod_box;
  Widget line_box, prev, prev_tog;
  Widget set_list, sym_list;
  Widget set_vp, sym_vp;

  toplevel = XtCreatePopupShell ("editKey", transientShellWidgetClass,
				 (Widget) widget, av, ac);
  box1 = XtCreateManagedWidget ("vertical", panedWidgetClass, toplevel, av,ac);
  box->label = make_label (box1, "label", 0, 0, 0);
  ac = 0;
  XtSetArg (av[ac], XtNorientation, "horizontal"); ac++;
  box2 = XtCreateManagedWidget ("horizontal", panedWidgetClass, box1, av, ac);

  ac = 0;
  keysym_box = XtCreateManagedWidget ("keysymBox", formWidgetClass, box2,
				      av, ac);
  prev = make_label (keysym_box, "symsOfCode", 0, 0, 0);
  prev = make_label (keysym_box, "spacer", "", 0, prev);
  ac = 0;
  prev_tog = 0;
  line_box = prev;
#define TOG(var, name, index) \
   { ac = 0; \
     XtSetArg (av[ac], XtNorientation, "horizontal"); ac++; \
     XtSetArg (av [ac], XtNfromVert, line_box); ac++; \
     XtSetArg (av[ac], XtNtop, XtChainTop); ac++; \
     XtSetArg (av[ac], XtNbottom, XtChainTop); ac++; \
     line_box = XtCreateManagedWidget ("keysymLine", boxWidgetClass, \
				       keysym_box, av, ac); \
     var = make_label (line_box, name, 0, 0, prev); \
     if (index) \
       var = make_toggle (line_box, "keysymValue" ,var, prev, 0, \
			  keysym_button_cb, box, MEDIUM_LENGTH_KEYSYM_NAME, \
			  prev_tog, (XtPointer) index); \
     else \
       var = make_button (line_box, "autoRepeatValue", "Yes", var, prev, \
			  autorepeat_button_cb, box, 0); \
       prev_tog = prev = var; }
  TOG (box->keysym_buttons [0], "keysym1", 1);
  TOG (box->keysym_buttons [1], "keysym2", 2);
  TOG (box->keysym_buttons [2], "keysym3", 3);
  TOG (box->keysym_buttons [3], "keysym4", 4);
  TOG (box->keysym_buttons [4], "keysym5", 5);
  TOG (box->keysym_buttons [5], "keysym6", 6);
  TOG (box->keysym_buttons [6], "keysym7", 7);
  TOG (box->keysym_buttons [7], "keysym8", 8);
  prev = prev_tog = 0;
  line_box = make_label (keysym_box, "spacer2", "", 0, line_box);
  TOG (box->autorepeat_widget, "autoRepeat", 0);
#undef TOG

  ac = 0;
  mod_box = XtCreateManagedWidget ("modifierBox", formWidgetClass,
				   box2, av, ac);
  prev = make_label (mod_box, "modifiers", 0, 0, 0);
  prev = make_label (mod_box, "spacer", "", 0, prev);
#define TOG(var, name) \
   { var = make_toggle (mod_box, name, 0, prev, 0, 0, 0, 0, 0, 0); \
     prev = var; }
  TOG (box->mod_buttons [0], "modShift");
  TOG (box->mod_buttons [1], "modLock");
  TOG (box->mod_buttons [2], "modControl");
  TOG (box->mod_buttons [3], "mod1");
  TOG (box->mod_buttons [4], "mod2");
  TOG (box->mod_buttons [5], "mod3");
  TOG (box->mod_buttons [6], "mod4");
  TOG (box->mod_buttons [7], "mod5");
#undef TOG

  ac = 0;
  keyset_box = XtCreateManagedWidget("keysetBox", formWidgetClass,
				     box2, av, ac);
  prev = make_label (keyset_box, "allKeySets", 0, 0, 0);
  ac = 0;
  XtSetArg (av[ac], XtNfromVert, prev); ac++;
  XtSetArg (av[ac], XtNleft, XtChainLeft); ac++;
  XtSetArg (av[ac], XtNright, XtChainRight); ac++;
  XtSetArg (av[ac], XtNtop, XtChainTop); ac++;
  XtSetArg (av[ac], XtNbottom, XtChainBottom); ac++;
  set_vp = XtCreateManagedWidget ("keysetsVp", viewportWidgetClass,
				  keyset_box, av, ac);
  ac = 0;
  XtSetArg (av[ac], XtNlist, all_keyset_names); ac++;
  set_list = XtCreateManagedWidget ("keysets", listWidgetClass, set_vp,av,ac);

  XtAddCallback (set_list, XtNcallback, keyset_list_cb, (XtPointer) box);

  ac = 0;
  keyset_syms_box = XtCreateManagedWidget ("keysetSymsBox", formWidgetClass,
					   box2, av, ac);
  prev = make_label (keyset_syms_box, "keySymsOfSet", 0, 0, 0);
  ac = 0;
  XtSetArg (av[ac], XtNfromVert, prev); ac++;
  XtSetArg (av[ac], XtNleft, XtChainLeft); ac++;
  XtSetArg (av[ac], XtNright, XtChainRight); ac++;
  XtSetArg (av[ac], XtNtop, XtChainTop); ac++;
  XtSetArg (av[ac], XtNbottom, XtChainBottom); ac++;
  sym_vp = XtCreateManagedWidget ("keysymsVp", viewportWidgetClass,
				  keyset_syms_box, av, ac);
  ensure_keysym_name_buffer (256);
  bcopy (all_keyset_names, keysym_name_buffer, sizeof (all_keyset_names));
  keysym_name_buffer [0] = LONGEST_KEYSYM_NAME;
  ac = 0;
  XtSetArg (av[ac], XtNlist, keysym_name_buffer); ac++;
  sym_list = XtCreateManagedWidget ("keysyms", listWidgetClass, sym_vp,av,ac);
  XtAddCallback (sym_list, XtNcallback, keysym_list_cb, (XtPointer) box);

  ac = 0;
  XtSetArg (av[ac], XtNskipAdjust, True); ac++;
  button_box = XtCreateManagedWidget ("buttons", boxWidgetClass, box1, av, ac);

  prev = make_button (button_box, "undo",  0,    0, 0, undo_button_cb, box, 0);
  prev = make_button (button_box, "abort", 0, prev, 0, abort_button_cb, box,0);
  prev = make_button (button_box, "ok",    0, prev, 0, ok_button_cb, box, 0);

  box->keyboard = widget;
  box->shell = toplevel;
  box->keyset_list = set_list;
  box->keysym_list = sym_list;
  return box;
}

void
pop_up_key_dbox  (parent, client_data, call_data)
     Widget parent;
     XtPointer client_data, call_data;
{
  KeyboardWidget widget = (KeyboardWidget) client_data;
  KeyWidget key = widget->keyboard.key_under_mouse;
  struct edit_key_box *edit_key_box =
    widget->keyboard.key_menus->edit_key_box;
  {
    Widget topmost = parent;
    int x, y, w, h;
    XtRealizeWidget (edit_key_box->shell);
    w = edit_key_box->shell->core.width;
    h = edit_key_box->shell->core.height;
    while (topmost->core.parent)
      topmost = topmost->core.parent;
    if (topmost->core.width < w) x = topmost->core.x;
    else x = topmost->core.x + ((topmost->core.width - w) / 2);
    if (topmost->core.height < h) y = topmost->core.y;
    else y = topmost->core.y + ((topmost->core.height - h) / 2);
    XtMoveWidget (edit_key_box->shell, x, y);
  }
  XtPopup (edit_key_box->shell, XtGrabNonexclusive);
  undo_button_cb (edit_key_box->shell, edit_key_box, 0);
}



static void
yorn_button_cb (widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
  *((int *) client_data) = 1;
}


int
y_or_n_p (widget, name, name0, name1, name2)
     KeyboardWidget widget;
     char *name, *name0, *name1, *name2;
{
  XtAppContext app = XtWidgetToApplicationContext ((Widget) widget);
  Widget topmost = (Widget) widget;
  XEvent event;
  int x, y, w, h;
  Arg av [10];
  int ac = 0;
  Widget shell, box1, box2, prev;
  int ans [3];

  shell = XtCreatePopupShell (name, transientShellWidgetClass,
			      (Widget) widget, av, ac);
  box1 = XtCreateManagedWidget ("paned", panedWidgetClass, shell, av, ac);
  make_label (box1, "label", 0, 0, 0);
  ac = 0;
  XtSetArg (av[ac], XtNorientation, "horizontal"); ac++;
  box2 = XtCreateManagedWidget ("buttons", boxWidgetClass, box1, av, ac);
  prev = 0;
  if (name0) prev = make_button (box2,name0,0,prev,0,yorn_button_cb,&ans[0],0);
  if (name1) prev = make_button (box2,name1,0,prev,0,yorn_button_cb,&ans[1],0);
  if (name2) prev = make_button (box2,name2,0,prev,0,yorn_button_cb,&ans[2],0);
  XtRealizeWidget (shell);
  w = shell->core.width;
  h = shell->core.height;
  while (topmost->core.parent)
    topmost = topmost->core.parent;
  if (topmost->core.width < w) x = topmost->core.x;
  else x = topmost->core.x + ((topmost->core.width - w) / 2);
  if (topmost->core.height < h) y = topmost->core.y;
  else y = topmost->core.y + ((topmost->core.height - h) / 2);
  XtMoveWidget (shell, x, y);

  XtPopup (shell, XtGrabNonexclusive);
  bzero (ans, sizeof (ans));
  while (! (ans[0] || ans[1] || ans[2]))
    {
      XtAppNextEvent (app, &event);
      if (event.xany.type == KeymapNotify)
	keyboard_handle_keymap_notify ((Widget) widget, 0, &event, 0);
      else if (event.xany.type == MappingNotify)
	keyboard_handle_mapping_notify ((Widget) widget, 0, &event, 0);
      XtDispatchEvent (&event);
    }
  XtPopdown (shell);
  XtDestroyWidget (shell);
  if (ans[0]) return 0;
  if (ans[1]) return 1;
  if (ans[2]) return 2;
  exit (-69);
}



KeyWidget
prompt_for_key (keyboard, msg)
     KeyboardWidget keyboard;
     char *msg;
{
  XtAppContext app = XtWidgetToApplicationContext ((Widget) keyboard);
  KeyWidget key;
  XEvent event;
  message (keyboard, msg);

  if (XGrabPointer (XtDisplay (keyboard), XtWindow (keyboard), True,
		    ButtonPressMask | ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, 0,
		    keyboard->keyboard.select_cursor,
		    CurrentTime))
    {
      XBell (XtDisplay (keyboard), 0);
      message (keyboard, "Grab failed.");
      return 0;
    }
  
  while (1)
    {
      XtAppNextEvent (app, &event);

      if (event.xany.type == ButtonPress)
	{
	  XUngrabPointer (XtDisplay (keyboard), CurrentTime);
	  if (key = keyboard->keyboard.key_under_mouse)
	    return key;
	  XBell (XtDisplay (keyboard), 0);
	  message (keyboard, "You must click on a key.");
	  return 0;
	}
      else if (event.xany.type == ButtonRelease ||
	       event.xany.type == KeyPress ||
	       event.xany.type == KeyRelease)
	{
	  XUngrabPointer (XtDisplay (keyboard), CurrentTime);
	  XBell (XtDisplay (keyboard), 0);
	  XPutBackEvent (XtDisplay (keyboard), &event);
	  message (keyboard, "Aborted.");
	  return 0;
	}
      else
	{
	  if (event.xany.type == KeymapNotify)
	    keyboard_handle_keymap_notify ((Widget) keyboard, 0, &event, 0);
	  else if (event.xany.type == MappingNotify)
	    keyboard_handle_mapping_notify ((Widget) keyboard, 0, &event, 0);
	  XtDispatchEvent (&event);
	}
    }
}
