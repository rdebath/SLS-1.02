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

#include "version.h"

#if __STDC__
#include <stdlib.h>
#endif

#include <stdio.h>
#include <ctype.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xproto.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xaw/Simple.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Box.h>
#include <X11/Xmu/Error.h>

#include "KbdWidgetP.h"
#include "KeyWidgetP.h"

#include "xkeycaps.h"
#include "defaults.h"

char *progname;
char *short_version;

extern struct info_labels *make_info_widget ();
extern Widget make_command_widgets ();
extern struct key_menus *make_key_menus ();
extern void keyboard_handle_mapping_notify ();
extern void keyboard_handle_keymap_notify ();

Window XSendEvent_BadWindow = 0;

xkeycaps_error_handler (dpy, error)
     Display *dpy;
     XErrorEvent *error;
{
  switch (error->request_code)
    {
    case X_GetKeyboardMapping:
      return 0;
    case X_SendEvent:
      if (error->error_code == BadWindow && !XSendEvent_BadWindow)
	XSendEvent_BadWindow = error->resourceid;
      else
	XmuPrintDefaultErrorMessage (dpy, error, stderr);
      return 0;
    default:
      XmuPrintDefaultErrorMessage (dpy, error, stderr);
      exit (-1);
    }
}


static KeyboardWidget
make_keyboard (box, box2, info, name)
     Widget box, box2;
     struct info_labels *info;
     char *name;
{
  Arg av[20];
  int ac = 0;
  KeyboardWidget keyboard;
  XtSetArg (av[ac], XtNfromVert, box2); ac++;
  XtSetArg (av[ac], XtNleft, XtChainLeft); ac++;
  XtSetArg (av[ac], XtNright, XtChainRight); ac++;
  XtSetArg (av[ac], XtNtop, XtChainTop); ac++;
  XtSetArg (av[ac], XtNbottom, XtChainBottom); ac++;
  if (name) XtSetArg (av[ac], "keyboard", name), ac++;
  keyboard = (KeyboardWidget)
    XtCreateManagedWidget ("keyboard", keyboardWidgetClass, box, av, ac);
  keyboard->keyboard.label_widgets = info;
  keyboard->keyboard.key_menus = make_key_menus (keyboard);
  return keyboard;
}


static void
maybe_relabel_window (keyboard)
     KeyboardWidget keyboard;
{
  /* If the user hasn't specified the -title option, set the window title
     to be something more informative.
   */
  Widget toplevel = (Widget) keyboard;
  char buf1 [100], buf2 [100];
  XrmValue value;
  char *type;
  XrmDatabase db = XtDatabase (XtDisplay (keyboard));
  char *name, *class;
  while (XtParent (toplevel)) toplevel = XtParent (toplevel);
  XtGetApplicationNameAndClass (XtDisplay (keyboard), &name, &class);
  sprintf (buf1, "%s.title", name);
  sprintf (buf2, "%s.Title", class);
  if (XrmGetResource (db, buf1, buf2, &type, &value)) return;
  sprintf (buf1, "%s; %s keyboard", class, keyboard->keyboard.kbd->long_name);
  XStoreName (XtDisplay (toplevel), XtWindow (toplevel), buf1);
}



static void
create_icon_pixmap (shell, keyboard)
     Widget shell;
     KeyboardWidget keyboard;
{
  Pixmap pixmap = 0, mask = 0;
  Arg av [5];
  int ac = 0;
  XtSetArg (av [ac], XtNiconPixmap, &pixmap); ac++;
  XtSetArg (av [ac], XtNiconMask, &mask); ac++;
  XtGetValues (shell, av, ac);
  if (pixmap) XFreePixmap (XtDisplay (shell), pixmap);
  if (mask)   XFreePixmap (XtDisplay (shell), mask);

  {
    int i, j, k;
    struct keyboard *kbd = keyboard->keyboard.kbd;
    int scale = 3;
    int icon_width, icon_height;
    int N_width = 0;
    int N_height = 0;
    GC draw_gc, erase_gc, mask_draw_gc, mask_erase_gc;
    unsigned long gc_mask;
    XGCValues gc_values;

    for (i = 0; i < kbd->nrows; i++)
      for (j = 0; j < kbd->rows[i].nkeys; j++)
	for (k = 0; k < 8; k++)
	  if (kbd->rows[i].keys[j].default_keysyms[k] == 'N' ||
	      kbd->rows[i].keys[j].default_keysyms[k] =='n')
	    {
	      N_width = kbd->rows[i].keys[j].width;
	      N_height = kbd->rows[i].keys[j].width;
	      goto DONE;
	    }
  DONE:
    if (! N_width) exit (-1);

    icon_width = ((keyboard->keyboard.max_width / N_width) + 2) * scale;
    icon_height = ((keyboard->keyboard.max_height / N_height) + 2) * scale;

    pixmap = XCreatePixmap (XtDisplay (shell), XtWindow (shell),
			    icon_width, icon_height, 1);
    mask = XCreatePixmap (XtDisplay (shell), XtWindow (shell),
			  icon_width, icon_height, 1);

    gc_mask = GCFunction | GCForeground;
    gc_values.function = GXcopy;
    gc_values.foreground = BlackPixelOfScreen (XtScreen (shell));
    erase_gc = XCreateGC (XtDisplay (shell), pixmap, gc_mask, &gc_values);
    gc_values.foreground = WhitePixelOfScreen (XtScreen (shell));
    draw_gc = XCreateGC (XtDisplay (shell), pixmap, gc_mask, &gc_values);
    gc_values.foreground = 1;
    mask_draw_gc = XCreateGC (XtDisplay (shell), mask, gc_mask, &gc_values);
    gc_values.foreground = 0;
    mask_erase_gc = XCreateGC (XtDisplay (shell), mask, gc_mask, &gc_values);
    
    XFillRectangle (XtDisplay (shell), pixmap, erase_gc, 0, 0,
		    icon_width, icon_height);
    XFillRectangle (XtDisplay (shell), pixmap, draw_gc, 1, 1,
		    icon_width-2, icon_height-2);
    XFillRectangle (XtDisplay (shell), mask, mask_erase_gc, 0, 0,
		    icon_width, icon_height);

    for (i = 0; i < kbd->nrows; i++)
      for (j = 0; j < kbd->rows[i].nkeys; j++)
	{
	  KeyWidget key = (KeyWidget) kbd->rows[i].keys[j].widget;
	  int x, y, x2, y2, w, h;
	  if (! key) continue;

	  x = (key->key.x * scale / N_width) + scale;
	  y = (key->key.y * scale / N_height) + scale;
	  x2 = ((key->key.x + kbd->rows[i].keys[j].width) * scale / N_width)
	    + scale;
	  y2 = ((key->key.y + kbd->rows[i].keys[j].height) * scale / N_height)
	    + scale;
	  w = x2-x-1; if (w<0) w=1;
	  h = y2-y-1; if (h<0) h=1;
	  XFillRectangle (XtDisplay (shell), pixmap, erase_gc, x-1, y-1,
			  w+2, h+2);
	  XFillRectangle (XtDisplay (shell), pixmap, draw_gc, x, y, w, h);
	  XFillRectangle (XtDisplay (shell), mask, mask_draw_gc, x-1, y-1,
			  w+2, h+2);
	}
    XFreeGC (XtDisplay (shell), draw_gc);
    XFreeGC (XtDisplay (shell), erase_gc);
    XFreeGC (XtDisplay (shell), mask_draw_gc);
    XFreeGC (XtDisplay (shell), mask_erase_gc);
    ac = 0;
    XtSetArg (av [ac], XtNiconPixmap, pixmap); ac++;
    XtSetArg (av [ac], XtNiconMask, mask); ac++;
    XtSetValues (shell, av, ac);
  }
}

void
replace_keyboard (keyboard, name)
     KeyboardWidget *keyboard;
     char *name;
{
  /* Doing this seems to leak about 8k each time, but I really don't care. */
  Widget box, box2;
  KeyboardWidget new_kbd;
  Widget toplevel;
  Arg av[20];
  int ac = 0;
  toplevel = box = XtParent (*keyboard);
  while (XtParent (toplevel)) toplevel = XtParent (toplevel);
  XtSetArg (av[ac], XtNfromVert, box2); ac++;
  XtGetValues ((Widget) *keyboard, av, ac);
  new_kbd = make_keyboard (box, box2, (*keyboard)->keyboard.label_widgets,
			   name);
  XtUnmanageChild ((Widget) *keyboard);
  XtDestroyWidget ((Widget) *keyboard);
  *keyboard = new_kbd;
  XtSetKeyboardFocus (toplevel, (Widget) *keyboard);
  maybe_relabel_window (*keyboard);
  create_icon_pixmap (toplevel, *keyboard);
}


void
xkeycaps_DispatchEvent_hook (keyboard, event)
     KeyboardWidget keyboard;
     XEvent *event;
{
  /* MappingNotify and KeymapNotify events don't have an associated
     window, so there's no way to register an event-handler function
     for one of these with Xt.  Lose, lose.
   */
  if (event->xany.type == KeymapNotify)
    keyboard_handle_keymap_notify ((Widget) keyboard, 0, event, 0);
  else if (event->xany.type == MappingNotify)
    keyboard_handle_mapping_notify ((Widget) keyboard, 0, event, 0);
}


static void
xkeycaps_main_loop (keyboard, app)
     KeyboardWidget keyboard;
     XtAppContext app;
{
  XEvent event;
  while (1)
    {
      XtAppNextEvent (app, &event);
      xkeycaps_DispatchEvent_hook (keyboard, &event);
      XtDispatchEvent (&event);
    }
}


main (argc, argv)
     int argc;
     char **argv;
{
  char *class = "XKeyCaps";
  XtAppContext app;
  Widget toplevel, box, box2, buttons;
  struct info_labels *info;
  KeyboardWidget keyboard = 0;
  struct keyboard *kbd;
  Arg av [20];
  int ac = 0;
  int i;
  char *tmp;

  toplevel = XtAppInitialize (&app, class, options, XtNumber (options),
			      &argc, argv, xkeycapsDefaults, NULL, 0);

  XtGetApplicationNameAndClass (XtDisplay (toplevel), &progname, &class);

  if (argc > 1)
    {
      fprintf (stderr, "%s: unknown option %s\n", progname, argv [1]);
      exit (-1);
    }
  ac = 0;
  box = XtCreateManagedWidget ("vertical", panedWidgetClass, toplevel, av, ac);
  ac = 0;
  XtSetArg (av[ac], XtNorientation, "horizontal"); ac++;
  box2 = XtCreateManagedWidget ("horizontal", panedWidgetClass, box, av, ac);
  buttons = make_command_widgets (box2, &keyboard);
  info = make_info_widget (box2, buttons);

  keyboard = make_keyboard (box, box2, info, 0);

  XtAddEventHandler ((Widget) keyboard, KeymapStateMask, False,
		     keyboard_handle_keymap_notify, 0);
  XtAddEventHandler ((Widget) keyboard, 0, True,
		     keyboard_handle_mapping_notify, 0);

  XtRealizeWidget (toplevel);
  create_icon_pixmap (toplevel, keyboard);

  XtSetKeyboardFocus (toplevel, (Widget) keyboard);
  maybe_relabel_window (keyboard);
  XSetErrorHandler (xkeycaps_error_handler);
  {
    char buf [255];
    int index, i2;
    for (index = 0;; index++)
      if (!strncmp ("Copyright (c)", version+index, 13)) break;
    memcpy (buf, version, i2 = index);
    buf [index] = '\251';
    strcpy (buf + index + 1, version + index + 13);
    for (;; index++)
      if (isdigit(version[index]) && isdigit(version[index+1]) &&
	  isdigit(version[index+2]) && isdigit(version[index+3]) &&
	  version[index+4] != ',')
	break;
    short_version = (char *) malloc (i2 + 11 + strlen (version + index));
    memcpy (short_version, version, i2 + 10);
    strcpy (short_version + i2 + 10, version + index);
    message (keyboard, "");
    message2 (keyboard, buf);
  }
#ifdef HAVE_XTRAP
  if (keyboard->keyboard.trap_data)
    xkeycaps_xtrap_main_loop (keyboard, app);
  else
#endif
    xkeycaps_main_loop (keyboard, app);
}
