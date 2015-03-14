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
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include "KbdWidgetP.h"
#include "KeyWidgetP.h"

#ifdef unix
#include <netdb.h>
static int display_is_on_console_p ();
#endif

#ifdef sun
char *xkeycaps_sunOS_guess_local_keyboard_type ();
#endif

static char *guess_tek_keyboard_type ();
static char *tek_kbd_type, *tek_kbd_layout;

static void
print_kbd_choices ()
{
  int i = 0;
  while (all_kbds [i])
    {
      fprintf (stderr, "    %s%s\t- %s\n",
	       all_kbds [i]->short_name,
	       (strlen (all_kbds [i]->short_name) < 4 ? "\t" : ""),
	       all_kbds [i]->long_name);
      i++;
    }
}


#define strprefix(str1,str2)				\
      ((strlen ((str1)) >= strlen ((str2))) &&		\
       !(strncmp ((str1), (str2), strlen ((str2)))))


struct keyboard *
choose_kbd (dpy, kbd_name)
     Display *dpy;
     char *kbd_name;
{
  char *vendor = XServerVendor (dpy);
  tek_kbd_type = 0;
  tek_kbd_layout = 0;

#ifdef sun
  if ((!kbd_name || !*kbd_name) &&
      display_is_on_console_p (dpy) &&
      (kbd_name = xkeycaps_sunOS_guess_local_keyboard_type ()))
    {
      /* evil hack - append "OW" to the default keyboard name if running
	 OpenWindows, since the default keymaps are different...
       */
      if (!strcmp (vendor, "X11/NeWS - Sun Microsystems Inc."))
	{
	  int L = strlen (kbd_name);
	  char *s = (char *) malloc (L + 4);
	  strncpy (s, kbd_name, L);
	  strcat (s + L, (L <= 6 ? "OW" : "-OW"));
	  kbd_name = s;
	}
      fprintf (stdout, "%s: keyboard hardware claims to be \"%s\".\n",
	       progname, kbd_name);
    }
  else
#endif /* sun */

  if (!kbd_name || !*kbd_name)
    {
      if (!strcmp (vendor,
	  "DECWINDOWS (Compatibility String) Network Computing Devices Inc."))
	kbd_name = "N97";
      else if (!strcmp (vendor, "X11/NeWS - Sun Microsystems Inc."))
	kbd_name = "Sun4OW";
      else if (!strcmp (vendor, "Texas Instruments Explorer"))
	kbd_name = "Explorer";
      else if (!strcmp (vendor, "Silicon Graphics"))
	kbd_name = "Silicon Graphics";
      else if (!strcmp (vendor, "International Business Machines"))
        kbd_name = "RS6k";
      else if (strprefix (vendor, "AGE Logic, Inc.")) /* Xstation 130 */
        kbd_name = "RS6k";
      else if (!strcmp (vendor, "Labtam Australia Pty Ltd"))
        kbd_name = "Labtam";
      else if (strprefix (vendor, "Digital Equipment Corporation"))
	kbd_name = "LK201";
      else if (strprefix (vendor, "DECWINDOWS DigitalEquipmentCorporation"))
	kbd_name = "LK401";
      else if (strprefix (vendor, "Hewlett-Packard Company"))
	kbd_name = "HPHIL";
      else if (strprefix (vendor, "The Santa Cruz Operation"))
	kbd_name = "SCO110";
      else if (strprefix (vendor, "Tektronix, Inc"))
	kbd_name = guess_tek_keyboard_type (dpy);
      else
	kbd_name = 0;

      if (kbd_name)
	{
	  fprintf (stderr,
 "%s: a keyboard type was not specified.  Based on the vendor\n\
 identification string of the display \"%s\", which is\n \"%s\"\n",
		   progname, DisplayString (dpy), vendor);
	  if (tek_kbd_type && tek_kbd_layout)
	    fprintf (stderr, " and the root window's _TEK_KEYBOARD_TYPE\
 and _TEK_KEYBOARD_LAYOUT\n properties, which are \"%s\" and \"%s\",\n",
		     tek_kbd_type, tek_kbd_layout);
	  else if (tek_kbd_type)
	    fprintf (stderr, " and the root window's _TEK_KEYBOARD_TYPE\
 property, which is \"%s\",\n",
		     tek_kbd_type);
	  fprintf (stderr,
" we will assume that you are using a keyboard of type \"%s\".\n\
 If this is incorrect, please supply the -keyboard option with\n\
 one of the following names:\n\n",
		   kbd_name);
	  print_kbd_choices ();
	}
#ifdef DEFAULT_KBD_NAME
      else
	{
	  kbd_name = DEFAULT_KBD_NAME;
	  if (kbd_name)
	    {
	      fprintf (stderr,
 "%s: a keyboard type was not specified, and the vendor ID string,\n\
 \"%s\"\n\
 is not recognised.  We will guess that you are using a keyboard of\n\
 type \"%s\".  If this is incorrect, please supply the -keyboard\n\
 option with one of the following names:\n\n",
		       progname, vendor, kbd_name);
	      print_kbd_choices ();
	    }
	}
#endif
    }

  {
    int i = 0;
    struct keyboard *kbd;

    while (kbd = all_kbds [i++])
      if (string_equal (kbd_name, kbd->short_name) ||
	  string_equal (kbd_name, kbd->long_name))
	return kbd;

    if (kbd_name)
      fprintf (stderr, "%s: unknown keyboard type \"%s\".\n\
Please specify the -keyboard option with one of the following names:\n\n",
	       progname, kbd_name);
    else
      fprintf (stderr,
	   "%s: please specify -keyboard with one of the following names:\n\n",
	       progname);
    
    print_kbd_choices ();
    exit (-1);
  }
}

#ifdef unix

static int
display_is_on_console_p (display)
     Display *display;
{
  int not_on_console = 1;
  char *dpy = XDisplayString(display);
  char *tail = (char *) strchr (dpy, ':');
  if (! tail || strncmp (tail, ":0", 2))
    not_on_console = 1;
  else
    {
      char dpyname[255], localname[255];
      strncpy (dpyname, dpy, tail-dpy);
      dpyname [tail-dpy] = 0;
      if (!*dpyname ||
	  !strcmp(dpyname, "unix") ||
	  !strcmp(dpyname, "localhost"))
	not_on_console = 0;
      else if (gethostname (localname, sizeof (localname)))
	not_on_console = 1;  /* can't find hostname? */
      else
	{
	  /* gethostbyname() reuses the structure it returns,
	     so we have to copy the string out of it. */
	  struct hostent *h = gethostbyname (dpyname);
	  not_on_console = (!h || !!(strcmp (localname, h->h_name)));
	}
    }
  return !not_on_console;
}

#endif

static char *
get_prop_string (dpy, prop)
    Display *dpy;
     char *prop;
{
  Atom prop_atom;
  unsigned char *value;
  Boolean status;
  Atom actual_type;
  int actual_format;
  unsigned long nitems, bytes_after;

  prop_atom = XInternAtom (dpy, prop, True);
  if (prop_atom == None)
    return 0;
  status = XGetWindowProperty (dpy, DefaultRootWindow (dpy), prop_atom,
			       0L, 6L, False, XA_STRING, &actual_type,
			       &actual_format, &nitems, &bytes_after,
			       &value);
  if (status != Success || value == 0 || actual_type != XA_STRING ||
      actual_format != 8 || bytes_after != 0)
    return 0;
  else
    return (char *) value;
}

static char *
guess_tek_keyboard_type (dpy)
    Display *dpy;
{
  /* Tek stores the keyboard type on a property on the root window.
     The _TEK_KEYBOARD_TYPE property is ibm101, ibm102, or vt200.
     If _TEK_KEYBOARD_TYPE is vt200, then _TEK_KEYBOARD_LAYOUT is
     ultrix, vms, x_esc, or x_f11. Also, _TEK_KEYBOARD_NATIONALITY
     is usascii, uk, french, swedish, danish, norwegian, german,
     italian, spanish, swiss-german, katakana, or finnish.
   */
  char buf [255];
  tek_kbd_type = get_prop_string (dpy, "_TEK_KEYBOARD_TYPE");

  if (!tek_kbd_type || !strcmp (tek_kbd_type, "ibm101"))
    {
      /* the default keymap changed between release 4 and release 5. */
      if (VendorRelease(dpy) <= 4) return "TEK101-4";
      return "TEK101";
    }
  else if (!strcmp (tek_kbd_type, "ibm102"))
    return "TEK102";
  else if (!strcmp (tek_kbd_type, "vt200"))
    {
      tek_kbd_layout = get_prop_string (dpy, "_TEK_KEYBOARD_LAYOUT");
      if (!tek_kbd_layout)
	return "TEKvt200";
      else if (!strcmp (tek_kbd_layout, "ultrix"))
	return "TEKvt200u";
      else
	{
	  char *s;
	  sprintf (buf, "TEKvt200%s", tek_kbd_layout);
	  s = (char *) malloc (strlen (buf) + 1);
	  strcpy (s, buf);
	  return s;
	}
    }
  else
    {
      char *s;
      /* some otherwise unknown type of keyboard... */
      sprintf (buf, "TEK_%s", tek_kbd_type);
      s = (char *) malloc (strlen (buf) + 1);
      strcpy (s, buf);
      return s;
    }
}
