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
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>

#include <X11/Xaw/Simple.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Box.h>

#include "xkeycaps.h"

#include "KbdWidgetP.h"
#include "KeyWidgetP.h"

struct info_labels {
  Widget info;
#ifdef MULTIPLE_WIDGETS
  Widget keycode [5];
  Widget keysym [9];
  Widget ascii [5];
  Widget modifiers [2];
  Widget autorepeat [2];
#else
  Widget keycode;
  Widget keysym;
  Widget ascii;
  Widget modifiers;
  Widget autorepeat;
#endif
  Widget message;
  Widget message2;
};

extern void key_to_event ();


Widget
make_label_1 (parent, name, string, left, top, class, callback, data)
     Widget parent;
     char *name, *string;
     Widget left, top;
     WidgetClass class;
     void (*callback) ();
     XtPointer data;
{
  Arg av [20];
  int ac = 0;
  Widget w;
  XtSetArg (av[ac], XtNleft, XtChainLeft); ac++;
  XtSetArg (av[ac], XtNright, XtChainLeft); ac++;
  XtSetArg (av[ac], XtNtop, XtChainTop); ac++;
  XtSetArg (av[ac], XtNbottom, XtChainTop); ac++;
  XtSetArg (av[ac], XtNjustify, XtJustifyLeft); ac++;
  XtSetArg (av[ac], XtNresize, True); ac++;
  if (string) XtSetArg (av[ac], XtNlabel, string), ac++;
  if (left) XtSetArg (av[ac], XtNfromHoriz, left), ac++;
  if (top)  XtSetArg (av[ac], XtNfromVert,  top),  ac++;
  w = XtCreateManagedWidget (name, class, parent, av, ac);
  if (callback) XtAddCallback (w, XtNcallback, callback, data);
  return w;
}

Widget
make_label (parent, name, string, left, top)
     Widget parent;
     char *name, *string;
     Widget left, top;
{
  return make_label_1 (parent, name, string, left, top, labelWidgetClass, 0,0);
}

#ifdef MULTIPLE_WIDGETS

struct info_labels *
make_info_widget (parent, parent_left)
     Widget parent, parent_left;
{
  Widget info, left, farleft;
  Widget label_col, line_widget;
  struct info_labels *labels =
    (struct info_labels *) malloc (sizeof (struct info_labels));
  Arg av [10];
  int ac = 0;

  XtSetArg (av[ac], XtNleft, XtChainLeft); ac++;
  XtSetArg (av[ac], XtNright, XtChainLeft); ac++;
  XtSetArg (av[ac], XtNtop, XtChainTop); ac++;
  XtSetArg (av[ac], XtNbottom, XtChainTop); ac++;
  if (parent_left) XtSetArg (av[ac], XtNfromHoriz, parent_left), ac++;
  info = XtCreateManagedWidget ("info", formWidgetClass, parent, av, ac);

  ac = 0;
  XtSetArg (av[ac], XtNleft, XtChainLeft); ac++;
  XtSetArg (av[ac], XtNright, XtChainLeft); ac++;
  XtSetArg (av[ac], XtNtop, XtChainTop); ac++;
  XtSetArg (av[ac], XtNbottom, XtChainTop); ac++;
  label_col = XtCreateManagedWidget ("labels", formWidgetClass, info, av, ac);

  farleft = 0;

#define NEWLABEL(store, name,val) \
  { ac = 0; \
    XtSetArg (av[ac], XtNfromHoriz, label_col), ac++; \
    XtSetArg (av[ac], XtNorientation, "horizontal"), ac++; \
    XtSetArg (av[ac], XtNleft, XtChainLeft); ac++; \
    XtSetArg (av[ac], XtNright, XtChainRight); ac++; \
    XtSetArg (av[ac], XtNtop, XtChainTop); ac++; \
    XtSetArg (av[ac], XtNbottom, XtChainTop); ac++; \
    if (farleft) XtSetArg (av[ac], XtNfromVert, farleft), ac++; \
    line_widget = XtCreateManagedWidget("line",boxWidgetClass, info, av, ac); \
    farleft = make_label (label_col, (name),(val),0, farleft); \
    left = 0; \
    store = farleft; \
  }

#define NEWVALUE(store, name) \
  { left = make_label (line_widget, (name),"",0, 0); \
    store = left; \
  }

  NEWLABEL (labels->keycode [0], "label", "KeyCode:");
  NEWVALUE (labels->keycode [1], "keycode");
  NEWVALUE (labels->keycode [2], "keycode16");
  NEWVALUE (labels->keycode [3], "keycode10");
  NEWVALUE (labels->keycode [4], "keycode8");

  NEWLABEL (labels->keysym [0], "label", "KeySym:");
  NEWVALUE (labels->keysym [1], "keysym");
  NEWVALUE (labels->keysym [2], "keysym");
  NEWVALUE (labels->keysym [3], "keysym");
  NEWVALUE (labels->keysym [4], "keysym");
  NEWVALUE (labels->keysym [5], "keysym");
  NEWVALUE (labels->keysym [6], "keysym");
  NEWVALUE (labels->keysym [7], "keysym");
  NEWVALUE (labels->keysym [8], "keysym");

  NEWLABEL (labels->ascii [0], "label","ASCII:");
  NEWVALUE (labels->ascii [1], "ascii");
  NEWVALUE (labels->ascii [2], "ascii16");
  NEWVALUE (labels->ascii [3], "ascii10");
  NEWVALUE (labels->ascii [4], "ascii8");

  NEWLABEL (labels->modifiers [0], "label", "Modifiers:");
  NEWVALUE (labels->modifiers [1], "modifiers");

  NEWLABEL (labels->autorepeat [0], "label", "AutoRepeat:");
  NEWVALUE (labels->autorepeat [1], "autoRepeat");
  
  labels->message = make_label (info, "message", "", info,
				labels->autorepeat [0]);
  labels->message2 = make_label (info, "message2", "", info,
				 labels->message);
#undef NEWLABEL
#undef NEWVALUE

  labels->info = info;
  return labels;
}

#else

struct info_labels *
make_info_widget (parent, parent_left)
     Widget parent, parent_left;
{
  Widget info, left, farleft;
  Widget label_col, line_widget;
  struct info_labels *labels =
    (struct info_labels *) malloc (sizeof (struct info_labels));
  Arg av [10];
  int ac = 0;

  XtSetArg (av[ac], XtNleft, XtChainLeft); ac++;
  XtSetArg (av[ac], XtNright, XtChainLeft); ac++;
  XtSetArg (av[ac], XtNtop, XtChainTop); ac++;
  XtSetArg (av[ac], XtNbottom, XtChainTop); ac++;
  if (parent_left) XtSetArg (av[ac], XtNfromHoriz, parent_left), ac++;
  info = XtCreateManagedWidget ("info", formWidgetClass, parent, av, ac);

  ac = 0;
  XtSetArg (av[ac], XtNleft, XtChainLeft); ac++;
  XtSetArg (av[ac], XtNright, XtChainLeft); ac++;
  XtSetArg (av[ac], XtNtop, XtChainTop); ac++;
  XtSetArg (av[ac], XtNbottom, XtChainTop); ac++;
  label_col = XtCreateManagedWidget ("labels", formWidgetClass, info, av, ac);

  farleft = 0;

#define NEWLABEL(name,val) \
  { ac = 0; \
    XtSetArg (av[ac], XtNfromHoriz, label_col), ac++; \
    XtSetArg (av[ac], XtNorientation, "horizontal"), ac++; \
    XtSetArg (av[ac], XtNleft, XtChainLeft); ac++; \
    XtSetArg (av[ac], XtNright, XtChainRight); ac++; \
    XtSetArg (av[ac], XtNtop, XtChainTop); ac++; \
    XtSetArg (av[ac], XtNbottom, XtChainTop); ac++; \
    if (farleft) XtSetArg (av[ac], XtNfromVert, farleft), ac++; \
    line_widget = XtCreateManagedWidget("line",boxWidgetClass, info, av, ac); \
    farleft = make_label (label_col, (name),(val),0, farleft); \
    left = 0; \
  }

#define NEWVALUE(store, name) \
  { left = make_label (line_widget, (name),"",0, 0); \
    store = left; \
  }

  NEWLABEL ("keycode", "KeyCode:");
  NEWVALUE (labels->keycode, "keycode");
  NEWLABEL ("keysym", "KeySym:");
  NEWVALUE (labels->keysym, "keysym");
  NEWLABEL ("ascii","ASCII:");
  NEWVALUE (labels->ascii, "ascii");
  NEWLABEL ("modifiers", "Modifiers:");
  NEWVALUE (labels->modifiers, "modifiers");
  NEWLABEL ("autoRepeat", "AutoRepeat:");
  NEWVALUE (labels->autorepeat, "autoRepeat");
  NEWLABEL ("message", "");
  NEWVALUE (labels->message, "message");
  NEWLABEL ("message2", "");
  NEWVALUE (labels->message2, "message2");
#undef NEWLABEL
#undef NEWVALUE

  labels->info = info;
  return labels;
}

#endif


void message2 ();

void
message (widget, str)
     KeyboardWidget widget;
     char *str;
{
  Arg av[10];
  int ac = 0;
  XtSetArg (av [ac], XtNlabel, str); ac++;
  XtSetValues (widget->keyboard.label_widgets->message, av, ac);
  message2 (widget, "");
}

void
message2 (widget, str)
     KeyboardWidget widget;
     char *str;
{
  Arg av[10];
  int ac = 0;
  XtSetArg (av [ac], XtNlabel, str); ac++;
  XtSetValues (widget->keyboard.label_widgets->message2, av, ac);
}


int 
string_width (string, font)
     char *string;
     XFontStruct *font;
{
  int size = 0;
  for (; *string; string++)
    if (font->per_char)
      size += font->per_char [(*string) - font->min_char_or_byte2].width;
    else
      size += font->max_bounds.width;
  return size;
}


extern void key_to_event ();

static void
key_to_ascii (widget, string, pretty, keysym, font)
     KeyWidget widget;
     char *string, *pretty;
     KeySym *keysym;
     XFontStruct *font;
{
  XEvent event;
  char *p = pretty;
  int i, size;
  key_to_event (widget, &event, 1);

  /* We can't really pass in an XComposeStatus structure here, because we call
     XLookupString() as a result of mouse-motion as well as keypresses, and if
     the user moved the mouse over Multi_key, things would get all confused.
     Actually, I've been told that the Xlib that comes with OpenWindows 3 does
     compose processing even if you pass 0 as the XComposeStatus argument to
     XLookupString(), so no doubt things will be confused on that system
     anyway.  (Yeah, big surprise...)
   */
  size = XLookupString ((XKeyEvent *) &event, string, 50, keysym, 0);
  string [size] = 0;
  for (i = 0; i < size; i++)
    {
      unsigned char c = (unsigned char) string [i];
      unsigned char hic = 0;

      if (c >= 0200)
	{
	  *p++ = 'M';
	  *p++ = '-';
	  hic = c;
	  c -= 0200;
	}

      if (c < 040)
	{
	  *p++ = '^';
	  *p++ = c + ('A'-1);
	  if (! hic) hic = c;
	}
      else if (c == 0177)
	{
	  *p++ = '^';
	  *p++ = '?';
	  if (! hic) hic = c;
	}
      else
	*p++ = c;

      if (hic && font && size == 1 &&
	  hic != '\n' && hic != ' ' &&
	  hic >= font->min_char_or_byte2 &&
	  hic <= font->max_char_or_byte2 &&
	  (!font->per_char ||
	   font->per_char [hic - font->min_char_or_byte2].width))
	{
	  *p++ = ' ';
	  *p++ = '(';
	  *p++ = hic;
	  *p++ = ')';
	}
    }
  *p = 0;
}


void
describe_key (widget)
     KeyWidget widget;
{
  KeyboardWidget keyboard = (KeyboardWidget) widget->core.parent;
  struct info_labels *labels = keyboard->keyboard.label_widgets;
  Arg av [10];
  int i, ac = 0;
  KeySym keysym, *keysyms;
  int syms_per_code;
  XFontStruct *ascii_font = 0;

  char buf [255];
  char buf2 [255];
  char *b;
  int ascii = 0;

  XtSetArg (av[0], XtNfont, &ascii_font);
  XtGetValues (labels->ascii, av, 1);
  if (! ascii_font) exit (-69);

#define SETLABEL(label,string) \
  XtSetArg (av[0], XtNlabel, string); \
  XtSetValues (label, av, 1);

  keysyms = 0;
  if (widget->key.key->keycode)
    keysyms = XGetKeyboardMapping (XtDisplay (widget),
				   widget->key.key->keycode,
				   1, &syms_per_code);
  if (! keysyms) syms_per_code = 0;
  for (; syms_per_code && (!keysyms[syms_per_code-1]); syms_per_code--)
    ;

  key_to_ascii (widget, buf, buf2, &keysym, ascii_font);
  ascii = (unsigned char) buf[0];

  sprintf (buf, "%s  0x%02X  %u  0%02o",
	   widget->key.key_name,
	   widget->key.key->keycode,
	   widget->key.key->keycode,
	   widget->key.key->keycode);
  SETLABEL (labels->keycode, buf);

  b = buf;
  *b = 0;
  for (i = 0; i < syms_per_code; i++)
    {
      char yabuf [255];
      char *b2;
      int j;
      if (! keysyms [i])
	b2 = "NoSymbol";
      else
	{
	  b2 = XKeysymToString (keysyms [i]);

#ifdef sun	/* What is *up* with that damn OpenWindows libX, anyway?? */
	  if (b2) ;
	  else if (keysyms [i] == 0x1005FF10) b2 = "SunXK_F36";
	  else if (keysyms [i] == 0x1005FF11) b2 = "SunXK_F37";
	  else if (keysyms [i] == 0x1005FF75) b2 = "SunXK_Cut";
	  else if (keysyms [i] == 0x1005FF70) b2 = "SunXK_Props";
	  else if (keysyms [i] == 0x1005FF71) b2 = "SunXK_Front";
	  else if (keysyms [i] == 0x1005FF72) b2 = "SunXK_Copy";
	  else if (keysyms [i] == 0x1005FF73) b2 = "SunXK_Open";
	  else if (keysyms [i] == 0x1005FF74) b2 = "SunXK_Paste";
	  else
#endif
	  if (! b2)
	    {
	      sprintf (yabuf, "unknown_keysym_0x%02X", keysyms [i]);
	      b2 = yabuf;
	    }
	}
      j = strlen (b2);
      if (b != buf) {
	b [0] = ' '; b++;
	b [0] = ' '; b++;
      }
      strncpy (b, b2, j+1);
      b += j;
    }
  SETLABEL (labels->keysym, buf);

  if (ascii)
    sprintf (buf, "%s  0x%02X  %u  0%02o", buf2, ascii, ascii, ascii);
  else
    *buf = 0;
  SETLABEL (labels->ascii, buf);

sprintf (buf, "%s%s%s%s%s%s%s%s",
	 ((widget->key.modifier_bits & ShiftMask)   ? "Shift "   : ""),
	 ((widget->key.modifier_bits & LockMask)    ? "Lock "    : ""),
	 ((widget->key.modifier_bits & ControlMask) ? "Control " : ""),
	 ((widget->key.modifier_bits & Mod1Mask)    ? "Mod1 "    : ""),
	 ((widget->key.modifier_bits & Mod2Mask)    ? "Mod2 "    : ""),
	 ((widget->key.modifier_bits & Mod3Mask)    ? "Mod3 "    : ""),
	 ((widget->key.modifier_bits & Mod4Mask)    ? "Mod4 "    : ""),
	 ((widget->key.modifier_bits & Mod5Mask)    ? "Mod5 "    : ""));
  SETLABEL (labels->modifiers, buf);
  SETLABEL (labels->autorepeat, (widget->key.auto_repeat_p ? "yes" : "no"));

#undef SETLABEL

  if (keysyms) XFree ((char *) keysyms);

}
