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

#ifndef _XKEYCAPS_H_
#define _XKEYCAPS_H_

#include "KeyWidget.h"
#include "KbdWidget.h"

struct key {
  KeyCode keycode;		/* The raw scancode this key generates */

  char *top_keysym;		/* The strings actually printed on the key */
  char *bottom_keysym;		/*  If one of these is 0, there's only one */

  unsigned short width;		/* Width of the key in some arbitrary units */
  unsigned short height;	/* Height of the key in some arbitrary units */

  /* The default state of this key: which bucky-bits it sets, and which
     keysyms it generates by default.  We use this to implement a command
     to reset the keyboard to the default state, since the X server itself
     doesn't retain that information after the keymap has been altered.
   */
  unsigned long default_mods;
  KeySym default_keysyms[8];
  
  KeyWidget widget;		/* Backpointer; filled in at run-time */

};

struct row {
  unsigned int nkeys;
  unsigned int height;
  struct key *keys;
};

struct keyboard {
  char *short_name;
  char *long_name;
  unsigned int nrows;
  struct row *rows;
  int default_scale;
  int horiz_border, vert_border;
};


extern char *progname;

extern struct keyboard *all_kbds [];

struct keyboard *choose_kbd ();

/* screw it */
#define bcopy(from,to,size) memcpy((to),(from),(size))
#define bzero(addr,size) memset((addr),0,(size))
#define bcmp memcmp

#if __STDC__
extern void message (KeyboardWidget, char *);
extern void message2 (KeyboardWidget, char *);
extern int string_equal (char *, char *);
extern int string_width (char *, XFontStruct *);
extern void describe_key (KeyWidget);
extern void replace_keyboard (KeyboardWidget *, char *);
extern void keyboard_handle_keymap_notify (Widget, XtPointer,
					   XEvent *, Boolean *);
extern void keyboard_handle_mapping_notify (Widget, XtPointer,
					    XEvent *, Boolean *);
#endif

#endif /* _XKEYCAPS_H_ */
