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

#ifndef _KeyWidgetP_H_
#define _KeyWidgetP_H_

#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/cursorfont.h>
#include "KeyWidget.h"
#include "xkeycaps.h"

typedef struct {
  void (* highlight_key) ();
  void (* dehighlight_key) ();
} KeyClassPart;

/* Full class record declaration */
typedef struct _KeyClassRec {
    CoreClassPart	core_class;
    KeyClassPart	key_class;
} KeyClassRec;

extern KeyClassRec keyClassRec;

/* New fields for the Key widget record */
typedef struct {
  struct key *key;
  int gutter_width;
  Pixel highlight_pixel, background_pixel;
  Pixel keycap_pixel, keycode_pixel;
  XFontStruct *keycap_font, *keycode_font, *cursor_font;
  GC keycap_gc, keycode_gc, cursor_gc;
  String key_name;
  int highlighted_p;		/* Whether it's drawn highlighted now */
  unsigned short x, y;		/* Position in keyboard units */
  int key_highlighted;		/* Whether this key (the real one) is down
				 * This is duplicated in kbd->key_state_vector
				 */
  int mouse_highlighted;	/* Whether a button is depressed on this */
  unsigned long modifier_bits;	/* Which modifiers this key sets */
  int auto_repeat_p;		/* Whether this key autorepeats */
} KeyPart;

typedef struct _KeyRec {		/* full instance record */
    CorePart core;
    KeyPart key;
} KeyRec;

#endif /* _KeyWidgetP_H_ */
