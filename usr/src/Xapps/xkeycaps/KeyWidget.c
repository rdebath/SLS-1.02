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

#include <X11/cursorfont.h>
#include <X11/Intrinsic.h>
#include "KbdWidget.h"
#include "KeyWidgetP.h"
#include <stdio.h>

#define MAX(a,b) ((a)>(b)?(a):(b))
#define MIN(a,b) ((a)<(b)?(a):(b))

static void KeyRealize ();
static void KeyExpose ();
static void KeyInitialize ();

static void KeyHighlightProc (), KeyDehighlightProc ();

#define XtNgutterWidth  "gutterWidth"
#define XtCGutterWidth  "GutterWidth"
#define XtNkeycapColor  "keycapColor"
#define XtNkeycodeColor "keycodeColor"
#define XtNkeycapFont   "keycapFont"
#define XtNkeycodeFont  "keycodeFont"
#define XtNcursorFont   "cursorFont"

#ifndef CURSORFONT
#define CURSORFONT "cursor"
#endif

static XtResource key_resources [] = {
  { XtNhighlight, XtCBackground, XtRPixel, sizeof (String),
      XtOffset (KeyWidget, key.highlight_pixel), XtRString,
      XtDefaultBackground },
  { XtNgutterWidth, XtCGutterWidth, XtRInt, sizeof (int),
      XtOffset (KeyWidget, key.gutter_width), XtRString, "3" },
  { XtNkeycapColor, XtCForeground, XtRPixel, sizeof (String),
      XtOffset (KeyWidget, key.keycap_pixel), XtRString,
      XtDefaultForeground },
  { XtNkeycodeColor, XtCForeground, XtRPixel, sizeof (String),
      XtOffset (KeyWidget, key.keycode_pixel), XtRString,
      XtDefaultForeground },
  { XtNkeycapFont, XtCFont, XtRFontStruct, sizeof (String),
      XtOffset (KeyWidget, key.keycap_font),
      XtRString, "*-helvetica-bold-r-*-*-*-100-*-*-*-*-*-*" },
  { XtNkeycodeFont, XtCFont, XtRFontStruct, sizeof (String),
      XtOffset (KeyWidget, key.keycode_font),
      XtRString, "*-courier-medium-r-*-*-*-100-*-*-*-*-*-*" },
  { XtNcursorFont, XtCFont, XtRFontStruct, sizeof (String),
      XtOffset (KeyWidget, key.cursor_font),
      XtRString, CURSORFONT }
};

KeyClassRec keyClassRec = {
    { /*
       * 	core fields
       */
    /* superclass		*/	&widgetClassRec,
    /* class_name		*/	"Key",
    /* widget_size		*/	sizeof (KeyRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	KeyInitialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	KeyRealize,
    /* actions			*/	NULL,
    /* num_actions		*/	0,
    /* resources		*/	key_resources,
    /* resource_count		*/	XtNumber (key_resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	NULL,
    /* resize			*/	XtInheritResize,
    /* expose			*/	KeyExpose,
    /* set_values		*/	NULL,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	XtInheritAcceptFocus,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	NULL,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
    },
    { /*
       * 	key_class fields
       */
    /* highlight_key		*/	KeyHighlightProc,
    /* dehighlight_key		*/	KeyDehighlightProc
    }
};

WidgetClass keyWidgetClass = (WidgetClass) &keyClassRec;

static void 
KeyInitialize (request, new)
     KeyWidget request, new;
{
  if (new->core.width <= 0) new->core.width = 3;
  if (new->core.height <= 0) new->core.height = 3;
  new->key.x = new->key.y = 0;
  new->key.highlighted_p = 0;
  new->key.key_highlighted = 0;
  new->key.mouse_highlighted = 0;
  new->key.modifier_bits = 0;
  new->key.auto_repeat_p = 0;
}


static void
KeyRealize (gw, valuemaskp, attr)
     Widget gw;
     XtValueMask *valuemaskp;
     XSetWindowAttributes *attr;
{
  char *b, *top, *bot, buf [255];
  XGCValues gcvalues;
  KeyWidget w = (KeyWidget) gw;
  Display *dpy = XtDisplay (w);
  XtCreateWindow ((Widget) w, InputOutput, (Visual *)CopyFromParent,
		  *valuemaskp, attr);
  gcvalues.foreground = w->key.keycap_pixel;
  gcvalues.font = w->key.keycap_font->fid;
  w->key.keycap_gc  = XtGetGC (gw, (unsigned) GCFont|GCForeground, &gcvalues);
  gcvalues.font = w->key.cursor_font->fid;
  w->key.cursor_gc = XtGetGC (gw, (unsigned) GCFont|GCForeground, &gcvalues);
  gcvalues.foreground = w->key.keycode_pixel;
  gcvalues.font = w->key.keycode_font->fid;
  w->key.keycode_gc = XtGetGC (gw, (unsigned) GCFont|GCForeground, &gcvalues);

  top = w->key.key->top_keysym;
  bot = w->key.key->bottom_keysym;
  if (top && !*top) top = 0;
  if (bot && !*bot) bot = 0;
  b = buf;
  b[0] = 0;
  if (top)
    {
      int i = strlen (top);
      strncpy (buf, top, i+1);
      if (i > 1 && (buf [i-1] == ' ' || buf [i-1] == '-'))
	buf [i-1] = 0, i--;
      else if (i > 0 && bot)
	buf [i] = ' ', buf [i+1] = 0, i++;
      b = buf+i;
    }
  if (bot) strcpy (b, bot);
  b = buf;
  if (b[0] == ' ' && b[1] == 0) b = "space";
  w->key.key_name = XtNewString (b);
}


static void draw_key ();

static void
KeyExpose (gw, valuemaskp, attr)
     Widget gw;
     XtValueMask *valuemaskp;
     XSetWindowAttributes *attr;
{
  draw_key ((KeyWidget) gw);
}


static void
draw_key (w)
     KeyWidget w;
{
  Display *dpy = XtDisplay (w);
  Window window = XtWindow (w);
  int height = w->core.height;
  struct key *key = w->key.key;
  XFontStruct *keycap_font  = w->key.keycap_font;
  XFontStruct *keycode_font = w->key.keycode_font;
  XFontStruct *cursor_font  = w->key.cursor_font;
  GC keycap_gc  = w->key.keycap_gc;
  GC keycode_gc = w->key.keycode_gc;
  GC cursor_gc  = w->key.cursor_gc;
  char *top = key->top_keysym;
  char *bot = key->bottom_keysym;
  XFontStruct *top_font = cursor_font;
  XFontStruct *bot_font = cursor_font;
  GC top_gc = cursor_gc;
  GC bot_gc = cursor_gc;
  int inner_margin = 2;
  int x, y;
  char buf [100];

  char left[2], right[2], up[2], down[2];
  left[1] = right[1] = up[1] = down[1] = 0;
  left[0] = XC_sb_left_arrow; right[0] = XC_sb_right_arrow;
  up[0] = XC_sb_up_arrow; down[0] = XC_sb_down_arrow;

  if (top && !*top) top = 0;
  if (bot && !*bot) bot = 0;

  if      (string_equal (top, "leftArrow"))  top = left;
  else if (string_equal (top, "rightArrow")) top = right;
  else if (string_equal (top, "upArrow"))    top = up;
  else if (string_equal (top, "downArrow"))  top = down;
  else top_font = keycap_font, top_gc = keycap_gc;

  if      (string_equal (bot, "leftArrow"))  bot = left;
  else if (string_equal (bot, "rightArrow")) bot = right;
  else if (string_equal (bot, "upArrow"))    bot = up;
  else if (string_equal (bot, "downArrow"))  bot = down;
  else bot_font = keycap_font, bot_gc = keycap_gc;

#define PERCHAR(font,c) \
  (font->per_char \
   ? &font->per_char[(c) - font->min_char_or_byte2] \
   : &font->max_bounds)

#define STRHEIGHT(font, var) \
  (font == cursor_font ? PERCHAR (font, var[0])->ascent : font->ascent)

#define MAXSTRHEIGHT(font, font2, var) \
  MAX (font->ascent, \
       (font2 == cursor_font \
	? PERCHAR (font2, var[0])->ascent \
	: font2->ascent))

  XClearWindow (dpy, window);
  x = y = 0;
  if (top)
    {
      x = inner_margin - PERCHAR (top_font, top[0])->lbearing;
      y = inner_margin + STRHEIGHT (top_font, top);
      XDrawString (dpy, window, top_gc, x, y, top, strlen (top));
    }
  if (bot)
    {
      x = inner_margin - PERCHAR (bot_font, bot[0])->lbearing;
      y = inner_margin
	+ (top ? MAXSTRHEIGHT (keycap_font, top_font, top)
	   : keycap_font->ascent)
	  + STRHEIGHT (bot_font, bot);
      XDrawString (dpy, window, bot_gc, x, y, bot, strlen (bot));
    }
#undef PERCHAR
#undef STRHEIGHT
#undef MAXSTRHEIGHT

  if (! key->keycode) return;
  sprintf (buf, "%02X", key->keycode);
  x = w->core.width - (w->core.border_width * 2
		       + string_width (buf, keycode_font));
  y = w->core.height - (w->core.border_width * 2 + keycode_font->descent);
  if (x <= inner_margin) return;
  if (y - keycode_font->ascent <= inner_margin) return;
  XDrawString (dpy, window, keycode_gc, x, y, buf, strlen (buf));
}



void
KeyHighlight (keyw)
     KeyWidget keyw;
{
  if (keyw->key.highlighted_p) return;
  (* ((KeyWidgetClass) keyw->core.widget_class)->key_class.highlight_key)
    (keyw);
  keyw->key.highlighted_p = 1;
}

void
KeyDehighlight (keyw)
     KeyWidget keyw;
{
  if (! keyw->key.highlighted_p) return;
  (* ((KeyWidgetClass) keyw->core.widget_class)->key_class.dehighlight_key)
    (keyw);
  keyw->key.highlighted_p = 0;
}


#include <X11/bitmaps/gray>

/* This is a little sleazy: we're caching a pixmap in global space instead 
   of caching it per-display, which means if someone were to include this
   widget-set in an application that used multiple displays (not bloody 
   likely!) this would have to be fixed.  But the alternative is adding
   a new slot and resource to each and every key, which isn't worth it.
 */
static Pixmap highlight_pixmap = 0;

static Pixmap
make_highlight_pixmap (w)
     KeyWidget w;
{
  return XCreatePixmapFromBitmapData
    (XtDisplay (w), XtWindow (w), gray_bits, gray_width, gray_height,
     w->key.keycap_pixel, w->core.background_pixel,
     DefaultDepthOfScreen (DefaultScreenOfDisplay (XtDisplay (w))));
}


#ifdef LOSE_LIKE_Xt	/* Doing this the Xt way is just too damn slow... */

static void
KeyHighlightProc (w)
     KeyWidget w;
{
  Arg av [2];
  int ac = 0;
  w->key.background_pixel = w->core.background_pixel;
  if (w->key.background_pixel == w->key.highlight_pixel)
    {
      if (! highlight_pixmap) highlight_pixmap = make_hilight_pixmap (w);
      XtSetArg (av[ac], XtNbackgroundPixmap, highlight_pixmap);
    }
  else
    XtSetArg (av[ac], XtNbackground, w->key.highlight_pixel);
  ac++;
  XtSetValues (w, av, ac);
}


static void
KeyDehighlightProc (keyw)
     KeyWidget keyw;
{
  Arg av [2];
  int ac = 0;
  if (highlight_pixmap)
    XtSetArg (av[ac], XtNbackgroundPixmap, XtUnspecifiedPixmap);
  else
    XtSetArg (av[ac], XtNbackground, keyw->key.background_pixel);
  ac++;
  XtSetValues (keyw, av, ac);
}


#else /* !LOSE_LIKE_Xt */

static void
KeyHighlightProc (w)
     KeyWidget w;
{
  w->key.background_pixel = w->core.background_pixel;
  if (w->key.background_pixel == w->key.highlight_pixel)
    {
      if (! highlight_pixmap) highlight_pixmap = make_highlight_pixmap (w);
      XSetWindowBackgroundPixmap (XtDisplay (w), XtWindow (w), highlight_pixmap);
    }
  else
    XSetWindowBackground (XtDisplay (w), XtWindow (w), w->key.highlight_pixel);
  draw_key (w);
}


static void
KeyDehighlightProc (w)
     KeyWidget w;
{
  XSetWindowBackground (XtDisplay (w), XtWindow (w), w->key.background_pixel);
  draw_key (w);
}

#endif /* !LOSE_LIKE_Xt */
