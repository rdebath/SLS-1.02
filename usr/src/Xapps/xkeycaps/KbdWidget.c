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

#include <stdio.h>
#include "KbdWidgetP.h"
#include "KeyWidgetP.h"
#include "all-kbds.h"

#define MAX(a,b) ((a)>(b)?(a):(b))
#define MIN(a,b) ((a)<(b)?(a):(b))

static void KbdResize();
static void KbdRealize();
static void KbdInitialize();
static void ChangeManaged();
static void SetValues();
static XtGeometryResult GeometryManager ();
static XtGeometryResult PreferredSize ();
static void make_key_widgets ();
static void place_keys ();

extern void init_modifier_mapping ();


static XtResource keyboard_resources [] = {
  { "keyboard", "Keyboard", XtRString, sizeof (String),
      XtOffset (KeyboardWidget, keyboard.kbd_name), XtRString, "" },
#ifdef HAVE_XTRAP
  {  "useXTrap", "UseXTrap", XtRBoolean, sizeof (XtRBoolean),
       XtOffset (KeyboardWidget, keyboard.use_xtrap), XtRImmediate, False },
#endif
  { "selectCursor", "Cursor", XtRCursor, sizeof (Cursor),
      XtOffset (KeyboardWidget, keyboard.select_cursor), XtRString,
      "crosshair" }
};

extern void highlight_key_action ();
extern void unhighlight_key_action ();
extern void toggle_key_action ();
extern void simulate_KeyPress_action ();
extern void simulate_KeyRelease_action ();
extern void track_key_action ();
extern void untrack_key_action ();
extern void describe_key_action ();
extern void keyboard_track_motion_hook ();
extern void key_menu_popup_action ();

XtActionsRec keyboard_actions [] = {
  {"HighlightKey",	highlight_key_action},
  {"UnhighlightKey",	unhighlight_key_action},
  {"ToggleKey",		toggle_key_action},
  {"SimulateKeyPress",	simulate_KeyPress_action},
  {"SimulateKeyRelease",simulate_KeyRelease_action},
  {"TrackKey",		track_key_action},
  {"UntrackKey",	untrack_key_action},
  {"DescribeKey",	describe_key_action}
};


char keyboard_default_translations[] = "\
<Motion>:	DescribeKey(mouse, unlessTracking)	\n\
\
<KeyDown>:	HighlightKey()				\
		DescribeKey(unlessMod)			\
		DescribeKey(displayed)			\
		SimulateKeyPress()			\n\
\
<KeyUp>:	UnhighlightKey()			\
		DescribeKey(displayed)			\
		SimulateKeyRelease()			\n\
\
<Btn1Down>:	HighlightKey(unlessMod)			\
		ToggleKey(ifMod)			\
		TrackKey(unlessMod)			\
		SimulateKeyPress(ifHighlighted)		\
		SimulateKeyRelease(unlessHighlighted)	\n\
\
<Btn1Up>:	UntrackKey(highlighted)			\
		SimulateKeyRelease(highlighted, unlessMod) \
		UnhighlightKey(highlighted, unlessMod)	\n\
\
<Btn3Down>:	XawPositionSimpleMenu(keyMenu)		\
		MenuPopup(keyMenu)			\n\
";

KeyboardClassRec keyboardClassRec = {
    { /*
       *	core_class fields
       */
    /* superclass		*/	(WidgetClass)&compositeClassRec,
    /* class_name		*/	"Keyboard",
    /* widget_size		*/	sizeof (KeyboardRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	KbdInitialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	KbdRealize,
    /* actions			*/	keyboard_actions,
    /* num_actions		*/	XtNumber (keyboard_actions),
    /* resources		*/	keyboard_resources,
    /* resource_count		*/	XtNumber (keyboard_resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	NULL,
    /* resize			*/	KbdResize,
    /* expose			*/	NULL,
    /* set_values		*/	NULL,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	keyboard_default_translations,
    /* query_geometry		*/	PreferredSize,
    /* display_accelerator	*/	NULL,
    /* extension		*/	NULL
    },
    { /*
       *	composite_class fields
       */
    /* geometry_manager		*/	GeometryManager,
    /* change_managed		*/	ChangeManaged,
    /* insert_child		*/	XtInheritInsertChild,
    /* delete_child		*/	XtInheritDeleteChild,
    /* extension		*/	NULL
    }
};

WidgetClass keyboardWidgetClass = (WidgetClass) &keyboardClassRec;

static void 
KbdInitialize (request, new)
     KeyboardWidget request, new;
{
  struct keyboard *kbd = choose_kbd (XtDisplay (new), new->keyboard.kbd_name);
  new->keyboard.kbd = kbd;
  new->keyboard.key_under_mouse = 0;
  new->keyboard.mouse_highlighted_key = 0;
  new->keyboard.documented_key = 0;
  new->keyboard.tracking_key = 0;
  new->keyboard.target_window = 0;
  make_key_widgets (new);
  place_keys (new);
  init_modifier_mapping (new);
}

static void
do_layout (parent)
     KeyboardWidget parent;
{
  struct keyboard *kbd = parent->keyboard.kbd;

  int i, j;
  int width = parent->core.width;
  int height = parent->core.height;
  int max_width = parent->keyboard.max_width + (kbd->horiz_border * 2);
  int max_height = parent->keyboard.max_height + (kbd->vert_border * 2);
  float x_scale = (float) width / (float) max_width;
  float y_scale = (float) height / (float) max_height;
  int x_off, y_off;

  if (x_scale < 1) x_scale = 1;
  if (y_scale < 1) y_scale = 1;

  /* Be square */
  if (x_scale < y_scale) y_scale = x_scale;
  else if (y_scale < x_scale) x_scale = y_scale;

  x_off = ((((float) width) - (max_width * x_scale)) / 2
	   + kbd->horiz_border * x_scale);
  y_off = ((((float) height) - (max_height * y_scale)) / 2
	   + kbd->vert_border * y_scale);
    
  if (XtWindow (parent))
    XUnmapSubwindows (XtDisplay (parent), XtWindow (parent)); /* sleazy */

  for (i = 0; i < kbd->nrows; i++)
    {
      for (j = 0; j < kbd->rows[i].nkeys; j++)
	{
	  int off;
	  struct key *key = &kbd->rows[i].keys[j];
	  KeyWidget child = (KeyWidget) key->widget;
	  if (! child) continue;
	  off = child->core.border_width * 2 + child->key.gutter_width;
	  XtMoveWidget ((Widget) child,
			(int) (x_off + child->key.x * x_scale),
			(int) (y_off + child->key.y * y_scale));
	  XtResizeWidget ((Widget) child,
			  MAX (1, (int) (key->width * x_scale - off)),
			  MAX (1, (int) (key->height * y_scale - off)),
			  child->core.border_width);
	}
    }

  if (XtWindow (parent))
    XMapSubwindows (XtDisplay (parent), XtWindow (parent)); /* yzaels */

  parent->keyboard.x_scale = x_scale;
  parent->keyboard.y_scale = y_scale;
}


static void KbdResize (w)
     KeyboardWidget w;
{
    do_layout (w);
}


static XtGeometryResult
PreferredSize (w, request, preferred)
     KeyboardWidget w;
     XtWidgetGeometry *request, *preferred;
{
  return XtGeometryYes;
}

static XtGeometryResult
GeometryManager (w, request, reply)
     KeyboardWidget w;
     XtWidgetGeometry *request, *reply;
{
  return XtGeometryNo;
}


static void ChangeManaged (w)
     KeyboardWidget w;
{
    if (w->core.width <= 0 || w->core.height <= 0)
      {
	int default_scale = w->keyboard.kbd->default_scale;
	int horiz = (w->keyboard.kbd->horiz_border * 2) + 1;
	int vert = (w->keyboard.kbd->vert_border * 2) + 1;
	w->core.width  = (w->keyboard.max_width + horiz) * default_scale;
	w->core.height = (w->keyboard.max_height + vert) * default_scale;
      }
    do_layout (w);
}


static void
KbdRealize (widget, value_mask, attributes)
    Widget widget;
    Mask *value_mask;
    XSetWindowAttributes *attributes;
{
  XtAppContext app = XtWidgetToApplicationContext (widget);
  XtAppAddActionHook (app, keyboard_track_motion_hook, (XtPointer) widget);
  if (widget->core.width == 0) widget->core.width = 10;
  if (widget->core.height == 0) widget->core.height = 10;
  XtCreateWindow (widget, (unsigned int) InputOutput,
		  (Visual *) CopyFromParent, *value_mask, attributes);
#ifdef HAVE_XTRAP
  xkeycaps_xtrap_open_connection ((KeyboardWidget) widget);
#endif
}



static void
place_keys (widget)
     KeyboardWidget widget;
{
  int i, j, k;
  int x = 0;
  int y = 0;
  int max_syms = 0;
  struct keyboard *kbd = widget->keyboard.kbd;
  widget->keyboard.max_width = x;
  widget->keyboard.max_height = y;
  for (i = 0; i < kbd->nrows; i++)
    {
      struct row *row = &kbd->rows [i];
      for (j = 0; j < row->nkeys; j++)
	{
	  struct key *key = &row->keys [j];
	  if (key->widget)
	    {
	      key->widget->key.x = x;
	      key->widget->key.y = y;
	    }
	  x += key->width;
	  for (k = 7; k && !key->default_keysyms [k]; k--) ;
	  if (k > max_syms) max_syms = k;
	}
      if (x > widget->keyboard.max_width)
	widget->keyboard.max_width = x;
      x = 0;
      y += row->height;
      if (y > widget->keyboard.max_height)
	widget->keyboard.max_height = y;
    }
  widget->keyboard.default_keysyms_per_code = max_syms + 1;
}


static void
make_key_widgets (widget)
     KeyboardWidget widget;
{
  Display *dpy = XtDisplay (widget);
  struct keyboard *kbd = widget->keyboard.kbd;
  Arg av [20];
  int ac;
  int i, j;
  int default_scale = widget->keyboard.kbd->default_scale;
  int pixel_width = (widget->keyboard.max_width + 1) * default_scale;
  int pixel_height = (widget->keyboard.max_height + 1) * default_scale;
  widget->keyboard.x_scale = widget->keyboard.y_scale = default_scale;
  ac = 0;
  XtSetArg (av[ac], XtNwidth, 10); ac++;
  XtSetArg (av[ac], XtNheight, 10); ac++;

  for (i = 0; i < kbd->nrows; i++)
    for (j = 0; j < kbd->rows[i].nkeys; j++)
      {
	struct key *key = &kbd->rows[i].keys[j];
	if (!key->top_keysym && !key->bottom_keysym && !key->keycode)
	  key->widget = None;
	else
	  {
	    key->widget = (KeyWidget)
	      XtCreateManagedWidget (key->top_keysym, keyWidgetClass,
				     (Widget) widget, av, ac);
	    ((KeyWidget) key->widget)->key.key = key;
	  }
      }
}

