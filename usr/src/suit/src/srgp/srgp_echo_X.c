#include "HEADERS.h"
#include "srgplocal.h"

/** INFORMATION ABOUT THE LOW-LEVEL ECHOING DRIVERS

SRGP__initEchoModule ()
   Must be called once, during initialization.
   The echo-attribute globals must have already been given
      default initial values!

SRGP__updateLocatorRubberAnchor ()
   Acesses current locator rubber anchor from globals.
   May be called whether rubber echo is active or not.

SRGP__enableLocatorRubberEcho ()
   Accesses current locator measure info from global variables.
   May be called carelessly:
      1) Will not re-enable if already enabled
      2) Will refuse to enable echo for a device that is:
	  A) not active currently, and 
	  B) not in a state for which echo is desired

SRGP__updateLocatorRubberEcho ()
   Accesses current locator measure info from global variables.
   May be called carelessly:
      Will not update if device is currently disabled.

SRGP__disableLocatorRubberEcho ()
   May be called carelessly:
      Will not re-disable if already disabled


SRGP__updateLocatorCursorShape ()
   May be called whether or not the cursor echo is active.

SRGP__enableLocatorCursorEcho ()
SRGP__disableLocatorCursorEcho ()
      May be called carelessly.

SRGP__updateRawCursorPosition ()
      Accesses cur_locator_measure from global variables.
      Informs the underlying graphics package of the desired "cursor warp".
      Automatically updates any type of locator echo: cursor or rubber.


SRGP__updateKeyboardEchoAttributes ()
   May be called whether or not key echo is active.
   Obtains attributes from global variables.

SRGP__enableKeyboardEcho ()
SRGP__updateKeyboardEcho ()
SRGP__disableKeyboardEcho ()
      Similar to above: may be called carelessly.

**/


/* FOR LOCATOR ECHO */
static boolean locator_echo_is_active=FALSE;
static GC echo__locator_gc;
static Cursor echo__locator_cursor_shape;
static point echo__locator_rubber_anchor, echo__locator_previous_position;
     /* IN X, not SRGP, COORDS */

/* FOR KEYBOARD ECHO */
static boolean keyboard_echo_is_active=FALSE;
static GC echo__keyboard_gc;
static XFontStruct *echo__keyboard_font;
static int
	/* THE SCREEN LOCATION OF THE ECHO IN X's COORDINATE SYSTEM!
	 */
    echo__keyboard_left,   /* X */
    echo__keyboard_origin;  /* Y */
static XCharStruct echo__keyboard_overall;
static int font_ascent, font_descent;



void
SRGP__initEchoModule ()
{
   /* CREATE THE TWO GC's. */
   echo__keyboard_gc = XCreateGC (srgpx__display, srgpx__screenwin, 0L, NULL);
   echo__locator_gc = XCreateGC (srgpx__display, srgpx__screenwin, 0L, NULL);

   /* INITIALIZE KEYBOARD GC: font, color, origin */
   SRGP__updateKeyboardEchoAttributes ();

   /* DEFAULT LOCATOR-ECHO RUBBER ANCHOR (same as keyboard echo). */
   echo__locator_rubber_anchor =
      SRGP_defPoint(echo__keyboard_left, echo__keyboard_origin);

   /* INITIALIZE LOCATOR RUBBER-ECHO GC: write-mode XOR. */
   if (XWHITE == 0)
       XSetFunction (srgpx__display, echo__locator_gc, GXxor);
   else
       XSetFunction(srgpx__display, echo__locator_gc, GXequiv);
   XSetForeground (srgpx__display, echo__locator_gc, XBLACK);
}



static void
ToggleRubberRect ()
/* DRAWS BETWEEN rubber_anchor AND previous_position */
{
   int tlx, tly, width, height;

   if (echo__locator_rubber_anchor.x < echo__locator_previous_position.x) {
      tlx = echo__locator_rubber_anchor.x;
      width = echo__locator_previous_position.x - tlx + 1;
   }
   else {
      tlx = echo__locator_previous_position.x;
      width = echo__locator_rubber_anchor.x - tlx + 1;
   }

   if (echo__locator_rubber_anchor.y < echo__locator_previous_position.y) {
      tly = echo__locator_rubber_anchor.y;
      height = echo__locator_previous_position.y - tly + 1;
   }
   else {
      tly = echo__locator_previous_position.y;
      height = echo__locator_rubber_anchor.y - tly + 1;
   }

   XDrawRectangle (srgpx__display, srgpx__screenwin,
		   echo__locator_gc,
		   tlx,tly,  width, height);
}

	 
static void
ToggleRubberLine()
/* DRAWS BETWEEN rubber_anchor AND previous_position */
{
   XDrawLine (srgpx__display, srgpx__screenwin,
	      echo__locator_gc,
	      ExplodePt(echo__locator_previous_position),
	      ExplodePt(echo__locator_rubber_anchor));
}


static void
ToggleRubberEcho()
{
   if (srgp__cur_locator_echo_type == RUBBER_RECT) 
      ToggleRubberRect();
   else
      ToggleRubberLine();
}


void
SRGP__enableLocatorRubberEcho ()
{
   if ( ! locator_echo_is_active)
      if (srgp__cur_mode[LOCATOR] != INACTIVE) 
	 if (srgp__cur_locator_echo_type > CURSOR) {
	    echo__locator_previous_position =
	       SRGP_defPoint 
		  (srgp__cur_locator_measure.position.x, 
		  SCREENFIXED(srgp__cur_locator_measure.position.y));
	    echo__locator_rubber_anchor =
	       SRGP_defPoint
		  (srgp__cur_locator_echo_anchor.x,
		   SCREENFIXED(srgp__cur_locator_echo_anchor.y));
	    locator_echo_is_active = TRUE;
	    ToggleRubberEcho();
	 }
}


void
SRGP__disableLocatorRubberEcho()
{
   if (locator_echo_is_active) {
      ToggleRubberEcho();
      locator_echo_is_active = FALSE;
   }
}


void
SRGP__updateLocatorRubberEcho ()
{
   if (locator_echo_is_active) {
      ToggleRubberEcho();
      echo__locator_previous_position =
	 SRGP_defPoint (srgp__cur_Xcursor_x, srgp__cur_Xcursor_y);
      ToggleRubberEcho();
   }      
}
   


void
SRGP__updateLocatorRubberAnchor ()
{
   if (locator_echo_is_active)
      ToggleRubberEcho();

   echo__locator_rubber_anchor =
      SRGP_defPoint
	 (srgp__cur_locator_echo_anchor.x,
	  SCREENFIXED(srgp__cur_locator_echo_anchor.y));

   if (locator_echo_is_active)
      ToggleRubberEcho();
}



void
SRGP__enableLocatorCursorEcho ()
{
   if (srgp__cur_mode[LOCATOR] != INACTIVE) 
      if (srgp__cur_locator_echo_type == CURSOR) {
	 XDefineCursor (srgpx__display, srgpx__screenwin, 
			echo__locator_cursor_shape);
      }
}


void
SRGP__disableLocatorCursorEcho ()
{ 
   XUndefineCursor (srgpx__display, srgpx__screenwin);
}


void
SRGP__updateLocatorCursorShape ()
{
   echo__locator_cursor_shape = srgp__cursorTable[srgp__cur_cursor];
   SRGP__enableLocatorCursorEcho ();
}






/** KEYBOARD ECHO **/

static void
DrawText ()
{
   int direction;

   XDrawImageString (srgpx__display, srgpx__screenwin, echo__keyboard_gc,
		     echo__keyboard_left, echo__keyboard_origin,
		     srgp__cur_keyboard_measure.buffer, 
		     srgp__cur_keyboard_measure_length);
   XTextExtents (echo__keyboard_font,
  		 srgp__cur_keyboard_measure.buffer, 
  		 srgp__cur_keyboard_measure_length,	
 		 &direction, &font_ascent, &font_descent,
 		 &echo__keyboard_overall);
   XFillRectangle (srgpx__display, srgpx__screenwin, echo__keyboard_gc,
  		   echo__keyboard_left + echo__keyboard_overall.width + 2,
 		   echo__keyboard_origin - font_ascent,
 		   7, font_ascent + font_descent);
  }


static void
EraseText()
{
   XClearArea (srgpx__display, srgpx__screenwin,
	       echo__keyboard_left, 
	       echo__keyboard_origin - font_ascent,
	       echo__keyboard_overall.width + 9, 
 	       font_ascent + font_descent,
	       FALSE);
}


void
SRGP__enableKeyboardEcho ()
{
   if ( ! keyboard_echo_is_active)
      if (srgp__cur_mode[KEYBOARD] != INACTIVE) 
	 if (srgp__cur_keyboard_processing_mode == EDIT) {
	    keyboard_echo_is_active = TRUE;
	    DrawText();
	 }
}


void
SRGP__disableKeyboardEcho ()
{
   if (keyboard_echo_is_active) {
      EraseText();
      keyboard_echo_is_active = FALSE;
   }
}


void
SRGP__updateKeyboardEcho ()
{
   if (keyboard_echo_is_active) {
      EraseText();
      DrawText();
   }
}


void
SRGP__updateKeyboardEchoAttributes ()
{
   if (keyboard_echo_is_active) {
      EraseText();
   }

   echo__keyboard_left = srgp__cur_keyboard_echo_origin.x;
   echo__keyboard_origin = SCREENFIXED(srgp__cur_keyboard_echo_origin.y);

   XSetForeground (srgpx__display, echo__keyboard_gc, 
		   XCOLOR(COLORINDEX(srgp__cur_keyboard_echo_color)));
   XSetBackground (srgpx__display, echo__keyboard_gc, 
		   XCOLOR(COLORINDEX(SRGP_WHITE)));

   XSetFont (srgpx__display, echo__keyboard_gc, 
	     (echo__keyboard_font =
	        srgp__fontTable[srgp__cur_keyboard_echo_font])->fid);

   if (keyboard_echo_is_active) {
      DrawText();
   }
}






