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
static RgnHandle savedclip;
static PenState rubberpenstate;
static point echo__locator_rubber_anchor, echo__locator_previous_position;
     /* IN MAC, not SRGP, COORDS */

/* FOR KEYBOARD ECHO */
static boolean keyboard_echo_is_active=FALSE;
static int
	/* THE SCREEN LOCATION OF THE ECHO IN THE RAW COORDINATE SYSTEM!
	 */
    echo__keyboard_left,   /* X */
    echo__keyboard_origin;  /* Y */



void
SRGP__initEchoModule ()
{
   SRGP__updateKeyboardEchoAttributes ();

   /* DEFAULT LOCATOR-ECHO RUBBER ANCHOR (same as keyboard echo). */
   echo__locator_rubber_anchor =
      SRGP_defPoint(echo__keyboard_left, echo__keyboard_origin);

   savedclip=NewRgn();
   SetPt (&rubberpenstate.pnSize, 1, 1);
   rubberpenstate.pnMode = patXor;
   memcpy (rubberpenstate.pnPat, black, (size_t)sizeof(Pattern));;
}



static void
ToggleRubberRect (void)
/* DRAWS BETWEEN rubber_anchor AND previous_position */
{
   int tlx, tly, trx, try;
   Rect r;

   if (echo__locator_rubber_anchor.x < echo__locator_previous_position.x) {
      tlx = echo__locator_rubber_anchor.x;
      trx = echo__locator_previous_position.x;
   }
   else {
      tlx = echo__locator_previous_position.x;
      trx = echo__locator_rubber_anchor.x;
   }

   if (echo__locator_rubber_anchor.y < echo__locator_previous_position.y) {
      tly = echo__locator_rubber_anchor.y;
      try = echo__locator_previous_position.y;
   }
   else {
      tly = echo__locator_previous_position.y;
      try = echo__locator_rubber_anchor.y;
   }
   
   SetRect (&r, tlx, tly, trx, try);
   FrameRect (&r);
}

	 
static void
ToggleRubberLine (void)
/* DRAWS BETWEEN rubber_anchor AND previous_position */
{
   MoveTo (ExplodePt(echo__locator_previous_position));
   LineTo (ExplodePt(echo__locator_rubber_anchor));
}


static void
ToggleRubberEcho (void)
{
   PenState savedstate;
   GrafPtr savedgraf;

   GetPort (&savedgraf);
   SetPort (srgp__canvasTable[0].drawable.win);
   GetClip (savedclip);
   GetPenState (&savedstate);

   SetPenState (&rubberpenstate);
   if (srgp__cur_locator_echo_type == RUBBER_RECT) 
      ToggleRubberRect();
   else
      ToggleRubberLine();

   SetPenState (&savedstate);
   SetClip (savedclip);
   SetPort (savedgraf);
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
      if (srgp__cursorTable[srgp__cur_cursor] == (CursHandle)-1)
         InitCursor();
      else
         SetCursor (*(srgp__cursorTable[srgp__cur_cursor]));
}


void
SRGP__disableLocatorCursorEcho ()
{
   InitCursor();
}


void
SRGP__updateLocatorCursorShape ()
{
   SRGP__enableLocatorCursorEcho ();
}






/** KEYBOARD ECHO **/

static FontInfo theinfo;
static short overallwidth=0;  /* does not include width of cursor */


static void
DrawText (void)
{
   CGrafPort saveport;
   CGrafPtr screenport = (CGrafPtr)srgpmac__cwindow;
      
   SetPort (screenport);
   saveport = *screenport;
   
   CtoPstr (srgp__cur_keyboard_measure.buffer);

   screenport->fgColor = srgp__cur_keyboard_echo_color;
   TextFont (srgp__fontTable[srgp__cur_keyboard_echo_font].txFont);
   TextSize (srgp__fontTable[srgp__cur_keyboard_echo_font].txSize);
   TextFace (srgp__fontTable[srgp__cur_keyboard_echo_font].txFace);
   PenPat (black);
   TextMode (srcCopy);
   MoveTo (echo__keyboard_left, echo__keyboard_origin);
   DrawString (srgp__cur_keyboard_measure.buffer);

   GetFontInfo (&theinfo);
   Move (0, -theinfo.ascent);
   PenSize (5, theinfo.ascent + theinfo.descent);
   Line (0,0);
   
   overallwidth = screenport->pnLoc.h - echo__keyboard_left;

   PtoCstr (srgp__cur_keyboard_measure.buffer);

   *screenport = saveport;
   
   /* Restore port to what it was before... */
   SetPort (srgp__canvasTable[srgp__curActiveCanvasId].drawable.win);
   
}


static void
EraseText (void)
{   
   Rect r;
      
   SetPort ((CGrafPtr)srgpmac__cwindow);
   SetRect (&r, echo__keyboard_left, 
	       echo__keyboard_origin - theinfo.ascent,
	       echo__keyboard_left + overallwidth + 5, 
	       echo__keyboard_origin + theinfo.descent);
   EraseRect (&r);
   SetPort (srgp__canvasTable[srgp__curActiveCanvasId].drawable.win);
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

/*
   XSetForeground (srgpx__display, echo__keyboard_gc, 
		   COLORINDEX(srgp__cur_keyboard_echo_color));
   XSetBackground (srgpx__display, echo__keyboard_gc, 
		   COLORINDEX(SRGP_WHITE));
   XSetFont (srgpx__display, echo__keyboard_gc, 
	     (echo__keyboard_font =
	        srgp__fontTable[srgp__cur_keyboard_echo_font])->fid);
*/

   if (keyboard_echo_is_active) {
      DrawText();
   }
}






