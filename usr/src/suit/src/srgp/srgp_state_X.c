#include "HEADERS.h"
#include "srgplocal.h"

/** X11 VERSION OF state.c
This contains no conditionally compiled code because it should be used
only in the X11 version.
**/


static XWindowAttributes	srgpx__windowattrs;
static XSetWindowAttributes	srgpx__setwindowattrs;
static XSizeHints 		srgpx__sizehints;
static XWMHints			wm_hints;


/** ERROR HANDLER FOR X SERVER EXCEPTIONS
**/

int SRGP__handlerForXerrors (Display *d, XErrorEvent *err)
{
   char msg[80];

   XGetErrorText (d, err->error_code, msg, 80);
   SRGP__error (ERR_X_SERVER, msg);
}



void SRGP__initGraphicsDevice 
   (char *name, int requested_planes, boolean debugasap)
{
   int cnt, i, j;
   int width, height;
   XEvent report;

   if ( (srgpx__display = XOpenDisplay(0)) == 0) {
      fprintf (stderr, "unable to open to x server.\n");
      exit(1);
   }
   
   if (debugasap)
      SRGP_enableSynchronous();
      
   XSetErrorHandler (SRGP__handlerForXerrors);

   srgpx__screen = DefaultScreen(srgpx__display);

   /* CREATE WINDOW FOR VIRTUAL SCREEN (canvas #0) */
   srgp__curActiveCanvasSpec.drawable.win =
      XCreateSimpleWindow (srgpx__display,
			   RootWindow(srgpx__display,srgpx__screen), 0,0,
			   width=srgp__curActiveCanvasSpec.max_xcoord+1,
			   height=srgp__curActiveCanvasSpec.max_ycoord+1,
			   1, 0L, 0L);

   SRGP__initColor (requested_planes);


   /* SET GRAPHICS CONTEXT */
   srgp__curActiveCanvasSpec.gc_frame =
      XCreateGC (srgpx__display, srgp__curActiveCanvasSpec.drawable.win,
		 0L, NULL);
   srgp__curActiveCanvasSpec.gc_fill =
      XCreateGC (srgpx__display, srgp__curActiveCanvasSpec.drawable.win,
		 0L, NULL);

   /* INIT INPUT */
   XSelectInput
      (srgpx__display, srgp__curActiveCanvasSpec.drawable.win, 
       ExposureMask|PropertyChangeMask);

   /* SET 0th CANVAS-WINDOW PROPERTIES */
   srgpx__sizehints.flags = PSize | PMinSize | PMaxSize;
   srgpx__sizehints.width = width;     srgpx__sizehints.height = height;
   srgpx__sizehints.min_width = width; srgpx__sizehints.min_height = height;
   srgpx__sizehints.max_width = width; srgpx__sizehints.max_height = height;
   XSetStandardProperties
      (srgpx__display, srgp__curActiveCanvasSpec.drawable.win,
       name, name, 0, 0, 0, &srgpx__sizehints);
   wm_hints.flags = InputHint;
   wm_hints.input = TRUE;
   XSetWMHints (srgpx__display, srgp__curActiveCanvasSpec.drawable.win,
		&wm_hints);

   /* MAP...  Wait for the expose and property events */
   XMapWindow (srgpx__display, srgp__curActiveCanvasSpec.drawable.win);
   i=0; j=0;

   do {
      XNextEvent (srgpx__display, &report);
      switch (report.type) {
       case Expose:
	 if (report.xexpose.count == 0) i++; break;
       case PropertyNotify:
	 j++; srgpx__starttime = srgpx__cur_time = report.xproperty.time; 
	 break;
      }
   } while (i==0 || j==0);

   /* No need for catching property events any more. */
   XSelectInput
      (srgpx__display, srgp__curActiveCanvasSpec.drawable.win, 
                                    ExposureMask);

   /* We set up for backing store. */
  srgpx__setwindowattrs.backing_store = Always;

   XChangeWindowAttributes
      (srgpx__display,
       srgp__curActiveCanvasSpec.drawable.win,
       CWBackingStore, &srgpx__setwindowattrs);
}




/** SRGP_refresh
**/

void
SRGP_refresh ()
{
   XSync (srgpx__display, FALSE);
   SRGP__handleRawEvents (FALSE,FALSE);
}



/** SRGP_enableSynchronous
For use by a system administrator only.
Puts X into synch. mode.
**/

void
SRGP_enableSynchronous ()
{
   XSynchronize (srgpx__display, 1);
   XSync (srgpx__display, FALSE);
}



/*!*/

static boolean resize_allowed = FALSE;

static void
InformWindowManagerOfResizeStrategy()
{
   if (resize_allowed) {
      srgpx__sizehints.flags = PMinSize | PMaxSize | PResizeInc;
      srgpx__sizehints.min_width = 5; srgpx__sizehints.min_height = 5;
      srgpx__sizehints.max_width = 5000; srgpx__sizehints.max_height = 5000;
      srgpx__sizehints.width_inc = 1; srgpx__sizehints.height_inc = 1;
      XSetStandardProperties
	 (srgpx__display, srgp__canvasTable[0].drawable.win,
	  NULL, NULL, 0, 0, 0, &srgpx__sizehints);
   }
   else {
      int width = srgp__canvasTable[0].max_xcoord + 1;
      int height = srgp__canvasTable[0].max_ycoord + 1;
      srgpx__sizehints.flags = PSize | PMinSize | PMaxSize;
      srgpx__sizehints.width = width;     srgpx__sizehints.height = height;
      srgpx__sizehints.min_width = width; srgpx__sizehints.min_height = height;
      srgpx__sizehints.max_width = width; srgpx__sizehints.max_height = height;
      XSetStandardProperties
	 (srgpx__display, srgp__curActiveCanvasSpec.drawable.win,
	  NULL, NULL, 0, 0, 0, &srgpx__sizehints);
   }
}


void
SRGP_allowResize (boolean allow)
{
   resize_allowed = allow;
   InformWindowManagerOfResizeStrategy();
}


void
SRGP__forceScreenResize (int newwidth, int newheight)
{
   XResizeWindow (srgpx__display, srgp__canvasTable[0].drawable.win,
		  newwidth, newheight);
   srgp__canvasTable[0].max_xcoord = newwidth - 1;
   srgp__canvasTable[0].max_ycoord = newheight - 1;
   InformWindowManagerOfResizeStrategy();
}
