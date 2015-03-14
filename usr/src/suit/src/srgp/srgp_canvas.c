#include "HEADERS.h"
#include "srgplocal.h"

#ifdef THINK_C
#include "ChooseWhichQuickDraw.h"
#endif

/** ABOUT CANVASES
At any given time, there is one ACTIVE canvas.

The ACTIVE canvas is the one affected by all output primitives.

Initially, canvas #0 is the only canvas; it is always visible and
   is the size of the screen.  Its coordinate system is the
   one involved in all input commands.

The permanent spec for each canvas is stored in a
   table (srgp__canvasTable).
But the spec for the currently active canvas is
   stored in a cache (srgp__curActiveCanvasSpec) and sometimes
   is more current than the permanent version.
**/
   



/** INTERNAL: setting canvas defaults
Sets the defaults for the currently active canvas.
Affects the cache (srgp__curActiveCanvasSpec) first.
Then copies into the permanent table.
Assumes that "max_coord" fields are already set.
**/

void
SRGP__setCanvasDefaults ()
{
   PUSH_TRACE;

   /* Fool the attribute routines so they *have* to do the work. */
   memset (&srgp__curActiveCanvasSpec.attributes, -1, 
	   sizeof(srgp__attribute_group));

   SRGP_setWriteMode (WRITE_REPLACE);
   SRGP_setClipRectangle
      (SRGP_defRectangle
	 (0,0,
	  srgp__curActiveCanvasSpec.max_xcoord,
	  srgp__curActiveCanvasSpec.max_ycoord));

   SRGP_setFont (0);
   SRGP_setLineStyle (CONTINUOUS);
   SRGP_setLineWidth (1);
   SRGP_setMarkerStyle (MARKER_CIRCLE);
   SRGP_setMarkerSize (10);
   SRGP_setFillStyle (SOLID);
   SRGP_setPenStyle (SOLID);
   SRGP_setFillBitmapPattern (0);
   SRGP_setFillPixmapPattern (0);
   SRGP_setPenBitmapPattern (0);
   SRGP_setPenPixmapPattern (0);

   /* ERASE CANVAS MANUALLY. */
   SRGP_setColor (0);
   SRGP_fillRectangleCoord
      (0,0,
       srgp__curActiveCanvasSpec.max_xcoord,
       srgp__curActiveCanvasSpec.max_ycoord);
   SRGP_setColor (1);
   SRGP_setBackgroundColor (0);

   /* MAKE SURE CHANGES GET INTO THE ACTUAL TABLE (not just in the cache) */
   srgp__canvasTable[srgp__curActiveCanvasId] = srgp__curActiveCanvasSpec;

   POP_TRACE;
}




/*!*/
void
SRGP_useCanvas (canvasID canvas_id)
{
   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_useCanvas: %d\n", canvas_id);
      srgp_check_system_state();
      srgp_check_extant_canvas(canvas_id);
      LeaveIfNonFatalErr();
   }

   /* UPDATE PERMANENT COPY OF SPEC FOR CURRENT ACTIVE CANVAS. */
   srgp__canvasTable[srgp__curActiveCanvasId] = srgp__curActiveCanvasSpec;

   /* SET NEW ACTIVE, and LOAD CACHE. */
   srgp__curActiveCanvasId = canvas_id;
   srgp__curActiveCanvasSpec = srgp__canvasTable[srgp__curActiveCanvasId];

#ifdef THINK_C
   SetPort (srgp__canvasTable[srgp__curActiveCanvasId].drawable.win);
#endif
}




/*!*/
canvasID
SRGP_createCanvas (int width, int height)
{
   register int new_canvas_id;
   boolean free_one_found;
   canvas_spec *canvsptr;

   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_createCanvas: %d %d\n", width, height);
      srgp_check_system_state();
      LeaveIfNonFatalErr();
   }

   /* FIND AN EMPTY ENTRY IN CANVAS TABLE. */
   new_canvas_id = 1;
   free_one_found = FALSE;
   while ((new_canvas_id <= MAX_CANVAS_INDEX) && !free_one_found)
      if (srgp__canvasTable[new_canvas_id].drawable.bitmap != 0) 
         new_canvas_id++;
      else
	 free_one_found = TRUE;

   if (!free_one_found) {
      SRGP__error (ERR_CANVAS_TABLE_FULL);
      return (-1);
   }

   /* ALL SYSTEMS ARE GO! */
   canvsptr = &(srgp__canvasTable[new_canvas_id]);

   /* ALLOCATE THE BITMAP */
#ifdef X11
   if ( (canvsptr->drawable.bitmap =
	 XCreatePixmap (srgpx__display,
			srgp__canvasTable[0].drawable.win,
			width, height,
			srgp__available_depth)) 
                                        == 0) {
      SRGP__error (ERR_MALLOC);
      return (-1);
   }
#endif

#ifdef THINK_C
#ifdef COLOR_QUICKDRAW
{
   CGrafPtr cgptr;
   int depth, rowBytes;
   long size;
   Rect bounds;
   Ptr myBits;
   
   SetRect (&bounds, 0, 0, width, height);
   OpenCPort (cgptr = canvsptr->drawable.xid = 
	      (CGrafPtr)(NewPtr(sizeof(CGrafPort))));
   depth = (**(*cgptr).portPixMap).pixelSize;
   rowBytes = (((depth * width) + 15) >> 4) << 1;
   size = (long) height * rowBytes;   
   if ((myBits = NewPtr(size)) == NULL) {
      SRGP__error (ERR_MALLOC);
      return (-1);
   }
   (**(*cgptr).portPixMap).baseAddr = myBits;
   (**(*cgptr).portPixMap).rowBytes = rowBytes + 0x8000;
   (**(*cgptr).portPixMap).bounds = bounds;
}

#else

{
   GrafPtr cgptr;
   int rowBytes;
   long memreq;
   Rect bounds;
   BitMap newBitMap;
   Ptr myBits;

   SetRect (&bounds, 0, 0, width, height);
   OpenPort (cgptr = canvsptr->drawable.xid = 
	     (GrafPtr)(NewPtr(sizeof(GrafPort))));
   newBitMap.rowBytes = (width>>3);
   if ((width % 8) > 0)
      newBitMap.rowBytes ++;
   if ((rowBytes % 2) > 0) 
      newBitMap.rowBytes++;
   SetRect (&newBitMap.bounds, 0, 0, width, height);
   memreq = newBitMap.rowBytes * height;
   if ((newBitMap.baseAddr = NewPtr(memreq)) == NULL) {
      SRGP__error (ERR_MALLOC);
      return (-1);
   }
   else {
      SetPortBits (&newBitMap);
      thePort->portRect = newBitMap.bounds;
   }
}
#endif
#endif

   /* SET VARIOUS IMPORTANT FIELDS IN THE CANVAS SPEC. */
   canvsptr->max_xcoord = width-1;
   canvsptr->max_ycoord = height-1;


#ifdef X11
   canvsptr->gc_fill =
      XCreateGC (srgpx__display, canvsptr->drawable.bitmap, 0L, NULL);
   canvsptr->gc_frame =
      XCreateGC (srgpx__display, canvsptr->drawable.bitmap, 0L, NULL);
#endif


   /* TELL THE WORLD TO PREPARE TO OUTPUT TO NEW BITMAP */
   PUSH_TRACE;
   SRGP_useCanvas (new_canvas_id);
   SRGP__setCanvasDefaults();
   POP_TRACE;

   return new_canvas_id;
}




/*!*/
void
SRGP_deleteCanvas (canvasID canvas_id)
{
   DEBUG_AIDS {
      SRGP_trace (SRGP_logStream, "SRGP_deleteCanvas: %d\n", canvas_id);
      srgp_check_system_state();
      srgp_check_extant_canvas(canvas_id);
      LeaveIfNonFatalErr();
   }

   if (canvas_id == SCREEN_CANVAS)
      SRGP__error (ERR_DELETION_OF_SCREEN);
   if (canvas_id == srgp__curActiveCanvasId)
      SRGP__error (ERR_DELETION_OF_ACTIVE_CANVAS);
   LeaveIfNonFatalErr();

#ifdef X11
   XFreePixmap (srgpx__display, srgp__canvasTable[canvas_id].drawable.bitmap);
#endif

#ifdef THINK_C
#ifdef COLOR_QUICKDRAW
#  define canvasGrafport (*((CGrafPtr)(srgp__canvasTable[canvas_id].drawable.xid)))
   DisposPtr ((**(canvasGrafport.portPixMap)).baseAddr);
#else
#  define canvasGrafport (*((GrafPtr)(srgp__canvasTable[canvas_id].drawable.xid)))
   DisposPtr (canvasGrafport.portBits.baseAddr);
#endif
   ClosePort (srgp__canvasTable[canvas_id].drawable.xid);
   DisposPtr (srgp__canvasTable[canvas_id].drawable.xid);
#endif

   srgp__canvasTable[canvas_id].drawable.bitmap = 0; /* FREES THE TABLE ENTRY */
}

