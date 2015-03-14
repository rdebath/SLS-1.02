#include "HEADERS.h"
#include "srgplocal.h"

#ifdef THINK_C
#include "ChooseWhichQuickdraw.h"
#endif

#define DFIXED(yy)\
(srgp__curActiveCanvasSpec.max_ycoord - (yy))
#define SFIXED(yy)\
(srgp__canvasTable[source_canvas_id].max_ycoord - (yy))


void
SRGP_copyPixel (canvasID source_canvas_id, 
		rectangle source_rect, 
		point dest_corner)
{
   register int height;

   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_copyPixel: %d (%d,%d)->(%d,%d) -----> (%d,%d)\n",
		  source_canvas_id, ExplodeRect(source_rect), ExplodePt(dest_corner));
      srgp_check_system_state();
      srgp_check_extant_canvas (source_canvas_id);
      srgp_check_rectangle
	 (source_rect.bottom_left.x, source_rect.bottom_left.y,
	  source_rect.top_right.x, source_rect.top_right.y);
      LeaveIfNonFatalErr();
   }


#ifdef X11
   height = (source_rect.top_right.y - source_rect.bottom_left.y + 1);
   XCopyArea (srgpx__display,
	      srgp__canvasTable[source_canvas_id].drawable.bitmap,
	      srgp__curActiveCanvasSpec.drawable.bitmap,
	      srgp__curActiveCanvasSpec.gc_fill,
	      source_rect.bottom_left.x, SFIXED(source_rect.top_right.y),
	      (source_rect.top_right.x - source_rect.bottom_left.x + 1),
	      height,
	      dest_corner.x, DFIXED(dest_corner.y + height - 1));
#endif

#ifdef THINK_C
{
   Rect sRect, dRect;
   int modeTable[] = {srcCopy, srcXor, srcOr, notSrcBic};
#ifdef COLOR_QUICKDRAW
   CGrafPtr curcgport = (CGrafPtr)thePort;
   long savefgcolor, savebgcolor;
#endif
   
   
   SetRect (&sRect, source_rect.bottom_left.x, SFIXED(source_rect.top_right.y), 
	   source_rect.top_right.x+1, SFIXED(source_rect.bottom_left.y) + 1);
   SetRect(&dRect, 
	   dest_corner.x, 
	   DFIXED(dest_corner.y + source_rect.top_right.y - source_rect.bottom_left.y),
	   dest_corner.x + source_rect.top_right.x - source_rect.bottom_left.x + 1, 
	   DFIXED(dest_corner.y) + 1);

#ifdef COLOR_QUICKDRAW	   
   savefgcolor = thePort->fgColor; savebgcolor = thePort->bkColor;
   thePort->fgColor = 255;  thePort->bkColor = 0;  /*THIS IS DANGEROUSLY STUPID*/
   
#  define canvasGrafport(i) (*((CGrafPtr)(srgp__canvasTable[i].drawable.xid)))
   CopyBits (*(canvasGrafport(source_canvas_id).portPixMap), 
	     *(canvasGrafport(srgp__curActiveCanvasId).portPixMap), 
		     &sRect, &dRect, 
		     modeTable[srgp__curActiveCanvasSpec.attributes.write_mode], NULL);

   thePort->fgColor = savefgcolor;  thePort->bkColor = savebgcolor;
#else
#  define canvasGrafport(i) (*((GrafPtr)(srgp__canvasTable[i].drawable.xid)))
   CopyBits (&(canvasGrafport(source_canvas_id).portBits), 
	     &(canvasGrafport(srgp__curActiveCanvasId).portBits), 
		     &sRect, &dRect, 
		     modeTable[srgp__curActiveCanvasSpec.attributes.write_mode], NULL);

#endif
}
#endif

}
