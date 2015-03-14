#include "HEADERS.h"
#include "srgplocal.h"

#ifdef THINK_C
static Rect r;
#endif

int srgp__marker_radius;

void SRGP__drawSquareMarker (int x, int y)
{
#ifdef X11
   XDrawRectangle
      (srgpx__display,
       srgp__curActiveCanvasSpec.drawable.xid,
       srgp__curActiveCanvasSpec.gc_frame,
       x-srgp__marker_radius, FIXED(y)-srgp__marker_radius,
       srgp__curActiveCanvasSpec.attributes.marker_size,
       srgp__curActiveCanvasSpec.attributes.marker_size);
#endif
#ifdef THINK_C
   SetRect (&r, x-srgp__marker_radius, FIXED(y)-srgp__marker_radius, 
                x+srgp__marker_radius, FIXED(y)+srgp__marker_radius);
   FrameRect (&r);
#endif
   
}

void SRGP__drawCircleMarker (int x, int y)
{
#ifdef X11
   XDrawArc
      (srgpx__display,
       srgp__curActiveCanvasSpec.drawable.xid,
       srgp__curActiveCanvasSpec.gc_frame,
       x-srgp__marker_radius, FIXED(y)-srgp__marker_radius,
       srgp__curActiveCanvasSpec.attributes.marker_size,
       srgp__curActiveCanvasSpec.attributes.marker_size,
       0, 23040);  /* anglestart=0; anglesweep=360 */
#endif
#ifdef THINK_C
   SetRect (&r, x-srgp__marker_radius, FIXED(y)-srgp__marker_radius, 
                x+srgp__marker_radius, FIXED(y)+srgp__marker_radius);
   FrameOval (&r);
#endif
}

void SRGP__drawXMarker (int x, int y)
{
   int lx, rx, ty, by;  /* IN LOW-LEVEL COORD SYS */

   lx = x - (srgp__marker_radius>>1);
   rx = lx + srgp__curActiveCanvasSpec.attributes.marker_size;
   ty = FIXED(y) - (srgp__marker_radius>>1);
   by = ty + srgp__curActiveCanvasSpec.attributes.marker_size;
   
#ifdef X11
   XDrawLine
      (srgpx__display,
       srgp__curActiveCanvasSpec.drawable.xid,
       srgp__curActiveCanvasSpec.gc_frame,
       lx,ty, rx,by);
   XDrawLine
      (srgpx__display,
       srgp__curActiveCanvasSpec.drawable.xid,
       srgp__curActiveCanvasSpec.gc_frame,
       lx,by, rx,ty);
#endif

#ifdef THINK_C 
	MoveTo (lx,ty);
	LineTo (rx,by);
	MoveTo (lx,by);
	LineTo (rx,ty);
#endif
}

