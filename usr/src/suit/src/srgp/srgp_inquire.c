#include "HEADERS.h"
#include "srgplocal.h"


Drawable SRGP_inquireXDrawable (canvasID canvas_id)
{
   return srgp__canvasTable[canvas_id].drawable.xid;
}


Display *SRGP_inquireXDisplay (void)
{
    return srgpx__display;
}


Window SRGP_inquireXWindow (void)
{
    return srgp__curActiveCanvasSpec.drawable.win;
}


void SRGP_inquireAttributes (attribute_group *att_group)
{
   *att_group = srgp__curActiveCanvasSpec.attributes;
}

rectangle SRGP_inquireClipRectangle (void)
{
   return srgp__curActiveCanvasSpec.attributes.clip_rectangle;
}   


canvasID SRGP_inquireActiveCanvas ()
{
   return srgp__curActiveCanvasId;
}


rectangle SRGP_inquireCanvasExtent (canvasID canvas_id)
{
   rectangle rect;

   rect.bottom_left.x = 0;  rect.bottom_left.y = 0;
   rect.top_right.x = srgp__canvasTable[canvas_id].max_xcoord;
   rect.top_right.y = srgp__canvasTable[canvas_id].max_ycoord;
   return rect;
}


void
SRGP_inquireCanvasSize (int canvas_id, int *width, int *height)
{
   *width = 1 + srgp__canvasTable[canvas_id].max_xcoord;
   *height = 1 + srgp__canvasTable[canvas_id].max_ycoord;
}


int
SRGP_inquireCanvasDepth ()
{
   return srgp__application_depth;
}
