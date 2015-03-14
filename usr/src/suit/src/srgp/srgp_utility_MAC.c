#include "HEADERS.h"
#include "srgplocal.h"

Rect FIXED_RECT (int lx, int by, int rx, int ty)
{
   Rect arect;
   
   arect.left = lx;
   arect.top = srgp__curActiveCanvasSpec.max_ycoord - ty;
   arect.right = rx + 1;
   arect.bottom = srgp__curActiveCanvasSpec.max_ycoord - by + 1;
   return arect;
}

