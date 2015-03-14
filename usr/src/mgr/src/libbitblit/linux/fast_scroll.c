#include <string.h>

#include "screen.h"

#define BYTESWIDE(x)	((x->primary->wide+7)>>3)

void fast_scroll(map,x,y,wide,high,delta) BITMAP *map; int x,y,wide,high,delta;
{
  register char *dst = (unsigned char *) ((long)(map->data)+(y*BYTESWIDE(map) + (x>>3)));
  register char *src = dst + delta*BYTESWIDE(map);
  register long count = wide>>3;
  register long skip = BYTESWIDE(map)-count;
  register long ncount = high - delta;

/*{{{}}}*/
  /*{{{  old code to pick the right bitplanes*/
#  if 0
  write_mode(1);
  setmapmask(0x0f);
#  endif
  /*}}}  */
  while (ncount--)
  {
#    ifdef NEED_ADJUST
    if IS_SCREEN(map) memcpy(adjust(dst),adjust(src),count);
    else memcpy(dst,src,count);
#    else
    memcpy(dst,src,count);
#    endif
    dst+=(count+skip);
    src+=(count+skip);
  }
  /*{{{   old code to pick the right bitplanes*/
#  if 0
  write_mode(0);
#  endif
  /*}}}  */
}
