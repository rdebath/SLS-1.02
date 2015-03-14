#include <string.h>

#include "sun.h"

#define BYTESWIDE(x)	((x->primary->wide+7)>>3)

void fast_scroll(map,x,y,wide,high,delta)
     BITMAP *map;
     int x,y,wide,high,delta;
{
  register char *dst = (unsigned char *) ((long)(map->data)+(y*BYTESWIDE(map) + (x>>3)));
  register char *src = dst + delta*BYTESWIDE(map);
  register long count = wide>>3;
  register long skip = BYTESWIDE(map)-count;
  register long ncount = high - delta;
  register long i;

  while (ncount--) {
    memcpy(dst,src,count);
    dst+=(count+skip);
    src+=(count+skip);
  }
}
