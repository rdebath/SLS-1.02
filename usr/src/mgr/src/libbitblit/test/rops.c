#include <stdio.h>
#include "bitmap.h"

int main(argc,argv) int argc; char **argv;
{
  register BITMAP *screen;
  register int x,y,w,h,op,xs,ys;
  int maxx, maxy;
  int min,max;
  int count;

  if (argc < 3)
  {
    printf("usage: %s min_size max_size count\n",*argv);
    exit(1);
  }

  screen = bit_open(SCREEN_DEV);

  min = atoi(argv[1]);
  max = atoi(argv[2]);
  count = atoi(argv[3]);

  if (min >= max)
  max = min +1;
  maxx = max-min;
  maxy = max-min;

  while (count-- > 0) 
  {
    op = rand()&15;
    w = min + rand()%maxx;
    h = min + rand()%maxy;
    x = rand()%(BIT_WIDE(screen)-w);
    y = rand()%(BIT_HIGH(screen)-h);
    xs = rand()%(BIT_WIDE(screen)-w);
    ys= rand()%(BIT_HIGH(screen)-h);
    bit_blit(screen,x,y,w,h,op,screen,xs,ys);
  }
  bit_destroy(screen);
}
