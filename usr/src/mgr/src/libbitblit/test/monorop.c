/* test small bit_blit functions */

#include <stdio.h>

#include "bitblit.h"

#define SIZE		32

int main(argc,argv) int argc; char **argv;
{
  register BITMAP *screen;
  BITMAP *src,*dst;
  int wait=1;
  int x0;
  register int i;

  screen = bit_open(SCREEN_DEV);

  /* make src and dst */

  src = bit_alloc(SIZE,SIZE,0,1);
  dst = bit_alloc(SIZE,SIZE,0,1);

  /* Make src and dst */

  bit_blit(src,0,0,SIZE/2,SIZE,BIT_CLR,0,0,0);
  bit_blit(src,SIZE/2,0,SIZE/2,SIZE,BIT_SET,0,0,0);

  bit_blit(dst,0,0,SIZE,SIZE/2,BIT_CLR,0,0,0);
  bit_blit(dst,0,SIZE/2,SIZE,SIZE/2,BIT_SET,0,0,0);

  /* test 16 bitmem_rop functions */

  /* 16 borders */
  for(i=0;i<16;i++) 
  {
    x0 = i*(SIZE+10);
    bit_blit(screen,10+x0%(screen->high),100+(SIZE+10)*(x0/(screen->high)),SIZE,SIZE,BIT_SET,0,0,0);
    if (wait) sleep(1);
  }
  /* 16 dst patterns */
  for(i=0;i<16;i++) 
  {
    x0 = i*(SIZE+10);
    bit_blit(screen,12+x0%(screen->high),102+(SIZE+10)*(x0/(screen->high)),SIZE-4,SIZE-4,BIT_SRC,dst,2,2);
    if (wait) sleep(1);
  }
  /* 16 bit-blt functions */
  for(i=0;i<16;i++) 
  {
    x0 = i*(SIZE+10);
    bit_blit(screen,12+x0%(screen->high),102+(SIZE+10)*(x0/(screen->high)),SIZE-4,SIZE-4,i,src,2,2);
    if (wait) sleep(1);
  }
  bit_destroy(screen);
  exit(0);
}
