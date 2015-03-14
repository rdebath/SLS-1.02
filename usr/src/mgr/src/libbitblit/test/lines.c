#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#include "bitblit.h"
#ifdef MOVIE
#include "share.h"
#endif

int main(int argc, char *argv[])
{
  BITMAP *bp;
  int i;

#ifdef MOVIE
  log_start(fopen("lines.log","w"));
#endif
  bp=bit_open(SCREEN_DEV);
  bit_blit(bp,0,0,BIT_WIDE(bp),BIT_HIGH(bp),BIT_CLR,(BITMAP*)0,0,0);
  srand(getpid()*time((time_t*)0));
  for (i=0; i<1000; i++)
  bit_line(bp,rand()%BIT_WIDE(bp),rand()%BIT_HIGH(bp),rand()%BIT_WIDE(bp),rand()%BIT_HIGH(bp),BIT_SET);
  bit_destroy(bp);
#ifdef MOVIE
  log_end();
#endif
  exit(0);
}
