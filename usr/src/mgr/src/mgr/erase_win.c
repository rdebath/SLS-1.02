/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* erase a pixrect to background pattern */

/*{{{}}}*/
/*{{{  #includes*/
#include <stdio.h>

#include "bitblit.h"

#include "defs.h"

#include "icon_server.h"
/*}}}  */

/*{{{  Bit_pattern -- fill DST bitmap with SRC, preserving alignment*/
static void Bit_pattern(dst,dx,dy,wide,high,func,src,d1,d2)
register BITMAP *dst,*src;
register int dx,dy;
int wide,high;
int func;
int d1,d2;		/* unused */
   {
   register int incr;
   register int sw = BIT_WIDE(src);
   register int sh = BIT_HIGH(src);
   int x = BIT_X(dst) + dx;
   int y = BIT_Y(dst) + dy;
   int xdel = x % sw;
   int ydel = y % sh;
   int de;

   dx -= xdel, wide += xdel;
   de=dx+wide;

   /* get partial strip */

   if (ydel) {
      for(incr=dx;incr<de;incr+=sw)
         bit_blit(dst,incr,dy-ydel,sw,sh,func,src,0,0);
      dy += sh-ydel;
      }

   /* get 1st strip */

   for(incr=dx;incr<de;incr+=sw)
      bit_blit(dst,incr,dy,sw,sh,func,src,0,0);

   /* get the rest */

   de = dy+high;
   for(incr=dy+sh;incr<de;incr+=sh,sh<<=1)
      bit_blit(dst,dx,incr,wide,sh,func,dst,dx,dy);
   }
/*}}}  */

/*{{{  erase_win*/
void erase_win(map) BITMAP *map;
{
  Bit_pattern(map,0,0,BIT_WIDE(map),BIT_HIGH(map),
			BUILDOP(BIT_SRC,color_map[6],color_map[7]),
			&pattern);
}
/*}}}  */
