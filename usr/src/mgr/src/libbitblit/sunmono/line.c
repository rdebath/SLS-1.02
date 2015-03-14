/*                        Copyright (c) 1989 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

#include "sun.h"
#include <stdio.h>

/*  Draw a line  - Bresenham method , portable Bitblt version (S. A. Uhler)
 */

void bit_line(dest, x0, y0, x1, y1, func)
register BITMAP *dest;				/* destination bitmap */	
int x0, y0, x1, y1;					/* line coordinates */
int func;								/* set, clear, or invert */
   {
   register unsigned bit;			/* bit offset in current word */
   register DATA *dst;				/* current word in bitmap */
	register int count;				/* current x position in loop */
	register int err;					/* accumulated error */
   register int d_incr;				/* words to next scan line */
   register int rincr, rdecr;
   int dx, dy;							/* # of pixels in x and y */
   int temp;

   /* clip here */

#ifndef NOCLIP

#define TOP	001
#define BOTTOM	002
#define LEFT	004
#define RIGHT	010
#define CROSS(x,y) \
	  (x<0 ? LEFT : x>= (dest->wide) ? RIGHT : 0) + \
	  (y < 0 ? TOP : y >=  (dest -> high) ? BOTTOM : 0)

      {

      /* The classic line clipping algorithm */
		/* (I don't remember anymore where I got it from, sorry -sau) */

      register int cross0 = CROSS(x0, y0);
      register int cross1 = CROSS(x1, y1);

      while (cross0 || cross1) {
	      int cross, x, y;
	      if (cross0 & cross1)
	         return;
	      if (cross0 != 0)
	         cross = cross0;
	      else
	         cross = cross1;
	      if (cross & (LEFT | RIGHT)) {
	         int edge = (cross & LEFT) ? 0 : dest->wide - 1;
	         y = y0 + (y1 - y0) * (edge - x0) / (x1 - x0);
	         x = edge;
	         }
	      else if (cross & (TOP | BOTTOM)) {
	         int edge = (cross & TOP) ? 0 : dest->high - 1;
	         x = x0 + (x1 - x0) * (edge - y0) / (y1 - y0);
	         y = edge;
	         }
	      if (cross == cross0) {
	         x0 = x;
	         y0 = y;
	         cross0 = CROSS(x, y);
	         }
	      else {
	         x1 = x;
	         y1 = y;
	         cross1 = CROSS(x, y);
	         }
         }
      }

   /* end of clipping */

#endif

   x0 += dest->x0;
   y0 += dest->y0;
   x1 += dest->x0;
   y1 += dest->y0;

   /* always draw left to right */

   if (x1 < x0) {
      temp = x1, x1 = x0, x0 = temp;
      temp = y1, y1 = y0, y0 = temp;
      }
   dx = x1 - x0;
   dy = y1 - y0;

   d_incr = BIT_LINE(dest);
   dst = y0 * d_incr + (x0>>LOGBITS) +  (dest->data);
   bit = GETLSB(MSB,(x0&BITS));

   if (dy <= 0)
      d_incr = -d_incr, dy = -dy;

#ifdef INVERT
	/* invert all raster ops */

	func = op_invert[15&func];
#endif

#define XMOVE if ((bit=GETLSB(bit,1))==0) {bit = MSB; dst++;}
#define YMOVE dst += d_incr

#define STEP(dx,dy,xmove,ymove,op) {		\
    rincr = (dx - dy)<<1;			\
    rdecr = -(dy<<1);				\
    err = dx + rdecr;				\
    for (count = dx; count >= 0; count--) {	\
        op;	    				\
        xmove;	    				\
        if (err < 0) {				\
            ymove;				\
            err += rincr;				\
            }					\
        else {					\
            err += rdecr;				\
            }					\
        } 					\
    }

   if (dx > dy) {			/* gentle slope (this could be made faster) */
      switch (OPCODE(func)) {
	      case OPCODE(SRC):
	      case OPCODE(SRC | DST):
	      case OPCODE(SRC | ~DST):
	      case OPCODE(~0):
    	      STEP(dx, dy, XMOVE, YMOVE, *dst |= bit);				/* set */
	         break;
	      case OPCODE(~SRC):
	      case OPCODE(~(SRC|DST)):
	      case OPCODE(DST & ~SRC):
	      case OPCODE(0):
    	      STEP(dx, dy, XMOVE, YMOVE, *dst &= ~bit);			/* clear */
	         break;
	      case OPCODE(SRC ^ DST):
	      case OPCODE(~DST):
	      case OPCODE(SRC & ~DST):
	      case OPCODE(~(SRC&DST)):
    	      STEP(dx, dy, XMOVE, YMOVE, *dst ^= bit);				/* invert */
	         break;
         }
      }
   else	{			/* steep slope */
      switch (OPCODE(func)) {
	      case OPCODE(SRC):
	      case OPCODE(SRC | DST):
	      case OPCODE(SRC | ~DST):
	      case OPCODE(~0):
    	      STEP(dy, dx, YMOVE, XMOVE, *dst |= bit);				/* set */
	         break;
	      case OPCODE(~SRC):
	      case OPCODE(~(SRC|DST)):
	      case OPCODE(DST & ~SRC):
	      case OPCODE(0):
    	      STEP(dy, dx, YMOVE, XMOVE, *dst &= ~bit);			/* clear */
	         break;
	      case OPCODE(SRC ^ DST):
	      case OPCODE(~DST):
	      case OPCODE(SRC & ~DST):
	      case OPCODE(~(SRC&DST)):
    	      STEP(dy, dx, YMOVE, XMOVE, *dst ^= bit);				/* invert */
	         break;
         }
	   }
   }
