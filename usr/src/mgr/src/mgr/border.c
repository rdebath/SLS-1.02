/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* draw a border around a window  */

#include "bitblit.h"

#include "defs.h"

void border(win,b,w)
WINDOW  *win;			/* window */
int b, w;			/* black width, white width */
   {
	register int clr,set;
	register BITMAP *bdr;

   clr = PUTOP(BIT_NOT(BIT_SRC),W(style));
   set = PUTOP(BIT_SRC,W(style));

	if (W(flags)&W_ACTIVE)
		bdr = W(border);
	else
		bdr = W(save);

   bit_blit(bdr,0,0,BIT_WIDE(bdr),(b+w),clr,NULL_DATA,0,0);
   bit_blit(bdr,BIT_WIDE(bdr)-(b+w),0,(b+w),BIT_HIGH(bdr),clr,NULL_DATA,0,0);
   bit_blit(bdr,0,BIT_HIGH(bdr)-(b+w),BIT_WIDE(bdr),(b+w),clr,NULL_DATA,0,0);
   bit_blit(bdr,0,0,(b+w),BIT_HIGH(bdr),clr,NULL_DATA,0,0);

   bit_blit(bdr,b,0,BIT_WIDE(bdr)-2*b,b,set,NULL_DATA,0,0);
   bit_blit(bdr,BIT_WIDE(bdr)-b,0,b,BIT_HIGH(bdr),set,NULL_DATA,0,0);
   bit_blit(bdr,b,BIT_HIGH(bdr)-b,BIT_WIDE(bdr)-2*b,b,set,NULL_DATA,0,0);
   bit_blit(bdr,0,0,b,BIT_HIGH(bdr),set,NULL_DATA,0,0);
   }
