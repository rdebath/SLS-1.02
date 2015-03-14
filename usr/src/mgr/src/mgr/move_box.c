/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* drag a box around the screen */

/*{{{  #includes*/
#include "bitblit.h"

#include "defs.h"

#include "get_rect.h"
#include "mouse_get.h"
/*}}}  */

/*{{{}}}*/
/*{{{  move_box*/
void move_box(screen,mouse,x,y,dx,dy,how)
BITMAP *screen;			/* where to sweep out the box */
int mouse;			/* file to get mouse coords from */
register int *x,*y;		/* starting position */
register int dx,dy;             /* box size */
int how;					/* termination condition */
   {
   int x_mouse, y_mouse;
   register int button, skip=1;

   box(screen,*x,*y,dx,dy);
   do {
      button=mouse_get(mouse,&x_mouse,&y_mouse);
      if (skip)
         box(screen,*x,*y,dx,dy);
      *x += x_mouse;
      *y -= y_mouse;
      *x = BETWEEN(0,*x,BIT_WIDE(screen)-dx);
      *y = BETWEEN(0,*y,BIT_HIGH(screen)-dy);
      if (skip = !mouse_count())
         box(screen,*x,*y,dx,dy);
      }
	while (how ? button!=0 : button==0); 

   if (skip)
      box(screen,*x,*y,dx,dy);
   }
/*}}}  */
