/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* see if two windows intersect */
/*}}}  */

/*{{{  #includes*/
#include <stdio.h>

#include "bitblit.h"

#include "defs.h"
/*}}}  */

/*{{{  intersect*/
int intersect(win1,win2) register WINDOW *win1, *win2;
{
	return 
        (!(
	    win1->x0 + win1->BIT_WIDE(border) < win2->x0 ||
	    win2->x0 + win2->BIT_WIDE(border) < win1->x0 ||
	    win1->y0 + win1->BIT_HIGH(border) < win2->y0 ||
	    win2->y0 + win2->BIT_HIGH(border) < win1->y0
        ));
}
/*}}}  */
/*{{{  alone -- see if any window intersects any other*/
int
alone(check)
register WINDOW *check;
{
	register WINDOW *win;

        for(win=active;win != (WINDOW *) 0;win=win->next)
          if (check!=win && intersect(check,win))
             return(0);
        return(1);
}
/*}}}  */
/*{{{  mousein -- see if mouse is in window*/
int mousein(x,y,win,how)
register int x,y;
register WINDOW *win;
int how;		/* how:  0-> intersect   else -> point */
{
   if (how == 0)
	return(!(
	    x+16 < W(x0) || x > W(x0) + win->BIT_WIDE(border) ||
            y+16 < W(y0) || y > W(y0) + win->BIT_HIGH(border)
            ));
   else
	return(!(
	    x < W(x0) || x > W(x0) + win->BIT_WIDE(border) ||
            y < W(y0) || y > W(y0) + win->BIT_HIGH(border)
            ));
}
/*}}}  */
/*{{{  in_text -- see if mouse is in text region*/
int
in_text(x,y,win)
register int x,y;
register WINDOW *win;
   {
   if (W(text.wide)) {
      int x0 = W(x0)+W(text.x);
      int y0 = W(y0)+W(text.y);
      return(!(
	    x < x0 || x > x0 + W(text.wide) ||
            y < y0 || y > y0 + W(text.high)
            ));
      }
   else
      return(mousein(x,y,win,1));
   }
/*}}}  */
