/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
*/

/* re-shape a window */
/*}}}  */

/*{{{  #includes*/
#include <stdio.h>

#include "bitblit.h"
#include "font.h"

#include "defs.h"
#include "event.h"

#include "border.h"
#include "do_button.h"
#include "do_event.h"
#include "erase_win.h"
#include "font_subs.h"
#include "get_rect.h"
#include "icon_server.h"
#include "scroll.h"
/*}}}  */

/*{{{  shape_window -- reshape a window with the mouse */
int shape_window()
{
  int dx=16 ,dy=16 ;

  SETMOUSEICON(&mouse_box);
  move_mouse(screen,mouse,&mousex,&mousey,0);
  SETMOUSEICON(&mouse_arrow);
  get_rect(screen,mouse,mousex,mousey,&dx,&dy,0);
  do_button(0);

  /* look for shape event here */
  do_event(EVENT_SHAPE,active,E_MAIN);

  return(shape(mousex,mousey,dx,dy));
}
/*}}}  */
/*{{{  shape -- reshape a window to specified dimentions*/
int
shape(x,y,dx,dy)
int x,y,dx,dy;
{
  int sx,sy,w,h;
  register WINDOW *win;

  if (dx>0)
  {
    sx= x; w = dx;
  }
  else 
  {
    sx= x+dx; w = -dx;
  }
  if (dy>0) 
  {
    sy= y; h = dy;
  }
  else 
  {
    sy= y+dy; h = -dy;
  }

  if (sx < 0) sx = 0;
   
  if (sx + w >= BIT_WIDE(screen))
  w = BIT_WIDE(screen) - sx;

  if (sy + h >= BIT_HIGH(screen))
  h = BIT_HIGH(screen) - sy;

  if (w < SUM_BDR + ACTIVE(font)->head.wide*MIN_X +1 ||
  h < SUM_BDR + ACTIVE(font)->head.high*MIN_Y +1)
  return(-1);

#ifdef ALIGN
  alignwin(screen,&sx,&w,SUM_BDR);
#endif

  /* remove current window position */
  save_win(active);
  erase_win(ACTIVE(border));
  clip_bad(active);	/* invalidate clip lists */

  /* redraw remaining windows */
  repair(active);

  /* adjust window state */
  ACTIVE(x0) = sx;
  ACTIVE(y0) = sy;
  bit_destroy(ACTIVE(window));
  bit_destroy(ACTIVE(border));
  ACTIVE(border) = bit_create(screen,sx,sy,w,h);
  ACTIVE(window) = bit_create(ACTIVE(border),
  SUM_BDR,SUM_BDR,w-SUM_BDR*2,h-SUM_BDR*2);

  for(win=ACTIVE(next);win != (WINDOW *) 0;win=W(next)) 
  {
    if (W(flags)&W_ACTIVE && intersect(active,win))
    save_win(win);
  }

  CLEAR(ACTIVE(window),PUTOP(BIT_NOT(BIT_SRC),ACTIVE(style)));

  border(active,BLK_BDR,WH_BDR);
  bit_blit(ACTIVE(border),0,0,BIT_WIDE(ACTIVE(save))-SUM_BDR,
  BIT_HIGH(ACTIVE(save))-SUM_BDR,BIT_SRC,ACTIVE(save),0,0);

  /* make sure character cursor is in a good spot */
  if (ACTIVE(x) > BIT_WIDE(ACTIVE(window))) 
  {
    ACTIVE(x) = 0;
    ACTIVE(y) += ((int)(ACTIVE(font)->head.high));
  }
  if (ACTIVE(y) > BIT_HIGH(ACTIVE(window))) 
  {
#ifdef WIERD
    ACTIVE(y) = BIT_HIGH(ACTIVE(window));
    scroll(ACTIVE(window),0,BIT_HIGH(ACTIVE(window)),
    ((int)(ACTIVE(font)->head.high)),SWAPCOLOR(ACTIVE(style)));
    bit_blit(ACTIVE(window),0,BIT_HIGH(ACTIVE(window))-((int)(ACTIVE(font)->head.high)),
    BIT_WIDE(ACTIVE(save)),((int)(ACTIVE(font)->head.high)),BIT_SRC,
    ACTIVE(save),SUM_BDR,BIT_HIGH(ACTIVE(save))-((int)(ACTIVE(font)->head.high))-SUM_BDR);
#else
    ACTIVE(y) = BIT_HIGH(ACTIVE(window))-((int)(ACTIVE(font)->head.high));
#endif
  }

  bit_destroy(ACTIVE(save));
  ACTIVE(save)=(BITMAP *)0;

  /* invalidate clip lists */
  clip_bad(active);
  un_covered();
  set_size(active);
  return(0);
}
/*}}}  */
