/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* sweep out text rectangle */

/*{{{}}}*/
/*{{{  #includes*/
#include <stdio.h>

#include "bitblit.h"
#include "font.h"

#include "defs.h"
#include "event.h"

#include "font_subs.h"
#include "mouse_get.h"
/*}}}  */

/*{{{  #defines*/
#define FSIZE(c)	((int) (W(font)->head.c))
#define THICK	2

#define TOP	1
#define MIDDLE	2
#define BOTTOM	3
#define ALL	4
/*}}}  */

/*{{{  tbox -- draw a box*/
#define INVERT(screen,x,y,wide,high) \
	bit_blit(screen,x,y,wide,high,BIT_NOT(BIT_DST),NULL_DATA,0,0);

static void tbox(screen,x1,y1,dx,dy,side)
BITMAP *screen;
int x1,y1,dx,dy;
int side;
   {
   switch (side) {
      case TOP:
         INVERT(screen,x1,y1,dx,THICK);
         INVERT(screen,x1+dx,y1,THICK, dy);
         INVERT(screen,x1+THICK,y1+dy,dx-THICK,THICK);
         INVERT(screen,x1,y1+THICK,THICK, dy-THICK);
         break;
      case MIDDLE:
         INVERT(screen,x1+THICK,y1,dx-THICK,THICK);
         INVERT(screen,x1+dx,y1+THICK,THICK, dy-THICK);
         INVERT(screen,x1+THICK,y1+dy,dx-THICK,THICK);
         INVERT(screen,x1,y1+THICK,THICK, dy-THICK);
         break;
      case BOTTOM:
         INVERT(screen,x1+THICK,y1,dx-THICK,THICK);
         INVERT(screen,x1+dx,y1+THICK,THICK, dy-THICK);
         INVERT(screen,x1+THICK,y1+dy,dx,THICK);
         INVERT(screen,x1,y1+THICK,THICK, dy);
         break;
      case ALL:
         INVERT(screen,x1,y1,dx,THICK);
         INVERT(screen,x1+dx,y1,THICK, dy);
         INVERT(screen,x1+THICK,y1+dy,dx,THICK);
         INVERT(screen,x1,y1+THICK,THICK, dy);
         break;
      }
   }
/*}}}  */
/*{{{  do_box -- piece boxes*/
static int
do_box(screen,x1,y1,px,py,top,left,cols,rows,gx,gy)
BITMAP *screen;
int x1,y1;		/* starting pos in rows/cols */
int *px,*py;		/* ending delta in rows/cols */
int top,left;		/* start of window in pixels */
int cols,rows;		/* size of window */
int gx,gy;		/* character size (in pixels) */
   {
   register int dx = *px;
   register int dy = *py;

   if (dy < 0)
      dy = 0;

   if (dy == 0 && dx < 0)
      dx = 0;

   if (x1 + dx < 0)
      dx = -x1;

   if (x1+dx > cols)
      dx = cols-x1;

   if (y1+ dy >= rows)
      dy = rows-y1-1;

   switch(dy) {
      case 0:		/* 1 line */
         tbox(screen, left+x1*gx-1, top+y1*gy-2,   dx*gx,       gy+3, ALL);
         break;
      case 1:		/* two lines */
         tbox(screen, left+x1*gx-1, top+y1*gy-2,       (cols-x1)*gx,  gy+2, TOP);
         tbox(screen, left-1,       top + (y1+1)*gy, (x1+dx)*gx,    gy+1, BOTTOM);
         break;
      default:		/* many lines */
         tbox(screen, left+x1*gx-1, top + y1*gy-2,      (cols-x1)*gx, gy+2, TOP);
         tbox(screen, left-1,       top + (y1+1)*gy,  cols*gx,      (dy-1)*gy, MIDDLE);
         tbox(screen, left-1,       top + (y1+dy)*gy, (x1+dx)*gx,   gy+1, BOTTOM);
         break;
      }
   if (*px != dx || *py != dy) {
      *px = dx; *py = dy;
      return(1);
      }
   else
      return(0);
   }
/*}}}  */

/*{{{  get_text*/
int
get_text(screen,mouse,x,y,dx,dy,win,c)
BITMAP *screen;		/* where to sweep out the box */
int mouse;			/* file to get mouse coords from */
int x,y;			/* starting position */
register int *dx,*dy;		/* box width,height */
WINDOW *win;			/* text window */
int c;				/* E_SWTEXT or E_SWTEXTT */
   {
   register int button;
   int left,top;
   int cols,rows;
   int gx,gy;
   int x_mouse, y_mouse;
   int lastdx, lastdy;				/* previous dx,dy */
	int newx = *dx * FSIZE(wide);
	int newy = *dy * FSIZE(high);  
   rectangle text;

   /* set up text regions */

   if (c == E_SWTEXT)				/* no text region */
      text.x = text.y = text.wide = text.high = 0;
   else if (!in_text(x,y,win)) {
      button=move_mouse(screen,mouse,&mousex,&mousey,1);
      return(0);
      }
   else 
      text = W(text);

   left = W(x0) + SUM_BDR + text.x;	/* edge of window (pixels) */
   top = W(y0) + SUM_BDR + text.y;	/* top of window (pixels) */
   cols = (text.wide ? text.wide : BIT_WIDE(W(window)))/FSIZE(wide);
   rows = (text.wide ? text.high : BIT_HIGH(W(window)))/FSIZE(high);
   gx = FSIZE(wide);			/* char width (pixels) */
   gy = FSIZE(high);			/* char height (pixels) */

   x = (x-left)/gx;
   y = (y-top)/gy;

   do_box(screen,x,y,dx,dy,top,left,cols,rows,gx,gy);	 /* on */
   do {
      button=mouse_get(mouse,&x_mouse,&y_mouse);
      newx += x_mouse<<1;
      newy -= y_mouse<<1;
      lastdx = *dx, *dx = newx/gx;
      lastdy = *dy ,*dy = newy/gy;
      if (lastdx != *dx || lastdy != *dy) {
         do_box(screen,x,y,&lastdx,&lastdy,top,left,cols,rows,gx,gy);	/* off*/
         if (do_box(screen,x,y,dx,   dy,   top,left,cols,rows,gx,gy))	/* on */
            newx = gx * *dx,  newy = gy * *dy;
         }
      }
   while (button!=0);

   do_box(screen,x,y,dx,dy,top,left,cols,rows,gx,gy);			/* off*/
   return(1);
   }
/*}}}  */
