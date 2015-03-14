/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* Create a new window */
/*}}}  */
/*{{{  #includes*/
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "bitblit.h"
#include "font.h"

#include "clip.h"
#include "defs.h"
#include "menu.h"

#include "Write.h"
#include "border.h"
#include "do_button.h"
#include "font_subs.h"
#include "get_font.h"
#include "get_rect.h"
#include "getshell.h"
#include "icon_server.h"
#include "put_window.h"
/*}}}  */

/*{{{  insert_win -- insert a new window into the window list*/
WINDOW *
insert_win(win)
WINDOW *win;
   {
   if (win == (WINDOW *) 0 &&
           (win = (WINDOW *) malloc(sizeof(WINDOW))) == (WINDOW *) 0) {
      if( debug )
	 fprintf(stderr,"Can't malloc window space\n");
      return(win);
      }

   if (active) {
      W(prev) = ACTIVE(prev);
      ACTIVE(prev) = win;
      W(next) = active;
      }
   else {
      W(prev) = win;
      W(next) = (WINDOW *) 0;
      }
   return(win);
   }
/*}}}  */
/*{{{  check_window -- check window size*/
int
check_window(x,y,dx,dy,fnt)
int x, y, dx, dy;
int fnt;
   {
   struct font *curr_font;

   if (dx<0)
      x += dx, dx = -dx;
   if (dy<0)
      y += dy, dy = -dy;

   if (x >= BIT_WIDE(screen) || y >= BIT_HIGH(screen))
       return(0);

   if (x + dx >= BIT_WIDE(screen))
      dx = BIT_WIDE(screen)-x;

   if (y + dy >= BIT_HIGH(screen))
      dy = BIT_HIGH(screen)-y;

   curr_font = Get_font(fnt);

#ifdef DEBUG
   dprintf(n)(stderr,"starting: (%d,%d)  %d x %d\r\n",x,y,dx,dy);
#endif

   if (dx < SUM_BDR + curr_font->head.wide*MIN_X +1 ||
              dy < SUM_BDR + curr_font->head.high*MIN_Y +1)
      return(0);
   else
      return(1);
   }
/*}}}  */
/*{{{  new_windowset_id -- Look through all the windows for the next available window set id.*/
int
next_windowset_id()
   {
      char		list[ MAXWIN + 2 ];
      register char	*cp;
      register WINDOW	*win;

      for( cp = list;  cp < &list[ MAXWIN + 2 ];  cp++ )
	 *cp = 0;

      for( win = active;  win != (WINDOW *)0;  win = W(next) )
	 list[ W(setid) ] = 1;

      /*	There is no window set ID zero.
      */
      for( cp = list + 1;  *cp;  cp++ )
	 ;

      return cp - list;
   }
/*}}}  */
/*{{{  setup_window -- initialize window state*/
int
setup_window(win,curr_font,x,y,dx,dy)
register WINDOW *win;
int x,y,dx,dy;
struct font *curr_font;
   {
   register int i;

#ifdef ALIGN
   alignwin(screen,&x,&dx,SUM_BDR);
#endif

   W(font) = curr_font;
   W(x) = 0;
   W(y) = curr_font->head.high;
   W(esc_cnt) = 0;
   W(esc[0]) = 0;
   W(flags) = W_ACTIVE | init_flags;
#ifdef CUT
   W(flags) |= W_SNARFABLE;
#endif
   W(style) = BIT_SRC;
	W(curs_type) = CS_BLOCK;
   W(x0) = x;
   W(y0) = y;
   W(border) = bit_create(screen,x,y,dx,dy);
   W(window) = bit_create(W(border),SUM_BDR,SUM_BDR,dx-SUM_BDR*2,dy-SUM_BDR*2);

   W(text.x) = 0;
   W(text.y) = 0;
   W(text.wide) = 0;
   W(text.high) = 0;

   W(bitmap) = (BITMAP *) 0;
   for(i=0;i<MAXBITMAPS;i++)
      W(bitmaps)[i] = (BITMAP *) 0;

   W(save) = (BITMAP *) 0;
   W(stack) = (WINDOW *) 0;
   W(main) = win;
   W(alt) = (WINDOW *) 0;
   W(esc_cnt) = 0;
   W(esc[0])=0;
   W(clip_list) = (char *) 0;

   for(i=0;i<MAXMENU;i++)
      W(menus[i]) = (struct menu_state *) 0;

   W(menu[0]) = W(menu[1]) = -1;
   W(event_mask) = 0;

   for(i=0;i<MAXEVENTS;i++)
      W(events)[i] = (char *) 0;

   W(snarf) = (char *) 0;
   W(gx) = 0;
   W(gy) = 0;
   W(op) = BIT_OR;
   W(max) = 0;
   W(current) = 0;
   strcpy(W(tty), last_tty());
   W(num) = 0;
   clip_bad(win);	/* invalidate clip lists */
   return(W(border) && W(window));
   }
/*}}}  */
/*{{{  make_window -- draw the window on the screen*/
int make_window(screen,x,y,dx,dy,fnt,start)
BITMAP *screen;
int x, y, dx, dy;
int fnt;
char *start;
   {
   register WINDOW *win = active;
   struct font *curr_font;

   if (dx<0)
      x += dx, dx = -dx;
   if (dy<0)
      y += dy, dy = -dy;

   if (x < 0) x = 0;

   if (x + dx >= BIT_WIDE(screen))
      dx = BIT_WIDE(screen)-x;

   if (y + dy >= BIT_HIGH(screen))
      dy = BIT_HIGH(screen)-y;

   curr_font = Get_font(fnt);
   if (curr_font == font) {
#ifdef DEBUG
      dprintf(n)(stderr,"Can't find font %d, using default\r\n", fnt);
#endif
      }

#ifdef DEBUG
   dprintf(n)(stderr,"starting window: (%d,%d)  %d x %d font (%d,%d)\r\n",
             x,y,dx,dy,curr_font->head.wide, curr_font->head.high);
   dprintf(n)(stderr,"min size: %d x %d\r\n",
           SUM_BDR + curr_font->head.wide*MIN_X +1,
           SUM_BDR + curr_font->head.high*MIN_Y +1);
#endif

   if (dx < SUM_BDR + curr_font->head.wide*MIN_X +1 ||
       dy < SUM_BDR + curr_font->head.high*MIN_Y +1)
       return(-1);

#ifdef DEBUG
   dprintf(n)(stderr,"adjusted to: (%d,%d)  %d x %d\r\n",x,y,dx,dy);
#endif

   if (!setup_window(win,curr_font,x,y,dx,dy)) {
      fprintf(stderr,"Out of memory for window creation -- bye!\n");
      quit();
      }

   next_window++;

   /* make the window */

   set_covered(win);
   border(win,BLK_BDR,WH_BDR);
   CLEAR(W(window),PUTOP(BIT_NOT(BIT_SRC),W(style)));

   /* set up file descriptor modes */

#ifndef FNDELAY
   if (fcntl(W(from_fd),F_SETFL,fcntl(W(from_fd),F_GETFL,0)|O_NDELAY) == -1)
#else
   if (fcntl(W(from_fd),F_SETFL,fcntl(W(from_fd),F_GETFL,0)|FNDELAY) == -1)
#endif
      fprintf(stderr,"%s: fcntl failed for fd %d\n",W(tty),W(from_fd));

   mask |= 1<<W(to_fd);
	set_size(win);

   /* send initial string (if any) */

   if (start && *start) {
#ifdef DEBUG
      dprintf(n)(stderr,"Sending initial string: [%s]\n",start);
#endif
      Write(W(to_fd),start,strlen(start));
      }
   return(0);
   }
/*}}}  */
/*{{{  create_window -- create a new window given coords*/
int
create_window(x,y,dx,dy,font_num,argv)
int x,y,dx,dy;
int font_num;
char **argv;
   {
   register WINDOW * win;

   if (next_window >= MAXWIN)
       return(-1);
   if (check_window(x,y,dx,dy,font_num) == 0)
      return(-1);

   /* alloc window space */

   if ((win = (WINDOW *) malloc(sizeof(WINDOW))) == (WINDOW *) 0) {
      fprintf(stderr,"Can't malloc window space\n");
      return(-1);
      }

   if ((W(pid) = get_command(argv,&W(from_fd))) < 0) {
      free(win);
      fprintf(stderr,"mgr: Can't get a pty\n");
      return(-1);
      }
   W(to_fd) = W(from_fd);
   W(setid) = next_windowset_id();

   active = insert_win(win);

   make_window(screen,x,y,dx,dy,font_num,"");
   return(0);
   }
/*}}}  */
/*{{{  half_window -- create a new window given coords, with only 1/2 a ptty*/
char *
half_window(x,y,dx,dy,font_num)
int x,y,dx,dy;
int font_num;
   {
   register WINDOW * win;
   char *tty;

   if (next_window >= MAXWIN)
       return(NULL);
   if (check_window(x,y,dx,dy,font_num) == 0)
      return(NULL);

   /* alloc window space */

   if ((win = (WINDOW *) malloc(sizeof(WINDOW))) == (WINDOW *) 0) {
      fprintf(stderr,"Can't malloc window space\n");
      return(NULL);
      }

   if ((tty = half_open(&W(from_fd))) == NULL) {
      free(win);
      fprintf(stderr,"Can't get a pty\n");
      return(NULL);
      }

   W(setid) = next_windowset_id();
   active = insert_win(win);

   W(to_fd) = W(from_fd);
   make_window(screen,x,y,dx,dy,font_num,"");
   W(pid) = 1;
   W(flags) |= W_NOKILL;

   return(tty);
   }
/*}}}  */
/*{{{  new_window -- sweep out a new window*/
int new_window()
   {
   int dx=16,dy=16;

   if (next_window >= MAXWIN)
       return(-1);
   SETMOUSEICON(&mouse_box);
   move_mouse(screen,mouse,&mousex,&mousey,0);
   SETMOUSEICON(&mouse_arrow);
   get_rect(screen,mouse,mousex,mousey,&dx,&dy,0);
   do_button(0);

   return( create_window( mousex, mousey, dx, dy, -1, 0 ) );
   }
/*}}}  */
