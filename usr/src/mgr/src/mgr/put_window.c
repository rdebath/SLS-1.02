/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* Terminal emulator */
/*}}}  */
/*{{{  #includes*/
#include <termios.h>
#include <sys/time.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#include "bitblit.h"
#include "font.h"

#include "clip.h"
#include "defs.h"
#include "event.h"
#include "menu.h"

#include "Write.h"
#include "border.h"
#include "do_event.h"
#include "do_menu.h"
#include "down_load.h"
#include "font_subs.h"
#include "get_font.h"
#include "get_info.h"
#include "get_menus.h"
#include "icon_server.h"
#include "intersect.h"
#include "scroll.h"
/*}}}  */
/*{{{  #defines*/
/* macros for putting a character on a window */

#define DO_CHAR(font,c) \
	((font)->glyph[c])

#define PUT_CHAR(dest,x,y,font,op,c)	\
	bit_blit(dest,x,y-fsizehigh,fsizewide,fsizehigh, \
	op, DO_CHAR(font,c),0,0)

/* fix the border color */

#define BORDER(win) \
   ((win==active) ? border(win,SUM_BDR-1,1) : border(win,BLK_BDR,WH_BDR))

#define BG_OP			PUTOP(BIT_NOT(BIT_SRC),W(style))	/* short hand */

/* flseep is experimental */

#define fsleep() \
   { \
   struct timeval time; \
   time.tv_sec = 0; \
   time.tv_usec = 330000; \
   select(0,0,0,0,&time); \
   }
/*}}}  */

/*{{{  variables*/
rect clip;				/* clipping rectangle */
/*}}}  */

/*{{{  set_winsize*/
static void set_winsize(fd,rows,cols,ypixel,xpixel) int fd,rows,cols,ypixel,xpixel;
{
  struct winsize size;

  size.ws_row=rows;
  size.ws_col=cols;
  size.ws_xpixel=xpixel;
  size.ws_ypixel=ypixel;
  ioctl(fd,TIOCSWINSZ,&size);
}
/*}}}  */
/*{{{  standout*/
static
standout( win )
register WINDOW *win;
{
	int	cnt;

	if (W(flags)&W_STANDOUT)
		return;

	if (BIT_DEPTH(W(window)) >1 && GETFCOLOR(~W(style)) != GETBCOLOR(W(style)))
		W(style)  = PUTFCOLOR(W(style),GETFCOLOR(~W(style)));
	else
		W(style)  = PUTOP(BIT_NOT(W(style)),W(style));
	W(flags) |= W_STANDOUT;
}
/*}}}  */
/*{{{  standend*/
static
standend( win )
register WINDOW *win;
{
	int	cnt;

	if (!(W(flags)&W_STANDOUT))
		return;

	if (BIT_DEPTH(W(window)) >1 && GETFCOLOR(~W(style)) != GETBCOLOR(W(style)))
		W(style)  = PUTFCOLOR(W(style),GETFCOLOR(~W(style)));
	else
		W(style)  = PUTOP(BIT_NOT(W(style)),W(style));
	W(flags) &= ~ W_STANDOUT;
}
/*}}}  */

/*{{{  set_size*/
/* set the kernel's idea of the screen size */

void set_size(win) WINDOW *win;
{
  if (win==(WINDOW*)0) return;

  if (W(text.wide) > 0)
  {
    set_winsize(ACTIVE(to_fd),W(text.high)/FSIZE(high),W(text.wide)/FSIZE(wide));
#    ifdef DEBUG
    dprintf(t)(stderr,"setting tty [text] %dx%d\n",W(text.wide),W(text.high));
#    endif
  }
  else
  {
    set_winsize(W(to_fd),BIT_HIGH(W(window))/FSIZE(high),BIT_WIDE(W(window))/FSIZE(wide));
#    ifdef DEBUG
    dprintf(t)(stderr,"setting tty %dx%d\n",BIT_HIGH(W(window))/FSIZE(high),BIT_WIDE(W(window))/FSIZE(wide));
#    endif
  }
}
/*}}}  */
/*{{{  put_window*/
/* send a string to a window, interpret ESC's
 * return # of chars actually processed
 */

int
put_window(win,buff,buff_count)
register WINDOW *win;					/* window to update */
register unsigned char *buff;					/* ptr to update text */
int buff_count;						/* # of characters */
   {
   /*{{{  variables*/
   register BITMAP *window;			/* bitmap to update */
   register BITMAP *text=(BITMAP*)0;		/* current text region */
   register int indx;				/* index into buff */
   register int cnt;				/* # of esc. numbers */
   register unsigned char c;			/* current char */
   register int done=0;				/* set to 1 to exit */
   int bell=0;					/* 1 if screen flashed once */
   int sub_window = 0;				/* sub window created */
   int fsizehigh, fsizewide;			/* variables to save deref. */
   int offset = 0;                              /* font glyph offset */
   /*}}}  */

   /* set up environment */

   if (W(flags)&W_ACTIVE)
   {
      window = W(window);
   }
   else
   {
      window = bit_create(W(save),SUM_BDR,SUM_BDR,BIT_WIDE(W(window)),BIT_HIGH(W(window)));
      sub_window++;
   }

   if (window==(BITMAP*)0)
   {
      perror("Bit_create failed for window");
      return(0);
   }

   /* avoid repeated dereferencing of pointers */

   fsizehigh = FSIZE(high);
   fsizewide = FSIZE(wide);

  if (W(flags)&W_SPECIAL)
  {
        if (W(flags)&W_UNDER) offset = MAXGLYPHS;
        if (W(flags)&W_BOLD) offset += 2*MAXGLYPHS;
  }

   if (Do_clip()) {
      Set_clipall();
      Set_cliplow(W(x)+W(text).x,W(y)+W(text).y-fsizehigh);
      }

   if (W(text.wide))
      text = bit_create(window,W(text.x),W(text.y),W(text.wide),W(text.high));
   if (text == (BITMAP *) 0)
      text = window;

   if (W(flags)&W_ACTIVE && mousein(mousex,mousey,win,0)) {
      MOUSE_OFF(screen,mousex,mousey);
      }

   if (win==active)
      cursor_off();

   /* do each character */

   for(indx=0;c= *buff++, indx<buff_count && !done; indx++)
   switch (W(flags)&W_STATE) {

      /* download and process text strings */

   case W_TEXT:				/* down load a text string */
      cnt = W(esc_cnt);
      W(snarf[W(esc)[TEXT_COUNT]++]) = c;
      if (W(esc)[TEXT_COUNT] >= W(esc)[cnt]) {
         W(snarf)[W(esc)[TEXT_COUNT]] = '\0';
         W(flags) &= ~W_TEXT;
         if (W(snarf) && W(code)!=T_BITMAP && W(code)!=T_GRUNCH)
            (void) trans(W(snarf));
         down_load(win,window,text);
         done++;
         }
      break;

   /**********************************************************************
    *	Handle escape codes
    */

   case W_ESCAPE:			/* process an escape code */
      W(flags) &= ~(W_ESCAPE);
      cnt = W(esc_cnt);
      switch(c) {
         /*{{{  ESC*/
         case ESC : 			/* turn on escape mode */
                    W(flags) |= W_ESCAPE;
                    W(esc_cnt) = 0;
                    W(esc[0]) = 0;
                    break;
         /*}}}  */
         /*{{{  0, 1, 2, 3, 4, 5, 6, 7, 8, 9*/
         case '0': case '1': case '2': case '3': case '4':
         case '5': case '6': case '7': case '8': case '9':
               W(esc)[W(esc_cnt)] = W(esc)[W(esc_cnt)]*10 + c-'0';
               W(flags) |= W_ESCAPE;
               if (W(flags)&W_MINUS && W(esc)[W(esc_cnt)] > 0) {
                  W(esc)[W(esc_cnt)] = -(W(esc)[W(esc_cnt)]);
                  W(flags) &= ~(W_MINUS);
                  }
               break;
         /*}}}  */
         /*{{{  E_SEP1, E_SEP2*/
         case E_SEP1:			/* primary field separator */
         case E_SEP2:			/* secondary field separator */
               if (W(esc_cnt)+1 < MAXESC)
                   W(esc_cnt)++;
               W(esc)[W(esc_cnt)] = 0;
               W(flags) &= ~(W_MINUS);
               W(flags) |= W_ESCAPE;
               break;
         /*}}}  */
         /*{{{  E_MINUS*/
         case E_MINUS:  		/* set the MINUS flag */
               W(flags) |= (W_ESCAPE|W_MINUS);
               break;
         /*}}}  */
         case E_NULL:			/* do nothing */
               done++;
               break;

         case E_ADDLINE:  		/* add a new line */
               if (*W(esc)) {
                  register int count = *W(esc);
                  scroll(win,text,W(y)-fsizehigh,T_HIGH,- count*(fsizehigh),BG_OP);
                  }
               else {
                  scroll(win,text,W(y)-fsizehigh,T_HIGH,-(fsizehigh),BG_OP);
                  }
               done++;
               break;

         case E_ADDCHAR:  		/* insert a character */
               {
               register int wide = (fsizewide) * (*W(esc) ? *W(esc) : 1);
               if (wide+W(x)>T_WIDE)
                  wide = T_WIDE-W(x);
               bit_blit(text,W(x)+wide,W(y)-fsizehigh,
                        T_WIDE-W(x)-wide, fsizehigh,BIT_SRC,
                        text,W(x),W(y)-fsizehigh);
               bit_blit(text,W(x),W(y)-fsizehigh,wide,
                    fsizehigh,BG_OP,0,0,0);
               }
               break;

         case E_DELETELINE:  		/* delete a line */
               if (*W(esc)) {
                  register int count = *W(esc);
                  scroll(win,text,W(y)-fsizehigh,T_HIGH,count*fsizehigh,
                      BG_OP);
                  }
               else {
                  scroll(win,text,W(y)-fsizehigh,T_HIGH,fsizehigh,
                      BG_OP);
                  }
               done++;
               break;

         case E_DELETECHAR:  		/* delete a character */
               {
               register int wide = (fsizewide) * (*W(esc) ? *W(esc) : 1);
               if (wide+W(x)>T_WIDE)
                  wide = T_WIDE-W(x);
               bit_blit(text,W(x),W(y)-fsizehigh,
                        T_WIDE-W(x)-wide, fsizehigh,BIT_SRC,
                        text,W(x)+wide,W(y)-fsizehigh);
               bit_blit(text,T_WIDE-wide,W(y)-fsizehigh,wide,
                    fsizehigh,BG_OP,0,0,0);
               }
               break;

         case E_UPLINE:  		/* up 1 line */
#ifdef FRACCHAR
               if (cnt>0) {	/* move up fractions of a character line */
                  int div = W(esc)[1] == 0 ? 1 : W(esc)[1];
                  int n = W(esc)[0] * fsizehigh / div;
                  if (W(y)>n) {
                     W(y) -= n;
                     if (Do_clip())
                        Set_cliplow(10000,W(y)+W(text).y-fsizehigh);
                     }
                  break;
                  }
#endif
               if (W(y)>fsizehigh) W(y) -= fsizehigh;
               if (Do_clip())
                  Set_cliplow(10000,W(y)+W(text).y-fsizehigh);
               break;

         case E_RIGHT:  		/* right 1 line */
#ifdef FRACCHAR
               if (cnt>0) {	/* move right/left a fraction of a character */
                  int div = W(esc)[1] == 0 ? 1 : W(esc)[1];
                  int n = W(esc)[0] * fsizewide / div;
                  W(x) += n;
                  if (W(x) < 0)
                     W(x) = 0;
                  break;
                  }
#endif
               W(x) += fsizewide;
               break;

         case E_DOWN:			/* down 1 line */
#ifdef FRACCHAR
               if (cnt>0) {	/* move down a fraction of a character */
                  int div = W(esc)[1] == 0 ? 1 : W(esc)[1];
                  int n = W(esc)[0] * fsizehigh / div;
						if (W(y)+n > T_HIGH) {
							scroll(win,text,0,T_HIGH,n,
										BG_OP);
							done++;
							}
                  else {
							W(y) += n;
							if (Do_clip())
								Set_cliphigh(0,W(y)+W(text).y);
                     }
                  break;
                  }
#endif
               if (W(y)+fsizehigh > T_HIGH) {
                  scroll(win,text,0,T_HIGH,fsizehigh,BG_OP);
                  done++;
                  }
               else {
                  W(y) += fsizehigh;
                  if (Do_clip())
                     Set_cliphigh(0,W(y)+W(text).y);
                  }
               break;

         case E_FCOLOR:       /* set forground color */
					if (W(flags)&W_STANDOUT) {
						standend(win);
						W(style) = W(flags)&W_REVERSE ?
								PUTBCOLOR(W(style),*W(esc)):PUTFCOLOR(W(style),*W(esc));
						standout(win);
						}
					else
						W(style) = W(flags)&W_REVERSE ?
								PUTBCOLOR(W(style),*W(esc)):PUTFCOLOR(W(style),*W(esc));
               BORDER(win);
               break;

         case E_BCOLOR:       /* set background color */
					W(style) = W(flags)&W_REVERSE ?
							 PUTFCOLOR(W(style),*W(esc)):PUTBCOLOR(W(style),*W(esc));
               BORDER(win);
               break;

         case E_STANDOUT:		/* inverse video (characters) */
	       standout(win);
               break;

         /* standend also sets character attributes */

         case E_STANDEND:		/* normal video (characters) */
         {
           int mode = *W(esc);

           if (mode)
           {
              enhance_font(W(font));
              done++;
           }
           offset=0;
                        if (mode&1){                   /* standout */
                                standout(win);
                                offset = 1;
                                }
           else {
              standend(win);
           }
           if (mode&2) { /* bold */
             W(flags)|=W_BOLD;
             offset|=2;
           }
           else { W(flags) &= ~W_BOLD;
           }
           if (mode&4){ /* underline */
           W(flags) |= W_UNDER;
           offset|=4;
           }
           else
           { W(flags) &= ~W_UNDER; }
           offset *= MAXGLYPHS;

               break;
               }

         case E_CLEAREOL:  		/* clear to end of line */
               bit_blit(text,W(x),W(y)-fsizehigh,T_WIDE-W(x),fsizehigh,BG_OP,0,0,0);
               if (Do_clip())
                  Set_cliphigh(BIT_WIDE(W(window)),0);
               break;

         case E_CLEAREOS:  		/* clear to end of window */
               bit_blit(text,W(x),W(y)-fsizehigh,T_WIDE-W(x),fsizehigh,BG_OP,0,0,0);
               bit_blit(text,0,W(y),T_WIDE,T_HIGH-W(y),BG_OP,0,0,0);
               if (Do_clip())
                  Set_cliphigh(BIT_WIDE(W(window)),BIT_HIGH(window));
               break;

         case E_SETCURSOR:			/* set the character cursor */
					W(curs_type) = *W(esc);
					break;
         case E_BLEEP:			/* highlight a section of the screen */
               if (cnt>2) {
                  register int *p = W(esc);
                  if (p[0]<0 || p[1]<0 )
                     break;
                  p[2] = BETWEEN(1,p[2],BIT_WIDE(screen)-1);
                  p[3] = BETWEEN(1,p[3],BIT_WIDE(screen)-1);
                  bit_blit(screen,p[0],p[1],p[2],p[3],BIT_NOT(BIT_DST),0,0,0);
                  fsleep();
                  bit_blit(screen,p[0],p[1],p[2],p[3],BIT_NOT(BIT_DST),0,0,0);
                  done++;
                  }
               break;

         case E_FONT:  			/* pick a new font */
               W(flags) &= ~W_SNARFABLE;
               W(flags) &= ~W_SPECIAL;         /* reset bold or underline */
               offset=0;
               if (cnt > 0) {
                  W(esc)[TEXT_COUNT] = 0;
                  if (W(esc)[cnt]>0 && (W(snarf)=malloc(W(esc)[cnt]+1)) != (unsigned char *)0)
                     W(flags) |= W_TEXT;
                  W(code) = T_FONT;
                  break;
                  }

                  {
                  int font_count = W(esc)[cnt];
                  int baseline = FSIZE(baseline);

                  W(font)=Get_font(font_count);
		            fsizehigh = FSIZE(high);
		            fsizewide = FSIZE(wide);
                  W(y) += FSIZE(baseline)-baseline;
                  if (W(y) < fsizehigh) {
                     scroll(win,text,W(y)-fsizehigh,T_HIGH,W(y)-fsizehigh,BG_OP);
                     W(y)=fsizehigh;
                     done++;
                     }
                  }
					set_size(win);
               break;

         case E_MOUSE:  		/* testing  -- move the mouse */
               MOUSE_OFF(screen,mousex,mousey);
               if (W(esc_cnt) < 1) {
                  mousex = W(x0) + W(x);
                  mousey = W(y0) + W(y);
                  }
               else {
                  mousex = W(esc)[cnt-1];
                  mousey = W(esc)[cnt];
                  mousex = BETWEEN(0,mousex,BIT_WIDE(screen)-1);
                  mousey = BETWEEN(0,mousey,BIT_HIGH(screen)-1);
                  }
               if (!(W(flags)&W_ACTIVE && mousein(mousex,mousey,win,0)))
                  MOUSE_ON(screen,mousex,mousey);
               break;

         case E_SIZE:  			/* reshape window: cols, rows */
               if (!W(flags)&W_ACTIVE) break;
               if (cnt >= 1) {
                  int cols = W(esc)[cnt-1];
                  int lines = W(esc)[cnt];
                  int x = W(x0), y = W(y0);

                  MOUSE_OFF(screen,mousex,mousey);

                  if (cnt>=3) {
                     x = W(esc)[0];
                     y = W(esc)[1];
                     }

                  if (win!=active)
                     cursor_off();
                  ACTIVE_OFF();
                  expose(win);
                  shape(x, y,
                        cols?cols*fsizewide+2*SUM_BDR:
                             2*SUM_BDR + WIDE,
                        lines?lines*fsizehigh+2*SUM_BDR:
                             2*SUM_BDR + HIGH);
                  ACTIVE_ON();
                  if (!(W(flags)&W_ACTIVE && mousein(mousex,mousey,win,0)))
                     MOUSE_ON(screen,mousex,mousey);
                  done++;
                  }
               break;

         case E_PUTSNARF:  		/* put the snarf buffer */
               if (snarf)
                  Write(W(to_fd),snarf,strlen(snarf));
#ifdef DEBUG
               dprintf(y)(stderr,"%s: sending yank buffer [%s]\n",W(tty),
                         snarf?snarf:"EMPTY");
#endif
               break;

         case E_GIMME:  		/* snarf text into input queue */
               W(esc)[TEXT_COUNT] = 0;
               if (W(esc)[cnt]>0 && W(esc)[cnt]<MAXSHELL &&
                          (W(snarf)=malloc(W(esc)[cnt]+1)) != (unsigned char *)0)
                  W(flags) |= W_TEXT;
               W(code) = T_GIMME;
               break;

         case E_GMAP:  		/* read a bitmap from a file */
#ifdef DEBUG
              dprintf(*)(stderr,"%s: fetching bitmap %d\r\n",
                               W(tty),*W(esc)-1);
#endif
               W(esc)[TEXT_COUNT] = 0;
               if (W(esc)[cnt]>0 && W(esc)[cnt]<MAX_PATH &&
                          (W(snarf)=malloc(W(esc)[cnt]+1)) != (unsigned char *)0)
                  W(flags) |= W_TEXT;
               W(code) = T_GMAP;
               break;

         case E_SMAP:  		/* save a bitmap on a file */
               W(esc)[TEXT_COUNT] = 0;
               if (W(esc)[cnt]>0 && W(esc)[cnt]<MAX_PATH &&
                          (W(snarf)=malloc(W(esc)[cnt]+1)) != (unsigned char *)0) {
                  W(flags) |= W_TEXT;
#ifdef DEBUG
                  dprintf(*)(stderr,"%s: saving bitmap %d\r\n",
                               W(tty),*W(esc)-1);
#endif
                  }
               W(code) = T_SMAP;
               break;

         case E_SNARF:  		/* snarf text into the snarf buffer */
               W(esc)[TEXT_COUNT] = 0;
               if (W(esc)[cnt]>=0 &&	/*** was just > */
                          (W(snarf)=malloc(W(esc)[cnt]+1)) != (unsigned char *)0)
                  W(flags) |= W_TEXT;
               W(code) = T_YANK;
               break;

         case E_STRING:  		/* write text into the offscreen bitmap */
               W(esc)[TEXT_COUNT] = 0;
               if (W(esc)[cnt]>0 && /* W(bitmaps)[*W(esc)-1] && */
                          (W(snarf)=malloc(W(esc)[cnt]+1)) != (unsigned char *)0)
                  W(flags) |= W_TEXT;
               W(code) = T_STRING;
               break;

         case E_GRUNCH:  		/* graphics scrunch mode  (experimental) */
               W(esc)[TEXT_COUNT] = 0;
               if (W(esc)[cnt]>=0 &&	/*** was just > */
                          (W(snarf)=malloc(W(esc)[cnt]+1)) != (unsigned char *)0)
                  W(flags) |= W_TEXT;
               W(code) = T_GRUNCH;
               break;

#ifdef XMENU
         case E_XMENU:  			/* extended menu stuff */
										/* ^[3X	remove menu 3 from window */
										/* ^[3,4X	select item 4 of menu 3 */
										/* ^[1,2,3X	display menu 3 at 1,2 */
										/* ^[1,2,3,4Xhighlight menu 3 item 4 at 1,2 */
               {
               register int *p = W(esc);
               register struct menu_state *menu;
               switch(cnt) {
                  case 0:			/* remove menu from display */
                     if (p[0]>=0 && p[0]<MAXMENU && (menu=W(menus[p[0]])))
                        menu_remove(menu);
                  case 1:			/* select active item */
                     if (p[0]>=0 && p[0]<MAXMENU && (menu=W(menus[p[0]])))
                        menu->current = p[1];
                     break;
                  case 2:			/* display menu  on window */
                     if (p[2]>=0 && p[2]<MAXMENU && (menu=W(menus[p[2]])))
                        menu_setup(menu,window,Scalex(p[0]),Scaley(p[1]),-1);
                     break;
                  case 3:			/* highlight menu item on window */
                     if (p[2]>=0 && p[2]<MAXMENU &&
                                   (menu=W(menus[p[2]])) && menu->menu) {
                        bit_blit(window, Scalex(p[0])+MENU_BORDER,
                                 Scaley(p[1])+(p[3]-1)*menu->bar_sizey+
                                 MENU_BORDER,
                               menu->bar_sizex, menu->bar_sizey,
                               BIT_NOT(BIT_DST), 0, 0, 0);
                        }
                     break;
                     }
               }
               break;
#endif
         case E_MENU:  			/* get a menu */
               {			/* should be split into several cases */
               register int b = (W(esc)[0]<0);		/* which button */
               register int n = ABS(W(esc)[0]);		/* menu number */

               /* setup menu pointer */

               if (cnt > 2) {
                  int parent = n;		/* parent menu # */
                  int child = W(esc[2]);	/* child menu number */
                  int item = W(esc[1]);		/* item # of parent */
                  int flags = W(esc[3]);	/* menu flags */

                  if (parent<0 || parent >= MAXMENU || child >= MAXMENU ||
                      W(menus[parent])==(struct menu_state*)0)
                     break;

#ifdef DEBUG
                  dprintf(M)(stderr,"Linking menu %d to parent %d at item %d\n",
                             child,parent,item);
#endif

                  if (item<0)					/* page link */
                     W(menus[parent])->next = child;
                  else if (item < W(menus[parent])->count)	/* slide lnk */
                     menu_setnext(W(menus[parent]),item) = child;

                  /* menu flags */

                  if (flags > 0)
                     W(menus[parent])->flags = flags;

                  break;
                  }

               /* download a menu */

               if (cnt > 0) {
                  W(esc)[TEXT_COUNT] = 0;
                  if (W(menus)[n]) {
                     menu_destroy(W(menus)[n]);
                     W(menus)[n] = (struct menu_state *) 0;
                     if (W(menu[0])== n)
                        W(menu[0]) = -1;
                     if (W(menu[1])== n)
                        W(menu[1]) = -1;
                     }
                  if (W(esc)[cnt]>0 && (W(snarf)=malloc(W(esc)[cnt]+1))
                                     != (unsigned char *)0) {
                     W(flags) |= W_TEXT;
                     W(code) = T_MENU;
                     }
#ifdef DEBUG
                  dprintf(M)(stderr,"downloading menu %d\n",n);
#endif
                  }

               /* select menu number */

               else if (n < MAXMENU && W(menus)[n]) {
                  int last_menu = W(menu[b]);

#ifdef DEBUG
                  dprintf(M)(stderr,"selecting menu %d on button %d\n",n,b);
#endif
                  W(menu[b]) = n;
                  if (last_menu<0 && button_state==(b?BUTTON_1:BUTTON_2))
                     go_menu(b);
                  }
               else
                  W(menu[b]) = -1;
               }
               break;

         case E_EVENT:			/* get an event */
               switch(cnt) {
                  case 2:	/* append to an event */
                  case 1:	/* set an event */
                     W(esc)[TEXT_COUNT] = 0;
                     if (W(esc)[cnt]>0 && (W(snarf)=malloc(W(esc)[cnt]+1))
                                  != (unsigned char *)0) {
                        W(flags) |= W_TEXT;
                        W(code) = T_EVENT;
                        }
                     break;
                  case 0:
                    cnt = W(esc)[0];
		    if( !CHK_EVENT(cnt) )
		       break;
                    EVENT_CLEAR_MASK(win,cnt);
                    if (W(events)[GET_EVENT(cnt)]) {
                       free (W(events)[GET_EVENT(cnt)]);
                       W(events)[GET_EVENT(cnt)] = (unsigned char *) 0;
                      }
                    break;
                  }
               break;

         case E_SEND:			/* send a message */
               W(esc)[TEXT_COUNT] = 0;
               if (W(esc)[cnt]>0 && (W(snarf)=malloc(W(esc)[cnt]+1))
                                  != (unsigned char *)0) {
                  W(flags) |= W_TEXT;
                  W(code) = T_SEND;
                  }
               break;

         case E_BITGET:			/* upload a bitmap (temporary)*/
               {
               int offset = W(esc)[2];
               int which = *W(esc);
               int size = W(esc)[1];
               BITMAP *m = W(bitmaps)[which-1];

               if (cnt>1 && which>0 && which < MAXBITMAPS &&
                        m != (BITMAP *) 0 &&
                        size+offset<bit_size(BIT_HIGH(m),BIT_WIDE(m),BIT_DEPTH(m)))
                  write(W(to_fd),BIT_DATA(m)+offset,size);
               }
               break;
         case E_BITCRT:  		/* create/destroy a bitmap */
               switch(cnt) {
                  case 0:		/* destroy a bitmap */
                     if (W(esc)[0] && W(esc[0]<=MAXBITMAPS) &&
                                  W(bitmaps)[W(esc)[0]-1] != (BITMAP *) 0) {
                        bit_destroy(W(bitmaps)[W(esc)[0]-1]);
                        W(bitmaps)[W(esc)[0]-1] = (BITMAP *) 0;
#ifdef DEBUG
                        dprintf(B)(stderr,"%s: Destroyed bitmap %d\r\n",
                                  W(tty),*W(esc)-1);
#endif
                        }
                     break;
                  case 2:		/* create new bitmap - same depth as window */
                  case 3:		/* " - specify depth, 1->1 bit, otherwise DEPTH */
                     if (W(esc)[0] && W(esc[0]<=MAXBITMAPS) &&
                                  W(bitmaps)[W(esc)[0]-1] == (BITMAP *) 0) {
                        W(bitmaps)[W(esc)[0]-1] =
                              bit_alloc(Scalex(W(esc)[1]),Scalex(W(esc)[2]),0,
                              cnt==3&&W(esc)[3]==1 ? 1 : BIT_DEPTH(W(window)));
#ifdef DEBUG
                        dprintf(B)(stderr,"%s: created bitmap %d (%d,%d)\r\n",
                                W(tty),*W(esc),W(esc)[1],W(esc)[2]);
#endif
                        }
                     break;
                  }
               break;

			/* download a bitmap */

         case E_BITLOAD:
               if (cnt >=2) {
			int format = bmap_size(W(esc[0]),W(esc[1]),W(esc[cnt]));

			/* can't handle 8 bit images on 1 bit displays, punt */

			if (format & 0x80 && BIT_DEPTH(W(window))==1) {
#ifdef DEBUG
                     dprintf(*)(stderr,"Cant display color image\r\n");
#endif
							break;
						}

                  if (format & 0x80)	/* 8 bits */
                     W(bitmap) = bit_alloc(W(esc[0]),W(esc[1]),0,BIT_DEPTH(W(window)));
                  else
                     W(bitmap) =bit_alloc(W(esc[0]),W(esc[1]),0,1);

                  if (W(bitmap) != (BITMAP *) 0) {
                     W(flags) |= W_TEXT;
                     W(snarf) = (unsigned char *) BIT_DATA(W(bitmap));
                     }
                  W(esc)[TEXT_COUNT] = 0;
                  W(code) = T_BITMAP;
                  }
               break;

         case E_SHAPE:  		/* reshape window, make it active */

               MOUSE_OFF(screen,mousex,mousey);

               ACTIVE_OFF();
               if (win!=active) {
                  cursor_off();
                  expose(win);
                  }

               if (cnt >= 3)
                  shape(W(esc)[cnt-3], W(esc)[cnt-2],
                        W(esc)[cnt-1], W(esc)[cnt]);
               else if (cnt == 1)
                  shape(W(esc)[cnt-1], W(esc)[cnt],
                        BIT_WIDE(W(border)),
                        BIT_HIGH(W(border)));

               ACTIVE_ON();
               if (!(W(flags)&W_ACTIVE && mousein(mousex,mousey,win,0)))
                  MOUSE_ON(screen,mousex,mousey);

               done++;
               break;

         case E_BITBLT: 		/* do a bit blit */
               win_rop(win,window);
               done++;
               break;

         case E_CIRCLE:  		/* Plot a circle (or ellipse) */
               circle_plot(win,window);
               break;

         case E_LINE:  			/* Plot a line */
               win_plot(win,window);
               break;

         case E_GO:  			/* Go; move graphics pointer */
               win_go(win);
               break;

         case E_MOVE:  			/* move to x,y pixels */
                  W(flags) &= ~W_SNARFABLE;
                  if (Do_clip())
                     Set_cliphigh(W(x)+W(text).x+fsizewide,W(y)+W(text).y);
		  if (cnt>0) {
                     W(x) = Scalex(*W(esc));
                     W(y) = Scaley(W(esc)[1]);
                     }
                   else {
                     W(x) += Scalex(*W(esc));
                     }
                   if (W(x)+fsizewide > WIDE && !(W(flags)&W_NOWRAP))
                      W(x) = WIDE-fsizewide;
                   if (W(y) > HIGH)
                      W(y) = HIGH - fsizehigh;
                   if (Do_clip())
                      Set_cliplow(W(x)+W(text).x,W(y)+W(text).y-fsizehigh);
               break;

         case E_CUP:  			/* move to col,row (zero based) */
               if (cnt < 1) break;
                  {
                  register int x = W(esc)[cnt-1] * fsizewide;
                  register int y = W(esc)[cnt] * fsizehigh;
                  if (x == BETWEEN(-1,x,T_WIDE-fsizewide) &&
                      y == BETWEEN(-1,y,T_HIGH)) {
                      if (Do_clip())
                        Set_cliphigh(W(x)+W(text).x+fsizewide,W(y)+W(text).y);
                      W(y) = y+fsizehigh;
                      W(x) = x;
                      if (Do_clip())
                         Set_cliplow(W(x)+W(text).x,W(y)+W(text).y-fsizehigh);
                      }
                  }
               break;

         case E_VI:  			/* turn on vi hack */
               W(flags) |= W_VI;
               break;

         case E_NOVI:  			/* turn off vi hack */
               W(flags) &= ~W_VI;
               break;

         case E_PUSH:			/* push environment */
              win_push(win,*W(esc));
              break;

         case E_POP:			/* pop old environment */
              MOUSE_OFF(screen,mousex,mousey);
              win_pop(win);
              if (!(W(flags)&W_ACTIVE && mousein(mousex,mousey,win,0)))
                 MOUSE_ON(screen,mousex,mousey);
              done++;
              break;

         case E_TEXTREGION:		/* setup text region */
              switch (cnt) {
                 case 1:	/* setup scrolling region (~aka vt100) */
                      if (W(esc)[0] >=0 && W(esc)[1] >= W(esc)[0] &&
                              	W(esc)[1]*fsizehigh < BIT_HIGH(W(window))) {
                         W(text.x) = 0;
                         W(text.wide) = BIT_WIDE(W(window));
                         W(text.y) = fsizehigh*W(esc[0]);
                         W(text.high) = fsizehigh*(1+W(esc[1])) - W(text.y);
                         if (W(y) < W(text.y)+fsizehigh)
                            W(y) = W(text.y) + fsizehigh;
                         if (W(y) > W(text.high))
                            W(y) = W(text.high);
                         }
                      break;
                 case 3:		/* set up entire region */
                      W(text.wide) = Scalex(W(esc[2]));
                      W(text.high) = Scaley(W(esc[3]));
                      W(text.x) = Scalex(W(esc[0]));
                      W(text.y) = Scaley(W(esc[1]));
                      if (W(text.high) >= fsizehigh*MIN_Y  &&
                                       W(text.wide) >= fsizewide*MIN_X) {
                         W(x) = 0;
                         W(y) = fsizehigh;
                         W(flags) &= ~W_SNARFABLE;
                         break;
                         }
                      W(text.x) = 0;
                      W(text.y) = 0;
                      W(text.wide) = 0;
                      W(text.high) = 0;
                      break;
                 case 4:	/* set up entire region (use rows, cols) */
                      W(text.x) = W(esc[0]) * fsizewide;
                      W(text.y) = W(esc[1]) * fsizehigh;
                      W(text.wide) = W(esc[2]) * fsizewide;
                      W(text.high) = W(esc[3]) * fsizehigh;
                      if (W(text.high) >= fsizehigh*MIN_Y  &&
                                       W(text.wide) >= fsizewide*MIN_X) {
                          W(x) = 0;
                          W(y) = fsizehigh;
                          break;
                          }
                      break;
                 case 0: 		/* clear text region */
                      if (W(text.x)%fsizewide!= 0 || W(text.y)%fsizehigh!=0)
                         W(flags) &= ~W_SNARFABLE;
                      W(text.x) = 0;
                      W(text.y) = 0;
                      W(text.wide) = 0;
                      W(text.high) = 0;
                      break;
                 }
              done++;
#ifdef REGION_HACK
	      set_size(win);
#endif
              break;

         case E_SETMODE:  		/* set a window mode */
#ifdef DEBUG
              dprintf(E)(stderr,"%s: setting mode %d\r\n",W(tty),*W(esc));
#endif
              switch(W(esc)[0]) {
		 case M_STANDOUT:
		      standout(win);
		      break;
                 case M_BACKGROUND:	/* enable background writes */
                      W(flags) |= W_BACKGROUND;
                      break;
                 case M_NOINPUT:	/* disable keyboard input */
                      W(flags) |= W_NOINPUT;
                      break;
                 case M_AUTOEXPOSE:	/* auto expose upon write */
                      W(flags) |= W_EXPOSE;
                      break;
                 case M_WOB:		/* set white on black */
                      if (W(flags)&W_REVERSE)
                         break;
                      W(flags) |= W_REVERSE;
                      W(style) = SWAPCOLOR(W(style));
                      CLEAR(window,BG_OP);
                      BORDER(win);
                      if (Do_clip())
                         Set_all();
                      break;
                 case M_NOWRAP:  	/* turn on no-wrap */
                      W(flags) |= W_NOWRAP;
                      break;
                 case M_OVERSTRIKE:	/* turn on overstrike */
                      W(style) = PUTOP(W(op),W(style));
                      W(flags) |= W_OVER;
                      break;
                 case M_ABS:		/* set absolute coordinate mode */
                      W(flags) |= W_ABSCOORDS;
                      break;
                 case M_DUPKEY:		/* duplicate esc char from keyboard */
                      W(flags) |= W_DUPKEY;
                      if (cnt > 0)
                         W(dup) = W(esc[1])&0xff;
                      else
                         W(dup) = DUP_CHAR;
                      break;
                 case M_NOBUCKEY:	/* set no buckey interpretation mode */
                      W(flags) |= W_NOBUCKEY;
                      break;
#ifndef NOSTACK
                 case M_STACK:		/* enable event stacking */
                      EVENT_SET_MASK(win,EVENT_STACK);
                      break;
#endif
                 case M_SNARFLINES:	/* only cut lines */
                      W(flags) |= W_SNARFLINES;
                      break;
                 case M_SNARFTABS:	/* change spaces to tabs */
                      W(flags) |= W_SNARFTABS;
                      break;
                 case M_SNARFHARD:	/* snarf even if errors */
                      W(flags) |= W_SNARFHARD;
                      break;
                 case M_ACTIVATE:	/* activate the window */
                      if (win == active)
                          break;
                      
                      MOUSE_OFF(screen,mousex,mousey);

                      cursor_off();
                      ACTIVE_OFF();
                      expose(win);
                      ACTIVE_ON();
                      cursor_on();
                      done++;

                      if (!(W(flags)&W_ACTIVE && mousein(mousex,mousey,win,0)))
                         MOUSE_ON(screen,mousex,mousey);
                      break;
                      }
              break;

         case E_CLEARMODE:  		/* clear a window mode  */
#ifdef DEBUG
              dprintf(E)(stderr,"%s: clearing mode %d\r\n",W(tty),*W(esc));
#endif
              switch(W(esc)[0]) {
		 case M_STANDOUT:
		      standend(win);
		      break;
                 case M_BACKGROUND:	/* don't permit background writes */
                      W(flags) &= ~W_BACKGROUND;
                      break;
                 case M_NOINPUT:	/* permit keyboard input */
                      W(flags) &= ~W_NOINPUT;
                      break;
                 case M_AUTOEXPOSE:	/* don't auto expose */
                      W(flags) &= ~W_EXPOSE;
                      break;
                 case M_WOB:		/* set black-on-white */
                      if (!(W(flags)&W_REVERSE)) 
                         break;
                      W(flags) &= ~W_REVERSE;
                      W(style) = SWAPCOLOR(W(style));
                      CLEAR(window,BG_OP);
                      BORDER(win);
                      if (Do_clip())
                         Set_all();
                      break;
                 case M_NOWRAP:  	/* turn off no-wrap */
                      W(flags) &= ~W_NOWRAP;
                      break;
                 case M_OVERSTRIKE:	/* turn off overstrike */
                      if (W(flags)&W_STANDOUT)
							 	W(style) = PUTOP(BIT_NOT(BIT_SRC),W(style));
                      else
							 	W(style) = PUTOP(BIT_SRC,W(style));
                      W(flags) &= ~W_OVER;
                      break;
                 case M_ABS:		/* set relative coordinate mode */
                      W(flags) &= ~W_ABSCOORDS;
                      break;
                 case M_DUPKEY:		/* reset keyboard dup-ky mode */
                      W(flags) &= ~W_DUPKEY;
                      break;
                 case M_NOBUCKEY:	/* reset no buckey interpretation mode */
                      W(flags) &= ~W_NOBUCKEY;
                      break;
#ifndef NOSTACK
                 case M_STACK:		/* enable event stacking */
                      EVENT_CLEAR_MASK(win,EVENT_STACK);
                      break;
#endif
                 case M_SNARFLINES:	/* only cut lines */
                      W(flags) &= ~W_SNARFLINES;
                      break;
                 case M_SNARFTABS:	/* change spaces to tabs */
                      W(flags) &= ~W_SNARFTABS;
                      break;
                 case M_SNARFHARD:	/* snarf even if errors */
                      W(flags) &= ~W_SNARFHARD;
                      break;
                 case M_ACTIVATE:	/* hide the window */
                      if (!(W(flags)&W_ACTIVE) || next_window==1)
                          break;
                      MOUSE_OFF(screen,mousex,mousey);
                      if (win!=active)
                         cursor_off();
                      ACTIVE_OFF();
                      hide(win);
                      ACTIVE_ON();
                      if (win!=active)
                         cursor_on();
                      if (!(W(flags)&W_ACTIVE && mousein(mousex,mousey,win,0)))
                         MOUSE_ON(screen,mousex,mousey);

                      done++;
                      break;
                      }
              break;

         case E_GETINFO:  		/* send window info back to shell */
              get_info(win,window,text);
              break;
                    
         case E_MAKEWIN:		/* make or goto a new window */
              MOUSE_OFF(screen,mousex,mousey);
              win_make(win,indx);
              done++;
              break;

         case E_HALFWIN:		/* make a 1/2 window */
              {
              register int *p = W(esc);
              char *tty = (char*)0;

              if (cnt < 3 ||  cnt > 4)
                 break;
              MOUSE_OFF(screen,mousex,mousey);
              if (win!=active)
                 cursor_off();
              ACTIVE_OFF();

              switch (cnt) {
                 case 4:
                    tty = half_window(p[0],p[1],p[2],p[3],p[4]);
                    break; 
                 case 3:
                    tty = half_window(p[0],p[1],p[2],p[3],-1);
                    break; 
                 }
              if (win!=active)
                 cursor_on();
              if (W(flags)&W_DUPKEY)
                 sprintf(buff,"%c %s\n",W(dup),tty?tty:"");
              else
                 sprintf(buff,"%s\n",tty?tty:"");
              if (tty) {
                 ACTIVE_ON();
                 }
              write(W(to_fd),buff,strlen(buff));

              done++;
              }
              break;
         case E_CURSOR:			/* set the mouse cursor */
              {
              BITMAP *map = W(bitmaps[*W(esc)]);
              int x0=0,y0=0;

              if (cnt > 0)
                 x0 = W(esc[1]);
              if (cnt > 0)
                 y0 = W(esc[2]);

              if (W(cursor)) {
                 bit_destroy(W(cursor));
                 W(cursor) = (BITMAP*)0;
                 SETMOUSEICON(&mouse_arrow);
                 }

              if ( cursor_ok(map,x0,y0) && (W(cursor) = 
                            bit_alloc(16,32,0,1)))
                 bit_blit(W(cursor),0,0,16,32,BIT_SRC,map,x0,y0);
              else
                 W(cursor) = (BITMAP*)0;
              }
              break;

         default:			/* not implemented */
                    break;
         }
      if (!(W(flags)&W_ESCAPE))
         W(flags) &= ~W_MINUS;
      break;

   /****************************************************************************
    *	Normal characters
    */

   default:
      switch(c) {
         case ESC : 			/* turn on escape mode */
                    W(flags) |= W_ESCAPE;
                    W(flags) &= ~(W_MINUS);
                    W(esc_cnt) = 0;
                    W(esc[0]) = 0;
                    break;

         case C_NULL:			/* null character -- ignore */
                    break;

         case C_BS:			/* back space */
                    if (Do_clip()) {
                       Set_cliphigh(W(x)+W(text).x + fsizewide,0);
                       }
                    W(x) -= fsizewide;
                    if (W(x) < 0)
                       W(x) = 0;
                    if (Do_clip()) {
                       Set_cliplow(W(x)+W(text).x,10000);
                       }
                    break;

         case C_FF:			/* form feed */
                    CLEAR(text,BG_OP);
                    W(x)=0;
                    W(y)=fsizehigh;
                    W(flags) |= W_SNARFABLE;
                    if (Do_clip())
                       Set_all();
                    done++;
                    break;

         case C_BELL:			/* ring the bell  */
                    bell_on();
                    if (!bell++) {
                       CLEAR(W(window),BIT_NOT(BIT_DST));
                       CLEAR(W(window),BIT_NOT(BIT_DST));
                       }
                    break;

         case C_TAB:			/* tab */
                    W(x) = ((W(x)/fsizewide +8)& ~ 7) * fsizewide;
                    if (W(x)+fsizewide >= T_WIDE) {
                       W(x)=0;
                       if (W(y)+fsizehigh > T_HIGH) {
                          scroll(win,text,0,T_HIGH,fsizehigh,BG_OP);
                          done++;
                          }
                       else
                          W(y) += fsizehigh;
                       }
                    break;

         case C_RETURN:			/* return */
                    if (Do_clip()) {
                       Set_cliphigh(W(x)+W(text).x + fsizewide,0);
                       Set_cliplow(W(text).x,10000);
                       }
                    W(x)=0;
                    break;

         case C_NL: 			/* line feed */
                    if (W(y)+fsizehigh > T_HIGH) {
                       scroll(win,text,0,T_HIGH,fsizehigh,BG_OP);
                       done++;
                       }
                    else
                       W(y) += fsizehigh;
                    break;
         default:			/* print a character */
                    if (W(y) > T_HIGH) 
                       W(y) = T_HIGH-fsizehigh;
                    PUT_CHAR(text,W(x),W(y),W(font),W(style),offset+c);

                    W(x) += fsizewide;
                    if (W(x)+fsizewide > T_WIDE && !(W(flags)&W_NOWRAP)) {
                       if (Do_clip()) {
                          Set_cliphigh(W(x)+W(text).x + fsizewide,0);
                          Set_cliplow(W(text).x,10000);
                          }
                       W(x)=0;
                       W(y) += fsizehigh;
                       if (W(y) > T_HIGH) {
                          W(y) -= fsizehigh;
                          scroll(win,text,0,T_HIGH,fsizehigh,BG_OP);
                          done++;
                          }
                       }
                    break;
         }
      break;
      }

   if (Do_clip())
      Set_cliphigh(W(x)+W(text).x+fsizewide,W(y)+W(text).y);

   cursor_on();

   MOUSE_ON(screen,mousex,mousey);

   /* this is probably wrong */
   if (text != window) bit_destroy(text);
   if (sub_window) bit_destroy(window);

   if (W(flags)&W_BACKGROUND && !(W(flags)&W_ACTIVE)) update(win, &clip);
   return(indx);
   }
/*}}}  */
