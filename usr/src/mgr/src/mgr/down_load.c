/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
*/

/* down load text -- called from put_window.c */
/*}}}  */

/*{{{  #includes*/
#include <sys/file.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "bitblit.h"
#include "font.h"

#include "defs.h"
#include "menu.h"
#include "event.h"

#include "do_event.h"
#include "do_menu.h"
#include "font_subs.h"
#include "get_font.h"
#include "get_menus.h"
/*}}}  */

/*{{{  get_map -- find bitmap associated with window id*/
static BITMAP *get_map(id,sub)
int id;				/* pid of process controlling window */
int sub;			/* window number of this window */
{
  register WINDOW *win;
  register BITMAP *map;

  for(win=active;win != (WINDOW *) 0;win=W(next))
  if (W(pid)==id && W(num)==sub) 
  {
    map=bit_alloc(BIT_WIDE(W(window)),BIT_HIGH(W(window)),(DATA*)0,BIT_DEPTH(W(window)));
    if (map && W(flags)&W_ACTIVE) bit_blit(map,0,0,BIT_WIDE(map),BIT_HIGH(map),BIT_SRC,W(window),0,0);
    else if (map) bit_blit(map,0,0,BIT_WIDE(map),BIT_HIGH(map),BIT_SRC,W(save),SUM_BDR,SUM_BDR);
    return(map);
  }
  return((BITMAP*)0);
}
/*}}}  */

/*{{{  down_load*/
void down_load(win,window,text) register WINDOW *win; BITMAP *window, *text;
{
  WINDOW *win2;
  int cnt;
  int id;

  cnt = W(esc_cnt);
  switch(W(code)) 
  {
    /*{{{  T_INVALID -- invalid text mode*/
    case T_INVALID:
#    ifdef DEBUG
    if (debug) fprintf(stderr,"mgr: invalid download code\n");
#    endif
    break;
    /*}}}  */
    /*{{{  T_MENU    -- down load menu*/
    case T_MENU:
    {
      struct font *f;
      int fn = W(esc[1]);

      if (*W(snarf) && cnt>1 )
      f = Get_font(fn);
      else
      f = W(font);

      if (*W(snarf)) 
      {
        W(menus)[*W(esc)] = do_menu(W(snarf),f,W(style));
        if (active == win ) 
        {
          if (W(menu[0]) == *W(esc) && button_state==BUTTON_2)
          go_menu(0);
          else if (W(menu[1]) == *W(esc) && button_state==BUTTON_1)
          go_menu(1);
        }
      }
      else
      W(menus)[*W(esc)] = (struct menu_state *) 0;
    }
    break;
    /*}}}  */
    /*{{{  T_EVENT   -- down load an event*/
    case T_EVENT:
    cnt=W(esc)[0];
    if (!CHK_EVENT(cnt)) 
    {
      break;
    }
    if (W(events)[GET_EVENT(cnt)]) 
    {
      free(W(events)[GET_EVENT(cnt)]);
      W(events)[GET_EVENT(cnt)] = (char *) 0;
    }
    if (*W(snarf)) 
    {
      W(events)[GET_EVENT(cnt)] = W(snarf);
      W(snarf) = NULL;
      EVENT_SET_MASK(win,cnt);
#      ifdef DEBUG
      dprintf(e)(stderr,"%s: setting event %d (%d)[%s]\r\n",
      W(tty),GET_EVENT(cnt),strlen(W(snarf)), W(snarf));
#      endif
      /* if button is down, then do the associated event */

      if (win == active  &&
      ((cnt==EVENT_B1_DOWN && button_state == BUTTON_1) ||
      (cnt==EVENT_B2_DOWN && button_state == BUTTON_2)))
      do_event(button_state,win,E_MAIN);
    }
    else 
    {
      EVENT_CLEAR_MASK(win,cnt);
#      ifdef DEBUG
      dprintf(e)(stderr,"%s: clearing event %d\r\n",
      W(tty),GET_EVENT(cnt));
#      endif
    }
    break;
    /*}}}  */
    /*{{{  T_STRING  -- draw text into offscreen bitmap*/
    case T_STRING:
    {
      int x = cnt>1 ? Scalex(W(esc[1])) : 0;
      int y = cnt>2 ? Scaley(W(esc[2])) : 0;

      if (y<FSIZE(high))
      y=FSIZE(high);
      if (x<0)
      x=0;
#      ifdef DEBUG
      dprintf(y)(stderr,"%s: drawing [%s] to %d\r\n",
      W(tty),W(snarf),*W(esc));
#      endif
      if (*W(esc)>0 && W(bitmaps)[*W(esc)-1] == (BITMAP *) 0) 
      {
        W(bitmaps)[*W(esc)-1] = bit_alloc(x+strlen(W(snarf))*FSIZE(wide), y, (DATA*)0,1);	/* text is always 1 bit deep */
#        ifdef DEBUG
        dprintf(y)(stderr,"%s: STRING creating %d (%dx%d)\n",
        W(tty),*W(esc),
        x+strlen(W(snarf))*FSIZE(wide),y);
#        endif
      }
      if (*W(esc) > 0)
      put_str(W(bitmaps)[*W(esc)-1],x,y,W(font),W(op),W(snarf));
      else
      put_str(window,x,y,W(font),W(op),W(snarf));
    }
    break;
    /*}}}  */
    /*{{{  T_YANK    -- fill yank buffer*/
    case T_YANK:
    if (snarf) free(snarf);
    snarf = W(snarf);
#    ifdef DEBUG
    dprintf(y)(stderr,"%s: yanking [%s]\r\n",W(tty),snarf);
#    endif
    id_message = W(pid);
    W(snarf) = (char *) 0;
    for(win2=active;win2 != (WINDOW *) 0;win2=win2->next)
    do_event(EVENT_SNARFED,win2,E_MAIN);
    break;

    /*}}}  */
    /*{{{  T_SEND    -- send a message*/
    case T_SEND:
    id = *W(esc);
    if (message) 
    {
      free(message);
      message = (char *) 0;
    }
    message = W(snarf);
    id_message = W(pid);
    W(snarf) = NULL;
#    ifdef DEBUG
    dprintf(e)(stderr,"%s: sending [%s]\r\n",W(tty),W(snarf));
    dprintf(c)(stderr,"sending %d->%d: %s\r\n",
    W(pid),cnt==0?0:id,message);
#    endif
    for(win2=active;win2 != (WINDOW *) 0;win2 = win2->next)
    if (cnt==0 || win2->pid==id) 
    {
      do_event(EVENT_ACCEPT,win2,E_MAIN);
      if (cnt)
      break;
    }
    break;
    /*}}}  */
    /*{{{  T_GMAP    -- load bitmap from a file*/
    case T_GMAP:
    {
      BITMAP *b;
      FILE *fp = (FILE*)0;
      char filename[MAX_PATH];
      char buff[20];
      char c = *W(snarf);

      /* make relative to icon directory */

      if (c == '/' || (c == '.' && W(snarf)[1]=='/'))
      strcpy(filename,W(snarf));
      else
      sprintf(filename,"%s/%s",icon_dir,W(snarf));

      if (W(flags)&W_DUPKEY) sprintf(buff,"%c ",W(dup)); else *buff = '\0';

      if (*W(esc) > 0 && *W(esc) < MAXBITMAPS &&
      read_ok(filename) &&
      (fp = fopen(filename,"r")) != (FILE*)0 &&
      (b = bitmapread(fp))) 
      {
        if (W(bitmaps[*W(esc)-1])) bit_destroy(W(bitmaps[*W(esc)-1]));
        W(bitmaps[*W(esc)-1]) = b;
        sprintf(buff+strlen(buff),"%d %d %d\n",BIT_WIDE(b),BIT_HIGH(b),BIT_DEPTH(b));
      }
      else 
      {
        strcat(buff,"\n");
      }
      write(W(to_fd),buff,strlen(buff));

      if (fp!=(FILE*)0) fclose(fp);
    }
    break;
    /*}}}  */
    /*{{{  T_SMAP    -- save a bitmap on a file*/
    case T_SMAP:
    {
      FILE *fp;
      BITMAP *b;
      int exists;		/* file already exists */
      int free_b = 0;
      int num = *W(esc);

      switch(cnt) 
      {
        case 1:			/* off screen bitmap */
        if (num > 0) b = W(bitmaps[num-1]); else b = screen;
        break;
        case 0:			/* my window */
        free_b++;
        b = bit_alloc(BIT_WIDE(window),BIT_HIGH(window),(DATA*)0,BIT_DEPTH(window));
        if (b) bit_blit(b,0,0,BIT_WIDE(b),BIT_HIGH(b),BIT_SRC,window,0,0);
        break;
        case 2:			/* other guy's window */
        free_b++;
        b = get_map(num,W(esc[1]));
        break;
      }

#      ifdef DEBUG
      dprintf(*)(stderr,"saving...\n");
#      endif
      if (b && W(snarf) && ((exists=access(W(snarf),0)),
      write_ok(W(snarf))) &&
      (fp = fopen(W(snarf),"w")) != (FILE*)0)
      {
#        ifdef DEBUG
        dprintf(y)(stderr,"saving bitmap %d x %d on %s (%d)\n",BIT_WIDE(b),BIT_HIGH(b),W(snarf),fileno(fp));
#        endif
        if (exists<0)	/* file just created */
        fchown(fileno(fp),getuid(),getgid());
        bitmapwrite( fp, b, get_bm_type() );
        fclose(fp);
#        ifdef DEBUG
        dprintf(y)(stderr,"saved %d on %s\n",size,W(snarf));
#        endif
      }

      if (b && free_b) bit_destroy(b);
    }
    break;
    /*}}}  */
    /*{{{  T_GIMME   -- send to process*/
    case T_GIMME:
    if (W(snarf) && *W(snarf)) 
    write_event(win,W(snarf),E_LIST_UP);
#    ifdef DEBUG
    dprintf(y)(stderr,"%s: sending [%s]\r\n",W(snarf));
#    endif
    break;
    /*}}}  */
    /*{{{  T_GRUNCH  -- graphics scrunch mode (experimental)*/
    case T_GRUNCH:
    if (W(snarf)) grunch(win,window);
#    ifdef DEBUG
    dprintf(y)(stderr,"%s: grunching [%d]\r\n",W(tty),W(esc)[cnt]);
#    endif
    break;
    /*}}}  */
    /*{{{  T_FONT    -- change a font name*/
    case T_FONT:
    if (W(esc)[0] <= MAXFONT && W(esc)[0] > 0) 
    {
      if (fontlist[W(esc[0])-1]) free(fontlist[W(esc[0])-1]);
      fontlist[W(esc[0])-1] = W(snarf);
      W(snarf) = NULL;
    }
    break;
    /*}}}  */
    /*{{{  T_BITMAP  -- down load a bitmap*/
    case T_BITMAP:				/* down-load a bitmap */
    win_map(win,window);
    W(snarf) = NULL;
    break;
    /*}}}  */
  }
  if (W(snarf)) free(W(snarf));
  W(snarf) = NULL;
}
/*}}}  */
