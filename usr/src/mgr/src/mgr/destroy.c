/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* destroy a window */
/*}}}  */

/*{{{  #includes*/
#include <sys/wait.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#include "bitblit.h"
#include "font.h"
#include "window.h"

#include "defs.h"
#include "event.h"

#include "border.h"
#include "do_event.h"
#include "erase_win.h"
#include "font_subs.h"
#include "get_menus.h"
/*}}}  */
/*{{{  #defines*/
#define ALL	1
/*}}}  */

/*{{{  detach -- unlink an alternate window from list*/
static void detach(win2) WINDOW *win2;
   {
   register WINDOW *win = win2;

   if (!(W(main)))
      return;
   for(win=win2->main;W(alt)!=win2;win=W(alt))
      ;
   W(alt)= win2->alt;
   }
/*}}}  */
/*{{{  set_dead -- notify alternate windows of impending death*/
static void set_dead(win) register WINDOW *win;
   {
   for(win = W(alt); win != (WINDOW *) 0; win = W(alt)) {
#ifdef DEBUG
      dprintf(d)(stderr,"%s: telling %d\r\n",W(tty),W(num));
#endif
      W(main) = (WINDOW *) 0;
      }
   }
/*}}}  */

/*{{{  unlink_win -- free all space associated with a window*/
void unlink_win(win,how) register WINDOW *win;		/* window to unlink */
int how;			/* if how, unlink window stack as well */
   {
   register int i;

#ifdef DEBUG
   dprintf(u)(stderr,"Unlinking %s %s\n",W(tty),how?"ALL":"");
#endif

   if (how && W(stack))
      unlink_win(W(stack),how);
   if (W(window))
       bit_destroy(W(window));
   for(i=0; i< MAXBITMAPS;i++)
      if (W(bitmaps)[i])
          bit_destroy(W(bitmaps)[i]);
   if (W(border))
       bit_destroy(W(border));
   if (W(save))
       bit_destroy(W(save));
   if (W(snarf))
      free(W(snarf));
   if (W(bitmap))
      free(W(bitmap));
   zap_cliplist(win);

   for(i=0; i< MAXEVENTS;i++)
       if (W(events)[i])
          free(W(events)[i]);

   for(i=0; i< MAXMENU;i++) 
      if (W(menus)[i])
         menu_destroy(W(menus)[i]);

   free(win);
	win=NULL;
   }
/*}}}  */
/*{{{  destroy -- destroy a window*/
int destroy(win) register WINDOW *win;
   {
   int i;
   int status;

   if (win == (WINDOW *) 0)
      return(-1);

   MOUSE_OFF(screen,mousex,mousey);
   cursor_off();

   if (win != active) {
      ACTIVE_OFF();
      expose(win);
      }

   active = W(next);

   /* deallocate window slot */

   if (active)
      ACTIVE(prev) = W(prev);

   /* remove window from screen */

   erase_win(W(border));

   if (W(main)==win) {		/* kill process associated with the window */
#ifdef DEBUG
      dprintf(d)(stderr,"%s: destroy main %s\r\n",W(tty),W(alt)?"ALT":"");
#endif
      if (W(pid)>1) killpg(W(pid),SIGHUP);

      if (geteuid() < 1) {
         chmod(W(tty),0666);
         chown(W(tty),0,0);
         }

      close(W(to_fd));
      mask &= ~(1<<W(to_fd));
      poll &= ~(1<<W(to_fd));
#ifdef WHO
      rm_utmp(W(tty));
#endif

      /* tell alternate windows main died */

      set_dead(win);

      /* wait for shell to die */

#ifdef DEBUG
      dprintf(d)(stderr,"waiting for ..."); fflush(stderr);
#endif
      if (W(pid)>1 && !(W(flags)&W_DIED)) {
         i = wait3(&status,WNOHANG,0L);
         if (i == 0) { 					/* start it so it can die */
            kill(W(pid),SIGCONT);
            i = wait3(&status,WNOHANG,0L);
		if (i==0) fprintf(stderr,"MGR: Wait for %d failed\n",W(pid));
            }
         }
#ifdef DEBUG
      dprintf(d)(stderr," %d\r\n",i);
#endif
      next_window--; 
      }

   else if (W(main) && !(W(main)->flags&W_DIED)) {	/* main still alive */
#ifdef DEBUG
      dprintf(d)(stderr,"%s: destroy alt %d\r\n",W(tty),W(num));
#endif
      do_event(EVENT_DESTROY,win,E_MAIN);
      if (W(from_fd)) {		/* re-attach output to main window */
         W(main)->from_fd = W(main)->to_fd;
         W(main)->max = W(max) - W(current); /* ??? */
#ifdef DEBUG
      dprintf(d)(stderr,"%s: copy %d chars at %d\r\n",
               W(main)->max, W(current));
#endif
         bcopy(W(buff)+W(current)+1,W(main)->buff,W(main)->max);
         W(main)->current = 0;
#ifdef DEBUG
         dprintf(d)(stderr,"%s: reattaching main %d chars\r\n",W(tty),W(max));
#endif
         }
      detach(win);
      }
   else if (W(main)) {		/* tell main alts know they are dead */
      W(main)->alt = (WINDOW *) 0;
#ifdef DEBUG
      dprintf(d)(stderr,"%s: destroy alt, (tell main)\r\n",W(tty));
#endif
      }
   else {
#ifdef DEBUG
      dprintf(d)(stderr,"%s: destroy alt, (dead main)\r\n",W(tty));
#endif
      }

   /* fix up display if any windows left */

   if (active) {
      repair(win);
      un_covered();
      clip_bad(win);	/* invalidate clip lists */
      ACTIVE_ON();
      cursor_on();
      }

   /* free space associated with dead window */

   unlink_win(win,ALL);

#ifdef DEBUG
   dprintf(d)(stderr,"Active: %s-%d\r\n",
          active?ACTIVE(tty):"NONE", active?ACTIVE(num):0);
#endif

   MOUSE_ON(screen,mousex,mousey);

   return(0);
   }
/*}}}  */
/*{{{  destroy_window -- mark active window for destruction*/
int destroy_window()
   {
   ACTIVE(flags) |= W_DIED;
   return 0;
   }
/*}}}  */
