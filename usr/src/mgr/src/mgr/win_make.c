/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* make an alternate window */

#include <stdio.h>

#include "bitblit.h"
#include "font.h"

#include "defs.h"
#include "event.h"

#include "border.h"
#include "font_subs.h"
#include "new_window.h"

/* manipulte an alternate (client) window - called by put_window() */

int
win_make(win,indx)
WINDOW *win;			/* window issuing make-call */
int indx;			/* current index into char string (yuk!) */
   {
   register int *p = W(esc);	/* array of ESC digits */
   int buff[10];
   WINDOW *win2=win;

   switch (W(esc_cnt)) {
      case 1:			/*  destroy the window */
#ifdef DEBUG
         dprintf(N)(stderr,"%s: destroying %d\n",W(tty),p[0]);
#endif
         if (p[0]<=0 || W(main)->alt == (WINDOW *) 0) {
            break;
            }
         for (win = W(main)->alt;win!=(WINDOW *) 0; win=W(alt))  {
            if (W(num)==p[0])
               break;
            }
         if (win != (WINDOW *) 0)
            W(flags) |= W_DIED;

         break;

      case 0:		/* goto a new window */
         if (W(num)==p[0] || W(main)->alt == (WINDOW *) 0) {
            break;
            }
         for (win = W(main);win!=(WINDOW *) 0; win=W(alt))  {
            if (W(num)==p[0])
               break;
            }

         /* move contents of shell buffer to new window */

         if (win != (WINDOW *) 0) {
            W(from_fd) = W(to_fd);
            win2->from_fd = 0;
            W(max) = win2->max - win2->current - indx - 1;
            bcopy(win2->buff + win2->current + indx + 1,W(buff),W(max));
            W(current) = 0;
#ifdef DEBUG
            dprintf(N)(stderr,"%s: xfer %d\r\n",W(tty),W(max));
#endif
            }
         break;

      case 3:		/* make a new window */
         p[4] = -1;
         /* no break */
      case 4:		/* new window + specify window number */
#ifdef DEBUG
         dprintf(N)(stderr,"%s: making alternate window\n",W(tty));
#endif
         if (check_window(p[0],p[1],p[2],p[3],-1) == 0) {
				if (ACTIVE(flags)&W_DUPKEY)
					sprintf(buff,"%c \n",ACTIVE(dup));
				else
					sprintf(buff,"\n");
				write(ACTIVE(to_fd),buff,strlen(buff));
            break;
            }

         if (win!=active)
            cursor_off();
         ACTIVE_OFF();
         if ((active = insert_win((WINDOW *) 0)) == (WINDOW *) 0 ||
                     !setup_window(active,font,p[0],p[1],p[2],p[3])) {
            fprintf(stderr,"Out of memory for window creation -- bye!\n");
            quit();
            }

         /* next_window++;  (this needs more thought) */

         /* make the window */

         set_covered(active);
         border(active,BLK_BDR,WH_BDR);
         CLEAR(ACTIVE(window),BIT_CLR);
         ACTIVE_ON();
         cursor_on();

#ifdef DEBUG
         dprintf(N)(stderr,"%s: window created\n",W(tty));
#endif
         /* fix pointer chain */

         ACTIVE(to_fd) =  W(to_fd);
         ACTIVE(main) = W(main);
         ACTIVE(pid) =  W(pid);
         ACTIVE(setid) =  W(setid);
         strcpy(ACTIVE(tty),ACTIVE(main)->tty);
         ACTIVE(from_fd) = 0;
         ACTIVE(alt) =  W(main)->alt;
         ACTIVE(main)->alt = active;
			if (p[4] > 0)
            ACTIVE(num) = p[4];
         else if (ACTIVE(alt))
            ACTIVE(num) = ACTIVE(alt)->num + 1;
         else
            ACTIVE(num) = 1;

#ifdef DEBUG
         dprintf(N)(stderr,"%s: created num %d\r\n",ACTIVE(tty),ACTIVE(num));
#endif
         if (W(flags)&W_DUPKEY)
            sprintf(buff,"%c %d\n",W(dup),ACTIVE(num));
         else
            sprintf(buff,"%d\n",ACTIVE(num));
         write(ACTIVE(to_fd),buff,strlen(buff));
         clip_bad(active);	/* invalidate clip lists */
         break;
      case 5:		/* nothing */
         break;
     }
   }
