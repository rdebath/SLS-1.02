/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* star-trek lock screen (sau/sdh) */

#ifdef SGTTY
#include <sgtty.h>
#endif
#ifdef _POSIX_SOURCE
#include <termios.h>
#endif
#include <stdio.h>
#include <pwd.h>
#include <sys/time.h>
#include <signal.h>
#include <string.h>

#include "bitmap.h"

#define WIDE	BIT_WIDE(display)
#define HIGH	BIT_HIGH(display)
#define NICE	10

#define BG_COLOR	4		/* usually blue */

struct passwd *pwd, *getpwuid();
struct timeval poll = {0,50000};
#ifdef SGTTY
struct sgttyb sg,save;
#endif
#ifdef _POSIX_SOURCE
struct termios sg,save;
#endif
char buff[100];
static int dir = 1;

main()
   {
   int read;
	int pid;
	int flop();
   BITMAP *display = bit_open(SCREEN_DEV);
   BITMAP *stash = bit_alloc(WIDE,HIGH,NULL,1);

   if (!(display&&stash)) {
      printf("Can't initialize the display, sorry\n");
		exit(1);
      }

   pwd = getpwuid(getuid());

   signal(SIGINT,SIG_IGN);
   signal(SIGHUP,SIG_IGN);
   signal(SIGTERM,SIG_IGN);
   signal(SIGQUIT,SIG_IGN);
   signal(SIGTSTP,SIG_IGN);

#ifdef _POSIX_SOURCE
   tcgetattr(0,&sg);
   save = sg;
   sg.c_iflag=BRKINT;
   sg.c_lflag=ISIG;
   memset(sg.c_cc,255,NCCS);
   sg.c_cc[VTIME]=0;
   sg.c_cc[VMIN]=1;
   tcsetattr(0,TCSANOW,&sg);
#endif
#ifdef SGTTY
   gtty(0,&sg);
   save = sg;
   sg.sg_flags &= ~(ECHO|RAW);
   sg.sg_flags |= CBREAK;
   stty(0,&sg);
#endif

   bit_blit(stash,0,0,WIDE,HIGH,BIT_SRC,display,0,0);
   bit_blit(display,0,0,WIDE,HIGH,BIT_SRC|GETCOLOR(BG_COLOR),NULL,0,0);
   if (NICE > 0)
      nice(NICE);
	if ((pid=fork()) == 0) {/* child */
		signal(SIGALRM,flop);
		fly(display);
		}
   else {
		while(1) {
			read = 1;
			if (select(32,&read,0,0,&poll) && read) {
				fgets(buff,sizeof(buff),stdin);
				kill(pid,SIGALRM);	/* change directions */
				if (strcmp(pwd->pw_passwd,crypt(buff,pwd->pw_passwd)) == 0) {
#ifdef SGTTY
					stty(0,&save);
#endif
#ifdef _POSIX_SOURCE
                                        tcsetattr(0,TCSANOW,&save),
#endif
					kill(pid,SIGKILL);
					while(wait(0)!=pid);
					bit_blit(display,0,0,WIDE,HIGH,BIT_SRC,stash,0,0);
                                        if (strcmp(getenv("TERM"),"mgr")) bit_destroy(display);
					exit(0);
					}
				}
			}
		}
	}

/* star trek effect */
/*
 * porter.c  Steve Hawley 4/3/87
 * rehacked 5/18/1988 for extra speed.
 * re-re hacked 6/20/88 for MGR (SAU)
 * A demo to get stars inspired by Star Trek without
 * using quickdraw, and by addressing the screen directly.
 * this version is roughly 8 times faster than its quickdraw
 * equivalent.
 * In considering the bit drawing routines, remember that
 * on the macintosh, a bit turned on is black, not white.
 */

#define SSIZE	2	/* star size */
#define MAXZ 500 /* maximum z depth */
#define NSTARS 256 /* maximum number of stars */
#define SPEED	6		/* star speed */
#define SCALE	(short)7	/* for rotator */
#define COUNT	(short)3	/* for rotator */
#define ON 1  /* plotting states */
#define OFF 0
#define Random() rand()

short maxv, maxh; /* display size */
short hmaxv, hmaxh;	/* 1/2 display size */

struct st {
   short x, y, z;
	short color;
	short dir;
   } stars[NSTARS]; /* our galaxy */

fly (where)
BITMAP *where;
{
        register short i;
        register struct st *stp;

        init_all(where);     /* set up global variables */
        for (i=0, stp = stars; i< NSTARS; i++, stp++) {
                /* initialize galaxy */
                do {
                        stp->x = Random();
                        stp->y = Random();
                        stp->z = (Random() % MAXZ) + 1;
                        stp->dir = dir ? COUNT : -COUNT;
                        stp->color = Random() % 23;
								if (stp->color == BG_COLOR)
                        	stp->color++;
			
                } while(project(where,stp->x, stp->y, stp->z, stp->color, ON)); /* on screen? */
        }
        while (1) { /* loop 'til button */
                i = NSTARS;
                stp = stars;
                do {
                        project(where,stp->x, stp->y, stp->z, stp->color, OFF); /* turn star off*/
                        if ((stp->z -= SPEED) <= 0) { /* star went past us */
                                stp->x = Random();
                                stp->y = Random();
                                stp->z = MAXZ;
                                stp->dir = dir ? COUNT : -COUNT;
                        }
								else {		/* rotate universe */
									cordic(&stp->x,&stp->y,SCALE,stp->dir);
								}
                        if (project(where,stp->x, stp->y, stp->z, stp->color, ON)) {
                                /* if projection is off screen, get a new position */
                                stp->x = Random();
                                stp->y = Random();
                                stp->z = MAXZ;
                                stp->dir = dir ? COUNT : -COUNT;
                        }
                ++stp;
                } while(--i);
        }
}

project(where,x, y, z, col, state)
register BITMAP *where;
register short x, y, z;
register int col;
register short state;
{
        
        /* one-point perspective projection */
        /* the offsets (maxh/2) and maxv/2) ensure that the
         * projection is screen centered
         */
		x = (x/z) + hmaxh;
		y = (y/z) + hmaxv;
        return(xplot(where,x, y, z, col, state));

}

init_all(where)
register BITMAP *where;
{
   maxv = BIT_HIGH(where);
   hmaxv = maxv>>1;
   maxh = BIT_WIDE(where);
   hmaxh = maxh>>1;
}       

xplot(where,x, y, z, col, state)
register BITMAP *where;
register int x, y;
register int z;	/* actually z position */
register int col;
int state;
{
        register int size = SSIZE;
        /* are we on the screen? If not, let the caller know*/
        if (x < 0 || x >= maxh || y < 0 || y >= maxv )
            return(1);

      if (z > (3*MAXZ/4))
         size--;
      else if ( z< (MAXZ/4))
         size++;
      bit_blit(where,x,y,size,size, state ?
		BIT_SRC^BIT_DST | GETCOLOR(col)^GETCOLOR(BG_COLOR) :
		BIT_SRC | GETCOLOR(BG_COLOR),
                 0l,0,0);
        return(0);
}

/* CORDIC rotator. Takes as args a point (x,y) and spins it */
/* count steps counter-clockwise.                   1       */
/*                                Rotates atan( --------- ) */
/*                                                  scale   */
/*                                                 2        */
/* Therefore a scale of 5 is 1.79 degrees/step and          */
/* a scale of 4 is 3.57 degrees/step                        */

cordic(x, y, scale, count)
short *x, *y;
register short scale, count;

{
   register short tempx, tempy;

   tempx = *x;
   tempy = *y;

   if (count > 0) /* positive count (counter-clockwise) */
      for (; count; count--){
         tempx -= (tempy >> scale);
         tempy += (tempx >> scale); 
      }
   else          /* negative count (clockwise) */
      for (; count; count++){
         tempx += (tempy >> scale);
         tempy -= (tempx >> scale);
      }

   *x = tempx;
   *y = tempy;
}

int flop()
   {
   dir = 1-dir;
	}
