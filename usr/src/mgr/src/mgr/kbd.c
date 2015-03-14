/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
*/

/*

Very ugly stuff to use machine dependent things.

*/
/*}}}  */

/*{{{  #includes*/
#include <sys/time.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <signal.h>
#include <stdio.h>
#ifdef KBD
#ifdef sun
#define u_char unsigned char
#define u_short unsigned short
#include <sundev/kbio.h>
#include <sys/ioctl.h>
#endif
#endif

#include "bitblit.h"

#include "defs.h"
/*}}}  */

/*{{{  variables*/
#ifdef BELL
static int ring = 0;
static int bell_fd = -1;
#endif
#ifdef KBD
static int kbd_fd = -1;
#endif
/*}}}  */

/*{{{  initbell -- initialize bell*/
void initbell()
{
#ifdef BELL
#ifdef sun
  int i;
  if ( !debug  &&  (i=open("/dev/bell",1))>=0 )
  bell_fd = i;
  else
  bell_fd = -1;
#endif
#endif
}
/*}}}  */
/*{{{  bell_on -- turn on the bell*/
#ifdef BELL
/*{{{  set_timer*/
static void
set_timer(time)
int time;		/* time in 100'th of seconds */
{
  struct	itimerval new,old;

  new.it_interval.tv_sec = 0L;
  new.it_interval.tv_usec = 0L;
  new.it_value.tv_sec = time/100;
  new.it_value.tv_usec = (time%100) * 10000;

  setitimer(ITIMER_REAL,&new,&old);
}
/*}}}  */
#endif
#ifdef BELL
/*{{{  bell_off*/
int
bell_off(n)
int n;			/* signal #, ignored */
{
#ifdef sun
  write(bell_fd,"\003",1);	/* turns the bell off */
#endif
  set_timer(0);		/* insure timer is off */
  ring = 0;
}
/*}}}  */
#endif

void bell_on()
{
  #ifdef BELL
  #ifdef sun
  if( bell_fd >= 0 )
  {
    if (ring==0)
    write( bell_fd, "\002", 1 );
    signal( SIGALRM, bell_off );
    set_timer(15);		/* set alarm for 100'th seconds */
    ring++;
    return;
  }
  #endif
  #endif
  /*
  We are looking for some way to make a noise when ever possible.
  This line breaks when the console is redirected to an mgr window and
  mgr's stderr (fd 2) is the console.

  write( 2, "\007", 1 );
  */
}
/*}}}  */

/*{{{  kbd_reset -- reset the keyboard*/
void kbd_reset()
{
  #ifdef BELL
  #ifdef sun
  if( bell_fd >= 0 ) write(bell_fd,"\001",1);	/* this resets the kbd and turns bell off */
  set_timer(0);		/* insure timer is off */
  ring = 0;
  #endif
  #endif
}
/*}}}  */

/*{{{  set_kdb -- set/reset direct mode*/
/* When setting direct mode, returns file descriptor of keyboard */
int set_kbd(how) int how;	/* 1=direct, 0=no direct */
{
  #ifdef KBD
  #ifdef sun
  int one = 1;
  int zero = 0;
  int cons;

  #ifndef  KBD_CMD_RESET
  #define  KBD_CMD_RESET	0x01	/* Should be in a header file, but .. */
  #endif
  if (how == 0)
  { /* make sure kbd is released */
    if (kbd_fd == -1)
    return(-1);
    ioctl(kbd_fd, KIOCSDIRECT, &zero);	/* turn off direct mode */
    ioctl(kbd_fd, KIOCCMD, KBD_CMD_RESET);	/* reset the keyboard */
    close(kbd_fd);				/* close the keyboard */
    cons = open("/dev/console", 0);	/* put console messages back */
    ioctl(cons, TIOCCONS, &one);
    close(cons);
    return(0);
  }
  else {		/* open the kbd for input */
    kbd_fd = open("/dev/kbd", 0);
    ioctl(kbd_fd, KIOCSDIRECT, &one);		/* turn on direct mode */
    ioctl(kbd_fd, KIOCCMD, KBD_CMD_RESET);	/* reset the keyboard */
  }
  return(kbd_fd);
  #endif
  #else
  return(0);
  #endif
}
/*}}}  */
/*{{{  initkbd -- initialize the keyboard, especially when it is a separate device*/
void initkbd()
{
  #ifdef KBD
  #ifdef sun
  int fd;

  if ((fd = set_kbd(1)) > 0)
  {
    kbd_fd = 0; 
    close(kbd_fd);
    dup(fd);
    close(fd);
  }
  else if( fd == 0 )
  return;
  else  
  {
    #ifdef DEBUG
    if( debug )
    fprintf(stderr,"Can't find keyboard, using stdin\n");
    #endif
    kbd_fd = -1;
  }
  #endif
  #endif
}
/*}}}  */
