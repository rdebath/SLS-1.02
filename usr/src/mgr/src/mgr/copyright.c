/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
*/

/*

Flash a copyright notice at Mgr startup.  You are not allowed to remove
this, it is a part of the conditions on which you are allowed to use MGR 
without paying fees.

*/

/*
 * porter.c  Steve Hawley 4/3/87
 * rehacked 5/18/1988 for extra speed.
 * re-re hacked 6/20/88 for MGR (SAU)
*/
/*}}}  */

/*{{{  #includes*/
#include <sys/time.h>
#include <sys/signal.h>
#include <unistd.h>
#include <stdlib.h>

#include "bitblit.h"
#include "font.h"

#include "defs.h"

#include "icon_server.h"
/*}}}  */
/*{{{  #defines*/
#define SSIZE	3		/* star size */

/* index into colormap map table */
# define BG_COLOR		1	/* background color (black)*/
# define LOGO_COLOR	0	
# define CR_COLOR		2	/* copyright color */

#define MAXZ 500 /* maximum z depth */
#define MAXZ 500 /* maximum z depth */
#define NSTARS 256 /* maximum number of stars */
#define SPEED	4		/* star speed */
#define SCALE	(short)6	/* for rotator */
#define COUNT	(short)2	/* for rotator */
#define ON 1  /* plotting states */
#define OFF 0
#define Random() ((unsigned int)rand())
/*}}}  */

/*{{{  types*/
/* for "star trek" clip areas */

struct rect 
{
  short x1,y1;
  short x2,y2;
}c1,c2,c3;
/*}}}  */
/*{{{  variables*/
static BITMAP *logo[] =
{ &ball_1, &ball_2, &ball_3, &ball_4, &ball_5, &ball_6, &ball_7, &ball_8};

static struct timeval delay = 
{
  0L, 120000L
};

static short maxv, maxh; /* display size */
static short hmaxv, hmaxh;	/* 1/2 display size */

static struct st 
{
  short x, y, z;
  short color;
} stars[NSTARS]; /* our galaxy */
/*}}}  */

/*{{{  init_all*/
void init_all(where) register BITMAP *where;
{
  maxv = BIT_HIGH(where);
  hmaxv = maxv>>1;
  maxh = BIT_WIDE(where);
  hmaxh = maxh>>1;
}       
/*}}}  */
/*{{{  cordic*/
/* CORDIC rotator. Takes as args a point (x,y) and spins it */
/* count steps counter-clockwise.                   1       */
/*                                Rotates atan( --------- ) */
/*                                                  scale   */
/*                                                 2        */
/* Therefore a scale of 5 is 1.79 degrees/step and          */
/* a scale of 4 is 3.57 degrees/step                        */

static void cordic(x, y, scale, count)
short *x, *y;
register short scale, count;

{
  register short tempx, tempy;

  tempx = *x;
  tempy = *y;

  if (count > 0) /* positive count (counter-clockwise) */
  for (; count; count--)
  {
    tempx -= (tempy >> scale);
    tempy += (tempx >> scale); 
  }
  else          /* negative count (clockwise) */
  for (; count; count++)
  {
    tempx += (tempy >> scale);
    tempy += (tempx >> scale);
  }

  *x = tempx;
  *y = tempy;
}
/*}}}  */
/*{{{  xplot*/
static int xplot(where,x, y, col, state,clip1,clip2,clip3)
register BITMAP *where;
register int x, y;
register int col;
int state;
struct rect clip1, clip2,clip3;
{
  /* are we on the screen? If not, let the caller know*/
  if (x < 0 || x >= maxh || y < 0 || y >= maxv ) return(1);
  if (!(x < clip1.x1 || x >= clip1.x2 || y < clip1.y1 || y >= clip1.y2 )) return(0);
  if (!(x < clip2.x1 || x >= clip2.x2 || y < clip2.y1 || y >= clip2.y2 )) return(0);
  if (!(x < clip3.x1 || x >= clip3.x2 || y < clip3.y1 || y >= clip3.y2 )) return(0);

  bit_blit(where,x,y,SSIZE,SSIZE, state ?
  BUILDOP(BIT_SRC,col,color_map[BG_COLOR]) :
  BUILDOP(BIT_NOT(BIT_SRC),col,color_map[BG_COLOR]) ,
  (BITMAP*)0,0,0);
  return(0);
}
/*}}}  */
/*{{{  project*/
static int project(where,x, y, z, col, state,clip1,clip2,clip3)
register BITMAP *where;
register short x, y, z;
register int col;
register short state;
struct rect clip1, clip2,clip3;
{
        
  /* one-point perspective projection */
        /* the offsets (maxh/2) and maxv/2) ensure that the
         * projection is screen centered
  */
  x = (x/z) + hmaxh;
  y = (y/z) + hmaxv;
  return(xplot(where,x, y, col, state,clip1,clip2,clip3));

}
/*}}}  */
/*{{{  fly*/
static void fly (where,clip1,clip2, clip3) BITMAP *where; struct rect clip1, clip2, clip3; /* "holes" in galaxy */
{
  register short i;
  register struct st *stp;

  init_all(where);     /* set up global variables */
  for (i=0,stp=stars; i<NSTARS; i++,stp++)
  {
    /* initialize galaxy */
    do 
    {
      stp->x = Random();
      stp->y = Random();
      stp->z = (Random() % MAXZ) + 1;
      stp->color = Random() & ((1 << BIT_DEPTH(where))-1);
    } while(project(where,stp->x, stp->y, stp->z, stp->color, ON,clip1,clip2,clip3)); /* on screen? */
  }
}
/*}}}  */
/*{{{  dofly*/
static void dofly (where,clip1,clip2, clip3) BITMAP *where; struct rect clip1, clip2, clip3; /* "holes" in galaxy */
{
  register short i;
  register struct st *stp;

  i = NSTARS;
  stp = stars;
  do 
  {
    project(where,stp->x, stp->y, stp->z, stp->color, OFF,clip1,clip2,clip3); /* turn star off*/
    if ((stp->z -= SPEED) <= 0) { /* star went past us */
      stp->x = Random();
      stp->y = Random();
      stp->z = MAXZ;
    }
    else {		/* rotate universe */
      cordic(&stp->x,&stp->y,SCALE,COUNT);
    }
    if (project(where,stp->x, stp->y, stp->z, stp->color, ON,clip1,clip2,clip3)) 
    {
      /* if projection is off screen, get a new position */
      stp->x = Random();
      stp->y = Random();
      stp->z = MAXZ;
    }
    ++stp;
  } while(--i);
}
/*}}}  */

/*{{{  copyright*/
void copyright(where) BITMAP *where;
{
  BITMAP *notice = &cr;
  fd_set mask;
  register int i;
  char c;

  /* get the cr notice hole */

  c1.x1 = (BIT_WIDE(where)-BIT_WIDE(notice))/2 - SSIZE;
  c1.y1 = (BIT_HIGH(where))/2 - SSIZE + 80;
  c1.x2 = c1.x1 + SSIZE + BIT_WIDE(notice);
  c1.y2 = c1.y1 + SSIZE + BIT_HIGH(notice);

  /* clear display */
        
  bit_blit(where,0,0,BIT_WIDE(where),BIT_HIGH(where),~BIT_SRC,(BITMAP*)0,0,0);
  bit_blit(where,c1.x1+SSIZE,c1.y1+SSIZE,BIT_WIDE(notice),BIT_HIGH(notice),BUILDOP(BIT_SRC,color_map[CR_COLOR],color_map[BG_COLOR]),notice,0,0);

  /* get the globe hole */

  c2.x1 = (BIT_WIDE(where)-BIT_WIDE(logo[0]))/2-SSIZE;
  c2.y1 = (BIT_HIGH(where)-BIT_HIGH(logo[0]))/2-SSIZE-80;
  c2.x2 = c2.x1 + SSIZE + BIT_WIDE(logo[0]);
  c2.y2 = c2.y1 + SSIZE + BIT_HIGH(logo[0]);

  /* get the message hole */

#ifdef MESSAGE
  c3.x1 = 10 - SSIZE;
  c3.y1 = BIT_HIGH(where) - font->head.high - 10 - SSIZE;
  c3.x2 = 10 + 2*SSIZE + strlen(MESSAGE) * font->head.wide;
  c3.y2 = BIT_HIGH(where) - 10 + 2*SSIZE;
  put_str(where,10,c3.y2-SSIZE,font,BIT_SRC,MESSAGE);
#else
  c3 = c2;
#endif

  /* kick off stars */

  fly(where,c1,c2,c3);
  FD_ZERO(&mask);
  FD_SET(fileno(stdin),&mask);
  i=0;
  while (select(FD_SETSIZE,&mask,(fd_set*)0,(fd_set*)0,&delay)<=0)
  {
    dofly(where,c1,c2,c3);
    bit_blit(where,c2.x1+SSIZE,c2.y1+SSIZE,BIT_WIDE(logo[0]),BIT_HIGH(logo[0]),BUILDOP(BIT_NOT(BIT_SRC),color_map[LOGO_COLOR],color_map[BG_COLOR]),logo[i%8],0,0);
    i++;
    FD_SET(fileno(stdin),&mask);
  }
  read(fileno(stdin),&c,sizeof(char));
#ifdef HMMMM
  bit_destroy(notice);
  for(i=0;i<8;i++) bit_destroy(logo[i]);
#endif
}
/*}}}  */
