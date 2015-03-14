/*{{{}}}*/
/*{{{  #includes*/
#include <sys/time.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#include "term.h"
/*}}}  */

/*{{{  #defines*/
#define MAXX	999
#define MAXY	999
#define MAXV	60
#define MINV	20
#define LCT	10
#define SLOW	60000		/* usec to sleep between lines */

#define fsleep(x) \
{ \
  struct timeval time; \
  time.tv_sec = 0; \
  time.tv_usec = x; \
  select(0,0,0,0,&time); \
}
/*}}}  */

/*{{{  variables*/
int vx1, vy1, vx2, vy2;
int x1, y1, x2, y2;
int thex1[LCT];
int they1[LCT];
int thex2[LCT];
int they2[LCT];
int ptr;
int lcolor,bcolor;
/*}}}  */

/*{{{  diddle*/
void diddle(ptr)
int *ptr;
{
  int tmp;
	/*
	**	pick a number between MAXV and MINV
  */
  tmp = (rand()% (MAXV-MINV)) + MINV;
	/*
	**	 and get the sign right
  */
  if (*ptr < 0)
  *ptr = -tmp;
  else
  *ptr = tmp;
}
/*}}}  */
/*{{{  mvpoint*/
void mvpoint(tx,ty,v_x,v_y) int *tx,*ty,*v_x,*v_y;
{
  *tx += *v_x;	/* move the point */
  *ty += *v_y;

  if ( *tx >= MAXX) 	/* bounce */
  {
    *v_x = (*v_x > 0) ? -(*v_x) : *v_x;
    diddle(v_x);
  }
  if ( *ty >= MAXY)
  {
    *v_y = (*v_y > 0) ? -(*v_y) : *v_y;
    diddle(v_y);
  }

  if ( *tx <= 0)
  {
    *v_x = (*v_x < 0) ? -(*v_x) : *v_x;
    diddle(v_x);
  }
  if ( *ty <= 0)
  {
    *v_y = (*v_y < 0) ? -(*v_y) : *v_y;
    diddle(v_y);
  }
}
/*}}}  */

/*{{{  main*/
int main(int argc, char *argv[])
{
  int sleep = 0;

  ckmgrterm( *argv );

  m_setup(0);
  m_push(P_EVENT|P_FLAGS);
  srand(getpid());
  vx1 = 50;
  vy1 = 50;
  x1 = 500;
  y1 = 1;
  vx2 = -50;
  vy2 = -50;
  x2 = 500;
  y2 = MAXY;

  if (argc>1 && strcmp(argv[1],"-s")==0) sleep++;

  m_clearmode(M_BACKGROUND);
  for (ptr=0;ptr<LCT;ptr++)
  {
    thex1[ptr] = they1[ptr] = thex2[ptr] = they2[ptr] = -1;
  }

  bcolor = rand()%24;
  while((lcolor=rand()%24) == bcolor);
  m_bcolor(bcolor);
  m_fcolor(lcolor);
  m_linecolor(BIT_XOR,lcolor);
  m_clear();
  for(;;)
  {
    ptr = (ptr+1) % LCT;
    if (thex1[ptr] >= 0)
    m_line(thex1[ptr],they1[ptr],thex2[ptr],they2[ptr]);

    mvpoint(&x1,&y1,&vx1,&vy1);
    mvpoint(&x2,&y2,&vx2,&vy2);
    thex1[ptr] = x1;
    they1[ptr] = y1;
    thex2[ptr] = x2;
    they2[ptr] = y2;
			
    if (thex1[ptr] >= 0) m_line(thex1[ptr],they1[ptr],thex2[ptr],they2[ptr]);
    m_flush();
    if (sleep) fsleep(90000);
  }
}
/*}}}  */
