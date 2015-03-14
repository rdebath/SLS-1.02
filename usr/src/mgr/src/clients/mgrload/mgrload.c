/*{{{}}}*/
/*{{{  Notes*/
/*
mgrload - show cpu load average in an mgr window.

Original version by: Mark Dapoz 90/06/21, mdapoz@hybrid.UUCP or mdapoz%hybrid@cs.toronto.edu
Heavily edited by: Michael Haardt
*/
/*}}}  */
/*{{{  #includes*/
#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <sys/utsname.h>

#include "term.h"

#include "getload.h"
/*}}}  */
/*{{{  #defines*/
#ifndef INTERVAL
#define INTERVAL        5      /* time interval (sec) between samples*/
#endif
#ifndef PSIZE
#define PSIZE           50      /* size of one display partition; .50 proc */
#endif

#define CONTEXT P_POSITION | P_WINDOW | P_FLAGS | P_EVENT | P_CURSOR | P_MENU
/*}}}  */

/*{{{  variables*/
int xscale, yscale, rscale;
int xmin, xmax, ymin, ymax;
int x,y;
/*}}}  */

/*{{{  max*/
static int max(nums) /* find maximum load avarage in queue */
int *nums;
{
  static int i,j;

  for (i=j=0; i < xmax; nums++, i++)
  j=*nums > j ? *nums : j;
  return(j);
}
/*}}}  */
/*{{{  min*/
static int min(nums) /* find minimum load avarage in queue */
int *nums;
{
  static int i,j;

  for (i=0,j=99999; i < xmax; nums++, i++)
  j=*nums && *nums < j ? *nums : j; /* ignore zero values */
  return(j);
}
/*}}}  */
/*{{{  draw_scale -- draw scale lines on the graph*/
static void draw_scale(sections, width) int sections; int width;
{
  int i,j;

  m_func(BIT_XOR);
  for (i=1; i < sections; i++)
  {
    /* draw scale lines */
    j=ymax-i*PSIZE*yscale/rscale;
    m_line(0, j, width-1, j);
  }
}
/*}}}  */
/*{{{  draw_bar*/
void draw_bar(int load,int column)
{
  int foo=load*yscale/rscale;

  if (column==0) { m_func(BIT_CLR); m_bitwrite(0, 0, 1, ymax-foo); }
  m_func(BIT_SET); m_bitwrite(column,ymax-foo,1,foo);
}
/*}}}  */
/*{{{  redraw -- redraw graph from history*/
static void redraw(nums, head) int *nums; int head;
{
  register int j,p;

  m_clear();
  for (p=xmax-1, j=head+1; p >= 0; p--)
  {
    j=j%xmax;
    if (nums[j]) draw_bar(nums[j],p);
    j++;
  }
}
/*}}}  */
/*{{{  timer_event*/
static void timer_event() /* cause the load average to be sampled */
{
  m_sendme("S\n"); /* sample load average event */
  signal(SIGALRM, timer_event);
}
/*}}}  */
/*{{{  done*/
static void done() /* general purpose exit */
{
  m_ttyreset(); /* reset communication channel */
  m_popall(); /* restore window */
  exit(0);
}
/*}}}  */

/*{{{  main*/
int main(argc,argv) int argc; char **argv;
{
  /*{{{  variables*/
  int *samples;           /* circular queue of samples */
  int head=0;                     /* queue pointer */
  int partitions = 0, last_part = 0;
  char event[80];              /* mgr event queue */
  char *options, scale[16], stats[16*3], 
  	hostname[sizeof(struct utsname)+32];
  struct utsname unixname;
  /*}}}  */

  /*{{{  check if an mgr terminal*/
  ckmgrterm(*argv);
  /*}}}  */
  /*{{{  init mgr*/
  m_setup(M_MODEOK);
  m_push(CONTEXT);
  m_setmode(M_ABS);
  m_setcursor(CS_INVIS);
  m_ttyset();
  /*}}}  */
  /*{{{  init variables*/
  xmin = 0;       /* mgr virtual window size */
  ymin = 0;
  get_size(&x,&y,&xmax,&ymax);
  xscale = xmax-xmin;
  yscale = ymax-ymin;
  samples=malloc(sizeof(int)*xmax);
  memset(samples, 0, xmax*sizeof(int));
  if (uname(&unixname) < 0)
  {
    perror("mgrload: unable to get system name: ");
    exit(1);
  }
  /*}}}  */
  /*{{{  set up signal handlers*/
  signal(SIGALRM, timer_event);
  signal(SIGTERM, done);
  signal(SIGINT, done);
  signal(SIGQUIT, done);
  signal(SIGHUP, done);
  /*}}}  */
  /*{{{  set up mgr events*/
  m_setevent(ACTIVATE,  "A\n");
  m_setevent(COVERED,   "C\n");
  m_setevent(UNCOVERED, "U\n");
  m_setevent(RESHAPE,   "H\n");
  m_setevent(REDRAW,    "R\n");
  /*}}}  */
  /*{{{  set up menus*/
  options="|stats ->|scale ->|refresh|reset|||R\n|X\n|";
  m_loadmenu(1, options); /* top level options */
  sprintf(scale, "|top = %3.2f||", 0.0); /* current scale */
  m_loadmenu(2, scale); /* load second level menu */
  m_linkmenu(1, 1, 2, MF_AUTO); /* link to main menu */
  sprintf(stats,"|max  = %3.2f|min  = %3.2f|last = %3.2f||||",0.0, 0.0, 0.0);
  m_loadmenu(3, stats);
  m_linkmenu(1, 0, 3, MF_AUTO); /* link stats to main menu */
  m_selectmenu2(1); /* bind menu to right button */
  sprintf(hostname, "|%s|%s %s %s %s|||", unixname.nodename,unixname.sysname, unixname.release, unixname.version,unixname.machine);
  m_loadmenu(10, hostname);
  m_selectmenu(10); /* bind host info to middle button */
  /*}}}  */
  /*{{{  set up window and start first event*/
  m_clear();
  m_sendme("S\n");
  /*}}}  */
  while (1)
  {
    m_flush();
    /*{{{  get event*/
    if (m_gets(event) == (char*)0)
    /* restart interrupted call */
    if (errno == EINTR) continue; else break;
    /*}}}  */
    alarm(0);
    switch (*event)
    {
      /*{{{  A -- activate, C -- covered, U -- uncovered*/
      case 'A':       /* window activated */
      case 'C':       /* window covered */
      case 'U':       /* window uncovered */
      break;
      /*}}}  */
      /*{{{  X -- reset stored samples*/
      case 'X':       /* reset stored samples */
      memset(samples, 0, sizeof(samples));
      head=0;
      m_sendme("R\nS\n"); /* force redraw and sample */
      break;
      /*}}}  */
      /*{{{  R -- redraw, H -- reshape*/
      case 'R':       /* redraw window */
      case 'H':       /* reshape window */

      m_shapewindow(x,y,xmax,ymax);
      redraw(samples, head-1 < 0 ? xmax-1 : head-1);
      draw_scale(partitions, xmax);
      break;
      /*}}}  */
      /*{{{  S -- get load average*/
      case 'S':
      samples[head]=(int)(getload()*100); /* 1 min avg */
      partitions=max(samples)/PSIZE+1;
      sprintf(stats,"|max  = %3.2f|min  = %3.2f|last = %3.2f||||",max(samples)/100.0, min(samples)/100.0,samples[head]/100.0);
      m_loadmenu(3, stats); /* update pop up menu info */
      rscale=partitions*PSIZE;
      if (last_part == partitions)
      /*{{{  fits on last scale*/
      {
        /* scroll graph right */
        m_func(BIT_SRC); m_bitcopy(1, 0, xmax-1, ymax, 0, 0);
        draw_bar(samples[head],0);
        draw_scale(partitions, 1);
      }
      /*}}}  */
      else
      /*{{{  change scale*/
      {
        sprintf(scale, "|top = %3.2f||",partitions*PSIZE/100.0);
        m_loadmenu(2, scale); /* update pop up menu info */
        last_part=partitions;
        redraw(samples, head);
        draw_scale(partitions, xmax);
      }
      /*}}}  */
      head=((head+1)%xmax);
      break;
      /*}}}  */
      /*{{{  default*/
      default: break;
      /*}}}  */
    }
    alarm(INTERVAL);
  }
  exit(0);
}
/*}}}  */
