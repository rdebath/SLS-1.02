/*{{{  #includes*/
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>

#include "term.h"
#include "bitmap.h"
/*}}}  */
/*{{{  #defines*/
#define POLL 60
#define MAILBOX "/usr/spool/mail"
/*}}}  */

/*{{{  variables*/
/*{{{  icons*/
struct icon mbox_full = { "mbox_full",0,0,0,(char*)0 };
struct icon mbox_new = { "mbox2",0,0,0,(char*)0 };
struct icon mbox_empty = { "mbox_closed",0,0,0,(char*)0 };
/*}}}  */
/*{{{  mailbox*/
char mailbox[256];
/*}}}  */
/*}}}  */

/*{{{  clean up and exit*/
void clean(n) int n;
{
  m_ttyreset();
  m_popall();
  m_setcursor(0);
  exit(n);
}
/*}}}  */
/*{{{  down load an icon*/
void download_icon(icon,where) struct icon *icon; int where;
{
  int w_in, h_in;

  m_bitfile(where,icon->name,&w_in,&h_in);
  icon->w=w_in;
  icon->h=h_in;
  icon->type=where;
  if (h_in==0 || w_in==0)
  {
    fprintf(stderr,"Can't find icon %s\n",icon->name);
    clean(1);
  }
}
/*}}}  */
/*{{{  set_icon(name)*/
void set_icon(name) struct icon name;
{
  m_bitcopyto(0,0,name.w,name.h,0,0,0,name.type);
  m_flush();
}
/*}}}  */

/*{{{  check the spool file for new mail and update message*/
void update()
{
  struct stat statb;

  alarm(0);
  if (stat(mailbox,&statb)<0 || statb.st_size==0) set_icon(mbox_empty);
  else
  {
    if (statb.st_mtime>statb.st_atime) set_icon(mbox_new);
    else set_icon(mbox_full);
  }
  alarm(POLL);
}
/*}}}  */

/*{{{  main*/
int main(argc,argv) int argc; char *argv[];
{
  /*{{{  variables*/
  char line[MAXLINE];
  int x,y,w,h,border;
  /*}}}  */

  /*{{{  set up*/
  if (argc==2) strcpy(mailbox,argv[1]);
  else sprintf(mailbox,"%s/%s",MAILBOX,getlogin());
  ckmgrterm(*argv);
  m_setup(M_FLUSH);
  m_push(P_MENU|P_BITMAP|P_FONT|P_EVENT|P_FLAGS|P_POSITION);
  m_ttyset();
  m_setmode(M_NOWRAP);
  m_setmode(M_ABS);
  m_func(BIT_SRC);
  m_setcursor(5);
  /*}}}  */
  /*{{{  load icons*/
  download_icon(&mbox_empty,1);
  download_icon(&mbox_new,2);
  download_icon(&mbox_full,3);
  /*}}}  */
  /*{{{  signals*/
  signal(SIGHUP,clean);
  signal(SIGTERM,clean);
  signal(SIGINT,clean);
  signal(SIGALRM,update);
  /*}}}  */
  /*{{{  reshape*/
  get_size(&x,&y,&w,&h);
  get_param(0,0,0,&border);
  m_shapewindow(x,y,2*border+mbox_full.w, 2*border+mbox_full.h);
  /*}}}  */
  /*{{{  set events*/
  m_setevent(ACTIVATE,"A\n");
  m_setevent(REDRAW,"R\n");
  /*}}}  */
  /*{{{  wait for an event*/
  update();
  while(1)
  {
    if(m_gets(line)==NULL) clearerr(m_termin);
    else
    switch(*line)
    {
      /*{{{  A -- activate*/
      case 'A':
      update();
      break;
      /*}}}  */
      /*{{{  R -- redraw*/
      case 'R':
      m_clear();
      update();
      break;
      /*}}}  */
    }
  }
  /*}}}  */
}
/*}}}  */
