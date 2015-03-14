/* TTY interface for BSD UNIX
   Copyright (C) 1991 Joseph H. Allen

This file is part of JOE (Joe's Own Editor)

JOE is free software; you can redistribute it and/or modify it under the 
terms of the GNU General Public License as published by the Free Software 
Foundation; either version 1, or (at your option) any later version.  

JOE is distributed in the hope that it will be useful, but WITHOUT ANY 
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more 
details.  

You should have received a copy of the GNU General Public License
along with JOE; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <sgtty.h>
#include <fcntl.h>
#include <stdio.h>
#include <signal.h>
#include <sys/time.h>
#include "config.h"
#include "heap.h"
#include "tty.h"

#ifndef HZ
#define HZ 10			/* Clock ticks/second */
#endif

/* The terminal */

FILE *term=0;

/* The original tty state */

static struct sgttyb oarg;
static struct tchars otarg;
static struct ltchars oltarg;

/* The output buffer, index and size. */

char *obuf=0;
int obufp=0;
int obufsiz;

/* The baud rate */

unsigned baud;
unsigned long upc;

/* Code to baud-rate conversion table */

static int speeds[]=
{
B50,50,B75,75,B110,110,B134,134,B150,150,B200,200,B300,300,B600,600,B1200,1200,
B1800,1800,B2400,2400,B4800,4800,B9600,9600,EXTA,19200,EXTB,38400
};

/* Input buffer, typeahead indication flag and editor is about to exit flag */

int have=0;
static char havec;
int leave=0;

/* TTY mode flag.  1 for open, 0 for closed */

static int ttymode=0;

void sigjoe()
{
signal(SIGHUP,ttsig);
signal(SIGTERM,ttsig);
signal(SIGINT,SIG_IGN);
signal(SIGPIPE,SIG_IGN);
signal(SIGQUIT,SIG_IGN);
}

void signrm()
{
signal(SIGHUP,SIG_DFL);
signal(SIGTERM,SIG_DFL);
signal(SIGINT,SIG_DFL);
signal(SIGPIPE,SIG_DFL);
signal(SIGQUIT,SIG_DFL);
}

void ttopen()
{
sigjoe();
ttopnn();
}

void ttopnn()
{
int x;
struct sgttyb arg;
struct tchars targ;
struct ltchars ltarg;
if(!term && !(term=fopen("/dev/tty","r+")))
 {
 fprintf(stderr,"Couldn\'t open tty\n");
 exit(1);
 }
if(ttymode) return;
ttymode=1;
fflush(term);
ioctl(fileno(term),TIOCGETP,&arg);
ioctl(fileno(term),TIOCGETC,&targ);
ioctl(fileno(term),TIOCGLTC,&ltarg);
oarg=arg; otarg=targ; oltarg=ltarg;
arg.sg_flags=( (arg.sg_flags&~(ECHO|CRMOD) ) | CBREAK) ;
targ.t_intrc= -1;
targ.t_quitc= -1;
targ.t_eofc= -1;
targ.t_brkc= -1;
ltarg.t_suspc= -1;
ltarg.t_dsuspc= -1;
ltarg.t_rprntc= -1;
ltarg.t_flushc= -1;
ltarg.t_werasc= -1;
ltarg.t_lnextc= -1;
ioctl(fileno(term),TIOCSETN,&arg);
ioctl(fileno(term),TIOCSETC,&targ);
ioctl(fileno(term),TIOCSLTC,&ltarg);
baud=9600;
upc=0;
for(x=0;x!=30;x+=2)
 if(arg.sg_ospeed==speeds[x])
  {
  baud=speeds[x+1];
  break;
  }
{
char *bs=getenv("BAUD");
if(bs)
 {
 sscanf(bs,"%u",&baud);
 }
}
upc=DIVIDEND/baud;
if(obuf) free(obuf);
if(!(TIMES*upc)) obufsiz=4096;
else
 {
 obufsiz=1000000/(TIMES*upc);
 if(obufsiz>4096) obufsiz=4096;
 }
if(!obufsiz) obufsiz=1;
obuf=(char *)malloc(obufsiz);
}

void ttclose()
{
ttclsn();
signrm();
}

void ttclsn()
{
int oleave=leave;
if(ttymode) ttymode=0;
else return;
leave=1;
ttflsh();
ioctl(fileno(term),TIOCSETN,&oarg);
ioctl(fileno(term),TIOCSETC,&otarg);
ioctl(fileno(term),TIOCSLTC,&oltarg);
leave=oleave;
}

static int yep;
static dosig() { yep=1; } 

int ttflsh()
{
if(obufp)
 {
 struct itimerval a,b;
 unsigned long usec=obufp*upc;
 if(usec>=500000/HZ && baud<38400)
  {
  a.it_value.tv_sec=usec/1000000;
  a.it_value.tv_usec=usec%1000000;
  a.it_interval.tv_usec=0;
  a.it_interval.tv_sec=0;
  signal(SIGALRM,dosig);
  yep=0;
  sigsetmask(sigmask(SIGALRM));
  setitimer(ITIMER_REAL,&a,&b);
  write(fileno(term),obuf,obufp);
  while(!yep) sigpause(0);
  signal(SIGALRM,SIG_DFL);
  }
 else write(fileno(term),obuf,obufp);
 obufp=0;
 }
if(!have && !leave)
 {
 fcntl(fileno(term),F_SETFL,FNDELAY);
 if(read(fileno(term),&havec,1)==1) have=1;
 fcntl(fileno(term),F_SETFL,0);
 }
return 0;
}

int ttgetc()
{
ttflsh();
if(have) have=0;
else if(read(fileno(term),&havec,1)<1) ttsig(0);
return havec;
}

void ttputs(s)
char *s;
{
while(*s)
 {
 obuf[obufp++]= *(s++);
 if(obufp==obufsiz) ttflsh();
 }
}

void ttgtsz(x,y)
int *x, *y;
{
#ifdef TIOCGSIZE
struct ttysize getit;
#else
#ifdef TIOCGWINSZ
struct winsize getit;
#endif
#endif
*x=0; *y=0;
#ifdef TIOCGSIZE
if(ioctl(fileno(term),TIOCGSIZE,&getit)!= -1)
 {
 *x=getit.ts_cols;
 *y=getit.ts_lines;
 }
#else
#ifdef TIOCGWINSZ
if(ioctl(fileno(term),TIOCGWINSZ,&getit)!= -1)
 {
 *x=getit.ws_col;
 *y=getit.ws_row;
 }
#endif
#endif
}

void ttshell(cmd)
char *cmd;
{
int x,omode=ttymode;
char *s=getenv("SHELL");
if(!s) return;
ttclsn();
if(x=fork())
 {
 if(x!= -1) wait(0);
 if(omode) ttopnn();
 }
else
 {
 signrm();
 if(cmd) execl(s,s,"-c",cmd,NULL);
 else
  {
  fprintf(stderr,"You are at the command shell.  Type 'exit' to return\n");
  execl(s,s,NULL);
  }
 _exit(0);
 }
}

static int gotsig;

static void dosi()
{
gotsig=1;
}

void ttsusp()
{
#ifdef SIGCONT
int omode=ttymode;
ttclsn();
gotsig=0;
fprintf(stderr,"You have suspended the program.  Type \'fg\' to return\n");
signal(SIGCONT,dosi);
sigsetmask(sigmask(SIGCONT));
kill(0,SIGTSTP);
while(!gotsig) sigpause(0);
signal(SIGCONT,SIG_DFL);
if(omode) ttopnn();
#else
ttshell(NULL);
#endif
}

char *getwd();
char *pwd()
{
static buf[1024];
return getwd(buf);
}
