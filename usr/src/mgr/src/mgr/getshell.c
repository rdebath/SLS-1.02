/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* start a shell */
/*}}}  */

/*{{{  #includes*/
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
/* hack !!! */
#undef _POSIX_SOURCE
#include <signal.h>
#define _POSIX_SOURCE
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <termios.h>

#include "bitblit.h"

#include "defs.h"

#include "do_button.h"
#include "utmp.h"
/*}}}  */

/*{{{  #defines*/
#define SHELL		"/bin/sh"
/*}}}  */

/*{{{  variables*/
static char line[] = {"/dev/ptypX"};
extern char **environ;
/*}}}  */

/*{{{  getapty -- get a pty line*/
static int getapty()
{
  int i;
  int pty_fd, tty_fd;

  for(line[8]='p';line[8]<='t';line[8]++) for (i=0;i<=16;i++)
  {
    line[9]="0123456789abcdef"[i%16];
    line[5] = 'p';
    if ((pty_fd=open(line,O_RDWR))>=0)
    {
      line[5] = 't';
      if ((tty_fd=open(line,O_RDWR))>=0) { close(tty_fd); return(pty_fd); }
      else close(pty_fd);
    }
  }
  return(-1);
}
/*}}}  */
/*{{{  last_tty*/
char *
last_tty()
   {
   return(line);
   }
/*}}}  */
/*{{{  doenv -- change an environment variable*/
void do_env(name,value) char *name,*value;
   {
   register int i;
   int n = strlen(name);

   for(i=0;environ[i] != (char *) 0;i++)
      if (strncmp(environ[i],name,n) == 0) {
         strcpy(environ[i]+n,value);
         break;
         }
   }
/*}}}  */
/*{{{  get_path -- get a complete path name from command*/
static char path[512];
static char start[512];

char *
get_path(name)
char *name;
   {
   register char *next, *list;

   if (index("/.",*name))
      if (access(name,X_OK)==0)
         return(name);
      else
         return((char *)0);

   strcpy(start,getenv("PATH"));
   for(list=start;next=index(list,':');list=next+1) {
      *next = '\0';
      sprintf(path,"%s/%s",list,name);
      if (access(path,X_OK) == 0)
         return(path);
      }

   sprintf(path,"%s/%s",list,name);
   if (list && access(path,X_OK) == 0) {
      return(path);
      }
   else {
      return((char *) 0);
      }
   }
/*}}}  */
/*{{{  get_command -- start a command*/
int get_command(argv,file) char **argv; int *file;
{
   /*{{{  variables*/
   register int i;				/* counter */
   int fd;					/* file desc */
   pid_t pid;					/* pid of shell */
   int tty_slots;				/* # of tty slots */
   int tty;
   char *name;
   char *shell = getenv("SHELL");
   char *arg[2];
   /*}}}  */

   if (argv == (char **) 0 )
   {
      argv = arg;
      *argv = shell?shell:SHELL;
      *(argv+1) = (char *) 0;
   }
   name = get_path(argv[0]);

   if (name == (char *) 0 || *name == '\0') return(-2);

   if ((*file=getapty())<0) return(-1);
   if ((pid=fork())>0)
   /*{{{  parent side of fork*/
   return(pid);
   /*}}}  */
   else if (pid<0)
   /*{{{  error side of fork*/
   return(pid);
   /*}}}  */

   /*{{{  child side of fork*/
   /*{{{  set signal handlers to default*/
   for(i=0;i<NSIG;i++) signal(i,SIG_DFL);
   /*}}}  */
   /*{{{  void association with controlling terminal*/
#      ifdef SYSV
   tty_slots = 20;
#      else
   tty_slots = getdtablesize();
#      endif

   for(i=0;i<tty_slots;i++) close(i);

#   ifdef TIOCNOTTY
   tty=open("/dev/tty",O_RDONLY);
   ioctl(tty,TIOCNOTTY,0);
   close(tty);
#   endif
   /*}}}  */
   /*{{{  set process group and session ID*/
#      ifdef _POSIX_SOURCE
   setpgrp();
   setsid();
#      endif
   /*}}}  */
   /*{{{  make it controlling tty*/
   /* set the uid-0 stuff up */

   fd=open(line,
#   ifdef RDWR_FD
   O_RDWR
#   else
   O_RDONLY
#   endif
   );
   if (fd!=0) perror("mgr: internal error with opening fd 0");
   if (geteuid() < 1) {
      int uid = getuid();
      fchmod(fd,0622);
      fchown(fd,uid,-1);
      setreuid(uid,uid);

      uid = getgid();
      fchown(fd,-1,uid);
      setregid(uid,uid);
      }
   if (open(line,
#   ifdef RDWR_FD
   O_RDWR
#   else
   O_WRONLY
#   endif
   )!=1) { perror("mgr: internal error with opening fd 1"); exit(1); }
   if (open(line,
#   ifdef RDWR_FD
   O_RDWR
#   else
   O_WRONLY
#   endif
   )!=2) { perror("mgr: internal error with opening fd 2"); exit(1); }
   /*}}}  */
   /*{{{  set up tty mode*/
   adjust_mode(0,ECHO|ICANON|ISIG);
   restore_modes(0);
   /*}}}  */
   /*{{{  add a utmp entry*/
#      ifdef WHO
   add_utmp(line);
#      endif
   /*}}}  */
   /*{{{  start the command*/
   do_env("TERM=",TERMNAME);
   execve(name,argv,environ);
   _exit(1);
   /*}}}  */
   /*}}}  */
}
/*}}}  */
/*{{{  half_open -- half open a ptty then return*/
char *
half_open(file)
int *file;
   {
   if ((*file=getapty()) < 0) return((char *) 0);
   return(line);
   }
/*}}}  */
