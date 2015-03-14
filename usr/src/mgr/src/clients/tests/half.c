/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
*/

/* Start a new window with a command running in it */
/*}}}  */
/*{{{  #includes*/
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <termios.h>
#include <limits.h>
#include <stdio.h>
#include <errno.h>

#include "term.h"
/*}}}  */
/*{{{  #defines*/
#define W	300
#define H	200
#define X	100
#define Y	100
/*}}}  */

/*{{{  main*/
int main(int argc, char *argv[])
{
  /*{{{  variables*/
  register int i;
  int pid;		/* pid of new shell */
  char name[50];	/* name of tty to open */
  /*}}}  */

  /*{{{  check arguments*/
  ckmgrterm(argv[0]);
  if (argc < 2) 
  {
    fprintf(stderr,"Usage: %s <command> \n",*argv);
    exit(1); 
  }
  /*}}}  */
  /*{{{  setup*/
  m_setup(M_MODEOK);
  m_ttyset();
  /*}}}  */
  /*{{{  make window and get tty name to open*/
  m_halfwin(X,Y,W,H);
  m_flush();

  m_gets(name);
  name[strlen(name)-1] = '\0';
  /*}}}  */
  /*{{{  complain and exit if window creation failed*/
  if (strlen(name) < 4)
  {
    printf("%s: Sorry, couldn't create window\r\n",*argv);
    exit(1);
  }
  /*}}}  */
  /*{{{  make sure we can open tty*/
  if (access(name,R_OK|W_OK) < 0) 
  {
    printf("Serious error, can't open %s; error no %d\n",name, errno);
    exit(1);
  }
  /*}}}  */

  /* start new process */
  if ((pid=fork())>0)
  /*{{{  parent*/
  {
    m_ttyreset();
    printf("starting %s as %d on %s\n",argv[1],pid,name);
    exit(0);
  }
  /*}}}  */
  else if (pid == 0)
  /*{{{  child*/
  {
#      ifdef TIOCNOTTY
    /* remove controlling tty -- is this sysv or bsd or posix or what? */
    ioctl(open("/dev/tty",O_RDONLY),TIOCNOTTY,0);
#      endif

    /* get our own process group */
    setsid();

    /* close all files */
    for(i=0; i<getdtablesize(); i++) close(i);

    /* open new ones */
    open(name,O_RDONLY);	/* stdin */
    open(name,O_WRONLY);	/* stdout */
    open(name,O_WRONLY);	/* stderr */
    m_termout=stdout;

    /* fix the tty modes */
    m_ttyreset();

    /* start the command */
    execvp(argv[1],argv+1);
    printf("%s: Can't find %s\n",argv[0],argv[1]);
    exit(1);
  }
  /*}}}  */
  else
  /*{{{  error*/
  {
    m_ttyreset();
    printf("Can't fork!!\n");
    exit(1);
  }
  /*}}}  */
}
/*}}}  */
