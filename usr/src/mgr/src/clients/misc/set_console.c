/*{{{}}}*/
/*{{{  Notes*/
/* redirect sun console output to this window */
/*}}}  */
/*{{{  #includes*/
#ifdef sun
#undef _POSIX_SOURCE
#define u_char unsigned char
#define u_short unsigned short
#include <sys/ioctl.h>
#include <sundev/kbio.h>
#endif
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#ifdef _POSIX_SOURCE
#include <termios.h>
#endif
#include <stdio.h>
/*}}}  */

/*{{{  main*/
int main(argc,argv) int argc; char **argv;
{
  /*{{{  variables*/
  #  ifdef sun
  int kbd;
  int mode = 0;
  #  endif
  int fd;
  int one = 1;
  char *name;
  /*}}}  */

  /*{{{  make sure kbd is in direct mode (suns only)*/
  #  ifdef sun
  if ((kbd = open("/dev/kbd",0)) < 0 )
  {
    perror("set_console: can't open keyboard: ");
    exit(1);
  }
  if (ioctl(kbd,KIOCGDIRECT,&mode) < 0 )
  {
    perror("set_console: can't get keyboard status: ");
    exit(1);
  }
  if (mode != 1)
  {
    fprintf(stderr,"set_console: keyboard not in direct mode\n");
    exit(1);
  }
  #  endif
  /*}}}  */
  /*{{{  set new console device*/
  if ((fd=(**argv == 'r' ? open("/dev/console",O_RDWR) : fileno(stderr)))>=0)
  {
    name = ttyname(fd);
    if (name)
    {
      if (ioctl(fd,TIOCCONS,&one)<0)
      {
        perror("set_console: can't set new console device: ");
        exit(1);
      }
      printf("set_console: redirecting console output to %s\n",name);
    }
  }
  else
  {
    perror("set_console: can't open new console device: ");
    exit(1);
  }
  /*}}}  */
  exit(0);
}
/*}}}  */
