/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       This document contains proprietary information that shall
 *       be distributed or routed only within Bellcore and its
 *       authorized clients, except with written permission of Bellcore.
 */

/* muck with tty modes */

#ifdef v7
/*{{{}}}*/
/*{{{  sgtty*/
#include <sgtty.h>

/* set up tty input modes */

void set_tty(file)
int file;				/* file descriptor */
   {
   set_mode(file,RAW,ECHO,0);
   }

/* setup mouse input modes */

int set_mouseio(file)
int file;				/* file descriptor */
   {
   set_mode(file,RAW,ECHO,B1200);
   return(ioctl(file,TIOCEXCL,0));
   }

/* reset input tty modes */

void reset_tty(file)
int file;				/* file descriptor */
   {
   set_mode(file,ECHO,RAW,0);
   }

/*
 *******************************************************************************
 *
 *	Set the terminal mode
 */

static set_mode(file,on,off,speed)
int file;		/* file pointer */
int on;			/* flags to turn on */
int off;		/* flags to turn off */
{
	struct sgttyb buff;

	gtty(file,&buff);
	buff.sg_flags |= on;
	buff.sg_flags &= ~off;
	if (speed) 
	   buff.sg_ispeed = buff.sg_ospeed = speed;
	stty(file,&buff);
        return(0);
}

/* save tty modes for getshell */

/* place to save tty modes */

static int t_ldisc;
static struct sgttyb t_sgttyb;
static struct tchars t_tchars;
static struct ltchars t_ltchars;
static int t_lflags;

save_modes(fd)
int fd;			/* fd to save tty modes from */
	{
   ioctl(fd,TIOCGETD,&t_ldisc);
   ioctl(fd,TIOCGETP,&t_sgttyb);
   ioctl(fd,TIOCGETC,&t_tchars);
   ioctl(fd,TIOCGLTC,&t_ltchars);
   ioctl(fd,TIOCLGET,&t_lflags);
	}

restore_modes(fd)
int fd;
	{
   ioctl(fd,TIOCSETD,&t_ldisc);
   ioctl(fd,TIOCSETP,&t_sgttyb);
   ioctl(fd,TIOCSETC,&t_tchars);
   ioctl(fd,TIOCSLTC,&t_ltchars);
   ioctl(fd,TIOCLSET,&t_lflags);
	}

adjust_mode(disc,flags)
int flags;		/* flags */
int disc;		/* line disc */
	{
   t_ldisc=disc;
   t_sgttyb.sg_flags = flags;
	}

/*}}}  */
#else
/*{{{  termios*/
/*{{{  #includes*/
#ifdef sun
#define u_char unsigned char
#define u_short unsigned short
#include <sys/time.h>
#include <sundev/vuid_event.h>
#endif
#include <unistd.h>
#include <termios.h>
/*}}}  */

/*{{{  buffer for set_tty() and reset_tty()*/
static struct termios orig_tty;
/*}}}  */
/*{{{  set_tty*/
void set_tty(file) int file;
{
  struct termios buff;

  tcgetattr(file,&orig_tty);
  buff=orig_tty;
  buff.c_lflag = 0;
  buff.c_iflag = 0;
  buff.c_cflag &= ~CSIZE;
  buff.c_cflag |= CS8;
  buff.c_cc[VMIN] = 1;
  buff.c_cc[VTIME] = 0;
  tcsetattr(file,TCSANOW,&buff);
}
/*}}}  */
/*{{{  reset_tty*/
void reset_tty(file) int file;
{
  tcsetattr(file,TCSANOW,&orig_tty);
}
/*}}}  */

/*{{{  buffer for save_modes() and restore_modes()*/
static struct termios termio_b;
/*}}}  */
/*{{{  save_modes*/
void save_modes(fd) int fd;
{
  tcgetattr(fd,&termio_b);
}
/*}}}  */
/*{{{  restore_modes*/
void restore_modes(fd) int fd;
{
  tcsetattr(fd,TCSANOW,&termio_b);
}
/*}}}  */

/*{{{  adjust_mode*/
void adjust_mode(disc,flags) int flags; int disc;
{
  termio_b.c_lflag=flags;
}
/*}}}  */

/*{{{  set_mouseio*/
int set_mouseio(file) int file;
{
  struct termios buff;
  int res;

  if ((res=tcgetattr(file,&buff))<0) return (res);
  buff.c_lflag=0;
  cfsetispeed(&buff,B1200);
  buff.c_cc[VMIN]=1;
#  ifdef Coherent
  buff.c_cc[VTIME]=0;
#  else
  buff.c_cc[VTIME]=1;
#  endif
  if ((res=tcsetattr(file,TCSANOW,&buff))<0) return (res);
#  ifdef TIOCEXCL
  if ((res=ioctl(file,TIOCEXCL,0))<0) return (res);
#  endif
#  ifdef VUIDSFORMAT
  /* SunOS: set normal mode because X (sometimes?) leaves it broken */
  if ((res=ioctl(file,VUIDSFORMAT,0))<0) return (res);
#  endif
  return 0;
}
/*}}}  */
/*}}}  */
#endif
