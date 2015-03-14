/*
 * This file is part of the Minicom Communications Program,
 * written by Miquel van Smoorenburg 1991/1992.
 *
 * sysdep1.c 	  - system dependant routines.
 *
 * m_dtrtoggle	  - dropt dtr and raise it again
 * m_break	  - send BREAK signal
 * m_getdcd	  - get modem dcd status
 * m_setdcd	  - set modem dcd status
 * m_savestate	  - save modem state
 * m_restorestate - restore saved modem state
 * m_nohang	  - tell driver not to hang up at DTR drop
 * m_setparms	  - set baudrate, parity and number of bits.
 * m_wait	  - wait for child to finish. System dependant too.
 *
 */
#include <sys/types.h>
#ifndef _COHERENT
#  include <sys/wait.h>
#endif
#ifdef _V7
#  include <sgtty.h>
#endif
#ifdef _SYSV
#  include <termio.h>
#endif
#ifdef _HPUX_SOURCE
#  include <sys/modem.h>
#endif
#if defined(_BSD43) || defined (_SYSV)
#  include <sys/ioctl.h>
#endif
#ifdef _POSIX_SOURCE
#  include <stdlib.h>
#  include <unistd.h>
#endif
#include <stdio.h>
#include <setjmp.h>
#include "window.h"
#include "minicom.h"

#if !defined(_BSD43) && !defined(WEXITSTATUS)
#  define WEXITSTATUS(s) (((s) >> 8) & 0377)
#endif

/*
 * Drop DTR line and raise it again.
 */
void m_dtrtoggle(fd) 
int fd;
{
#ifdef _V7
#ifndef _COHERENT
  struct sgttyb sg, ng;
  
  ioctl(fd, TIOCGETP, &sg);
  ioctl(fd, TIOCGETP, &ng);
  
  ng.sg_ispeed = ng.sg_ospeed = 0;
  ioctl(fd, TIOCSETP, &ng);
  sleep(1);
  ioctl(fd, TIOCSETP, &sg);
#endif
#endif
#if (defined(_BSD43) || defined(_COHERENT))
  ioctl(fd, TIOCCDTR, 0);
  sleep(1);
  ioctl(fd, TIOCSDTR, 0);
#endif
#if defined(_SYSV) && !defined(_HPUX_SOURCE)
  struct termio sg, ng;
  
  ioctl(fd, TCGETA, &sg);
  ioctl(fd, TCGETA, &ng);
  
  ng.c_cflag = (ng.c_cflag & ~CBAUD) | B0;
  ioctl(fd, TCSETA, &ng);
  sleep(1);
  ioctl(fd, TCSETA, &sg);
#endif
#ifdef _HPUX_SOURCE
  unsigned long mflag = 0L;

  ioctl(fd, MCSETAF, &mflag);
  ioctl(fd, MCGETA, &mflag);
  mflag = MRTS | MDTR;
  sleep(1);
  ioctl(fd, MCSETAF, &mflag);
#endif
}

/*
 * Send a break
 */
void m_break(fd)
int fd;
{ 
#ifdef _V7
  struct sgttyb sg, ng;

  ioctl(fd, TIOCGETP, &sg);
  ioctl(fd, TIOCGETP, &ng);
  ng.sg_ispeed = ng.sg_ospeed = B110;
#ifdef _COHERENT
  ng.sg_flags = RAW;
#else
  ng.sg_flags = BITS8 | RAW;
#endif
  ioctl(fd, TIOCSETP, &ng);
  write(fd, "\0\0\0\0\0\0\0\0\0\0", 10);
  ioctl(fd, TIOCSETP, &sg);
#endif
#ifdef _BSD43  
  ioctl(fd, TIOCSBRK, 0);
  sleep(1);
  ioctl(fd, TIOCCBRK, 0);
#endif
#ifdef _SYSV
  ioctl(fd, TCSBRK, 0);
#endif
}

/*
 * Get the dcd status
 */
int m_getdcd(fd)
int fd;
{
#ifdef _MINIX
  struct sgttyb sg;
  
  ioctl(fd, TIOCGETP, &sg);
  return(sg.sg_flags & DCD ? 1 : 0);
#endif
#ifdef _BSD43
  int mcs;
   
  ioctl(fd, TIOCMODG, &mcs);
  return(mcs & TIOCM_CAR ? 1 : 0);
#endif
#if defined(_SYSV) && !defined(_HPUX_SOURCE)
  /* Impossible!!!! */
  return(0);
#endif
#ifdef _HPUX_SOURCE
  unsigned long mflag;
  
  ioctl(fd, MCGETA, &mflag);
  return(mflag & MDCD ? 1 : 0);
#endif
#ifdef _COHERENT
  /* Not Yet Implemented */
  return(0);
#endif
}

/*
 * Set the DCD status
 */
/*ARGSUSED*/
void m_setdcd(fd, what)
int fd, what;
{
#ifdef _MINIX
  /* Just a kludge for my Minix rs 232 driver */
  struct sgttyb sg;
  
  ioctl(fd, TIOCGETP, &sg);
  if (what)
  	sg.sg_flags |= DCD;
  else
  	sg.sg_flags &= ~DCD;
  ioctl(fd, TIOCSETP, &sg);
#endif
}

/* Variables to save states in */
#if defined (_BSD43) || defined (_V7)
static struct sgttyb sg;
static struct tchars tch;
static int lsw;
#endif
#ifdef _SYSV
static struct termio sg;
#endif

/*
 * Save the state of a port
 */
void m_savestate(fd)
int fd;
{
#if defined(_BSD43) || defined(_V7)
  ioctl(fd, TIOCGETP, &sg);
  ioctl(fd, TIOCGETC, &tch);
#endif
#ifdef _BSD43
  ioctl(fd, TIOCLGET, &lsw);
#endif
#ifdef _SYSV
  ioctl(fd, TCGETA, &sg);
#endif
}

/*
 * Restore the state of a port
 */
void m_restorestate(fd)
int fd;
{
#if defined(_BSD43) || defined(_V7)
  ioctl(fd, TIOCSETP, &sg);
  ioctl(fd, TIOCSETC, &tch);
#endif
#ifdef _BSD43  
  ioctl(fd, TIOCLSET, &lsw);
#endif
#ifdef _SYSV
  ioctl(fd, TCSETA, &sg);
#endif
}

/*
 * Set the line status so that it will not kill our process
 * if the line hangs up.
 */
/*ARGSUSED*/ 
void m_nohang(fd)
int fd;
{
#ifdef _BSD43
  int lsw;
  struct sgttyb sg;
  
  ioctl(fd, TIOCLGET, &lsw);
  lsw |= LNOHANG;
  ioctl(fd, TIOCLSET, &lsw);
#if 0
  ioctl(fd, TIOCGETP, &sg);
  sg.sg_flags |= NOHANG;
  ioctl(fd, TIOCSETP, &sg);
#endif
#endif
#ifdef _MINIX
  /* So? What about 1.6 ? */
#endif
#ifdef _COHERENT
  /* Doesn't know about this either, me thinks. */
#endif
#ifdef _SYSV
  struct termio sgg;
  
  ioctl(fd, TCGETA, &sgg);
  sgg.c_cflag |= CLOCAL;
  ioctl(fd, TCSETA, &sgg);
#endif
}

/*
 * Flush the buffers
 */
void m_flush(fd)
int fd;
{
#ifdef TIOCFLUSH
#ifdef _COHERENT
  ioctl(fd, TIOCFLUSH, 0);
#else
  ioctl(fd, TIOCFLUSH, (void *)0);
#endif
#endif
#ifdef TCFLSH
  ioctl(fd, TCFLSH, 2);
#endif
}

/*
 * Set baudrate, parity and number of bits.
 */
void m_setparms(fd, baudr, par, bits)
int fd;
char *baudr;
char *par;
char *bits;
{
#if defined (_V7) || defined (_BSD43)
  struct sgttyb tty;
  int spd = -1;
  int mcs;

  ioctl(fd, TIOCGETP, &tty);
#endif

#ifdef _SYSV
  struct termio tty;
  int spd = -1;
  
  ioctl(fd, TCGETA, &tty);
#endif

  switch(atoi(baudr)) {
  	case 0:
#ifdef B0
			spd = B0;	break;
#else
			spd = 0;	break;
#endif
  	case 300:	spd = B300;	break;
  	case 600:	spd = B600;	break;
  	case 1200:	spd = B1200;	break;
  	case 2400:	spd = B2400;	break;
  	case 4800:	spd = B4800;	break;
  	case 9600:	spd = B9600;	break;
#ifdef B19200
  	case 19200:	spd = B19200;	break;
#else
#  ifdef EXTA
	case 19200:	spd = EXTA;	break;
#   else
	case 19200:	spd = B9600;	break;
#   endif	
#endif	
  }
  
#ifdef _BSD43
  if (spd != -1) tty.sg_ispeed = tty.sg_ospeed = spd;
  /* Number of bits is ignored */

  tty.sg_flags = RAW | TANDEM;
  if (par[0] == 'E')
	tty.sg_flags |= EVENP;
  else if (par[0] == 'O')
	tty.sg_flags |= ODDP;
  else
  	tty.sg_flags |= PASS8 | ANYP;

  ioctl(fd, TIOCSETP, &tty);

#if 0 /* Why does this *drop* DTR ?? */
  ioctl(fd, TIOCMODG, &mcs);
  mcs |= TIOCM_DTR | TIOCM_DSR | TIOCM_CTS;
  ioctl(fd, TIOCMODS, &mcs);
#else
  ioctl(fd, TIOCSDTR, 0);
#endif
#endif

#ifdef _V7
  if (spd != -1) tty.sg_ispeed = tty.sg_ospeed = spd;
#ifdef _MINIX
  switch(bits[0]) {
  	case '5' : spd = BITS5; break;
  	case '6' : spd = BITS6; break;
  	case '7' : spd = BITS7; break;
  	case '8' :
  	default: spd = BITS8; break;
  }
  tty.sg_flags = RAW | spd;
#else
  tty.sg_flags = RAW;
#endif
  if (par[0] == 'E')
	tty.sg_flags |= EVENP;
  else if (par[0] == 'O')
	tty.sg_flags |= ODDP;

  ioctl(fd, TIOCSETP, &tty);
#endif

#ifdef _SYSV

  tty.c_cflag = (tty.c_cflag & ~CBAUD) | spd;

  switch (bits[0]) {
  	case '5':
  		tty.c_cflag = (tty.c_cflag & ~CSIZE) | CS5;
  		break;
  	case '6':
  		tty.c_cflag = (tty.c_cflag & ~CSIZE) | CS6;
  		break;
  	case '7':
  		tty.c_cflag = (tty.c_cflag & ~CSIZE) | CS7;
  		break;
  	case '8':
	default:
  		tty.c_cflag = (tty.c_cflag & ~CSIZE) | CS8;
  		break;
  }		
  /* Set into raw, no echo mode */
  tty.c_iflag &= ~(IGNBRK | IGNCR | INLCR | IUCLC | 
  	IXANY | IXON | IXOFF | INPCK | ISTRIP);
  tty.c_iflag |= (BRKINT | IGNPAR);
  tty.c_oflag &= ~OPOST;
  tty.c_lflag = ~(ICANON | ISIG | ECHO | ECHONL | ECHOE | ECHOK);
  tty.c_cflag |= CREAD;
  tty.c_cc[4] = 1;
  tty.c_cc[5] = 5;

  tty.c_cflag &= ~(PARENB | PARODD);
  if (par[0] == 'E')
	tty.c_cflag |= PARENB;
  else if (par[0] == 'O')
	tty.c_cflag |= PARODD;

  ioctl(fd, TCSETA, &tty);
#endif
}

/*
 * Wait for child and return pid + status
 */
int m_wait(stt)
int *stt;
{
#ifdef _BSD43
  int pid;
  union wait st1;
  
  pid = wait(&st1);
  *stt = st1.w_retcode;
  return(pid);
#else
  int pid;
  int st1;
  
  pid = wait(&st1);
  *stt = WEXITSTATUS(st1);
  return(pid);
#endif
}
