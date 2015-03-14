#define I_IOCTL
#define	I_TTY
#include "includes.h"

void lose_ctty(void) {
#ifdef USE_TIOCNOTTY
  {
    int slavefd;
    if ((slavefd = open ("/dev/tty", O_RDWR, 0)) >= 0) {
      ioctl (slavefd, TIOCNOTTY, (char *) 0);
      close (slavefd);
    }
  }
#endif
#ifdef USE_SETSID
  setsid();
#endif
#ifdef USE_SETPGRP
#ifdef SYSV
  setpgrp();
#else
  { int mypid;
  mypid = getpid();
  setpgrp(mypid, 0); }
#endif
#endif
    
#ifdef USE_BSDJOBS
  { int mypid;
  mypid = getpid();
  tcsetpgrp(0, mypid); }
#endif
}

#ifdef USE_TERMIOS

#ifdef ultrix
#include <sys/ioctl.h>
#define TCSETS TCSANOW
#define TCGETS TCGETP
#endif

static struct termios oldterm;

void terminal_save(int fd) {
#ifdef USE_TCATTR
  tcgetattr(fd, &oldterm);
#else
  ioctl( fd, TCGETS, &oldterm);
#endif
}

void terminal_raw(int fd) {
  struct termios tempio;
#ifdef USE_TCATTR
  tcgetattr(fd, &tempio);
#else
  ioctl( fd, TCGETS, &tempio);
#endif
  tempio.c_iflag = 0;
  tempio.c_oflag = 0;
  tempio.c_lflag = 0;
  tempio.c_cc[VMIN] = 1;
  tempio.c_cc[VTIME] = 0;
#ifdef USE_TCATTR
  tcsetattr(fd, TCSANOW, &tempio);
#else
  ioctl( fd, TCSETS, &tempio);
#endif
}

void terminal_restore(int fd) {
#ifdef USE_TCATTR
  tcsetattr(fd, TCSANOW, &oldterm);
#else
  ioctl( fd, TCSETS, &oldterm);
#endif
}

#else /* bsd */

static int o_ldisc;
static struct sgttyb o_ttyb;
static struct tchars o_tchars;
static int o_lmode;
static struct ltchars o_ltchars;

void terminal_save(int fd)
{
  ioctl(fd, TIOCGETD, &o_ldisc);
  ioctl(fd, TIOCGETP, &o_ttyb);
  ioctl(fd, TIOCGETC, &o_tchars);
  ioctl(fd, TIOCLGET, &o_lmode);
  ioctl(fd, TIOCGLTC, &o_ltchars);

}

void terminal_restore(int fd)
{
  ioctl(fd, TIOCSETD, &o_ldisc);
  ioctl(fd, TIOCSETP, &o_ttyb);
  ioctl(fd, TIOCSETC, &o_tchars);
  ioctl(fd, TIOCLSET, &o_lmode);
  ioctl(fd, TIOCSLTC, &o_ltchars);
}

void terminal_raw(int fd)
{
  struct sgttyb m_ttyb;
  struct tchars m_tchars;
  struct ltchars m_ltchars;
  int m_ldisc;
  int m_lmode;

  /* initialize structures */
  ioctl(fd, TIOCGETP, &m_ttyb);
  ioctl(fd, TIOCGETC, &m_tchars);
  ioctl(fd, TIOCGLTC, &m_ltchars);

  m_ldisc = NTTYDISC;
  m_lmode = LLITOUT;

  /* modify structures */

/*  HSW 93/02/03, these shouldn't be set!
  m_ttyb.sg_ispeed = B9600;
  m_ttyb.sg_ospeed = B9600;
*/

  m_ttyb.sg_erase = -1;
  m_ttyb.sg_kill = -1;
  m_ttyb.sg_flags = RAW;

  m_tchars.t_quitc = -1;

  m_ltchars.t_suspc = -1;
  m_ltchars.t_dsuspc = -1;
  m_ltchars.t_flushc = -1;
  m_ltchars.t_lnextc = -1;
  
  /* update terminal */
  ioctl(fd, TIOCSETD, &m_ldisc);
  ioctl(fd, TIOCSETP, &m_ttyb);
  ioctl(fd, TIOCSETC, &m_tchars);
  ioctl(fd, TIOCLSET, &m_lmode);
  ioctl(fd, TIOCSLTC, &m_ltchars);

}

#endif


