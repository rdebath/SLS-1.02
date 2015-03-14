/* sgtty.c - emulate BSD sgtty stuff with termios - ross biro, rick sladkey */

#define _SGTTY_SOURCE

#include <sgtty.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <termios.h>

struct mask
{
   unsigned short mask;
   unsigned short res;
};

struct sf 
{
   struct mask iflag;
   struct mask oflag;
   struct mask cflag;
   struct mask lflag;
};

static struct sf trans[] =
{
   /* order is important here. */
   /* iflag oflag cflag lflag */
   /* this needs to be fixed. */
   {{0,0}, {OPOST,0}, {0,0}, {ISIG,0}},			/* O_RAW */
   {{0,0}, {0,0}, {0,0}, {XCASE,XCASE}},		/* O_LCASE */
   {{ICRNL,ICRNL}, {ONLCR, ONLCR}, {0,0}, {0,0}},	/* O_CRMOD */
   {{0,0}, {0,0}, {0,0}, {ECHO,ECHO}},			/* O_ECHO */
   {{0,0}, {0,0}, {PARENB|PARODD,PARENB|PARODD}, {0,0}},/* O_ODDP */
   {{0,0}, {0,0}, {PARENB|PARODD,PARENB}, {0,0}},	/* O_EVENP */
   {{0,0}, {0,0}, {0,0}, {ICANON,0}},			/* O_CBREAK */
};

#define _BSD_VDISABLE	255

static int
bchar (unsigned char c)
{
   return c == _POSIX_VDISABLE ? _BSD_VDISABLE : c;
}

static int
pchar (unsigned char c)
{
   return c == _BSD_VDISABLE ? _POSIX_VDISABLE : c;
}

static int 
tiocgetp (int fd, struct sgttyb *sg)
{
   struct termios t;
   int err;
   int i;
   err = ioctl (fd, TCGETS, &t);
   if (err < 0) return (err);
   sg->sg_ispeed = cfgetispeed (&t);
   sg->sg_ospeed = cfgetospeed (&t);
   sg->sg_erase = bchar (t.c_cc[VERASE]);
   sg->sg_kill = bchar (t.c_cc[VKILL]);
   sg->sg_flags = 0;
   for (i = 0; i < sizeof (trans) / sizeof (struct sf); i++)
     {
	if ((t.c_iflag & trans[i].iflag.mask) == trans[i].iflag.res &&
	    (t.c_oflag & trans[i].oflag.mask) == trans[i].oflag.res &&
	    (t.c_cflag & trans[i].cflag.mask) == trans[i].cflag.res &&
	    (t.c_lflag & trans[i].lflag.mask) == trans[i].lflag.res)
	  {
	     sg->sg_flags |= 1 << i;
	  }
     }
   return (0);
}

static int 
tiocset (int fd, struct sgttyb *sg, int method)
{
   struct termios t;
   int err;
   int i;
   err = ioctl (fd, TCGETS, &t);
   if (err < 0) return (err);

   cfsetispeed (&t, sg->sg_ispeed);
   cfsetospeed (&t, sg->sg_ospeed);
   t.c_cc[VERASE] = pchar (sg->sg_erase);
   t.c_cc[VKILL] = pchar (sg->sg_kill); 
   for (i = sizeof (trans) / sizeof (struct sf) - 1; i >= 0; i--)
     {
	t.c_iflag &= ~trans[i].iflag.mask;
	t.c_oflag &= ~trans[i].oflag.mask;
	t.c_cflag &= ~trans[i].cflag.mask;
	t.c_lflag &= ~trans[i].lflag.mask;
	if (sg->sg_flags & (1 << i))
	  {
	     t.c_iflag |= trans[i].iflag.res;
	     t.c_oflag |= trans[i].oflag.res;
	     t.c_cflag |= trans[i].cflag.res;
	     t.c_lflag |= trans[i].lflag.res;
	  }
	else
	  {
	     t.c_iflag |= (~trans[i].iflag.res) & trans[i].iflag.mask;
	     t.c_oflag |= (~trans[i].oflag.res) & trans[i].oflag.mask;
	     t.c_cflag |= (~trans[i].cflag.res) & trans[i].cflag.mask;
	     t.c_lflag |= (~trans[i].lflag.res) & trans[i].lflag.mask;
	  }
     }
   return (ioctl (fd, method, &t));
}

static int 
tiocsetp (int fd, struct sgttyb *sg)
{
   return tiocset (fd, sg, TCSETSF);
}

static int 
tiocsetn (int fd, struct sgttyb *sg)
{
   return tiocset (fd, sg, TCSETS);
}

static int
tiocgetc (int fd, struct tchars *tc)
{
   struct termios t;
   int err;
   err = ioctl (fd, TCGETS, &t);
   if (err < 0) return (err);
   tc->t_intrc = bchar (t.c_cc[VINTR]);
   tc->t_quitc = bchar (t.c_cc[VQUIT]);
   tc->t_eofc = bchar (t.c_cc[VEOF]);
   tc->t_startc = bchar (t.c_cc[VSTART]);
   tc->t_stopc = bchar (t.c_cc[VSTOP]);
   tc->t_brkc = bchar (t.c_cc[VEOL]);
   return (0);
}

static int
tiocsetc (int fd, struct tchars *tc)
{
   struct termios t;
   int err;
   err = ioctl (fd, TCGETS, &t);
   if (err < 0) return (err);
   t.c_cc[VINTR] = pchar (tc->t_intrc);
   t.c_cc[VQUIT] = pchar (tc->t_quitc);
   t.c_cc[VEOF] = pchar (tc->t_eofc);
   t.c_cc[VEOL] = pchar (tc->t_brkc);
   t.c_cc[VSTART] = pchar (tc->t_startc);
   t.c_cc[VSTOP] = pchar (tc->t_stopc);
   return (ioctl (fd, TCSETS, &t));
}

static int
tiocgltc (int fd, struct ltchars *tc)
{
   struct termios t;
   int err;
   err = ioctl (fd, TCGETS, &t);
   if (err < 0) return (err);
   tc->t_werasc = bchar (t.c_cc[VERASE]);
   tc->t_suspc = bchar (t.c_cc[VSUSP]);
   tc->t_dsuspc = bchar (t.c_cc[VSUSP]);
   tc->t_rprntc = bchar (t.c_cc[VREPRINT]);
   tc->t_flushc = bchar (t.c_cc[VDISCARD]);
   tc->t_lnextc = bchar (t.c_cc[VLNEXT]);
   return (0);
}

static int
tiocsltc (int fd, struct ltchars *tc)
{
   struct termios t;
   int err;
   err = ioctl (fd, TCGETS, &t);
   if (err < 0) return (err);
   t.c_cc[VERASE] = pchar (tc->t_werasc);
   t.c_cc[VSUSP] = pchar (tc->t_suspc);
   t.c_cc[VSUSP] = pchar (tc->t_dsuspc);
   t.c_cc[VREPRINT] = pchar (tc->t_rprntc);
   t.c_cc[VDISCARD] = pchar (tc->t_flushc);
   t.c_cc[VLNEXT] = pchar (tc->t_lnextc);
   return (ioctl (fd, TCSETS, &t));
}

static int
tioclget (int fd, int *lflagsp)
{
   struct termios t;
   int lflags = 0;

   *lflagsp = lflags;
   return 0;
}

static int
tioclset (int fd, int *lflagsp)
{
   return 0;
}

static int
tiocflush (int fd, int *arg)
{
   return 0;
}

int
bsd_ioctl (int fd, int option, void *arg)
{
   switch (option) {
   case TIOCGETP:
      return tiocgetp(fd, (struct sgttyb *) arg);
   case TIOCSETP:
      return tiocsetp(fd, (struct sgttyb *) arg);
   case TIOCGETC:
      return tiocgetc(fd, (struct tchars *) arg);
   case TIOCSETC:
      return tiocsetc(fd, (struct tchars *) arg);
   case TIOCGLTC:
      return tiocgltc(fd, (struct ltchars *) arg);
   case TIOCSLTC:
      return tiocsltc(fd, (struct ltchars *) arg);
   case TIOCLGET:
      return tioclget(fd, (int *) arg);
   case TIOCLSET:
      return tioclset(fd, (int *) arg);
   case TIOCFLUSH:
      return tiocflush(fd, (int *) arg);
   case TIOCSETN:
      return tiocsetn(fd, (struct sgttyb *) arg);
   default:
      return ioctl(fd, option, arg);
   }
}

