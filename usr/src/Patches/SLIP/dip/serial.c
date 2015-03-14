/*
 * dip		A program for handling dialup IP connecions.
 *		UNIX RS-232 support functions.  This file takes care of
 *		opening and setting up the seriel line, and it takes
 *		care of allocating the buffers and initializing their
 *		control structures.
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1992 MicroWalt Corporation
 *		This program is free software; you can redistribute it and/or
 *		modify it under the terms of the GNU General Public License
 *		as published by the Free Software Foundation; either version
 *		2 of the License, or (at your option) any later version.
 */
#include "dip.h"


#define TTY_BUFSIZE	1024		/* size of one serial buffer	*/


static char *in_buff, *out_buff;	/* line input/output buffers	*/
static char *in_ptr, *out_ptr;
static int in_size, out_size;
static int in_cnt, out_cnt;
static struct termios old_tty;		/* saved TTY device state	*/
static int tty_fd = -1;			/* TTY file descriptor		*/
static struct {
  int	speed;
  int	code;
} allbauds[] = {			/* table of usable baud rates	*/
  { 50,		B50	},
  { 75,		B75  	},	
  { 110,	B110	},
  { 300,	B300	},
  { 600,	B600	},
  { 1200,	B1200	},
  { 2400,	B2400	},
  { 4800,	B4800	},
  { 9600,	B9600	},
#ifdef B14400
  { 14400,	B14400	},
#endif
#ifdef B19200
  { 19200,	B19200	},
#endif
#ifdef B38400
  { 38400,	B38400	},
#endif
#ifdef B57600
  { 57600,	B57600	},
#endif
#ifdef B115200
  { 115200,	B115200	},
#endif
  { -1,		0	}
};


static _PROTOTYPE( int set_baud, (int)					);


/* Set the desired baud rate. */
static int set_baud(what)
register int what;
{
  register int i;

  i = 0;
  while (allbauds[i].speed != -1) {
	if (allbauds[i].speed == what) return(allbauds[i].code);
	i++;
  }
  return(-EINVAL);
}


/* Save current TTY state and put into RAW mode. */
int tty_raw(fd, baud, attr)
int fd;
int baud;
char *attr;
{
  struct termios tty;
  int speed, i;

  /* Fetch the current TTY state and save it. */
  if (ioctl(fd, TCGETS, &old_tty) < 0) {
	fprintf(stderr, "dip: cannot ioctl(%d, GETS): %d\n", fd, errno);
	return(-errno);
  }

  /* Set the RAW, NO ECHO flags. */
  for(i = 0; i < NCCS; i++) tty.c_cc[i] = '\0';		/* no spec chr	*/
  tty.c_cc[VMIN] = 1;
  tty.c_cc[VTIME] = 0;
  tty.c_iflag = (IGNBRK | IGNPAR);			/* input flags	*/
  tty.c_oflag = (0);					/* output flags	*/
  tty.c_lflag = (0);					/* local flags	*/
  tty.c_cflag = (CRTSCTS | HUPCL | CREAD);		/* UART flags	*/

  /* Fetch the baud rate, and set it if needed. */
  if (baud != -1) {
  	if ((speed = set_baud(baud)) < 0) return(i);
  	tty.c_cflag |= ((tty.c_cflag & ~CBAUD) | speed);
  } else {
  	tty.c_cflag |= (old_tty.c_cflag & CBAUD);
	speed = (tty.c_cflag & CBAUD);
  }
  if (opt_v) printf("BAUD %d SPEED %d\n", baud, speed);

  /* Fetch and set the character size. */
  tty.c_cflag &= ~CSIZE;
  switch(attr[0]) {
	case ' ':
		tty.c_cflag |= (old_tty.c_cflag & CSIZE);
		break;
	case '5':
		tty.c_cflag |= CS5;
		break;
	case '6':
		tty.c_cflag |= CS6;
		break;
	case '7':
		tty.c_cflag |= CS7;
		break;
	case '8':
		tty.c_cflag |= CS8;
		break;
	default:
		return(-EINVAL);
  }
  if (opt_v) printf("CS=%c\n", attr[0]);

  /* Fetch and set the type of parity. */
  switch(attr[1]) {
	case ' ':
		tty.c_cflag |= (old_tty.c_cflag & (PARENB | PARODD));
		break;
	case 'N':
  		tty.c_cflag &= ~(PARENB | PARODD);
		break;	
	case 'O':
  		tty.c_cflag &= ~(PARENB | PARODD);
		tty.c_cflag |= (PARENB | PARODD);
		break;
	case 'E':
  		tty.c_cflag &= ~(PARENB | PARODD);
		tty.c_cflag |= (PARENB);
		break;
	default:
		return(-EINVAL);
  }
  if (opt_v) printf("PARITY=%c\n", attr[1]);

  /* Fetch and set the number of stop bits. */
  switch(attr[2]) {
	case ' ':
		tty.c_cflag |= (old_tty.c_cflag & CSTOPB);
		break;
	case '1':
		tty.c_cflag &= ~CSTOPB;
		break;
	case '2':
		tty.c_cflag |= CSTOPB;
		break;
	default:
		return(-EINVAL);
  }
  if (opt_v) printf("STOP=%c\n", attr[2]);

  if (ioctl(fd, TCSETS, &tty) < 0) {
	fprintf(stderr, "dip: cannot ioctl(%d, SETS): %d\n", fd, errno);
	return(-errno);
  }
  return(0);
}


/* Restore a terminal into CANONICAL (COOKED) mode. */
int tty_can(fd)
int fd;
{
  if (ioctl(fd, TCSETS, &old_tty) < 0) {
	fprintf(stderr, "dip: cannot ioctl(%d, SETS): %d\n", fd, errno);
	return(-errno);
  }
  return(0);
}


/* Initialize the serial line. */
int tty_init(name)
char *name;
{
  char path[PATH_MAX];
  register char *sp;
  int fd, i;

  /* Try opening the TTY device. */
  if ((sp = strrchr(name, '/')) != (char *)NULL) *sp++ = '\0';
    else sp = name;
  sprintf(path, "/dev/%s", sp);
  if ((fd = open(path, O_RDWR)) < 0) {
	fprintf(stderr, "dip: open(%s, RW) == %d\n", path, errno);
	return(-errno);
  }
  if (opt_v) printf("TTY = %s (%d) ", path, fd);

  /* Save the current TTY state, and set new speed and flags. */
  if ((i = tty_raw(fd, -1, "8N1")) < 0) return(i);

  /* Size and allocate the I/O buffers. */
  in_size = TTY_BUFSIZE;
  out_size = in_size;
  in_buff = (char *) malloc(in_size);
  out_buff = (char *) malloc(out_size);
  if (in_buff == (char *)NULL || out_buff == (char *)NULL) {
	fprintf(stderr, "dip: cannot allocate (%d, %d) buffers (%d)\n",
						in_size, out_size, errno);
	return(-ENOMEM);
  }
  in_cnt = 0; out_cnt = 0;
  in_ptr = in_buff; out_ptr = out_buff;
  if (opt_v) printf("IBUF=%d OBUF=%d\n", in_size, out_size);
  out_size -= 4; /* safety */
  tty_fd = fd;
  return(fd);
}


/* Change the speed of the serial line. */
int tty_speed(speed)
int speed;
{
  struct termios tty;
  int baud;

  if (ioctl(tty_fd, TCGETS, &tty) < 0) {
	fprintf(stderr, "dip: cannot ioctl(%d, GETS): %d\n", tty_fd, errno);
	return(-errno);
  }
  if ((baud = set_baud(speed)) < 0) return(baud);
  tty.c_cflag |= ((tty.c_cflag & ~CBAUD) | baud);
  if (ioctl(tty_fd, TCSETS, &tty) < 0) {
	fprintf(stderr, "dip: cannot ioctl(%d, SETS): %d\n", tty_fd, errno);
	return(-errno);
  }
  return(0);
}


/* Read one character (byte) from the SLIP link. */
int tty_getc()
{
  register int s;

  if (in_cnt == 0) {
	s = read(tty_fd, in_buff, in_size);
	if (s < 0) return(-1);
	in_cnt = s;
	in_ptr = in_buff;
  }

  if (in_cnt < 0) {
	if (opt_v == 1) printf("SERIAL: I/O error.\n");
	return(-1);
  }

  s = (int) *in_ptr;
  s &= 0xFF;
  in_ptr++;
  in_cnt--;
  return(s);
}


/* Write one character (byte) to the SLIP link. */
int tty_putc(c)
register int c;
{
  register int s;

  if ((out_cnt == out_size) || (c == -1)) {
	s = write(tty_fd, out_buff, out_cnt);
	out_cnt = 0;
	out_ptr = out_buff;
	if (s < 0) return(-1);
  }

  if (c != -1) {
	*out_ptr = (char) c;
	out_ptr++;
	out_cnt++;
  }

  return(0);
}


/* Output a string of characters. */
void tty_puts(s)
register char *s;
{
  while(*s != '\0') tty_putc((int) *s++);
  tty_putc(-1);
}


/* Restore the SLIP terminal line. */
void tty_stop()
{
  if (tty_fd != -1) (void) tty_can(tty_fd);
}


/* Return the SERIAL driver's file descriptor. */
int tty_askfd()
{
  return(tty_fd);
}
