/*
 * dip		A program for handling dialup IP connecions.
 *		This module handles the SLIP protocol.
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1992 MicroWalt Corporation
 *		This program is free software; you can redistribute it and/or
 *		modify it under the terms of the GNU General Public License
 *		as published by the Free Software Foundation; either version
 *		2 of the License, or (at your option) any later version.
 */
#include "dip.h"

static int done = 0;

#if DIP_SLIP

/* SLIP protocol constants. */
#define	END		0xC0		/* end-of-datagram marker	*/
#define	ESC		0xDB		/* Binary Escape marker		*/
#define	ESC_END		0xDC		/* Escaped END character	*/
#define	ESC_ESC		0xDD		/* Escaped ESCAPE character	*/


static int sl_mtu = -1;			/* SLIP max transfer unit	*/
static int sl_fd = -1;			/* SLIP file descriptor		*/
static int sl_odisc = 0;		/* SLIP prev. line disc.	*/


static _PROTOTYPE( int sl_recvp, (char *p, int maxlen)			);
static _PROTOTYPE( void sl_sendp, (char *ptr, int len)			);
static _PROTOTYPE( void in_slip, (int fd)				);
static _PROTOTYPE( void out_slip, (int fd)				);


/* Catch any signals. */
static void sig_catcher(sig)
int sig;
{
  (void) ioctl(sl_fd, TIOCSETD, &sl_odisc);
  (void) tty_can(sl_fd);
  exit(0);
}


/*
 * Receive a full IP datagram from a SLIP link.
 * This means, that we have to store incoming characters in
 * a buffer, and, when a full datagram has been received, we
 * have to send it to the TNET program for further processing.
 *
 * We perform the upcall to TNET by writing our IP datagram,
 * with its total length in bytes prepended, to the write end
 * of our pipe link with TNET.
 */
static int sl_recvp(p, maxlen)
char *p;
int maxlen;
{
  int received;
  int all_of_it;
  register int c;
  register char *x;

  received = 0;
  all_of_it = 0;
  x = p;

  while(!all_of_it) {
	c = tty_getc();
	if (c == -1) return(-1);

	switch(c) {
		case END:
			if (received > 0) all_of_it = 1;
			  else p = x;
			break;
		case ESC:
			c = tty_getc();
			if (c == -1) {
				all_of_it = 1;
				c = 0;
			}
			switch(c) {
				case ESC_END:
					c = END;
					break;
				case ESC_ESC:
					c = ESC;
					break;
			}
			/* FALL_THROUGH */
		default:
			if (received++ < maxlen) *p++ = (char) c;
	}
  }

  /* We now have a full IP datagram in the buffer. */
  if (opt_v) printf("dip: IP LEN=%d received.\n", received);

  /* Dump contents of datagram if needed. */
  if (opt_v) ip_dump(x, received);

  return(received);
}


/*
 * Encapsulate incoming IP datagram bytes into a SLIP frame, and
 * send the frame to ths serial line when a complete frame is done.
 */
static void sl_sendp(ptr, len)
register char *ptr;
int len;
{
  register int c;

  tty_putc(END);
  while(len--) {
	c = ((int) *ptr) & 0xFF;
	switch(c) {
		case END:
			tty_putc(ESC);
			tty_putc(ESC_END);
			break;
		case ESC:
			tty_putc(ESC);
			tty_putc(ESC_ESC);
			break;
		default:
			tty_putc(c);
	}
	ptr++;
  }
  tty_putc(END);
  tty_putc(-1);			/* there it goes... */
}


/*
 * This is the input half of the SLIP driver.
 * We read bytes from the serial line (via a buffered READ),
 * and de-slip the encapsulated IP datagram on the fly. The
 * resulting datagram is then written to the output channel,
 * prepended with the total datagram size in bytes.
 */
static void in_slip(fd)
int fd;
{
  int s, n;
  int maxlen;
  char *buffer;

  /* Allocate a buffer for the decoded IP datagrams. */
  maxlen = 2 * sl_mtu;
  buffer = (char *) malloc(maxlen + sizeof(int));
  if (buffer == (char *)NULL) {
	fprintf(stderr, "dip: IN: cannot allocate my buffer!\n");
	return;
  }

  while(1) {
	n = sl_recvp(buffer + sizeof(int), maxlen);
	if (n < 0) continue;

	*(int *)buffer = n;
	s = write(fd, buffer, sizeof(int) + n);
	if (s != (sizeof(int) + n)) {
		fprintf(stderr, "dip: IN: dgram write error %d(%d)\n",
								errno, fd);
	}
  }
}


/*
 * This is the SLIP datagram sender.
 * We start an endless loop, reading the "SLIP header" from the
 * I/O input channel.  This header is simply a word which tells
 * us the total length of the IP datagram which we have to en-
 * capsulate into a SLIP frame.  When the encapsulation is done,
 * the frame is sent to the serial line.
 */
static void out_slip(fd)
int fd;
{
  int s, n;
  int maxlen;
  char *buffer;

  /* Allocate a buffer for the outgoing SLIP frame. */
  maxlen = 2 * sl_mtu;
  buffer = (char *) malloc(maxlen);
  if (buffer == (char *)NULL) {
	fprintf(stderr, "dip: OUT: cannot allocate my buffer!\n");
	return;
  }

  while(1) {
	s = read(fd, (char *) &n, sizeof(int));
	if (s == 0) return;
	if (s != sizeof(int)) {
		fprintf(stderr, "dip: OUT: dghdr read error %d(%d)\n",
								errno, fd);
		continue;
	}
	if (n > maxlen) continue;

	/* Read in the entire datagram. */
	s = read(fd, buffer, n);
	if (s != n) {
		fprintf(stderr, "dip: OUT: dg read error %d(%d)\n",
								errno, fd);
		continue;
	}

	/* We now have a full IP datagram in the buffer. */
	if (opt_v) {
		printf("dip: OUT: IP LEN=%d sent.\n", n);
		ip_dump(buffer, n);
	}
	(void) sl_sendp(buffer, n);
  }
}


int do_slip(fd, addr, mtu)
int fd;
struct in_addr addr;
int mtu;
{
  char ifname[32];
  char pid_path[255];
  FILE *pid_fp;
  int disc, unit, i, pid;
  void * die();

  /* First off, ask the SERIAL driver for its fd. */
  if (fd == -1) fd = tty_askfd();
  sl_fd = fd;
  sl_mtu = mtu;

  /* Catch all signals. */
  for(unit = 1; unit < 32; unit++) (void) signal(unit, sig_catcher);

  /* Save current line discipline for later restore. */
  if (ioctl(sl_fd, TIOCGETD, &sl_odisc) < 0) {
	perror("slip GETD");
	return(-1);
  }
  (void) tty_raw(sl_fd, -1, "8N1");

  /* Put line in SLIP discipline. */
  disc = N_SLIP;
  if (ioctl(sl_fd, TIOCSETD, &disc) < 0) {
	perror("slip SETD");
	return(-1);
  }

  /*
   * Usually, when running in SLIP discipline, one can ask the kernel
   * for the unit number of the SLIP driver that it allocated to this
   * line by issuing another "GETD" ioctl call.  With this unit number,
   * one can then call the external programs "ifconfig" and "route" to
   * set up the newly allocated interface.
   */
  if (ioctl(sl_fd, TIOCGETD, &unit) < 0) {
	perror("slip GUNIT");
	return(-1);
  }
  sprintf(ifname, "sl%d", unit);
  
  /* Add the route to that host. */
  (void) rt_add(ifname, &addr);

  fprintf(stderr, "SLIP connection made.  Type 'kill `cat /etc/dip.pid.%d`' to kill\n", unit);

  if (sl_fd == 0) {
	while(1) {
		sleep(60);
	};
  } else {
	if ((pid = fork()) == -1) {
		perror("fork");
		exit(1);
	}
	if (pid)
		exit(0);
	setsid();
	sprintf(pid_path, "%s.%d", PID_PATH, unit);
	if ((pid_fp = fopen(pid_path, "w")) == NULL) {
		perror("dip");
		sig_catcher(0);
		return(0);
	}
	fprintf(pid_fp, "%d", getpid());
	fclose(pid_fp);
	(void) signal(SIGTERM, die);
	(void) signal(SIGINT, SIG_IGN);
	(void) signal(SIGQUIT, SIG_IGN);
	(void) signal(SIGALRM, SIG_IGN);
	while (!done) pause();
	unlink(pid_path);
  }

  sig_catcher(0);
  return(0);
}

void *
die()
{
	done = 1;
}
#endif	/* DIP_SLIP */
