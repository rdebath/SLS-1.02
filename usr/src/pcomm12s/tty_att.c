/*
 * System V specific routines for manipulating the TTY
 */

#include <stdio.h>
#ifdef XENIX_3
#include <sys/types.h>
#include <sys/ioctl.h>
#endif /* XENIX_3 */
#include <termio.h>
#include <fcntl.h>
#include "dial_dir.h"
#include "param.h"

/*
 * Change the communication line settings to the new values.
 */

void
line_set()
{
	extern int fd;
	struct termio tbuf;

	/*
	 * The manual dial entry also serves to store the previous
	 * line settings.  How else would the manual dial entry
	 * know what line setting to use?
	 */
	if (dir->d_cur != 0) {
		dir->baud[0] = dir->baud[dir->d_cur];
		dir->parity[0] = dir->parity[dir->d_cur];
		dir->dbits[0] = dir->dbits[dir->d_cur];
		dir->sbits[0] = dir->sbits[dir->d_cur];
	}
					/* nothing to do! */
	if (fd == -1)
		return;
					/* get the current settings */
	ioctl(fd, TCGETA, &tbuf);
					/* set some beginning values */
	tbuf.c_cc[4] = 1;		/* VMIN */
	tbuf.c_cc[5] = 0;		/* VTIME */
	tbuf.c_oflag = 0;
	tbuf.c_iflag = 0;
	tbuf.c_cflag = (CREAD|HUPCL|CLOCAL);
	tbuf.c_lflag = 0;

	if (*param->flow == 'X')
		tbuf.c_iflag |= (IXON|IXOFF);
					/* strip high bit? */
	if (*param->strip == 'Y')
		tbuf.c_iflag |= ISTRIP;
					/* the baud rate */
	switch (dir->baud[dir->d_cur]) {
		case 300:
			tbuf.c_cflag |= B300;
			break;
		case 1200:
			tbuf.c_cflag |= B1200;
			break;
		case 2400:
			tbuf.c_cflag |= B2400;
			break;
		case 4800:
			tbuf.c_cflag |= B4800;
			break;
		case 9600:
			tbuf.c_cflag |= B9600;
			break;
		case 19200:
#ifdef B19200
			tbuf.c_cflag |= B19200;
#else /* B19200 */
#ifdef EXTA
			tbuf.c_cflag |= EXTA;
#endif /* EXTA */
#endif /* B19200 */
			break;
	}
					/* the parity */
	switch (dir->parity[dir->d_cur]) {
		case 'N':
			break;
		case 'O':
			tbuf.c_cflag |= (PARENB|PARODD);
			break;
		case 'E':
			tbuf.c_cflag |= PARENB;
			break;
	}
					/* the data bits */
	if (dir->dbits[dir->d_cur] == 8)
		tbuf.c_cflag |= CS8;
	else
		tbuf.c_cflag |= CS7;
					/* the stop bits */
	if (dir->sbits[dir->d_cur] == 2)
		tbuf.c_cflag |= CSTOPB;

					/* now set 'em! */
	ioctl(fd, TCSETAF, &tbuf);
	return;
}

/*
 * Put the stdin/stdout in terminal mode.  We've divided up the
 * responsibility for the line settings options between the serial port
 * and the stdin and stdout.
 */

void
term_mode()
{
	struct termio tbuf;

	ioctl(0, TCGETA, &tbuf);

	tbuf.c_cc[4] = 1;		/* VMIN */
	tbuf.c_cc[5] = 0;		/* VTIME */
	tbuf.c_iflag = 0;
	tbuf.c_oflag = 0;
	tbuf.c_lflag = 0;
					/* duplex */
	if (dir->duplex[dir->d_cur] == 'H')
		tbuf.c_lflag = ECHO;

	ioctl(0, TCSETAF, &tbuf);
	return;
}

/*
 * Put the TTY driver in the mode suitable for xmodem transfers.
 */

void
xmodem_mode(fds)
int fds;
{
	struct termio tbuf;

	ioctl(fds, TCGETA, &tbuf);
	/*
	 * Turn off the XON/XOFF flow control, turn off echoing, and
	 * switch to 8 bit no parity.
	 */
	tbuf.c_cc[4] = 1;		/* VMIN */
	tbuf.c_cc[5] = 0;		/* VTIME */
	tbuf.c_iflag = 0;		/* no flow control or mapping */
	tbuf.c_oflag = 0;		/* no char mapping or delays */
	tbuf.c_lflag = 0;		/* no echo or signals */
	tbuf.c_cflag &= ~(PARENB|CSIZE);/* no parity */
	tbuf.c_cflag |= CS8;		/* 8 bit */

	ioctl(fds, TCSETAF, &tbuf);
	return;
}

/*
 * Put the TTY line in a mode suitable for the ASCII transfer.  Puts the
 * terminal in the raw, non-blocking mode.
 */

void
ascii_mode(up)
int up;
{
	extern int fd;
	struct termio tbuf;

	ioctl(fd, TCGETA, &tbuf);
	tbuf.c_oflag = 0;
					/* flow control & 8th bit stripping */
	if (up) {
		tbuf.c_iflag = (ISTRIP|IXON);

					/* if no CR's, use NL delays */
		if (!strcmp(param->cr_up, "STRIP"))
			tbuf.c_oflag = (OPOST|ONLRET);

					/* CR delay times */
		switch (param->cr_delay) {
			case 0:
				break;
			case 100:
				tbuf.c_oflag |= (OPOST|CR2);
				break;
			case 150:
				tbuf.c_oflag |= (OPOST|CR3);
				break;
		}
	}
					/* if down loading */
	else
		tbuf.c_iflag = (ISTRIP|IXOFF);

	ioctl(fd, TCSETAF, &tbuf);
	return;
}

/*
 * Flush the file descriptor
 */

int
tty_flush(fds, mode)
int fds, mode;
{
	return(ioctl(fds, TCFLSH, mode));
}

/*
 * Wait for the output to drain
 */

int
tty_drain(fds)
int fds;
{
	return(ioctl(fds, TCSBRK, 1));
}

/*
 * Send a modem break
 */

int
tty_break(fds)
int fds;
{
	return(ioctl(fds, TCSBRK, 0));
}

/*
 * Fix the file descriptor so that a read is satisfied immediately.  When
 * read() is called it returns the character in the queue, or an error if
 * no key was pressed.
 */

int
tty_noblock(fds, on)
int fds, on;
{
	int current;

	current = fcntl(fds, F_GETFL, 0);
	if (on)
		return(fcntl(fds, F_SETFL, current | O_NDELAY));
	else
		return(fcntl(fds, F_SETFL, current & ~O_NDELAY));
}

/*
 * Get the current baud rate of the terminal
 */

int
my_speed()
{
	static int speed[15] = {0, 50, 75, 110, 134, 150, 200, 300, 600,
	 1200, 1800, 2400, 4800, 9600, 19200};
	struct termio tbuf;

	ioctl(0, TCGETA, &tbuf);
	return(speed[tbuf.c_cflag & CBAUD]);
}
