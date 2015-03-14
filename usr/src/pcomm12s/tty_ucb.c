/*
 * Berkeley specific routines for manipulating the TTY
 */

#include <stdio.h>
#include <sgtty.h>
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
	struct sgttyb tbuf;

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
	ioctl(fd, TIOCGETP, &tbuf);
					/* set some beginning values */
	tbuf.sg_flags = CBREAK;

	if (*param->flow == 'X')
		tbuf.sg_flags |= TANDEM;
					/* the baud rate */
	switch (dir->baud[dir->d_cur]) {
		case 300:
			tbuf.sg_ispeed = B300;
			tbuf.sg_ospeed = B300;
			break;
		case 1200:
			tbuf.sg_ispeed = B1200;
			tbuf.sg_ospeed = B1200;
			break;
		case 2400:
			tbuf.sg_ispeed = B2400;
			tbuf.sg_ospeed = B2400;
			break;
		case 4800:
			tbuf.sg_ispeed = B4800;
			tbuf.sg_ospeed = B4800;
			break;
		case 9600:
			tbuf.sg_ispeed = B9600;
			tbuf.sg_ospeed = B9600;
			break;
		case 19200:
#ifdef B19200
			tbuf.sg_ispeed = B19200;
			tbuf.sg_ospeed = B19200;
#else /* B19200 */
#ifdef EXTA
			tbuf.sg_ispeed = EXTA;
			tbuf.sg_ospeed = EXTA;
#endif /* EXTA */
#endif /* B19200 */
			break;
	}
					/* the parity */
	switch (dir->parity[dir->d_cur]) {
		case 'N':
			tbuf.sg_flags |= ANYP;
			break;
		case 'O':
			tbuf.sg_flags |= ODDP;
			break;
		case 'E':
			tbuf.sg_flags |= EVENP;
			break;
	}
					/* now set 'em! */
	ioctl(fd, TIOCSETP, &tbuf);
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
	struct sgttyb tbuf;

	ioctl(0, TIOCGETP, &tbuf);
	
	tbuf.sg_flags |= CBREAK;
	tbuf.sg_flags &= ~(RAW|CRMOD|ECHO);

	if (dir->duplex[dir->d_cur] == 'H')
		tbuf.sg_flags |= ECHO;

	ioctl(0, TIOCSETP, &tbuf);
	return;
}

/*
 * Put the TTY driver in the mode suitable for xmodem transfers.
 */

void
xmodem_mode(fds)
int fds;
{
	struct sgttyb tbuf;

	ioctl(fds, TIOCGETP, &tbuf);
	/*
	 * Turn off the XON/XOFF flow control, turn off echoing, and
	 * switch to 8 bit no parity.
	 */
	tbuf.sg_flags |= (RAW|ANYP);
	tbuf.sg_flags &= ~ECHO;
	ioctl(fds, TIOCSETP, &tbuf);
	return;
}

/*
 * Put the TTY line in a mode suitable for the ASCII transfer.
 */

void
ascii_mode(up)
int up;
{
	extern int fd;
	struct sgttyb tbuf;

	ioctl(fd, TIOCGETP, &tbuf);

	tbuf.sg_flags |= (CBREAK|TANDEM);
	tbuf.sg_flags &= ~(RAW|CRMOD|ECHO|CRDELAY);

	if (up) {
					/* CR delay times */
		switch (param->cr_delay) {
			case 0:
				break;
			case 100:
				tbuf.sg_flags |= CR1;
				break;
			case 150:
				tbuf.sg_flags |= CR2;
				break;
		}
	}

	ioctl(fd, TIOCSETP, &tbuf);
	return;
}

/*
 * Flush the file descriptor.  Very messy... flushing the input causes a
 * wait for the ouput to drain, and there is no output flushing.
 */

int
tty_flush(fds, mode)
int fds, mode;
{
	int ret_code = 0;
	struct sgttyb tbuf;

	switch(mode) {
		case 0:			/* flush input queue */
			ioctl(fds, TIOCGETP, &tbuf);
			ioctl(fds, TIOCSETP, &tbuf);
			break;
		case 1:			/* flush output queue */
			/* sorry! */
			break;
		case 2:			/* flush both input and output */
			ioctl(fds, TIOCFLUSH, 0);
			break;
		default:
			ret_code++;
			break;
	}
	return(ret_code);
}

/*
 * Wait for the output to drain
 */

int
tty_drain(fds)
int fds;
{
	struct sgttyb tbuf;
					/* this flushes the input too */
	ioctl(fds, TIOCGETP, &tbuf);
	return(ioctl(fds, TIOCSETP, &tbuf));
}

/*
 * Send a modem break
 */

int
tty_break(fds)
int fds;
{
	unsigned int sleep();

	ioctl(fds, TIOCSBRK, (struct sgttyb *) 0);
	sleep(1);
	return(ioctl(fds, TIOCCBRK, (struct sgttyb *) 0));
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
		return(fcntl(fds, F_SETFL, current | FNDELAY));
	else
		return(fcntl(fds, F_SETFL, current & ~FNDELAY));
}

/*
 * Get the current baud rate of the terminal
 */

int
my_speed()
{
	static int speed[15] = {0, 50, 75, 110, 134, 150, 200, 300, 600,
	 1200, 1800, 2400, 4800, 9600, 19200};
	struct sgttyb tbuf;

	ioctl(0, TIOCGETP, &tbuf);
	return(speed[tbuf.sg_ispeed]);
}
