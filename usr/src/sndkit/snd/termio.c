/* termio.c */
/* special termio discipline for sun/sgi,
 * for non blocking io and such.
 * These functions should not be too difficult
 * to write for a PC.
 */

/* $Id: termio.c,v 3.7 1993/01/16 16:23:33 espie Exp espie $
 * $Log: termio.c,v $
 * Revision 3.7  1993/01/16  16:23:33  espie
 * Hsavolai fix.
 *
 * Revision 3.6  1993/01/15  14:00:28  espie
 * Added bg/fg test.
 *
 * Revision 3.5  1993/01/06  17:58:39  espie
 * Added changes for linux.
 *
 * Revision 3.4  1992/12/03  15:00:50  espie
 * restore stty.
 *
 * Revision 3.3  1992/11/27  10:29:00  espie
 * General cleanup
 *
 * Revision 3.2  1992/11/22  17:20:01  espie
 * Added update_frequency call, mostly unchecked
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 */

#ifdef linux
#include <termios.h>
#else
#include <sys/termio.h>
#endif
#include <stdio.h>
#include <signal.h>
#include "defs.h"

LOCAL struct termio sanity;

LOCAL BOOL is_fg;

/* signal handler */

LOCAL void goodbye(sig)
int sig;
    {
    fprintf(stderr, "\nSignal %d", sig);
    discard_buffer();
    end_all();
    }

BOOL run_in_fg()
	{
	int val;
	/* real check for running in foreground */
	if (ioctl(fileno(stdin), TIOCGPGRP, &val))
		return FALSE; 
	if (val == getpgrp())
		return TRUE;
	else
		return FALSE;
	}

LOCAL void switch_mode()
	{
	struct termio zap;

	signal(SIGCONT, switch_mode);
	signal(2, goodbye);
	signal(3, goodbye);

	if (run_in_fg())
		{
		ioctl(fileno(stdin), TCGETA, &zap);
#ifdef linux
		zap.c_cc[VMIN] = 0;
		zap.c_cc[VTIME] = 0;
		zap.c_lflag = 0;
#else
		zap.c_cc[VEOL] = 0;
		zap.c_cc[VEOF] = 0;
		zap.c_lflag &= ~(ICANON | ECHO);
#endif
		ioctl(fileno(stdin), TCSETA, &zap);
		is_fg = TRUE;
		}
	else
		is_fg = FALSE;
	}

void nonblocking_io()
	{
	/* try to renice our own process to get more cpu time */
	nice(-15);
	ioctl(fileno(stdin), TCGETA, &sanity);
	switch_mode();
	}


void sane_tty()
	{
	ioctl(fileno(stdin), TCSETA, &sanity);
	}

int may_getchar()
	{
	char buffer;

	if (run_in_fg() && !is_fg)
		switch_mode();
	if (run_in_fg() && read(fileno(stdin), &buffer, 1))
		return buffer;
	return EOF;
	}

