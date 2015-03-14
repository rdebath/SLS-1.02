
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
**	lib_tstp.c
**
**	The routine tstp().
**
*/

#include "terminfo.h"
#include "curses.h"
#include "curses.priv.h"
#include <signal.h>

static void cont(int);
static int creceived;

void
tstp()
{
struct sigaction act, oact;
#ifdef TRACE
	if (_tracing)
	    _tracef("tstp() called");
#endif

	endwin();

	sigprocmask(0, NULL, &act.sa_mask);
	act.sa_handler = cont;
	act.sa_flags = 0;
	sigaction(SIGCONT, &act, &oact);
	sigdelset(&act.sa_mask, SIGCONT);
	creceived = 0;
	while (!creceived)
		sigsuspend(&act.sa_mask);
	sigaction(SIGCONT, &oact, NULL);

	reset_prog_mode();
	flushinp();
	tputs(enter_ca_mode, 1, outc);
	wrefresh(curscr);
}


void cont(int signo)
{
	creceived = 1;
}
