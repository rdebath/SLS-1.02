
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
**	lib_newterm.c
**
** 	The newterm() function.
**
*/

#include <sys/ioctl.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include "curses.h"
#include "terminfo.h"
#include "curses.priv.h"

extern void tstp();

static void cleanup(int sig)
{

	if (sig == SIGSEGV)
		fprintf(stderr, "Got a segmentation violation signal, cleaning up and exiting\n");
	endwin();
	exit(1);
}

SCREEN *SP;

SCREEN * newterm(char *term, FILE *ofp, FILE *ifp)
{
struct sigaction act;
int	errret;

#ifdef TRACE
	_init_trace();
	if (_tracing)
	    _tracef("newterm(%s,%x,%x) called", term, ofp, ifp);
#endif

	if (setupterm(term, fileno(ofp), &errret) != 1)
	    return NULL;

	if ((SP = (SCREEN *) malloc(sizeof *SP)) == NULL)
	    return NULL;

	if (ofp == stdout && ifp == stdin)
	{
	    SP->_ofp       = stdout;
	    SP->_ifp       = stdin;
	}
	else
	{
	    SP->_ofp       = ofp;
	    SP->_ifp       = ofp;
	}
	SP->_term      	= cur_term;
	SP->_cursrow   	= -1;
	SP->_curscol   	= -1;
	SP->_keytry    	= UNINITIALISED;
	SP->_nl        	= TRUE;
	SP->_raw       	= FALSE;
	SP->_cbreak    	= FALSE;
	SP->_echo      	= TRUE;
	SP->_nlmapping 	= TRUE;
	SP->_backcnt	= 0;

	if (enter_ca_mode)
	    putp(enter_ca_mode);

	if (enter_alt_charset_mode)
		init_acs(); 

	if ((newscr = newwin(lines, columns, 0, 0)) == (WINDOW *)NULL)
	    return(NULL);

	if ((curscr = newwin(lines, columns, 0, 0)) == (WINDOW *)NULL)
	    return(NULL);

	if (_slk_initialize)
		_slk_initialize();

	SP->_newscr = newscr;
	SP->_curscr = curscr;

	newscr->_clear = TRUE;
	curscr->_clear = FALSE;

	act.sa_handler = tstp;
	sigemptyset(&act.sa_mask);
	sigaddset(&act.sa_mask, SIGCONT);
	act.sa_flags = 0;
	sigaction(SIGTSTP, &act, NULL);
	act.sa_handler = cleanup;
	sigemptyset(&act.sa_mask);
	act.sa_flags = 0;
	sigaction(SIGINT, &act, NULL);
	sigaction(SIGSEGV, &act, NULL);
	if (stdscr == NULL)
	    if ((stdscr = newwin(LINES, columns, 0, 0)) == NULL)
		return(NULL);

#ifdef TRACE
	if (_tracing)
	    _tracef("\tnewterm returns %x", SP);
#endif

	return(SP);
}
