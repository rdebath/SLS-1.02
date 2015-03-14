
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
**	lib_addstr.c
*
**	The routines waddstr(), waddchstr().
**
*/

#include "curses.h"
#include "curses.priv.h"

int
waddnstr(WINDOW *win, char *str, int n)
{
#ifdef TRACE
	if (_tracing)
		_tracef("waddnstr(%x,%s,%d) called", win, str, n);
#endif

	if (n < 0) {
		while (*str) {
		    if (waddch(win, (chtype)*str++) == ERR)
			return(ERR);
		}
		return OK;
	}

	while(n-- > 0) {
		if (waddch(win, (chtype)*str++) == ERR)
			return ERR;
	}
	return OK;
}

int
waddchnstr(WINDOW *win, chtype *str, int n)
{
#ifdef TRACE
	if (_tracing)
		_tracef("waddchnstr(%x,%x,%d) called", win, str, n);
#endif

	if (n < 0) {
		while (*str) {
		    if (waddch(win, (chtype)*str++) == ERR)
			return(ERR);
		}
		return OK;
	}

	while(n-- > 0) {
		if (waddch(win, (chtype)*str++) == ERR)
			return ERR;
	}
	return OK;
}
