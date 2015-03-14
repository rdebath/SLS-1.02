
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
**	lib_scanw.c
**
**	The routines scanw(), wscanw() and friend.
**
*/

#include "curses.h"
#include "curses.priv.h"
#include <stdarg.h>

static int sscans(WINDOW *, char *, ...);

int scanw(char *fmt, ...)
{
va_list ap;

#ifdef TRACE
	if (_tracing)
	    _tracef("scanw(%s,...) called", fmt);
#endif

	va_start(ap, fmt);
	return(sscans(stdscr, fmt, ap));
}



int wscanw(WINDOW *win, char *fmt, ...)
{
va_list ap;

#ifdef TRACE
	if (_tracing)
	    _tracef("wscanw(%x,%s,...) called", win, fmt);
#endif

	va_start(ap, fmt);
	return(sscans(win, fmt, ap));
}



int mvscanw(int y, int x, char *fmt, ...)
{
va_list ap;

	va_start(ap, fmt);
	return(move(y, x) == OK ? sscans(stdscr, fmt, ap) : ERR);
}



int mvwscanw(WINDOW *win, int y, int x, char *fmt, ...)
{
va_list ap;

	va_start(ap, fmt);
	return(wmove(win, y, x) == OK ? sscans(win, fmt, ap) : ERR);
}


/*
**	This routine actually executes the scanf from the window.
**
*/

static
int sscans(WINDOW *win, char *fmt, ...)
{
va_list argp; 
char buf[100];
int result;

	va_start(argp,fmt);
	if (wgetstr(win, buf) == ERR)
	    return(ERR);
	
	result = sscanf(buf, fmt, argp);
	va_end(argp);
	return result;   
}
