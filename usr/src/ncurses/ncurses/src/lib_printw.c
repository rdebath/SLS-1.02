
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
**	lib_printw.c
**
**	The routines printw(), wprintw() and friend.
**
*/

#include "curses.h"
#include "curses.priv.h"
#include <stdarg.h>


int printw(const char *fmt, ...)
{
va_list argp;
char buf[1024];

#ifdef TRACE
	if (_tracing)
	    _tracef("printw(%s,...) called", fmt);
#endif

	va_start(argp, fmt);
	vsprintf(buf, fmt, argp);
	va_end(argp);
	return(waddstr(stdscr, buf));
}



int wprintw(WINDOW *win, const char *fmt, ...)
{
va_list argp;
char buf[1024];

#ifdef TRACE
	if (_tracing)
	    _tracef("wprintw(%x,%s,...) called", win, fmt);
#endif

	va_start(argp, fmt);
	vsprintf(buf, fmt, argp);
	va_end(argp);
	return(waddstr(win, buf));
}



int mvprintw(int y, int x, const char *fmt, ...)
{
va_list argp;
char buf[1024];

	va_start(argp, fmt);
	vsprintf(buf, fmt, argp);
	va_end(argp);
	return(move(y, x) == OK ? waddstr(stdscr, buf) : ERR);
}



int mvwprintw(WINDOW *win, int y, int x, const char *fmt, ...)
{
va_list argp;
char buf[1024];

	va_start(argp, fmt);
	vsprintf(buf, fmt, argp);
	va_end(argp);
	return(wmove(win, y, x) == OK ? waddstr(win, buf) : ERR);
}

