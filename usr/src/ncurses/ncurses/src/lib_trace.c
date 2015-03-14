
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
 *	lib_trace.c - Tracing/Debugging routines
 */

#include <fcntl.h>
#include <unistd.h>
#include <stdarg.h>
#include <errno.h>
#include "terminfo.h"
#include "curses.h"
#include "curses.priv.h"

int _tracing =
#ifdef TRACE
	1
#else
	0
#endif
;  

int	tracefd;

void _tracef(char *fmt, ...);

void _init_trace()
{
static int	been_here = 0;

	if (! been_here)
	{
	    been_here = 1;

	    if ((tracefd = creat("trace", 0644)) < 0)
	    {
		write(2, "curses: Can't open 'trace' file: ", 33);
		write(2, sys_errlist[errno], strlen(sys_errlist[errno]));
		write(2, "\n", 1);
		exit(1);
	    }
	}
}


void traceon()
{
	_tracef("traceon() called");

    	_tracing = 1;
}


void traceoff()
{
	_tracef("traceoff() called");

    	_tracing = 0;
}


void
_tracef(char *fmt, ...)
{
va_list ap;
char buffer[256];

	va_start(ap, fmt);
	vsprintf(buffer, fmt, ap);
	write(tracefd, buffer, strlen(buffer));
	write(tracefd, "\n", 1);
}

