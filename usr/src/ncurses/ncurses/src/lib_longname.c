
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
**	lib_longname.c
**
**	The routine longname().
**
*/

#include "curses.h"
#include "curses.priv.h"

char *
longname()
{
    	char	*ptr;

#ifdef TRACE
	if (_tracing)
	    _tracef("longname() called");
#endif

	for (ptr = ttytype + strlen(ttytype); ptr > ttytype; ptr--)
	    if (*ptr == '|')
		return(ptr + 1);

        return(ttytype);
}
