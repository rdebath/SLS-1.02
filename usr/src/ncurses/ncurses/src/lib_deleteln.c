
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
**	lib_deleteln.c
**
**	The routine wdeleteln().
**
*/

#include "curses.h"
#include "curses.priv.h"


int wdeleteln(WINDOW *win)
{
	chtype	*end, *temp;
	int	y;

#ifdef TRACE
	if (_tracing)
	    _tracef("wdeleteln(%x) called", win);
#endif

	temp = win->_line[win->_cury];

	for (y = win->_cury; y < win->_maxy; y++)
	{
	    win->_line[y] = win->_line[y+1];

	    win->_firstchar[y] = 0;
	    win->_lastchar[y] = win->_maxx;
	}

	win->_line[win->_maxy] = temp;
	win->_firstchar[win->_maxy] = 0;
	win->_lastchar[win->_maxy] = win->_maxx;

	for (end = &(temp[win->_maxx]); temp <= end; )
	    *temp++ = ' ' | win->_attrs;
}
