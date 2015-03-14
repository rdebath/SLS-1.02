
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
**	lib_insertln.c
**
**	The routine winsertln().
**
*/

#include "curses.h"
#include "curses.priv.h"


int  winsertln(WINDOW *win)
{
	chtype	*temp, *end;
	int	y;

#ifdef TRACE
	if (_tracing)
	    _tracef("winsertln(%x) called", win);
#endif

	temp = win->_line[win->_regbottom];

	win->_firstchar[win->_cury] = 0;
	win->_lastchar[win->_cury] = win->_maxx;

	for (y = win->_regbottom;  y > win->_cury;  y--)
	{
	    win->_line[y] = win->_line[y-1];

	    win->_firstchar[y] = 0;
	    win->_lastchar[y] = win->_maxx;
	}

	win->_line[win->_cury] = temp;

	for (end = &temp[win->_maxx];  temp <= end;  temp++)
	    *temp = ' ' | win->_attrs;
}
