
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
**	lib_clrbot.c
**
**	The routine wclrtobot().
**
*/

#include "curses.h"
#include "curses.priv.h"


int wclrtobot(WINDOW *win)
{
	chtype	*ptr, *end, *maxx = NULL;
	int	y, startx, minx;
	chtype	blank = ' ' | win->_attrs;

#ifdef TRACE
	if (_tracing)
	    _tracef("wclrtobot(%x) called", win);
#endif

	startx = win->_curx;

	for (y = win->_cury; y <= win->_regbottom; y++)
	{
	    minx = _NOCHANGE;
	    end = &win->_line[y][win->_maxx];

	    for (ptr = &win->_line[y][startx]; ptr <= end; ptr++)
	    {
		if (*ptr != blank)
		{
		    maxx = ptr;
		    if (minx == _NOCHANGE)
			minx = ptr - win->_line[y];
		    *ptr = blank;
		}
	    }

	    if (minx != _NOCHANGE)
	    {
		if (win->_firstchar[y] > minx
					||  win->_firstchar[y] == _NOCHANGE)
		    win->_firstchar[y] = minx;

		if (win->_lastchar[y] < maxx - win->_line[y])
		    win->_lastchar[y] = maxx - win->_line[y];
	    }

	    startx = 0;
	}
}
