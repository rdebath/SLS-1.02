
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
**	lib_erase.c
**
**	The routine werase().
**
*/

#include "curses.h"
#include "curses.priv.h"

int  werase(win)
WINDOW	*win;
{
int	y;
chtype	*sp, *end, *start, *maxx = NULL;
int	minx;
chtype	blank = ' ' | win->_attrs;

#ifdef TRACE
	if (_tracing)
	    _tracef("werase(%x) called ", win);
#endif

	for (y = win->_regtop; y <= win->_regbottom; y++)
	{
	    minx = _NOCHANGE;
	    start = win->_line[y];
	    end = &start[win->_maxx];

	    maxx = start;
	    for (sp = start; sp <= end; sp++)
	    {
		    maxx = sp;
		    if (minx == _NOCHANGE)
			minx = sp - start;

		    *sp = blank;

	    }

	    if (minx != _NOCHANGE)
	    {
		if (win->_firstchar[y] > minx
		    ||  win->_firstchar[y] == _NOCHANGE)
		    win->_firstchar[y] = minx;

		if (win->_lastchar[y] < maxx - win->_line[y])
		    win->_lastchar[y] = maxx - win->_line[y];
	    }
	}

	win->_curx = win->_cury = 0;
}
