
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
**	lib_move.c
**
**	The routine wmove().
**
*/

#include "curses.h"
#include "curses.priv.h"

int
wmove(win, y, x)
WINDOW	*win;
int	y, x;
{
#ifdef TRACE
	if (_tracing)
	    _tracef("wmove(%x,%d,%d) called", win, y, x);
#endif

	if (0 <= x  &&  x <= win->_maxx  &&
		win->_regtop <= y  &&  y <= win->_regbottom)
	{
	    win->_curx = x;
	    win->_cury = y;

	    return(OK);
	} else
	    return(ERR);
}
