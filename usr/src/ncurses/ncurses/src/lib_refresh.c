
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
 *	lib_refresh.c
 *
 *	The routines wrefresh() and wnoutrefresh().
 *
 */

#include "curses.h"
#include "curses.priv.h"


int wrefresh(WINDOW *win)
{
#ifdef TRACE
	if (_tracing)
	    _tracef("wrefresh(%x) called", win);
#endif

	if (win == curscr)
	    curscr->_clear = TRUE;
	else
	    wnoutrefresh(win);
	doupdate();
}



int wnoutrefresh(WINDOW *win)
{
	int	i, j;
	int	begx = win->_begx;
	int	begy = win->_begy;
	int	m, n;

#ifdef TRACE
	if (_tracing)
	    _tracef("wnoutrefresh(%x) called", win);
#endif

	for (i=0, m=begy; i <= win->_maxy; i++, m++)
	{

	    if (win->_firstchar[i] != _NOCHANGE)
	    {
		j = win->_firstchar[i];
		n = j + begx;
		for (; j <= win->_lastchar[i]; j++, n++)
		{
		    if (win->_line[i][j] != newscr->_line[m][n])
		    {
			newscr->_line[m][n] = win->_line[i][j];

			if (newscr->_firstchar[m] == _NOCHANGE)
			    newscr->_firstchar[m] = newscr->_lastchar[m] = n;
			else if (n < newscr->_firstchar[m])
			    newscr->_firstchar[m] = n;
			else if (n > newscr->_lastchar[m])
			    newscr->_lastchar[m] = n;
		    }
		}
	    }

	    win->_firstchar[i] = win->_lastchar[i] = _NOCHANGE;
	}

	if (win->_clear)
	{
	    win->_clear = FALSE;
	    newscr->_clear = TRUE;
	}

	if (! win->_leave)
	{
	    newscr->_cury = win->_cury + win->_begy;
	    newscr->_curx = win->_curx + win->_begx;
	}
}
