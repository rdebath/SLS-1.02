/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
 * lib_pad.c
 * newpad	-- create a new pad
 * pnoutrefresh -- refresh a pad, no update
 * pechochar	-- add a char to a pad and refresh
 */

#include <stdlib.h>
#include "curses.h"
#include "curses.priv.h"

extern WINDOW *makewin(int,int,int,int);

WINDOW *newpad(int l, int c)
{
WINDOW *win;
chtype *ptr;
int i, j;

	if (l <= 0 || c <= 0)
		return NULL;

	if ((win = makewin(l,c,0,0)) == NULL)
		return NULL;

	win->_flags |= _ISPAD;

	for (i = 0; i < l; i++) {
	    if ((win->_line[i] = (chtype *) calloc(c, sizeof(chtype))) == NULL) {
		for (j = 0; j < i; j++)
		    free(win->_line[j]);

		free(win->_firstchar);
		free(win->_lastchar);
		free(win->_line);
		free(win);

		return NULL;
	    }
	    else
		for (ptr = win->_line[i]; ptr < win->_line[i] + c; )
		    *ptr++ = ' ';
	}

#ifdef TRACE
	if (_tracing)
	    _tracef("newpad: returned window is %x", win);
#endif

	return(win);
}

int prefresh(WINDOW *win, int pminrow, int pmincol,
	int sminrow, int smincol, int smaxrow, int smaxcol)
{
int	i, j;
int	m, n;

	if (win->_flags & _ISPAD)
		return ERR;

	if (pminrow < 0) pminrow = 0;
	if (pmincol < 0) pmincol = 0;
	if (sminrow < 0) sminrow = 0;
	if (smincol < 0) smincol = 0;

	if (smaxrow >= LINES || smaxcol >= COLS)
		return ERR;

	if ((pminrow + smaxrow > win->_maxy) || (pmincol + smaxcol > win->_maxx))
		return ERR;

	for (i = pminrow, m = sminrow; i <= smaxrow + pminrow; i++, m++) {
		j = pmincol; 
		n = j + smincol;
		for (; j <= pmincol + smaxcol; j++, n++) {
		    if (win->_line[i][j] != newscr->_line[m][n]) {
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

	win->_begx = smincol;
	win->_begy = sminrow;

	if (win->_clear) {
	    win->_clear = FALSE;
	    newscr->_clear = TRUE;
	}

	if (! win->_leave) {
	    newscr->_cury = win->_cury + win->_begy;
	    newscr->_curx = win->_curx + win->_begx;
	}
	return OK;
}

int pechochar(WINDOW *pad, chtype ch)
{
int x, y;

	if (pad->_flags & _ISPAD)
		return ERR;

	x = pad->_begx + pad->_curx;
	y = pad->_begy + pad->_cury;

	waddch(curscr, ch);
	doupdate();
	return OK;
}

