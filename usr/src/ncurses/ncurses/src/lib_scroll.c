
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
**	lib_scroll.c
**
**	The routine scroll().
**
*/

#include "curses.h"
#include "curses.priv.h"

int
wscrl(WINDOW *win, int n)
{
int	line, i;
chtype	*ptr, *temp;
chtype	blank = ' ' | win->_attrs;

#ifdef TRACE
	if (_tracing)
	    _tracef("wscrl(%x,%d) called", win, n);
#endif

	if (! win->_scroll)
	    return ERR;

	/* test for scrolling region == entire screen */

	if (n < 0) {

		temp = win->_line[win->_regbottom];
		for (line = win->_regbottom; line > win->_regtop; line--) {
			win->_line[line] = win->_line[line+n];
		}
		
		win->_line[win->_regtop] = temp;
		for (i = win->_regtop; i < win->_regtop-n; i++) {
			temp = win->_line[i];
			for (ptr = temp; ptr - temp <= win->_maxx; ptr++)
	    			*ptr = blank;
		}
	} else
	if (n > 0) {

		temp = win->_line[win->_regtop];
		for (line = win->_regtop; line < win->_regbottom; line++) {
	    		win->_line[line] = win->_line[line+n];
		}

		win->_line[win->_regbottom] = temp;
		for (i = win->_regbottom; i > win->_regbottom - n; i--) {
			temp = win->_line[i];
			for (ptr = temp; ptr - temp <= win->_maxx; ptr++)
	    			*ptr = blank;
		}
	}
	if ( n != 0)
		touchline(win, win->_regtop, win->_regbottom - win->_regtop + 1);
	return OK;
}
