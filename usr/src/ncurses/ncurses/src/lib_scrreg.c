
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
**	lib_scrreg.c
**
**	The routine wsetscrreg().
**
*/

#include "curses.h"
#include "curses.priv.h"


int wsetscrreg(WINDOW *win, int top, int bottom)
{
#ifdef TRACE
	if (_tracing)
	    _tracef("wsetscrreg(%x,%d,%d) called", win, top, bottom);
#endif

    	if (top >= win->_begy  && top <= win->_maxy &&
		bottom >= win->_begy  &&  bottom <= win->_maxy &&
		bottom > top)
	{
	    win->_regtop = top;
	    win->_regbottom = bottom;

	    return(OK);
	}
	else
	    return(ERR);
}
