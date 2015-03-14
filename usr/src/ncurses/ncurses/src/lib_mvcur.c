
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
**
**	lib_mvcur.c
**
**	mvcur() 
**
*/

#include "terminfo.h"
#include "curses.h"
#include "curses.priv.h"


/*
**
**	mvcur(oldrow, oldcol, newrow, newcol)
**
**	mvcur() optimally moves the cursor from the position
**  specified by (oldrow, oldcol) to (newrow, newcol).  If
**  (oldrow, oldcol) == (-1, -1), mvcur() does not use relative
**  cursor motions.  If the coordinates are otherwise
**  out of bounds, it mods them into range.
**
**	Revisions needed:
**		eat_newline_glitch, auto_right_margin
*/

int mvcur(int oldrow, int oldcol, int newrow, int newcol)
{

#ifdef TRACE
	if (_tracing)
	    _tracef("mvcur(%d,%d,%d,%d) called",
		oldrow, oldcol, newrow, newcol);
#endif


	newrow %= lines;
	newcol %= columns;

	if (cursor_address)
		putp(tparm(cursor_address, newrow, newcol));
		
}

