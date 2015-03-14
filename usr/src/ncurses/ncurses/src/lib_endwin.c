
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
**	lib_endwin.c
**
**	The routine endwin().
**
*/

#include "terminfo.h"
#include "curses.h"
#include "curses.priv.h"

int _isendwin;

int isendwin()
{
	return _isendwin;
}

extern int coloron;

int
endwin()
{
char buf[32];

#ifdef TRACE
	if (_tracing)
	    _tracef("endwin() called");
#endif

	reset_shell_mode();

	_isendwin = 1;

	mvcur(-1, -1, lines - 1, 0);

	if (exit_ca_mode)
	    tputs(exit_ca_mode, 1, outc);

	if (coloron == 1)
		tputs(orig_pair, 1, outc);

	if (curscr  &&  (curscr->_attrs != A_NORMAL))
	{
	    vidputs(A_NORMAL, outc);

	    curscr->_attrs = A_NORMAL;
	}

	fflush(SP->_ofp);
}
