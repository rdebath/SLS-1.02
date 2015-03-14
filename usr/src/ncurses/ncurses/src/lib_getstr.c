
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
**	lib_getstr.c
**
**	The routine wgetstr().
**
*/

#include "curses.h"
#include "curses.priv.h"
#include "unctrl.h"

#ifdef ORIGINAL
#define backspace() {							\
		    mvwaddstr(curscr, win->_begy + win->_cury,		\
				      win->_begx + win->_curx, "\b \b");\
		    waddstr(win, "\b \b");				\
		    fputs("\b \b", SP->_ofp);				\
		    fflush(SP->_ofp);					\
		    }
#else
#define backspace() {							\
		    mvwaddstr(curscr, win->_begy + win->_cury,		\
				      win->_begx + win->_curx, "\b \b");\
		    waddstr(win, "\b \b");				\
		    fputs("\b \b", SP->_ofp);				\
		    fflush(SP->_ofp);					\
		    SP->_curscol--;                                     \
		    }
#endif

int wgetstr(WINDOW *win,char *str)
{
	bool	oldnl, oldecho, oldraw, oldcbreak;
	char	erasec;
	char	killc;
	char	*oldstr;

#ifdef TRACE
	if (_tracing)
	    _tracef("wgetstr(%x,%x) called", win, str);
#endif

	oldnl = SP->_nl;
	oldecho = SP->_echo;
	oldraw = SP->_raw;
	oldcbreak = SP->_cbreak;
	nl();
	noecho();
	noraw();
	cbreak();

	erasec = erasechar();
	killc = killchar();

	oldstr = str;

	while ((*str = getc(SP->_ifp)) != ERR  &&  *str != '\n')
	{
	    if (*str == erasec)
	    {
		if (str > oldstr)
		{
		    str--;
		    backspace();
		    if (*str < ' ' ||  *str == '\177')
			backspace();
		}
	    }
	    else if (*str == killc)
	    {
		while (str > oldstr)
		{
		    str--;
		    backspace();
		    if (*str < ' ' ||  *str == '\177')
			backspace();
		}
	    }
	    else
	    {
		mvwaddstr(curscr, win->_begy + win->_cury,
				  win->_begx + win->_curx, unctrl(*str));
		waddstr(win, unctrl(*str));
		fputs(unctrl(*str), SP->_ofp);
		fflush(SP->_ofp);
#ifndef ORIGINAL
		SP->_curscol++;
#endif
		str++;
	    }
	}
#ifndef ORIGINAL
	waddch(win, '\n');
	wrefresh(win);
#endif

	if (! oldnl)
	    nonl();

	if (oldecho)
	    echo();

	if (oldraw)
	    raw();

	if (! oldcbreak)
	    nocbreak();

	if (*str == ERR)
        {
	    *str = '\0';
	    return(ERR);
	}

	*str = '\0';

#ifdef TRACE
	if (_tracing)
	    _tracef("\twgetstr returns %s", oldstr);
#endif

	return(OK);
}
