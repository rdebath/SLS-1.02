
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
**	lib_overlay.c
**
**	The routines overlay() and overwrite().
**
*/

#include "curses.h"
#include "curses.priv.h"

/*
**
**	overlay(win1, win2)
**
**
**	overlay() writes win1 on win2 non-destructively.
**
**/

int overlay(WINDOW *win1, WINDOW *win2)
{
	int	col, line, last_line, last_col;
	short   *firstchar, *lastchar;
	chtype	*w1ptr, *w2ptr, attrs;

#ifdef TRACE
	if (_tracing)
	    _tracef("overlay(%x, %x) called", win1, win2);
#endif
	
	last_col = min(win1->_maxx, win2->_maxx);
	last_line = min(win1->_maxy, win2->_maxy);
	attrs = win2->_attrs;
	firstchar = win2->_firstchar;
	lastchar = win2->_lastchar;

	for(line = 0;  line <= last_line;  line++)
	{
	    short   fc, lc;
	    
	    w1ptr = win1->_line[line];
	    w2ptr = win2->_line[line];
	    fc = _NOCHANGE;
	    lc = 0;

	    for(col = 0;  col <= last_col;  col++)
	    {
		if ((*w1ptr & A_CHARTEXT) != ' ')
		{
		    *w2ptr = (*w1ptr & A_CHARTEXT) | attrs;
		    if (fc == _NOCHANGE)
		        fc = col;
		    lc = col;
		}

		w1ptr++;
		w2ptr++;
	    }
	    
	    if (*firstchar == _NOCHANGE)
	    {
	        *firstchar = fc;
		*lastchar = lc;
	    }
	    else if (fc != _NOCHANGE)
	    {
	        if (fc < *firstchar)
		    *firstchar = fc;

	        if (lc > *lastchar)
		    *lastchar = lc;
            }

	    firstchar++;
	    lastchar++;
	}
}


/*
**
**	overwrite(win1, win2)
**
**
**	overwrite() writes win1 on win2 destructively.
**
**/

int overwrite(WINDOW *win1, WINDOW *win2)
{
	int	col, line, last_line, last_col;
	short   *firstchar, *lastchar;
	chtype	*w1ptr, *w2ptr, attrs;

#ifdef TRACE
	if (_tracing)
	    _tracef("overwrite(%x, %x) called", win1, win2);
#endif
	
	last_col = min(win1->_maxx, win2->_maxx);
	last_line = min(win1->_maxy, win2->_maxy);
	attrs = win2->_attrs;
	firstchar = win2->_firstchar;
	lastchar = win2->_lastchar;

	for(line = 0;  line <= last_line;  line++)
	{
	    short   fc, lc;
	    
	    w1ptr = win1->_line[line];
	    w2ptr = win2->_line[line];
	    fc = _NOCHANGE;
	    lc = 0;

	    for(col = 0;  col <= last_col;  col++)
	    {
		if ((*w1ptr & A_CHARTEXT) != (*w2ptr & A_CHARTEXT))
		{
		    *w2ptr = (*w1ptr & A_CHARTEXT) | attrs;
		    if (fc == _NOCHANGE)
		        fc = col;
		    lc = col;
		}

		w1ptr++;
		w2ptr++;
	    }

	    if (*firstchar == _NOCHANGE)
	    {
	        *firstchar = fc;
		*lastchar = lc;
	    }
	    else if (fc != _NOCHANGE)
	    {
	        if (fc < *firstchar)
		    *firstchar = fc;

	        if (lc > *lastchar)
		    *lastchar = lc;
            }
	    
	    firstchar++;
	    lastchar++;
	}
}
