
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
 *	lib_doupdate.c
 *
 *	The routine doupdate() and its dependents
 *
 */

#include <signal.h>
#include "curses.h"
#include "curses.priv.h"
#include "terminfo.h"

inline void PutAttrChar(chtype ch)
{
	if (curscr->_attrs != (ch & A_ATTRIBUTES)) {
		curscr->_attrs = ch & A_ATTRIBUTES;
		vidputs(curscr->_attrs, outc);
	}
	putc(ch & A_CHARTEXT, SP->_ofp);
}

inline void PutChar(chtype ch)
{
	if (!auto_right_margin || SP->_cursrow != lines-1 || SP->_curscol != columns) {   
	    PutAttrChar(ch); 
	    SP->_curscol++; 
	    if (SP->_curscol >= columns)  
			if (auto_right_margin) {     
			    SP->_curscol = 0;       
			    SP->_cursrow++;        
			} else 
			    SP->_curscol--;    
		}
}	

inline void GoTo(int row, int col)
{
	mvcur(SP->_cursrow, SP->_curscol, row, col); 
	SP->_cursrow = row; 
	SP->_curscol = col; 
}

static  void ClearScreen(void);
static  void TransformLine(int);
static  void UpdateLines(void),ClrUpdate(WINDOW *);

int outc(char ch)
{
    	putc(ch, SP->_ofp);
}

extern int _isendwin;

int doupdate()
{
int	i;
struct sigaction act, oact;

#ifdef TRACE
	if (_tracing)
	    _tracef("doupdate() called");
#endif

	act.sa_handler = SIG_IGN;
	sigemptyset(&act.sa_mask);
	act.sa_flags = 0;
	sigaction(SIGTSTP, &act, &oact);

	if (_isendwin == 1) {
		reset_prog_mode();
		if (enter_alt_charset_mode)
			init_acs();  
		curscr->_clear = TRUE;
		_isendwin = 0;
	}

	if (curscr->_clear)
	{
	    ClrUpdate(curscr);
	    curscr->_clear = FALSE;

	    GoTo(curscr->_cury, curscr->_curx);
	} else {
	    if (newscr->_clear) {
			ClrUpdate(newscr);
			newscr->_clear = FALSE;
	    } else
	    	UpdateLines();

	    for (i = 0; i < lines; i++)
	    {
			newscr->_firstchar[i] = _NOCHANGE;
			newscr->_lastchar[i] = _NOCHANGE;
	    }

	    curscr->_curx = newscr->_curx;
	    curscr->_cury = newscr->_cury;

	    GoTo(curscr->_cury, curscr->_curx);
	}

	fflush(SP->_ofp);

	sigaction(SIGTSTP, &oact, NULL);
}

/*
**	ClrUpdate(scr)
**
**	Update by clearing and redrawing the entire screen.
**
*/

static void ClrUpdate(WINDOW *scr)
{
int	i, j;

#ifdef TRACE
	if (_tracing)
		_tracef("ClrUpdate(%x) called", scr);
#endif

	ClearScreen();

	for (i = 0; i < lines; i++)
	{
	    for (j = 0; j < columns; j++)
			PutAttrChar(scr->_line[i][j]);

	    if (! auto_right_margin)
	    {
			SP->_cursrow = i;
			SP->_curscol = 0;
			GoTo(i + 1, 0);
	    }
	}

	if (scr != curscr)
	{
	    for (i = 0; i < lines; i++)
			for (j = 0; j < columns; j++)
			    curscr->_line[i][j] = scr->_line[i][j];
	}
}


/*
**	UpdateLines()
**
**	Update screen without using Insert/Delete Line capabilities
**
*/

static void UpdateLines()
{
int	i;

#ifdef TRACE
	if (_tracing)
	    _tracef("UpdateLines() called");
#endif

	for (i = 0; i < lines; i++)
	    if (newscr->_firstchar[i] != _NOCHANGE)
			TransformLine(i);
}



/*
**	TransformLine(lineno)
**
**	Transform the given line in curscr to the one in newscr, without
**	using Insert/Delete Character.
**
**		firstChar = position of first different character in line
**		lastChar = position of last different character in line
**
**		overwrite all characters between firstChar and lastChar.
**
*/

static void TransformLine(int lineno)
{
int	firstChar, lastChar;
chtype	*newLine = newscr->_line[lineno];
chtype	*oldLine = curscr->_line[lineno];
int     k;

#ifdef TRACE
	if (_tracing)
	    _tracef("TransformLine(%d) called", lineno);
#endif

	firstChar = 0;
	while (firstChar < columns && newLine[firstChar] == oldLine[firstChar]) {
	    firstChar++;
	}

	if (firstChar >= columns)
	    return;

	lastChar = columns - 1;
	while (lastChar > firstChar && newLine[lastChar] == oldLine[lastChar])
	    lastChar--;
    
	GoTo(lineno, firstChar);

	for (k = firstChar; k <= lastChar; k++)
	{
	    PutChar(newLine[k]);
	    oldLine[k] = newLine[k];
	}
}


/*
**	ClearScreen()
**
**	Clear the physical screen and put cursor at home
**
*/

static void ClearScreen()
{
	if (clear_screen)
	{
	    tputs(clear_screen, 1, outc);
	    SP->_cursrow = SP->_curscol = 0;
	}
	else if (clr_eos)
	{
	    SP->_cursrow = SP->_curscol = -1;
	    GoTo(0,0);

	    tputs(clr_eos, 1, outc);
	}
	else if (clr_eol)
	{
	    SP->_cursrow = SP->_curscol = -1;

	    while (SP->_cursrow < lines)
	    {
		GoTo(SP->_cursrow, 0);
		tputs(clr_eol, 1, outc);
	    }

	    GoTo(0,0);
	}
}


