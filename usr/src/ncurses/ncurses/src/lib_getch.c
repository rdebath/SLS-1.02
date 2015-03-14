
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
**	lib_getch.c
**
**	The routine getch().
**
*/

#include <sys/ioctl.h>
#include <sys/time.h>
#include <unistd.h>
#include <signal.h>
#include "curses.h"
#include "curses.priv.h"

static int n = 0;

inline int nextc()
{
unsigned char buffer[10];
int i;

	if (SP->_backcnt > 0) {
		return SP->_backbuf[--SP->_backcnt];
	} else {
		n = read(fileno(SP->_ifp), buffer, 10);
		for (i = 0; i < n; i++) 
			SP->_backbuf[i] = buffer[n-i-1];
		SP->_backcnt = n; 
		return SP->_backbuf[--SP->_backcnt];
	}

}

int ungetch(int ch)
{
	if (SP->_backcnt >= 10)
		return ERR;
	else
		SP->_backbuf[SP->_backcnt++] = ch;
	return OK;

}


static int kgetch(WINDOW *);

fd_set set;
struct timeval timeout;

int
wgetch(win)
WINDOW	*win;
{
bool	setHere = FALSE;	/* cbreak mode was set here */
int	ch; 

#ifdef TRACE
	if (_tracing)
	    _tracef("wgetch(%x) called", win);
#endif

	if (! win->_scroll  
		&&  (SP->_echo)
		&&  (win->_flags & _FULLWIN)
		&&  win->_curx == win->_maxx
		&&  win->_cury == win->_maxy)
	    return(ERR);

	if (is_wintouched(win))
		wrefresh(win);

	if (SP->_echo  &&  ! (SP->_raw  ||  SP->_cbreak))
	{
		cbreak();
		setHere = TRUE;
	}


	FD_ZERO(&set);FD_SET(fileno(SP->_ifp), &set);

	if (win->_delay >= 0) {
	int result;

		timeout.tv_sec = 0;
		timeout.tv_usec = win->_delay;
		result = select(fileno(SP->_ifp)+1, &set, NULL, NULL, &timeout);
		if (result == 0)	/* no character available */
			return ERR;
		/* else go on to read data avialable */
	}

    if (win->_use_keypad)
        ch = kgetch(win);
    else
	    ch = nextc();

	if (SP->_echo  &&  ch < 0400)    /* ch < 0400 => not a keypad key */
	{
	    mvwaddch(curscr, win->_begy + win->_cury,
                             win->_begx + win->_curx, ch | win->_attrs);
	    waddch(win, ch | win->_attrs);
	}
	if (setHere)
	    nocbreak();

#ifdef TRACE
	_tracef("wgetch got %c, %d", ch, ch);
#endif

	return(ch);
}


/*
**      short
**      kgetch()
**
**      Get an input character, but take care of keypad sequences, returning
**      an appropriate code when one matches the input.  After each character
**      is received, set a one-second alarm call.  If no more of the sequence
**      is received by the time the alarm goes off, pass through the sequence
**      gotten so far.
**
*/

static int
kgetch(WINDOW *win)
{
struct try  *ptr;
char        ch;
int result;

#ifdef TRACE
	if (_tracing)
		_tracef("kgetch(%x) called", win);
#endif

	ptr = SP->_keytry;

	timeout.tv_sec = 0;
	timeout.tv_usec = 0;

	do
	{
		if (win->_notimeout || (timeout.tv_sec == 0 && timeout.tv_usec == 0)) {
			ch = nextc();
		} else {
			/* wait for the rest of sequence */
			if (SP->_backcnt > 0) {
				ch = nextc();
			} else {
				result = select(fileno(SP->_ifp)+1, &set, NULL, NULL, &timeout);
				if (result == 0) {
					goto resume;
				}
				else 
					ch = nextc();
			}
		}

    	while (ptr != NULL  &&  ptr->ch != ch) {
        	ptr = ptr->sibling;
	   	}

    	if (ptr != NULL)
        	if (ptr->value != (short) NULL) {
	    		return(ptr->value);
			} else {
				timeout.tv_sec = 1;
				timeout.tv_usec = 0;
	    		ptr = ptr->child;
			}
		} while (ptr != NULL);

	SP->_backcnt = n--;
	ch = nextc();

resume:
	return(ch);
}


