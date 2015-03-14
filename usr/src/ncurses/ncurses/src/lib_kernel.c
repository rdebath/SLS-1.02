
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
 *	lib_kernel.c
 *
 *	Routines:
 *		def_prog_mode()
 *		def_shell_mode()
 *		baudrate()
 *		erasechar()
 *		killchar()
 *		flushinp()
 *		savetty()
 *		resetty()
 *
 *
 */

#include "curses.h"
#include "curses.priv.h"
#include "terminfo.h"
#ifndef TERMIOS
#include <sys/ioctl.h>
#endif


int wattron(WINDOW *win, chtype at)
{
	win->_attrs &= 0xffff00ff;
	win->_attrs |= at;
	return OK;
}

int def_prog_mode()
{
#ifdef TRACE
	if (_tracing)
	    _tracef("def_prog_mode() called");
#endif

#ifdef TERMIOS
	tcsetattr(cur_term->Filedes, TCSANOW, &cur_term->Nttyb);
#else
        stty(cur_term->Filedes, &cur_term->Nttyb);
#endif

	return OK; 
}


int def_shell_mode()
{
#ifdef TRACE
	if (_tracing)
	    _tracef("def_shell_mode() called");
#endif

#ifdef TERMIOS
	tcsetattr(cur_term->Filedes, TCSANOW, &cur_term->Ottyb);
#else
        stty(cur_term->Filedes, &cur_term->Ottyb);
#endif

	return OK; 
}


int curs_set(int vis)
{
#ifdef TRACE
	if (_tracing)
		_tracef("curs_set(%d)", vis);
#endif

	if (vis < 0 || vis > 2)
		return ERR;

	switch(vis) {
	case 2:
		if (cursor_visible)
			tputs(cursor_visible, 1, outc);
		break;
	case 1:
		if (cursor_normal)
			tputs(cursor_normal, 1, outc);
		break;
	case 0:
		if (cursor_invisible)
			tputs(cursor_invisible, 1, outc);
		break;
	}
	return OK;	
}

/*
 *	erasechar()
 *
 *	Return erase character as given in cur_term->Ottyb.
 *
 */

char
erasechar()
{
#ifdef TRACE
	if (_tracing)
	    _tracef("erasechar() called");
#endif

#ifdef TERMIOS
    return(cur_term->Ottyb.c_cc[VERASE]);
#else
    return(cur_term->Ottyb.sg_erase);
#endif

}



/*
 *	killchar()
 *
 *	Return kill character as given in cur_term->Ottyb.
 *
 */

char
killchar()
{
#ifdef TRACE
	if (_tracing)
	    _tracef("killchar() called");
#endif

#ifdef TERMIOS
    return(cur_term->Ottyb.c_cc[VKILL]);
#else
    return(cur_term->Ottyb.sg_kill);
#endif
}



/*
 *	flushinp()
 *
 *	Flush any input on cur_term->Filedes
 *
 */

int flushinp()
{
#ifdef TRACE
	if (_tracing)
	    _tracef("flushinp() called");
#endif

#ifdef TERMIOS
	tcflush(cur_term->Filedes, TCIFLUSH);
#else
        ioctl(cur_term->Filedes, TIOCFLUSH, 0);
#endif    
        if (SP)
	    	SP->_backcnt = 0;

}



/*
 *	int
 *	baudrate()
 *
 *	Returns the current terminal's baud rate.
 *
 */

static int speeds[] =
{
	0, 50, 75, 110, 134, 150, 200, 300, 600,
	1200, 1800, 2400, 4800, 9600, 19200, 38400
};

int
baudrate()
{
#ifdef UNTRACE
	if (_tracing)
	    _tracef("baudrate() called");
#endif
#ifdef TERMIOS
	return(speeds[cur_term->Nttyb.c_cflag & CBAUD]); 
#else
	return(speeds[cur_term->Nttyb.sg_ospeed]);
#endif
}


/*
**	savetty()  and  resetty()
**
*/

#ifdef TERMIOS
static struct termios   termbuf;
#else
static struct sgttyb	sgbuf;
#endif

int savetty()
{
#ifdef TRACE
	if (_tracing)
	    _tracef("savetty() called");
#endif

#ifdef TERMIOS
	tcgetattr(cur_term->Filedes, &termbuf);
#else
	gtty(cur_term->Filedes, &sgbuf);
#endif
}

int resetty()
{
#ifdef TRACE
	if (_tracing)
	    _tracef("resetty() called");
#endif

#ifdef TERMIOS
	tcsetattr(cur_term->Filedes, TCSANOW, &termbuf);
#else
        stty(cur_term->Filedes, &sgbuf);
#endif
}
