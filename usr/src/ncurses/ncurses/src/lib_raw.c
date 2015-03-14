
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
 *	raw.c
 *
 *	Routines:
 *		raw()
 *		echo()
 *		nl()
 *		cbreak()
 *		crmode()
 *		noraw()
 *		noecho()
 *		nonl()
 *		nocbreak()
 *		nocrmode()
 *
 *	cbreak() == crmode()
 *	nocbreak() == crmode()
 *
 *
 */

#include "curses.h"
#include "curses.priv.h"
#include "terminfo.h"

int raw()
{
#ifdef TRACE
	if (_tracing)
	    _tracef("raw() called");
#endif

#ifdef TERMIOS
	cur_term->Nttyb.c_lflag &= ~(ICANON|ISIG);
	cur_term->Nttyb.c_iflag &= ~(INPCK|ISTRIP|IXON);
	cur_term->Nttyb.c_oflag &= ~(OPOST);
	cur_term->Nttyb.c_cc[VMIN] = 1;
	cur_term->Nttyb.c_cc[VTIME] = 0;
	SP->_raw = TRUE;
	SP->_nlmapping = TRUE;
	tcsetattr(cur_term->Filedes, TCSANOW, &cur_term->Nttyb);
#else
    cur_term->Nttyb.sg_flags |= RAW;
    stty(cur_term->Filedes, &cur_term->Nttyb);

    SP->_raw = TRUE;
    SP->_nlmapping = TRUE;
#endif

}


int cbreak()
{
#ifdef TRACE
	if (_tracing)
	    _tracef("cbreak() called");
#endif

#ifdef TERMIOS
	cur_term->Nttyb.c_lflag &= ~ICANON; 
	cur_term->Nttyb.c_lflag |= ISIG;
	cur_term->Nttyb.c_cc[VMIN] = 1;
	cur_term->Nttyb.c_cc[VTIME] = 0;
	SP->_cbreak = TRUE;
	tcsetattr(cur_term->Filedes, TCSANOW, &cur_term->Nttyb);
#else
    cur_term->Nttyb.sg_flags |= CBREAK;
    stty(cur_term->Filedes, &cur_term->Nttyb);

    SP->_cbreak = TRUE;
#endif

}

int crmode()
{
#ifdef TRACE
	if (_tracing)
	    _tracef("crmode() called");
#endif

    cbreak();
}


int echo()
{
#ifdef TRACE
	if (_tracing)
	    _tracef("echo() called");
#endif

#ifdef TERMIOS
	cur_term->Nttyb.c_lflag |= ECHO|ECHOCTL|ECHOPRT|ECHOKE;
	SP->_echo = TRUE;
	tcsetattr(cur_term->Filedes, TCSANOW, &cur_term->Nttyb);
#else
    cur_term->Nttyb.sg_flags |= ECHO;
    stty(cur_term->Filedes, &cur_term->Nttyb);

    SP->_echo = TRUE;
#endif

}


int nl()
{
#ifdef TRACE
	if (_tracing)
	    _tracef("nl() called");
#endif

#ifdef TERMIOS
	cur_term->Nttyb.c_iflag |= IXON|ICRNL|IXOFF;
	cur_term->Nttyb.c_oflag |= OPOST|ONLCR;
	SP->_nl = TRUE;
	SP->_nlmapping = ! SP->_raw;
	tcsetattr(cur_term->Filedes, TCSANOW, &cur_term->Nttyb);
#else
    cur_term->Nttyb.sg_flags |= CRMOD;
    stty(cur_term->Filedes, &cur_term->Nttyb);

    SP->_nl = TRUE;
    SP->_nlmapping = ! SP->_raw;
#endif

}


int noraw()
{
#ifdef TRACE
	if (_tracing)
	    _tracef("noraw() called");
#endif

#ifdef TERMIOS
	cur_term->Nttyb.c_lflag |= ISIG|ICANON;
	cur_term->Nttyb.c_iflag |= IXON;
	cur_term->Nttyb.c_oflag |= OPOST;
	SP->_raw = FALSE;
	SP->_nlmapping = SP->_nl;
	tcsetattr(cur_term->Filedes, TCSANOW, &cur_term->Nttyb);
#else
    cur_term->Nttyb.sg_flags &= ~RAW;
    stty(cur_term->Filedes, &cur_term->Nttyb);

    SP->_raw = FALSE;
    SP->_nlmapping = SP->_nl;
#endif

}


int nocbreak()
{
#ifdef TRACE
	if (_tracing)
	    _tracef("nocbreak() called");
#endif

#ifdef TERMIOS
	cur_term->Nttyb.c_lflag |= ICANON;
	SP->_cbreak = FALSE;
	tcsetattr(cur_term->Filedes, TCSANOW, &cur_term->Nttyb);
#else 
    cur_term->Nttyb.sg_flags &= ~CBREAK;
    stty(cur_term->Filedes, &cur_term->Nttyb);

    SP->_cbreak = FALSE;
#endif

}

int nocrmode()
{
#ifdef TRACE
	if (_tracing)
	    _tracef("nocrmode() called");
#endif

    nocbreak();
}


int noecho()
{
#ifdef TRACE
	if (_tracing)
	    _tracef("noecho() called");
#endif

#ifdef TERMIOS
	cur_term->Nttyb.c_lflag &= ~(ECHO|ECHOCTL|ECHOPRT|ECHOKE);
	SP->_echo = FALSE;
	tcsetattr(cur_term->Filedes, TCSANOW, &cur_term->Nttyb);
#else
    cur_term->Nttyb.sg_flags &= ~ECHO;
    stty(cur_term->Filedes, &cur_term->Nttyb);

    SP->_echo = FALSE;
#endif

}


int nonl()
{
#ifdef TRACE
	if (_tracing)
	    _tracef("nonl() called");
#endif

#ifdef TERMIOS
	cur_term->Nttyb.c_iflag &= ~ICRNL;
	cur_term->Nttyb.c_oflag &= ~ONLCR;
	SP->_nl = SP->_nlmapping = FALSE;
	tcsetattr(cur_term->Filedes, TCSANOW, &cur_term->Nttyb);
#else
    cur_term->Nttyb.sg_flags &= ~CRMOD;
    stty(cur_term->Filedes, &cur_term->Nttyb);

    SP->_nl = FALSE;
    SP->_nlmapping = FALSE;
#endif

}

