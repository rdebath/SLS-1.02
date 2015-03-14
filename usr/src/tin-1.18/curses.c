/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : curses.c
 *  Author    : D.Taylor & I.Lea
 *  Created   : 01-01-86
 *  Updated   : 11-10-92
 *  Notes     : This is a screen management library borrowed with permission
 *              from the Elm mail system (a great mailer--I highly recommend
 *              it!).This library was hacked to provide what tin needs.
 *  Copyright : Copyright (c) 1986-92 Dave Taylor & Iain Lea
 *              The Elm Mail System  -  $Revision: 2.1 $   $State: Exp $
 */

#ifndef ns32000
#	undef	sinix
#endif

#include <stdio.h>
#ifndef	AMIGA
#	include <curses.h>
#	include <sys/errno.h>
#else
#	include <errno.h>
#	define	TRUE 	1
#	define	FALSE 	0
#endif

#define DEFAULT_LINES_ON_TERMINAL	24
#define DEFAULT_COLUMNS_ON_TERMINAL	80

int LINES = 23;
int COLS  = 80;
int inverse_okay = TRUE;
static int _inraw = FALSE;	/* are we IN rawmode?    */
int _hp_glitch = FALSE;		/* standout not erased by overwriting on HP terms */

#ifndef INDEX_DAEMON

#define		BACKSPACE	'\b'
#define		VERY_LONG_STRING	2500

#if defined(AMIGA) || defined(BSD)
#	ifndef BSD4_1
#		include <sgtty.h>
#	else
#		include <termio.h>
#	endif
#else
#	ifndef SYSV
#		ifndef MINIX
#			ifdef sinix
#				include <termios.h>
#			else
#				include <termio.h>
#			endif
#		else
#			include <sgtty.h>
#		endif
#	else
#		ifdef __hpux
#			include <termio.h>
#		endif
#	endif
#endif

#define TTYIN	0

#ifdef SHORTNAMES
# define _clearinverse	_clrinv
# define _cleartoeoln	_clrtoeoln
# define _cleartoeos	_clr2eos
#endif

#if defined(AMIGA) || defined(BSD) || defined(MINIX)
#	ifdef TCGETA
#		undef TCGETA
#	endif
#	define TCGETA	TIOCGETP
#	ifdef TCSETAW
#		undef TCSETAW
#	endif
#	define TCSETAW	TIOCSETP
struct sgttyb _raw_tty,
	      _original_tty;
#else
#	ifdef sinix
#		ifndef TCGETA
#			define TCGETA	STCGETA
#		endif
#		ifndef TCSETA
#			define TCSETAW	STCSETAW
#		endif
struct termios _raw_tty, 
              _original_tty;
#	else
struct termio _raw_tty, 
              _original_tty;
#	endif
#endif

static char *_clearscreen, *_moveto, *_cleartoeoln, *_cleartoeos,
			*_setinverse, *_clearinverse, *_setunderline, *_clearunderline,
			*_terminalinit, *_terminalend, *_keypadlocal, *_keypadxmit;

static int _lines,_columns;

#ifndef AMIGA
static char _terminal[1024];              /* Storage for terminal entry */
static char _capabilities[1024];           /* String for cursor motion */

static char *ptr = _capabilities;	/* for buffering         */

extern char	*tgetstr ();		/* Get termcap capability (string) */
extern char	*tgoto ();		/* and the goto stuff    */
extern int	tgetflag ();		/* Get termcap capability (boolean) */
extern int	tgetnum ();		/* Get termcap capability (number) */
#endif	/* AMIGA */

static int in_inverse;			/* 1 when in inverse, 0 otherwise */
int	outchar ();			/* char output for tputs */

#endif /* INDEX_DAEMON */

#include "tin.h"


void setup_screen ()
{
	/*
	 * get screen size from termcap entry & setup sizes
	 */
	ScreenSize (&LINES, &COLS);
	cmd_line = FALSE;
	Raw (TRUE);
	set_win_size (&LINES, &COLS);
}


#ifndef AMIGA

int InitScreen ()
{
#ifndef INDEX_DAEMON

	extern int tgetent();      /* get termcap entry */
	char termname[40], *p;
	
	if ((p = (char *) getenv ("TERM")) == NULL) {
		fprintf (stderr, "%s: TERM variable must be set to use screen capabilities\n", progname);
		return (FALSE);
	}
	if (strcpy (termname, p) == NULL) {
		fprintf (stderr,"%s: Can't get TERM variable\n", progname);
		return (FALSE);
	}
	if (tgetent (_terminal, termname) != 1) {
		fprintf (stderr,"%s: Can't get entry for TERM\n", progname);
		return (FALSE);
	}

	/* load in all those pesky values */
	_clearscreen    = tgetstr ("cl", &ptr);
	_moveto         = tgetstr ("cm", &ptr);
	_cleartoeoln    = tgetstr ("ce", &ptr);
	_cleartoeos     = tgetstr ("cd", &ptr);
	_lines          = tgetnum ("li");
	_columns        = tgetnum ("co");
	_setinverse     = tgetstr ("so", &ptr);
	_clearinverse   = tgetstr ("se", &ptr);
	_setunderline   = tgetstr ("us", &ptr);
	_clearunderline = tgetstr ("ue", &ptr);
	_terminalinit   = tgetstr ("ti", &ptr);
	_terminalend    = tgetstr ("te", &ptr);
	_keypadlocal    = tgetstr ("ke", &ptr);
	_keypadxmit     = tgetstr ("ks", &ptr);

	_hp_glitch = tgetflag ("xs");

	InitWin ();

	if (!_clearscreen) {
		fprintf (stderr,
			"%s: Terminal must have clearscreen (cl) capability\n",progname);
		return (FALSE);
	}
	if (!_moveto) {
		fprintf (stderr,
			"%s: Terminal must have cursor motion (cm)\n", progname);
		return (FALSE);
	}
	if (!_cleartoeoln) {
		fprintf (stderr,
			"%s: Terminal must have clear to end-of-line (ce)\n", progname);
		return (FALSE);
	}
	if (!_cleartoeos) {
		fprintf (stderr,
			"%s: Terminal must have clear to end-of-screen (cd)\n", progname);
		return (FALSE);
	}
	if (_lines == -1)
		_lines = DEFAULT_LINES_ON_TERMINAL;
	if (_columns == -1)
		_columns = DEFAULT_COLUMNS_ON_TERMINAL;
	/* 
	 * kludge to workaround no inverse 
	 */
	if (_setinverse == 0) {
		_setinverse = _setunderline;
		_clearinverse = _clearunderline;
		if (_setinverse == 0)
			draw_arrow_mark = 1;
	}
	return (TRUE);

#else

	return (FALSE);

#endif /* INDEX_DAEMON */
}

#else	/* AMIGA InitScreen() */

int InitScreen ()
{
#ifndef			INDEX_DAEMON

	char c,*ptr,buf[32];

	/* 
	 * we're going to assume a terminal here... 
	 */

	_clearscreen	= "\033[1;1H\033[J";
	_moveto		= "\033[%d;%dH";	/* not a termcap string! */
	_cleartoeoln	= "\033[K";
	_cleartoeos	= "\033[J";

	_setinverse	= "\033[7m";
	_clearinverse	= "\033[0m";
	_setunderline	= "\033[4m";
	_clearunderline	= "\033[0m";
	_terminalinit	= "\033c";
	_terminalend	= "\033c";
	_keypadlocal	= "";
	_keypadxmit	= "";

	InitWin ();

	_lines = _columns = -1;

	/* 
	 * Get lines and columns from environment settings - useful when
         * you're using something other than an Amiga window 
         */
         
	if (ptr = getenv("LINES")) {
		_lines = atol(ptr);
	}
	if (ptr = getenv("COLUMNS")) {
		_columns = atol(ptr);
	}

	/* 
	 * If that failed, try get a response from the console itself 
	 */

	if (_lines == -1 || _columns == -1) {
		Raw (TRUE);

		tputs ("\2330 q",1,outchar);	/* identify yourself */
		fflush (stdout);

getsize:
		while (ReadCh () != 0x9b) {
			;	/* Look for escape */
		}
		/* get top */
		ptr = buf;
		do {	
			c = *ptr++ = ReadCh ();
		} while (isdigit(c));

		if (c != ';') { 
			goto getsize;
		}
		
		/* get right */
		ptr = buf;
		do {	
			c = *ptr++ = ReadCh ();
		} while (isdigit(c));

		if (c != ';') { 
			goto getsize;
		}
		
		/* get bottom */
		ptr = buf;
		do {	
			c = *ptr++ = ReadCh ();
		} while (isdigit(c));

		if (c != ';') {
			goto getsize;
		}

		*ptr = 0;
		_lines = atol (buf);

		/* get right */
		ptr = buf;
		do {	
			c = *ptr++ = ReadCh ();
		} while (isdigit(c));

		if (c != ' ') {
			goto getsize;
		}
		if (ReadCh () != 'r') { 
			goto getsize;
		}

		*ptr = 0;
		_columns = atol (buf);
	}

	Raw (FALSE);

	return (TRUE);
#else
	return (FALSE);

#endif /* INDEX_DAEMON */
}

#endif	/* AMIGA */

/*
 *  returns the number of lines and columns on the display.
 */
 
void ScreenSize (num_lines, num_columns)
	int *num_lines, *num_columns;
{
#ifndef INDEX_DAEMON

	if (_lines == 0) _lines = DEFAULT_LINES_ON_TERMINAL;
	if (_columns == 0) _columns = DEFAULT_COLUMNS_ON_TERMINAL;

	*num_lines = _lines - 1;		/* assume index from zero*/
	*num_columns = _columns;		/* assume index from one */

#endif /* INDEX_DAEMON */
}

void InitWin ()
{
#ifndef INDEX_DAEMON

	if (_terminalinit) {
		tputs (_terminalinit, 1, outchar);
		fflush (stdout);
	}	
	set_keypad_on ();

#endif /* INDEX_DAEMON */
}

void EndWin ()
{
#ifndef INDEX_DAEMON

	if (_terminalend) {
		tputs (_terminalend, 1, outchar);
		fflush (stdout);
	}
	set_keypad_off ();
	
	
#endif /* INDEX_DAEMON */
}

void set_keypad_on ()
{
#ifndef INDEX_DAEMON
#    ifdef HAVE_KEYPAD
 	if (use_keypad && _keypadxmit) {
		tputs (_keypadxmit, 1, outchar);
		fflush (stdout);
	}
#    endif
#endif /* INDEX_DAEMON */
}

void set_keypad_off ()
{
#ifndef INDEX_DAEMON
#    ifdef HAVE_KEYPAD
	if (use_keypad && _keypadlocal) {
		tputs (_keypadlocal, 1, outchar);
		fflush (stdout);
	}
#    endif
#endif /* INDEX_DAEMON */
}

/*
 *  clear the screen: returns -1 if not capable
 */

void ClearScreen ()
{
#ifndef INDEX_DAEMON

	tputs (_clearscreen, 1, outchar);
	fflush (stdout);      /* clear the output buffer */

#endif /* INDEX_DAEMON */
}

/*
 *  move cursor to the specified row column on the screen.
 *  0,0 is the top left!
 */

#ifndef AMIGA

void MoveCursor (row, col)
	int row, col;
{
#ifndef INDEX_DAEMON

	char *stuff, *tgoto();

	stuff = tgoto (_moveto, col, row);
	tputs (stuff, 1, outchar);
	fflush (stdout);

#endif /* INDEX_DAEMON */
}

#else	/* AMIGA MoveCursor() */

void MoveCursor (row, col)
	int row, col;
{
#ifndef INDEX_DAEMON

	char stuff[12], *tgoto();

	sprintf (stuff,_moveto, row+1, col+1);
	tputs (stuff, 1, outchar);
	fflush (stdout);

#endif /* INDEX_DAEMON */
}

#endif	/* AMIGA */

/*
 *  clear to end of line
 */

void CleartoEOLN ()
{
#ifndef INDEX_DAEMON

	tputs (_cleartoeoln, 1, outchar);
	fflush (stdout);  /* clear the output buffer */

#endif /* INDEX_DAEMON */
}

/*
 *  clear to end of screen
 */

void CleartoEOS ()
{
#ifndef INDEX_DAEMON

	int i;
	
	if (_cleartoeos) {
		tputs (_cleartoeos, 1, outchar);
	} else {
		for (i=_lines ; i < _lines ; i++) {
			MoveCursor (i, 0);
			CleartoEOLN ();
		}
	}
	fflush (stdout);  /* clear the output buffer */

#endif /* INDEX_DAEMON */
}

/*
 *  set inverse video mode
 */

void StartInverse ()
{
#ifndef INDEX_DAEMON

	in_inverse = 1;
	if (_setinverse && inverse_okay)
		tputs (_setinverse, 1, outchar);
	fflush (stdout);

#endif /* INDEX_DAEMON */
}

/*
 *  compliment of startinverse
 */

void EndInverse ()
{
#ifndef INDEX_DAEMON

	in_inverse = 0;
	if (_clearinverse && inverse_okay)
		tputs (_clearinverse, 1, outchar);
	fflush (stdout);

#endif /* INDEX_DAEMON */
}

/*
 *  toggle inverse video mode
 */

void ToggleInverse ()
{
#ifndef INDEX_DAEMON

	if (in_inverse == 0)
		StartInverse();
	else
		EndInverse();

#endif /* INDEX_DAEMON */
}

/*
 *  returns either 1 or 0, for ON or OFF
 */

int RawState()
{
	return (_inraw);
}

/*
 *  state is either TRUE or FALSE, as indicated by call
 */

void Raw(state)
	int state;
{
#ifndef INDEX_DAEMON

	if (state == FALSE && _inraw) {
	  (void) ioctl(TTYIN, TCSETAW, &_original_tty);
	  _inraw = 0;
	} else if (state == TRUE && ! _inraw) {
	  (void) ioctl(TTYIN, TCGETA, &_original_tty);	/** current setting **/
	  (void) ioctl(TTYIN, TCGETA, &_raw_tty);    /** again! **/

#if defined(AMIGA) || defined(BSD) || defined(MINIX)
	  _raw_tty.sg_flags &= ~(ECHO | CRMOD);	/* echo off */
	  _raw_tty.sg_flags |= CBREAK;		/* raw on */
#ifdef AMIGA	
	  _raw_tty.sg_flags |= RAW; /* Manx-C 5.2 does not support CBREAK */
#endif
#else
	  _raw_tty.c_lflag &= ~(ICANON | ECHO);	/* noecho raw mode        */

	  _raw_tty.c_cc[VMIN] = '\01';	/* minimum # of chars to queue    */
	  _raw_tty.c_cc[VTIME] = '\0';	/* minimum time to wait for input */
#endif

	  (void) ioctl(TTYIN, TCSETAW, &_raw_tty);
	  _inraw = 1;
	}
	
#endif /* INDEX_DAEMON */
}

/*
 *  read a character with Raw mode set!
 */

int ReadCh()
{
#ifndef INDEX_DAEMON
	extern int errno;
	char ch;
	register int result = 0;
	
#ifdef READ_CHAR_HACK
#undef getc
	while ((result = getc(stdin)) == EOF) {
		if (feof(stdin))
			break;

#ifdef EINTR
		if (ferror(stdin) && errno != EINTR)
#else
		if (ferror(stdin))
#endif
			break;

		clearerr(stdin);
	}

	return ((result == EOF) ? EOF : result & 0xFF);
#else
#  ifdef EINTR
	while ((result = read (0, &ch, 1)) < 0 && errno == EINTR)
		;	/* spin on signal interrupts */
#  else
	result = read (0, &ch, 1);
#  endif
        return((result <= 0 ) ? EOF : ch & 0xFF);
#endif		

#endif /* INDEX_DAEMON */
}

/*
 *  output a character. From tputs... (Note: this CANNOT be a macro!)
 */

int outchar (c)
	int c;
{
	fputc (c, stdout);
}
