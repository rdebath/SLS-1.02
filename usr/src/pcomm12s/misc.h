/*
 * Definitions to support the home-grown curses(3) functions and to make
 * the old curses(3) routines happy.  ("config.h" must be included first).
 */

#define mvwattrstr(w,y,x,a,s)	(wmove(w,y,x)==ERR?ERR:wattrstr(w,a,s))
#define mvwattrch(w,y,x,a,c)	(wmove(w,y,x)==ERR?ERR:wattrch(w,a,c))
#define mvwattrnum(w,y,x,a,n)	(wmove(w,y,x)==ERR?ERR:wattrnum(w,a,n))
#define mvattrstr(y,x,a,s)	(wmove(stdscr,y,x)==ERR?ERR:wattrstr(stdscr,a,s))
#define mvattrch(y,x,a,c)	(wmove(stdscr,y,x)==ERR?ERR:wattrch(stdscr,a,c))
#define mvattrnum(y,x,a,n)	(wmove(stdscr,y,x)==ERR?ERR:wattrnum(stdscr,a,n))
#define attrstr(a,s)		wattrstr(stdscr,a,s)
#define attrch(a,c)		wattrch(stdscr,a,c)
#define attrnum(a,n)		wattrnum(stdscr,a,n)

#ifdef OLDCURSES
#ifdef NOPROMOTE
#define A_BOLD		0
#define A_BLINK		0
#define A_REVERSE	1
#define A_DIM		0
#define A_STANDOUT	1
#define A_UNDERLINE	0
#else /* NOPROMOTE */
#define A_BOLD		1
#define A_BLINK		1
#define A_REVERSE	1
#define A_DIM		1
#define A_STANDOUT	1
#define A_UNDERLINE	1
#endif /* NOPROMOTE */
#endif /* OLDCURSES */

#ifdef OLDCURSES
typedef char chtype;
#endif /* OLDCURSES */

#ifdef ACS_HLINE
#define VERT		(chtype)0
#define HORZ		(chtype)0
#else /* ACS_HLINE */
#define VERT		(chtype)'|'
#define HORZ		(chtype)'-'
#define ACS_VLINE	(chtype)'|'
#define ACS_HLINE	(chtype)'-'
#endif /* ACS_HLINE */

/*
 * Other miscellaneous stuff
 */

#define BEL		7
#define BS		8
#define ESC		27
#define DEL		127

#define MANUAL_CLEAR	0
#define AUTO_CLEAR	1

#define DENIED		0
#define WRITE_OK	1
#define OK_BUT_EXISTS	2
