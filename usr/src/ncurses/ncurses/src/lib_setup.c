
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
 *	setupterm(termname, filedes, errret)
 *
 *	Find and read the appropriate object file for the terminal
 *	Make cur_term point to the structure.
 *	Turn off the XTABS bit in the tty structure if it was on
 *	If XTABS was on, remove the tab and backtab capabilities.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "curses.h"
#include "curses.priv.h"
#include "terminfo.h"

#define ret_error(code, fmt, arg)	if (errret) {\
					    *errret = code;\
					    return(code);\
					} else {\
					    fprintf(stderr, fmt, arg);\
					    exit(1);\
					}

#define ret_error0(code, msg)		if (errret) {\
					    *errret = code;\
					    return(code);\
					} else {\
					    fprintf(stderr, msg);\
					    exit(1);\
					}


static void do_prototype(void);

int _use_env = TRUE;

void use_env(bool f)
{
	_use_env = f;
}

static int resize(int fd)
{
struct winsize size;

	if (ioctl(fd, TIOCGWINSZ, &size) < 0)
		return 1;
	LINES = size.ws_row;
	COLS = size.ws_col;
	return 0;
}

char ttytype[NAMESIZE];

int setupterm(char *termname, int filedes, int *errret)
{
static int	_been_here = FALSE;
char		filename1[1024];
char		filename2[1024];
char		*directory = SRCDIR;
char		*terminfo;
struct term	*term_ptr;
char 		*rows, *cols;

	if (_been_here == FALSE) {
	_been_here = TRUE;
#ifdef TRACE
	_init_trace();
	if (_tracing)
	    _tracef("setupterm(%s,%d,%x) called", termname, filedes, errret);
#endif

	if (termname == NULL)
	{
	    termname = getenv("TERM");
	    if (termname == NULL)
		ret_error0(-1, "TERM environment variable not set.\n");
	}

        term_ptr = (struct term *) malloc(sizeof(struct term));

	if (term_ptr == NULL)
	    ret_error0(-1, "Not enough memory to create terminal structure.\n") ;

	if ((terminfo = getenv("TERMINFO")) != NULL)
	    directory = terminfo;

	sprintf(filename1, "%s/%c/%s", directory, termname[0], termname);
	sprintf(filename2, "%s/%c/%s", SRCDIR, termname[0], termname);

	if (read_entry(filename1, term_ptr) < 0 &&  read_entry(filename2, term_ptr) < 0)
	    ret_error(0, "'%s': Unknown terminal type.\n", termname);

	if (command_character  &&  getenv("CC"))
	    do_prototype();

	cur_term = term_ptr;
	strncpy(ttytype, cur_term->term_names, NAMESIZE - 1);
	ttytype[NAMESIZE - 1] = '\0';
	cur_term->Filedes = filedes;

	/* figure out the size of the screen */

	rows = getenv("LINES");
	if (rows != (char *)NULL)
		LINES = atoi(rows);
 
	cols = getenv("COLUMNS");
	if (cols != (char *)NULL)
		COLS = atoi(cols);

	if (_use_env == FALSE) 
		if (lines > 0 && columns > 0) {
			LINES = lines;
			COLS  = columns; 
		} 
	/* use envirronment; if not set then use window size */
#ifdef TRACE
	_tracef("using environment to find screen size");
#endif
	if (LINES <= 0 || COLS <= 0) {
		if (resize(filedes) == 1) {
			fprintf(stderr, "can't find the screen size");
			exit(1);
		} 
	}
	lines = LINES;
	columns = COLS;
			
#ifdef TRACE
	_tracef("screen size is %dx%d and %dx%d", COLS, LINES, columns, lines);
#endif

#ifdef TERMIOS
	tcgetattr(filedes, &cur_term->Ottyb);
	if (cur_term->Ottyb.c_oflag & XTABS)
		tab = back_tab = NULL;

	cur_term->Nttyb = cur_term->Ottyb;
	cur_term->Nttyb.c_oflag &= ~XTABS;
#else
	gtty(filedes, &cur_term->Ottyb);
	if (cur_term->Ottyb.sg_flags & XTABS)
	    tab = back_tab = NULL;

	cur_term->Nttyb = cur_term->Ottyb;
	cur_term->Nttyb.sg_flags &= ~XTABS;
#endif

	def_prog_mode(); 

	if (errret)
	    *errret = 1;
	}	
	return(1);
}


/*
**	do_prototype()
**
**	Take the real command character out of the CC environment variable
**	and substitute it in for the prototype given in 'command_character'.
**
*/

static void
do_prototype()
{
    	int	i, j;
	char	CC;
	char	proto;
	char    *tmp;

	tmp = getenv("CC");
	CC = *tmp;
	proto = *command_character;

	for (i=0; i < STRCOUNT; i++)
	{
	    j = 0;
	    while (cur_term->Strings[i][j])
	    {
		if (cur_term->Strings[i][j] == proto)
		    cur_term->Strings[i][j] = CC;
		j++;
	    }
	}
}
