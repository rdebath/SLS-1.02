
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
 *	curses.priv.h
 *
 *	Header file for curses library objects which are private to
 *	the library.
 *
 */

#include "curses.h"

#define min(a,b)	((a) > (b)  ?  (b)  :  (a))
#define max(a,b)	((a) < (b)  ?  (b)  :  (a))

#define CHANGED     -1

WINDOW	*newscr;

extern int  _tracing;
extern int outc(char);

struct try
{
        struct try      *child;     /* ptr to child.  NULL if none          */
        struct try      *sibling;   /* ptr to sibling.  NULL if none        */
        char            ch;         /* character at this node               */
        short           value;      /* code of string so far.  NULL if none */
};
  
/*
 * Structure for soft labels.
 */
  
typedef struct {
	char dirty;			/* all labels have changed */
	char hidden;			/* soft lables are hidden */
	WINDOW *win;
 	struct slk_ent {
 	    char text[9];		/* text for the label */
 	    char form_text[9];		/* formatted text (left/center/...) */
 	    int x;			/* x coordinate of this field */
 	    char dirty;			/* this label has changed */
 	    char visible;		/* field is visible */
	} ent[8];
} SLK;

struct screen
{
   	FILE		*_ifp;	    /* input file ptr for this terminal     */
   	FILE		*_ofp;	    /* output file ptr for this terminal    */
	struct term	*_term;	    /* used by terminfo stuff               */
	WINDOW		*_curscr;   /* windows specific to a given terminal */
	WINDOW		*_newscr;
	struct try  	*_keytry;   /* "Try" for use with keypad mode       */
	char        	_backbuf[10]; /* Buffer for pushed back characters  */
	int         	_backcnt;   /* How many characters in _backbuf?     */
	int         	_cursrow;   /* Row and column of physical cursor    */
	int         	_curscol;
	bool		_nl;	    /* True if terminal has CRMOD bit on    */
	bool		_raw;	    /* True if in raw mode                  */
	bool		_cbreak;    /* True if in cbreak mode               */
	bool		_echo;	    /* True if echo on                      */
	bool		_nlmapping; /* True if terminal is really doing     */
				    /* NL mapping (fn of raw and nl)	    */
 	SLK		*_slk;	    /* ptr to soft key struct / NULL	    */
};

struct screen	*SP;

int _slk_format;		/* format specified in slk_init() */
void (*_slk_initialize)(void);	/* ptr to soft key init function */
				/* We use a pointer to avoid linking
				   of the soft label routines, when
				   no soft labels are used */
  

#define MAXCOLUMNS    135
#define MAXLINES      66
#define UNINITIALISED ((struct try * ) -1)
