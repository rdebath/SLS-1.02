/*
** $Id: main.h, v 2.1 93/04/01 18:00:00 kris Rel $
**
** stuff for main.c
*/


/* includes & defines
*/

#include "getty.h"
#include "defaults.h"
#include "table.h"

#include <signal.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdlib.h>
#include <termcap.h>
#include <unistd.h>

#ifdef PWD
#include <pwd.h>
#endif /* PWD */

#define USAGE	"\r\n%s:Usage:\t [options] line [speed [term [lined]]]\r\n"

/* stuff for main.c only
*/

#ifdef MAIN

#ifdef	WARNCASE
char	*bad_case[] = {
	"\r\n",
	"If your terminal supports lower case letters, please\r\n",
	"use them.  Login again, using lower case if possible.\r\n",
	(char *) NULL
};
#endif /* WARNCASE */


/* speedtab table for modem speeds
*/

struct speedtab {
	ushort	cbaud;		/* baud rate */
	int	nspeed;		/* speed in numeric format */
	char	*speed;		/* speed in display format */
} speedtab[] = {
        { B50,    50,    "50"    },
        { B75,    75,    "75"    },
        { B110,   110,   "110"   },
        { B134,   134,   "134"   },
        { B150,   150,   "150"   },
        { B200,   200,   "200"   },
        { B300,   300,   "300"   },
        { B600,   600,   "600"   },
        { B1200,  1200,  "1200"  },
        { B1800,  1800,  "1800"  },
        { B2400,  2400,  "2400"  },
        { B4800,  4800,  "4800"  },
        { B9600,  9600,  "9600"  },
#ifdef  B19200
        { B19200, 19200, "19200" },
#endif  /* B19200 */
#ifdef  B38400
        { B38400, 38400, "38400" },
#endif  /* B38400 */
        { EXTA,   0,     "EXTA"  },
        { EXTB,   0,     "EXTB"  },
	{ 0,      0,     ""      }
};

#endif /* MAIN */


extern char	*nextword();
extern int	expect();

#undef EXTERN
#ifdef MAIN
#define EXTERN
#else
#define EXTERN extern
#endif

EXTERN char		term[16];	/* terminal type */
EXTERN char		*speed;		/* terminal speed */
EXTERN char		*defname;	/* defaults file name */
EXTERN boolean		clear;		/* TRUE, clear screen */
EXTERN char		*clrscr;	/* string to clear screen with */
EXTERN char		*login;		/* login program */
EXTERN char		*init;		/* init string */
EXTERN char		*waitfor;	/* waitfor string */
EXTERN char		*connect;	/* connect string */
EXTERN boolean		waitchar;	/* TRUE, wait for a character */
EXTERN unsigned int	delay;		/* seconds to delay before the prompt */
EXTERN GTAB		*gtab;		/* terminal mode */
EXTERN char		devname[MAXLINE + 1];	
					/* name of tty device */
EXTERN char		initdevname[MAXLINE + 1];	
					/* name of init device */

#ifdef ISSUE
EXTERN char		*issue;		/* login banner file */
#endif /* ISSUE */

#ifdef FIDO
EXTERN char		*fido;		/* fido program */
#endif /* FIDO */

#ifdef SCHED
EXTERN boolean		allow;		/* TRUE, logins permitted */
EXTERN unsigned	alrm;		/* time to die */
#endif /* SCHED */

#ifdef RBGETTY
EXTERN time_t		minrbtime;	/* min time between calls */
EXTERN unsigned		maxrbtime;	/* max time between calls */
EXTERN time_t		interring;	/* time between rings */
EXTERN int		minrings;	/* min rings for first call */
EXTERN int		maxrings;	/* max rings for first call */
EXTERN boolean		rbmode;		/* TRUE, rb is enabled */
#endif /* RBGETTY */
