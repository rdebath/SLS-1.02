/*	xc.h -- header  file for XC
	This file uses 4-character tabstops
*/

/* Local preferences and modems */

/*	ESCAPE_CHR
	This is the character used as the "escape" character in terminal mode.
	The default is ASCII 01 (control-A). To change this, redefine ESCAPE_CHR
	and also ESC_STR.

	ESC_STR
	This is the string used to describe the "escape" character within the
	XC on-line help text. For example, if you want to change the "escape"
	character to ESCAPE (ASCII 27), you'd redefine ESCAPE_CHR as 27 and ESC_STR
	as "ESC".

	We deliberately did not choose the ESCAPE key itself because it used so
	often by other programs: if you were logged into a remote system and
	running, say, /bin/vi, it would be a great annoyance to have to hit ESCAPE
	twice to get it transmitted once.)
*/

#define MY_ESC	01			/* control-A */
#define ESC_STR	"Ctrl-A"	/* How MY_ESC is shown in on-line help */

/*	CAPTFILE
	This is the default name given to the capture buffer in terminal mode (and
	during script processing). You can always reset this while running the
	program.
*/
#define	CAPTFILE "capture.log"	/* Default capture file */

/*	LIBDIR
	This is the default name given to a directory where PHFILE, STARTUP, and
	any XC scripts might be found. XC will search for such files
		1st) in a path in a colon-separated list of directories in
			 XC_PATH, if such an environment variable exists,
		2nd) in the current directory,
		3rd) in your HOME directory, if HOME is an environment variable,
		4th) in LIBDIR.
*/
#define LIBDIR "/usr/uucp"	/* System wide storage of dot-files */

/*	PHFILE
	This is the default name given to the dialing directory.
*/
#define PHFILE "phonelist"		/* Default phonelist file */

/*	STARTUP
	This is the default name of the startup script for XC. If this file is
	found it will be executed immediately on XC startup.
*/
#define STARTUP "xc.init"			/* XC Startup Script */

/*	DIALSTRT and DIALSTRP
	A format string to send a telephone number to the modem with the
	appropriate dialing command (tone or pulse line) .
*/
#define DIALSTRT "ATDT"		/* printf format for tone dial command */
#define DIALSTRP "ATDP"     /* prinft format for pulse dial command */

/* DTR_DROPS_CARRIER
	On some (most?) modems, dropping the DTR signal will instruct the modem
	to disconnect the phone line. On most (some?) Unix systems, setting a
	bit/second rate of 0 will drop carrier.

#define DTR_DROPS_CARRIER   1

/* ATH_HANGS_UP 
    This alternate function sends the modem the ATTEN string, waits a few
    seconds, and then sends the HANGUP string, so check if those are correct
    for your modem.
*/

#define ATH_HANGS_UP       1
# define ATTEN "+++"			/* Modem "attention" signal */
# define HANGUP "ATH\r"			/* Modem "hang up" command */


/* Local Unix peculiarities */

/*	strchr and strrchr vs. index and rindex
	Some Berkely and Xenix systems have index() and rindex() which are
	functionally identical to the more standard strchr() and strrchr()
	functions. Include these defines if your Unix supports index and
	rindex INSTEAD of strchr and strrchr.
*/
/* #define strchr	index	/**/
/* #define strrchr	rindex	/**/

/*	DUP2
	dup2() is not included with all versions of Unix. If your implementation
	does not have dup2() (or if you are just not sure), define this as 0 and a
	functional equivalent will be included in the source code.
*/
#define DUP2	0			/* 0 if dup2() not available */

/*	STRSTR
	The draft Ansi C standard specifies the "strstr" function to return the
	position of a substring within a string. This is NOT included in many Unix
	systems, so code for this function is included unless you define this as 1.
*/
#define	STRSTR   1  		/* 0 if strstr() not available */

/*	MEMSET
	This function is not on earlier implementations of Unix. Define this as 0
	if you don't have it; making it 1 will not include our equivalent code.
*/
#define MEMSET	1			/* 0 if memset() not available */

/*	STRDUP
	Again, a function not present on earlier versions of Unix, and again,
	defining this as 0 will thereby include our functional equivalent.
*/
#define STRDUP  0  			/* 0 if strdup() not available */


/* The reader is kindly invited to leave the rest of this just as it is! */

#define SOH		0x1		/* ^A */
#define STX     0x2     /* ^B */
#define ETX		0x3		/* ^C */
#define EOT		0x4		/* ^D */
#define ENQ		0x5		/* ^E */
#define ACK		0x6		/* ^F */
#define DLE		0x10	/* ^P */
#define XON		0x11	/* ^Q */
#define XOFF	0x13	/* ^S */
#define NAK	 	0x15	/* ^U */
#define CAN		0x18	/* ^X */

#ifndef TRUE
# define TRUE	1
# define FALSE	0
# define SUCCESS	1
# define FAILURE	0
#endif

#define SM_BUFF	 256
#define LG_BUFF  2048 
#define S	show(1,Msg)
#define S2	show(2,Msg)
#define	NULLS	((char*)0)
#define	NULLF	((FILE*)0)

#define purge()  while(readbyte(1)!=-1)   /* macro for purge function  tgm */

#ifdef DEBUG
# define fprintf Fprintf
# define fputc Fputc
#endif
#include <ctype.h>
/* globals in three or more files */

extern int	CO, LI, bitmask, beep(), hangup(), mrate(), cbaud;
extern short	capture, cismode, flowflag, linkflag, reterm;
extern char captfile[], ddsname[], phonefile[], *mport(), word[], *wptr,
			line[], Msg[], *lptr, Name[], *getenv(), *strstr(), *strcat(),
			*strncat(), *strcpy(),*strncpy(), *strchr(), *strrchr(), *strdup();
extern unsigned sleep();
extern unsigned short getuid(), getgid(), geteuid(), getegid();
extern void	cls(), cur_on(), cur_off(), do_script(), drawline(), exit(),
			free(), getline(), getword(), intdel(), lc_word(), mescs(),
			send_mbyte(), send_slowly(), sendbyte(), show(),
			show_abort(), ttgoto(), xc_setflow();
extern FILE *tfp,			/* the local terminal */
			*openfile(), *QueryCreate();
extern struct termio newmode, sigmode;

