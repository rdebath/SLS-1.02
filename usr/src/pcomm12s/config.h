/*
 * Various tunable parameters.  This should appear before any other local
 * header file.
 */

#ifdef linux
#define TIOCGETP TCGETS
#define TIOCSETP TCSETS
#define sgttyb termios
#define SIGCLD SIGCHLD
/* #define ttyname ctermid */
#endif /* linux */

/* Are you using a Berkeley flavor of Unix? */
#undef	BSD

/* Use the dialing routines specific to the AT&T Unix PC 7300/3b1 */
#undef	UNIXPC

/* Older versions of curses(3) use termcap in lieu of terminfo */
#define	OLDCURSES

/* Use shared memory in lieu of a file for the virtual screen */
#undef	SHAREDMEM

/* Should a missing video attribute be promoted to standout? */
#define NOPROMOTE

/* Use extra precautions if Pcomm is set-user-id or set-group-id */
#undef	SETUGID

/* Should Pcomm make a log of all phone calls? */
#undef	LOG_CALLS

/* The name of the log file (if used).  */
#undef	LOGFILE		"/usr/adm/phone.calls"

/* Should long distance (toll) calls be limited to a specific group? */
#undef	LIMIT_LD

/* The name of the privileged group for limiting long distance calls */
#define	GROUP_NAME	"uucp"

/* The path to the line printer program */
#define	LPR		"lp"

/* The path to the "pretty" printer program (if none, use "pr | lp") */
#define	LPRINT		"pr | lp"

/* The path to the default directory containing the Pcomm support files */
#ifndef DEFAULT_DIR
#define	DEFAULT_DIR	"/usr/local/lib/pcomm"
#endif

/* The path to the directory where UUCP locks are found */
#define	LOCK_DIR	"/usr/spool/uucp"

/* Do the lock files use ASCII encoded PID's? */
#undef	ASCII_PID

/* Fold the last character of the lock to lower case? */
#undef	XENIX_LOCKS

/* Should Pcomm optimize redialing by keeping the TTY port open */
#define	KEEP_PORT

/* Does the status line scroll up on "magic cookie" terminals? */
#undef	XMC_BROKE

/* Does the alarm() system call work correctly with the wgetch() function? */
#undef	WGETCH_BROKE

/* The size of the serial port character buffer */
#define CLIST_SIZ	64

/* The size of the input buffer (should be about the same as CLIST_SIZ) */
#define INPUT_BUF	64

/* The size of the output buffer (should be about one half INPUT_BUF) */
#define OUTPUT_BUF	32

/* Does memmove() exist or is memcpy() well behaved when overlapping? */
#define MEMMOVE	memcpy

/* Does your Unix allow flip-flop between real and effective user IDs? */
#define SETUID_BROKE

typedef void SIG_TYPE;
/* typedef int SIG_TYPE; */

#ifdef BSD
#define strchr index
#define strrchr rindex
#endif /* BSD */
