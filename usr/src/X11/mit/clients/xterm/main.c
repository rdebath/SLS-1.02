/* $Header: /home/x_cvs/mit/clients/xterm/main.c,v 1.5 1992/09/16 15:03:51 dawes Exp $ */
#ifndef lint
static char *rid="$XConsortium: main.c,v 1.200 92/03/11 17:36:12 gildea Exp $";
#endif /* lint */

/*
 * 				 W A R N I N G
 * 
 * If you think you know what all of this code is doing, you are
 * probably very mistaken.  There be serious and nasty dragons here.
 *
 * This client is *not* to be taken as an example of how to write X
 * Toolkit applications.  It is in need of a substantial rewrite,
 * ideally to create a generic tty widget with several different parsing
 * widgets so that you can plug 'em together any way you want.  Don't
 * hold your breath, though....
 */

/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard,
Massachusetts, and the Massachusetts Institute of Technology,
Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/


/* main.c */

#include "ptyx.h"
#include "data.h"
#include "error.h"
#include "menu.h"
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include <X11/Xos.h>
#include <X11/cursorfont.h>
#include <X11/Xaw/SimpleMenu.h>
#include <pwd.h>
#include <ctype.h>

#ifdef linux
#define USE_SYSV_TERMIO
#define	USE_SYSV_PGRP
#define USE_SYSV_UTMP
#define USE_SYSV_SIGNALS
#endif

#ifdef att
#define ATT
#endif

#ifdef SVR4
#define SYSV			/* SVR4 is (approx) superset of SVR3 */
#define USE_SYSV_UTMP
#define ATT
#define USE_TERMIOS
#endif
  
#ifdef SYSV386
#define USE_SYSV_UTMP
#define ATT
#define USE_HANDSHAKE
static Bool IsPts = False;
#endif

#ifdef ATT
#define USE_USG_PTYS
#else
#define USE_HANDSHAKE
#endif

#if defined(SYSV) && !defined(SVR4)
/* older SYSV systems cannot ignore SIGHUP.
   Shell hangs, or you get extra shells, or something like that */
#define USE_SYSV_SIGHUP
#endif

#if defined(sony) && defined(bsd43) && !defined(KANJI)
#define KANJI
#endif
#include <sys/ioctl.h>
#include <sys/stat.h>

#ifdef USE_TERMIOS
#include <termios.h>
/* this hacked termios support only works on SYSV */
#define USE_SYSV_TERMIO
#define termio termios
#undef TCGETA
#define TCGETA TCGETS
#undef TCSETA
#define TCSETA TCSETS
#else /* USE_TERMIOS */
#ifdef SYSV
#include <sys/termio.h>
#endif /* SYSV */
#endif /* USE_TERMIOS else */

#ifdef SVR4
#undef TIOCSLTC				/* defined, but not useable */
#endif

#ifdef SYSV
#ifdef USE_USG_PTYS			/* AT&T SYSV has no ptyio.h */
#include <sys/stream.h>			/* get typedef used in ptem.h */
#include <sys/ptem.h>			/* get struct winsize */
#include <sys/stropts.h>		/* for I_PUSH */
#include <poll.h>			/* for POLLIN */
#endif /* USE_USG_PTYS */
#define USE_SYSV_TERMIO
#define USE_SYSV_SIGNALS
#define	USE_SYSV_PGRP
#define USE_SYSV_ENVVARS		/* COLUMNS/LINES vs. TERMCAP */
/*
 * now get system-specific includes
 */
#ifdef CRAY
#define USE_SYSV_UTMP
#define HAS_UTMP_UT_HOST
#define HAS_BSD_GROUPS
#endif
#ifdef macII
#define HAS_UTMP_UT_HOST
#define HAS_BSD_GROUPS
#include <sys/ttychars.h>
#undef USE_SYSV_ENVVARS
#undef FIOCLEX
#undef FIONCLEX
#define setpgrp2 setpgrp
#include <sgtty.h>
#include <sys/resource.h>
#endif
#ifdef hpux
#define HAS_BSD_GROUPS
#include <sys/ptyio.h>
#endif /* hpux */
#ifdef sgi
#include <sys/sysmacros.h>
#endif /* sgi */
#endif /* SYSV */

#ifndef SYSV				/* BSD systems */
#include <sgtty.h>
#include <sys/resource.h>
#define HAS_UTMP_UT_HOST
#define HAS_BSD_GROUPS
#endif	/* !SYSV */

#ifdef _POSIX_SOURCE
#define USE_POSIX_WAIT
#endif
#ifdef SVR4
#define USE_POSIX_WAIT
#endif

#ifdef __386BSD__
#define LASTLOG
#define WTMP
#endif

#include <stdio.h>
#include <errno.h>
#include <setjmp.h>

#ifdef hpux
#include <sys/utsname.h>
#endif /* hpux */

#ifdef apollo
#define ttyslot() 1
#endif /* apollo */

#include <utmp.h>
#ifdef LASTLOG
#ifndef __386BSD__
#include <lastlog.h>
#endif
#endif
#include <sys/param.h>	/* for NOFILE */

#ifdef  PUCC_PTYD
#include <local/openpty.h>
int	Ptyfd;
#endif /* PUCC_PTYD */

#ifdef sequent
#define USE_GET_PSEUDOTTY
#endif

#ifndef UTMP_FILENAME
#ifdef _PATH_UTMP
#define UTMP_FILENAME _PATH_UTMP
#else
#define UTMP_FILENAME "/etc/utmp"
#endif
#endif
#ifndef LASTLOG_FILENAME
#ifdef _PATH_LASTLOG
#define LASTLOG_FILENAME _PATH_LASTLOG
#else
#define LASTLOG_FILENAME "/usr/adm/lastlog"  /* only on BSD systems */
#endif
#endif
#ifndef WTMP_FILENAME
#ifdef _PATH_WTMP
#define WTMP_FILENAME _PATH_WTMP
#else
#if defined(SYSV)
#define WTMP_FILENAME "/etc/wtmp"
#else
#define WTMP_FILENAME "/usr/adm/wtmp"
#endif
#endif
#endif

#include <signal.h>

#if defined(SCO) || defined(ISC)
#undef SIGTSTP			/* defined, but not the BSD way */
#endif

#ifdef SIGTSTP
#include <sys/wait.h>
#ifdef hpux
#include <sys/bsdtty.h>
#endif
#endif

#ifdef SIGNALRETURNSINT
#define SIGNAL_T int
#define SIGNAL_RETURN return 0
#else
#define SIGNAL_T void
#define SIGNAL_RETURN return
#endif

SIGNAL_T Exit();

#ifndef X_NOT_POSIX
#include <unistd.h>
#else
extern long lseek();
#ifdef USG
extern unsigned sleep();
#else
extern void sleep();
#endif
#endif

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
extern char *malloc();
extern char *calloc();
extern char *realloc();
extern char *getenv();
extern void exit();
#endif
#ifdef X_NOT_POSIX
extern char *ttyname();
#endif
#if defined(macII) && !defined(__STDC__)  /* stdlib.h fails to define these */
char *malloc(), *realloc(), *calloc();
char *ttyname(); /* and we don't get this from unistd.h */
#endif /* macII */

#ifdef SYSV
extern char *ptsname();
#endif

extern char *strindex ();
extern void HandlePopupMenu();

int switchfb[] = {0, 2, 1, 3};

static SIGNAL_T reapchild ();

static Bool added_utmp_entry = False;

static char **command_to_exec;

#ifdef USE_SYSV_TERMIO
/* The following structures are initialized in main() in order
** to eliminate any assumptions about the internal order of their
** contents.
*/
static struct termio d_tio;
#ifdef TIOCSLTC
static struct ltchars d_ltc;
#endif	/* TIOCSLTC */
#ifdef TIOCLSET
static unsigned int d_lmode;
#endif	/* TIOCLSET */
#else /* not USE_SYSV_TERMIO */
static struct  sgttyb d_sg = {
        0, 0, 0177, CKILL, EVENP|ODDP|ECHO|XTABS|CRMOD
};
static struct  tchars d_tc = {
        CINTR, CQUIT, CSTART,
        CSTOP, CEOF, CBRK,
};
static struct  ltchars d_ltc = {
        CSUSP, CDSUSP, CRPRNT,
        CFLUSH, CWERASE, CLNEXT
};
static int d_disipline = NTTYDISC;
static long int d_lmode = LCRTBS|LCRTERA|LCRTKIL|LCTLECH;
#ifdef sony
static long int d_jmode = KM_SYSSJIS|KM_ASCII;
static struct jtchars d_jtc = {
	'J', 'B'
};
#endif /* sony */
#endif /* USE_SYSV_TERMIO */

static int parse_tty_modes ();
/*
 * SYSV has the termio.c_cc[V] and ltchars; BSD has tchars and ltchars;
 * SVR4 has only termio.c_cc, but it includes everything from ltchars.
 */
static int override_tty_modes = 0;
struct _xttymodes {
    char *name;
    int len;
    int set;
    char value;
} ttymodelist[] = {
{ "intr", 4, 0, '\0' },			/* tchars.t_intrc ; VINTR */
#define XTTYMODE_intr 0
{ "quit", 4, 0, '\0' },			/* tchars.t_quitc ; VQUIT */
#define XTTYMODE_quit 1
{ "erase", 5, 0, '\0' },		/* sgttyb.sg_erase ; VERASE */
#define XTTYMODE_erase 2
{ "kill", 4, 0, '\0' },			/* sgttyb.sg_kill ; VKILL */
#define XTTYMODE_kill 3
{ "eof", 3, 0, '\0' },			/* tchars.t_eofc ; VEOF */
#define XTTYMODE_eof 4
{ "eol", 3, 0, '\0' },			/* VEOL */
#define XTTYMODE_eol 5
{ "swtch", 5, 0, '\0' },		/* VSWTCH */
#define XTTYMODE_swtch 6
{ "start", 5, 0, '\0' },		/* tchars.t_startc */
#define XTTYMODE_start 7
{ "stop", 4, 0, '\0' },			/* tchars.t_stopc */
#define XTTYMODE_stop 8
{ "brk", 3, 0, '\0' },			/* tchars.t_brkc */
#define XTTYMODE_brk 9
{ "susp", 4, 0, '\0' },			/* ltchars.t_suspc ; VSUSP */
#define XTTYMODE_susp 10
{ "dsusp", 5, 0, '\0' },		/* ltchars.t_dsuspc ; VDSUSP */
#define XTTYMODE_dsusp 11
{ "rprnt", 5, 0, '\0' },		/* ltchars.t_rprntc ; VREPRINT */
#define XTTYMODE_rprnt 12
{ "flush", 5, 0, '\0' },		/* ltchars.t_flushc ; VDISCARD */
#define XTTYMODE_flush 13
{ "weras", 5, 0, '\0' },		/* ltchars.t_werasc ; VWERASE */
#define XTTYMODE_weras 14
{ "lnext", 5, 0, '\0' },		/* ltchars.t_lnextc ; VLNEXT */
#define XTTYMODE_lnext 15
{ NULL, 0, 0, '\0' },			/* end of data */
};

#ifdef USE_SYSV_UTMP
#ifndef SVR4			/* otherwise declared in utmp.h */
extern struct utmp *getutent();
extern struct utmp *getutid();
extern struct utmp *getutline();
extern void pututline();
extern void setutent();
extern void endutent();
extern void utmpname();
#endif /* !SVR4 */

#ifndef SYSV386			/* could remove paragraph unconditionally? */
extern struct passwd *getpwent();
extern struct passwd *getpwuid();
extern struct passwd *getpwnam();
extern void setpwent();
extern void endpwent();
#endif

extern struct passwd *fgetpwent();
#else	/* not USE_SYSV_UTMP */
static char etc_utmp[] = UTMP_FILENAME;
#ifdef LASTLOG
static char etc_lastlog[] = LASTLOG_FILENAME;
#endif 
#endif	/* USE_SYSV_UTMP */

#ifdef WTMP
static char etc_wtmp[] = WTMP_FILENAME;
#endif

/*
 * Some people with 4.3bsd /bin/login seem to like to use login -p -f user
 * to implement xterm -ls.  They can turn on USE_LOGIN_DASH_P and turn off
 * WTMP and LASTLOG.
 */
#ifdef USE_LOGIN_DASH_P
#ifndef LOGIN_FILENAME
#define LOGIN_FILENAME "/bin/login"
#endif
static char bin_login[] = LOGIN_FILENAME;
#endif

static int inhibit;
static char passedPty[2];	/* name if pty if slave */

#ifdef TIOCCONS
static int Console;
#include <X11/Xmu/SysUtil.h>	/* XmuGetHostname */
#define MIT_CONSOLE_LEN	12
#define MIT_CONSOLE "MIT_CONSOLE_"
static char mit_console_name[255 + MIT_CONSOLE_LEN + 1] = MIT_CONSOLE;
static Atom mit_console;
#endif	/* TIOCCONS */

#ifndef USE_SYSV_UTMP
static int tslot;
#endif	/* USE_SYSV_UTMP */
static jmp_buf env;

char *ProgramName;
Boolean sunFunctionKeys;

static struct _resource {
    char *xterm_name;
    char *icon_geometry;
    char *title;
    char *icon_name;
    char *term_name;
    char *tty_modes;
    Boolean utmpInhibit;
    Boolean sunFunctionKeys;	/* %%% should be widget resource? */
    Boolean wait_for_map;
    Boolean useInsertMode;
} resource;

/* used by VT (charproc.c) */

#define offset(field)	XtOffsetOf(struct _resource, field)

static XtResource application_resources[] = {
    {"name", "Name", XtRString, sizeof(char *),
	offset(xterm_name), XtRString, "xterm"},
    {"iconGeometry", "IconGeometry", XtRString, sizeof(char *),
	offset(icon_geometry), XtRString, (caddr_t) NULL},
    {XtNtitle, XtCTitle, XtRString, sizeof(char *),
	offset(title), XtRString, (caddr_t) NULL},
    {XtNiconName, XtCIconName, XtRString, sizeof(char *),
	offset(icon_name), XtRString, (caddr_t) NULL},
    {"termName", "TermName", XtRString, sizeof(char *),
	offset(term_name), XtRString, (caddr_t) NULL},
    {"ttyModes", "TtyModes", XtRString, sizeof(char *),
	offset(tty_modes), XtRString, (caddr_t) NULL},
    {"utmpInhibit", "UtmpInhibit", XtRBoolean, sizeof (Boolean),
	offset(utmpInhibit), XtRString, "false"},
    {"sunFunctionKeys", "SunFunctionKeys", XtRBoolean, sizeof (Boolean),
	offset(sunFunctionKeys), XtRString, "false"},
    {"waitForMap", "WaitForMap", XtRBoolean, sizeof (Boolean),
        offset(wait_for_map), XtRString, "false"},
    {"useInsertMode", "UseInsertMode", XtRBoolean, sizeof (Boolean),
        offset(useInsertMode), XtRString, "false"},
};
#undef offset

static char *fallback_resources[] = {
    "XTerm*SimpleMenu*menuLabel.vertSpace: 100",
    "XTerm*SimpleMenu*HorizontalMargins: 16",
    "XTerm*SimpleMenu*Sme.height: 16",
    "XTerm*SimpleMenu*Cursor: left_ptr",
    "XTerm*mainMenu.Label:  Main Options (no app-defaults)",
    "XTerm*vtMenu.Label:  VT Options (no app-defaults)",
    "XTerm*fontMenu.Label:  VT Fonts (no app-defaults)",
    "XTerm*tekMenu.Label:  Tek Options (no app-defaults)",
    NULL
};

/* Command line options table.  Only resources are entered here...there is a
   pass over the remaining options after XrmParseCommand is let loose. */

static XrmOptionDescRec optionDescList[] = {
{"-geometry",	"*vt100.geometry",XrmoptionSepArg,	(caddr_t) NULL},
{"-132",	"*c132",	XrmoptionNoArg,		(caddr_t) "on"},
{"+132",	"*c132",	XrmoptionNoArg,		(caddr_t) "off"},
{"-ah",		"*alwaysHighlight", XrmoptionNoArg,	(caddr_t) "on"},
{"+ah",		"*alwaysHighlight", XrmoptionNoArg,	(caddr_t) "off"},
{"-b",		"*internalBorder",XrmoptionSepArg,	(caddr_t) NULL},
{"-cb",		"*cutToBeginningOfLine", XrmoptionNoArg, (caddr_t) "off"},
{"+cb",		"*cutToBeginningOfLine", XrmoptionNoArg, (caddr_t) "on"},
{"-cc",		"*charClass",	XrmoptionSepArg,	(caddr_t) NULL},
{"-cn",		"*cutNewline",	XrmoptionNoArg,		(caddr_t) "off"},
{"+cn",		"*cutNewline",	XrmoptionNoArg,		(caddr_t) "on"},
{"-cr",		"*cursorColor",	XrmoptionSepArg,	(caddr_t) NULL},
{"-cu",		"*curses",	XrmoptionNoArg,		(caddr_t) "on"},
{"+cu",		"*curses",	XrmoptionNoArg,		(caddr_t) "off"},
{"-e",		NULL,		XrmoptionSkipLine,	(caddr_t) NULL},
{"-fb",		"*boldFont",	XrmoptionSepArg,	(caddr_t) NULL},
{"-j",		"*jumpScroll",	XrmoptionNoArg,		(caddr_t) "on"},
{"+j",		"*jumpScroll",	XrmoptionNoArg,		(caddr_t) "off"},
{"-l",		"*logging",	XrmoptionNoArg,		(caddr_t) "on"},
{"+l",		"*logging",	XrmoptionNoArg,		(caddr_t) "off"},
{"-lf",		"*logFile",	XrmoptionSepArg,	(caddr_t) NULL},
{"-ls",		"*loginShell",	XrmoptionNoArg,		(caddr_t) "on"},
{"+ls",		"*loginShell",	XrmoptionNoArg,		(caddr_t) "off"},
{"-mb",		"*marginBell",	XrmoptionNoArg,		(caddr_t) "on"},
{"+mb",		"*marginBell",	XrmoptionNoArg,		(caddr_t) "off"},
{"-mc",		"*multiClickTime", XrmoptionSepArg,	(caddr_t) NULL},
{"-ms",		"*pointerColor",XrmoptionSepArg,	(caddr_t) NULL},
{"-nb",		"*nMarginBell",	XrmoptionSepArg,	(caddr_t) NULL},
{"-rw",		"*reverseWrap",	XrmoptionNoArg,		(caddr_t) "on"},
{"+rw",		"*reverseWrap",	XrmoptionNoArg,		(caddr_t) "off"},
{"-aw",		"*autoWrap",	XrmoptionNoArg,		(caddr_t) "on"},
{"+aw",		"*autoWrap",	XrmoptionNoArg,		(caddr_t) "off"},
{"-s",		"*multiScroll",	XrmoptionNoArg,		(caddr_t) "on"},
{"+s",		"*multiScroll",	XrmoptionNoArg,		(caddr_t) "off"},
{"-sb",		"*scrollBar",	XrmoptionNoArg,		(caddr_t) "on"},
{"+sb",		"*scrollBar",	XrmoptionNoArg,		(caddr_t) "off"},
{"-sf",		"*sunFunctionKeys", XrmoptionNoArg,	(caddr_t) "on"},
{"+sf",		"*sunFunctionKeys", XrmoptionNoArg,	(caddr_t) "off"},
{"-si",		"*scrollTtyOutput",	XrmoptionNoArg,		(caddr_t) "off"},
{"+si",		"*scrollTtyOutput",	XrmoptionNoArg,		(caddr_t) "on"},
{"-sk",		"*scrollKey",	XrmoptionNoArg,		(caddr_t) "on"},
{"+sk",		"*scrollKey",	XrmoptionNoArg,		(caddr_t) "off"},
{"-sl",		"*saveLines",	XrmoptionSepArg,	(caddr_t) NULL},
{"-t",		"*tekStartup",	XrmoptionNoArg,		(caddr_t) "on"},
{"+t",		"*tekStartup",	XrmoptionNoArg,		(caddr_t) "off"},
{"-tm",		"*ttyModes",	XrmoptionSepArg,	(caddr_t) NULL},
{"-tn",		"*termName",	XrmoptionSepArg,	(caddr_t) NULL},
{"-ut",		"*utmpInhibit",	XrmoptionNoArg,		(caddr_t) "on"},
{"+ut",		"*utmpInhibit",	XrmoptionNoArg,		(caddr_t) "off"},
{"-im",		"*useInsertMode", XrmoptionNoArg,	(caddr_t) "on"},
{"+im",		"*useInsertMode", XrmoptionNoArg,	(caddr_t) "off"},
{"-vb",		"*visualBell",	XrmoptionNoArg,		(caddr_t) "on"},
{"+vb",		"*visualBell",	XrmoptionNoArg,		(caddr_t) "off"},
{"-wf",		"*waitForMap",	XrmoptionNoArg,		(caddr_t) "on"},
{"+wf",		"*waitForMap",	XrmoptionNoArg,		(caddr_t) "off"},
/* bogus old compatibility stuff for which there are
   standard XtAppInitialize options now */
{"%",		"*tekGeometry",	XrmoptionStickyArg,	(caddr_t) NULL},
{"#",		".iconGeometry",XrmoptionStickyArg,	(caddr_t) NULL},
{"-T",		"*title",	XrmoptionSepArg,	(caddr_t) NULL},
{"-n",		"*iconName",	XrmoptionSepArg,	(caddr_t) NULL},
{"-r",		"*reverseVideo",XrmoptionNoArg,		(caddr_t) "on"},
{"+r",		"*reverseVideo",XrmoptionNoArg,		(caddr_t) "off"},
{"-rv",		"*reverseVideo",XrmoptionNoArg,		(caddr_t) "on"},
{"+rv",		"*reverseVideo",XrmoptionNoArg,		(caddr_t) "off"},
{"-w",		".borderWidth", XrmoptionSepArg,	(caddr_t) NULL},
};

static struct _options {
  char *opt;
  char *desc;
} options[] = {
{ "-help",                 "print out this message" },
{ "-display displayname",  "X server to contact" },
{ "-geometry geom",        "size (in characters) and position" },
{ "-/+rv",                 "turn on/off reverse video" },
{ "-bg color",             "background color" },
{ "-fg color",             "foreground color" },
{ "-bd color",             "border color" },
{ "-bw number",            "border width in pixels" },
{ "-fn fontname",          "normal text font" },
{ "-iconic",               "start iconic" },
{ "-name string",          "client instance, icon, and title strings" },
{ "-title string",         "title string" },
{ "-xrm resourcestring",   "additional resource specifications" },
{ "-/+132",                "turn on/off column switch inhibiting" },
{ "-/+ah",                 "turn on/off always highlight" },
{ "-b number",             "internal border in pixels" },
{ "-/+cb",                 "turn on/off cut-to-beginning-of-line inhibit" },
{ "-cc classrange",        "specify additional character classes" },
{ "-/+cn",                 "turn on/off cut newline inhibit" },
{ "-cr color",             "text cursor color" },
{ "-/+cu",                 "turn on/off curses emulation" },
{ "-fb fontname",          "bold text font" },
{ "-/+im",		   "use insert mode for TERMCAP" },
{ "-/+j",                  "turn on/off jump scroll" },
{ "-/+l",                  "turn on/off logging" },
{ "-lf filename",          "logging filename" },
{ "-/+ls",                 "turn on/off login shell" },
{ "-/+mb",                 "turn on/off margin bell" },
{ "-mc milliseconds",      "multiclick time in milliseconds" },
{ "-ms color",             "pointer color" },
{ "-nb number",            "margin bell in characters from right end" },
{ "-/+aw",                 "turn on/off auto wraparound" },
{ "-/+rw",                 "turn on/off reverse wraparound" },
{ "-/+s",                  "turn on/off multiscroll" },
{ "-/+sb",                 "turn on/off scrollbar" },
{ "-/+sf",                 "turn on/off Sun Function Key escape codes" },
{ "-/+si",                 "turn on/off scroll-on-tty-output inhibit" },
{ "-/+sk",                 "turn on/off scroll-on-keypress" },
{ "-sl number",            "number of scrolled lines to save" },
{ "-/+t",                  "turn on/off Tek emulation window" },
{ "-tm string",            "terminal mode keywords and characters" },
{ "-tn name",              "TERM environment variable name" },
#ifdef UTMP
{ "-/+ut",                 "turn on/off utmp inhibit" },
#else
{ "-/+ut",                 "turn on/off utmp inhibit (not supported)" },
#endif
{ "-/+vb",                 "turn on/off visual bell" },
{ "-/+wf",                 "turn on/off wait for map before command exec" },
{ "-e command args ...",   "command to execute" },
{ "%geom",                 "Tek window geometry" },
{ "#geom",                 "icon window geometry" },
{ "-T string",             "title name for window" },
{ "-n string",             "icon name for window" },
#ifdef TIOCCONS
{ "-C",                    "intercept console messages" },
#else
{ "-C",                    "intercept console messages (not supported)" },
#endif
{ "-Sxxd",                 "slave mode on \"ttyxx\", file descriptor \"d\"" },
{ NULL, NULL }};

static char *message[] = {
"Fonts must be fixed width and, if both normal and bold are specified, must",
"have the same size.  If only a normal font is specified, it will be used for",
"both normal and bold text (by doing overstriking).  The -e option, if given,",
"must be appear at the end of the command line, otherwise the user's default",
"shell will be started.  Options that start with a plus sign (+) restore the",
"default.",
NULL};

static void Syntax (badOption)
    char *badOption;
{
    struct _options *opt;
    int col;

    fprintf (stderr, "%s:  bad command line option \"%s\"\r\n\n",
	     ProgramName, badOption);

    fprintf (stderr, "usage:  %s", ProgramName);
    col = 8 + strlen(ProgramName);
    for (opt = options; opt->opt; opt++) {
	int len = 3 + strlen(opt->opt);	 /* space [ string ] */
	if (col + len > 79) {
	    fprintf (stderr, "\r\n   ");  /* 3 spaces */
	    col = 3;
	}
	fprintf (stderr, " [%s]", opt->opt);
	col += len;
    }

    fprintf (stderr, "\r\n\nType %s -help for a full description.\r\n\n",
	     ProgramName);
    exit (1);
}

static void Help ()
{
    struct _options *opt;
    char **cpp;

    fprintf (stderr, "usage:\n        %s [-options ...] [-e command args]\n\n",
	     ProgramName);
    fprintf (stderr, "where options include:\n");
    for (opt = options; opt->opt; opt++) {
	fprintf (stderr, "    %-28s %s\n", opt->opt, opt->desc);
    }

    putc ('\n', stderr);
    for (cpp = message; *cpp; cpp++) {
	fputs (*cpp, stderr);
	putc ('\n', stderr);
    }
    putc ('\n', stderr);

    exit (0);
}

#ifdef TIOCCONS
/* ARGSUSED */
static Boolean
ConvertConsoleSelection(w, selection, target, type, value, length, format)
    Widget w;
    Atom *selection, *target, *type;
    XtPointer *value;
    unsigned long *length;
    int *format;
{
    /* we don't save console output, so can't offer it */
    return False;
}
#endif /* TIOCCONS */


extern WidgetClass xtermWidgetClass;

Arg ourTopLevelShellArgs[] = {
	{ XtNallowShellResize, (XtArgVal) TRUE },	
	{ XtNinput, (XtArgVal) TRUE },
};
int number_ourTopLevelShellArgs = 2;
	
XtAppContext app_con;
Widget toplevel;
Bool waiting_for_initial_map;

extern void do_hangup();

/*
 * DeleteWindow(): Action proc to implement ICCCM delete_window.
 */
/* ARGSUSED */
void
DeleteWindow(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
  if (w == toplevel)
    if (term->screen.Tshow)
      hide_vt_window();
    else
      do_hangup(w);
  else
    if (term->screen.Vshow)
      hide_tek_window();
    else
      do_hangup(w);
}

/* ARGSUSED */
void
KeyboardMapping(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    switch (event->type) {
       case MappingNotify:
	  XRefreshKeyboardMapping(&event->xmapping);
	  break;
    }
}

XtActionsRec actionProcs[] = {
    "DeleteWindow", DeleteWindow,
    "KeyboardMapping", KeyboardMapping,
};

Atom wm_delete_window;

main (argc, argv)
int argc;
char **argv;
{
	register TScreen *screen;
	register int i, pty;
	int Xsocket, mode;
	char *base_name();
	int xerror(), xioerror();

	ProgramName = argv[0];

	ttydev = (char *) malloc (strlen (TTYDEV) + 1);
	ptydev = (char *) malloc (strlen (PTYDEV) + 1);
	if (!ttydev || !ptydev) {
	    fprintf (stderr, 
	    	     "%s:  unable to allocate memory for ttydev or ptydev\n",
		     ProgramName);
	    exit (1);
	}
	strcpy (ttydev, TTYDEV);
	strcpy (ptydev, PTYDEV);

#ifdef USE_SYSV_TERMIO
	/* Initialization is done here rather than above in order
	** to prevent any assumptions about the order of the contents
	** of the various terminal structures (which may change from
	** implementation to implementation).
	*/
#if defined(macII) || defined(ATT) || defined(CRAY)
	d_tio.c_iflag = ICRNL|IXON;
	d_tio.c_oflag = OPOST|ONLCR|TAB3;
    	d_tio.c_cflag = B9600|CS8|CREAD|PARENB|HUPCL;
    	d_tio.c_lflag = ISIG|ICANON|ECHO|ECHOE|ECHOK;

#ifndef USE_TERMIOS
	d_tio.c_line = 0;
#endif

	d_tio.c_cc[VINTR] = CINTR;
	d_tio.c_cc[VQUIT] = CQUIT;
	d_tio.c_cc[VERASE] = CERASE;
	d_tio.c_cc[VKILL] = CKILL;
    	d_tio.c_cc[VEOF] = CEOF;
	d_tio.c_cc[VEOL] = CNUL;
	d_tio.c_cc[VEOL2] = CNUL;
	d_tio.c_cc[VSWTCH] = CNUL;

#ifdef USE_TERMIOS
	d_tio.c_cc[VSUSP] = CSUSP;
	d_tio.c_cc[VDSUSP] = CDSUSP;
	d_tio.c_cc[VREPRINT] = CNUL;
	d_tio.c_cc[VDISCARD] = CNUL;
	d_tio.c_cc[VWERASE] = CNUL;
	d_tio.c_cc[VLNEXT] = CNUL;
#endif
#ifdef TIOCSLTC
        d_ltc.t_suspc = CSUSP;		/* t_suspc */
        d_ltc.t_dsuspc = CDSUSP;	/* t_dsuspc */
        d_ltc.t_rprntc = 0;		/* reserved...*/
        d_ltc.t_flushc = 0;
        d_ltc.t_werasc = 0;
        d_ltc.t_lnextc = 0;
#endif /* TIOCSLTC */
#ifdef TIOCLSET
	d_lmode = 0;
#endif /* TIOCLSET */
#else  /* else !macII */
	d_tio.c_iflag = ICRNL|IXON;
	d_tio.c_oflag = OPOST|ONLCR|TAB3;
#ifdef BAUD_0
    	d_tio.c_cflag = CS8|CREAD|PARENB|HUPCL;
#else	/* !BAUD_0 */
    	d_tio.c_cflag = B9600|CS8|CREAD|PARENB|HUPCL;
#endif	/* !BAUD_0 */
    	d_tio.c_lflag = ISIG|ICANON|ECHO|ECHOE|ECHOK;
	d_tio.c_line = 0;
	d_tio.c_cc[VINTR] = 0x7f;		/* DEL  */
	d_tio.c_cc[VQUIT] = '\\' & 0x3f;	/* '^\'	*/
	d_tio.c_cc[VERASE] = '#';		/* '#'	*/
	d_tio.c_cc[VKILL] = '@';		/* '@'	*/
    	d_tio.c_cc[VEOF] = 'D' & 0x3f;		/* '^D'	*/
	d_tio.c_cc[VEOL] = '@' & 0x3f;		/* '^@'	*/
#ifdef VSWTCH
	d_tio.c_cc[VSWTCH] = '@' & 0x3f;	/* '^@'	*/
#endif	/* VSWTCH */
	/* now, try to inherit tty settings */
	{
	    int i;

	    for (i = 0; i <= 2; i++) {
		struct termio deftio;
		if (ioctl (i, TCGETA, &deftio) == 0) {
		    d_tio.c_cc[VINTR] = deftio.c_cc[VINTR];
		    d_tio.c_cc[VQUIT] = deftio.c_cc[VQUIT];
		    d_tio.c_cc[VERASE] = deftio.c_cc[VERASE];
		    d_tio.c_cc[VKILL] = deftio.c_cc[VKILL];
		    d_tio.c_cc[VEOF] = deftio.c_cc[VEOF];
		    d_tio.c_cc[VEOL] = deftio.c_cc[VEOL];
#ifdef VSWTCH
		    d_tio.c_cc[VSWTCH] = deftio.c_cc[VSWTCH];
#endif /* VSWTCH */
		    break;
		}
	    }
	}
#ifdef TIOCSLTC
        d_ltc.t_suspc = '\000';		/* t_suspc */
        d_ltc.t_dsuspc = '\000';	/* t_dsuspc */
        d_ltc.t_rprntc = '\377';	/* reserved...*/
        d_ltc.t_flushc = '\377';
        d_ltc.t_werasc = '\377';
        d_ltc.t_lnextc = '\377';
#endif	/* TIOCSLTC */
#ifdef USE_TERMIOS
	d_tio.c_cc[VSUSP] = '\000';
	d_tio.c_cc[VDSUSP] = '\000';
	d_tio.c_cc[VREPRINT] = '\377';
	d_tio.c_cc[VDISCARD] = '\377';
	d_tio.c_cc[VWERASE] = '\377';
	d_tio.c_cc[VLNEXT] = '\377';
#endif
#ifdef TIOCLSET
	d_lmode = 0;
#endif	/* TIOCLSET */
#endif  /* macII */
#endif	/* USE_SYSV_TERMIO */

	/* Init the Toolkit. */
	toplevel = XtAppInitialize (&app_con, "XTerm", 
				    optionDescList, XtNumber(optionDescList), 
				    &argc, argv, fallback_resources, NULL, 0);

	XtGetApplicationResources(toplevel, (XtPointer) &resource,
				  application_resources,
				  XtNumber(application_resources), NULL, 0);

	waiting_for_initial_map = resource.wait_for_map;

	/*
	 * ICCCM delete_window.
	 */
	XtAppAddActions(app_con, actionProcs, XtNumber(actionProcs));

	/*
	 * fill in terminal modes
	 */
	if (resource.tty_modes) {
	    int n = parse_tty_modes (resource.tty_modes, ttymodelist);
	    if (n < 0) {
		fprintf (stderr, "%s:  bad tty modes \"%s\"\n",
			 ProgramName, resource.tty_modes);
	    } else if (n > 0) {
		override_tty_modes = 1;
	    }
	}

	xterm_name = resource.xterm_name;
	sunFunctionKeys = resource.sunFunctionKeys;
	if (strcmp(xterm_name, "-") == 0) xterm_name = "xterm";
	if (resource.icon_geometry != NULL) {
	    int scr, junk;
	    int ix, iy;
	    Arg args[2];

	    for(scr = 0;	/* yyuucchh */
		XtScreen(toplevel) != ScreenOfDisplay(XtDisplay(toplevel),scr);
		scr++);

	    args[0].name = XtNiconX;
	    args[1].name = XtNiconY;
	    XGeometry(XtDisplay(toplevel), scr, resource.icon_geometry, "",
		      0, 0, 0, 0, 0, &ix, &iy, &junk, &junk);
	    args[0].value = (XtArgVal) ix;
	    args[1].value = (XtArgVal) iy;
	    XtSetValues( toplevel, args, 2);
	}

	XtSetValues (toplevel, ourTopLevelShellArgs, 
		     number_ourTopLevelShellArgs);


	/* Parse the rest of the command line */
	for (argc--, argv++ ; argc > 0 ; argc--, argv++) {
	    if(**argv != '-') Syntax (*argv);

	    switch(argv[0][1]) {
	     case 'h':
		Help ();
		/* NOTREACHED */
	     case 'C':
#ifdef TIOCCONS
		{
		    struct stat sbuf;

		    /* Must be owner and have read/write permission.
		       xdm cooperates to give the console the right user. */
		    if ( !stat("/dev/console", &sbuf) &&
			 (sbuf.st_uid == getuid()) &&
			 !access("/dev/console", R_OK|W_OK))
		    {
			Console = TRUE;
		    } else
			Console = FALSE;
		}
#endif	/* TIOCCONS */
		continue;
	     case 'S':
		if (sscanf(*argv + 2, "%c%c%d", passedPty, passedPty+1,
			   &am_slave) != 3)
		    Syntax(*argv);
		continue;
#ifdef DEBUG
	     case 'D':
		debug = TRUE;
		continue;
#endif	/* DEBUG */
	     case 'e':
		if (argc <= 1) Syntax (*argv);
		command_to_exec = ++argv;
		break;
	     default:
		Syntax (*argv);
	    }
	    break;
	}

	XawSimpleMenuAddGlobalActions (app_con);
	XtRegisterGrabAction (HandlePopupMenu, True,
			      (ButtonPressMask|ButtonReleaseMask),
			      GrabModeAsync, GrabModeAsync);

        term = (XtermWidget) XtCreateManagedWidget(
	    "vt100", xtermWidgetClass, toplevel, NULL, 0);
            /* this causes the initialize method to be called */

        screen = &term->screen;

	if (screen->savelines < 0) screen->savelines = 0;

	term->flags = 0;
	if (!screen->jumpscroll) {
	    term->flags |= SMOOTHSCROLL;
	    update_jumpscroll();
	}
	if (term->misc.reverseWrap) {
	    term->flags |= REVERSEWRAP;
	    update_reversewrap();
	}
	if (term->misc.autoWrap) {
	    term->flags |= WRAPAROUND;
	    update_autowrap();
	}
	if (term->misc.re_verse) {
	    term->flags |= REVERSE_VIDEO;
	    update_reversevideo();
	}

	inhibit = 0;
	if (term->misc.logInhibit) 	    inhibit |= I_LOG;
	if (term->misc.signalInhibit)		inhibit |= I_SIGNAL;
	if (term->misc.tekInhibit)			inhibit |= I_TEK;

	term->initflags = term->flags;

	if (term->misc.appcursorDefault) {
	    term->keyboard.flags |= CURSOR_APL;
	    update_appcursor();
	}

	if (term->misc.appkeypadDefault) {
	    term->keyboard.flags |= KYPD_APL;
	    update_appkeypad();
	}

/*
 * Set title and icon name if not specified
 */

	if (command_to_exec) {
	    Arg args[2];

	    if (!resource.title) {
		if (command_to_exec) {
		    resource.title = base_name (command_to_exec[0]);
		} /* else not reached */
	    }

	    if (!resource.icon_name) 
	      resource.icon_name = resource.title;
	    XtSetArg (args[0], XtNtitle, resource.title);
	    XtSetArg (args[1], XtNiconName, resource.icon_name);		

	    XtSetValues (toplevel, args, 2);
	}


	if(inhibit & I_TEK)
		screen->TekEmu = FALSE;

	if(screen->TekEmu && !TekInit())
		exit(ERROR_INIT);

	/* set up stderr properly */
	i = -1;
#ifdef DEBUG
	if(debug)
		i = open ("xterm.debug.log", O_WRONLY | O_CREAT | O_TRUNC,
		 0666);
#endif	/* DEBUG */
	if(i >= 0) {
#if defined(USE_SYSV_TERMIO) && !defined(SVR4) && !defined(linux)
		/* SYSV has another pointer which should be part of the
		** FILE structure but is actually a seperate array.
		*/
		unsigned char *old_bufend;

		old_bufend = (unsigned char *) _bufend(stderr);
		stderr->_file = i;
		_bufend(stderr) = old_bufend;
#else	/* USE_SYSV_TERMIO */
#ifdef linux
		setfileno(stderr, i);
#else
		stderr->_file = i;
#endif
#endif	/* USE_SYSV_TERMIO */

		/* mark this file as close on exec */
		(void) fcntl(i, F_SETFD, 1);
	}

	/* open a terminal for client */
	get_terminal ();
	spawn ();
	/* Child process is out there, let's catch its termination */
	signal (SIGCHLD, reapchild);

	/* Realize procs have now been executed */

	Xsocket = ConnectionNumber(screen->display);
	pty = screen->respond;

	if (am_slave) { /* Write window id so master end can read and use */
	    char buf[80];

	    buf[0] = '\0';
	    sprintf (buf, "%lx\n", 
	    	     screen->TekEmu ? XtWindow (XtParent (tekWidget)) :
				      XtWindow (XtParent (term)));
	    write (pty, buf, strlen (buf));
	}

	if (term->misc.log_on) {
		StartLog(screen);
	}
	screen->inhibit = inhibit;

#ifdef AIXV3
	/* In AIXV3, xterms started from /dev/console have CLOCAL set.
	 * This means we need to clear CLOCAL so that SIGHUP gets sent
	 * to the slave-pty process when xterm exits. 
	 */

	{
	    struct termio tio;

	    if(ioctl(pty, TCGETA, &tio) == -1)
		SysError(ERROR_TIOCGETP);

	    tio.c_cflag &= ~(CLOCAL);

	    if (ioctl (pty, TCSETA, &tio) == -1)
		SysError(ERROR_TIOCSETP);
	}
#endif
#ifdef USE_SYSV_TERMIO
	if (0 > (mode = fcntl(pty, F_GETFL, 0)))
		Error();
#ifdef O_NDELAY
	mode |= O_NDELAY;
#else
	mode |= O_NONBLOCK;
#endif /* O_NDELAY */
	if (fcntl(pty, F_SETFL, mode))
		Error();
#else	/* USE_SYSV_TERMIO */
	mode = 1;
	if (ioctl (pty, FIONBIO, (char *)&mode) == -1) SysError (ERROR_FIONBIO);
#endif	/* USE_SYSV_TERMIO */
	
	pty_mask = 1 << pty;
	X_mask = 1 << Xsocket;
	Select_mask = pty_mask | X_mask;
	max_plus1 = (pty < Xsocket) ? (1 + Xsocket) : (1 + pty);

#ifdef DEBUG
	if (debug) printf ("debugging on\n");
#endif	/* DEBUG */
	XSetErrorHandler(xerror);
	XSetIOErrorHandler(xioerror);
	for( ; ; ) {
		if(screen->TekEmu) {
			TekRun();
		} else
			VTRun();
	}
}

char *base_name(name)
char *name;
{
	register char *cp;

	cp = rindex(name, '/');
	return(cp ? cp + 1 : name);
}

/* This function opens up a pty master and stuffs its value into pty.
 * If it finds one, it returns a value of 0.  If it does not find one,
 * it returns a value of !0.  This routine is designed to be re-entrant,
 * so that if a pty master is found and later, we find that the slave
 * has problems, we can re-enter this function and get another one.
 */

get_pty (pty)
    int *pty;
{
#if defined(SYSV) && defined(SYSV386) && !defined(SVR4)
        /*
	  The order of this code is *important*.  On SYSV/386 we want to open
	  a /dev/ttyp? first if at all possible.  If none are available, then
	  we'll try to open a /dev/pts??? device.
	  
	  The reason for this is because /dev/ttyp? works correctly, where
	  as /dev/pts??? devices have a number of bugs, (won't update
	  screen correcly, will hang -- it more or less works, but you
	  really don't want to use it).
	  
	  Most importantly, for boxes of this nature, one of the major
	  "features" is that you can emulate a 8086 by spawning off a UNIX
	  program on 80386/80486 in v86 mode.  In other words, you can spawn
	  off multiple MS-DOS environments.  On ISC the program that does
	  this is named "vpix."  The catcher is that "vpix" will *not* work
	  with a /dev/pts??? device, will only work with a /dev/ttyp? device.
	  
	  Since we can open either a /dev/ttyp? or a /dev/pts??? device,
	  the flag "IsPts" is set here so that we know which type of
	  device we're dealing with in routine spawn().  That's the reason
	  for the "if (IsPts)" statement in spawn(); we have two different
	  device types which need to be handled differently.
	  */
        if (pty_search(pty) == 0)
	    return 0;
#endif /* SYSV && SYSV386 */
#ifdef ATT
	if ((*pty = open ("/dev/ptmx", O_RDWR)) < 0) {
	    return 1;
	}
#if defined(SVR4) || defined(SYSV386)
	strcpy(ttydev, ptsname(*pty));
#if defined (SYSV) && defined(SYSV386)
	IsPts = True;
#endif
#endif
	return 0;
#else /* ATT else */
#ifdef AIXV3
	if ((*pty = open ("/dev/ptc", O_RDWR)) < 0) {
	    return 1;
	}
	strcpy(ttydev, ttyname(*pty));
	return 0;
#endif
#if defined(sgi) && OSMAJORVERSION >= 4
	{
	    char    *tty_name;

	    tty_name = _getpty (pty, O_RDWR, 0622, 0);
	    if (tty_name == 0)
		return 1;
	    strcpy (ttydev, tty_name);
	    return 0;
	}
#endif
#ifdef __convex__
        {
	    char *pty_name, *getpty();

	    while ((pty_name = getpty()) != NULL) {
		if ((*pty = open (pty_name, O_RDWR)) >= 0) {
		    strcpy(ptydev, pty_name);
		    strcpy(ttydev, pty_name);
		    ttydev[5] = 't';
		    return 0;
		}
	    }
	    return 1;
	}
#endif /* __convex__ */
#ifdef USE_GET_PSEUDOTTY
	return ((*pty = getpseudotty (&ttydev, &ptydev)) >= 0 ? 0 : 1);
#else
#if (defined(sgi) && OSMAJORVERSION < 4) || (defined(umips) && defined (SYSTYPE_SYSV))
	struct stat fstat_buf;

	*pty = open ("/dev/ptc", O_RDWR);
	if (*pty < 0 || (fstat (*pty, &fstat_buf)) < 0) {
	  return(1);
	}
	sprintf (ttydev, "/dev/ttyq%d", minor(fstat_buf.st_rdev));
#ifndef sgi
	sprintf (ptydev, "/dev/ptyq%d", minor(fstat_buf.st_rdev));
	if ((*tty = open (ttydev, O_RDWR)) < 0) {
	  close (*pty);
	  return(1);
	}
#endif /* !sgi */
	/* got one! */
	return(0);
#else /* sgi or umips */

	return pty_search(pty);

#endif /* sgi or umips else */
#endif /* USE_GET_PSEUDOTTY else */
#endif /* ATT else */
}

/*
 * Called from get_pty to iterate over likely pseudo terminals
 * we might allocate.  Used on those systems that do not have
 * a functional interface for allocating a pty.
 * Returns 0 if found a pty, 1 if fails.
 */
int pty_search(pty)
    int *pty;
{
    static int devindex, letter = 0;

#ifdef CRAY
    for (; devindex < 256; devindex++) {
	sprintf (ttydev, "/dev/ttyp%03d", devindex);
	sprintf (ptydev, "/dev/pty/%03d", devindex);

	if ((*pty = open (ptydev, O_RDWR)) >= 0) {
	    /* We need to set things up for our next entry
	     * into this function!
	     */
	    (void) devindex++;
	    return 0;
	}
    }
#else /* CRAY */
    while (PTYCHAR1[letter]) {
	ttydev [strlen(ttydev) - 2]  = ptydev [strlen(ptydev) - 2] =
	    PTYCHAR1 [letter];

	while (PTYCHAR2[devindex]) {
	    ttydev [strlen(ttydev) - 1] = ptydev [strlen(ptydev) - 1] =
		PTYCHAR2 [devindex];
	    if ((*pty = open (ptydev, O_RDWR)) >= 0) {
		/* We need to set things up for our next entry
		 * into this function!
		 */
		(void) devindex++;
		return 0;
	    }
	    devindex++;
	}
	devindex = 0;
	(void) letter++;
    }
#endif /* CRAY else */
    /*
     * We were unable to allocate a pty master!  Return an error
     * condition and let our caller terminate cleanly.
     */
    return 1;
}

get_terminal ()
/* 
 * sets up X and initializes the terminal structure except for term.buf.fildes.
 */
{
	register TScreen *screen = &term->screen;
	
	screen->arrow = make_colored_cursor (XC_left_ptr, 
					     screen->mousecolor,
					     screen->mousecolorback);
}

/*
 * The only difference in /etc/termcap between 4014 and 4015 is that 
 * the latter has support for switching character sets.  We support the
 * 4015 protocol, but ignore the character switches.  Therefore, we 
 * choose 4014 over 4015.
 *
 * Features of the 4014 over the 4012: larger (19") screen, 12-bit
 * graphics addressing (compatible with 4012 10-bit addressing),
 * special point plot mode, incremental plot mode (not implemented in
 * later Tektronix terminals), and 4 character sizes.
 * All of these are supported by xterm.
 */

static char *tekterm[] = {
	"tek4014",
	"tek4015",		/* 4014 with APL character set support */
	"tek4012",		/* 4010 with lower case */
	"tek4013",		/* 4012 with APL character set support */
	"tek4010",		/* small screen, upper-case only */
	"dumb",
	0
};

/* The VT102 is a VT100 with the Advanced Video Option included standard.
 * It also adds Escape sequences for insert/delete character/line.
 * The VT220 adds 8-bit character sets, selective erase.
 * The VT320 adds a 25th status line, terminal state interrogation.
 * The VT420 has up to 48 lines on the screen.
 */

static char *vtterm[] = {
#ifdef USE_X11TERM
	"x11term",		/* for people who want special term name */
#endif
	"xterm",		/* the prefered name, should be fastest */
	"vt102",
	"vt100",
	"ansi",
	"dumb",
	0
};

/* ARGSUSED */
SIGNAL_T hungtty(i)
	int i;
{
	longjmp(env, 1);
	SIGNAL_RETURN;
}

#ifdef USE_HANDSHAKE
typedef enum {		/* c == child, p == parent                        */
	PTY_BAD,	/* c->p: can't open pty slave for some reason     */
	PTY_FATALERROR,	/* c->p: we had a fatal error with the pty        */
	PTY_GOOD,	/* c->p: we have a good pty, let's go on          */
	PTY_NEW,	/* p->c: here is a new pty slave, try this        */
	PTY_NOMORE,	/* p->c; no more pty's, terminate                 */
	UTMP_ADDED,	/* c->p: utmp entry has been added                */
	UTMP_TTYSLOT,	/* c->p: here is my ttyslot                       */
	PTY_EXEC	/* p->c: window has been mapped the first time    */
} status_t;

typedef struct {
	status_t status;
	int error;
	int fatal_error;
	int tty_slot;
	int rows;
	int cols;
	char buffer[1024];
} handshake_t;

/* HsSysError()
 *
 * This routine does the equivalent of a SysError but it handshakes
 * over the errno and error exit to the master process so that it can
 * display our error message and exit with our exit code so that the
 * user can see it.
 */

void
HsSysError(pf, error)
int pf;
int error;
{
	handshake_t handshake;

	handshake.status = PTY_FATALERROR;
	handshake.error = errno;
	handshake.fatal_error = error;
	strcpy(handshake.buffer, ttydev);
	write(pf, (char *) &handshake, sizeof(handshake));
	exit(error);
}

static int pc_pipe[2];	/* this pipe is used for parent to child transfer */
static int cp_pipe[2];	/* this pipe is used for child to parent transfer */

void first_map_occurred ()
{
    handshake_t handshake;
    register TScreen *screen = &term->screen;

    handshake.status = PTY_EXEC;
    handshake.rows = screen->max_row;
    handshake.cols = screen->max_col;
    write (pc_pipe[1], (char *) &handshake, sizeof(handshake));
    close (cp_pipe[0]);
    close (pc_pipe[1]);
    waiting_for_initial_map = False;
}
#else
/*
 * temporary hack to get xterm working on att ptys
 */
void first_map_occurred ()
{
    return;
}
#define HsSysError(a,b)
#endif /* USE_HANDSHAKE else !USE_HANDSHAKE */


spawn ()
/* 
 *  Inits pty and tty and forks a login process.
 *  Does not close fd Xsocket.
 *  If slave, the pty named in passedPty is already open for use
 */
{
	extern char *SysErrorMsg();
	register TScreen *screen = &term->screen;
	int Xsocket = ConnectionNumber(screen->display);
#ifdef USE_HANDSHAKE
	handshake_t handshake;
#else
	int fds[2];
#endif
	int tty = -1;
	int discipline;
	int done;
#ifdef USE_SYSV_TERMIO
	struct termio tio;
	struct termio dummy_tio;
#ifdef TIOCLSET
	unsigned lmode;
#endif	/* TIOCLSET */
#ifdef TIOCSLTC
	struct ltchars ltc;
#endif	/* TIOCSLTC */
	int one = 1;
	int zero = 0;
	int status;
#else	/* else not USE_SYSV_TERMIO */
	unsigned lmode;
	struct tchars tc;
	struct ltchars ltc;
	struct sgttyb sg;
#ifdef sony
	int jmode;
	struct jtchars jtc;
#endif /* sony */
#endif	/* USE_SYSV_TERMIO */

	char termcap [1024];
	char newtc [1024];
	char *ptr, *shname, *shname_minus;
	int i, no_dev_tty = FALSE;
#ifdef USE_SYSV_TERMIO
	char *dev_tty_name = (char *) 0;
	int fd;			/* for /etc/wtmp */
#endif	/* USE_SYSV_TERMIO */
	char **envnew;		/* new environment */
	int envsize;		/* elements in new environment */
	char buf[64];
	char *TermName = NULL;
	int ldisc = 0;
#ifdef sun
#ifdef TIOCSSIZE
	struct ttysize ts;
#endif	/* TIOCSSIZE */
#else	/* not sun */
#ifdef TIOCSWINSZ
	struct winsize ws;
#endif	/* TIOCSWINSZ */
#endif	/* sun */
	struct passwd *pw = NULL;
#ifdef UTMP
	struct utmp utmp;
#ifdef LASTLOG
	struct lastlog lastlog;
#endif	/* LASTLOG */
#endif	/* UTMP */

	screen->uid = getuid();
	screen->gid = getgid();

#ifdef SIGTTOU
	/* so that TIOCSWINSZ || TIOCSIZE doesn't block */
	signal(SIGTTOU,SIG_IGN);
#endif

	if (am_slave) {
		screen->respond = am_slave;
		ptydev[strlen(ptydev) - 2] = ttydev[strlen(ttydev) - 2] =
			passedPty[0];
		ptydev[strlen(ptydev) - 1] = ttydev[strlen(ttydev) - 1] =
			passedPty[1];

		setgid (screen->gid);
		setuid (screen->uid);
	} else {
		Bool tty_got_hung = False;

 		/*
 		 * Sometimes /dev/tty hangs on open (as in the case of a pty
 		 * that has gone away).  Simply make up some reasonable
 		 * defaults.
 		 */
 		signal(SIGALRM, hungtty);
 		alarm(2);		/* alarm(1) might return too soon */
 		if (! setjmp(env)) {
 			tty = open ("/dev/tty", O_RDWR, 0);
 			alarm(0);
 		} else {
			tty_got_hung = True;
 			tty = -1;
 			errno = ENXIO;
 		}
 		signal(SIGALRM, SIG_DFL);
 
		/*
		 * Check results and ignore current control terminal if
		 * necessary.  ENXIO is what is normally returned if there is
		 * no controlling terminal, but some systems (e.g. SunOS 4.0)
		 * seem to return EIO.
		 */
 		if (tty < 0) {
			if (tty_got_hung || errno == ENXIO || errno == EIO ||
			    errno == ENOTTY) {
				no_dev_tty = TRUE;
#ifdef TIOCSLTC
				ltc = d_ltc;
#endif	/* TIOCSLTC */
#ifdef TIOCLSET
				lmode = d_lmode;
#endif	/* TIOCLSET */
#ifdef USE_SYSV_TERMIO
				tio = d_tio;
#else	/* not USE_SYSV_TERMIO */
				sg = d_sg;
				tc = d_tc;
				discipline = d_disipline;
#ifdef sony
				jmode = d_jmode;
				jtc = d_jtc;
#endif /* sony */
#endif	/* USE_SYSV_TERMIO */
			} else {
			    SysError(ERROR_OPDEVTTY);
			}
		} else {
			/* Get a copy of the current terminal's state,
			 * if we can.  Some systems (e.g., SVR4 and MacII)
			 * may not have a controlling terminal at this point
			 * if started directly from xdm or xinit,     
			 * in which case we just use the defaults as above.
			 */
#ifdef TIOCSLTC
			if(ioctl(tty, TIOCGLTC, &ltc) == -1)
				ltc = d_ltc;
#endif	/* TIOCSLTC */
#ifdef TIOCLSET
			if(ioctl(tty, TIOCLGET, &lmode) == -1)
				lmode = d_lmode;
#endif	/* TIOCLSET */
#ifdef USE_SYSV_TERMIO
		        if(ioctl(tty, TCGETA, &tio) == -1)
			        tio = d_tio;

#else	/* not USE_SYSV_TERMIO */
			if(ioctl(tty, TIOCGETP, (char *)&sg) == -1)
			        sg = d_sg;
			if(ioctl(tty, TIOCGETC, (char *)&tc) == -1)
			        tc = d_tc;
			if(ioctl(tty, TIOCGETD, (char *)&discipline) == -1)
			        discipline = d_disipline;
#ifdef sony
			if(ioctl(tty, TIOCKGET, (char *)&jmode) == -1)
			        jmode = d_jmode;
			if(ioctl(tty, TIOCKGETC, (char *)&jtc) == -1)
				jtc = d_jtc;
#endif /* sony */
#endif	/* USE_SYSV_TERMIO */
			close (tty);
			/* tty is no longer an open fd! */
			tty = -1;
		}

#ifdef 	PUCC_PTYD
		if(-1 == (screen->respond = openrpty(ttydev, ptydev,
				(resource.utmpInhibit ?  OPTY_NOP : OPTY_LOGIN),
				getuid(), XDisplayString(screen->display)))) {
#else /* not PUCC_PTYD */
		if (get_pty (&screen->respond)) {
#endif /* PUCC_PTYD */
			/*  no ptys! */
			(void) fprintf(stderr, "%s: no available ptys\n",
				       xterm_name);
			exit (ERROR_PTYS);
#ifdef PUCC_PTYD
		}
#else
		}			/* keep braces balanced for emacs */
#endif
#ifdef PUCC_PTYD
		  else {
			/*
			 *  set the fd of the master in a global var so
			 *  we can undo all this on exit
			 *
			 */
			Ptyfd = screen->respond;
		  }
#endif /* PUCC_PTYD */
	}

	/* avoid double MapWindow requests */
	XtSetMappedWhenManaged( screen->TekEmu ? XtParent(tekWidget) :
			        XtParent(term), False );
	wm_delete_window = XInternAtom(XtDisplay(toplevel), "WM_DELETE_WINDOW",
				       False);
	if (!screen->TekEmu)
	    VTInit();		/* realize now so know window size for tty driver */
#ifdef TIOCCONS
	if (Console) {
	    /*
	     * Inform any running xconsole program
	     * that we are going to steal the console.
	     */
	    XmuGetHostname (mit_console_name + MIT_CONSOLE_LEN, 255);
	    mit_console = XInternAtom(screen->display, mit_console_name, False);
	    /* the user told us to be the console, so we can use CurrentTime */
	    XtOwnSelection(screen->TekEmu ? XtParent(tekWidget) : XtParent(term),
			   mit_console, CurrentTime,
			   ConvertConsoleSelection, NULL, NULL);
	}
#endif
	if(screen->TekEmu) {
		envnew = tekterm;
		ptr = newtc;
	} else {
		envnew = vtterm;
		ptr = termcap;
	}
	TermName = NULL;
	if (resource.term_name) {
	    if (tgetent (ptr, resource.term_name) == 1) {
		TermName = resource.term_name;
		if (!screen->TekEmu)
		    resize (screen, TermName, termcap, newtc);
	    } else {
		fprintf (stderr, "%s:  invalid termcap entry \"%s\".\n",
			 ProgramName, resource.term_name);
	    }
	}
	if (!TermName) {
	    while (*envnew != NULL) {
		if(tgetent(ptr, *envnew) == 1) {
			TermName = *envnew;
			if(!screen->TekEmu)
			    resize(screen, TermName, termcap, newtc);
			break;
		}
		envnew++;
	    }
	    if (TermName == NULL) {
		fprintf (stderr, "%s:  unable to find usable termcap entry.\n",
			 ProgramName);
		Exit (1);
	    }
	}

#ifdef sun
#ifdef TIOCSSIZE
	/* tell tty how big window is */
	if(screen->TekEmu) {
		ts.ts_lines = 38;
		ts.ts_cols = 81;
	} else {
		ts.ts_lines = screen->max_row + 1;
		ts.ts_cols = screen->max_col + 1;
	}
#endif	/* TIOCSSIZE */
#else	/* not sun */
#ifdef TIOCSWINSZ
	/* tell tty how big window is */
	if(screen->TekEmu) {
		ws.ws_row = 38;
		ws.ws_col = 81;
		ws.ws_xpixel = TFullWidth(screen);
		ws.ws_ypixel = TFullHeight(screen);
	} else {
		ws.ws_row = screen->max_row + 1;
		ws.ws_col = screen->max_col + 1;
		ws.ws_xpixel = FullWidth(screen);
		ws.ws_ypixel = FullHeight(screen);
	}
#endif	/* TIOCSWINSZ */
#endif	/* sun */

	if (!am_slave) {
#ifdef USE_HANDSHAKE
	    if (pipe(pc_pipe) || pipe(cp_pipe))
		SysError (ERROR_FORK);
#endif
	    if ((screen->pid = fork ()) == -1)
		SysError (ERROR_FORK);
		
	    if (screen->pid == 0) {
		/*
		 * now in child process
		 */
		extern char **environ;
#if defined(_POSIX_SOURCE) || defined(SVR4) || defined(__convex__)
		int pgrp = setsid();
#else
		int pgrp = getpid();
#endif
#ifdef USE_SYSV_TERMIO
		char numbuf[12];
#if defined(UTMP) && defined(USE_SYSV_UTMP)
		char *ptyname;
#endif
#endif	/* USE_SYSV_TERMIO */

#ifdef USE_USG_PTYS
#if defined(SYSV) && defined(SYSV386)
                if (IsPts) {	/* SYSV386 supports both, which did we open? */
#endif /* SYSV && SYSV386 */
		int ptyfd;

		setpgrp();
		grantpt (screen->respond);
		unlockpt (screen->respond);
		if ((ptyfd = open (ptsname(screen->respond), O_RDWR)) < 0) {
		    SysError (1);
		}
		if (ioctl (ptyfd, I_PUSH, "ptem") < 0) {
		    SysError (2);
		}
#if !defined(SVR4) && !defined(SYSV386)
		if (!getenv("CONSEM") && ioctl (ptyfd, I_PUSH, "consem") < 0) {
		    SysError (3);
		}
#endif /* !SVR4 */
		if (ioctl (ptyfd, I_PUSH, "ldterm") < 0) {
		    SysError (4);
		}
#ifdef SVR4			/* from Sony */
		if (ioctl (ptyfd, I_PUSH, "ttcompat") < 0) {
		    SysError (5);
		}
#endif /* SVR4 */
		tty = ptyfd;
		close (screen->respond);
#ifdef TIOCSWINSZ
                /* tell tty how big window is */
                if(screen->TekEmu) {
                        ws.ws_row = 24;
                        ws.ws_col = 80;
                        ws.ws_xpixel = TFullWidth(screen);
                        ws.ws_ypixel = TFullHeight(screen);
                } else {
                        ws.ws_row = screen->max_row + 1;
                        ws.ws_col = screen->max_col + 1;
                        ws.ws_xpixel = FullWidth(screen);
                        ws.ws_ypixel = FullHeight(screen);
                }
#endif
#if defined(SYSV) && defined(SYSV386)
                } else {	/* else pty, not pts */
#endif /* SYSV && SYSV386 */
#endif /* USE_USG_PTYS */

#ifdef USE_HANDSHAKE		/* warning, goes for a long ways */
		/* close parent's sides of the pipes */
		close (cp_pipe[0]);
		close (pc_pipe[1]);

		/* Make sure that our sides of the pipes are not in the
		 * 0, 1, 2 range so that we don't fight with stdin, out
		 * or err.
		 */
		if (cp_pipe[1] <= 2) {
			if ((i = fcntl(cp_pipe[1], F_DUPFD, 3)) >= 0) {
				(void) close(cp_pipe[1]);
				cp_pipe[1] = i;
			}
		}
		if (pc_pipe[0] <= 2) {
			if ((i = fcntl(pc_pipe[0], F_DUPFD, 3)) >= 0) {
				(void) close(pc_pipe[0]);
				pc_pipe[0] = i;
			}
		}

		/* we don't need the socket, or the pty master anymore */
		close (Xsocket);
		close (screen->respond);

		/* Now is the time to set up our process group and
		 * open up the pty slave.
		 */
#ifdef	USE_SYSV_PGRP
#if defined(CRAY) && (OSMAJORVERSION > 5)
		(void) setsid();
#else
		(void) setpgrp();
#endif
#endif /* USE_SYSV_PGRP */
		while (1) {
#ifdef TIOCNOTTY
			if (!no_dev_tty && (tty = open ("/dev/tty", O_RDWR)) >= 0) {
				ioctl (tty, TIOCNOTTY, (char *) NULL);
				close (tty);
			}
#endif /* TIOCNOTTY */
			if ((tty = open(ttydev, O_RDWR, 0)) >= 0) {
#if defined(CRAY) && defined(TCSETCTTY)
			    /* make /dev/tty work */
			    ioctl(tty, TCSETCTTY, 0);
#endif
#ifdef	USE_SYSV_PGRP
				/* We need to make sure that we are acutally
				 * the process group leader for the pty.  If
				 * we are, then we should now be able to open
				 * /dev/tty.
				 */
				if ((i = open("/dev/tty", O_RDWR, 0)) >= 0) {
					/* success! */
					close(i);
					break;
				}
#else	/* USE_SYSV_PGRP */
				break;
#endif	/* USE_SYSV_PGRP */
			}

#ifdef TIOCSCTTY
			ioctl(tty, TIOCSCTTY, 0);
#endif
			/* let our master know that the open failed */
			handshake.status = PTY_BAD;
			handshake.error = errno;
			strcpy(handshake.buffer, ttydev);
			write(cp_pipe[1], (char *) &handshake,
			    sizeof(handshake));

			/* get reply from parent */
			i = read(pc_pipe[0], (char *) &handshake,
			    sizeof(handshake));
			if (i <= 0) {
				/* parent terminated */
				exit(1);
			}

			if (handshake.status == PTY_NOMORE) {
				/* No more ptys, let's shutdown. */
				exit(1);
			}

			/* We have a new pty to try */
			free(ttydev);
			ttydev = malloc((unsigned)
			    (strlen(handshake.buffer) + 1));
			strcpy(ttydev, handshake.buffer);
		}

		/* use the same tty name that everyone else will use
		** (from ttyname)
		*/
		if (ptr = ttyname(tty))
		{
			/* it may be bigger */
			ttydev = realloc (ttydev, (unsigned) (strlen(ptr) + 1));
			(void) strcpy(ttydev, ptr);
		}
#if defined(SYSV) && defined(SYSV386)
                } /* end of IsPts else clause */
#endif /* SYSV && SYSV386 */

#endif /* USE_HANDSHAKE -- from near fork */

#ifdef USE_TTY_GROUP
	{ 
#include <grp.h>
		struct group *ttygrp;
		if (ttygrp = getgrnam("tty")) {
			/* change ownership of tty to real uid, "tty" gid */
			chown (ttydev, screen->uid, ttygrp->gr_gid);
			chmod (ttydev, 0620);
		}
		else {
			/* change ownership of tty to real group and user id */
			chown (ttydev, screen->uid, screen->gid);
			chmod (ttydev, 0622);
		}
		endgrent();
	}
#else /* else !USE_TTY_GROUP */
		/* change ownership of tty to real group and user id */
		chown (ttydev, screen->uid, screen->gid);

		/* change protection of tty */
		chmod (ttydev, 0622);
#endif /* USE_TTY_GROUP */

		/*
		 * set up the tty modes
		 */
		{
#ifdef USE_SYSV_TERMIO
#if defined(umips) || defined(CRAY)
		    /* If the control tty had its modes screwed around with,
		       eg. by lineedit in the shell, or emacs, etc. then tio
		       will have bad values.  Let's just get termio from the
		       new tty and tailor it.  */
		    if (ioctl (tty, TCGETA, &tio) == -1)
		      SysError (ERROR_TIOCGETP);
		    tio.c_lflag |= ECHOE;
#endif /* umips */
		    /* Now is also the time to change the modes of the
		     * child pty.
		     */
		    /* input: nl->nl, don't ignore cr, cr->nl */
		    tio.c_iflag &= ~(INLCR|IGNCR);
		    tio.c_iflag |= ICRNL;
		    /* ouput: cr->cr, nl is not return, no delays, ln->cr/nl */
		    tio.c_oflag &=
		     ~(OCRNL|ONLRET|NLDLY|CRDLY|TABDLY|BSDLY|VTDLY|FFDLY);
		    tio.c_oflag |= ONLCR;
#ifdef OPOST
		    tio.c_oflag |= OPOST;
#endif /* OPOST */		    
#ifdef BAUD_0
		    /* baud rate is 0 (don't care) */
		    tio.c_cflag &= ~(CBAUD);
#else	/* !BAUD_0 */
		    /* baud rate is 9600 (nice default) */
		    tio.c_cflag &= ~(CBAUD);
		    tio.c_cflag |= B9600;
#endif	/* !BAUD_0 */
		    /* enable signals, canonical processing (erase, kill, etc),
		    ** echo
		    */
		    tio.c_lflag |= ISIG|ICANON|ECHO;
		    /* reset EOL to defalult value */
		    tio.c_cc[VEOL] = '@' & 0x3f;		/* '^@'	*/
		    /* certain shells (ksh & csh) change EOF as well */
		    tio.c_cc[VEOF] = 'D' & 0x3f;		/* '^D'	*/

#define TMODE(ind,var) if (ttymodelist[ind].set) var = ttymodelist[ind].value;
		    if (override_tty_modes) {
			/* sysv-specific */
			TMODE (XTTYMODE_intr, tio.c_cc[VINTR]);
			TMODE (XTTYMODE_quit, tio.c_cc[VQUIT]);
			TMODE (XTTYMODE_erase, tio.c_cc[VERASE]);
			TMODE (XTTYMODE_kill, tio.c_cc[VKILL]);
			TMODE (XTTYMODE_eof, tio.c_cc[VEOF]);
			TMODE (XTTYMODE_eol, tio.c_cc[VEOL]);
#ifdef VSWTCH
			TMODE (XTTYMODE_swtch, d_tio.c_cc[VSWTCH]);
#endif
#ifdef TIOCSLTC
			/* both SYSV and BSD have ltchars */
			TMODE (XTTYMODE_susp, ltc.t_suspc);
			TMODE (XTTYMODE_dsusp, ltc.t_dsuspc);
			TMODE (XTTYMODE_rprnt, ltc.t_rprntc);
			TMODE (XTTYMODE_flush, ltc.t_flushc);
			TMODE (XTTYMODE_weras, ltc.t_werasc);
			TMODE (XTTYMODE_lnext, ltc.t_lnextc);
#endif
		    }
#undef TMODE

		    if (ioctl (tty, TCSETA, &tio) == -1)
			    HsSysError(cp_pipe[1], ERROR_TIOCSETP);
#ifdef TIOCSLTC
		    if (ioctl (tty, TIOCSLTC, &ltc) == -1)
			    HsSysError(cp_pipe[1], ERROR_TIOCSETC);
#endif	/* TIOCSLTC */
#ifdef TIOCLSET
		    if (ioctl (tty, TIOCLSET, (char *)&lmode) == -1)
			    HsSysError(cp_pipe[1], ERROR_TIOCLSET);
#endif	/* TIOCLSET */
#else	/* USE_SYSV_TERMIO */
		    sg.sg_flags &= ~(ALLDELAY | XTABS | CBREAK | RAW);
		    sg.sg_flags |= ECHO | CRMOD;
		    /* make sure speed is set on pty so that editors work right*/
		    sg.sg_ispeed = B9600;
		    sg.sg_ospeed = B9600;
		    /* reset t_brkc to default value */
		    tc.t_brkc = -1;
#ifdef sony
		    if (screen->input_eight_bits)
			lmode |= LPASS8;
		    else
			lmode &= ~(LPASS8);
		    jmode &= ~KM_KANJI;
#endif /* sony */

#define TMODE(ind,var) if (ttymodelist[ind].set) var = ttymodelist[ind].value;
		    if (override_tty_modes) {
			TMODE (XTTYMODE_intr, tc.t_intrc);
			TMODE (XTTYMODE_quit, tc.t_quitc);
			TMODE (XTTYMODE_erase, sg.sg_erase);
			TMODE (XTTYMODE_kill, sg.sg_kill);
			TMODE (XTTYMODE_eof, tc.t_eofc);
			TMODE (XTTYMODE_start, tc.t_startc);
			TMODE (XTTYMODE_stop, tc.t_stopc);
			TMODE (XTTYMODE_brk, tc.t_brkc);
			/* both SYSV and BSD have ltchars */
			TMODE (XTTYMODE_susp, ltc.t_suspc);
			TMODE (XTTYMODE_dsusp, ltc.t_dsuspc);
			TMODE (XTTYMODE_rprnt, ltc.t_rprntc);
			TMODE (XTTYMODE_flush, ltc.t_flushc);
			TMODE (XTTYMODE_weras, ltc.t_werasc);
			TMODE (XTTYMODE_lnext, ltc.t_lnextc);
		    }
#undef TMODE

		    if (ioctl (tty, TIOCSETP, (char *)&sg) == -1)
			    HsSysError (cp_pipe[1], ERROR_TIOCSETP);
		    if (ioctl (tty, TIOCSETC, (char *)&tc) == -1)
			    HsSysError (cp_pipe[1], ERROR_TIOCSETC);
		    if (ioctl (tty, TIOCSETD, (char *)&discipline) == -1)
			    HsSysError (cp_pipe[1], ERROR_TIOCSETD);
		    if (ioctl (tty, TIOCSLTC, (char *)&ltc) == -1)
			    HsSysError (cp_pipe[1], ERROR_TIOCSLTC);
		    if (ioctl (tty, TIOCLSET, (char *)&lmode) == -1)
			    HsSysError (cp_pipe[1], ERROR_TIOCLSET);
#ifdef sony
		    if (ioctl (tty, TIOCKSET, (char *)&jmode) == -1)
			    HsSysError (cp_pipe[1], ERROR_TIOCKSET);
		    if (ioctl (tty, TIOCKSETC, (char *)&jtc) == -1)
			    HsSysError (cp_pipe[1], ERROR_TIOCKSETC);
#endif /* sony */
#endif	/* !USE_SYSV_TERMIO */
#ifdef TIOCCONS
		    if (Console) {
			int on = 1;
			if (ioctl (tty, TIOCCONS, (char *)&on) == -1)
			    fprintf(stderr, "%s: cannot open console\n",
				    xterm_name);
		    }
#endif	/* TIOCCONS */
		}

		signal (SIGCHLD, SIG_DFL);
#ifdef USE_SYSV_SIGHUP
		/* watch out for extra shells (I don't understand either) */
		signal (SIGHUP, SIG_DFL);
#else
		signal (SIGHUP, SIG_IGN);
#endif
		/* restore various signals to their defaults */
		signal (SIGINT, SIG_DFL);
		signal (SIGQUIT, SIG_DFL);
		signal (SIGTERM, SIG_DFL);

		/* copy the environment before Setenving */
		for (i = 0 ; environ [i] != NULL ; i++)
		    ;
		/* compute number of Setenv() calls below */
		envsize = 1;	/* (NULL terminating entry) */
		envsize += 3;	/* TERM, WINDOWID, DISPLAY */
#ifdef UTMP
		envsize += 1;   /* LOGNAME */
#endif /* UTMP */
#ifdef USE_SYSV_ENVVARS
#ifndef TIOCSWINSZ		/* window size not stored in driver? */
		envsize += 2;	/* COLUMNS, LINES */
#endif /* TIOCSWINSZ */
#ifdef UTMP
		envsize += 2;   /* HOME, SHELL */
#endif /* UTMP */
#else /* USE_SYSV_ENVVARS */
		envsize += 1;	/* TERMCAP */
#endif /* USE_SYSV_ENVVARS */
		envnew = (char **) calloc ((unsigned) i + envsize, sizeof(char *));
		bcopy((char *)environ, (char *)envnew, i * sizeof(char *));
		environ = envnew;
		Setenv ("TERM=", TermName);
		if(!TermName)
			*newtc = 0;

		sprintf (buf, "%lu", screen->TekEmu ? 
			 ((unsigned long) XtWindow (XtParent(tekWidget))) :
			 ((unsigned long) XtWindow (XtParent(term))));
		Setenv ("WINDOWID=", buf);
		/* put the display into the environment of the shell*/
		Setenv ("DISPLAY=", XDisplayString (screen->display));

		signal(SIGTERM, SIG_DFL);

		/* this is the time to go and set up stdin, out, and err
		 */
		{
#if defined(CRAY) && (OSMAJORVERSION >= 6)
		    (void) close(tty);
		    (void) close(0);

		    if (open ("/dev/tty", O_RDWR)) {
			fprintf(stderr, "cannot open /dev/tty\n");
			exit(1);
		    }
		    (void) close(1);
		    (void) close(2);
		    dup(0);
		    dup(0);
#else
		    /* dup the tty */
		    for (i = 0; i <= 2; i++)
			if (i != tty) {
			    (void) close(i);
			    (void) dup(tty);
			}

#ifndef ATT
		    /* and close the tty */
		    if (tty > 2)
			(void) close(tty);
#endif
#endif /* CRAY */
		}

#ifndef	USE_SYSV_PGRP
#ifdef TIOCSCTTY
		setsid();
		ioctl(0, TIOCSCTTY, 0);
#endif
		ioctl(0, TIOCSPGRP, (char *)&pgrp);
		setpgrp(0,0);
		close(open(ttydev, O_WRONLY, 0));
		setpgrp (0, pgrp);
#endif /* !USE_SYSV_PGRP */

#ifdef UTMP
		pw = getpwuid(screen->uid);
		if (pw && pw->pw_name)
		    Setenv ("LOGNAME=", pw->pw_name); /* for POSIX */
#ifdef USE_SYSV_UTMP
		/* Set up our utmp entry now.  We need to do it here
		** for the following reasons:
		**   - It needs to have our correct process id (for
		**     login).
		**   - If our parent was to set it after the fork(),
		**     it might make it out before we need it.
		**   - We need to do it before we go and change our
		**     user and group id's.
		*/
#ifdef CRAY
#define PTYCHARLEN 4
#else
#define PTYCHARLEN 2
#endif

		(void) setutent ();
		/* set up entry to search for */
		ptyname = ttydev;
		(void) strncpy(utmp.ut_id,ptyname + strlen(ptyname)-PTYCHARLEN,
			       sizeof (utmp.ut_id));
		utmp.ut_type = DEAD_PROCESS;

		/* position to entry in utmp file */
		(void) getutid(&utmp);

		/* set up the new entry */
		utmp.ut_type = USER_PROCESS;
#ifndef linux
		utmp.ut_exit.e_exit = 2;
#endif
		(void) strncpy(utmp.ut_user,
			       (pw && pw->pw_name) ? pw->pw_name : "????",
			       sizeof(utmp.ut_user));
		    
		(void)strncpy(utmp.ut_id, ptyname + strlen(ptyname)-PTYCHARLEN,
			sizeof(utmp.ut_id));
		(void) strncpy (utmp.ut_line,
			ptyname + strlen("/dev/"), sizeof (utmp.ut_line));

#ifdef HAS_UTMP_UT_HOST
		(void) strncpy(buf, DisplayString(screen->display),
			       sizeof(buf));
	        {
		    char *disfin = rindex(buf, ':');
		    if (disfin)
			*disfin = '\0';
		}
		(void) strncpy(utmp.ut_host, buf, sizeof(utmp.ut_host));
#endif
		(void) strncpy(utmp.ut_name, pw->pw_name, 
			       sizeof(utmp.ut_name));

		utmp.ut_pid = getpid();
		utmp.ut_time = time ((long *) 0);

		/* write out the entry */
		if (!resource.utmpInhibit)
		    (void) pututline(&utmp);
#ifdef WTMP
		if ( term->misc.login_shell &&
		     (i = open(etc_wtmp, O_WRONLY|O_APPEND)) >= 0) {
		    write(i, (char *)&utmp, sizeof(struct utmp));
		    close(i);
		}
#endif
		/* close the file */
		(void) endutent();

#else	/* USE_SYSV_UTMP */
		/* We can now get our ttyslot!  We can also set the initial
		 * UTMP entry.
		 */
		tslot = ttyslot();
		added_utmp_entry = False;
		{
			if (pw && !resource.utmpInhibit &&
			    (i = open(etc_utmp, O_WRONLY)) >= 0) {
				bzero((char *)&utmp, sizeof(struct utmp));
				(void) strncpy(utmp.ut_line,
					       ttydev + strlen("/dev/"),
					       sizeof(utmp.ut_line));
				(void) strncpy(utmp.ut_name, pw->pw_name,
					       sizeof(utmp.ut_name));
#ifdef HAS_UTMP_UT_HOST
				(void) strncpy(utmp.ut_host, 
					       XDisplayString (screen->display),
					       sizeof(utmp.ut_host));
#endif
				time(&utmp.ut_time);
				lseek(i, (long)(tslot * sizeof(struct utmp)), 0);
				write(i, (char *)&utmp, sizeof(struct utmp));
				close(i);
				added_utmp_entry = True;
#ifdef WTMP
				if (term->misc.login_shell &&
				(i = open(etc_wtmp, O_WRONLY|O_APPEND)) >= 0) {
				    int status;
				    status = write(i, (char *)&utmp,
						   sizeof(struct utmp));
				    status = close(i);
				}
#endif /* WTMP */
#ifdef LASTLOG
				if (term->misc.login_shell &&
				(i = open(etc_lastlog, O_WRONLY)) >= 0) {
				    bzero((char *)&lastlog,
					sizeof (struct lastlog));
				    (void) strncpy(lastlog.ll_line, ttydev +
					sizeof("/dev"),
					sizeof (lastlog.ll_line));
				    (void) strncpy(lastlog.ll_host, 
					  XDisplayString (screen->display),
					  sizeof (lastlog.ll_host));
				    time(&lastlog.ll_time);
				    lseek(i, (long)(screen->uid *
					sizeof (struct lastlog)), 0);
				    write(i, (char *)&lastlog,
					sizeof (struct lastlog));
				    close(i);
				}
#endif /* LASTLOG */
			} else
				tslot = -tslot;
		}

		/* Let's pass our ttyslot to our parent so that it can
		 * clean up after us.
		 */
#ifdef USE_HANDSHAKE
		handshake.tty_slot = tslot;
#endif /* USE_HANDSHAKE */
#endif /* USE_SYSV_UTMP */

#ifdef USE_HANDSHAKE
		/* Let our parent know that we set up our utmp entry
		 * so that it can clean up after us.
		 */
		handshake.status = UTMP_ADDED;
		handshake.error = 0;
		strcpy(handshake.buffer, ttydev);
		(void)write(cp_pipe[1], (char *)&handshake, sizeof(handshake));
#endif /* USE_HANDSHAKE */
#endif/* UTMP */

		(void) setgid (screen->gid);
#ifdef HAS_BSD_GROUPS
		if (geteuid() == 0 && pw)
		  initgroups (pw->pw_name, pw->pw_gid);
#endif
		(void) setuid (screen->uid);

#ifdef USE_HANDSHAKE
		/* mark the pipes as close on exec */
		fcntl(cp_pipe[1], F_SETFD, 1);
		fcntl(pc_pipe[0], F_SETFD, 1);

		/* We are at the point where we are going to
		 * exec our shell (or whatever).  Let our parent
		 * know we arrived safely.
		 */
		handshake.status = PTY_GOOD;
		handshake.error = 0;
		(void)strcpy(handshake.buffer, ttydev);
		(void)write(cp_pipe[1], (char *)&handshake, sizeof(handshake));

		if (waiting_for_initial_map) {
		    i = read (pc_pipe[0], (char *) &handshake,
			      sizeof(handshake));
		    if (i != sizeof(handshake) ||
			handshake.status != PTY_EXEC) {
			/* some very bad problem occurred */
			exit (ERROR_PTY_EXEC);
		    }
		    if(handshake.rows > 0 && handshake.cols > 0) {
			screen->max_row = handshake.rows;
			screen->max_col = handshake.cols;
#ifdef sun
#ifdef TIOCSSIZE
			ts.ts_lines = screen->max_row + 1;
			ts.ts_cols = screen->max_col + 1;
#endif /* TIOCSSIZE */
#else /* !sun */
#ifdef TIOCSWINSZ
			ws.ws_row = screen->max_row + 1;
			ws.ws_col = screen->max_col + 1;
			ws.ws_xpixel = FullWidth(screen);
			ws.ws_ypixel = FullHeight(screen);
#endif /* TIOCSWINSZ */
#endif /* sun else !sun */
		    }
		}
#endif /* USE_HANDSHAKE */

#ifdef USE_SYSV_ENVVARS
#ifndef TIOCSWINSZ		/* window size not stored in driver? */
		sprintf (numbuf, "%d", screen->max_col + 1);
		Setenv("COLUMNS=", numbuf);
		sprintf (numbuf, "%d", screen->max_row + 1);
		Setenv("LINES=", numbuf);
#endif /* TIOCSWINSZ */
#ifdef UTMP
		if (pw) {	/* SVR4 doesn't provide these */
		    if (!getenv("HOME"))
			Setenv("HOME=", pw->pw_dir);
		    if (!getenv("SHELL"))
			Setenv("SHELL=", pw->pw_shell);
		}
#endif /* UTMP */
#else /* USE_SYSV_ENVVAR */
		if(!screen->TekEmu) {
		    strcpy (termcap, newtc);
		    resize (screen, TermName, termcap, newtc);
		}
		if (term->misc.titeInhibit) {
		    remove_termcap_entry (newtc, ":ti=");
		    remove_termcap_entry (newtc, ":te=");
		}
		/*
		 * work around broken termcap entries */
		if (resource.useInsertMode)	{
		    remove_termcap_entry (newtc, ":ic=");
		    /* don't get duplicates */
		    remove_termcap_entry (newtc, ":im=");
		    remove_termcap_entry (newtc, ":ei=");
		    remove_termcap_entry (newtc, ":mi");
		    strcat (newtc, ":im=\\E[4h:ei=\\E[4l:mi:");
		}
		Setenv ("TERMCAP=", newtc);
#endif /* USE_SYSV_ENVVAR */


		/* need to reset after all the ioctl bashing we did above */
#ifdef sun
#ifdef TIOCSSIZE
		ioctl  (0, TIOCSSIZE, &ts);
#endif	/* TIOCSSIZE */
#else	/* not sun */
#ifdef TIOCSWINSZ
		ioctl (0, TIOCSWINSZ, (char *)&ws);
#endif	/* TIOCSWINSZ */
#endif	/* sun */

		signal(SIGHUP, SIG_DFL);
		if (command_to_exec) {
			execvp(*command_to_exec, command_to_exec);
			/* print error message on screen */
			fprintf(stderr, "%s: Can't execvp %s\n", xterm_name,
			 *command_to_exec);
		} 

#ifdef USE_SYSV_SIGHUP
		/* fix pts sh hanging around */
		signal (SIGHUP, SIG_DFL);
#endif

#ifdef UTMP
		if(((ptr = getenv("SHELL")) == NULL || *ptr == 0) &&
		 ((pw == NULL && (pw = getpwuid(screen->uid)) == NULL) ||
		 *(ptr = pw->pw_shell) == 0))
#else	/* UTMP */
		if(((ptr = getenv("SHELL")) == NULL || *ptr == 0) &&
		 ((pw = getpwuid(screen->uid)) == NULL ||
		 *(ptr = pw->pw_shell) == 0))
#endif	/* UTMP */
			ptr = "/bin/sh";
		if(shname = rindex(ptr, '/'))
			shname++;
		else
			shname = ptr;
		shname_minus = malloc(strlen(shname) + 2);
		(void) strcpy(shname_minus, "-");
		(void) strcat(shname_minus, shname);
#ifndef USE_SYSV_TERMIO
		ldisc = XStrCmp("csh", shname + strlen(shname) - 3) == 0 ?
		 NTTYDISC : 0;
		ioctl(0, TIOCSETD, (char *)&ldisc);
#endif	/* !USE_SYSV_TERMIO */

#ifdef USE_LOGIN_DASH_P
		if (term->misc.login_shell && pw && added_utmp_entry)
		  execl (bin_login, "login", "-p", "-f", pw->pw_name, 0);
#endif
		execlp (ptr, (term->misc.login_shell ? shname_minus : shname),
			0);

		/* Exec failed. */
		fprintf (stderr, "%s: Could not exec %s!\n", xterm_name, ptr);
		(void) sleep(5);
		exit(ERROR_EXEC);
	    }				/* end if in child after fork */

#ifdef USE_HANDSHAKE
	    /* Parent process.  Let's handle handshaked requests to our
	     * child process.
	     */

	    /* close childs's sides of the pipes */
	    close (cp_pipe[1]);
	    close (pc_pipe[0]);

	    for (done = 0; !done; ) {
		if (read(cp_pipe[0], (char *) &handshake, sizeof(handshake)) <= 0) {
			/* Our child is done talking to us.  If it terminated
			 * due to an error, we will catch the death of child
			 * and clean up.
			 */
			break;
		}

		switch(handshake.status) {
		case PTY_GOOD:
			/* Success!  Let's free up resources and
			 * continue.
			 */
			done = 1;
			break;

		case PTY_BAD:
			/* The open of the pty failed!  Let's get
			 * another one.
			 */
			(void) close(screen->respond);
			if (get_pty(&screen->respond)) {
			    /* no more ptys! */
			    (void) fprintf(stderr,
			      "%s: child process can find no available ptys\n",
			      xterm_name);
			    handshake.status = PTY_NOMORE;
			    write(pc_pipe[1], (char *) &handshake, sizeof(handshake));
			    exit (ERROR_PTYS);
			}
			handshake.status = PTY_NEW;
			(void) strcpy(handshake.buffer, ttydev);
			write(pc_pipe[1], (char *) &handshake, sizeof(handshake));
			break;

		case PTY_FATALERROR:
			errno = handshake.error;
			close(cp_pipe[0]);
			close(pc_pipe[1]);
			SysError(handshake.fatal_error);

		case UTMP_ADDED:
			/* The utmp entry was set by our slave.  Remember
			 * this so that we can reset it later.
			 */
			added_utmp_entry = True;
#ifndef	USE_SYSV_UTMP
			tslot = handshake.tty_slot;
#endif	/* USE_SYSV_UTMP */
			free(ttydev);
			ttydev = malloc((unsigned) strlen(handshake.buffer) + 1);
			strcpy(ttydev, handshake.buffer);
			break;
		default:
			fprintf(stderr, "%s: unexpected handshake status %d\n",
			        xterm_name, handshake.status);
		}
	    }
	    /* close our sides of the pipes */
	    if (!waiting_for_initial_map) {
		close (cp_pipe[0]);
		close (pc_pipe[1]);
	    }
#endif /* USE_HANDSHAKE */
	}				/* end if no slave */

	/*
	 * still in parent (xterm process)
	 */

#ifdef USE_SYSV_SIGHUP
	/* hung sh problem? */
	signal (SIGHUP, SIG_DFL);
#else
	signal (SIGHUP,SIG_IGN);
#endif

/*
 * Unfortunately, System V seems to have trouble divorcing the child process
 * from the process group of xterm.  This is a problem because hitting the 
 * INTR or QUIT characters on the keyboard will cause xterm to go away if we
 * don't ignore the signals.  This is annoying.
 */

#if defined(USE_SYSV_SIGNALS) && !defined(SIGTSTP)
	signal (SIGINT, SIG_IGN);

#ifndef SYSV
	/* hung shell problem */
	signal (SIGQUIT, SIG_IGN);
#endif
	signal (SIGTERM, SIG_IGN);
#else /* else is bsd or has job control */
#ifdef SYSV
	/* if we were spawned by a jobcontrol smart shell (like ksh or csh),
	 * then our pgrp and pid will be the same.  If we were spawned by
	 * a jobcontrol dump shell (like /bin/sh), then we will be in out
	 * parents pgrp, and we must ignore keyboard signals, or will will
	 * tank on everything.
	 */
	if (getpid() == getpgrp()) {
	    (void) signal(SIGINT, Exit);
	    (void) signal(SIGQUIT, Exit);
	    (void) signal(SIGTERM, Exit);
	} else {
	    (void) signal(SIGINT, SIG_IGN);
	    (void) signal(SIGQUIT, SIG_IGN);
	    (void) signal(SIGTERM, SIG_IGN);
	}
	(void) signal(SIGPIPE, Exit);
#else	/* SYSV */
	signal (SIGINT, Exit);
	signal (SIGQUIT, Exit);
	signal (SIGTERM, Exit);
        signal (SIGPIPE, Exit);
#endif	/* SYSV */
#endif /* USE_SYSV_SIGNALS and not SIGTSTP */

	return;
}							/* end spawn */

SIGNAL_T
Exit(n)
	int n;
{
	register TScreen *screen = &term->screen;
        int pty = term->screen.respond;  /* file descriptor of pty */
#ifdef UTMP
#ifdef USE_SYSV_UTMP
	struct utmp utmp;
	struct utmp *utptr;
	char *ptyname;
#ifdef WTMP
	int fd;			/* for /etc/wtmp */
	int i;
#endif
	/* cleanup the utmp entry we forged earlier */
	if (!resource.utmpInhibit
#ifdef USE_HANDSHAKE		/* without handshake, no way to know */
	    && added_utmp_entry
#endif /* USE_HANDSHAKE */
	    ) {
	    ptyname = ttydev;
	    utmp.ut_type = USER_PROCESS;
	    (void) strncpy(utmp.ut_id, ptyname + strlen(ptyname) - PTYCHARLEN,
			   sizeof(utmp.ut_id));
	    (void) setutent();
	    utptr = getutid(&utmp);
	    /* write it out only if it exists, and the pid's match */
	    if (utptr && (utptr->ut_pid == screen->pid)) {
		    utptr->ut_type = DEAD_PROCESS;
		    utptr->ut_time = time((long *) 0);
		    (void) pututline(utptr);
#ifdef WTMP
		    /* set wtmp entry if wtmp file exists */
		    if ((fd = open(etc_wtmp, O_WRONLY | O_APPEND)) >= 0) {
		      i = write(fd, utptr, sizeof(utmp));
		      i = close(fd);
		    }
#endif

	    }
	    (void) endutent();
	}
#else	/* not USE_SYSV_UTMP */
	register int wfd;
	register int i;
	struct utmp utmp;

	if (!resource.utmpInhibit && added_utmp_entry &&
	    (!am_slave && tslot > 0 && (wfd = open(etc_utmp, O_WRONLY)) >= 0)){
		bzero((char *)&utmp, sizeof(struct utmp));
		lseek(wfd, (long)(tslot * sizeof(struct utmp)), 0);
		write(wfd, (char *)&utmp, sizeof(struct utmp));
		close(wfd);
#ifdef WTMP
		if (term->misc.login_shell &&
		    (wfd = open(etc_wtmp, O_WRONLY | O_APPEND)) >= 0) {
			(void) strncpy(utmp.ut_line, ttydev +
			    sizeof("/dev"), sizeof (utmp.ut_line));
			time(&utmp.ut_time);
			i = write(wfd, (char *)&utmp, sizeof(struct utmp));
			i = close(wfd);
		}
#endif /* WTMP */
	}
#endif	/* USE_SYSV_UTMP */
#endif	/* UTMP */
        close(pty); /* close explicitly to avoid race with slave side */
	if(screen->logging)
		CloseLog(screen);

	if (!am_slave) {
		/* restore ownership of tty and pty */
		chown (ttydev, 0, 0);
#ifndef sgi
		chown (ptydev, 0, 0);
#endif /* !sgi */

		/* restore modes of tty and pty */
		chmod (ttydev, 0666);
#ifndef sgi
		chmod (ptydev, 0666);
#endif /* !sgi */
	}
	exit(n);
	SIGNAL_RETURN;
}

/* ARGSUSED */
resize(screen, TermName, oldtc, newtc)
TScreen *screen;
char *TermName;
register char *oldtc, *newtc;
{
#ifndef USE_SYSV_ENVVARS
	register char *ptr1, *ptr2;
	register int i;
	register int li_first = 0;
	register char *temp;

	if ((ptr1 = strindex (oldtc, "co#")) == NULL){
		strcat (oldtc, "co#80:");
		ptr1 = strindex (oldtc, "co#");
	}
	if ((ptr2 = strindex (oldtc, "li#")) == NULL){
		strcat (oldtc, "li#24:");
		ptr2 = strindex (oldtc, "li#");
	}
	if(ptr1 > ptr2) {
		li_first++;
		temp = ptr1;
		ptr1 = ptr2;
		ptr2 = temp;
	}
	ptr1 += 3;
	ptr2 += 3;
	strncpy (newtc, oldtc, i = ptr1 - oldtc);
	newtc += i;
	sprintf (newtc, "%d", li_first ? screen->max_row + 1 :
	 screen->max_col + 1);
	newtc += strlen(newtc);
	ptr1 = index (ptr1, ':');
	strncpy (newtc, ptr1, i = ptr2 - ptr1);
	newtc += i;
	sprintf (newtc, "%d", li_first ? screen->max_col + 1 :
	 screen->max_row + 1);
	ptr2 = index (ptr2, ':');
	strcat (newtc, ptr2);
#endif /* USE_SYSV_ENVVARS */
}

/* ARGSUSED */
static SIGNAL_T reapchild (n)
    int n;
{
#ifdef USE_POSIX_WAIT
        pid_t pid;

	pid = waitpid(-1, NULL, WNOHANG);
	if (pid <= 0) {
#ifdef USE_SYSV_SIGNALS
		(void) signal(SIGCHLD, reapchild);
#endif /* USE_SYSV_SIGNALS */
		SIGNAL_RETURN;
	}
#else /* USE_POSIX_WAIT */
#if defined(USE_SYSV_SIGNALS) && (defined(CRAY) || !defined(SIGTSTP))
	int status, pid;

	pid = wait(&status);
	if (pid == -1) {
		(void) signal(SIGCHLD, reapchild);
		SIGNAL_RETURN;
	}
#else	/* defined(USE_SYSV_SIGNALS) && (defined(CRAY) || !defined(SIGTSTP)) */
	union wait status;
	register int pid;
	
#ifdef DEBUG
	if (debug) fputs ("Exiting\n", stderr);
#endif	/* DEBUG */
	pid  = wait3 (&status, WNOHANG, (struct rusage *)NULL);
	if (!pid) {
#ifdef USE_SYSV_SIGNALS
		(void) signal(SIGCHLD, reapchild);
#endif /* USE_SYSV_SIGNALS */
		SIGNAL_RETURN;
	}
#endif /* defined(USE_SYSV_SIGNALS) && !defined(SIGTSTP) */
#endif /* USE_POSIX_WAIT else */

#ifdef PUCC_PTYD
		closepty(ttydev, ptydev, (resource.utmpInhibit ?  OPTY_NOP : OPTY_LOGIN), Ptyfd);
#endif /* PUCC_PTYD */

	if (pid != term->screen.pid) {
#ifdef USE_SYSV_SIGNALS
		(void) signal(SIGCHLD, reapchild);
#endif	/* USE_SYSV_SIGNALS */
		SIGNAL_RETURN;
	}
	
	/*
	 * Use pid instead of process group (which would have to get before
	 * the wait call above) so that we don't accidentally hose other
	 * applications.  Otherwise, somebody could write a program which put
	 * itself in somebody else's process group.  Also, we call Exit instead
	 * of Cleanup so that we don't do a killpg on -1 by accident.  Some
	 * operating systems seem to do very nasty things with that.
	 */
	if (pid > 1) {
	    kill_process_group (pid, SIGHUP);
	}
	Exit (0);
	SIGNAL_RETURN;
}

/* VARARGS1 */
consolepr(fmt,x0,x1,x2,x3,x4,x5,x6,x7,x8,x9)
char *fmt;
{
	extern int errno;
	extern char *SysErrorMsg();
	int oerrno;
	int f;
 	char buf[ BUFSIZ ];

	oerrno = errno;
 	strcpy(buf, "xterm: ");
 	sprintf(buf+strlen(buf), fmt, x0,x1,x2,x3,x4,x5,x6,x7,x8,x9);
 	strcat(buf, ": ");
 	strcat(buf, SysErrorMsg (oerrno));
 	strcat(buf, "\n");	
	f = open("/dev/console",O_WRONLY);
	write(f, buf, strlen(buf));
	close(f);
#ifdef TIOCNOTTY
	if ((f = open("/dev/tty", 2)) >= 0) {
		ioctl(f, TIOCNOTTY, (char *)NULL);
		close(f);
	}
#endif	/* TIOCNOTTY */
}


remove_termcap_entry (buf, str)
    char *buf;
    char *str;
{
    register char *strinbuf;

    strinbuf = strindex (buf, str);
    if (strinbuf) {
        register char *colonPtr = index (strinbuf+1, ':');
        if (colonPtr) {
            while (*colonPtr) {
                *strinbuf++ = *colonPtr++;      /* copy down */
            }
            *strinbuf = '\0';
        } else {
            strinbuf[1] = '\0';
        }
    }
    return;
}

/*
 * parse_tty_modes accepts lines of the following form:
 *
 *         [SETTING] ...
 *
 * where setting consists of the words in the modelist followed by a character
 * or ^char.
 */
static int parse_tty_modes (s, modelist)
    char *s;
    struct _xttymodes *modelist;
{
    struct _xttymodes *mp;
    int c;
    int count = 0;

    while (1) {
	while (*s && isascii(*s) && isspace(*s)) s++;
	if (!*s) return count;

	for (mp = modelist; mp->name; mp++) {
	    if (strncmp (s, mp->name, mp->len) == 0) break;
	}
	if (!mp->name) return -1;

	s += mp->len;
	while (*s && isascii(*s) && isspace(*s)) s++;
	if (!*s) return -1;

	if (*s == '^') {
	    s++;
	    c = ((*s == '?') ? 0177 : *s & 31);	 /* keep control bits */
	} else {
	    c = *s;
	}
	mp->value = c;
	mp->set = 1;
	count++;
	s++;
    }
}


int GetBytesAvailable (fd)
    int fd;
{
#ifdef FIONREAD
    static long arg;
    ioctl (fd, FIONREAD, (char *) &arg);
    return (int) arg;
#else
    struct pollfd pollfds[1];

    pollfds[0].fd = fd;
    pollfds[0].events = POLLIN;
    return poll (pollfds, 1, 0);
#endif
}

/* Utility function to try to hide system differences from
   everybody who used to call killpg() */

int
kill_process_group(pid, sig)
    int pid;
    int sig;
{
#ifndef X_NOT_POSIX
    return kill (-pid, sig);
#else
#if defined(SVR4) || defined(SYSV)
    return kill (-pid, sig);
#else
    return killpg (pid, sig);
#endif
#endif
}
