/*
 * This file is part of the Minicom Communications Program,
 * written by Miquel van Smoorenburg 1991/1992.
 *
 * config.h  -  default configuration.
 */

/*
 * Definitions below are not hard-coded - you can change them from
 * the setup menu in minicom, or you can start minicom with the
 * "-s" flag.
 * Recommended setting for some systems are commented. Uncomment
 * and adjust them to your system.
 */

/* Which tty to use */
/*#define DFL_PORT "/dev/tty1"		/* Minix, standard. */
/*#define DFL_PORT "/dev/tty64"		/* Minix, VC */
/*#define DFL_PORT "/dev/com1l"		/* Coherent port #1 */
/*#define DFL_PORT "/dev/ttys0"		/* Linux port 0 */
#define DFL_PORT "/dev/modem"           /* my /dev/modem */
/*#define DFL_PORT "/dev/cua2p0"  	/* HPUX */

/* Default baudrate  */
#define DEF_BAUD "2400" /**/
/*#define DEF_BAUD "19200" /**/

/* Default script interpreter */
#define SCRIPTPROG "/usr/local/bin/runscript"

/* Where is kermit and how do we call it */
#define KERMIT "/usr/local/bin/kermit5A -l %l -b %b" /* linux kermit5A */

/* The "callout" program - empty if you don't have one */
#define CALLOUT "" /* */
/*#define CALLOUT "/etc/ungetty -o tty64"	/* Minix, Dial in/out + VC */
/*#define CALLOUT "/usr/local/bin/callout"	/* Our BSD system */
/*#define CALLOUT "/etc/disable com1r"		/* Coherent */

/* The "callin" program - empty if you don't have one */
#define CALLIN "" /* */
/*#define CALLIN "/etc/ungetty -i tty64"	/* Minix, Dial in/out + VC */
/*#define CALLIN "/usr/local/bin/callin"	/**/
/*#define CALLIN "/etc/enable com1r"		/* Coherent */

/* The position of the lock files - again, empty if you don't have them */
/*#define UUCPLOCK "" /* */
#define UUCPLOCK "/usr/spool/uucp/LCK..modem" /* Name matches port name for Linux */
/*#define UUCPLOCK "/usr/spool/uucp/LCK..cua2p0"	/* HPUX */
/*#define UUCPLOCK "/usr/spool/uucp/LCK..com1l"	/* Coherent */

/*
 * The next definitions are permanent ones - you can't edit the
 * configuration from within minicom to change them
 * (unless you use a binary editor, like a real hacker :-)
 */

/* Location of "keyserv" program" */
#define KEYSERV "/usr/local/etc/keyserv" /**/
/*#define KEYSERV "/user1/e88/miquels/minicom/keyserv" /**/

/* Location of parameter files */
#define PARDIR "/usr/local/etc" /**/
/*#define PARDIR "/user1/e88/miquels/minicom" /**/

/* Menu Colors (for all possible colors - look in window.h) */
#define MFG	YELLOW	/* ForeGround */
#define MBG	BLUE	/* BackGround */

/* Terminal window colors */
#define SFG	BLUE
#define SBG	CYAN

/* The next automatically defines "KEY_KLUDGE" for a Minix system;
 * it improves the algorithm for decoding escape sequences but
 * is very Minix specific (read: DIRTY)
 */

#ifdef _MINIX
#  define KEY_KLUDGE 1
#endif

/*
 * Only and only define this if you have a slow machine and find
 * the output of minicom unaccepably jerky.
 */
#ifdef _MINIX
#  define SMOOTH /* */
#endif

#if defined (_SYSV) || defined (_BSD43)
#  if !defined(linux) && !defined(_SVR2)
#    define HAS_FCHOWN
#  endif
#endif

