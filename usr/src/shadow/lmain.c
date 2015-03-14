/*
 * Copyright 1989, 1990, 1991, 1992, John F. Haugh II
 * All rights reserved.
 *
 * Permission is granted to copy and create derivative works for any
 * non-commercial purpose, provided this copyright notice is preserved
 * in all copies of source code, or included in human readable form
 * and conspicuously displayed on all copies of object code or
 * distribution media.
 *
 * This software is provided on an AS-IS basis and the author makes
 * no warrantee of any kind.
 */

#include "config.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include "pwd.h"
#ifdef SVR4
#include <utmpx.h>
#else
#include <utmp.h>
#endif
#include <time.h>
#include <signal.h>
#ifndef	BSD
#include <string.h>
#include <memory.h>
#else
#include <strings.h>
#define	strchr	index
#define	strrchr	rindex
#endif
#ifndef	BSD
#include <termio.h>
#else
#include <sgtty.h>
#endif
#ifdef	STDLIB_H
#include <stdlib.h>
#endif
#ifdef	UNISTD_H
#include <unistd.h>
#endif

#include "lastlog.h"
#include "faillog.h"
#ifdef	SHADOWPWD
#include "shadow.h"
#endif
#include "pwauth.h"

#if !defined(BSD) && !defined(SUN)
#define	bzero(a,n)	memset(a, 0, n);
#endif

#ifdef	USE_SYSLOG
#include <syslog.h>

#ifndef	LOG_WARN
#define	LOG_WARN	LOG_WARNING
#endif
#endif

#ifndef	lint
static	char	sccsid[] = "@(#)lmain.c	3.21	13:40:20	7/31/92";
#endif

					/* danger - side effects */
#define STRFCPY(A,B)	strncpy((A), (B), sizeof(A)), *((A)+sizeof(A)-1) = '\0'

#if defined(RLOGIN) || defined(UT_HOST)
char	host[BUFSIZ];
char	term[128] = "TERM=";
#endif

struct	passwd	pwent;
#ifdef SVR4
struct	utmpx	utent, failent;
#else
struct	utmp	utent, failent;
#endif
struct	lastlog	lastlog;
int	pflg;
int	rflg;
int	fflg;
#ifdef	RLOGIN
int	hflg;
#endif
int	preauth_flag;
#if defined(USG) || defined(SUN4)
struct	termio	termio;
#endif

#ifndef	MAXENV
#define	MAXENV	64
#endif

/*
 * Global variables.
 */

char	*newenvp[MAXENV];
char	*Prog;
int	newenvc = 0;
int	maxenv = MAXENV;

/*
 * External identifiers.
 */

extern	char	*getenv ();
extern	char	*getpass ();
extern	char	*tz ();
extern	void	checkutmp ();
extern	void	addenv ();
extern	void	setenv ();
extern	unsigned alarm ();
extern	void	login ();
extern	void	setutmp ();
extern	void	subsystem ();
extern	void	log ();
extern	void	setup ();
extern	int	expire ();
extern	void	motd ();
extern	void	mailcheck ();
extern	void	shell ();
extern	long	a64l ();
extern	int	c64i ();
extern	char	*getdef_str();
extern	int	getdef_bool();
extern	int	getdef_num();
extern	long	getdef_long();
extern	int	optind;
extern	char	*optarg;
extern	char	**environ;
extern	int	pw_auth();

#ifdef HAVE_ULIMIT
extern	long	ulimit();
#endif

#ifndef	ALARM
#define	ALARM	60
#endif

#ifndef	RETRIES
#define	RETRIES	3
#endif

struct	faillog	faillog;

#define	NO_SHADOW	"no shadow password for `%s' on `%s'\n"
#define	BAD_PASSWD_HOST	"invalid password for `%s' on `%s' from `%s'\n"
#define	BAD_PASSWD	"invalid password for `%s' on `%s'\n"
#define	BAD_DIALUP	"invalid dialup password for `%s' on `%s'\n"
#define	BAD_TIME_HOST	"invalid login time for `%s' on `%s' from `%s'\n"
#define	BAD_TIME	"invalid login time for `%s' on `%s'\n"
#define	BAD_ROOT_LOGIN	"ILLEGAL ROOT LOGIN ON TTY `%s'\n"
#define	ROOT_LOGIN	"ROOT LOGIN ON TTY `%s'\n"
#define	FAILURE_CNT	"exceeded failure limit for `%s' on `%s'\n"
#define	NOT_A_TTY	"not a tty\n"
#define	NOT_ROOT	"-r or -f flag and not ROOT on `%s'\n"
#define AUTHFAIL	"authentication failed for user `%s'\n"

/*
 * usage - print login command usage and exit
 *
 * login [ name ]
 * login -r hostname	(for rlogind)
 * login -h hostname	(for telnetd, etc.)
 * login -f name	(for pre-authenticated login: datakit, xterm, etc.)
 */

void
usage ()
{
	fprintf (stderr, "usage: login [ -p ] [ name ]\n");
#ifdef	RLOGIN
	fprintf (stderr, "       login [ -p ] -r name\n");
	fprintf (stderr, "       login [ -p ] [ -f name ] -h host\n");
#else
	fprintf (stderr, "       login [ -p ] -f name\n");
#endif	/* RLOGIN */
	exit (1);
}

#ifdef	RLOGIN
rlogin (remote_host, name, namelen)
char	*remote_host;
char	*name;
int	namelen;
{
	struct	passwd	*pwd;
	char	remote_name[32];
	char	*cp;

	get_remote_string (remote_name, sizeof remote_name);
	get_remote_string (name, namelen);
	get_remote_string (term + 5, sizeof term - 5);
	if (cp = strchr (term, '/'))
		*cp = '\0';

#if defined(USG) || defined(SUN4)
	(void) ioctl (0, TCGETA, &termio);

	termio.c_iflag |= ICRNL|IXON;
	termio.c_oflag |= OPOST|ONLCR;
	termio.c_lflag |= ICANON|ECHO|ECHOE;
	(void) ioctl (0, TCSETA, &termio);
#endif
	if (! (pwd = getpwnam (name)))
		return 0;

	/*
	 * ruserok() returns 0 for success on modern systems, and 1 on
	 * older ones.  If you are having trouble with people logging
	 * in without giving a required password, THIS is the culprit -
	 * go fix the #define in config.h.
	 */

	return ruserok (remote_host, pwd->pw_uid == 0,
				remote_name, name) == RUSEROK;
}

get_remote_string (buf, size)
char	*buf;
int	size;
{
	for (;;) {
		if (read (0, buf, 1) != 1)
  			exit (1);
		if (*buf == '\0')
			return;
		if (--size > 0)
			++buf;
	}
	/*NOTREACHED*/
}
#endif

/*
 * login - create a new login session for a user
 *
 *	login is typically called by getty as the second step of a
 *	new user session.  getty is responsible for setting the line
 *	characteristics to a reasonable set of values and getting
 *	the name of the user to be logged in.  login may also be
 *	called to create a new user session on a pty for a variety
 *	of reasons, such as X servers or network logins.
 *
 *	the flags which login supports are
 *	
 *	-p - preserve the environment
 *	-r - perform autologin protocol for rlogin
 *	-f - do not perform authentication, user is preauthenticated
 *	-h - the name of the remote host
 */

int
main (argc, argv, envp)
int	argc;
char	**argv;
char	**envp;
{
	char	name[32];
	char	pass[32];
	char	tty[BUFSIZ];
	int	retries;
	int	failed;
	int	flag;
	int	subroot = 0;
	char	*fname;
	char	*cp;
	char	*tmp;
	char	buff[128];
	struct	passwd	*pwd;
#ifdef	SHADOWPWD
	struct	spwd	*spwd;
	struct	spwd	*getspnam();
#endif

	/*
	 * Some quick initialization.
	 */

	name[0] = '\0';

	/*
	 * Get the utmp file entry and get the tty name from it.  The
	 * current process ID must match the process ID in the utmp
	 * file if there are no additional flags on the command line.
	 */

	checkutmp (argc == 1 || argv[1][0] != '-');
	STRFCPY (tty, utent.ut_line);

	if (Prog = strrchr (argv[0], '/'))
		Prog++;
	else
		Prog = argv[0];

#ifdef	RLOGIN
	while ((flag = getopt (argc, argv, "pr:f:h:")) != EOF)
#else
	while ((flag = getopt (argc, argv, "pf:")) != EOF)
#endif
	{
		switch (flag) {
			case 'p': pflg++;
				break;
			case 'f':
				fflg++;
				preauth_flag++;
				STRFCPY (name, optarg);
				break;
#ifdef	RLOGIN
			case 'r':
				rflg++;
				STRFCPY (host, optarg);
#ifdef	UT_HOST
				STRFCPY (utent.ut_host, optarg);
#endif	/*UT_HOST*/
				if (rlogin (host, name, sizeof name))
					preauth_flag++;

				break;
			case 'h':
				hflg++;
				STRFCPY (host, optarg);
#ifdef	UT_HOST
				STRFCPY (utent.ut_host, optarg);
#endif	/*UT_HOST*/
				break;
#endif	/*RLOGIN*/
			default:
				usage ();
		}
	}

#ifdef	RLOGIN
	/*
	 * Neither -h nor -f should be combined with -r.
	 */

	if (rflg && (hflg || fflg))
		usage ();
#endif

	/*
	 * Allow authentication bypass only if real UID is zero.
	 */

	if ((rflg || fflg) && getuid () != 0) {
		fprintf(stderr, "%s: permission denied\n", Prog);
		exit (1);
	}

	if (! isatty (0) || ! isatty (1) || ! isatty (2))
		exit (1);		/* must be a terminal */

#ifdef	USE_SYSLOG
	openlog (Prog, LOG_PID|LOG_CONS|LOG_NOWAIT, LOG_AUTH);
#endif

#ifndef	BSD
	(void) ioctl (0, TCGETA, &termio); /* get terminal characteristics */

	/*
	 * Add your favorite terminal modes here ...
	 */

	termio.c_lflag |= ISIG;

	termio.c_cc[VERASE] = getdef_num("ERASECHAR", '\b');
	termio.c_cc[VKILL] = getdef_num("KILLCHAR", '\025');
#ifdef SVR4
	/*
	 * ttymon invocation prefers this, but these settings won't come into
	 * effect after the first username login 
	 */

	(void) ioctl (0, TCSETA, &termio);
#else
 	(void) ioctl (0, TCSETAF, &termio);
#endif
#endif	/* !BSD */
	umask (getdef_num("UMASK", 0));
#ifdef HAVE_ULIMIT
	{
		/* 
		 * Use the ULIMIT in the login.defs file, and if
		 * there isn't one, use the default value.  The
		 * user may have one for themselves, but otherwise,
		 * just take what you get.
		 */

		long limit = getdef_long("ULIMIT", -1L);

		if (limit != -1)
			ulimit (2, limit);
	}
#endif

	/*
	 * The entire environment will be preserved if the -p flag
	 * is used.
	 */

	if (pflg)
		while (*envp)		/* add inherited environment, */
			addenv (*envp++); /* some variables change later */

#ifdef	RLOGIN
	if (term[5] != '\0')		/* see if anything after "TERM=" */
		addenv (term);
#endif
	/*
	 * Add the timezone environmental variable so that time functions
	 * work correctly.
	 */

	if (tmp = getenv ("TZ")) {
		strcat (strcpy (buff, "TZ="), tmp);
		addenv (buff);
	} else if (cp = getdef_str ("ENV_TZ"))
		addenv (*cp == '/' ? tz (cp):cp);

	/* 
	 * Add the clock frequency so that profiling commands work
	 * correctly.
	 */

	if (tmp = getenv("HZ")) {
		strcat (strcpy (buff, "HZ="), tmp);
		addenv (buff);
	} else if (cp = getdef_str("ENV_HZ"))
		addenv (cp);

	if (optind < argc) {		/* get the user name */
		if (rflg || fflg)
			usage ();

#ifdef SVR4
		/*
		 * The "-h" option can't be used with a command-line username,
		 * because telnetd invokes us as: login -h host TERM=...
		 */

		if (! hflg) {
			STRFCPY (name, argv[optind]);
			++optind;
		}
#else
		STRFCPY (name, argv[optind]);
		++optind;
#endif
	}
#ifdef SVR4
	/*
	 * check whether ttymon has done the prompt for us already
	 */

	{
	    char *ttymon_prompt;

	    if ((ttymon_prompt = getenv("TTYPROMPT")) != NULL &&
		    (*ttymon_prompt != 0)) {
		login(name, 0);	/* read name, without prompt */
	    }
	}
#endif /* SVR4 */
	if (optind < argc)		/* now set command line variables */
		    setenv (argc - optind, &argv[optind]);

top:
	(void) alarm (ALARM);		/* only allow ALARM sec. for login */

	environ = newenvp;		/* make new environment active */
	retries = RETRIES;
	while (1) {	/* repeatedly get login/password pairs */
		pass[0] = '\0';

		if (! name[0]) {	/* need to get a login id */
			if (subroot) {
#ifdef	USE_SYSLOG
				closelog ();
#endif
				exit (1);
			}
#ifdef	RLOGIN
			preauth_flag = 0;
#endif
			login (name, "login: ");
			continue;
		}
		if (! (pwd = getpwnam (name)))
			pwent.pw_name = (char *) 0;
		else
			pwent = *pwd;

		if (pwent.pw_name) {
#ifdef	SHADOWPWD
			if (! (spwd = getspnam (name)))
#ifdef	USE_SYSLOG
				syslog (LOG_WARN, NO_SHADOW, name, tty);
#else
				;
#endif
			else
				pwent.pw_passwd = spwd->sp_pwdp;
#endif	/* SHADOWPWD */
			failed = 0;	/* hasn't failed validation yet */
		} else
			failed = 1;	/* will never pass validation */

#ifdef	RLOGIN
		/*
		 * The -r and -f flags provide a name which has already
		 * been authenticated by some server.
		 */

		if (pwent.pw_name && preauth_flag)
			goto have_name;
#endif	/*RLOGIN*/

		if (pwent.pw_name && pwent.pw_passwd[0] == '@') {
			if (pw_auth (pwent.pw_passwd + 1, name, PW_LOGIN)) {
				failed = 1;
#ifdef	USE_SYSLOG
				syslog (LOG_WARN, AUTHFAIL, name);
#endif
			}
			goto auth_done;
		}

	/*
	 * Get the user's password.  One will only be prompted for
	 * if the pw_passwd (or sp_pwdp) field is non-blank.  It
	 * will then be checked against the password entry, along
	 * with other options which prevent logins.
	 */
		cp = 0;
		if ((! pwent.pw_name || (strlen (pwent.pw_passwd) > 0))
				&& ! (cp = getpass ("Password:")))
			goto again;

		if (cp) {
			STRFCPY (pass, cp);
			bzero (cp, strlen (cp));
		}
		if (! valid (pass, &pwent)) { /* check encrypted passwords */
#ifdef	USE_SYSLOG
#ifdef UT_HOST
			if (*(utent.ut_host))
				syslog (LOG_WARN, BAD_PASSWD_HOST, name, tty,
					utent.ut_host);
			else
#endif /* UT_HOST */
				syslog (LOG_WARN, BAD_PASSWD, name, tty);
#endif /* USE_SYSLOG */
			failed = 1;
		}
		bzero (pass, sizeof pass);

		/*
		 * This is the point where password-authenticated users
		 * wind up.  If you reach this far, your password has
		 * been authenticated and so on.
		 */

auth_done:
#ifdef	RLOGIN
have_name:
#endif
		if (getdef_bool("DIALUPS_CHECK_ENAB")) {
			alarm (30);
			if (pwent.pw_name &&
					! dialcheck (tty, pwent.pw_shell[0] ?
					pwent.pw_shell:"/bin/sh")) {
#ifdef	USE_SYSLOG
				syslog (LOG_WARN, BAD_DIALUP, name, tty);
#endif
				failed = 1;
			}
		}
		if (getdef_bool("PORTTIME_CHECKS_ENAB") &&
			pwent.pw_name &&
			! isttytime (pwent.pw_name, tty, time ((time_t *) 0))
		) {
#ifdef	USE_SYSLOG
#ifdef UT_HOST
			if (*(utent.ut_host))
				syslog (LOG_WARN, BAD_TIME_HOST, name, tty,
				    utent.ut_host);
			else
#endif	/* UT_HOST */
				syslog (LOG_WARN, BAD_TIME, name, tty);
#endif	/* USE_SYSLOG */
				failed = 1;
		}
		if (! failed && pwent.pw_name && pwent.pw_uid == 0 &&
				! console (tty)) {
#ifdef	USE_SYSLOG
			syslog (LOG_CRIT, BAD_ROOT_LOGIN, tty);
#endif
			failed = 1;
		}
		if (getdef_bool("FAILLOG_ENAB") && pwent.pw_name &&
				! failcheck (pwent.pw_uid, &faillog, failed)) {
#ifdef	USE_SYSLOG
			syslog (LOG_CRIT, FAILURE_CNT, name, tty);
#endif
			failed = 1;
		}
		if (! failed)
			break;

		puts ("Login incorrect");
#ifdef	RLOGIN
		if (rflg || fflg) {
#ifdef	USE_SYSLOG
			closelog ();
#endif
			exit (1);
		}
#endif	/*RLOGIN*/

		/* don't log non-existent users */
		if (getdef_bool("FAILLOG_ENAB") && pwent.pw_name)
			failure (pwent.pw_uid, tty, &faillog);
		if (getdef_str("FTMP_FILE") != NULL) {
			failent = utent;

			if (pwent.pw_name)
				STRFCPY (failent.ut_name, pwent.pw_name);
			else
				if (getdef_bool("LOG_UNKFAIL_ENAB"))
					STRFCPY (failent.ut_name, name);
				else
					STRFCPY (failent.ut_name, "UNKNOWN");
#ifdef SVR4
			gettimeofday (&(failent.ut_tv));
#else
			time (&failent.ut_time);
#endif
#ifdef	USG_UTMP
			failent.ut_type = USER_PROCESS;
#endif
			failtmp (&failent);
		}

		if (--retries <= 0) {	/* only allow so many failures */
#ifdef	USE_SYSLOG
			closelog ();
#endif
			exit (1);
		}
again:
		bzero (name, sizeof name);
		bzero (pass, sizeof pass);

		/*
		 * Wait a while (a la SVR4 /usr/bin/login) before attempting
		 * to login the user again.  If the earlier alarm occurs
		 * before the sleep() below completes, login will exit.
		 */

		if (getdef_num ("FAIL_DELAY", 0))
			sleep (getdef_num ("FAIL_DELAY", 0));
	}
	(void) alarm (0);		/* turn off alarm clock */

	/*
	 * Check to see if system is turned off for non-root users.
	 * This would be useful to prevent users from logging in
	 * during system maintenance.
	 */

	fname = getdef_str("NOLOGINS_FILE");
	if (pwent.pw_uid != 0 && fname != NULL && access (fname, 0) == 0) {
		FILE	*nlfp;
		int	c;

		if (nlfp = fopen (fname, "r")) {
			while ((c = getc (nlfp)) != EOF) {
				if (c == '\n')
					putchar ('\r');

				putchar (c);
			}
			fflush (stdout);
			fclose (nlfp);
		} else
			printf ("\r\nSystem closed for routine maintenance\n");

#ifdef	USE_SYSLOG
		closelog ();
#endif
		exit (0);
	}
	if (getenv ("IFS"))		/* don't export user IFS ... */
		addenv ("IFS= \t\n");	/* ... instead, set a safe IFS */

	setutmp (name, tty);		/* make entry in utmp & wtmp files */
	if (pwent.pw_shell[0] == '*') {	/* subsystem root */
		subsystem (&pwent);	/* figure out what to execute */
		subroot++;		/* say i was here again */
		endpwent ();		/* close all of the file which were */
		endgrent ();		/* open in the original rooted file */
#ifdef	SHADOWPWD
		endspent ();		/* system.  they will be re-opened */
#endif
#ifdef	SHADOWGRP
		endsgent ();		/* in the new rooted file system */
#endif
		goto top;		/* go do all this all over again */
	}

	if (getdef_bool("LASTLOG_ENAB"))
		log ();			/* give last login and log this one */
	setup (&pwent);			/* set UID, GID, HOME, etc ... */
#ifdef	AGING
#ifdef	SHADOWPWD
	if (spwd) {			/* check for age of password */
		if (expire (&pwent, spwd)) {
			spwd = getspnam (name);
			pwd = getpwnam (name);
			pwent = *pwd;
		}
	}
#endif
#ifdef	ATT_AGE
#ifdef	SHADOWPWD
	else
#endif
	if (pwent.pw_age && pwent.pw_age[0]) {
		if (expire (&pwent, (void *) 0)) {
			pwd = getpwnam (name);
			pwent = *pwd;
		}
	}
#endif	/* ATT_AGE */
#endif	/* AGING */
	if (! hushed (&pwent)) {
		motd ();		/* print the message of the day */
		if (getdef_bool ("FAILLOG_ENAB") && faillog.fail_cnt != 0)
			failprint (&faillog);
		if (getdef_bool ("LASTLOG_ENAB") && lastlog.ll_time != 0)
			printf ("Last login: %.19s on %s\n",
				ctime (&lastlog.ll_time), lastlog.ll_line);
#ifdef	AGING
#ifdef	SHADOWPWD
		agecheck (&pwent, spwd);
#else
		agecheck (&pwent, (void *) 0);
#endif
#endif	/* AGING */
		mailcheck ();	/* report on the status of mail */
	}
	if (getdef_str("TTYTYPE_FILE") != NULL && getenv("TERM") == NULL)
  		ttytype (tty);

	signal (SIGINT, SIG_DFL);	/* default interrupt signal */
	signal (SIGQUIT, SIG_DFL);	/* default quit signal */
	signal (SIGTERM, SIG_DFL);	/* default terminate signal */
	signal (SIGALRM, SIG_DFL);	/* default alarm signal */

	endpwent ();			/* stop access to password file */
	endgrent ();			/* stop access to group file */
#ifdef	SHADOWPWD
	endspent ();			/* stop access to shadow passwd file */
#endif
#ifdef	SHADOWGRP
	endsgent ();			/* stop access to shadow group file */
#endif
#ifdef	USE_SYSLOG
	if (pwent.pw_uid == 0)
		syslog (LOG_INFO, ROOT_LOGIN, tty);

	closelog ();
#endif
	shell (pwent.pw_shell, (char *) 0); /* exec the shell finally. */
	/*NOTREACHED*/
}
