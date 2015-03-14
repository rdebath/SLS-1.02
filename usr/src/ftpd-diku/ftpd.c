/*
 * Copyright (c) 1985, 1988, 1990 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1985, 1988, 1990 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)ftpd.c	5.37 (Berkeley) 6/27/90";
#endif /* not lint */

/*
 * FTP server.
 */
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/wait.h>
#include <sys/dir.h>
#include <sys/utsname.h>
#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>

#define	FTP_NAMES
/* #include <arpa/ftp.h> */
#include "support/ftp.h"
#include <arpa/inet.h>
#include <arpa/telnet.h>

#include <ctype.h>
#include <stdio.h>
#include <signal.h>
#include <pwd.h>
#ifdef SHADOW_PWD
#	include <shadow.h>
#endif
#include <setjmp.h>
#include <netdb.h>
#include <malloc.h>
#include <errno.h>
#include <string.h>
#include <syslog.h>
#include <time.h>
#include <varargs.h>
#include "pathnames.h"
#include "extensions.h"

/*
 * File containing login names
 * NOT to be used on this machine.
 * Commonly used to disallow uucp.
 */
extern	int errno;
extern char *ctime();
#ifndef SHADOW_PWD
extern	char *crypt();
#endif
char version[256];
extern	char *home;		/* pointer to home directory for glob */
extern	FILE *ftpd_popen(), *fopen(), *freopen();
extern	int  ftpd_pclose(), fclose();
extern	char *getline(), *realpath();
extern	char cbuf[];
extern	off_t restart_point;

struct	sockaddr_in ctrl_addr;
struct	sockaddr_in data_source;
struct	sockaddr_in data_dest;
struct	sockaddr_in his_addr;
struct	sockaddr_in pasv_addr;

int	data;
jmp_buf	errcatch, urgcatch;
int	logged_in;
struct	passwd *pw;
int	debug;
int	timeout = 900;	  /* timeout after 15 minutes of inactivity */
int	maxtimeout = 7200;/* don't allow idle time to be set beyond 2 hours */
int	logging;
int	cmdlogging;
int	anonymous;
int	guest;
int	type;
int	form;
int	stru;			/* avoid C keyword */
int	mode;
int	usedefault = 1;		/* for data transfers */
int	pdata = -1;		/* for passive mode */
int	transflag;
off_t	file_size;
off_t	byte_count;
#if !defined(CMASK) || CMASK == 0
#undef CMASK
#define CMASK 027
#endif
int	defumask = CMASK;		/* default umask value */
char	tmpline[7];
char	hostname[MAXHOSTNAMELEN];
char	remotehost[MAXHOSTNAMELEN];
char	remoteaddr[MAXHOSTNAMELEN];

/* Access control and logging passwords */
int		use_accessfile = 1;
char	guestpw[MAXHOSTNAMELEN];
char	privatepw[MAXHOSTNAMELEN];
int		nameserved;
extern	char	authuser[];
extern	int		authenticated;
int	global_userno;

/* File transfer logging */
int		xferlog = 0;
int		log_outbound_xfers = 0;
int		log_incoming_xfers = 0;

/* Allow use of lreply(); this is here since some older FTP clients don't
 * support continuation messages.  In violation of the RFCs... */
int		dolreplies = 1;

/* Spontaneous reply text.  To be sent along with next reply to user */
char	*autospout = NULL;
int		autospout_free = 0;

/* allowed on-the-fly file manipulations (compress, tar) */
int		mangleopts = 0;

/* number of login failures before attempts are logged and FTP *EXITS* */
int		lgi_failure_threshold = 5;

/*
 * Timeout intervals for retrying connections
 * to hosts that don't accept PORT cmds.  This
 * is a kludge, but given the problems with TCP...
 */
#define	SWAITMAX	90	/* wait at most 90 seconds */
#define	SWAITINT	5	/* interval between retries */

int	swaitmax = SWAITMAX;
int	swaitint = SWAITINT;

int	lostconn();
int	myoob();
FILE	*getdatasock(), *dataconn();

#ifdef SETPROCTITLE
char	**Argv = NULL;		/* pointer to argument vector */
char	*LastArgv = NULL;	/* end of argv */
char	proctitle[BUFSIZ];	/* initial part of title */
#endif /* SETPROCTITLE */

main(argc, argv, envp)
	int argc;
	char *argv[];
	char **envp;
{
	int addrlen, on = 1, tos;
	char *cp;

	struct utsname u;
	uname(&u);
	sprintf(version,"%s %s %s %s %s",u.sysname,u.nodename,u.release,u.version,u.machine);

	addrlen = sizeof (his_addr);
	if (getpeername(0, (struct sockaddr *)&his_addr, &addrlen) < 0) {
		syslog(LOG_ERR, "getpeername (%s): %m",argv[0]);
#ifndef	DEBUG
		exit(1);
#endif
	}
	addrlen = sizeof (ctrl_addr);
	if (getsockname(0, (struct sockaddr *)&ctrl_addr, &addrlen) < 0) {
		syslog(LOG_ERR, "getsockname (%s): %m",argv[0]);
#ifndef	DEBUG
		exit(1);
#endif
	}
#ifdef IP_TOS
	tos = IPTOS_LOWDELAY;
	if (setsockopt(0, IPPROTO_IP, IP_TOS, (char *)&tos, sizeof(int)) < 0)
		syslog(LOG_WARNING, "setsockopt (IP_TOS): %m");
#endif
	data_source.sin_port = htons(ntohs(ctrl_addr.sin_port) - 1);
	debug = 0;
#ifdef	LOG_DAEMON
	openlog("ftpd", LOG_PID, LOG_DAEMON);
#else
	openlog("ftpd", LOG_PID);
#endif
#ifdef SETPROCTITLE
	/*
	 *  Save start and extent of argv for setproctitle.
	 */
	Argv = argv;
	while (*envp)
		envp++;
	LastArgv = envp[-1] + strlen(envp[-1]);
#endif /* SETPROCTITLE */

	argc--, argv++;
	while (argc > 0 && *argv[0] == '-') {
		for (cp = &argv[0][1]; *cp; cp++) switch (*cp) {

		case 'a':
			use_accessfile = 1;
			break;

		case 'A':
			use_accessfile = 0;
			break;

		case 'v':
			debug = 1;
			break;

		case 'd':
			debug = 1;
			break;

		case 'l':
			logging = 1;
			break;

		case 'L':
			cmdlogging = 1;
			break;

		case 'i':
			log_incoming_xfers = 1;
			break;

		case 'o':
			log_outbound_xfers = 1;
			break;

		case 't':
			timeout = atoi(++cp);
			if (maxtimeout < timeout)
				maxtimeout = timeout;
			goto nextopt;

		case 'T':
			maxtimeout = atoi(++cp);
			if (timeout > maxtimeout)
				timeout = maxtimeout;
			goto nextopt;

		case 'u':
			{
			int val = 0;

			while (*++cp && *cp >= '0' && *cp <= '9')
				val = val*8 + *cp - '0';
			if (*cp)
				fprintf(stderr, "ftpd: Bad value for -u\n");
			else
				defumask = val;
			goto nextopt;
			}

		default:
			fprintf(stderr, "ftpd: Unknown flag -%c ignored.\n",
				 *cp);
			break;
		}
nextopt:
		argc--, argv++;
	}
#ifndef DEBUG
	(void) freopen(_PATH_DEVNULL, "w", stderr);
#else
	(void) freopen("/tmp/DEBUG", "w", stderr);
#endif

	(void) signal(SIGCHLD, SIG_IGN);
	if ((int)signal(SIGURG, myoob) < 0)
		syslog(LOG_ERR, "signal: %m");

	/* Try to handle urgent data inline */
#ifdef SO_OOBINLINE
	if (setsockopt(0, SOL_SOCKET, SO_OOBINLINE, (char *)&on, sizeof(on)) < 0)
		syslog(LOG_ERR, "setsockopt: %m");
#endif

#ifdef	F_SETOWN
	if (fcntl(fileno(stdin), F_SETOWN, getpid()) == -1)
		syslog(LOG_ERR, "fcntl F_SETOWN: %m");
#endif
	dolog(&his_addr);
	/*
	 * Set up default state
	 */
	data = -1;
	type = TYPE_A;
	form = FORM_N;
	stru = STRU_F;
	mode = MODE_S;
	tmpline[0] = '\0';
	(void) gethostname(hostname, sizeof (hostname));

	access_init();
	authenticate();

	if (is_shutdown(1) != 0) {
		syslog(LOG_INFO, "connection refused (server shut down) from %s [%s]",
			remotehost, remoteaddr);
		reply(500, "%s FTP server shut down -- please try again later.",
			hostname);
		exit(0);
	}

	show_banner(220);
	(void) signal(SIGPIPE, lostconn);
	reply(220, "%s FTP server (%s) ready.", hostname, version);
	(void) setjmp(errcatch);

	for (;;)
		(void) yyparse();
	/* NOTREACHED */
}

lostconn()
{

	if (debug)
		syslog(LOG_DEBUG, "lost connection to %s [%s]", remotehost, remoteaddr);
	dologout(-1);
}

static char ttyline[20];

/*
 * Helper function for sgetpwnam().
 */
char *
sgetsave(s)
	char *s;
{
	char *new = malloc((unsigned) strlen(s) + 1);

	if (new == NULL) {
		perror_reply(421, "Local resource failure: malloc");
		dologout(1);
		/* NOTREACHED */
	}
	(void) strcpy(new, s);
	return (new);
}

/*
 * Save the result of a getpwnam.  Used for USER command, since
 * the data returned must not be clobbered by any other command
 * (e.g., globbing).
 */
struct passwd *
sgetpwnam(name)
	char *name;
{
	static struct passwd save;
	register struct passwd *p;
	char *sgetsave();

	if ((p = getpwnam(name)) == NULL)
		return (p);
	if (save.pw_name) {
		free(save.pw_name);
		free(save.pw_passwd);
		free(save.pw_gecos);
		free(save.pw_dir);
		free(save.pw_shell);
	}
	save = *p;
	save.pw_name = sgetsave(p->pw_name);
	save.pw_passwd = sgetsave(p->pw_passwd);
	save.pw_gecos = sgetsave(p->pw_gecos);
	save.pw_dir = sgetsave(p->pw_dir);
	save.pw_shell = sgetsave(p->pw_shell);
	return (&save);
}

int login_attempts;		/* number of failed login attempts */
int askpasswd;			/* had user command, ask for passwd */
#ifdef PINS
int is_pin;			/* was pin user, check pin-file */
#endif /* PINS */

/*
 * USER command.
 * Sets global passwd pointer pw if named account exists and is
 * acceptable; sets askpasswd if a PASS command is expected.  If logged in
 * previously, need to reset state.  If name is "ftp" or "anonymous", the
 * name is not in _PATH_FTPUSERS, and ftp account exists, set anonymous
 * and pw, then just return.  If account doesn't exist, ask for passwd
 * anyway.  Otherwise, check user requesting login privileges.  Disallow
 * anyone who does not have a standard shell as returned by
 * getusershell().  Disallow anyone mentioned in the file _PATH_FTPUSERS
 * to allow people such as root and uucp to be avoided.
 */
user(name)
	char *name;
{
	register char *cp;
	char *shell;
	char *getusershell();

	if (logged_in) {
		if (anonymous || guest) {
			reply(530, "Can't change user from guest login.");
			return;
		}
		end_login();
	}

	anonymous = 0;

#ifdef PINS
	is_pin == 0;

	if (!strcasecmp(name, "ftp") || !strcasecmp(name, "anonymous") ||
		!strcasecmp(name, "pin") ) {
		if (checkuser("ftp") || checkuser("anonymous") || checkuser("pin")) {
#else /* ! PINS */
	if (!strcasecmp(name, "ftp") || !strcasecmp(name, "anonymous")) {
		if (checkuser("ftp") || checkuser("anonymous")) {
#endif /* ! PINS */
			reply(530, "User %s access denied.", name);
			syslog(LOG_NOTICE,
				"FTP LOGIN REFUSED (ftp in /etc/ftpusers) FROM %s [%s], %s",
				remotehost, remoteaddr, name);
#ifdef PINS
		} else if (strcasecmp(name,"pin") && (pw = sgetpwnam("ftp")) != NULL) {
#else /* ! PINS */
		} else if ((pw = sgetpwnam("ftp")) != NULL) {
#endif /* ! PINS */
			anonymous = 1; /* for the access_ok call */
			if (access_ok(530)) {
				askpasswd = 1;
				acl_setfunctions(anonymous);
				reply(331, "Guest login ok, send e-mail address as password.");
			} else {
				reply(530, "User %s access denied.", name);
				syslog(LOG_NOTICE,
					"FTP LOGIN REFUSED (access denied) FROM %s [%s], %s",
					remotehost, remoteaddr, name);
				anonymous = 0;
			}
#ifdef PINS
		} else if ((pw = sgetpwnam("pin")) != NULL) {
			anonymous = 1; /* for the access_ok call */
			if (access_ok(530)) {
				askpasswd = 1;
				is_pin = 1;
				acl_setfunctions(anonymous);
				reply(332, "PIN login ok, send PIN as password.");
			} else {
				reply(530, "User %s access denied.", name);
				syslog(LOG_NOTICE,
					"PIN LOGIN REFUSED (access denied) FROM %s [%s], %s",
					remotehost, remoteaddr, name);
				anonymous = 0;
			}
#endif /* PINS */
		} else {
			reply(530, "User %s unknown.", name);
			syslog(LOG_NOTICE,
				"FTP LOGIN REFUSED (ftp not in /etc/passwd) FROM %s [%s], %s",
				remotehost, remoteaddr, name);
		}
		return;
	}

	if ((pw = sgetpwnam(name)) != NULL) {
		if ((shell = pw->pw_shell) == NULL || *shell == 0)
			shell = _PATH_BSHELL;
		while ((cp = getusershell()) != NULL)
			if (strcmp(cp, shell) == 0)
				break;
		endusershell();
		if (cp == NULL || checkuser(name)) {
			reply(530, "User %s access denied.", name);
			if (logging)
				syslog(LOG_NOTICE,
					"FTP LOGIN REFUSED (bad shell) FROM %s [%s], %s",
					remotehost, remoteaddr, name);
			pw = (struct passwd *) NULL;
			return;
		}
	}

	/* if user is a member of any of the guestgroups, cause a chroot() */
	guest = acl_guestgroup(pw);

	if (!access_ok(530)) {
		reply(530, "User %s access denied.", name);
		syslog(LOG_NOTICE, "FTP LOGIN REFUSED (access denied) FROM %s [%s], %s",
			remotehost, remoteaddr, name);
		return;
    } else acl_setfunctions(anonymous);

	reply(331, "Password required for %s.", name);
	askpasswd = 1;
	/*
	 * Delay before reading passwd after first failed
	 * attempt to slow down passwd-guessing programs.
	 */
	if (login_attempts)
		sleep((unsigned) login_attempts);
}

/*
 * Check if a user is in the file _PATH_FTPUSERS
 */
checkuser(name)
	char *name;
{
	register FILE *fd;
	register char *p;
	char line[BUFSIZ];

	if ((fd = fopen(_PATH_FTPUSERS, "r")) != NULL) {
		while (fgets(line, sizeof(line), fd) != NULL)
			if ((p = strchr(line, '\n')) != NULL) {
				*p = '\0';
				if (line[0] == '#')
					continue;
				if (strcmp(line, name) == 0)
					return (1);
			}
		(void) fclose(fd);
	}
	return (0);
}

/*
 * Terminate login as previous user, if any, resetting state;
 * used when USER command is given or login fails.
 */
end_login()
{

	(void) seteuid((uid_t)0);
	if (logged_in)
		logwtmp(ttyline, "", "");
	pw = NULL;
	logged_in = 0;
	anonymous = 0;
	guest = 0;
}

pass(passwd)
	char *passwd;
{
	char *xpasswd, *salt;

	if (logged_in || askpasswd == 0) {
		reply(503, "Login with USER first.");
		return;
	}
	askpasswd = 0;

	/* Disable lreply() if the first character of the password is '-' since
	 * some hosts don't understand continuation messages and hang...
	 */

	if (*passwd == '-') dolreplies = 0; else dolreplies = 1;

	if (!anonymous) {		/* "ftp" is only account allowed no password */
		if (*passwd == '-') passwd++;
		*guestpw = NULL;
		if (pw == NULL)
			salt = "xx";
		else
			salt = pw->pw_passwd;
		xpasswd = crypt(passwd, salt);
		/* The strcmp does not catch null passwords! */
		if (pw == NULL || *pw->pw_passwd == '\0' ||
			strcmp(xpasswd, pw->pw_passwd)) {
			reply(530, "Login incorrect.");
			pw = NULL;
			if (++login_attempts >= lgi_failure_threshold) {
				syslog(LOG_NOTICE, "repeated login failures from %s [%s]",
					remotehost, remoteaddr);
				exit(0);
			}
			return;
		}
#ifdef PINS
	} else if (is_pin) {
		long int pos;
		int found_flg = 0;
	    FILE *pinf, *packf;		/* fd for pinfile and package file */
		char buf[MAXPATHLEN * 2];		/* to read pin and pack file */
		char newpath[MAXPATHLEN];		/* gets copied into pw->pw_dir */
		char pin[MAXPATHLEN];			/* pin, this size is oversized */
		char package[MAXPATHLEN];		/* package length */

		if (passwd == NULL || passwd == '\0') {
			reply(530, "Login incorrect.");
			pw = NULL;
			if (++login_attempts >= lgi_failure_threshold) {
				syslog(LOG_NOTICE, "repeated pin-login failures from %s [%s]",
					remotehost, remoteaddr);
				exit(0);
			}
			return;
		}
		/* check pin, a long task */
		/* open pinfile */
		if ((pinf=fopen(_PATH_PINFILE,"r+")) == NULL) {
			reply(530, "PIN user: can't access pin file.");
			pw = NULL;
			syslog(LOG_NOTICE, "Can't access PIN file, from %s [%s]",
				remotehost, remoteaddr);
			return;
		}

		/* lock pinfile */
		while (flock(fileno(pinf), LOCK_EX)) {
			syslog(LOG_ERR, "sleeping: flock of pin file failed: %s",
				strerror(errno));
			sleep(1);
		}

		/* find pin in pin-file */
		pos=ftell(pinf);
		if (fgets(buf,MAXPATHLEN*2,pinf) == NULL) {
			reply(530, "PIN user: can't read pin file.");
			pw = NULL;
			syslog(LOG_NOTICE, "Can't read PIN file, from %s [%}",
				remotehost, remoteaddr);
			flock(pinf,LOCK_UN);
			fclose(pinf);
			return;
		}

		do {
			if (buf[0] == '#') { pos=ftell(pinf); continue; }
			if (sscanf(buf,"%s%s",pin,package) < 2) {
				pos=ftell(pinf); 
				continue; }
			if (strncmp(pin,passwd,8) == 0) {	/* found his pin ! */
				found_flg= 1;
				break; }
			pos=ftell(pinf);
		} while (fgets(buf,MAXPATHLEN*2,pinf) != NULL);

		/* pin not found */
		if (found_flg == 0) {
			flock(pinf,LOCK_UN);
			fclose(pinf);
			reply(530, "PIN incorrect.");
			pw = NULL;
			if (++login_attempts >= lgi_failure_threshold) {
				syslog(LOG_NOTICE, "repeated pin-login failures from %s [%s]",
					remotehost, remoteaddr);
				exit(0);
			}
			return;
		}

		/* first we check for package, then we'll invalidate the pin */
		if ((packf=fopen(_PATH_PACKAGES,"r")) == NULL) {
			reply(530, "PIN user: can't access config file.");
			pw = NULL;
			syslog(LOG_NOTICE, "Can't access packages file, from %s [%}",
				remotehost, remoteaddr);
			flock(pinf,LOCK_UN);
			fclose(pinf);
			return;
		}

		/* search for package */
		while(fgets(buf,MAXPATHLEN*2,packf) != NULL) {
			if (buf[0] == '#') { continue; }
			if (sscanf(buf,"%s%s",pin,newpath) < 2) continue;
			if (strcmp(pin,package) == 0) { break; }
		}

		if (feof(packf) != 0) {		/* no package found */
			reply(530, "PIN user: %s is invalid package.",package);
			pw = NULL;
			syslog(LOG_NOTICE, "No package found, from %s [%s]",
				remotehost, remoteaddr);
			flock(pinf,LOCK_UN);
			fclose(pinf);
			fclose(packf);
			return;
		}

		fclose(packf);

		/* we have now validated the pin and found a directory for it */

		/* trying to invalidate this pin */
		if (fseek(pinf,pos,SEEK_SET) != 0) {
			pw = NULL;
			syslog(LOG_NOTICE, "Seek failed: %s, from %s [%s]",
				strerror(errno), remotehost, remoteaddr);
			reply(530, "PIN user: can't invalidate pin.");
			flock(pinf,LOCK_UN);
			fclose(pinf);
			return;
		}
		
		if (fputc('#',pinf) == EOF) {
			pw = NULL;
			syslog(LOG_NOTICE, "fputc failed: %s, from %s [%s]",
				strerror(errno), remotehost, remoteaddr);
			reply(530, "PIN user: can't invalidate pin.");
			flock(pinf,LOCK_UN);
			fclose(pinf);
			return;
		}

		flock(pinf,LOCK_UN);
		if (fclose(pinf) == EOF) {
			pw = NULL;
			syslog(LOG_NOTICE, "fclose failed: %s, from %s [%s]",
				strerror(errno), remotehost, remoteaddr);
			reply(530, "PIN user: can't invalidate pin.");
			return;
		}

		free(pw->pw_name);		/* we'll put package at this place */
		free(pw->pw_dir);		/* we'll put newpath at this place */

		pw->pw_name = sgetsave(package);
		pw->pw_dir = sgetsave(newpath);

#endif PINS
	} else {
		char *pwin, *pwout = guestpw;
		if (!*passwd) {
			strcpy(guestpw, "[none_given]");
		} else {
			int	cnt = sizeof(guestpw) - 2;
			for (pwin = passwd; *pwin && cnt--; pwin++)
				if (!isgraph(*pwin))
					*pwout++ = '_';
				else
					*pwout++ = *pwin;
		}
	}

	/* if autogroup command applies to user's class change pw->pw_gid */
	if (anonymous)
		(void) acl_autogroup(pw, anonymous);

	login_attempts = 0;		/* this time successful */
	(void) setegid((gid_t)pw->pw_gid);
	(void) initgroups(pw->pw_name, pw->pw_gid);

	/* open wtmp before chroot */
	(void) sprintf(ttyline, "ftp%d", getpid());
	logwtmp(ttyline, pw->pw_name, remotehost);
	logged_in = 1;

	/* if logging is enabled, open logfile before chroot */
	if (log_outbound_xfers || log_incoming_xfers)
		xferlog = open(_PATH_XFERLOG, O_WRONLY|O_APPEND|O_CREAT, 0660);

	if (anonymous || guest) {
		/*
		 * We MUST do a chdir() after the chroot. Otherwise
		 * the old current directory will be accessible as "."
		 * outside the new root!
		 */
		if (chroot(pw->pw_dir) < 0 || chdir("/") < 0) {
			reply(550, "Can't set guest privileges.");
			goto bad;
		}
	} else {
		if (chdir(pw->pw_dir) < 0) {
			if (chdir("/") < 0) {
				reply(530, "User %s: can't change directory to %s.",
					pw->pw_name, pw->pw_dir);
				goto bad;
			} else
				lreply(230, "No directory! Logging in with home=/");
		}
	}
	if (seteuid((uid_t)pw->pw_uid) < 0) {
		reply(550, "Can't set uid.");
		goto bad;
	}
#ifdef PINS
	if (anonymous && !is_pin) {		/* ugly comparision, ups, PI */
#else /* ! PINS */
	if (anonymous) {
#endif /* ! PINS */
		if (!strchr(passwd, '@')) {
			lreply(230,
				"Next time please use your e-mail address as your password");
			lreply(230, "        for example: %s@%s",
				authenticated ? authuser : "joe", remotehost);
		}
		show_message(230, LOGIN);
		show_readme(230, LOGIN);

		(void) is_shutdown(0); /* display any shutdown messages now */

		reply(230, "Guest login ok, access restrictions apply.");
#ifdef SETPROCTITLE
		sprintf(proctitle, "%s: anonymous/%.*s", remotehost,
			sizeof(proctitle) - sizeof(remotehost) -
			sizeof(": anonymous/"), passwd);
		setproctitle(proctitle);
#endif /* SETPROCTITLE */
		if (logging)
			syslog(LOG_INFO, "ANONYMOUS FTP LOGIN FROM %s [%s], %s",
				remotehost, remoteaddr, passwd);
#ifdef PINS
	} else if (is_pin) {
		/* really needed, these messages ? */
		show_message(230, LOGIN);
		show_readme(230, LOGIN);

		(void) is_shutdown(0); /* display any shutdown messages now */

		reply(230, "PIN login ok, access restrictions apply.");
#ifdef SETPROCTITLE
		sprintf(proctitle, "%s: pin/%.*s", remotehost,
			sizeof(proctitle) - sizeof(remotehost) -
			sizeof(": anonymous/"), pw->pw_name);
		setproctitle(proctitle);
#endif /* SETPROCTITLE */
		if (logging)
			syslog(LOG_INFO, "PIN FTP LOGIN FROM %s [%s], %s",
				remotehost, remoteaddr, pw->pw_name);
#endif PINS
	} else {
		reply(230, "User %s logged in.%s", pw->pw_name, guest ?
			"  Access restrictions apply." : "");
#ifdef SETPROCTITLE
		sprintf(proctitle, "%s: %s", remotehost, pw->pw_name);
		setproctitle(proctitle);
#endif /* SETPROCTITLE */
		if (logging)
			syslog(LOG_INFO, "FTP LOGIN FROM %s [%s], %s",
				remotehost, remoteaddr, pw->pw_name);
	}
	home = pw->pw_dir;		/* home dir for globbing */
	(void) umask(defumask);
	return;
bad:
	/* Forget all about it... */
	xferlog = 0;
	end_login();
}

char *
opt_string(options)
int	options;

{
static	char	buf[100];
char	*ptr = buf;

   if ((options & O_COMPRESS) != NULL) *ptr++ = 'C';
   if ((options & O_TAR) != NULL) *ptr++ = 'T';
   if ((options & O_UNCOMPRESS) != NULL) *ptr++ = 'U';
   if (options == 0) *ptr++ = '_';
   *ptr++ = '\0';
   return(buf);
}

retrieve(cmd, name)
	char *cmd, *name;
{
	FILE	*fin, *dout;
	struct	stat st, junk;
	int		(*closefunc)() = NULL;
	int		options = 0;
	time_t	start_time = time(NULL);
	static	char	*logname;
	char	namebuf[MAXPATHLEN];

#include "conversions.h"
	
	if (!cmd && stat(name, &st)) {
		char	fnbuf[MAXPATHLEN], *ptr;

		do {
			if (!(mangleopts & O_COMPRESS) && (cvtptr->options & O_COMPRESS))
				continue;
			if (!(mangleopts & O_UNCOMPRESS)&&(cvtptr->options & O_UNCOMPRESS))
				continue;
			if (!(mangleopts & O_TAR) && (cvtptr->options & O_TAR))
				continue;

			if (cvtptr->postfix) {
				int pfxlen = strlen(cvtptr->postfix);
				int namelen = strlen(name);

				if (namelen <= pfxlen) continue;
				(void) strcpy(fnbuf, name);
				if (strcmp(fnbuf + namelen - pfxlen, cvtptr->postfix)) continue;
				*(fnbuf + namelen - pfxlen) = (char) NULL;
				if (stat(fnbuf, &st)) continue;
			}

			if (cvtptr->stripfix) {
				(void) strcpy(fnbuf, name);
				(void) strcat(fnbuf, cvtptr->stripfix);
				if (stat(fnbuf, &st)) continue;
			}

			if (S_ISDIR(st.st_mode)) {
				if (!(cvtptr->types & T_DIR)) {
					(void) reply(550, "Cannot %s directories.", cvtptr->name);
					return;
				}
				if (cvtptr->options & O_TAR) { 
					strcpy(namebuf, fnbuf);
					strcat(namebuf, "/.notar");
					if (!stat(namebuf, &junk)) {
						(void) reply(550,
							"Sorry, you may not TAR that directory.");
						return;
					}
				}
			}

			if (S_ISREG(st.st_mode) && !(cvtptr->types & T_REG)) {
				(void) reply(550, "Cannot %s plain files.", cvtptr->name);
				return;
			}

			if (!S_ISREG(st.st_mode) && !S_ISDIR(st.st_mode)) {
				(void) reply(550, "Cannot %s special files.", cvtptr->name);
				return;
			}

			if (!(cvtptr->types & T_ASCII) && deny_badasciixfer(550,""))
			 	return;

 			logname = fnbuf;
			options |= cvtptr->options;

			strcpy(namebuf, cvtptr->external_cmd);
			if ((ptr = strchr(namebuf, ' ')) != NULL)
				*ptr = '\0';
			if (stat(namebuf, &st) != NULL) {
				syslog(LOG_ERR, "external command %s not found",
					namebuf);
				(void) reply(550,
					"Local error: conversion program not found. Cannot %s file.",
					cvtptr->name);
				return;
			}
			(void) retrieve(cvtptr->external_cmd, fnbuf);

			goto dolog;
		} while ((++cvtptr)->name);
	} else logname = (char *) NULL;

	if (cmd == 0) {
		fin = fopen(name, "r"), closefunc = fclose;
		st.st_size = 0;
	} else {
		char line[BUFSIZ];

		(void) sprintf(line, cmd, name), name = line;
		fin = ftpd_popen(line, "r"), closefunc = ftpd_pclose;
		st.st_size = -1;
		st.st_blksize = BUFSIZ;
	}
	if (fin == NULL) {
		if (errno != 0)
			perror_reply(550, name);
		return;
	}
	if (cmd == 0 &&
		(fstat(fileno(fin), &st) < 0 || (st.st_mode&S_IFMT) != S_IFREG)) {
		reply(550, "%s: not a plain file.", name);
		goto done;
	}
	if (restart_point) {
		if (type == TYPE_A) {
			register int i, n, c;

			n = restart_point;
			i = 0;
			while (i++ < n) {
				if ((c=getc(fin)) == EOF) {
					perror_reply(550, name);
					goto done;
				}
				if (c == '\n')
					i++;
			}	
		} else if (lseek(fileno(fin), restart_point, L_SET) < 0) {
			perror_reply(550, name);
			goto done;
		}
	}
	dout = dataconn(name, st.st_size, "w");
	if (dout == NULL)
		goto done;
	send_data(fin, dout, st.st_blksize);
	(void) fclose(dout);

dolog:
	if (log_outbound_xfers && xferlog && (cmd == 0)) {
		char	msg[MAXPATHLEN];
		int	xfertime = time(NULL) - start_time;
		int	curtime = time(NULL);
		int loop;

		if (!xfertime) xfertime++;
		realpath(logname ? logname : name, namebuf);
		for (loop = 0; namebuf[loop]; loop++)
			if (isspace(namebuf[loop]) || iscntrl(namebuf[loop]))
				namebuf[loop] = '_';
		sprintf(msg, "%.24s %d %s %d %s %c %s %c %c %s ftp %d %s\n",
			ctime(&start_time),
			xfertime,
			remotehost,
			byte_count,
			namebuf,
			(type == TYPE_A) ? 'a' : 'b',
			opt_string(options),
			'o',
			anonymous ? 'a' : 'r',
			anonymous ? guestpw : pw->pw_name,
			authenticated,
			authenticated ? authuser : "*"
		);
		write(xferlog, msg, strlen(msg));
	}

	data = -1;
	pdata = -1;
done:
	if (closefunc) (*closefunc)(fin);
}

store(name, mode, unique)
	char *name, *mode;
	int unique;
{
	FILE *fout, *din;
	struct stat st;
	int (*closefunc)();
	char *gunique();
	time_t	start_time = time(NULL);

	if (unique && stat(name, &st) == 0 &&
		(name = gunique(name)) == NULL)
		return;

	if (restart_point)
		mode = "r+w";
	fout = fopen(name, mode);
	closefunc = fclose;
	if (fout == NULL) {
		perror_reply(553, name);
		return;
	}
	if (restart_point) {
		if (type == TYPE_A) {
			register int i, n, c;

			n = restart_point;
			i = 0;
			while (i++ < n) {
				if ((c=getc(fout)) == EOF) {
					perror_reply(550, name);
					goto done;
				}
				if (c == '\n')
					i++;
			}	
			/*
			 * We must do this seek to "current" position
			 * because we are changing from reading to
			 * writing.
			 */
			if (fseek(fout, 0L, L_INCR) < 0) {
				perror_reply(550, name);
				goto done;
			}
		} else if (lseek(fileno(fout), restart_point, L_SET) < 0) {
			perror_reply(550, name);
			goto done;
		}
	}
	din = dataconn(name, (off_t)-1, "r");
	if (din == NULL)
		goto done;
	if (receive_data(din, fout) == 0) {
		if (unique)
			reply(226, "Transfer complete (unique file name:%s).",
				name);
		else
			reply(226, "Transfer complete.");
	}
	(void) fclose(din);

dolog:
	if (log_incoming_xfers && xferlog) {
		char	namebuf[MAXPATHLEN],
				msg[MAXPATHLEN];
		int	xfertime = time(NULL) - start_time;
		int loop;

		if (!xfertime) xfertime++;
		realpath(name, namebuf);
		for (loop = 0; namebuf[loop]; loop++)
			if (isspace(namebuf[loop]) || iscntrl(namebuf[loop]))
				namebuf[loop] = '_';
		sprintf(msg, "%.24s %d %s %d %s %c %s %c %c %s ftp %d %s\n",
			ctime(&start_time),
			xfertime,
			remotehost,
			byte_count,
			namebuf,
			(type == TYPE_A) ? 'a' : 'b',
			opt_string(0),
			'i',
			anonymous ? 'a' : 'r',
			anonymous ? guestpw : pw->pw_name,
			authenticated,
			authenticated ? authuser : "*"
		);
		write(xferlog, msg, strlen(msg));
	}

	data = -1;
	pdata = -1;
done:
	(*closefunc)(fout);
}

FILE *
getdatasock(mode)
	char *mode;
{
	int s, on = 1, tries;

	if (data >= 0)
		return (fdopen(data, mode));
	s = socket(AF_INET, SOCK_STREAM, 0);
	if (s < 0)
		return (NULL);
	(void) seteuid((uid_t)0);
	if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR,
		(char *) &on, sizeof (on)) < 0)
		goto bad;
	/* anchor socket to avoid multi-homing problems */
	data_source.sin_family = AF_INET;
	data_source.sin_addr = ctrl_addr.sin_addr;
	for (tries = 1; ; tries++) {
		if (bind(s, (struct sockaddr *)&data_source,
			sizeof (data_source)) >= 0)
			break;
		if (errno != EADDRINUSE || tries > 10)
			goto bad;
		sleep(tries);
	}
	(void) seteuid((uid_t)pw->pw_uid);
#ifdef IP_TOS
	on = IPTOS_THROUGHPUT;
	if (setsockopt(s, IPPROTO_IP, IP_TOS, (char *)&on, sizeof(int)) < 0)
		syslog(LOG_WARNING, "setsockopt (IP_TOS): %m");
#endif
	return (fdopen(s, mode));
bad:
	(void) seteuid((uid_t)pw->pw_uid);
	(void) close(s);
	return (NULL);
}

FILE *
dataconn(name, size, mode)
	char *name;
	off_t size;
	char *mode;
{
	char sizebuf[32];
	FILE *file;
	int retry = 0, tos;

	file_size = size;
	byte_count = 0;
	if (size != (off_t) -1)
		(void) sprintf (sizebuf, " (%ld bytes)", size);
	else
		(void) strcpy(sizebuf, "");
	if (pdata >= 0) {
		struct sockaddr_in from;
		int s, fromlen = sizeof(from);

		s = accept(pdata, (struct sockaddr *)&from, &fromlen);
		if (s < 0) {
			reply(425, "Can't open data connection.");
			(void) close(pdata);
			pdata = -1;
			return(NULL);
		}
		(void) close(pdata);
		pdata = s;
#ifdef IP_TOS
		tos = IPTOS_LOWDELAY;
		(void) setsockopt(s, IPPROTO_IP, IP_TOS, (char *)&tos,
			sizeof(int));
#endif
		reply(150, "Opening %s mode data connection for %s%s.",
			 type == TYPE_A ? "ASCII" : "BINARY", name, sizebuf);
		return(fdopen(pdata, mode));
	}
	if (data >= 0) {
		reply(125, "Using existing data connection for %s%s.",
			name, sizebuf);
		usedefault = 1;
		return (fdopen(data, mode));
	}
	if (usedefault)
		data_dest = his_addr;
	usedefault = 1;
	file = getdatasock(mode);
	if (file == NULL) {
		reply(425, "Can't create data socket (%s,%d): %s.",
			inet_ntoa(data_source.sin_addr),
			ntohs(data_source.sin_port), strerror(errno));
		return (NULL);
	}
	data = fileno(file);
	do{
	connect(data, (struct sockaddr *)&data_dest,sizeof (data_dest));
		if (errno == EADDRINUSE && retry < swaitmax) {
			sleep((unsigned) swaitint);
			retry += swaitint;
			continue;
		}
		break;
	} while (1);
/*	while (connect(data, (struct sockaddr *)&data_dest,
		sizeof (data_dest)) < 0) {
		if (errno == EADDRINUSE && retry < swaitmax) {
			sleep((unsigned) swaitint);
			retry += swaitint;
			continue;
		}
		perror_reply(425, "Can't build data connection");
		(void) fclose(file);
		data = -1;
		return (NULL);
	}
*/
	reply(150, "Opening %s mode data connection for %s%s.",
		 type == TYPE_A ? "ASCII" : "BINARY", name, sizebuf);
	return (file);
}

/*
 * Tranfer the contents of "instr" to
 * "outstr" peer using the appropriate
 * encapsulation of the data subject
 * to Mode, Structure, and Type.
 *
 * NB: Form isn't handled.
 */
send_data(instr, outstr, blksize)
	FILE *instr, *outstr;
	off_t blksize;
{
	register int c, cnt, outcnt, newcnt;
	register char *buf;
	int netfd, filefd;

	transflag++;
	if (setjmp(urgcatch)) {
		transflag = 0;
		return;
	}
	switch (type) {

	case TYPE_A:
		while ((c = getc(instr)) != EOF) {
			byte_count++;
			if (c == '\n') {
				if (ferror(outstr))
					goto data_err;
				(void) putc('\r', outstr);
			}
			(void) putc(c, outstr);
		}
		fflush(outstr);
		transflag = 0;
		if (ferror(instr))
			goto file_err;
		if (ferror(outstr))
			goto data_err;
		reply(226, "Transfer complete.");
		return;

	case TYPE_I:
	case TYPE_L:
		if ((buf = malloc((u_int)blksize)) == NULL) {
			transflag = 0;
			perror_reply(451, "Local resource failure: malloc");
			return;
		}
		netfd = fileno(outstr);
		filefd = fileno(instr);
/*		while ((cnt = read(filefd, buf, (u_int)blksize)) > 0 &&
			write(netfd, buf, cnt) == cnt)
			byte_count += cnt; 
*/
		while ((cnt = read(filefd, buf, (u_int)blksize)) > 0)
			{
			outcnt=newcnt=0;
			while ((outcnt=write(netfd,
					     buf+newcnt, cnt-newcnt))
							!= cnt-newcnt)
				newcnt+=outcnt;
			byte_count += cnt;				
			}
		transflag = 0;
		(void)free(buf);
		if (cnt != 0) {
			if (cnt < 0)
				goto file_err;
			goto data_err;
		}
		reply(226, "Transfer complete.");
		return;
	default:
		transflag = 0;
		reply(550, "Unimplemented TYPE %d in send_data", type);
		return;
	}

data_err:
	transflag = 0;
	perror_reply(426, "Data connection");
	return;

file_err:
	transflag = 0;
	perror_reply(551, "Error on input file");
}

/*
 * Transfer data from peer to
 * "outstr" using the appropriate
 * encapulation of the data subject
 * to Mode, Structure, and Type.
 *
 * N.B.: Form isn't handled.
 */
receive_data(instr, outstr)
	FILE *instr, *outstr;
{
	register int c;
	int cnt, bare_lfs = 0;
	char buf[BUFSIZ];

	transflag++;
	if (setjmp(urgcatch)) {
		transflag = 0;
		return (-1);
	}
	switch (type) {

	case TYPE_I:
	case TYPE_L:
		while ((cnt = read(fileno(instr), buf, sizeof buf)) > 0) {
			if (write(fileno(outstr), buf, cnt) != cnt)
				goto file_err;
			byte_count += cnt;
		}
		if (cnt < 0)
			goto data_err;
		transflag = 0;
		return (0);

	case TYPE_E:
		reply(553, "TYPE E not implemented.");
		transflag = 0;
		return (-1);

	case TYPE_A:
		while ((c = getc(instr)) != EOF) {
			byte_count++;
			if (c == '\n')
				bare_lfs++;
			while (c == '\r') {
				if (ferror(outstr))
					goto data_err;
				if ((c = getc(instr)) != '\n') {
					(void) putc ('\r', outstr);
					if (c == '\0' || c == EOF)
						goto contin2;
				}
			}
			(void) putc(c, outstr);
	contin2:	;
		}
		fflush(outstr);
		if (ferror(instr))
			goto data_err;
		if (ferror(outstr))
			goto file_err;
		transflag = 0;
		if (bare_lfs) {
			lreply(230, "WARNING! %d bare linefeeds received in ASCII mode", bare_lfs);
			printf("   File may not have transferred correctly.\r\n");
		}
		return (0);
	default:
		reply(550, "Unimplemented TYPE %d in receive_data", type);
		transflag = 0;
		return (-1);
	}

data_err:
	transflag = 0;
	perror_reply(426, "Data Connection");
	return (-1);

file_err:
	transflag = 0;
	perror_reply(452, "Error writing file");
	return (-1);
}

statfilecmd(filename)
	char *filename;
{
	char line[BUFSIZ];
	FILE *fin;
	int c;

	(void) sprintf(line, _CMD_LS_LGA_ARG, filename);
	fin = ftpd_popen(line, "r");
	lreply(211, "status of %s:", filename);
	while ((c = getc(fin)) != EOF) {
		if (c == '\n') {
			if (ferror(stdout)){
				perror_reply(421, "control connection");
				(void) ftpd_pclose(fin);
				dologout(1);
				/* NOTREACHED */
			}
			if (ferror(fin)) {
				perror_reply(551, filename);
				(void) ftpd_pclose(fin);
				return;
			}
			(void) putc('\r', stdout);
		}
		(void) putc(c, stdout);
	}
	(void) ftpd_pclose(fin);
	reply(211, "End of Status");
}

statcmd()
{
	struct sockaddr_in *sin;
	u_char *a, *p;

	lreply(211, "%s FTP server status:", hostname, version);
	printf("     %s\r\n", version);
	printf("     Connected to %s", remotehost);
	if (!isdigit(remotehost[0]))
		printf(" (%s)", inet_ntoa(his_addr.sin_addr));
	printf("\r\n");
	if (logged_in) {
		if (anonymous)
			printf("     Logged in anonymously\r\n");
		else
			printf("     Logged in as %s\r\n", pw->pw_name);
	} else if (askpasswd)
		printf("     Waiting for password\r\n");
	else
		printf("     Waiting for user name\r\n");
	printf("     TYPE: %s", typenames[type]);
	if (type == TYPE_A || type == TYPE_E)
		printf(", FORM: %s", formnames[form]);
	if (type == TYPE_L)
#if NBBY == 8
		printf(" %d", NBBY);
#else
		printf(" %d", bytesize);	/* need definition! */
#endif
	printf("; STRUcture: %s; transfer MODE: %s\r\n",
		strunames[stru], modenames[mode]);
	if (data != -1)
		printf("     Data connection open\r\n");
	else if (pdata != -1) {
		printf("     in Passive mode");
		sin = &pasv_addr;
		goto printaddr;
	} else if (usedefault == 0) {
		printf("     PORT");
		sin = &data_dest;
printaddr:
		a = (u_char *) &sin->sin_addr;
		p = (u_char *) &sin->sin_port;
#define UC(b) (((int) b) & 0xff)
		printf(" (%d,%d,%d,%d,%d,%d)\r\n", UC(a[0]),
			UC(a[1]), UC(a[2]), UC(a[3]), UC(p[0]), UC(p[1]));
#undef UC
	} else
		printf("     No data connection\r\n");
	reply(211, "End of status");
}

fatal(s)
	char *s;
{
	reply(451, "Error in server: %s\n", s);
	reply(221, "Closing connection due to server error.");
	dologout(0);
	/* NOTREACHED */
}

/* VARARGS2 */
reply(n, fmt, p0, p1, p2, p3, p4, p5)
	int n;
	char *fmt;
{
	if (autospout != NULL) {
		char *ptr = autospout;

		printf("%d-", n);
		while (*ptr) {
			if (*ptr == '\n') {
				printf("\r\n");
				if (*(++ptr))
					printf("%d-", n);
			} else {
				putchar(*ptr++);
			}
		}
		if (*(--ptr) != '\n')
			printf("\r\n");
		if (autospout_free) {
			(void) free(autospout);
			autospout_free = 0;
		}
		autospout = 0;
	}

	printf("%d ", n);
	printf(fmt, p0, p1, p2, p3, p4, p5);
	printf("\r\n");
	(void)fflush(stdout);
	if (debug) {
		syslog(LOG_DEBUG, "<--- %d ", n);
		syslog(LOG_DEBUG, fmt, p0, p1, p2, p3, p4, p5);
	}
}

/* VARARGS2 */
lreply(n, fmt, p0, p1, p2, p3, p4, p5)
	int n;
	char *fmt;
{
	if (!dolreplies) return;
	printf("%d-", n);
	printf(fmt, p0, p1, p2, p3, p4, p5);
	printf("\r\n");
	(void)fflush(stdout);
	if (debug) {
		syslog(LOG_DEBUG, "<--- %d- ", n);
		syslog(LOG_DEBUG, fmt, p0, p1, p2, p3, p4, p5);
	}
}

ack(s)
	char *s;
{
	reply(250, "%s command successful.", s);
}

nack(s)
	char *s;
{
	reply(502, "%s command not implemented.", s);
}

/* ARGSUSED */
yyerror(s)
	char *s;
{
	char *cp;

	if ((cp = strchr(cbuf,'\n')) != NULL)
		*cp = '\0';
	reply(500, "'%s': command not understood.", cbuf);
}

delete(name)
	char *name;
{
	struct stat st;

	if (stat(name, &st) < 0) {
		perror_reply(550, name);
		return;
	}
	if ((st.st_mode&S_IFMT) == S_IFDIR) {
		if (rmdir(name) < 0) {
			perror_reply(550, name);
			return;
		}
		goto done;
	}
	if (unlink(name) < 0) {
		perror_reply(550, name);
		return;
	}
done:
	ack("DELE");
}

cwd(path)
	char *path;
{
	if (chdir(path) < 0)
		perror_reply(550, path);
	else {
		show_message(250, CWD);
		show_readme(250, CWD);
		ack("CWD");
	}
}

makedir(name)
	char *name;
{
	if (mkdir(name, 0777) < 0)
		perror_reply(550, name);
	else
		reply(257, "MKD command successful.");
}

removedir(name)
	char *name;
{
	if (rmdir(name) < 0)
		perror_reply(550, name);
	else
		ack("RMD");
}

pwd()
{
	char path[MAXPATHLEN + 1];
	extern char *getwd();

	if (getwd(path) == (char *)NULL)
		reply(550, "%s.", path);
	else
		reply(257, "\"%s\" is current directory.", path);
}

char *
renamefrom(name)
	char *name;
{
	struct stat st;

	if (stat(name, &st) < 0) {
		perror_reply(550, name);
		return ((char *)0);
	}
	reply(350, "File exists, ready for destination name");
	return (name);
}

renamecmd(from, to)
	char *from, *to;
{
	if (rename(from, to) < 0)
		perror_reply(550, "rename");
	else
		ack("RNTO");
}

dolog(sin)
	struct sockaddr_in *sin;
{
	struct hostent *hp = gethostbyaddr((char *)&sin->sin_addr,
		sizeof (struct in_addr), AF_INET);

	(void) strncpy(remoteaddr, inet_ntoa(sin->sin_addr), sizeof (remoteaddr));
	if (hp) {
		nameserved = 1;
		(void) strncpy(remotehost, hp->h_name, sizeof (remotehost));
	} else {
		nameserved = 0;
		(void) strncpy(remotehost, remoteaddr, sizeof (remotehost));
	}

#ifdef SETPROCTITLE
	sprintf(proctitle, "%s: connected", remotehost);
	setproctitle(proctitle);
#endif /* SETPROCTITLE */

	if (logging) syslog(LOG_INFO, "connection from %s [%s]", remotehost,
		remoteaddr);
}

/*
 * Record logout in wtmp file
 * and exit with supplied status.
 */
dologout(status)
	int status;
{
	if (logged_in) {
		(void) seteuid((uid_t)0);
		logwtmp(ttyline, "", "");
	}
	if (xferlog) close(xferlog);
	/* beware of flushing buffers after a SIGPIPE */
	_exit(status);
}

myoob()
{
	char *cp;

	/* only process if transfer occurring */
	if (!transflag)
		return;
	cp = tmpline;
	if (getline(cp, 7, stdin) == NULL) {
		reply(221, "You could at least say goodbye.");
		dologout(0);
	}
	upper(cp);
	if (strcmp(cp, "ABOR\r\n") == 0) {
		tmpline[0] = '\0';
		reply(426, "Transfer aborted. Data connection closed.");
		reply(226, "Abort successful");
		longjmp(urgcatch, 1);
	}
	if (strcmp(cp, "STAT\r\n") == 0) {
		if (file_size != (off_t) -1)
			reply(213, "Status: %lu of %lu bytes transferred",
				byte_count, file_size);
		else
			reply(213, "Status: %lu bytes transferred", byte_count);
	}
}

/*
 * Note: a response of 425 is not mentioned as a possible response to
 * 	the PASV command in RFC959. However, it has been blessed as
 * 	a legitimate response by Jon Postel in a telephone conversation
 *	with Rick Adams on 25 Jan 89.
 */
passive()
{
	int len;
	register char *p, *a;

	pdata = socket(AF_INET, SOCK_STREAM, 0);
	if (pdata < 0) {
		perror_reply(425, "Can't open passive connection");
		return;
	}
	pasv_addr = ctrl_addr;
	pasv_addr.sin_port = 0;
	(void) seteuid((uid_t)0);
	if (bind(pdata, (struct sockaddr *)&pasv_addr, sizeof(pasv_addr)) < 0) {
		(void) seteuid((uid_t)pw->pw_uid);
		goto pasv_error;
	}
	(void) seteuid((uid_t)pw->pw_uid);
	len = sizeof(pasv_addr);
	if (getsockname(pdata, (struct sockaddr *) &pasv_addr, &len) < 0)
		goto pasv_error;
	if (listen(pdata, 1) < 0)
		goto pasv_error;
	a = (char *) &pasv_addr.sin_addr;
	p = (char *) &pasv_addr.sin_port;

#define UC(b) (((int) b) & 0xff)

	reply(227, "Entering Passive Mode (%d,%d,%d,%d,%d,%d)", UC(a[0]),
		UC(a[1]), UC(a[2]), UC(a[3]), UC(p[0]), UC(p[1]));
	return;

pasv_error:
	(void) close(pdata);
	pdata = -1;
	perror_reply(425, "Can't open passive connection");
	return;
}

/*
 * Generate unique name for file with basename "local".
 * The file named "local" is already known to exist.
 * Generates failure reply on error.
 */
char *
gunique(local)
	char *local;
{
	static char new[MAXPATHLEN];
	struct stat st;
	char *cp = strrchr(local, '/');
	int count = 0;

	if (cp)
		*cp = '\0';
	if (stat(cp ? local : ".", &st) < 0) {
		perror_reply(553, cp ? local : ".");
		return((char *) 0);
	}
	if (cp)
		*cp = '/';
	(void) strcpy(new, local);
	cp = new + strlen(new);
	*cp++ = '.';
	for (count = 1; count < 100; count++) {
		(void) sprintf(cp, "%d", count);
		if (stat(new, &st) < 0)
			return(new);
	}
	reply(452, "Unique file name cannot be created.");
	return((char *) 0);
}

/*
 * Format and send reply containing system error number.
 */
perror_reply(code, string)
	int code;
	char *string;
{
	reply(code, "%s: %s.", string, strerror(errno));
}

static char *onefile[] = {
	"",
	0
};

send_file_list(whichfiles)
	char *whichfiles;
{
	struct stat st;
	DIR *dirp = NULL;
	struct direct *dir;
	FILE *dout = NULL;
	register char **dirlist, *dirname;
	int simple = 0;
	char *strpbrk();

	if (strpbrk(whichfiles, "~{[*?") != NULL) {
		extern char **glob(), *globerr;

		globerr = NULL;
		dirlist = glob(whichfiles);
		if (globerr != NULL) {
			reply(550, globerr);
			return;
		} else if (dirlist == NULL) {
			errno = ENOENT;
			perror_reply(550, whichfiles);
			return;
		}
	} else {
		onefile[0] = whichfiles;
		dirlist = onefile;
		simple = 1;
	}

	if (setjmp(urgcatch)) {
		transflag = 0;
		return;
	}
	while ((dirname = *dirlist++) != NULL) {
		if (stat(dirname, &st) < 0) {
			/*
			 * If user typed "ls -l", etc, and the client
			 * used NLST, do what the user meant.
			 */
			if (dirname[0] == '-' && *dirlist == NULL && transflag == 0) {
				retrieve(_CMD_LS_ARG, dirname);
				return;
			}
			perror_reply(550, whichfiles);
			if (dout != NULL) {
				(void) fclose(dout);
				transflag = 0;
				data = -1;
				pdata = -1;
			}
			return;
		}
		if ((st.st_mode&S_IFMT) == S_IFREG) {
			if (dout == NULL) {
				dout = dataconn("file list", (off_t)-1, "w");
				if (dout == NULL)
					return;
				transflag++;
			}
			fprintf(dout, "%s%s\n", dirname,
				type == TYPE_A ? "\r" : "");
			byte_count += strlen(dirname) + 1;
			continue;
		} else if ((st.st_mode&S_IFMT) != S_IFDIR)
			continue;

		if ((dirp = opendir(dirname)) == NULL)
			continue;

		while ((dir = readdir(dirp)) != NULL) {
			char nbuf[MAXPATHLEN];

			if (dir->d_name[0] == '.' && dir->d_namlen == 1)
				retrieve(_CMD_LS_ARG, dirname);
				return;
			if (dir->d_namlen == 2 && dir->d_name[0] == '.' &&
				dir->d_name[1] == '.') 
				retrieve(_CMD_LS_ARG, dirname);
				continue;

			sprintf(nbuf, "%s/%s", dirname, dir->d_name);

			/*
			 * We have to do a stat to insure it's
			 * not a directory or special file.
			 */
			if (simple || (stat(nbuf, &st) == 0 &&
				(st.st_mode&S_IFMT) == S_IFREG)) {
				if (dout == NULL) {
					dout = dataconn("file list", (off_t)-1,
						"w");
					if (dout == NULL)
						return;
					transflag++;
				}
				if (nbuf[0] == '.' && nbuf[1] == '/')
					fprintf(dout, "%s%s\n", &nbuf[2],
						type == TYPE_A ? "\r" : "");
				else
					fprintf(dout, "%s%s\n", nbuf,
						type == TYPE_A ? "\r" : "");
				byte_count += strlen(nbuf) + 1;
			}
		}
		(void) closedir(dirp);
	}

	if (dout == NULL)
		reply(550, "No files found.");
	else if (ferror(dout) != 0)
		perror_reply(550, "Data connection");
	else
		reply(226, "Transfer complete.");

	transflag = 0;
	if (dout != NULL)
		(void) fclose(dout);
	data = -1;
	pdata = -1;
}

#ifdef SETPROCTITLE
/*
 * clobber argv so ps will show what we're doing.
 * (stolen from sendmail)
 * warning, since this is usually started from inetd.conf, it
 * often doesn't have much of an environment or arglist to overwrite.
 */

/*VARARGS1*/
setproctitle(fmt, a, b, c)
char *fmt;
{
	register char *p, *bp, ch;
	register int i;
	char buf[BUFSIZ];

	(void) sprintf(buf, fmt, a, b, c);

	/* make ps print our process name */
	p = Argv[0];
	*p++ = '-';

	i = strlen(buf);
	if (i > LastArgv - p - 2) {
		i = LastArgv - p - 2;
		buf[i] = '\0';
	}
	bp = buf;
	while ((ch = *bp++) != (char) NULL)
		if (ch != '\n' && ch != '\r')
			*p++ = ch;
	while (p < LastArgv)
		*p++ = ' ';
}
#endif /* SETPROCTITLE */
