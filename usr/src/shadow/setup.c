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

#ifdef	SVR4
#include <utmpx.h>
#else
#include <sys/types.h>
#include <utmp.h>
#endif	/* SVR4 */

#include <stdio.h>
#include <grp.h>

#ifdef	BSD
#include <strings.h>
#define	strchr	index
#else
#include <string.h>
#include <memory.h>
#endif

#include "config.h"
#include "pwd.h"

#ifdef	USE_SYSLOG
#include <syslog.h>

#ifndef	LOG_WARN
#define	LOG_WARN	LOG_WARNING
#endif
#endif

#ifndef	lint
static	char	sccsid[] = "@(#)setup.c	3.12	13:02:46	7/27/92";
#endif

#ifndef	SU
#ifdef	SVR4
extern	struct	utmpx	utent;
#else
extern	struct	utmp	utent;
#endif	/* SVR4 */
#endif	/* !SU */

long	strtol ();
#ifdef	HAVE_ULIMIT
long	ulimit ();
#endif

void	addenv ();
extern	char	*getdef_str();
extern	int	getdef_bool();
extern	int	getdef_num();

static setallgroups (gid_t loggroup, char *member)
{
	int	i, j = 0, groups = 0;
	struct	group	*grp;
	struct	group	*getgrent();
	gid_t grlist[NGROUPS];

	setgrent ();

	while (grp = getgrent ()) {
		for (i = 0;grp->gr_mem[i];i++) {
			if (strcmp (grp->gr_mem[i], member) == 0) {
				if ((j+1) >= NGROUPS)
					fprintf (stderr, "too many groups\n");
				else
					grlist[j++] = grp->gr_gid;
			}
		}
	}
	grlist[j++] = loggroup;
	if (setgroups (j, grlist)) {
		perror ("unable to set groups");
	}
}

/*
 * setup - initialize login environment
 *
 *	setup() performs the following steps -
 *
 *	set the login tty to be owned by the new user ID with TTYPERM modes
 *	change to the user's home directory
 *	set the process nice, ulimit, and umask from the password file entry
 *	set the group ID to the value from the password file entry
 *	set the supplementary group IDs
 *	set the user ID to the value from the password file entry
 *	set the HOME, SHELL, MAIL, PATH, and LOGNAME or USER environmental
 *	variables.
 */

void	setup (info)
struct	passwd	*info;
{
	extern	int	errno;
	char	buf[BUFSIZ];
#ifndef	SU
	char	tty[sizeof utent.ut_line + 8];
#endif
	char	*cp;
	char	*group;		/* TTY group name or number */
	char	*maildir;	/* the directory in which the mailbox resides */
	char	*mailfile;	/* the name of the mailbox */
	struct	group	*grent;
	int	i;
	long	l;

#ifndef	SU
	if (utent.ut_line[0] != '/')
		(void) strcat (strcpy (tty, "/dev/"), utent.ut_line);
	else
		(void) strcpy (tty, utent.ut_line);

	if (! (group = getdef_str ("TTYGROUP")))
		i = info->pw_gid;
	else if (group[0] >= '0' && group[0] <= '9')
		i = atoi (group);
	else if (grent = getgrnam (group))
		i = grent->gr_gid;
	else
		i = info->pw_gid;

	if (chown (tty, info->pw_uid, i) ||
			chmod (tty, getdef_num("TTYPERM", 0622))) {
		(void) sprintf (buf, "Unable to change tty %s", tty);
#ifdef	USE_SYSLOG
		syslog (LOG_WARN, "unable to change tty `%s' for user `%s'\n",
			tty, info->pw_name);
		closelog ();
#endif
		perror (buf);
		exit (errno);
	}
#endif
	if (chdir (info->pw_dir) == -1) {
		(void) sprintf (buf, "Unable to cd to \"%s\"", info->pw_dir);
#ifdef	USE_SYSLOG
		syslog (LOG_WARN, "unable to cd to `%s' for user `%s'\n",
			info->pw_dir, info->pw_name);
		closelog ();
#endif
		perror (buf);
		exit (errno);
	}
	if ( getdef_bool("QUOTAS_ENAB") ) {
		for (cp = info->pw_gecos ; cp != NULL ; cp = strchr (cp, ',')) {
			if (*cp == ',')
				cp++;

			if (strncmp (cp, "pri=", 4) == 0) {
				i = atoi (cp + 4);
				if (i >= -20 && i <= 20)
					(void) nice (i);

				continue;
			}
#ifdef	HAVE_ULIMIT
			if (strncmp (cp, "ulimit=", 7) == 0) {
				l = strtol (cp + 7, (char **) 0, 10);
				(void) ulimit (2, l);

				continue;
			}
#endif
			if (strncmp (cp, "umask=", 6) == 0) {
				i = strtol (cp + 6, (char **) 0, 8) & 0777;
				(void) umask (i);

				continue;
			}
		}
	}
	if (setgid (info->pw_gid) == -1) {
		puts ("Bad group id");
#ifdef	USE_SYSLOG
		syslog (LOG_WARN, "bad group ID `%d' for user `%s'\n",
			info->pw_gid, info->pw_name);
		closelog ();
#endif
		exit (errno);
	}
#if NGROUPS > 1
	if(initgroups(info->pw_name,info->pw_gid) == -1) {
		puts ("initgroups failure");
#ifdef	USE_SYSLOG
	setallgroups(info->pw_gid, info->pw_name);
		syslog (LOG_WARN, "initgroups failed for user `%s'\n",
			info->pw_name);
		closelog ();
#endif
		exit (errno);
	}
#endif /* NGROUPS > 1 */
#ifndef	BSD
	if (setuid (info->pw_uid))
#else
	if (setreuid (info->pw_uid, info->pw_uid))
#endif
	{
		puts ("Bad user id");
#ifdef	USE_SYSLOG
		syslog (LOG_WARN, "bad user ID `%d' for user `%s'\n",
			info->pw_uid, info->pw_name);
		closelog ();
#endif
		exit (errno);
	}
	(void) strcat (strcpy (buf, "HOME="), info->pw_dir);
	addenv (buf);

	if (info->pw_shell == (char *) 0 || ! *info->pw_shell)
		info->pw_shell = "/bin/sh";

	(void) strcat (strcpy (buf, "SHELL="), info->pw_shell);
	addenv (buf);

	cp = getdef_str( info->pw_uid == 0 ? "ENV_SUPATH" : "ENV_PATH" );
	addenv( cp != NULL ? cp : "PATH=/bin:/usr/bin" );

#if defined(BSD) || defined(SUN) || defined(SUN4)
	(void) strcat (strcpy (buf, "USER="), info->pw_name);
#else
	(void) strcat (strcpy (buf, "LOGNAME="), info->pw_name);
#endif /* BSD || SUN || SUN4 */
	addenv (buf);

	if ( (cp=getdef_str("MAIL_DIR")) != NULL ) {
		maildir = cp;
		mailfile = info->pw_name;
	} else if ( (cp=getdef_str("MAIL_FILE")) != NULL) {
		maildir = info->pw_dir;
		mailfile = cp;
	} else {
		maildir = "/usr/spool/mail";
		mailfile = info->pw_name;
	}
		
	(void) strcat (strcat (strcat (strcpy (buf,
		"MAIL="), maildir), "/"), mailfile);
	addenv (buf);
}
