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

#ifdef SVR4
#include <utmpx.h>
#else
#include <sys/types.h>
#include <utmp.h>
#endif
#include "pwd.h"
#include <fcntl.h>
#include <time.h>
#ifndef	BSD
#include <string.h>
#include <memory.h>
#else
#include <strings.h>
#define	strchr	index
#define	strrchr	rindex
#endif
#include "config.h"

#ifndef	lint
static	char	sccsid[] = "@(#)log.c	3.4	13:02:34	7/27/92";
#endif

#include "lastlog.h"

#ifndef	LASTLOG_FILE
#ifdef	SVR4
#define	LASTLOG_FILE	"/var/adm/lastlog"
#else
#define	LASTLOG_FILE	"/usr/adm/lastlog"
#endif	/* SVR4 */
#endif	/* LASTLOG_FILE */

#ifdef SVR4
extern	struct	utmpx	utent;
#else
extern	struct	utmp	utent;
#endif
extern	struct	passwd	pwent;
extern	struct	lastlog	lastlog;
extern	char	**environ;

long	lseek ();
time_t	time ();

void	log ()
{
	int	fd;
	off_t	offset;
	struct	lastlog	newlog;

	if ((fd = open (LASTLOG_FILE, O_RDWR)) == -1)
		return;

	offset = pwent.pw_uid * sizeof lastlog;

	if (lseek (fd, offset, 0) != offset) {
		(void) close (fd);
		return;
	}
	if (read (fd, (char *) &lastlog, sizeof lastlog) != sizeof lastlog)
#ifndef	BSD
		memset ((char *) &lastlog, sizeof lastlog, 0);
#else
		bzero ((char *) &lastlog, sizeof lastlog);
#endif
	newlog = lastlog;

	(void) time (&newlog.ll_time);
	(void) strncpy (newlog.ll_line, utent.ut_line, sizeof newlog.ll_line);
	(void) lseek (fd, offset, 0);
	(void) write (fd, (char *) &newlog, sizeof newlog);
	(void) close (fd);
}

