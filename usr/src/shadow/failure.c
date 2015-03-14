/*
 * Copyright 1989, 1990, 1991, 1992, John F. Haugh II
 * All rights reserved.
 *
 * Permission is granted to copy and create derivative works for any
 * non-commercial purpose, provided this copyright notice is preserved
 * in all copies of source code, or included in human readable form
 * and conspicuously displayed on all copies of object code or
 * distribution media.
 */

#include <sys/types.h>
#include <fcntl.h>
#include <time.h>
#include <stdio.h>
#ifndef	BSD
#include <string.h>
#include <memory.h>
#else
#include <strings.h>
#define	strchr	index
#define	strrchr	rindex
#endif
#ifdef	UNISTD_H
#include <unistd.h>
#endif
#include "faillog.h"
#include "config.h"

#include <utmp.h>

#ifndef	lint
static	char	_sccsid[] = "@(#)failure.c	3.2	20:36:32	3/7/92";
#endif

#define	DAY	(24L*3600L)
#define	YEAR	(365L*DAY)
#define	NOW	(time ((time_t *) 0))

extern	struct	tm	*localtime ();
extern	char	*asctime ();
extern	void	failprint ();
extern	char	*getdef_str();

/*
 * failure - make failure entry
 */

void
failure (uid, tty, faillog)
int	uid;
char	*tty;
struct	faillog	*faillog;
{
	int	fd;

	if ((fd = open (FAILFILE, O_RDWR)) < 0)
		return;

	lseek (fd, (off_t) (sizeof *faillog) * uid, 0);
	if (read (fd, (char *) faillog, sizeof *faillog)
			!= sizeof *faillog)
#ifndef	BSD
		memset ((void *) faillog, 0, sizeof *faillog);
#else
		bzero ((char *) faillog, sizeof *faillog);
#endif

	if (faillog->fail_max == 0 || faillog->fail_cnt < faillog->fail_max)
		faillog->fail_cnt++;

	strncpy (faillog->fail_line, tty, sizeof faillog->fail_line);
	faillog->fail_time = time ((time_t *) 0);

	lseek (fd, (off_t) (sizeof *faillog) * uid, 0);
	write (fd, (char *) faillog, sizeof *faillog);
	close (fd);
}

/*
 * failcheck - check for failures > allowable
 *
 * failcheck() is called AFTER the password has been validated.
 */

int
failcheck (uid, faillog, failed)
int	uid;
struct	faillog	*faillog;
int	failed;
{
	int	fd;
	int	okay = 1;
	struct	faillog	fail;

	if ((fd = open (FAILFILE, O_RDWR)) < 0)
		return (1);

	lseek (fd, (off_t) (sizeof *faillog) * uid, 0);
	if (read (fd, (char *) faillog, sizeof *faillog) == sizeof *faillog) {
		if (faillog->fail_max != 0
				&& faillog->fail_cnt >= faillog->fail_max)
			okay = 0;
	}
	if (!failed && okay) {
		fail = *faillog;
		fail.fail_cnt = 0;

		lseek (fd, (off_t) sizeof fail * uid, 0);
		write (fd, (char *) &fail, sizeof fail);
	}
	close (fd);

	return (okay);
}

/*
 * failprint - print line of failure information
 */

void
failprint (fail)
struct	faillog	*fail;
{
	struct	tm	*tp;
#ifdef	SVR4
	char	lasttime[32];
#else
	char	*lasttime;
#endif

	if (fail->fail_cnt == 0)
		return;

	tp = localtime (&(fail->fail_time));

#if __STDC__
	/*
	 * Only print as much date and time info as it needed to
	 * know when the failure was.
	 */

	if (NOW - fail->fail_time >= YEAR)
	    strftime(lasttime, sizeof lasttime, NULL, tp);
	else if (NOW - fail->fail_time >= DAY)
	    strftime(lasttime, sizeof lasttime, "%A %T", tp);
	else
	    strftime(lasttime, sizeof lasttime, "%T", tp);
#else

	/*
	 * Do the same thing, but don't use strftime since it
	 * probably doesn't exist on this system
	 */

	lasttime = asctime (tp);
	lasttime[24] = '\0';

	if (NOW - fail->fail_time < YEAR)
		lasttime[19] = '\0';
	if (NOW - fail->fail_time < DAY)
		lasttime = lasttime + 11;

	if (*lasttime == ' ')
		lasttime++;
#endif	/* __STDC__ */
	printf ("%d %s since last login.  Last was %s on %s.\n",
		fail->fail_cnt, fail->fail_cnt > 1 ? "failures":"failure",
		lasttime, fail->fail_line);
}

void
failtmp (failent)
struct	utmp	*failent;
{
	int	fd;
	char	*ftmp;

	if ((ftmp = getdef_str ("FTMP_FILE")) == 0)
		return;

	if ((fd = open (ftmp, O_WRONLY|O_APPEND)) == -1)
		return;

	write (fd, (char *) failent, sizeof *failent);
	close (fd);
}
