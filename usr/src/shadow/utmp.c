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
#include <stdlib.h>
#include <utmpx.h>
extern	int	gettimeofday (struct timeval *tp);
#else
#include <sys/types.h>
#include <utmp.h>
#endif	/* SVR4 */

#include <fcntl.h>
#ifndef	BSD
#include <string.h>
#include <memory.h>
#define	bzero(a,n)	memset(a, 0, n)
#else
#include <strings.h>
#define	strchr	index
#define	strrchr	rindex
#endif
#include <stdio.h>
#ifdef	STDLIB_H
#include <stdlib.h>
#endif
#ifdef	UNISTD_H
#include <unistd.h>
#endif
#include "config.h"

#ifndef	UTMP_FILE
#define	UTMP_FILE	"/etc/utmp"
#endif

#if defined(SUN) || defined(BSD) || defined(SUN4)
#ifndef	WTMP_FILE
#define WTMP_FILE "/usr/adm/wtmp"
#endif
#endif	/* SUN || BSD */

#ifndef	lint
static	char	sccsid[] = "@(#)utmp.c	3.16	13:02:55	7/27/92";
#endif

#ifdef	SVR4
extern	struct	utmpx	utent;
#else
extern	struct	utmp	utent;
#endif

#ifndef	SVR4
extern	struct	utmp	*getutent();
extern	struct	utmp	*getutline();
extern	void	setutent();
extern	void	endutent();
extern	time_t	time();
extern	char	*ttyname();
extern	long	lseek();
#endif	/* SVR4 */

#define	NO_UTENT \
	"No utmp entry.  You must exec \"login\" from the lowest level \"sh\""
#define	NO_TTY \
	"Unable to determine your tty name."

/*
 * checkutmp - see if utmp file is correct for this process
 *
 *	System V is very picky about the contents of the utmp file
 *	and requires that a slot for the current process exist.
 *	The utmp file is scanned for an entry with the same process
 *	ID.  If no entry exists the process exits with a message.
 *
 *	The "picky" flag is for network and other logins that may
 *	use special flags.  It allows the pid checks to be overridden.
 *	This means that getty should never invoke login with any
 *	command line flags.
 */

void
checkutmp (picky)
int	picky;
{
	char	*line;
#ifdef	USG
#ifdef	SVR4
	struct	utmpx	*ut;
#else
	struct	utmp	*ut;
#endif	/* SVR4 */
#ifndef	NDEBUG
	int	pid = getppid ();
#else
	int	pid = getpid ();
#endif	/* !NDEBUG */
#endif	/* USG */

#if !defined(SUN) && !defined(SUN4)
#ifdef	SVR4
	setutxent ();
#else
	setutent ();
#endif
#endif	/* !SUN */

#ifdef	USG
	if (picky) {
#ifdef	SVR4
		while (ut = getutxent ())
#else
		while (ut = getutent ())
#endif
			if (ut->ut_pid == pid)
				break;

		if (ut)
			utent = *ut;

#ifdef	SVR4
		endutxent ();
#else
		endutent ();
#endif
		if (! ut) {
 			(void) puts (NO_UTENT);
			exit (1);
		}
#ifndef	UNIXPC

		/*
		 * If there is no ut_line value in this record, fill
		 * it in by getting the TTY name and stuffing it in
		 * the structure.  The UNIX/PC is broken in this regard
		 * and needs help ...
		 */

		if (utent.ut_line[0] == '\0')
#endif	/* !UNIXPC */
		{
			if (! (line = ttyname (0))) {
				(void) puts (NO_TTY);
				exit (1);
			}
			if (strncmp (line, "/dev/", 5) == 0)
				line += 5;
			(void) strncpy (utent.ut_line, line,
					(int) sizeof utent.ut_line);
		}
	} else {
		if (! (line = ttyname (0))) {
			puts (NO_TTY);
			exit (1);
		}
		if (strncmp (line, "/dev/", 5) == 0)
			line += 5;

 		(void) strncpy (utent.ut_line, line,
  						(int) sizeof utent.ut_line);
#ifdef	SVR4
		if (ut = getutline (&utent))
#else
		if (ut = getutline (&utent))
#endif
 			(void) strncpy (utent.ut_id, ut->ut_id,
 					(int) sizeof ut->ut_id);

		(void) strcpy (utent.ut_user, "LOGIN");
		utent.ut_pid = getpid ();
		utent.ut_type = LOGIN_PROCESS;
		(void) time (&utent.ut_time);
	}
#else	/* !USG */

	/*
	 * Hand-craft a new utmp entry.
	 */

	bzero (&utent, sizeof utent);
	if (! (line = ttyname (0))) {
		puts (NO_TTY);
		exit (1);
	}
	if (strncmp (line, "/dev/", 5) == 0)
		line += 5;

	(void) strncpy (utent.ut_line, line, sizeof utent.ut_line);
	(void) time (&utent.ut_time);
#endif	/* !USG */
}

/*
 * setutmp - put a USER_PROCESS entry in the utmp file
 *
 *	setutmp changes the type of the current utmp entry to
 *	USER_PROCESS.  the wtmp file will be updated as well.
 */

void
setutmp (name, line)
char	*name;
char	*line;
{
#ifdef SVR4
	struct	utmp	utmp;
	struct	utmpx	*utmpx, utxline;

	/*
	 * Update utmpx.  We create an empty entry in case there is
	 * no matching entry in the utmpx file.
	 */

	utmpxname (UTMPX_FILE);
	memset (&utxline, 0, sizeof utxline);
	strncpy (utxline.ut_line, line, sizeof utxline.ut_line);
	utmpx = getutxline (&utxline);

	/*
	 * If the entry matching `line' cannot be found, create a new
	 * entry with the device name in it.
	 */

	if (! utmpx) {

		/*
		 * Try appending /dev/ and looking again for the device.
		 */

		setutxent ();
		strcpy (utxline.ut_line, "/dev/");
		strnccat (utxline.ut_line, line, sizeof utxline.ut_line);
		if (! (utmpx = getutxline (&utxline)) {

			/*
			 * That didn't do it.  Just use the original
			 * terminal name.
			 */
			
			strncpy (utxline.ut_line, line, sizeof utxline.ut_line);
			utmpx = &utxline;
		}
	}

	/*
	 * Fill in the fields in the utmpx entry and write it out.
	 */

	strncpy (utmpx->ut_user, name, sizeof utmpx->ut_user);
	utmpx->ut_pid = getpid ();
	utmpx->ut_type = USER_PROCESS;
	gettimeofday (&(utmpx->ut_tv));
	strncpy (utmpx->ut_host, utent.ut_host, sizeof utmpx->ut_host);

	pututxline (utmpx);

	/*
	 * Now fill-in the regular utmp file entry.  All the information
	 * it needs is in utmpx.  We scribble it out as well.
	 */

	utmpname (UTMP_FILE);
	getutmp (utmpx, &utmp);
	pututline (&utmp);
	endutent ();

	/* 
	 * Update the WTMP and WTMPX files and end access to UTMPX.  The
	 * endutxent() can't be done until now since utmpx is still needed
	 * and endutext() trashes the contents.
	 */

	updwtmpx(WTMPX_FILE, utmpx);
	utent = *utmpx;

	endutxent();
#else /* !SVR4 */
	struct	utmp	utmp;
	int	fd;
	int	found = 0;

	if (! (fd = open (UTMP_FILE, O_RDWR)))
		return;

#if !defined(SUN) && !defined(BSD) && !defined(SUN4)
 	while (! found && read (fd, &utmp, sizeof utmp) == sizeof utmp) {
 		if (! strncmp (line, utmp.ut_line, (int) sizeof utmp.ut_line))
			found++;
	}
#endif
	if (! found) {

		/*
		 * This is a brand-new entry.  Clear it out and fill it in
		 * later.
		 */

  		(void) bzero (&utmp, sizeof utmp);
 		(void) strncpy (utmp.ut_line, line, (int) sizeof utmp.ut_line);
	}

	/*
	 * Fill in the parts of the UTMP entry.  BSD has just the name,
	 * while System V has the name, PID and a type.
	 */

#if defined(SUN) || defined(BSD) || defined(SUN4)
	(void) strncpy (utmp.ut_name, name, (int) sizeof utent.ut_name);
#else	/* SUN */
 	(void) strncpy (utmp.ut_user, name, (int) sizeof utent.ut_user);
	utmp.ut_type = USER_PROCESS;
	utmp.ut_pid = getpid ();
#endif	/* SUN || BSD */

	/*
	 * Put in the current time (common to everyone)
	 */

	(void) time (&utmp.ut_time);

#ifdef UT_HOST
	/*
	 * Update the host name field for systems with networking support
	 */

	(void) strncpy (utmp.ut_host, utent.ut_host, (int) sizeof utmp.ut_host);
#endif

	/*
	 * Locate the correct position in the UTMP file for this
	 * entry.
	 */

#if defined(SUN) || defined(BSD) || defined(SUN4)
	(void) lseek (fd, (long) (sizeof utmp) * ttyslot (), 0);
#else
	if (found)	/* Back up a splot */
		lseek (fd, (long) - sizeof utmp, 1);
	else		/* Otherwise, go to the end of the file */
		lseek (fd, (long) 0, 2);
#endif

	/*
	 * Scribble out the new entry and close the file.  We're done
	 * with UTMP, next we do WTMP (which is real easy, put it on
	 * the end of the file.
	 */

	(void) write (fd, &utmp, sizeof utmp);
	(void) close (fd);

	if ((fd = open (WTMP_FILE, O_WRONLY|O_APPEND)) >= 0) {
		(void) write (fd, &utmp, sizeof utmp);
		(void) close (fd);
	}
 	utent = utmp;
#endif /* SVR4 */
}
