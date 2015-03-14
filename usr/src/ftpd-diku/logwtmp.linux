/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 */

#ifndef lint
static char sccsid[] = "@(#)logwtmp.c	5.6 (Berkeley) 6/1/90";
#endif /* not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <utmp.h>
#include "pathnames.h"
#define min(x,y) (x<y?x:y)
static int fd = -1;

/*
 * Modified version of logwtmp that holds wtmp file open
 * after first call, for use with ftp (which may chroot
 * after login, but before logout).
 */
logwtmp(ttyn, username, hostname)
	char *ttyn, *username, *hostname;
{
		struct utmp ut;
		char *ttyabbrev;
		struct stat buf;
		
		memset((char *)&ut, 0, sizeof(ut));
		ut.ut_type = USER_PROCESS;
		ut.ut_pid = getpid();
		strncpy(ut.ut_line, ttyn, min(sizeof(ut.ut_line),sizeof(ttyn)-1));
		if (!strcmp(ttyn,"/dev/tty")){
			ttyabbrev = ttyn + sizeof("/dev/tty") - 1;
			strncpy(ut.ut_id, ttyabbrev, sizeof(ut.ut_id));
		}
		(void)time(&ut.ut_time);
		strncpy(ut.ut_user, username, sizeof(ut.ut_user));
		strncpy(ut.ut_host, hostname, sizeof(ut.ut_host));
		
	if (fd < 0 && (fd = open(_PATH_WTMP, O_WRONLY|O_APPEND, 0)) < 0)
		return;
	else

		if (write(fd, (char *)&ut, sizeof(struct utmp)) !=
		    sizeof(struct utmp))
			(void)ftruncate(fd, buf.st_size);
}
