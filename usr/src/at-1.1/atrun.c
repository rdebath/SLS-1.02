/* 
 *  atrun.c - run jobs queued by at; run with root privileges.
 *  Copyright (C) 1993  Thomas Koenig
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#define _POSIX_SOURCE 1

/* System Headers */

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <signal.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

/* Local headers */

#include "atrun.h"

/* Macros */

#ifndef ATJOB_DIR
#define ATJOB_DIR "/usr/spool/atjobs/"
#endif

#ifndef LOADAVG_MX
#define LOADAVG_MX 2.0
#endif

/* Structures and unions */

/* File scope variables */

static char *namep;
static char rcsid[] = "$Id: atrun.c,v 1.3 1993/01/16 00:55:06 kernel Exp $";

/* External variables */

/* Function declarations */

/* Signal catching functions */

/* Local functions */

static void perr(char *a)
{
/* Print error and die. */
	perror(a);
	exit(EXIT_FAILURE);
}

/* Global functions */

int main(int argc, char *argv[])
{
/* Browse through  ATJOB_DIR, checking all the jobfiles wether they should
 * be executed and or deleted. The queue is coded into the first byte of
 * the job filename, the date (in minutes since Eon) as a hex number in the
 * following eight bytes, followed by a dot and a serial number.  A file
 * which has not been executed yet is denoted by its execute - bit set.
 * Files whose time has come are executed using /bin/sh, then their x bit
 * is turned off. Files which already have run are removed during the next
 * invocation.
 */

	DIR *spool;
	struct dirent *dirent;
	struct stat buf;
	int pid;
	uid_t uid;
	gid_t gid;
	char *nul = NULL;
	char **nenvp = &nul;
	int older;
	unsigned long ctm;
	char queue;

	namep = argv[0];
	if (chdir(ATJOB_DIR) != 0)
		perr("Cannot change to " ATJOB_DIR);

/* Main loop. Open spool directory for reading and look over all the files in
 * there. If the filename indicates that the job should be run and the x bit is
 * set, fork off a child which sets its user and group id to that of the files
 * and exec a /bin/sh which executes the shell script. Unlink older files if
 * they should no longer be run.  For deletion, their r bit has to be turned on.
 */

	if ((spool = opendir(".")) == NULL)
		perr("Cannot read " ATJOB_DIR);

	while ((dirent = readdir(spool)) != NULL)
	{
		if (stat(dirent->d_name,&buf) != 0)
			perr("Cannot stat in " ATJOB_DIR);

/*		We don't want directories
 */
		if (!S_ISREG(buf.st_mode)) 
			continue;

		if (sscanf(dirent->d_name,"%c%8lx",&queue,&ctm) != 2)
			continue;

		if ((queue == 'b') && (gloadavg() > LOADAVG_MX))
			continue;

		older = (time_t) ctm*60 <= time(NULL);

/*		The file is executable and old enough
*/
		if (older && (S_IXUSR & buf.st_mode))
		{
/* 		Now we know we want to run the file, we can turn off the execute bit
 */
			if (chmod(dirent->d_name,S_IRUSR) != 0)
				perr("Cannot change file permissions");

			if ((pid = fork()) == -1)
				perr("Cannot fork");

			else if (pid == 0)
			{
				if (queue > 'b')
					nice(queue - 'b');

				gid = buf.st_gid;
				uid = buf.st_uid;
				if (setgid(gid) != 0)
					perr("Cannot change group");

				if (setuid(uid) != 0)
					perr("Cannot change uid");

				if(execle("/bin/sh","sh",dirent->d_name,(char *) NULL,
					nenvp) != 0)
					perr("Exec failed");
			}
		}
/*		Delete older files
 */
		if (older && !(S_IXUSR & buf.st_mode) && (S_IRUSR & buf.st_mode))
			unlink(dirent->d_name);

	}
	exit(EXIT_SUCCESS);
}
