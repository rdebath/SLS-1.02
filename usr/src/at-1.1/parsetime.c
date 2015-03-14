
/* 
 *  parsetime.c - parse time for at(1)
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
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

/* Local headers */

#include "at.h"
#include <getopt.h>

/* Macros */


/* Structures and unions */


/* File scope variables */

static char rcsid[] = "$Id: parsetime.c,v 1.2 1993/01/16 00:58:13 kernel Exp $";

/* External variables */


/* Function declarations */


/* Signal catching functions */


/* Local functions */


/* Global functions */

time_t parsetime(int argc, char **argv)
{
/* Do the argument parsing, die if necessary, and return the time the job
 * should be run.
 */
	time_t nowtimer, runtimer;
	struct tm nowtime, runtime;

	if ((argc - optind < 1) || (argc - optind > 2) || (strlen(argv[optind]) != 4))
		usage();

	nowtimer = time(NULL);
	nowtime = *localtime(&nowtimer);

	runtime = nowtime;
	runtime.tm_sec = 0;
	runtime.tm_isdst = 0;

	if (sscanf(argv[optind],"%2d%2d",&(runtime.tm_hour),&(runtime.tm_min)) != 2)
		usage();

	if (argc == 3)
	{
		if (strlen(argv[optind+1]) == 6)
		{
			if (sscanf(argv[optind+1],"%2d%2d%2d",
				&(runtime.tm_mday),&(runtime.tm_mon),
				&(runtime.tm_year)) !=3)
				usage();
			else
			{
				runtime.tm_mon --;
			}
				
		}
		else if (strlen(argv[optind+1]) == 8)
		{
			if (sscanf(argv[optind+1],"%2d%2d%4d",
				&(runtime.tm_mday),&(runtime.tm_mon),
				&(runtime.tm_year)) !=3)
				usage();
			else
			{
				runtime.tm_mon --;
				runtime.tm_year -= 1900;
			}
		}
		else
			usage();
	}

	runtimer = mktime(&runtime);

	if (runtimer < 0)
		panic("Illegal time specified");

	if ((argc -1 == optind) && (runtimer < nowtimer))
		runtimer += 24*3600;

	if (nowtimer > runtimer)
		panic("Trying to travel back in time");

	return runtimer;

}
