
/* 
 *  panic.c - terminate fast in case of error
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

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

/* Local headers */

#include "at.h"

/* Macros */


/* Structures and unions */


/* File scope variables */

static char rcsid[] = "$Id: panic.c,v 1.4 1993/01/17 23:06:41 kernel Exp kernel $";

/* External variables */


/* Function declarations */


/* Signal catching functions */


/* Local functions */


/* Global functions */

void panic(char *a)
{
/* Something fatal has happened, print error message and exit.
 */
	fprintf(stderr,"%s: %s\n",namep,a);
	if (fcreated)
		unlink(atfile);

	exit (EXIT_FAILURE);
}

void perr(char *a)
{
/* Some operating system error; print error message and exit.
 */
	perror(a);
	if (fcreated)
		unlink(atfile);

	exit(EXIT_FAILURE);
}

void usage(void)
{
/* Print usage and exit.
 */
	fprintf(stderr,"Usage: at [-q {[a-l]}] time [date]\nat -l\n"
			"at -d job [jobs...]\nbatch\n");
	exit(EXIT_FAILURE);
}
