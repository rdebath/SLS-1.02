
/* 
 *  gloadavg.c - get load average for Linux
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

#include <stdio.h>

/* Local headers */

#include "atrun.h"

/* Macros */

/* Structures and unions */

/* File scope variables */

static char rcsid[] = "$Id: gloadavg.c,v 1.1 1993/01/10 23:18:58 kernel Exp $";

/* External variables */

/* Function declarations */

/* Signal catching functions */

/* Local functions */

/* Global functions */

double gloadavg(void)
/* return the current load average as a floating point number, or <0 for
 * error
 */
{
	double result;
	FILE *fp;

	if((fp=fopen(PROC_DIR "loadavg","r")) == NULL)
		return -1.0;

	if(fscanf(fp,"%lf",&result) != 1)
		return -1.0;

	return result;
}
