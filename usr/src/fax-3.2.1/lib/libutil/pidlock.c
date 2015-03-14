/*
  This file is part of the NetFax system.

  (c) Copyright 1989 by David M. Siegel and Sundar Narasimhan.
      All rights reserved.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation.

    This program is distributed in the hope that it will be useful, 
    but WITHOUT ANY WARRANTY; without even the implied warranty of 
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <stdio.h>
#include <sys/file.h>
#include <errno.h>

#include "pidlock.h"

int
pid_lock(pathname)
char *pathname;
{
    FILE *fp;
    int pid;

    if ((fp = fopen(pathname, "r+")) != NULL) {
	rewind(fp);

	if (fscanf(fp, "%d\n", &pid) != 1) {
	    fclose(fp);
	    errno = EEXIST;
	    return (-1);
	}

	if ((kill (pid, 0) == 0) || (errno != ESRCH)) {
	    fclose(fp);
	    errno = EEXIST;
	    return (-1);
	}
	
	ftruncate(fileno(fp), 0);
	rewind(fp);
    } else {
	if ((fp = fopen(pathname, "w")) == NULL)
	  return (-1);
    }

    fprintf(fp, "%d\n", getpid());

    fclose(fp);

    return (0);
}

int
pid_unlock(pathname)
char *pathname;
{
    return (unlink(pathname));
}
