/*
  This file is part of the NetFax system.

  (c) Copyright 1989 by David M. Siegel. 
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
#include <errno.h>
#include <fcntl.h>
#include <sys/param.h>

#include "seq.h"

static int seq_lock(file)
     char *file;
{
    char lock[MAXPATHLEN];
    int lock_fd;

    sprintf(lock, "%s.lock", file);
    while ((lock_fd = open(lock, O_CREAT|O_EXCL, 0666)) < 0) {
	if (errno != EEXIST)
	  return (-1);
	sleep(1);
    }
    close(lock_fd);

    return (0);
}

static int seq_unlock(file)
     char *file;
{
    char lock[MAXPATHLEN];

    sprintf(lock, "%s.lock", file);
    unlink(lock);
    
    return (0);
}

static int seq_write(file, numb)
     char *file;
     int numb;
{
    FILE *fp;

    if ((fp = fopen(file, "w+")) == NULL)
      return (-1);

    fprintf(fp, "%d\n", numb);

    fclose(fp);

    return (numb);
}

int seq_next(file)
     char *file;
{
    FILE *fp;
    int numb;

    if (seq_lock(file) < 0)
      return (-1);

    if ((fp = fopen(file, "r")) == NULL) {
	if (seq_write(file, 2) < 0) {
	    seq_unlock(file);
	    return (-1);
	}
	seq_unlock(file);
	return (1);
    }

    if (fscanf(fp, "%d", &numb) != 1) {
	fclose(fp);
	seq_unlock(file);
	return (-1);
    }

    fclose(fp);

    if (seq_write(file, numb+1) < 0) {
	seq_unlock(file);
	return (-1);
    }

    seq_unlock(file);

    return (numb);
}
