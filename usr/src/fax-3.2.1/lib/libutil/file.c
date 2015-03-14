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
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ioctl.h>

#include "file.h"

#define BSD_NONBLOCKING

int
file_set_nonblocking(fd)
int fd;
{
#ifdef BSD_NONBLOCKING
    int on = 1;

    if (ioctl(fd, FIONBIO, &on) < 0)
      return (-1);
#else
    int flags;

    flags = fcntl(fd, F_GETFL, 0);
    flags |= O_NDELAY;

    if (fcntl(fd, F_SETFL, flags) < 0)
      return (-1);
#endif

    return (0);
}

int
file_read(fd, buf, buflen)
int fd;
char *buf;
int buflen;
{
    int count = 0;
    int rc;

    while (count < buflen) {
	rc = read(fd, buf, buflen-count);
	if (rc > 0) {
	    count += rc;
	    buf += rc;
	} else
	  return (rc);
    }

    return (count);
}
