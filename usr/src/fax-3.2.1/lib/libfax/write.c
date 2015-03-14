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
#include <sys/types.h>
#include <sys/time.h>
#include <varargs.h>

#include "log.h"
#include "write.h"

int nwrite(fd, buf, bufsize)
     int fd;
     char *buf;
     int bufsize;
{
    fd_set writefds;
    int left = bufsize;
    
    FD_ZERO(&writefds);
    FD_SET(fd, &writefds);

    for (;;) {
	switch (select(32, NULL, &writefds, NULL, NULL)) {
	  case -1:
	    log(L_EMERG, "do_write: select failed: %m");
	    return (-1);
	  case 1:
	    if (FD_ISSET(fd, &writefds)) {
		int wrote = write(fd, buf, left);
		if (wrote < 0) {
		    log(L_EMERG, "do_write: write failed: %m");
		    return (-1);
		}
		left -= wrote;
		buf += wrote;
	    }
	    if (left == 0)
	      return (bufsize);
	    break;
	  default:
	    break;
	}
    }
}

/* VARARGS */
int fdprintf(va_alist)
     va_dcl
{
    va_list ap;
    int fd;
    char *format;
    char buf[1024];

    va_start(ap);
    fd = va_arg(ap, int);
    format = va_arg(ap, char *);
    vsprintf(buf, format, ap);
    va_end(ap);

    return (nwrite(fd, buf, strlen(buf)));
}
