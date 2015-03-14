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
#include <errno.h>
#include <varargs.h>
#include <strings.h>
#include <syslog.h>

#include "log.h"

extern char *sys_errlist[];
extern int sys_nerr;
static int log_level = LOG_DEBUG;
static int use_syslog = 0;

/*
 * We break this routine out from below to avoid performing
 * this scan if we're not using the format string.  That is,
 * the loglevel is to low to force the printout.
 */
static void fix_format(format_in, format_out, len, errno_save)
     char *format_in;
     char *format_out;
     int len;
     int errno_save;
{
    char *f = format_in;
    char *b = format_out;
    char *b_end = &format_out[len];
    char c;

    while ((c = *f++) != '\0' && b < b_end) {
	switch (c) {
	  case '%':
	    c = *f++;
	    switch (c) {
	      case 'm':
		if ((unsigned)errno_save > sys_nerr)
		  sprintf(b, "error %d", errno_save);
		else
		  strcpy(b, sys_errlist[errno_save]);
		b += strlen(b);
		break;
	      default:
		*b++ = '%';
		*b++ = c;
		break;
	    }
	    break;
	  default:
	    *b++ = c;
	    break;
	}
    }

    *b = '\0';
}

/* VARARGS */
int log(va_alist)
     va_dcl
{
    va_list ap;
    int level;
    char *file;
    int line;
    char *format;
    char formatbuf[BUFSIZ];
    char finalbuf[BUFSIZ];
    char outbuf[BUFSIZ];
    char *outbuf_ptr = outbuf;
    char *slash, *ptr;
    int errno_save = errno;

    va_start(ap);

    level = va_arg(ap, int);

    if (level > log_level)
      return (0);

    file = va_arg(ap, char *);
    line = va_arg(ap, int);
    format = va_arg(ap, char *);

    if ((slash = rindex(file, '/')) != NULL)
      file = slash+1;

    fix_format(format, formatbuf, sizeof(formatbuf), errno_save);
    if (!use_syslog)
      fprintf(stderr, "(%s:%d) ", file, line);
    vsprintf(finalbuf, formatbuf, ap);

    for (ptr = finalbuf; *ptr != NULL; ptr++) {
	switch (*ptr) {
	  case '\r':
	    *(outbuf_ptr++) = '\\';
	    *(outbuf_ptr++) = 'r';
	    break;
	  case '\n':
	    *(outbuf_ptr++) = '\\';
	    *(outbuf_ptr++) = 'n';
	    break;
	  default:
	    *(outbuf_ptr++) = *ptr;
	    break;
	}
    }
    *(outbuf_ptr++) = '\n';
    *(outbuf_ptr++) = '\0';

    if (use_syslog)
      syslog(level, outbuf);
    else
      fputs(outbuf, stderr);
    
    va_end(ap);

    return (0);
}

void log_set_level(level)
     int level;
{
    log_level = level;
}

void log_enable_syslog(facility)
     int facility;
{
    openlog("fax", LOG_PID|LOG_NOWAIT, facility);
    use_syslog = 1;
}
