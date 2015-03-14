/*************************************************************************
General Logging routine
--------------------------------------------------------------------------

    Copyright (C) 1992  Anthony Rumble

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details. <copying>

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

--------------------------------------------------------------------------
RCS Info

$Header: /home/smilie/bbs/modem/RCS/flog.c,v 1.7 1992/10/09 14:47:07 smilie Exp $

$Log: flog.c,v $
 * Revision 1.7  1992/10/09  14:47:07  smilie
 * fixed a small problem
 *
 * Revision 1.6  1992/10/09  14:41:31  smilie
 * *** empty log message ***
 *
 * Revision 1.5  1992/10/09  14:38:57  smilie
 * fixed logging
 *
 * Revision 1.4  1992/10/09  13:48:02  smilie
 * fixed another warning up
 *
 * Revision 1.3  1992/10/09  13:42:43  smilie
 * fixed some warnings
 *
 * Revision 1.2  1992/10/09  13:18:11  smilie
 * fixed a few things
 *
 * Revision 1.1  1992/10/09  12:16:08  smilie
 * Initial revision
 *

*************************************************************************/

/* Feature test switches */
#define _POSIX_SOURCE 1
#define _FLOG_C

/* System Headers */
#include <stdio.h>		/* STD I/O header */
#include <stdarg.h>		/* STD Argument header */
#include <time.h>		/* Time function header */
#include <errno.h>

/* Local Headers */

/* Macros */

/* File scope variables */

static char flog_rcsid[] = "$Id: flog.c,v 1.7 1992/10/09 14:47:07 smilie Exp $";
#define RCSID flog_rcsid

/* External variables */

extern char stty[8];		/* Short TTY name */

/* External Functions */

/* Structures and unions */

/* Functions */

/************************************************************************
				FLOG
-------------------------------------------------------------------------
General LOGGING function, works similar to printf
*************************************************************************/
void flog(char *fmt, ...)
{
va_list	ap;		/* Variable List */
#if defined(LOGFILE) || defined(LOGDEV)
FILE *fh;
#endif
time_t t;
struct tm *lt;
char tmps[256];
/**/

(void)time(&t);			/* Get Current TIme */
lt = localtime(&t);		/* Put time into structure */

(void)strftime(tmps, sizeof(tmps), "%H:%M", lt);

#ifdef LOGDEV
if ((fh = fopen(LOGDEV, "w")) ==  NULL)
	{
	(void)fprintf(stderr, "flog: Cannot open logdevice\n");
	perror("logfile");
	(void)exit(1);
	}
	
(void)fprintf(fh, "%s %s ", stty, tmps);
va_start(ap, fmt);		/* Open Argument list */
vfprintf(fh, fmt, ap);		/* Print to STD error */
va_end(ap);			/* Close Argument list */
(void)fclose(fh);
#else
(void)fprintf(stderr, "%s %s ", stty, tmps);
va_start(ap, fmt);		/* Open Argument list */
vfprintf(stderr, fmt, ap);	/* Print to STD error */
va_end(ap);			/* Close Argument list */
#endif

#ifdef LOGFILE
if ((fh = fopen(LOGFILE, "at")) ==  NULL)
	{
	(void)fprintf(stderr, "flog: Cannot open logfile\n");
	perror("logfile");
	(void)exit(1);
	}

(void)strftime(tmps, sizeof(tmps), "%c", lt);

(void)fprintf(fh, "%s %s ", stty, tmps);
va_start(ap, fmt);
vfprintf(fh, fmt, ap);
va_end(ap);
(void)fclose(fh);
#endif
}



