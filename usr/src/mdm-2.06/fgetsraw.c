/*************************************************************************
Get String from a RAW TTY with timeout
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

$Header: /home/smilie/bbs/modem/RCS/fgetsraw.c,v 1.5 1992/10/10 06:04:17 smilie Exp $

$Log: fgetsraw.c,v $
 * Revision 1.5  1992/10/10  06:04:17  smilie
 * fixed a date
 *
 * Revision 1.4  1992/10/09  13:39:37  smilie
 * fixed some MORE warnings <phew>
 *
 * Revision 1.3  1992/10/09  13:37:50  smilie
 * fixed some warnings
 *
 * Revision 1.2  1992/10/09  10:14:45  smilie
 * added GPL message
 *
 * Revision 1.1  1992/10/09  10:10:14  smilie
 * Initial revision

*************************************************************************/

/* Feature test switches */
#define _POSIX_SOURCE 1
#define _FGETSRAW_C

/* System Headers */
#include <stdio.h>
#include <termios.h>
#include <unistd.h>

/* Local Headers */

/* Macros */

/* File scope variables */

static char fgetsraw_rcsid[] = "$Id: fgetsraw.c,v 1.5 1992/10/10 06:04:17 smilie Exp $";
#define RCSID fgetsraw_rcsid

/* External variables */

/* External Functions */

/* Structures and unions */

/* Functions */

/*************************************************************************
				FGETSRAW
--------------------------------------------------------------------------
s = string
len = max length of string
fh = file handle
to = time out

will return 1 if string was recieved ok
will return 0 if timeout

*************************************************************************/
int fgetsraw(char *s, int len, FILE *fh, int to)
{
struct termios t, told;		/* termio structs */
int a;
int ch;
char *ss = s;
/**/
s[0] = 0;			/* Initialise String */
tcgetattr(fileno(fh), &t);	/* Get TTY Attributes */
tcgetattr(fileno(fh), &told);
t.c_iflag |= (ICRNL);		/* Turn CR to NL mode on */
t.c_lflag &= ~(ICANON|ISIG);	/* Turn off CANON processing and Signal handling */
t.c_cc[VMIN] = 0;		/* Turn VMIN off */
t.c_cc[VTIME] = (to * 10);	/* Set timeout */
tcsetattr(fileno(fh), TCSANOW, &t);	/* Set TTY */

for (a=0; a<(len-1); a++)
	{
	if (!read(fileno(fh), (void *)&ch, 1))
		ch = -1;
	if (ch == -1)					/* Timeout or error */
		{
		tcsetattr(fileno(fh), TCSANOW, &told);	/* Reset TTY */
		fflush(fh);				/* Flush TTY */
		return 0;
		}
	if ((char)ch == '\r')				/* Return on CR's */
		{
		*ss = 0;				/* Set current to 0 */
		tcsetattr(fileno(fh), TCSANOW, &told);	/* Reset TTY */
		fflush(fh);				/* Flush TTY */
		return 1;
		}
	else
	if ((char)ch == '\n')				/* Return on Newlines */
		{
		*ss = 0;				/* Set current to 0 */
		tcsetattr(fileno(fh), TCSANOW, &told);	/* Reset TTY */
		fflush(fh);				/* Flush TTY */
		return 1;
		}
	else						/* Valid Character */
		{
		*ss = (char)ch;	/* Add to end of string */
		ss++;
		}
	}
*ss = 0;						/* Set end of string */
tcsetattr(fileno(fh), TCSANOW, &told);			/* Reset TTY */
fflush(fh);						/* Flush TTY */
return 1;
}

