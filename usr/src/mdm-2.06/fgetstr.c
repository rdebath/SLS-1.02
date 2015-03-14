/*************************************************************************
	    Get String like FGETS but without any newlines
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

$Header: /home/smilie/bbs/modem/RCS/fgetstr.c,v 1.1 1992/10/09 10:20:18 smilie Exp $

$Log: fgetstr.c,v $
 * Revision 1.1  1992/10/09  10:20:18  smilie
 * Initial revision
 *

*************************************************************************/

/* Feature test switches */
#define _POSIX_SOURCE 1
#define _FGETSTR_C

/* System Headers */
#include <stdio.h>

/* Local Headers */

/* Macros */

/* File scope variables */

static char fgetstr_rcsid[] = "$Id: fgetstr.c,v 1.1 1992/10/09 10:20:18 smilie Exp $";
#define RCSID fgetstr_rcsid

/* External variables */

/* External Functions */

/* Structures and unions */

/* Functions */

/***************************************************************************
			     FGETSTR
---------------------------------------------------------------------------
Is much the same as fgets, but makes sure there isint a /n at the
end of the returned string.
***************************************************************************/
char *fgetstr(char *s, int n, FILE * stream)
{
/*------------------------------------------------------------------------*/
if (!feof(stream))
	{
	if (fgets(s, n, stream) == NULL)
		{
		s[0] = 0;
		return NULL;
		}
	else
		{
		if (s[strlen(s)-1] == '\n')
			s[strlen(s)-1] = 0;
		}
	}
return s;
}


