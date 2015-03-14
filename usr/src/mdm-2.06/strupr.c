/*************************************************************************
Convert a String to Uppercase
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

$Header: /home/smilie/bbs/modem/RCS/strupr.c,v 1.3 1992/10/10 06:04:48 smilie Exp $

$Log: strupr.c,v $
 * Revision 1.3  1992/10/10  06:04:48  smilie
 * fixed a date
 *
 * Revision 1.2  1992/10/09  13:40:59  smilie
 * fixed some warnings
 *
 * Revision 1.1  1992/10/09  10:12:58  smilie
 * Initial revision
 *

*************************************************************************/

/* Feature test switches */
#define _POSIX_SOURCE 1
#define _STRUPR_C

/* System Headers */
#include <stdio.h>
#include <ctype.h>

/* Local Headers */

/* Macros */

/* File scope variables */

static char strupr_rcsid[] = "$Id: strupr.c,v 1.3 1992/10/10 06:04:48 smilie Exp $";
#define RCSID strupr_rcsid

/* External variables */

/* External Functions */

/* Structures and unions */

/* Functions */

/***************************************************************************
			      STRUPR
---------------------------------------------------------------------------
Makes a String Uppercase.
***************************************************************************/
char *strupr(char *st)
{
char *t = st;
/**/
while (t[0])
	{
	t[0] = toupper(t[0]);
	t++;
	}
return st;
}


