/*	Copyright (C) 1993 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <stdio.h>
#include <varargs.h>
#include "sysdef.h"

#ifdef __STDC__
#define CONST const
#undef NULL
#else
#define CONST
#endif

int
vfprintf (fp, s, ap)
     FILE *fp;
     CONST char *s;
     va_list ap;
{
  int len;

  len = _doprnt (s, ap, fp);
  return (ferror (fp) ? EOF : len);
}



