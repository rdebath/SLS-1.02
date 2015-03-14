/* xalloc and for smalltalk,
   Copyright (C) 1984, 1989 Free Software Foundation, Inc.

This file is part of GNU Smalltalk.

GNU Smalltalk is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Smalltalk is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Smalltalk; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <stdio.h>

/* This name is used by alloca.c.  */

char *
xmalloc(n)
register unsigned n;
{
  register char *block;

  block = (char*)calloc(n,1);
  if (block == NULL)
    {
      errorf("memory exhausted");
      exit(1);
    }

  return (block);
}
