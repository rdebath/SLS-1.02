/* Copyright (C) 1991, 1992 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <ansidecl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <alloca.h>
#include <fcntl.h>
#include <sys/types.h>

#if 1
#define	__access	access
#define	__open		open
#endif

/* Return a file descriptor with a unique temporary file name from TEMPLATE.
   The last six characters of TEMPLATE must be "XXXXXX";
   they are replaced with a string that makes the filename unique.
   Return -1 if no suitable file could be created. */
int
DEFUN(mkstemp, (template), char *template)
{
  char *buffer;
  size_t len;

  len = strlen(template);
  if (len < 6 || strcmp(&template[len - 6], "XXXXXX")) {
    errno = EINVAL;
    return -1;
  }

  buffer = (char *) __alloca(len + 1);
  strcpy (buffer, template);

  while (mktemp (template) != NULL) {

    /* Make sure `template' doesn't exist. */
    if (__access (template, F_OK))
      return __open (template, O_RDWR | O_CREAT, 0666);

    strcpy (template, buffer);
  }

  return -1;
}
