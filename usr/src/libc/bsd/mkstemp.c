/* Copyright (C) 1991, 1992, 1993 Free Software Foundation, Inc.
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
#include <fcntl.h>
#include <sys/types.h>

/* Return a file descriptor with a unique temporary file name from
   TEMPLATE. The last six characters of TEMPLATE must be "XXXXXX";
   they are replaced with a string that makes the filename unique.
   Return -1 if no suitable file could be created. */
int
DEFUN(mkstemp, (template), char *template)
{
  static CONST char letters[]
    = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
  CONST char *ptr = letters;
  pid_t pid = __getpid();
  int fd;
  size_t len;

  len = strlen(template);
  if (len < 6 || strcmp(&template[len - 6], "XXXXXX"))
    {
      errno = EINVAL;
      return -1;
    }

  if (sprintf (&template[len - 5], "%.5u",
  	 (unsigned int) pid % 100000) != 5)
    return -1;

  while (*ptr != '\0') {
    template[len - 6] = *ptr++;
    /* Make sure `template' doesn't exist. */
    fd = __open (template, O_RDWR | O_CREAT | O_EXCL, 0666);
    if (fd >= 0 || errno != EEXIST) return fd;
  }

  return -1;
}
