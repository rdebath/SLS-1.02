/* Copyright (C) 1993 Free Software Foundation, Inc.
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
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/file.h>

/* Apply or remove an advisory lock, according to OPERATION,
   on the file FD refers to.  */
int
DEFUN(__flock, (fd, operation), int fd AND int operation)
{
  struct flock flock;
  int cmd;

  switch (operation & ~LOCK_NB) {
  case LOCK_SH:
#ifdef __linux__
    flock.l_type = F_SHLCK;
#else
    flock.l_type = F_RDLCK;
#endif
    break;
  case LOCK_EX:
#ifdef __linux__
    flock.l_type = F_EXLCK;
#else
    flock.l_type = F_WRLCK;
#endif
    break;
  case LOCK_UN:
    flock.l_type = F_UNLCK;
    break;
  default:
    errno = EINVAL;
    return -1;
  }

  flock.l_whence = SEEK_SET;
  flock.l_start = flock.l_len = 0L;

  cmd = (operation & LOCK_NB) ?  F_SETLK : F_SETLKW;

  return (__fcntl (fd, cmd, &flock));
}
