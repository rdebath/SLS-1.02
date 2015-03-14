/*	Copyright (C) 1990, 1992, 1993 Free Software Foundation, Inc.

This file is part of Oleo, the GNU Spreadsheet.

Oleo is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Oleo is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Oleo; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdio.h>
#include "funcdef.h"
#include "sysdef.h"

#include "global.h"
#include "io-generic.h"
#include "io-abstract.h"


extern char **environ;
#ifdef __STDC__
extern int dup (int);
extern int close (int);
#ifndef linux
extern VOIDSTAR sbrk (size_t);
extern VOIDSTAR brk (VOIDSTAR);
#endif /* linux */
#else
extern int dup ();
extern int close ();
extern VOIDSTAR sbrk ();
extern VOIDSTAR brk ();
#endif



void
panic_write_file (fp, rng)
     FILE *fp;
     struct rng *rng;
{
  int fd;
  VOIDSTAR datend;
  VOIDSTAR datstart;
  unsigned long cnt;

  if (rng)
    {
      io_error_msg ("Can't write partial panic-save files");
      return;
    }
  fd = dup (fileno (fp));
  if (fd < 0)
    {
      io_error_msg ("Couldn't dup save file");
      return;
    }
  datstart = (VOIDSTAR) (&environ + sizeof (char **));
  datend = sbrk (0);
  if (datend == (char *) -1)
    {
      io_error_msg ("Couldn't sbrk(0)!");
      close (fd);
      return;
    }

  cnt = (char *) datend - (char *) datstart;
  if (write (fd, &cnt, sizeof (cnt)) != sizeof (cnt))
    {
      io_error_msg ("Couldn't write %lu (%d bytes) to save file", cnt, sizeof (cnt));
      close (fd);
      return;
    }
  if (write (fd, datstart, cnt) != cnt)
    {
      io_error_msg ("Couldn't write %lu bytes to save file", cnt);
      close (fd);
      return;
    }
  if (close (fd) < 0)
    {
      io_error_msg ("Couldn't close save file");
      return;
    }
}

void
panic_read_file (fp, ismerge)
     FILE *fp;
     int ismerge;
{
  int fd;
  unsigned long cnt;
  VOIDSTAR datstart;

  if (ismerge)
    {
      io_error_msg ("Can't merge panic-save files");
      return;
    }
  if ((fd = dup (fileno (fp))) < 0)
    {
      io_error_msg ("Couldn't dup save file");
      return;
    }
  if (read (fd, (VOIDSTAR) & cnt, sizeof (cnt)) != sizeof (cnt))
    {
      io_error_msg ("Couldn't read data_size (%d bytes) from save file", sizeof (cnt));
      close (fd);
      return;
    }
  datstart = (VOIDSTAR) (&environ + sizeof (char **));
  if (brk ((char *) datstart + cnt) == (VOIDSTAR) - 1)
    {
      io_error_msg ("Couldn't allocate %lu bytes of memory", cnt);
      close (fd);
      return;
    }
  if (read (fd, datstart, cnt) != cnt)
    {
      io_error_msg ("Couldn't read in %lu bytes of data", cnt);
      close (fd);
      return;
    }
  if (close (fd) < 0)
    io_error_msg ("Couldn't close save file");
  io_recenter_all_win ();
}

int
panic_set_options (set_opt, option)
     int set_opt;
     char *option;
{
  return -1;
}


void
panic_show_options ()
{
  io_text_line ("File format:  panic save   (quick-n-dirty data-segment dump)");
}
