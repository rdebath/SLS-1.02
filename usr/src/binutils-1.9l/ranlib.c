/* Dummy ranlib program for GNU.
   All it does is `ar rs LIBRARY' for each library specified.

   Copyright (C) 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef AR_H
#define	AR_H	<ar.h>
#endif

#include AR_H
#include <sys/types.h>
#include <sys/file.h>
#include <stdio.h>
#include <fcntl.h>
#include "getopt.h"
#include "signame.h"

#ifndef L_SET
#define L_SET 0
#define L_INCR 1
#endif

#ifndef X_OK
#define X_OK 1
#endif

#if defined(USG) || defined(OLD_LINUX)
#define bzero(s, n) (memset ((s), 0, (n)))
#define gettimeofday(TV,TZ) (time (TV))
#define seconds(TV) TV
long time();
#else
#define seconds(TV) TV.tv_sec
#include <sys/time.h>
#endif

#if defined(USG) || defined(OLD_LINUX)
#define	vfork	fork
#endif

void touch_symdefs ();
void usage ();

/* The makefile generates a -D switch to define AR_PROG
   as the location of the GNU `ar' program.  */
char *prog = AR_PROG;

/* The first place we look for `ar', before trying `prog'. */
#ifdef NON_NATIVE
char *prog_pref = CROSS_AR;
#else
# ifdef linux
char *prog_pref = "/usr/bin/ar";
# else
char *prog_pref = "/usr/local/bin/gnuar";
# endif
#endif

/* The name this program was run with. */
char *program_name;

void
main (argc, argv)
     int argc;
     char **argv;
{
  static int touch = 0, verbose = 0;
  static char short_opts[] = "tv";
  static struct option long_opts[] =
    {
      { "touch", 0, &touch, 0 },
      { "verbose", 0, &verbose, 0 },
      { 0, 0, 0, 0 }
    };
  register int c;
  char *args;
  int longind;
  char *ar_program;

  program_name = argv[0];

  while ((c = getopt_long (argc, argv, short_opts, long_opts, &longind))
	 != EOF)
    switch (c)
      {
      case 't':
	touch = 1;
	break;

      case 'v':
	verbose = 1;
	break;

      default:
	usage ();
      }

  if (optind == argc)
    usage ();

  if (access (prog_pref, X_OK) == 0)
    ar_program = prog_pref;
  else
    ar_program = prog;

  args = verbose ? "rsv" : "rs";

  if (touch)
    touch_symdefs (argc - optind, argv + optind);
  else
    {
      register int i;
      for (i = optind; i < argc; ++i)
	{
	  int pid;

	  if (verbose)
	    printf ("%s %s %s\n", ar_program, args, argv[i]);

	  fflush (stdout);
	  fflush (stderr);

	  pid = vfork ();
	  if (pid < 0)
	    {
	      fprintf (stderr, "%s: can not ", program_name);
	      perror ("vfork");
	    }
	  else if (pid == 0)
	    {
	      /* Child side.  */
	      execl (ar_program, ar_program, args, argv[i], 0);
	      fprintf (stderr, "%s: can not run ", program_name);
	      perror (ar_program);
	      exit (1);
	    }
	  else
	    {
	      /* Parent side.  */
	      int status;
	      if (wait (&status) != pid)
		{
		  fprintf (stderr, "%s: interrupted during ", program_name);
		  perror ("wait");
		}
	      if ((status & 0x7f) != 0)
		{
#ifdef GLIBC
		  extern char *strsignal (int);

  		  ar_program = strsignal (status & 0x7f);
#else
  		  psignal (status & 0x7f, ar_program);
#endif
		  exit (1);
		}
	      else if (((status & 0xff00) >> 8) != 0)
		exit ((status & 0xff00) >> 8);
	    }
	}
    }

  exit (0);
}

/* "touch" the archive files listed in LARGV, containing LARGC elements.
   Find the first symdef member in them and update the date to the
   current one.  */

void
touch_symdefs (largc, largv)
     int largc;
     char **largv;
{
#if defined(USG) || defined(OLD_LINUX)
  long tv;
#else
  struct timeval tv;
  struct timezone tz;
#endif
  struct ar_hdr archive_header;
  int i, rr;

  gettimeofday (&tv, &tz);

  while (largc--)
    {
      int fd = open (*largv, O_RDWR);

      if (fd < 0)
	{
	  fprintf (stderr, "%s: can not open `%s' for ", program_name, *largv);
	  perror ("read/write");
	  largv++;
	  continue;
	}

      lseek (fd, SARMAG, L_SET);

      rr = read (fd, &archive_header, sizeof (archive_header));

      /* In the general case this loop would be sped up by buffering,
         but in almost all cases the symdef will be the first member, so
	 I'm not going to bother.  */
      
      while (rr == sizeof (archive_header))
	{
	  if (!strncmp ("__.SYMDEF", archive_header.ar_name,
			sizeof ("__.SYMDEF") - 1))
	    {
	      bzero ((char *) archive_header.ar_date,
		     sizeof (archive_header.ar_date));

	      sprintf (archive_header.ar_date, "%d", seconds (tv));
	      
	      for (i = 0; i < sizeof archive_header.ar_date; i++)
		if (archive_header.ar_date[i] == '\0')
		  archive_header.ar_date[i] = ' ';

	      lseek (fd, - rr, L_INCR);
	      write (fd, &archive_header, sizeof (archive_header));
	      close (fd);
	      break;
	    }

	  lseek (fd, atoi (archive_header.ar_size), L_INCR);
	  rr = read (fd, &archive_header, sizeof (archive_header));
	}

      if (rr != sizeof (archive_header))
	/* We reached the end of the file.  */
	fprintf (stderr, "%s: can not find `__.SYMDEF' member in `%s'\n",
		 program_name, *largv);

      largv++;
    }
}

void
usage ()
{
  fprintf (stderr, "Usage: %s [-tv] [+touch] [+verbose] archive...\n",
	   program_name);
  exit (1);
}
