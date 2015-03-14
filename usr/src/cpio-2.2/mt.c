/* mt -- control magnetic tape drive operation
   Copyright (C) 1991, 1992 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* If -f is not given, the environment variable TAPE is used;
   if that is not set, a default device defined in sys/mtio.h is used.
   The device must be either a character special file or a remote
   tape drive with the form "[user@]system:path".
   The default count is 1.  Some operations ignore it.

   Exit status:
   0	success
   1	invalid operation or device name
   2	operation failed

   Operations (unique abbreviations are accepted):
   eof, weof	Write COUNT EOF marks at current position on tape.
   fsf		Forward space COUNT files.
		Tape is positioned on the first block of the file.
   bsf		Backward space COUNT files.
		Tape is positioned on the first block of the file.
   fsr		Forward space COUNT records.
   bsr		Backward space COUNT records.
   bsfm		Backward space COUNT file marks.
		Tape is positioned on the beginning-of-the-tape side of
		the file mark.
   asf		Absolute space to file number COUNT.
		Equivalent to rewind followed by fsf COUNT.
   eom		Space to the end of the recorded media on the tape
		(for appending files onto tapes).
   rewind	Rewind the tape.
   offline, rewoffl
		Rewind the tape and, if applicable, unload the tape.
   status	Print status information about the tape unit.
   retension	Rewind the tape, then wind it to the end of the reel,
		then rewind it again.
   erase	Erase the tape.

   David MacKenzie <djm@gnu.ai.mit.edu> */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <sys/file.h>
#include <fcntl.h>
#include <errno.h>
#include <getopt.h>

#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif
#include "rmt.h"

#if defined(HAVE_STRING_H) || defined(STDC_HEADERS)
#include <string.h>
#else
#include <strings.h>
#endif

#if defined(STDC_HEADERS)
#include <stdlib.h>
#else
extern int errno;
char *getenv ();
int atoi ();
void exit ();
#endif

int fstat ();

int argmatch ();
void check_type ();
void error ();
void invalid_arg ();
void perform_operation ();
void print_status ();
void usage ();

char *opnames[] =
{
  "eof", "weof", "fsf", "bsf", "fsr", "bsr",
  "rewind", "offline", "rewoffl", "status",
#ifdef MTBSFM
  "bsfm",
#endif
#ifdef MTEOM
  "eom",
#endif
#ifdef MTRETEN
  "retension",
#endif
#ifdef MTERASE
  "erase",
#endif
 "asf",
  NULL
};

#define MTASF 600		/* Random unused number.  */
short operations[] =
{
  MTWEOF, MTWEOF, MTFSF, MTBSF, MTFSR, MTBSR,
  MTREW, MTOFFL, MTOFFL, MTNOP,
#ifdef MTBSFM
  MTBSFM,
#endif
#ifdef MTEOM
  MTEOM,
#endif
#ifdef MTRETEN
  MTRETEN,
#endif
#ifdef MTERASE
  MTERASE,
#endif
  MTASF
};

/* If nonzero, don't consider file names that contain a `:' to be
   on remote hosts; all files are local.  Always zero for mt;
   since when do local device names contain colons?  */
int f_force_local = 0;

struct option longopts[] =
{
  {"file", 1, NULL, 'f'},
  {"version", 0, NULL, 'V'},
  {NULL, 0, NULL, 0}
};

/* The name this program was run with.  */
char *program_name;

void
main (argc, argv)
     int argc;
     char **argv;
{
  extern char *version_string;
  short operation;
  int count;
  char *tapedev;
  int tapedesc;
  int i;

  program_name = argv[0];
  tapedev = NULL;
  count = 1;

  while ((i = getopt_long (argc, argv, "f:V", longopts, (int *) 0)) != -1)
    {
      switch (i)
	{
	case 'f':
	  tapedev = optarg;
	  break;

	case 'V':
	  printf ("GNU mt %s", version_string);
	  exit (0);
	  break;

	default:
	  usage ();
	}
    }

  if (optind == argc)
    usage ();

  i = argmatch (argv[optind], opnames);
  if (i < 0)
    {
      invalid_arg ("tape operation", argv[optind], i);
      exit (1);
    }
  operation = operations[i];

  if (++optind < argc)
    count = atoi (argv[optind]);
  if (++optind < argc)
    usage ();

  if (tapedev == NULL)
    {
      tapedev = getenv ("TAPE");
      if (tapedev == NULL)
#ifdef DEFTAPE			/* From sys/mtio.h.  */
        tapedev = DEFTAPE;
#else
	error (1, 0, "no tape device specified");
#endif
    }

  tapedesc = rmtopen (tapedev, O_RDONLY, 0);
  if (tapedesc == -1)
    error (1, errno, "%s", tapedev);
  check_type (tapedev, tapedesc);

  if (operation == MTASF)
    {
      perform_operation (tapedev, tapedesc, MTREW, 1);
      operation = MTFSF;
    }
  perform_operation (tapedev, tapedesc, operation, count);
  if (operation == MTNOP)
    print_status (tapedev, tapedesc);

  if (rmtclose (tapedesc) == -1)
    error (2, errno, "%s", tapedev);

  exit (0);
}

void
check_type (dev, desc)
     char *dev;
     int desc;
{
  struct stat stats;

  if (_isrmt (desc))
    return;
  if (fstat (desc, &stats) == -1)
    error (1, errno, "%s", dev);
  if ((stats.st_mode & S_IFMT) != S_IFCHR)
    error (1, 0, "%s is not a character special file", dev);
}

void
perform_operation (dev, desc, op, count)
     char *dev;
     int desc;
     short op;
     int count;
{
  struct mtop control;

  control.mt_op = op;
  control.mt_count = count;
  if (rmtioctl (desc, MTIOCTOP, &control))
    error (2, errno, "%s", dev);
}

void
print_status (dev, desc)
     char *dev;
     int desc;
{
  struct mtget status;

  if (rmtioctl (desc, MTIOCGET, &status))
    error (2, errno, "%s", dev);

  printf ("drive type = %d\n", (int) status.mt_type);
#if defined(hpux) || defined(__hpux__)
  printf ("drive status (high) = %d\n", (int) status.mt_dsreg1);
  printf ("drive status (low) = %d\n", (int) status.mt_dsreg2);
#else
  printf ("drive status = %d\n", (int) status.mt_dsreg);
#endif
  printf ("sense key error = %d\n", (int) status.mt_erreg);
  printf ("residue count = %d\n", (int) status.mt_resid);
#if !defined(ultrix) && !defined(__ultrix__) && !defined(hpux) && !defined(__hppux__)
  printf ("file number = %d\n", (int) status.mt_fileno);
  printf ("block number = %d\n", (int) status.mt_blkno);
#endif
}

void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-V] [-f device] [--file=device] [--version] operation [count]\n",
	   program_name);
  exit (1);
}
