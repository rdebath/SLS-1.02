/* `time' utility to display resource usage of processes.
   Copyright (C) 1989, 1991, 1992 Free Software Foundation, Inc.

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

/* Most information is collected from the rusage struct.

   Usage: time [-avp] [-f format] [-o file] [--append] [--portability]
          [--verbose] [--format format] [--output-file file] command [arg...]

   Options:
   -a, --append			Append to output instead of overwriting.
   -f, --format STRING		Use STRING to format the statistics.
   -o, --output-file NAME	Send summary to NAME instead of stderr.
   -p, --portability		Use the POSIX.2 output format.
   -v, --verbose		Use a verbose format to print the statistics.

   Written by David Keppel, pardo@cs.washington.edu.
   Modified by David MacKenzie, djm@gnu.ai.mit.edu. */

#include <stdio.h>
#include <sys/types.h>
#include <signal.h>
#include <errno.h>
#include <getopt.h>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include "timeval.h"
#endif

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#include <sys/wait.h>
#else
#include "rusage.h"
#endif

#if defined(__STDC__)
# define P_(s) s
# define PTR void *
#else
# define P_(s) ()
# define PTR char *
# define const
#endif

#if defined(STDC_HEADERS) || defined(USG)
#include <string.h>
#else
#include <strings.h>
#endif

#ifdef STDC_HEADERS
#include <limits.h>
#include <stdlib.h>
#else
#define	LONG_MAX (~(1 << (sizeof (long) * 8 - 1)))
void abort P_((void));
void exit P_((int));
char *getenv P_((const char *));
PTR malloc P_((size_t));
extern int errno;
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef _POSIX_VERSION
int execvp P_((const char *, const char **));
int fork P_((void));
void _exit P_((int));
#endif /* !_POSIX_VERSION */

void error P_((int status, int errnum, char *message, ...));

int gettimeofday P_((struct timeval *tp, struct timezone *tz));
#ifdef HAVE_SYS_RESOURCE_H
int wait3 P_((union wait *status, int options, struct rusage *rusage));
#else
int wait3 P_((int *status, int options, struct rusage *rusage));
#endif

#include "getpagesize.h"
#ifndef getpagesize
int getpagesize P_((void));
#endif

void usage P_((void));

#ifndef RETSIGTYPE
#define RETSIGTYPE void
#endif
typedef RETSIGTYPE (*sighand_t) ();

/* The time in msec of one `tick' as used by the `getrusage' structure.
   At least one Unix man page has an error, as it reports
   units for average sizes in kb*sec.  Judging by the
   output of `time' (all versions), this looks like it
   ought to be in kb*ticks. */
#define ONETICK (20)		/* 20 msec/tick, 1/50th sec. */

/* Convert T msec to a number of ticks. */
#define TICK(t) ((t) / ONETICK)

typedef enum
{
  NO = 0, YES
} bool;

#define UL unsigned long

char *version_string = "GNU time version 1.4";

/* The default output format.  */
static const char *const default_format =
"%Uuser %Ssystem %Eelapsed %PCPU (%Xavgtext+%Davgdata %Mmaxresident)k\n\
%Iinputs+%Ooutputs (%Fmajor+%Rminor)pagefaults %Wswaps";

/* The output format for the -p option .*/
static const char *const posix_format = "real %e\nuser %U\nsys %S";

/* Format string for printing all statistics verbosely.
   The format string is used two ways: as a format string, and in
   verbose mode, to document all the possible formatting possiblities.
   When `longstats' is used as a format string, it has to be put into
   one contiguous string (e.g., into a `char[]').  We could alternatively
   store it as a `char *' and convert it into a `*char[]' when we need
   it as documentation, but most compilers choke on very long strings. */
static const char *const longstats[] =
{
  "\nSummary statistics:\n",
  "\tCommand being timed: \"%C\"\n",
  "\n",
  "\tUser time (seconds): %U\n",
  "\tSystem time (seconds): %S\n",
  "\tPercent of CPU this job got: %P\n",
  "\tElapsed (wall clock) time (h:mm:ss or m:ss): %E\n",
  "\n",
  "\tAverage shared text size (kbytes): %X\n",
  "\tAverage unshared data size (kbytes): %D\n",
  "\tAverage stack size (kbytes): %p\n",
  "\tAverage total size (kbytes): %K\n",
  "\tMaximum resident set size (kbytes): %M\n",
  "\tAverage resident set size (kbytes): %t\n",
  "\n",
  "\tMajor (requiring I/O) page faults: %F\n",
  "\tMinor (reclaiming a frame) page faults: %R\n",
  "\tVoluntary context switches: %w\n",
  "\tInvoluntary context switches: %c\n",
  "\tSwaps: %W\n",
  "\tFile system inputs: %I\n",
  "\tFile system outputs: %O\n",
  "\n",
  "\tSocket messages sent: %s\n",
  "\tSocket messages received: %r\n",
  "\tSignals delivered: %k\n",
  "\n",
  "\tPage size (bytes): %Z\n",
  "\tExit status: %x\n",
  NULL
};

/* If YES, show an English description next to each statistic. */
static bool verbose;

/* Name of output file.  Only used if -o option is given. */
static char const *outfile;

/* Output stream, stderr by default. */
static FILE *outfp;

/* If YES, append to `outfile' rather than truncating it. */
static bool append;

/* Controls the output format. */
static char const *output_format;

/* The command line to run and gather statistics on. */
static char const **command_line;

/* The name this program was invoked by. */
char *program_name;

static struct option longopts[] =
{
  {"append", 0, NULL, 'a'},
  {"format", 1, NULL, 'f'},
  {"output-file", 1, NULL, 'o'},
  {"portability", 0, NULL, 'p'},
  {"verbose", 0, NULL, 'v'},
  {"version", 0, NULL, 'V'},
  {NULL, 0, NULL, 0}
};

/* Print ARGV to FP, with each entry in ARGV separated by FILLER. */

void
fprintargv (fp, argv, filler)
     FILE *fp;
     char const *const argv[];
     char const *filler;
{
  char const *const *av;

  av = argv;
  fputs (*av, fp);
  while (*++av)
    {
      fputs (filler, fp);
      fputs (*av, fp);
    }
  if (ferror (fp))
    error (1, errno, "write error");
}

/* Return the number of kilobytes corresponding to a number of pages PAGES.
   Try to do arithmetic so that the risk of overflow errors is minimized.
   This is funky since the pagesize could be less than 1K.
   Note: Some machines express getrusage statistics in terms of K,
   others in terms of pages. */

unsigned long
tok (pages)
     unsigned long pages;
{
  static unsigned long ps = 0;
  unsigned long tmp;
  static long size = LONG_MAX;

  /* Initialization. */
  if (ps == 0)
    ps = (long) getpagesize ();

  /* Conversion. */
  if (pages > (LONG_MAX / ps))
    {				/* Could overflow.  */
      tmp = pages / 1024;	/* Smaller first, */
      size = tmp * ps;		/* then larger.  */
    }
  else
    {				/* Could underflow.  */
      tmp = pages * ps;		/* Larger first, */
      size = tmp / 1024;	/* then smaller.  */
    }
  return size;
}

/* summarize: Report on the system use of a command.

   Copy the FMT argument to FP except that `%' sequences
   have special meaning, and `\n' and `\t' are translated into
   newline and tab, respectively, and `\\' is translated into `\'.

   The character following a `%' can be:
   (* means the tcsh time builtin also recognizes it)
   % == a literal `%'
   C == command name
*  D == average unshared data size in K (ru_idrss+ru_isrss)
*  E == elapsed real (wall clock) time in [hour:]min:sec.usec
*  F == major page faults (required physical I/O) (ru_majflt)
*  I == file system inputs (ru_inblock)
*  K == average total mem usage (ru_idrss+ru_isrss+ru_ixrss)
*  M == maximum resident set size in K (ru_maxrss)
*  O == file system outputs (ru_oublock)
*  P == percent of CPU this job got (total cpu time / elapsed time)
*  R == minor page faults (reclaims; no physical I/O involved) (ru_minflt)
*  S == system (kernel) time (seconds:usec) (ru_stime)
*  U == user time (seconds:usec) (ru_utime)
*  W == times swapped out (ru_nswap)
*  X == average amount of shared text in K (ru_ixrss)
   Z == page size
*  c == involuntary context switches (ru_nivcsw)
*  k == signals delivered (ru_nsignals)
   p == average unshared stack size in K (ru_isrss)
*  r == socket messages received (ru_msgrcv)
*  s == socket messages sent (ru_msgsnd)
   t == average resident set size in K (ru_idrss)
*  w == voluntary context switches (ru_nvcsw)
   x == exit status of command

   Various memory usages are found by converting from page-seconds
   to kbytes by multiplying by the page size, dividing by 1024,
   and dividing by elapsed real time.

   FP is the stream to print to.
   FMT is the format string, interpreted as described above.
   COMMAND is the command and args that are being summarized.
   RUP is information on the command.
   TV is the elapsed time of the command.
   EXITSTAT is the command's exit status. */

void
summarize (fp, fmt, command, rup, tv, exitstat)
     FILE *fp;
     char const *fmt;
     char const *command[];
     struct rusage *rup;
     struct timeval *tv;
     int exitstat;
{
  unsigned long r;		/* Elapsed real milliseconds. */
  unsigned long v;		/* Elapsed virtual (CPU) time. */

  if (exitstat)
    fprintf (fp, "Command had non-zero exit status %d\n", exitstat);

  /* All times converted to milliseconds.  `r' is real, or wallclock
     time.  `v' is virtual, or cpu-seconds time.  Occasionally, one
     of these values comes out as zero.  Dividing by zero causes
     problems, so we first need to check the time value.  If it is
     zero, then we take `evasive action' instead of printing a
     value. */
  r = tv->tv_sec * 1000 + tv->tv_usec / 1000;
  v = rup->ru_utime.tv_sec * 1000 + rup->ru_utime.tv_usec / 1000 +
    rup->ru_stime.tv_sec * 1000 + rup->ru_stime.tv_usec / 1000;

  while (*fmt)
    {
      switch (*fmt)
	{
	case '%':
	  switch (*++fmt)
	    {
	    case '%':		/* Literal '%'.  */
	      putc ('%', fp);
	      break;
	    case 'C':		/* The command that got timed.  */
	      /* If no command, print nothing.  */
	      if (*command)
		fprintargv (fp, command, " ");
	      break;
	    case 'D':		/* Total unshared data size.  */
	      fprintf (fp, "%lu", (TICK (v) == 0)
		       ? 0
		       : tok ((UL) rup->ru_idrss) / TICK (v) +
		       tok ((UL) rup->ru_isrss) / TICK (v));
	      break;
	    case 'E':		/* Elapsed real (wall clock) time.  */
	      if (tv->tv_sec >= 3600)
		fprintf (fp, "%ld:%02ld:%02ld", tv->tv_sec / 3600,
			 (tv->tv_sec % 3600) / 60, tv->tv_sec % 60);
	      else
		fprintf (fp, "%ld:%02ld.%02ld", tv->tv_sec / 60,
			 tv->tv_sec % 60, tv->tv_usec / 10000);
	      break;
	    case 'F':		/* Major page faults.  */
	      fprintf (fp, "%ld", rup->ru_majflt);
	      break;
	    case 'I':		/* Inputs.  */
	      fprintf (fp, "%ld", rup->ru_inblock);
	      break;
	    case 'K':		/* Average mem usage == data+stack+text.  */
	      fprintf (fp, "%lu",
		       (TICK (v) == 0) ? 0 :
		       tok ((UL) rup->ru_idrss) / TICK (v) +
		       tok ((UL) rup->ru_isrss) / TICK (v) +
		       tok ((UL) rup->ru_ixrss) / TICK (v));
	      break;
	    case 'M':		/* Maximum resident set size.  */
	      fprintf (fp, "%lu", tok ((UL) rup->ru_maxrss));
	      break;
	    case 'O':		/* Outputs.  */
	      fprintf (fp, "%ld", rup->ru_oublock);
	      break;
	    case 'P':		/* Percent of CPU this job got.  */
	      /* % cpu is (total cpu time)/(elapsed time).  */
	      if (r > 0)
		fprintf (fp, "%lu%%", (v * 100 / r));
	      else
		fprintf (fp, "?%%");
	      break;
	    case 'R':		/* Minor page faults (reclaims).  */
	      fprintf (fp, "%ld", rup->ru_minflt);
	      break;
	    case 'S':		/* System time.  */
	      fprintf (fp, "%ld.%ld",
		       rup->ru_stime.tv_sec, rup->ru_stime.tv_usec / 10000);
	      break;
	    case 'U':		/* User time.  */
	      fprintf (fp, "%ld.%ld",
		       rup->ru_utime.tv_sec, rup->ru_utime.tv_usec / 10000);
	      break;
	    case 'W':		/* Times swapped out.  */
	      fprintf (fp, "%ld", rup->ru_nswap);
	      break;
	    case 'X':		/* Average shared text size.  */
	      fprintf (fp, "%lu", (TICK (v) == 0) ? 0
		       : tok ((UL) rup->ru_ixrss) / TICK (v));
	      break;
	    case 'Z':		/* Page size.  */
	      fprintf (fp, "%d", getpagesize ());
	      break;
	    case 'c':		/* Involuntary context switches.  */
	      fprintf (fp, "%ld", rup->ru_nivcsw);
	      break;
	    case 'e':		/* Elapsed real time in seconds.  */
	      fprintf (fp, "%ld.%ld", tv->tv_sec, tv->tv_usec / 10000);
	      break;
	    case 'k':		/* Signals delivered.  */
	      fprintf (fp, "%ld", rup->ru_nsignals);
	      break;
	    case 'p':		/* Stack segment.  */
	      fprintf (fp, "%lu", (TICK (v) == 0) ? 0
		       : tok ((UL) rup->ru_isrss) / TICK (v));
	      break;
	    case 'r':		/* Incoming socket messages received.  */
	      fprintf (fp, "%ld", rup->ru_msgrcv);
	      break;
	    case 's':		/* Outgoing socket messages sent.  */
	      fprintf (fp, "%ld", rup->ru_msgsnd);
	      break;
	    case 't':		/* Average resident set size.  */
	      fprintf (fp, "%lu",
		 (TICK (v) == 0) ? 0 : tok ((UL) rup->ru_idrss) / TICK (v));
	      break;
	    case 'w':		/* Voluntary context switches.  */
	      fprintf (fp, "%ld", rup->ru_nvcsw);
	      break;
	    case 'x':		/* Exit status.  */
	      fprintf (fp, "%d", exitstat);
	      break;
	    case '\0':
	      putc ('?', fp);
	      return;
	    default:
	      putc ('?', fp);
	      putc (*fmt, fp);
	    }
	  ++fmt;
	  break;

	case '\\':		/* Format escape. */
	  switch (*++fmt)
	    {
	    case 't':
	      putc ('\t', fp);
	      break;
	    case 'n':
	      putc ('\n', fp);
	      break;
	    case '\\':
	      putc ('\\', fp);
	      break;
	    default:
	      putc ('?', fp);
	      putc ('\\', fp);
	      putc (*fmt, fp);
	    }
	  ++fmt;
	  break;

	default:
	  putc (*fmt++, fp);
	}

      if (ferror (fp))
	error (1, errno, "write error");
    }
  putc ('\n', fp);

  if (ferror (fp))
    error (1, errno, "write error");
}

/* Return a null-terminated string containing the concatenation,
   in order, of all of the elements of ARGV.
   The '\0' at the end of each ARGV-element is not copied.
   Example:	char *argv[] = {"12", "ab", ".,"};
 		linear_argv(argv) == "12ab.,"
   Print a message and return NULL if memory allocation failed. */

static char *
linear_argv (argv)
     char const *const *argv;
{
  char const *const *s;		/* Each string in ARGV. */
  char *new;			/* Allocated space. */
  char *dp;			/* Copy in to destination. */
  char const *sp;		/* Copy from source. */
  int size;

  /* Find length of ARGV and allocate. */
  size = 1;
  for (s = argv; *s; ++s)
    size += strlen (*s);
  new = (char *) malloc (size);
  if (new == NULL)
    {
      fprintf (stderr, "%s: virtual memory exhausted\n", program_name);
      return NULL;
    }

  /* Copy each string in ARGV to the new string.  At the end of
     each string copy, back up `dp' so that on the next string,
     the `\0' will be overwritten. */
  for (s = argv, sp = *s, dp = new; *s; ++s)
    {
      sp = *s;
      while ((*dp++ = *sp++) != '\0')
	/* Do nothing. */ ;
      --dp;
    }

  return new;
}

/* Initialize the options and parse the command line arguments.
   Also note the position in ARGV where the command to time starts.

   By default, output is to stderr.

   ARGV is the array of command line arguments.
   ARGC is the number of command line arguments. */

void
getargs (argc, argv)
     int argc;
     char *argv[];
{
  int optc, longind;
  char *format;			/* Format found in environment. */

  /* Initialize the option flags. */
  verbose = NO;
  outfile = NULL;
  outfp = stderr;
  append = NO;
  output_format = default_format;
  program_name = argv[0];

  /* Set the format string from the environment.  Do this before checking
     the args so that we won't clobber a user-specified format. */
  format = getenv ("TIME");
  if (format)
    output_format = format;

  while ((optc = getopt_long (argc, argv, "+af:o:pvV", longopts, &longind))
	 != EOF)
    {
      switch (optc)
	{
	case 'a':
	  append = YES;
	  break;
	case 'f':
	  output_format = optarg;
	  break;
	case 'o':
	  outfile = optarg;
	  break;
	case 'p':
	  output_format = posix_format;
	  break;
	case 'v':
	  verbose = YES;
	  break;
	case 'V':
	  fprintf (stderr, "%s\n", version_string);
	  break;
	default:
	  usage ();
	}
    }

  if (optind == argc)
    usage ();

  command_line = (char const **) &argv[optind];

  if (outfile)
    {
      if (append)
	outfp = fopen (outfile, "a");
      else
	outfp = fopen (outfile, "w");
      if (outfp == NULL)
	error (1, errno, "%s", outfile);
    }

  /* If the user specified verbose output, we need to convert
     `longstats' to a `char *'. */
  if (verbose)
    {
      output_format = (char const *) linear_argv (longstats);
      if (output_format == NULL)
	exit (1);		/* Out of memory. */
    }
}

/* Run command CMD and return statistics on it.
   *RUSE gets most of the statistics.
   *FINISH gets the elapsed time.
   *EXITCODE gets the exit code of the child. */

void
run_command (ruse, finish, exitcode, cmd)
     struct rusage *ruse;
     struct timeval *finish;
     int *exitcode;
     char const *const cmd[];
{
#ifdef HAVE_SYS_RESOURCE_H
  union wait status;		/* Exit status of child. */
#else
  int status;
#endif
  int pid;			/* Pid of child. */
  int caught;			/* Pid got back from wait. */
  struct timeval start;		/* When job starts. */
  sighand_t interrupt_signal, quit_signal;

  /* Set time the command started.  Since this is BEFORE the
     fork/exec sequence, this number is probably too large. */
  gettimeofday (&start, (struct timezone *) NULL);

  pid = fork ();		/* Run CMD as child process. */
  if (pid < 0)
    error (1, errno, "cannot fork");
  else if (pid == 0)
    {				/* If child. */
      execvp (cmd[0], (const char **) cmd);
      error (0, errno, "cannot run %s", cmd[0]);
      _exit (errno == ENOENT ? 127 : 126);
    }

  /* Have signals kill the child but not self (if possible). */
  interrupt_signal = signal (SIGINT, SIG_IGN);
  quit_signal = signal (SIGQUIT, SIG_IGN);

  /* Ignore signals, but don't ignore the children.  When wait3
     returns the child process, set the time the command finished. */
  while ((caught = wait3 (&status, 0, ruse)) != pid)
    {
      if (caught == -1)
	error (1, errno, "error waiting for child process");
    }

  /* Re-enable signals. */
  signal (SIGINT, interrupt_signal);
  signal (SIGQUIT, quit_signal);

  gettimeofday (finish, (struct timezone *) NULL);

  /* Set `finish' to hold the elapsed "real" (wallclock) time of
     the command.  If finish.usec < start.usec, manually carry a one
     from the seconds field. */
  finish->tv_sec -= start.tv_sec;
  if (finish->tv_usec < start.tv_usec)
    {
      finish->tv_usec += 1000000;
      --finish->tv_sec;
    }
  finish->tv_usec -= start.tv_usec;

#ifdef HAVE_SYS_RESOURCE_H
  *exitcode = (int) status.w_T.w_Retcode;
#else
  *exitcode = status >> 8;
#endif
}

void
main (argc, argv)
     int argc;
     char **argv;
{
  int exitcode;			/* Exit status of job we ran. */
  struct timeval elapsed;	/* Wallclock time of job. */
  struct rusage ruse;		/* Use by children. */

  getargs (argc, argv);
  run_command (&ruse, &elapsed, &exitcode, command_line);
  summarize (outfp, output_format, command_line, &ruse, &elapsed, exitcode);
  exit (exitcode);		/* Exit with child's status. */
}

void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-apvV] [-f format] [-o file] [--append] [--portability]\n\
       [--verbose] [--format=format] [--output-file=file] [--version]\n\
       command [arg...]\n",
	   program_name);
  exit (1);
}
