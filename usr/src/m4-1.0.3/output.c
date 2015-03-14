/*
 * GNU m4 -- A simple macro processor
 * Copyright (C) 1989-1992 Free Software Foundation, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "m4.h"

/*
 * Output functions.  Most of the complexity is for handling cpp like
 * sync lines.
 *
 * This code is fairly entangled with the code in input.c, and maybe it
 * belongs there?
 */

/* Number of input line we are generating output for.  */
int output_current_line;

/* Current output stream.  */
static FILE *output;

/* Table of diversion files.  */
static FILE **divtab;


/*
 * Output initialisation.  It handles allocation of memory for
 * diversions.  This is done dynamically, to allow customisation of the
 * number of available diversions.
 */
void
output_init (void)
{
  int i;

  output = stdout;

  divtab = (FILE **) xmalloc (ndiversion * sizeof (FILE *));
  for (i = 0; i < ndiversion; i++)
    divtab[i] = NULL;
  divtab[0] = stdout;
}


/*
 * Output TEXT to either an obstack or a file.  If OBS is NULL, and there
 * is no output file, the text is discarded.
 *
 * If we are generating sync lines, the output have to be examined,
 * because we need to know how much output each input line generates.
 * In general, sync lines are output whenever a single input lines
 * generates several output lines, or when several input lines does not
 * generate any output.
 */
void
shipout_text (struct obstack *obs, char *text)
{
  static boolean start_of_output_line = TRUE;

  if (obs != NULL)
    {				/* output to obstack OBS */
      obstack_grow (obs, text, strlen (text));
      return;
    }
  if (output == NULL)		/* discard TEXT */
    return;

  if (!sync_output)
    fputs (text, output);
  else
    for (; *text; text++)
      {
	if (start_of_output_line)
	  {
	    start_of_output_line = FALSE;
	    output_current_line++;

#ifdef DEBUG_OUTPUT
	    printf ("DEBUG: cur %d, cur out %d\n",
		    current_line, output_current_line);
#endif

	    /* Output a `#line NUM' synchronisation directive if needed.
	       If output_current_line was previously given a negative
	       value (invalidated), rather output `#line NUM "FILE"'.  */

	    if (output_current_line != current_line)
	      {
		if (output != NULL)
		  {
		    fprintf (output, "#line %d", current_line);
		    if (output_current_line < 1)
		      fprintf (output, " \"%s\"", current_file);
		    putc ('\n', output);
		  }
		output_current_line = current_line;
	      }
	  }
	putc (*text, output);
	if (*text == '\n')
	  start_of_output_line = TRUE;
      }
}

/*
 * Functions for use by diversions.
 */
#ifndef HAVE_TMPFILE
/*
 * Implement tmpfile(3) for non-USG systems.
 */
static int mkstemp ();
extern int unlink ();

static FILE *
tmpfile (void)
{
  char buf[32];
  int fd;

  strcpy (buf, "/tmp/m4XXXXXX");
  fd = mkstemp (buf);
  if (fd < 0)
    return NULL;

  unlink (buf);
  return fdopen (fd, "w+");
}

#ifndef HAVE_MKSTEMP
/*
 * This implementation of mkstemp(3) does not avoid any races, but its
 * there.
 */
#include <sys/types.h>
#include <fcntl.h>

static int
mkstemp (char *tmpl)
{
  mktemp (tmpl);
  return open (tmpl, O_RDWR | O_TRUNC | O_CREAT, 0600);
}
#endif /* not HAVE_MKSTEMP */
#endif /* not HAVE_TMPFILE */

/*
 * Make a file for diversion DIVNUM, and install it in the diversion
 * table "divtab".  The file is opened read-write, so we can unlink it
 * immediately.
 */
void
make_diversion (int divnum)
{
  if (output != NULL)
    fflush (output);

  if (divnum < 0 || divnum >= ndiversion)
    {
      output = NULL;
      return;
    }

  if (divtab[divnum] == NULL)
    {
      divtab[divnum] = tmpfile ();
      if (divtab[divnum] == NULL)
	fatal ("can't create file for diversion: %s", syserr ());
    }
  output = divtab[divnum];
  output_current_line = -1;
}

/*
 *
 * Insert a FILE into the current output file, in tha same manner
 * diversions are handled.  This allows files to be included, without
 * having them rescanned by m4.
 */

void
insert_file (FILE *file)
{
  int ch;

  while ((ch = getc (file)) != EOF)
    putc (ch, output);
}

/*
 * Insert diversion number DIVNUM into the current output file.  The
 * diversion is NOT placed on the expansion obstack, because it must not
 * be rescanned.  When the file is closed, it is deleted by the system.
 */
void
insert_diversion (int divnum)
{
  FILE *div;

  if (divnum < 0 || divnum >= ndiversion)
    return;

  div  = divtab[divnum];
  if (div == NULL || div == output)
    return;

  if (output != NULL)
    {
      rewind (div);
      insert_file (div);
      output_current_line = -1;
    }
  fclose (div);
  divtab[divnum] = NULL;
}

/*
 * Get back all diversions.  This is done just before exiting from
 * main (), and from m4_undivert (), if called without arguments.
 */
void
undivert_all (void)
{
  int divnum;

  for (divnum = 1; divnum < ndiversion; divnum++)
    insert_diversion (divnum);
}
