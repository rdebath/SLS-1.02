/* Size of rel file utility (`size') for GNU.
   Copyright (C) 1986 Free Software Foundation, Inc.

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
#define AR_H    <ar.h>
#endif

#ifndef A_OUT_H
#define A_OUT_H    <a.out.h>
#endif

#include <stdio.h>
#include AR_H
#include <errno.h>
#include <sys/types.h>
#include <sys/file.h>

#if !defined(A_OUT) && !defined(MACH_O)
#define A_OUT
#endif

#ifdef A_OUT
#ifdef COFF_ENCAPSULATE
#include "a.out.encap.h"
#else
/* On native BSD systems, use the system's own a.out.h.  */
#include A_OUT_H
#endif
#endif

#ifdef MACH_O
#include <sys/loader.h>
#endif

#if defined(USG) || defined(linux)
#include <fcntl.h>
#include <string.h>
#else
#include <strings.h>
#endif

/* Number of input file names specified.  */
int number_of_files;

/* Current file's name */
char *input_name;

/* Current member's name, or 0 if processing a non-library file.  */
char *input_member;

/* Offset within archive of the current member,
   if we are processing an archive.  */
int member_offset;

/* The name this program was run with. */
char *program_name;

#ifdef __STDC__
#include <stdlib.h>
#else
char *malloc ();
#endif

void do_one_file ();
void do_one_rel_file ();
void error_with_file ();
void fatal ();
void perror_name ();
void print_file_name ();
char *xmalloc ();
char *concat ();

void
main (argc, argv)
     char **argv;
     int argc;
{
  int i;

  program_name = argv[0];

  number_of_files = argc - 1;

  printf ("text\tdata\tbss\tdec\thex\n");

  /* Now scan and describe the files.  */

  if (number_of_files == 0)
    do_one_file ("a.out");
  else
    for (i = 1; i < argc; i++)
      do_one_file (argv[i]);
  exit (0);
}

/* Print the filename of the current file on 'outfile' (a stdio stream).  */

void
print_file_name (outfile)
     FILE *outfile;
{
  fprintf (outfile, "%s", input_name);
  if (input_member)
    fprintf (outfile, "(%s)", input_member);
}

/* process one input file */
void scan_library ();

void
do_one_file (name)
     char *name;
{
  int desc;
  char armag[SARMAG];

  desc = open (name, O_RDONLY, 0);

  if (desc < 0)
    {
      perror_name (name);
      return;
    }

  input_name = name;
  input_member = 0;

  if (SARMAG != read (desc, armag, SARMAG) || strncmp (armag, ARMAG, SARMAG))
    do_one_rel_file (desc, 0L);
  else
    scan_library (desc);

  close (desc);
}

/* Read in the archive data about one member.
   SUBFILE_OFFSET is the address within the archive of the start of that data.

   Return the length of the member's contents, which does
   not include the archive data about the member.
   If there are no more valid members, return zero.  */

int
decode_library_subfile (desc, subfile_offset)
     int desc;
     int subfile_offset;
{
  int bytes_read;
  int namelen;
  int member_length;
  char *name;
  struct ar_hdr hdr1;

  lseek (desc, subfile_offset, 0);

  bytes_read = read (desc, &hdr1, sizeof hdr1);
  if (!bytes_read)
    ;		/* end of archive */

  else if (sizeof hdr1 != bytes_read)
    error_with_file ("malformed library archive");

  else if (sscanf (hdr1.ar_size, "%d", &member_length) != 1)
    error_with_file ("malformatted header of archive member");

  else
    {
      for (namelen = 0;
	   hdr1.ar_name[namelen] != 0
	   && hdr1.ar_name[namelen] != ' '
	   && hdr1.ar_name[namelen] != '/';
	   namelen++)
	;
      name = (char *) xmalloc (namelen+1);
      strncpy (name, hdr1.ar_name, namelen);
      *(name + namelen) = 0;

      input_member = name;

      return member_length;
    }
  return 0;   /* tell caller to exit loop */
}

/* Scan a library and describe each member.  */

void
scan_library (desc)
     int desc;
{
  int this_subfile_offset = SARMAG;
  int member_length;
  
  while (member_length = decode_library_subfile (desc, this_subfile_offset))
    {
      /* describe every member except the ranlib data if any */
      if (strcmp (input_member, "__.SYMDEF"))
	do_one_rel_file (desc, this_subfile_offset + sizeof (struct ar_hdr));

      this_subfile_offset += ((member_length + sizeof (struct ar_hdr)) + 1) & -2;
    }
}

/* Read a file's header and fill in various pieces of information.
   Return 0 on failure, 1 on success.  */

int
read_header_info (desc, offset, tsize, dsize, bsize)
     int desc;
     long int offset;
     unsigned int *tsize;
     unsigned int *dsize;
     unsigned int *bsize;
{
  int len;

#ifdef A_OUT
  {
    struct exec hdr;

    lseek (desc, offset, 0);
#ifdef HEADER_SEEK_FD
    /* Skip the headers that encapsulate our data in some other format
       such as COFF.  */
    HEADER_SEEK_FD (desc);
#endif
    len = read (desc, (char *) &hdr, sizeof (struct exec));
    if (len == sizeof (struct exec) && !N_BADMAG (hdr))
      {
	*tsize = hdr.a_text;
	*dsize = hdr.a_data;
	*bsize = hdr.a_bss;
	return 1;
      }
  }
#endif

#ifdef MACH_O
  {
    struct mach_header mach_header;
    char *hdrbuf;
    struct load_command *load_command;
    struct segment_command *segment_command;
    struct section *section;
    int len, cmd, seg;

    lseek (desc, offset, 0);
    len = read (desc, (char *) &mach_header, sizeof (struct mach_header));
    if (len == sizeof (struct mach_header) && mach_header.magic == MH_MAGIC)
      {
	hdrbuf = xmalloc (mach_header.sizeofcmds);
	len = read (desc, hdrbuf, mach_header.sizeofcmds);
	if (len != mach_header.sizeofcmds)
	  {
	    error_with_file ("failure reading Mach-O load commands");
	    return 0;
	  }
	load_command = (struct load_command *) hdrbuf;
	for (cmd = 0; cmd < mach_header.ncmds; ++cmd)
	  {
	    if (load_command->cmd == LC_SEGMENT)
	      {
		segment_command = (struct segment_command *) load_command;
		section = (struct section *) ((char *) (segment_command + 1));
		for (seg = 0; seg < segment_command->nsects; ++seg, ++section)
		  {
		    if (!strncmp(SECT_TEXT, section->sectname, sizeof section->sectname))
		      *tsize = section->size;
		    else if (!strncmp(SECT_DATA, section->sectname, sizeof section->sectname))
		      *dsize = section->size;
		    else if (!strncmp(SECT_BSS, section->sectname, sizeof section->sectname))
		      *bsize = section->size;
		  }
	      }
	    load_command = (struct load_command *)
	      ((char *) load_command + load_command->cmdsize);
	  }
	free (hdrbuf);
	return 1;
      }
  }
#endif

  return 0;
}

void
do_one_rel_file (desc, offset)
     int desc;
     long int offset;
{
  unsigned int tsize, dsize, bsize, total;

  if (read_header_info (desc, offset, &tsize, &dsize, &bsize))
    {
      total =  tsize + dsize + bsize;
      printf ("%u\t%u\t%u\t%u\t%x", tsize, dsize, bsize, total, total);
    }
  else
    {
      error_with_file ("malformed input file (not rel or archive)");
      return;
    }

  if (number_of_files > 1 || input_member)
    {
      printf ("\t");
      print_file_name (stdout);
    }
  printf ("\n");
}

/* Report a fatal error.
   STRING is a printf format string and ARG is one arg for it.  */

void
fatal (string, arg)
     char *string, *arg;
{
  fprintf (stderr, "%s: ", program_name);
  fprintf (stderr, string, arg);
  fprintf (stderr, "\n");
  exit (1);
}

/* Report a nonfatal error.
   STRING is a printf format string and ARG is one arg for it.  */

void
error (string, arg)
     char *string, *arg;
{
  fprintf (stderr, "%s: ", program_name);
  fprintf (stderr, string, arg);
  fprintf (stderr, "\n");
}

/* Report a nonfatal error.
   STRING is printed, followed by the current file name.  */

void
error_with_file (string)
     char *string;
{
  fprintf (stderr, "%s: ", program_name);
  print_file_name (stderr);
  fprintf (stderr, ": ");
  fprintf (stderr, string);
  fprintf (stderr, "\n");
}

/* Report an error using the message for the last failed system call,
   followed by the string NAME.  */

void
perror_name (name)
     char *name;
{
#if !defined(linux) || defined(NON_NATIVE)
  extern int errno, sys_nerr;
  extern char *sys_errlist[];
#endif
  char *s;

  if (errno < sys_nerr)
    s = concat (name, ": ", sys_errlist[errno]);
  else
    s = concat (name, ": ", "unknown error");
  error (s, name);
}


/* Like malloc but get fatal error if memory is exhausted.  */

char *
xmalloc (size)
     unsigned size;
{
  char *result = malloc (size);

  if (!result)
    fatal ("virtual memory exhausted", 0);
  return result;
}

/* Return a newly-allocated string
   whose contents concatenate those of S1, S2, S3.  */

char *
concat (s1, s2, s3)
     char *s1, *s2, *s3;
{
  int len1 = strlen (s1), len2 = strlen (s2), len3 = strlen (s3);
  char *result = (char *) xmalloc (len1 + len2 + len3 + 1);

  strcpy (result, s1);
  strcpy (result + len1, s2);
  strcpy (result + len1 + len2, s3);
  result[len1 + len2 + len3] = 0;

  return result;
}
