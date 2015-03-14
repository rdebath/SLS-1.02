/* strip certain symbols from a rel file.
   Copyright (C) 1986, 1990 Free Software Foundation, Inc.

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

#ifndef A_OUT_H
#define A_OUT_H    <a.out.h>
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <signal.h>
#include <errno.h>
#ifndef linux
extern int errno;
#endif
#include "getopt.h"

#if defined(USG) || defined(linux)
#include <fcntl.h>
#include <string.h>
#else
#include <strings.h>
#endif

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
#ifndef A_OUT
#include <nlist.h>
#include <reloc.h>
#endif
#include <sys/loader.h>
#endif

#ifdef nounderscore
#define LPREFIX '.'
#else
#define LPREFIX 'L'
#endif

#if defined (sun) && defined (sparc)
/* On the sparc, the name of the relocation info structure is
   different (on SunOS4, "struct relocation_info" does not exist).
   The meaning of the r_index field is the same as r_symbolnum
   in normal relocation_info's for external symbols.  Fortunately,
   we only use the field for external symbols.  */
typedef struct reloc_info_sparc *relocation_info_ptr;
#define RELOCATION_INFO_SYMBOL_NUM(ri) (ri)->r_index
#else /* not Sun and sparc.  */
typedef struct relocation_info *relocation_info_ptr;
#define RELOCATION_INFO_SYMBOL_NUM(ri) (ri)->r_symbolnum
#endif /* not Sun and sparc.  */

/* If BSD or HP-UX, we can use `ftruncate' and `rename'.  */

#if !defined(USG) || defined(hpux) || \
	 (defined(linux) && !defined(OLD_LINUX))
#define HAVE_FTRUNCATE
#define HAVE_RENAME
#endif /* !USG || hpux */

/* Number of nlist entries that are for local symbols. */
int local_sym_count;

/* Number of nlist entries that are for local symbols
   whose names don't start with L. */
int non_L_local_sym_count;

/* Number of nlist entries for debugger info.  */
int debugger_sym_count;

/* Number of global symbols referenced or defined.  */
int global_sym_count;

/* Total number of symbols to be preserved in the current file.  */
int nsyms;

/* Number of files specified in the command line. */
int number_of_files;

/* Kinds of files understood.  */
enum file_type { IS_UNKNOWN, IS_A_OUT, IS_MACH_O };

/* Each specified file has a file_entry structure for it.
   These are contained in the vector which file_table points to.  */

struct file_entry
{
  char *filename;
  enum file_type filetype;	/* what kind of file it is */

  /* Things obtained from the file's header.  */
  long int trel_offset;		/* offset to text relocation */
  unsigned int trel_size;	/* size of text relocation */
  long int drel_offset;		/* offset to data relocation */
  unsigned int drel_size;	/* size of data relocation */
  long int syms_offset;		/* offset to the symbol table */
  unsigned int syms_size;	/* size of the symbol table */
  long int strs_offset;		/* offset to the string table */
  unsigned int strs_size;	/* size of the string table */

  int ss_size;			/* size, in bytes, of symbols_and_strings data */
  struct nlist *symbols_and_strings;

  /* offset of the symtab_command in a mach-O file's header */
  long int symtab_cmd_offset;
};

struct file_entry *file_table;

/* Descriptor on which current file is open.  */
int input_desc;

/* Stream for writing that file using stdio.  */
FILE *outstream;

enum strip_action
{
  strip_undef,
  strip_all,			/* strip all symbols */
  strip_debug			/* strip all debugger symbols */
};

/* Which symbols to remove. */
enum strip_action strip_symbols;

enum locals_action
{
  locals_undef,
  locals_start_L,		/* discard locals starting with L */
  locals_all			/* discard all locals */
};

/* Which local symbols to remove. */
enum locals_action discard_locals;

/* The name this program was run with. */
char *program_name;

#ifdef __STDC__
#include <stdlib.h>
#else
char *malloc ();
#endif

char *concat ();
char *xmalloc ();
int file_open ();
int read_entry_symbols ();
int read_file_symbols ();
int read_header ();
void count_file_symbols ();
void error ();
void file_close ();
void rewrite_file_symbols ();
void strip_file ();
void usage ();

struct option long_options[] =
{
  {"strip-all", 0, 0, 's'},
  {"strip-debug", 0, 0, 'S'},
  {"discard-all", 0, 0, 'x'},
  {"discard-locals", 0, 0, 'X'},
  {0, 0, 0, 0}
};

void
main (argc, argv)
     char **argv;
     int argc;
{
  int c;
  int ind;
  int i;
  struct file_entry *p;

  program_name = argv[0];

  strip_symbols = strip_undef;	/* default is to strip everything.  */
  discard_locals = locals_undef;

  while ((c = getopt_long (argc, argv, "gsSxX", long_options, &ind)) != EOF) 
    {
      switch (c)
	{
	case  0 :
	  break;
	case 's':
	  strip_symbols = strip_all;
	  break;
	case 'g':
	case 'S':
	  strip_symbols = strip_debug;
	  break;
	case 'x':
	  discard_locals = locals_all;
	  break;
	case 'X':
	  discard_locals = locals_start_L;
	  break;
	default:
	  usage ();
	}
    }

  /* Default is to strip all symbols.  */
  if (strip_symbols == strip_undef && discard_locals == locals_undef)
    strip_symbols = strip_all;

  number_of_files = argc - optind;

  if (!number_of_files)
    usage ();

  p = file_table
    = (struct file_entry *) xmalloc (number_of_files * sizeof (struct file_entry));

  /* Now fill in file_table */

  for (i = 0; i < number_of_files; i++)
    {
      p->filename = argv[i + optind];
      p->filetype = IS_UNKNOWN;
      p->trel_offset = p->trel_size = p->drel_offset = p->drel_size = 0;
      p->syms_offset = p->syms_size = p->strs_offset = p->strs_size = 0;
      p->symbols_and_strings = 0;
      p->symtab_cmd_offset = 0;
      p++;
    }

  for (i = 0; i < number_of_files; i++)
    strip_file (&file_table[i]);
  exit (0);
}

int delayed_signal;

void
delay_signal (signo)
     int signo;
{
  delayed_signal = signo;
  signal (signo, delay_signal);
}

/* process one input file */

void
strip_file (entry)
     struct file_entry *entry;
{
  int val;
  int sigint_handled = 0;
  int sighup_handled = 0;
  int sigterm_handled = 0;

  local_sym_count = 0;
  non_L_local_sym_count = 0;
  debugger_sym_count = 0;
  global_sym_count = 0;

  val = file_open (entry);
  if (val < 0)
    return;

  if (strip_symbols != strip_all)
    /* Read in the existing symbols unless we are discarding everything.  */
    {
      if (read_file_symbols (entry) < 0)
	return;
    }

  /* Effectively defer handling of asynchronous kill signals.  */
  delayed_signal = 0;
  if (signal (SIGINT, SIG_IGN) != SIG_IGN)
    sigint_handled = 1, signal (SIGINT, delay_signal);
  if (signal (SIGHUP, SIG_IGN) != SIG_IGN)
    sighup_handled = 1, signal (SIGHUP, delay_signal);
  if (signal (SIGTERM, SIG_IGN) != SIG_IGN)
    sigterm_handled = 1, signal (SIGTERM, delay_signal);

  /* Change the file.  */

  rewrite_file_symbols (entry);
  if (strip_symbols != strip_all)
    free (entry->symbols_and_strings);

  file_close ();

  /* Effectively undefer handling.  */
  if (sigint_handled)
    signal (SIGINT, SIG_DFL);
  if (sighup_handled)
    signal (SIGHUP, SIG_DFL);
  if (sigterm_handled)
    signal (SIGTERM, SIG_DFL);

  /* Handle any signal that came in while they were deferred.  */
  if (delayed_signal)
    kill (getpid (), delayed_signal);
}

/** Convenient functions for operating on one or all files being processed.  */

/* Close the file that is now open.  */

void
file_close ()
{
  if (input_desc != -1)
    close (input_desc);
  input_desc = -1;
}

/* Open the file specified by 'entry', and return a descriptor,
   or -1 if the file cannot be opened or is not in rel format.
   The descriptor is also saved in input_desc.  */

int
file_open (entry)
     struct file_entry *entry;
{
  int desc;

  desc = open (entry->filename, O_RDWR, 0);

  if (desc > 0)
    {
      input_desc = desc;
      if (read_header (desc, entry) < 0)
	{
	  close (desc);
	  return -1;
	}
      return desc;
    }

  error (0, errno, "%s", entry->filename);
  return -1;
}

/* Validate file ENTRY and read its symbol and string sections into core.
   Return 0 if ok, -1 if error. */

int
read_file_symbols (entry)
     struct file_entry *entry;
{
  if (read_entry_symbols (input_desc, entry) < 0)
    {
      file_close ();
      return -1;
    }
  count_file_symbols (entry);
  return 0;
}

/* Read a file's header and fill in various fields of a file's entry.
   Return -1 on failure, 0 if successful.  */

int
read_header (desc, entry)
     int desc;
     struct file_entry *entry;
{
  int len;

#ifdef A_OUT
  {
    struct exec hdr;

    lseek (desc, 0, 0);
#ifdef HEADER_SEEK_FD
    /* Skip the headers that encapsulate our data in some other format
       such as COFF.  */
    HEADER_SEEK_FD (desc);
#endif
    len = read (desc, (char *) &hdr, sizeof (struct exec));
    if (len == sizeof (struct exec) && !N_BADMAG (hdr))
      {
	entry->filetype = IS_A_OUT;
#ifdef N_TRELOFF
	entry->trel_offset = N_TRELOFF (hdr);
#else
#ifdef N_DATOFF
	entry->trel_offset = N_DATOFF (hdr) + hdr.a_data;
#else
	entry->trel_offset = N_TXTOFF (hdr) + hdr.a_text + hdr.a_data;
#endif
#endif
	entry->trel_size = hdr.a_trsize;
#ifdef N_DRELOFF
	entry->drel_offset = N_DRELOFF (hdr);
#else
	entry->drel_offset = entry->trel_offset + entry->trel_size;
#endif
	entry->drel_size = hdr.a_drsize;
	entry->syms_offset = N_SYMOFF(hdr);
	entry->syms_size = hdr.a_syms;
	entry->strs_offset = N_STROFF(hdr);
	lseek(desc, entry->strs_offset, 0);
	if (read (desc, (char *) &entry->strs_size, sizeof entry->strs_size)
	    != sizeof entry->strs_size)
	  {
	    error (0, errno, "%s: cannot read string table size",
		   entry->filename);
	    return -1;
	  }
	return 0;
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
    struct symtab_command *symtab_command;
    int symtab_seen;
    int len, cmd, seg;

    symtab_seen = 0;

    lseek (desc, 0L, 0);
    len = read (desc, (char *) &mach_header, sizeof (struct mach_header));
    if (len == sizeof (struct mach_header) && mach_header.magic == MH_MAGIC)
      {
	entry->filetype = IS_MACH_O;
	hdrbuf = xmalloc (mach_header.sizeofcmds);
	len = read (desc, hdrbuf, mach_header.sizeofcmds);
	if (len != mach_header.sizeofcmds)
	  {
	    error (0, errno, "%s: cannot read Mach-O load commands",
		   entry->filename);
	    return -1;
	  }
	load_command = (struct load_command *) hdrbuf;
	for (cmd = 0; cmd < mach_header.ncmds; ++cmd)
	  {
	    switch (load_command->cmd)
	      {
	      case LC_SEGMENT:
		segment_command = (struct segment_command *) load_command;
		section = (struct section *) ((char *) (segment_command + 1));
		for (seg = 0; seg < segment_command->nsects; ++seg, ++section)
		  {
		    if (!strncmp(SECT_TEXT, section->sectname, sizeof section->sectname))
		      {
			entry->trel_offset = section->reloff;
			entry->trel_size = section->nreloc * sizeof (struct relocation_info);
		      }
		    else if (!strncmp(SECT_DATA, section->sectname, sizeof section->sectname))
		      {
			entry->drel_offset = section->reloff;
			entry->drel_size = section->nreloc * sizeof (struct relocation_info);
		      }
		  }
		break;
	      case LC_SYMTAB:
		if (symtab_seen)
		  error (0, 0, "%s: more than one LC_SYMTAB", entry->filename);
		else
		  {
		    symtab_seen = 1;
		    symtab_command = (struct symtab_command *) load_command;
		    entry->syms_offset = symtab_command->symoff;
		    entry->syms_size = symtab_command->nsyms * sizeof (struct nlist);
		    entry->strs_offset = symtab_command->stroff;
		    entry->strs_size = symtab_command->strsize;
		    entry->symtab_cmd_offset = (char *) load_command - hdrbuf
		      + sizeof (struct mach_header);
		  }
		break;
	      }
	    load_command = (struct load_command *)
	      ((char *) load_command + load_command->cmdsize);
	  }

	free (hdrbuf);

	if (!symtab_seen)
	  {
	    error (0, 0, "%s: no symbol table", entry->filename);
	    return -1;
	  }

	return 0;
      }
  }
#endif

  error (0, 0, "%s: not an executable or object file", entry->filename);
  return -1;
}

/* Read the symbols and strings of file ENTRY into core.
   Assume it is already open, on descriptor DESC.
   Return -1 on failure, 0 if successful.  */

int
read_entry_symbols (desc, entry)
     struct file_entry *entry;
     int desc;
{
  entry->ss_size = entry->syms_size + entry->strs_size;
  entry->symbols_and_strings = (struct nlist *) xmalloc (entry->ss_size);

  lseek (desc, entry->syms_offset, 0);
  if (entry->ss_size != read (desc, entry->symbols_and_strings, entry->ss_size))
    {
      error (0, errno, "%s: premature end of file in symbols/strings",
	     entry->filename);
      return -1;
    }
  return 0;
}

/* Count the number of symbols of various categories in the file of ENTRY.  */

void
count_file_symbols (entry)
     struct file_entry *entry;
{
  struct nlist *p, *end = entry->symbols_and_strings + entry->syms_size / sizeof (struct nlist);
  char *name_base = entry->syms_size + (char *) entry->symbols_and_strings;

  for (p = entry->symbols_and_strings; p < end; p++)
    if (p->n_type & N_EXT)
      global_sym_count++;
    else if (p->n_un.n_strx && !(p->n_type & (N_STAB | N_EXT)))
      {
	if ((p->n_un.n_strx + name_base)[0] != LPREFIX)
	  non_L_local_sym_count++;
	local_sym_count++;
      }
    else
      debugger_sym_count++;
}

void modify_relocation ();
void write_file_syms ();

/* Total size of string table strings allocated so far */
int strtab_size;

/* Vector whose elements are the strings to go in the string table */
char **strtab_vector;

/* Index in strtab_vector at which the next string will be stored */
int strtab_index;

int sym_written_count;

int
assign_string_table_index (name)
     char *name;
{
  int index = strtab_size;

  strtab_size += strlen (name) + 1;
  strtab_vector[strtab_index++] = name;

  return index;
}

void
rewrite_file_symbols (entry)
     struct file_entry *entry;
{
  int i;
  struct nlist *newsyms;

  /* Calculate number of symbols to be preserved.  */

  if (strip_symbols == strip_all)
    nsyms = 0;
  else
    {
      nsyms = global_sym_count;
      if (discard_locals == locals_start_L)
	nsyms += non_L_local_sym_count;
      else if (discard_locals == locals_undef)
	nsyms += local_sym_count;
    }

  if (strip_symbols == strip_undef)
    nsyms += debugger_sym_count;

  strtab_vector = (char **) xmalloc (nsyms * sizeof (char *));
  strtab_index = 0;

  strtab_size = 4;

  /* Accumulate in 'newsyms' the symbol table to be written.  */

  newsyms = (struct nlist *) xmalloc (nsyms * sizeof (struct nlist));

  sym_written_count = 0;

  if (strip_symbols != strip_all)
    /* Write into newsyms the symbols we want to keep.  */
    write_file_syms (entry, newsyms);

  if (sym_written_count != nsyms)
    {
      fprintf (stderr, "written = %d, expected = %d\n",
	       sym_written_count, nsyms);
      abort ();
    }

  /* Modify the symbol-numbers in the relocation in the file,
     to preserve its meaning */
  modify_relocation (input_desc, entry);

#ifndef	HAVE_FTRUNCATE
  {
    int size = entry->syms_offset, mode;
    int old_desc;
    char *renamed;
    char *copy_buffer = (char *)xmalloc (size);
    struct stat statbuf;

    renamed = (char *) xmalloc(strlen(entry->filename) + 3);
    strcpy(renamed, entry->filename);
    {
      char *file_p, *renamed_p;
      file_p = strrchr(entry->filename, '/');
      file_p = file_p ? ++file_p : entry->filename;
      renamed_p = renamed + (file_p - entry->filename);
      *renamed_p++ = '~';
      while (*renamed_p++ = *file_p++)
	;
      *renamed_p++ = '~';
      *renamed_p = '\0';
    }

    lseek (input_desc, 0, 0);
    if (read (input_desc, copy_buffer, size) != size)
      {
	error (0, errno, "%s: cannot read up to symbol table",
	       entry->filename);
	return;
      }
    mode = fstat (input_desc, &statbuf) ? 0666 : statbuf.st_mode;
    if (rename (entry->filename, renamed))
      {
	error (0, errno, "%s", entry->filename);
	return;
      }
    old_desc = input_desc;
    input_desc = open (entry->filename, O_RDWR | O_CREAT | O_TRUNC, mode);
    if (input_desc < 0)
      {
	error (0, errno, "%s", entry->filename);
	return;
      }
    if (write (input_desc, copy_buffer, size) != size)
      error (0, errno, "%s", entry->filename);
    if (unlink (renamed))
      error (0, errno, "%s", renamed);
    if (close (old_desc))
      error (0, errno, "%s", renamed);
    free (copy_buffer);
    free (renamed);
  }
#endif /* not HAVE_FTRUNCATE */

  /* Now write contents of NEWSYMS into the file. */

  lseek (input_desc, entry->syms_offset, 0);
  write (input_desc, newsyms, nsyms * sizeof (struct nlist));
  free (newsyms);

  /* Now write the string table.  */

  {
    char *strvec = (char *) xmalloc (strtab_size);
    char *p;

    *((long *) strvec) = strtab_size;

    p = strvec + sizeof (long);

    for (i = 0; i < strtab_index; i++)
      {
	int len = strlen (strtab_vector[i]);
	strcpy (p, strtab_vector[i]);
	*(p+len) = 0;
	p += len + 1;
      }

    write (input_desc, strvec, strtab_size);
    free (strvec);
  }

  /* Adjust file to be smaller */

#ifdef HAVE_FTRUNCATE
  if (ftruncate (input_desc, lseek (input_desc, 0L, 1)) < 0)
    error (0, errno, "%s", entry->filename);
#endif

  /* Write new symbol table size into file header.  */

#ifdef A_OUT
  if (entry->filetype == IS_A_OUT)
    {
      struct exec hdr;

      lseek (input_desc, 0L, 0);
#ifdef HEADER_SEEK_FD
      HEADER_SEEK_FD (input_desc);
#endif
      read (input_desc, (char *) &hdr, sizeof hdr);
      hdr.a_syms = nsyms * sizeof (struct nlist);
      lseek (input_desc, -(long) sizeof hdr, 1);
      write (input_desc, (char *) &hdr, sizeof hdr);
    }
#endif

#ifdef MACH_O
  if (entry->filetype == IS_MACH_O)
    {
      struct symtab_command cmd;

      lseek (input_desc, entry->symtab_cmd_offset, 0);
      read (input_desc, (char *) &cmd, sizeof cmd);
      cmd.nsyms = nsyms;
      cmd.stroff = cmd.symoff + cmd.nsyms * sizeof (struct nlist);
      cmd.strsize = strtab_size;
      lseek (input_desc, entry->symtab_cmd_offset, 0);
      write (input_desc, (char *) &cmd, sizeof cmd);
    }
#endif

  free (strtab_vector);
}

/* Copy into NEWSYMS the symbol entries to be preserved.
   Count them in sym_written_count.

   We record, for each symbol written, its symbol number in the resulting file.
   This is so that the relocation can be updated later.
   Since the symbol names will not be needed again,
   this index goes in the `n_strx' field.
   If a symbol is not written, -1 is stored there.  */

void
write_file_syms (entry, newsyms)
     struct file_entry *entry;
     struct nlist *newsyms;
{
  struct nlist *p = entry->symbols_and_strings;
  struct nlist *end = p + entry->syms_size / sizeof (struct nlist);
  char *string_base = (char *) end;   /* address of start of file's string table */
  struct nlist *outp = newsyms;

  for (; p < end; p++)
    {
      int write;
  
      if (p->n_type & N_EXT)
	write = 1;
      else if (p->n_un.n_strx && !(p->n_type & (N_STAB | N_EXT)))
	/* ordinary local symbol */
	write = (discard_locals != locals_all)
		&& !(discard_locals == locals_start_L &&
		     (p->n_un.n_strx + string_base)[0] == LPREFIX);
      else
	/* debugger symbol */
	write = (strip_symbols == strip_undef);

      if (write)
	{
	  if (p->n_un.n_strx)
	    p->n_un.n_strx = assign_string_table_index (p->n_un.n_strx + string_base);

	  *outp++ = *p;

	  p->n_un.n_strx = sym_written_count++;
	}
      else p->n_un.n_strx = -1;
    }
}

/* Read in ENTRY's relocation, alter the symbolnums in it,
   and write it out again.  */

void
modify_relocation (desc, entry)
     int desc;
     struct file_entry *entry;
{
  relocation_info_ptr reloc, p, end;
  int size;
  struct nlist *sym_base = (struct nlist *) entry->symbols_and_strings;
  int losing = 0;
  long int offsets[2];
  unsigned int sizes[2];
  int i;

  offsets[0] = entry->trel_offset;
  sizes[0] = entry->trel_size;
  offsets[1] = entry->drel_offset;
  sizes[1] = entry->drel_size;

  for (i = 0; i < 2; ++i)
    {
      size = sizes[i];
      reloc = (relocation_info_ptr) xmalloc (size);
      lseek (desc, offsets[i], 0);
      read (desc, reloc, size);

      p = reloc;
      end = (relocation_info_ptr) (size + (char *) reloc);
      while (p < end)
	{
	  if (p->r_extern)
	    {
	      int newnum = (sym_base == 0 ? -1
			    :((sym_base + RELOCATION_INFO_SYMBOL_NUM(p))
			      -> n_un.n_strx));
	      if (newnum < 0)
		{
		  if (losing == 0)
		    error (0, 0, "%s: warning: file is now unlinkable",
			   entry->filename);
		  losing = 1;
		}
	      RELOCATION_INFO_SYMBOL_NUM(p) = newnum;
	    }
	  p++;
	}

      lseek (desc, offsets[i], 0);
      write (desc, reloc, size);
      free ((char *) reloc);
    }
}

/* Return a newly-allocated string whose contents 
   concatenate those of S1, S2, S3.  */

char *
concat (s1, s2, s3)
     char *s1, *s2, *s3;
{
  int len1 = strlen (s1), len2 = strlen (s2), len3 = strlen (s3);
  char *result = (char *) xmalloc (len1 + len2 + len3 + 1);

  strcpy (result, s1);
  strcpy (result + len1, s2);
  strcpy (result + len1 + len2, s3);
  *(result + len1 + len2 + len3) = 0;

  return result;
}

/* Like malloc but get fatal error if memory is exhausted.  */

char *
xmalloc (size)
     unsigned size;
{
  /* Some implementations of malloc get unhappy if size==0.
   * Given that the code sometimes "wants" a 0-length array,
   * it seems cleaner to put a work-around here
   * than clutter up the code logic in various other places. */
#if 0 
  char *result = malloc (size ? size : 1);
#else
  char *result = malloc (size);
#endif

  if (!result)
    error (1, 0, "virtual memory exhausted");
  return result;
}


#ifndef HAVE_RENAME
int
rename (from, to)
     char *from, *to;
{
  unlink (to);
  if (link (from, to) < 0 || unlink (from) < 0)
    return -1;
  else
    return 0;
}
#else
#ifdef ATT
int Error;
#endif
#endif

void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-gsxSX] [+strip-all] [+strip-debug] [+discard-all]\n\
       [+discard-locals] file...\n", program_name);
  exit (1);
}
