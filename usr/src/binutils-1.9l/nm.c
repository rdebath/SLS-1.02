/* Describe symbol table of a rel file.
   Copyright (C) 1986, 1988, 1990 Free Software Foundation, Inc.

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
#define AR_H	<ar.h>
#endif
#ifndef A_OUT_H
#define A_OUT_H	<a.out.h>
#endif

#include <stdio.h>
#include AR_H
#include <errno.h>
#include <sys/types.h>
#include <sys/file.h>
#include "getopt.h"

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
#ifndef N_TEXT
#define N_TEXT 4
#define N_DATA 6
#define N_BSS 8
#endif
#ifndef N_FN
#define N_FN 15
#endif
#endif
#include <sys/loader.h>
#endif

/* Always use the GNU version of debugging symbol type codes, if possible.  */
#include "stab.h"

/* Struct or union for header of object file.  */

#ifdef USG
#include <string.h>
/* You might need to compile with -I/usr/include/sys if your fcntl.h
   isn't in /usr/include (which is where it should be according to POSIX).  */
#include <fcntl.h>
#else
#include <strings.h>
#endif

/* Alloca definitions and includes...  */

/* If compiled with GNU C, use the built-in alloca */
#ifdef __GNUC__
# ifndef alloca 
#  define alloca __builtin_alloca
# endif
#else
/* If on a sun 4, make sure to include the right definition.  */
#if defined(sun) && defined(sparc)
#include "alloca.h"
#else
char *alloca ();
#endif
#endif

#ifdef _STDC_
#include <stdlib.h>
#else
char *malloc (), *realloc ();
#endif

char *xmalloc (), *xrealloc ();

/* C++ demangler stuff.  */
char *cplus_demangle ();

int no_cplus;		/* don't cplus; print symbols as it is.  */

/* Print NAME on STREAM, demangling if necessary.  */
void
fprint_name (stream, name)
     FILE *stream;
     char *name;
{
  if (no_cplus)
      fputs (name, stream);
  else {
    char *demangled = cplus_demangle (name);
    if (demangled == NULL)
      fputs (name, stream);
    else {
      fputs (demangled, stream);
      free (demangled);
    }
  }
}

/* Special global symbol types understood by GNU LD.  */

/* The following type indicates the definition of a symbol as being
   an indirect reference to another symbol.  The other symbol
   appears as an undefined reference, immediately following this symbol.

   Indirection is asymmetrical.  The other symbol's value will be used
   to satisfy requests for the indirect symbol, but not vice versa.
   If the other symbol does not have a definition, libraries will
   be searched to find a definition.  */
#ifndef N_INDR
#define N_INDR 0xa
#endif

/* Warning message symbol.  The name of this symbol will be printed if
   any symbols from its file are required by the linker.  */
#ifndef N_WARNING
#define N_WARNING 0x1e
#endif
   

/* The following symbols refer to set elements.
   All the N_SET[ATDB] symbols with the same name form one set.
   Space is allocated for the set in the text section, and each set
   element's value is stored into one word of the space.
   The first word of the space is the length of the set (number of elements).

   The address of the set is made into an N_SETV symbol
   whose name is the same as the name of the set.
   This symbol acts like a N_DATA global symbol
   in that it can satisfy undefined external references.  */

#ifndef N_SETA
#define	N_SETA	0x14		/* Absolute set element symbol */
#endif				/* This is input to LD, in a .o file.  */

#ifndef N_SETT
#define	N_SETT	0x16		/* Text set element symbol */
#endif				/* This is input to LD, in a .o file.  */

#ifndef N_SETD
#define	N_SETD	0x18		/* Data set element symbol */
#endif				/* This is input to LD, in a .o file.  */

#ifndef N_SETB
#define	N_SETB	0x1A		/* Bss set element symbol */
#endif				/* This is input to LD, in a .o file.  */

/* Macros dealing with the set element symbols defined in a.out.h */
#define	SET_ELEMENT_P(x)	((x)>=N_SETA&&(x)<=(N_SETB|N_EXT))
#define TYPE_OF_SET_ELEMENT(x)	((x)-N_SETA+N_ABS)

#ifndef N_SETV
#define N_SETV	0x1C		/* Pointer to set vector in text area.  */
#endif				/* This is output from LD.  */

#ifndef __GNU_STAB__

/* Line number for the data section.  This is to be used to describe
   the source location of a variable declaration.  */
#ifndef N_DSLINE
#define N_DSLINE (N_SLINE+N_DATA-N_TEXT)
#endif

/* Line number for the bss section.  This is to be used to describe
   the source location of a variable declaration.  */
#ifndef N_BSLINE
#define N_BSLINE (N_SLINE+N_BSS-N_TEXT)
#endif

#endif /* not __GNU_STAB__ */

/* Name of this program.  */

char *program_name;

/* Number of input files specified.  */

int number_of_files;

/* Current file's name.  */

char *input_name;

/* Current member's name, or 0 if processing a non-library file.  */

char *input_member;

#ifdef MACH_O
/* Section ordinals for N_SECT references.  */
int text_section;
int data_section;
int bss_section;
#endif

/* Offset within archive of the current member,
   if we are processing an archive.  */

int member_offset;

/* Command options.  */

int external_only;	/* nonzero means print external symbols only.  */
int sort_numerically;   /* sort in numerical order rather than alphabetic  */
int reverse_sort;	/* sort in downward (alphabetic or numeric) order.  */
int no_sort;		/* don't sort; print symbols in order in the file.  */
int undefined_only;	/* print undefined symbols only.  */
int file_on_each_line;	/* print file name on each line.  */
int debugger_syms;	/* print the debugger-only symbols.  */
int print_symdefs;	/* describe the __.SYMDEF data in any archive file specified.  */

/* The __.SYMDEF member of an archive has the following format:
   1) A longword saying the size of the symdef data that follows
   2) Zero or more  struct symdef  filling that many bytes
   3) A longword saying how many bytes of strings follow
   4) That many bytes of string data.
*/

struct symdef
  {
    long stringoffset;	/* Offset of this symbol's name in the string data */
    long offset;	/* Offset in the archive of the header-data for the member that
			   defines this symbol.  */
  };

/* Create a table of debugging stab-codes and corresponding names.  */
#ifdef __GNU_STAB__
#define __define_stab(NAME, CODE, STRING) {NAME, STRING},
struct {enum __stab_debug_code code; char *string;} stab_names[]
  = {
#include "stab.def"
    };
#undef __define_stab
#endif

char *concat ();
int decode_arg ();
void decode_switch ();
void do_one_file ();
void do_one_rel_file ();
void do_symdef_member ();
void error ();
void error_with_file ();
void fatal ();
void perror_name ();
void print_file_name ();

void
main (argc, argv)
     char **argv;
     int argc;
{
  int i;
  int c;
  int ind;
  static struct option long_options[] = 
    {
      {"all",		0, &debugger_syms,	1},
      {"extern-only",	0, &external_only,	1},
      {"numeric-sort",	0, &sort_numerically,	1},
      {"print-file-name", 0, &file_on_each_line,1},
      {"no-sort",	0, &no_sort,		1},
      {"no-cplus",	0, &no_cplus,		1},
      {"reverse",	0, &reverse_sort,	1},
      {"print-symdefs",	0, &print_symdefs,	1},
      {"undefined-only",0, &undefined_only,	1},
      {NULL, 0, NULL, 0}
    };

  program_name = argv[0];

  number_of_files = 0;
  external_only = 0;
  sort_numerically = 0;
  reverse_sort = 0;
  no_sort = 0;
  no_cplus= 0;
  undefined_only = 0;
  file_on_each_line = 0;
  debugger_syms = 0;
  print_symdefs = 0;

  while ((c = getopt_long (argc, argv, "adgnoprsu", long_options, &ind)) != EOF)
    switch (c)
      {
      case 0:
      	break;
	
      case 'a':
	debugger_syms = 1;
	break;

      case 'd':
	no_cplus = 1;
	break;

      case 'g':
	external_only = 1;
	break;

      case 'n':
	sort_numerically = 1;
	break;

      case 'o':
	file_on_each_line = 1;
	break;

      case 'p':
	no_sort = 1;
	break;

      case 'r':
	reverse_sort = 1;
	break;

      case 's':
	print_symdefs = 1;
	break;

      case 'u':
	undefined_only = 1;
	break;

      default:
	fprintf (stderr, "\
Usage: %s [-adgnoprsu] [+all] [+extern-only] [+numeric-sort]\n\
       [+print-file-name] [+no-sort] [+no-cplus] [+reverse] [+print-symdefs]\n\
       [+undefined-only] [file...]\n", program_name);
	exit (1);
	break;
      }

  number_of_files = argc - optind;

  /* Now scan again and print the files.  */

  if (argc == optind)
    do_one_file ("a.out");
  else
    for (i = optind; i < argc; i++)
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
  int desc, nchars;
  char armag[SARMAG];

  desc = open (name, O_RDONLY, 0);

  if (desc < 0)
    {
      perror_name (name);
      return;
    }

  input_name = name;
  input_member = 0;

  nchars = read (desc, armag, SARMAG);
  if (nchars == SARMAG && !strncmp(armag, ARMAG, SARMAG))
    scan_library (desc);
  else
    do_one_rel_file (desc, 0);

  close (desc);
}

/* Read in the archive data about one member.
   SUBFILE_OFFSET is the address within the archive of the start of that data.

   Return the length of the member's contents, which does
   not include the archive data about the member.
   A pointer to the member's name is stored into *MEMBER_NAME_PTR.
   If there are no more valid members, return zero.  */

int
decode_library_subfile (desc, subfile_offset, member_name_ptr)
     int desc;
     int subfile_offset;
     char **member_name_ptr;
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
    error_with_file ("malformed library archive ");

  else if (sscanf (hdr1.ar_size, "%d", &member_length) != 1)
    error_with_file ("malformatted header of archive member in ");

  else
    {
      for (namelen = 0; ; namelen++)
	if (hdr1.ar_name[namelen] == 0 || hdr1.ar_name[namelen] == ' '
	    /* Some systems use a slash?  Strange.  */
	    || hdr1.ar_name[namelen] == '/')
	  break;

      name = (char *) xmalloc (namelen+1);
      strncpy (name, hdr1.ar_name, namelen);
      name[namelen] = 0;

      *member_name_ptr = name;

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

  if (!file_on_each_line)
    printf ("\n%s:\n", input_name);
  
  while (1)
    {
      member_length
	= decode_library_subfile (desc, this_subfile_offset, &input_member);
      if (member_length == 0)
	break;

      /* Describe every member except the ranlib data if any.  */

      if (strcmp (input_member, "__.SYMDEF"))
	do_one_rel_file (desc, this_subfile_offset + sizeof (struct ar_hdr));
      else if (print_symdefs)
	do_symdef_member (desc, this_subfile_offset + sizeof (struct ar_hdr), member_length);

      this_subfile_offset += ((member_length + sizeof (struct ar_hdr)) + 1) & -2;
    }
}

/* Read a file's header and fill in various pieces of information.
   Return 0 on failure.  */

int
read_header_info (desc, offset, syms_offset, syms_size, strs_offset, strs_size)
     int desc;
     long int offset;
     long int *syms_offset;
     unsigned int *syms_size;
     long int *strs_offset;
     unsigned int *strs_size;
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
	*syms_offset = N_SYMOFF(hdr);
	*syms_size = hdr.a_syms;
	*strs_offset = N_STROFF(hdr);
	lseek(desc, N_STROFF(hdr) + offset, 0);
	if (read (desc, (char *) strs_size, sizeof *strs_size) != sizeof *strs_size)
	  {
	    error_with_file ("cannot read string table size in ");
	    return 0;
	  }
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
    struct symtab_command *symtab_command;
    int symtab_seen;
    int len, cmd, seg, ordinal;

    symtab_seen = 0;

    lseek (desc, offset, 0);
    len = read (desc, (char *) &mach_header, sizeof (struct mach_header));
    if (len == sizeof (struct mach_header) && mach_header.magic == MH_MAGIC)
      {
	hdrbuf = xmalloc (mach_header.sizeofcmds);
	len = read (desc, hdrbuf, mach_header.sizeofcmds);
	if (len != mach_header.sizeofcmds)
	  {
	    error_with_file ("failure reading Mach-O load commands in ");
	    return 0;
	  }
	load_command = (struct load_command *) hdrbuf;
	ordinal = 1;
	for (cmd = 0; cmd < mach_header.ncmds; ++cmd)
	  {
	    switch (load_command->cmd)
	      {
	      case LC_SEGMENT:
		segment_command = (struct segment_command *) load_command;
		section = (struct section *) ((char *) (segment_command + 1));
		for (seg = 0; seg < segment_command->nsects; ++seg, ++section, ++ordinal)
		  {
		    if (!strncmp(SECT_TEXT, section->sectname, sizeof section->sectname))
		      text_section = ordinal;
		    else if (!strncmp(SECT_DATA, section->sectname, sizeof section->sectname))
		      data_section = ordinal;
		    else if (!strncmp(SECT_BSS, section->sectname, sizeof section->sectname))
		      bss_section = ordinal;
		  }
		break;
	      case LC_SYMTAB:
		if (symtab_seen)
		  error_with_file ("more than one LC_SYMTAB in ");
		else
		  {
		    symtab_seen = 1;
		    symtab_command = (struct symtab_command *) load_command;
		    *syms_offset = symtab_command->symoff;
		    *syms_size = symtab_command->nsyms * sizeof (struct nlist);
		    *strs_offset = symtab_command->stroff;
		    *strs_size = symtab_command->strsize;
		  }
		break;
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

void print_symbols ();
void print_one_symbol ();
void read_header ();
int alphacompare ();
int valuecompare ();
int filter_symbols ();

void
do_one_rel_file (desc, offset)
     int desc;
     int offset;
{
  struct nlist *symbols_and_strings;
  int symcount;
  int totalsize;
  char *strings;
  long int syms_offset, strs_offset;
  unsigned int syms_size, strs_size;

  if (!read_header_info (desc, offset, &syms_offset, &syms_size, &strs_offset, &strs_size))
    {
      error_with_file ("malformed input (not a rel file or archive) in ");
      return;
    }

  /* Number of symbol entries in the file.  */
  symcount = syms_size / sizeof (struct nlist);

  totalsize = strs_size + syms_size;

  /* Allocate space for symbol entries and string table.  */
  symbols_and_strings = (struct nlist *) xmalloc (totalsize);
  strings = (char *) symbols_and_strings + syms_size;

  /* Read them both in.  */
  lseek (desc, syms_offset + offset, 0);
  if (syms_size != read (desc, (char *) symbols_and_strings, syms_size))
    {
      error_with_file ("premature end of file in symbols of ");
      return;
    }
  lseek (desc, strs_offset + offset, 0);
  if (strs_size != read (desc, (char *) strings, strs_size))
    {
      error_with_file ("premature end of file in strings of ");
      return;
    }

  /* Identify this file, if desired.  */

  if (!file_on_each_line && (number_of_files > 1 || input_member))
    printf ("\n%s:\n", input_member ? input_member : input_name);

  /* Discard the symbols we don't want to print; compact the rest down.  */

  symcount = filter_symbols (symbols_and_strings, symcount, strings);
    
  /* Modify each symbol entry to point directly at the symbol name.
     This is so the sort routine does not need to be passed
     the value of `strings' separately.  */

  {
    struct nlist *p = symbols_and_strings;
    struct nlist *end = symbols_and_strings + symcount;

    for (; p < end; p++)
      {
	/* A zero index means there is no string.  */
	if (p->n_un.n_strx != 0)
	  {
	    if (p->n_un.n_strx > 0 && p->n_un.n_strx < strs_size)
	      p->n_un.n_name = strings + p->n_un.n_strx;
	    else
	      {
		error_with_file ("invalid string table offset in ");
		return;
	      }
	  }
      }
  }

  /* Sort the symbols if desired.  */

  if (!no_sort)
    qsort (symbols_and_strings, symcount, sizeof (struct nlist),
	   sort_numerically ? valuecompare : alphacompare);

  /* Print the symbols in the order they are now in.  */

  print_symbols (symbols_and_strings, symcount);

  free (symbols_and_strings);
}

/* Choose which symbol entries to print;
   compact them downward to get rid of the rest.
   Return the number of symbols to be printed.  */

int
filter_symbols (syms, symcount, strings)
     struct nlist *syms;
     int symcount;
     char *strings;
{
  struct nlist *from = syms, *to = syms;
  struct nlist *end = syms + symcount;

  while (from < end)
    {
      int keep = 0;

      /* undefined sym or common sym */
      if (from->n_type == N_EXT) keep = !undefined_only || !from->n_value;
      /* global defined sym */
      else if (from->n_type & N_EXT) keep = !undefined_only;
      /* debugger sym: normally don't print */
      else if (from->n_type & ~(N_TYPE | N_EXT)) keep = debugger_syms;
      /* local sym */
      else keep = !external_only && !undefined_only;

      if (keep)
	*to++ = *from;
      from++;
    }

  return to - syms;
}

/* Comparison functions for sorting symbols.  */

int
alphacompare (sym1, sym2)
     struct nlist *sym1, *sym2;
{
  if (reverse_sort)
    {
      if (!sym2->n_un.n_name)
	{
	  if (sym1->n_un.n_name) return -1;
	  else return 0;
	}
      if (!sym1->n_un.n_name) return 1;
      return strcmp (sym2->n_un.n_name, sym1->n_un.n_name);
    }
  else
    {
      if (!sym1->n_un.n_name)
	{
	  if (sym2->n_un.n_name) return -1;
	  else return 0;
	}
      if (!sym2->n_un.n_name) return 1;
      return strcmp (sym1->n_un.n_name, sym2->n_un.n_name);
    }
}


int
valuecompare (sym1, sym2)
     struct nlist *sym1, *sym2;
{
  if (reverse_sort)
    return sym2->n_value - sym1->n_value;
  else
    return sym1->n_value - sym2->n_value;
}

void
print_symbols (syms, symcount)
     struct nlist *syms;
     int symcount;
{
  int i;

  for (i = 0; i < symcount; i++)
    print_one_symbol (&syms[i]);
}

void
print_one_symbol (sym)
     struct nlist *sym;
{
  if (file_on_each_line)
    {
      print_file_name (stdout);
      printf (":");
    }

  if (undefined_only)
    {
      if (sym->n_type == N_EXT && !sym->n_value)
	{
	  fprint_name (stdout, sym->n_un.n_name);
	  printf ("\n");
	}
      return;
    }

  if (sym->n_type & ~N_EXT || sym->n_value)
    printf ("%08x ", sym->n_value);
  else printf ("         ");

  switch (sym->n_type)
    {
      case N_EXT:
        if (sym->n_value) printf ("C");
        else printf ("U");
	break;

      case 0:
        if (sym->n_value) printf ("c");
        else printf ("u");
	break;

      case N_ABS | N_EXT:
	printf ("A");
	break;

      case N_ABS:
	printf ("a");
	break;

      case N_TEXT | N_EXT:
	printf ("T");
	break;

      case N_TEXT:
	printf ("t");
	break;

      case N_DATA | N_EXT:
	printf ("D");
	break;

      case N_DATA:
	printf ("d");
	break;

      case N_BSS | N_EXT:
	printf ("B");
	break;

      case N_BSS:
	printf ("b");
	break;

      case N_SETV | N_EXT:
	printf ("V");
	break;

      case N_SETV:
	printf ("v");
	break;

      case N_SETA | N_EXT:
	printf ("L");
	break;
	
      case N_SETA:
	printf ("l");
	break;
	
      case N_SETT | N_EXT:
	printf ("X");
	break;
	
      case N_SETT:
	printf ("x");
	break;
	
      case N_SETD | N_EXT:
	printf ("Z");
	break;
	
      case N_SETD:
	printf ("z");
	break;
	
      case N_SETB | N_EXT:
	printf ("S");
	break;
	
      case N_SETB:
	printf ("s");
	break;
	
      case N_INDR | N_EXT:
	printf ("I");
	break;
	
      case N_INDR:
	printf ("i");
	break;
	
      case N_WARNING | N_EXT:
	printf ("W");
	break;
	
      case N_WARNING:
	printf ("w");
	break;
	
#ifdef N_SECT
      case N_SECT:
	if (sym->n_sect == text_section)
	  printf ("t");
	else if (sym->n_sect == data_section)
	  printf ("d");
	else if (sym->n_sect == bss_section)
	  printf ("b");
	else
	  printf ("%d", sym->n_sect);
	break;

      case N_SECT | N_EXT:
	if (sym->n_sect == text_section)
	  printf ("T");
	else if (sym->n_sect == data_section)
	  printf ("D");
	else if (sym->n_sect == bss_section)
	  printf ("B");
	else
	  printf ("%d", sym->n_sect);
	break;
#endif

      default:
	{
	  char *s;
	  int i;
#ifdef __GNU_STAB__
	  s = "";
	  for (i = sizeof (stab_names) / sizeof (stab_names[0]) - 1;
	       i >= 0; i--)
	    {
	      if (stab_names[i].code
		  == (enum __stab_debug_code) sym->n_type)
		{
		  s = stab_names[i].string;
		  break;
		}
	    }
#else /* not __GNU_STAB__ */
	  switch (sym->n_type)
	    {
	    case N_GSYM:
	      s = "GSYM";
	      break;
	    case N_FNAME:
	      s = "FNAME";
	      break;
	    case N_FUN:
	      s = "FUN";
	      break;
	    case N_STSYM:
	      s = "STSYM";
	      break;
	    case N_LCSYM:
	      s = "LCSYM";
	      break;
	    case N_RSYM:
	      s = "RSYM";
	      break;
	    case N_SLINE:
	      s = "SLINE";
	      break;
	    case N_DSLINE:
	      s = "DSLINE";
	      break;
	    case N_BSLINE:
	      s = "BSLINE";
	      break;
	    case N_SSYM:
	      s = "SSYM";
	      break;
	    case N_SO:
	      s = "SO";
	      break;
	    case N_LSYM:
	      s = "LSYM";
	      break;
	    case N_SOL:
	      s = "SOL";
	      break;
	    case N_PSYM:
	      s = "PSYM";
	      break;
	    case N_ENTRY:
	      s = "ENTRY";
	      break;
	    case N_LBRAC:
	      s = "LBRAC";
	      break;
	    case N_RBRAC:
	      s = "RBRAC";
	      break;
	    case N_BCOMM:
	      s = "BCOMM";
	      break;
	    case N_ECOMM:
	      s = "ECOMM";
	      break;
	    case N_ECOML:
	      s = "ECOML";
	      break;
	    case N_LENG:
	      s = "LENG";
	      break;
	    default:
	      s = "";
	    }
#endif /* not __GNU_STAB__ */
	  /* %x treats them as unsigned anyway, so if we didn't cast
	     them we'd get 0xffffffff for all 1's instead of 0xff
	     or 0xffff.  */
	  printf ("- %02x %04x %5s",
#ifdef N_SECT
		  (unsigned char) sym->n_sect,
#else
		  (unsigned char) sym->n_other,
#endif
		  (unsigned short) sym->n_desc, s);
	}
    }

  printf (" ");
  
  if (sym->n_un.n_name)
    fprint_name (stdout, sym->n_un.n_name);
  
  printf ("\n");
}

void
do_symdef_member (desc, offset, member_length)
     int desc;
     int offset;
     int member_length;
{
  int symdef_size;
  int nsymdefs;
  struct symdef *symdefs;
  int stringsize;
  char *strings;
  int i;
  char *member_name;
  int member_offset;

  /* read the string-table-length out of the file */

  lseek (desc, offset, 0);
  if (sizeof symdef_size != read (desc, &symdef_size, sizeof symdef_size))
    {
      error_with_file ("premature eof in ");
      return;
    }

  if (symdef_size < 0)
    {
      error_with_file ("invalid size value in ");
      return;
    }

  nsymdefs = symdef_size / sizeof (struct symdef);
  symdefs = (struct symdef *) alloca (symdef_size);
  if (symdef_size != read (desc, symdefs, symdef_size))
    {
      error_with_file ("premature eof in ");
      return;
    }

  if (stringsize < 0)
    {
      error_with_file ("invalid size value in ");
      return;
    }

  if (sizeof stringsize != read (desc, &stringsize, sizeof stringsize))
    {
      error_with_file ("premature eof in ");
      return;
    }

  strings = (char *) alloca (stringsize);
  if (stringsize != read (desc, strings, stringsize))
    {
      error_with_file ("premature eof in ");
      return;
    }

  if (stringsize + symdef_size + sizeof stringsize + sizeof symdef_size != member_length)
    {
      error_with_file ("size of data isn't what the data calls for in ");
      return;
    }

  if (!file_on_each_line && (number_of_files > 1 || input_member))
    printf ("\n%s:\n", input_member ? input_member : input_name);
    
  member_offset = -1;
  for (i = 0; i < nsymdefs; i++)
    {
      if (symdefs[i].stringoffset < 0 || symdefs[i].stringoffset >= stringsize)
	{
	  error_with_file ("invalid entry in ");
	  return;
	}
      if (member_offset != symdefs[i].offset)
	{
	  member_offset = symdefs[i].offset;
	  decode_library_subfile (desc, member_offset, &member_name);
	}
      if (file_on_each_line)
	{
	  print_file_name (stdout);
	  printf (":");
	}
      printf ("%s in %s\n", symdefs[i].stringoffset + strings, member_name);
    }
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
  fprintf (stderr, string);
  print_file_name (stderr);
  fprintf (stderr, "\n");
}

/* Report a fatal error using the message for the last failed system call,
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
    s = concat ("", sys_errlist[errno], " for %s");
  else
    s = "cannot open %s";
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

/* Like realloc but get fatal error if out of memory.  */

char *
xrealloc (p, size)
     char *p;
     unsigned size;
{
  char *result = realloc(p, size);

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

#if defined(USG) || defined(OLD_LINUX)

getpagesize ()
{
  return (4096);
}

#endif
