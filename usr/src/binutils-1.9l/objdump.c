/* objdump -- print information about an object file.
   Copyright (C) 1988, 1990 Free Software Foundation, Inc.

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
#define A_OUT_H	<a.out.h>
#endif

#include <stdio.h>
#include <errno.h>
#ifndef linux
extern int errno;
#endif
#include "getopt.h"

#ifndef COFF_ENCAPSULATE
#include A_OUT_H
/* Tell a.out.gnu.h not to try to redefine some things.  */
#define N_NLIST_DECLARED
#define N_RELOCATION_INFO_DECLARED
#ifndef linux
#define __STRUCT_EXEC_OVERRIDE__
#define N_MAGIC(exec) ((exec).a_magic)
#include "a.out.gnu.h"
#endif
#else
#include "a.out.encap.h"
#endif

#ifdef __STDC__
#include <stdlib.h>
#else
char *malloc ();
#endif

char *xmalloc ();
void dump_file ();
void dump_header ();
void dump_nstuff ();
void dump_reloc ();
void dump_reloc1 ();
void dump_sym ();
void error ();
void usage ();

/* Symbol table. */
struct nlist *symtbl;

/* Number of elements in `symtbl'. */
int nsyms;

/* String table. */
char *strtbl;

/* Number of elements in `strtbl'. */
int strsize;

/* Name this program was run with. */
char *program_name;

/* Name of object file currently being processed. */
char *filename;

void
read_symbols (execp, f)
     struct exec *execp;
     FILE *f;
{
  int i;
  struct nlist *sp;

  if (symtbl)
    return;
  nsyms = execp->a_syms / sizeof (struct nlist);
  if (nsyms == 0)
    return;

  symtbl = (struct nlist *) xmalloc (nsyms * sizeof (struct nlist));

  fseek (f, N_STROFF (*execp), 0);
  if (fread ((char *) &strsize, sizeof strsize, 1, f) != 1)
    error (1, errno, "%s: cannot read string table size", filename);
  strtbl = xmalloc (strsize);
  fseek (f, N_STROFF (*execp), 0);
  if (fread (strtbl, 1, strsize, f) != strsize)
    error (1, errno, "%s: cannot read string table", filename);

  fseek (f, N_SYMOFF (*execp), 0);
  if (fread ((char *) symtbl, sizeof (struct nlist), nsyms, f) != nsyms)
    error (1, errno, "%s: cannot read symbol table", filename);

  for (i = 0, sp = symtbl; i < nsyms; i++, sp++)
    {
      if (sp->n_un.n_strx == 0)
	sp->n_un.n_name = "";
      else if (sp->n_un.n_strx < 0 || sp->n_un.n_strx > strsize)
	sp->n_un.n_name = "<bad string table index>";
      else
	sp->n_un.n_name = strtbl + sp->n_un.n_strx;
    }
}

void
free_symbols ()
{
  if (symtbl)
    free (symtbl);
  symtbl = NULL;
  if (strtbl)
    free (strtbl);
  strtbl = NULL;
}

/* If nonzero, print header information. */
int print_header = 0;

/* If nonzero, print N_* information. */
int print_nstuff = 0;

/* If nonzero, print relocation information. */
int print_relocation = 0;

/* If nonzero, print symbol information. */
int print_symbols = 0;

/* Size of a page.  Required by N_DATADDR in a.out.gnu.h [VAX].  */
int page_size;

void
main (argc, argv)
     int argc;
     char **argv;
{
  int c;
  int ind;
  int option_given = 0;
  static struct option long_options[] =
  {
    {"symbols", 0, &print_symbols, 1},
    {"relocation", 0, &print_relocation, 1},
    {"nstuff", 0, &print_nstuff, 1},
    {"header", 0, &print_header, 1},
    {NULL, 0, NULL, 0}
  };

  page_size = getpagesize ();

  program_name = argv[0];

  while ((c = getopt_long (argc, argv, "hnrt", long_options, &ind))
	 != EOF)
    {
      option_given = 1;
      switch (c)
	{
	case 0:
	  break;		/* we've been given a long option */
	case 't':
	  print_symbols = 1;
	  break;
	case 'r':
	  print_relocation = 1;
	  break;
	case 'n':
	  print_nstuff = 1;
	  break;
	case 'h':
	  print_header = 1;
	  break;
	default:
	  usage ();
	}
    }

  if (option_given == 0 || optind == argc)
    usage ();

  while (optind < argc)
    dump_file (argv[optind++]);

  exit (0);
}

void
dump_file (name)
     char *name;
{
  FILE *f;
  struct exec exec;

  printf ("%s:\n", name);
  f = fopen (name, "r");
  if (f == NULL)
    {
      error (0, errno, "%s", name);
      return;
    }
  filename = name;

#ifdef HEADER_SEEK
  HEADER_SEEK (f);
#endif
  if (fread ((char *) &exec, sizeof exec, 1, f) != 1)
    {
      error (0, errno, "%s: cannot read header", filename);
      return;
    }

  if (N_BADMAG (exec))
    {
      error (0, 0, "%s: Not an object file", filename);
      return;
    }

  if (print_header)
    dump_header (&exec);

  if (print_nstuff)
    dump_nstuff (&exec);

  if (print_symbols)
    dump_sym (&exec, f);

  if (print_relocation)
    dump_reloc (&exec, f);

  free_symbols ();
}

void
dump_header (execp)
     struct exec *execp;
{
  int x;

#if defined (__GNU_EXEC_MACROS__) && !defined (__STRUCT_EXEC_OVERRIDE__)
  printf ("magic: 0x%x (%o)", N_MAGIC (*execp), N_MAGIC (*execp));
  printf ("machine type: %d", N_MACHTYPE (*execp));
  printf ("flags: 0x%x", N_FLAGS (*execp));
#else /* non-gnu struct exec.  */
  printf ("magic: 0x%x (%o) ", execp->a_magic, execp->a_magic);
#endif /* non-gnu struct exec.  */
  printf ("text 0x%x ", execp->a_text);
  printf ("data 0x%x ", execp->a_data);
  printf ("bss 0x%x\n", execp->a_bss);
  printf ("nsyms %d", execp->a_syms / sizeof (struct nlist));
  x = execp->a_syms % sizeof (struct nlist);
  if (x)
    printf (" (+ %d bytes)", x);
  printf (" entry 0x%x ", execp->a_entry);
  printf ("trsize 0x%x ", execp->a_trsize);
  printf ("drsize 0x%x\n", execp->a_drsize);
}

void
dump_nstuff (execp)
     struct exec *execp;
{
  printf ("N_BADMAG %d\n", N_BADMAG (*execp));
  printf ("N_TXTOFF 0x%x\n", N_TXTOFF (*execp));
  printf ("N_SYMOFF 0x%x\n", N_SYMOFF (*execp));
  printf ("N_STROFF 0x%x\n", N_STROFF (*execp));
  printf ("N_TXTADDR 0x%x\n", N_TXTADDR (*execp));
  printf ("N_DATADDR 0x%x\n", N_DATADDR (*execp));
}

void
dump_sym (execp, f)
     struct exec *execp;
     FILE *f;
{
  int i;
  struct nlist *sp;

  read_symbols (execp, f);
  if (nsyms == 0)
    {
      printf ("no symbols\n");
      return;
    }

  printf ("%3s: %4s %5s %4s %8s\n",
	  "#", "type", "other", "desc", "val");
  for (i = 0, sp = symtbl; i < nsyms; i++, sp++)
    {
      printf ("%3d: %4x %5x %4x %8x %s\n",
	      i,
	      sp->n_type & 0xff,
	      sp->n_other & 0xff,
	      sp->n_desc & 0xffff,
	      sp->n_value,
	      sp->n_un.n_name);
    }
}

void
dump_reloc (execp, f)
     struct exec *execp;
     FILE *f;
{
  read_symbols (execp, f);
  if (execp->a_trsize)
    {
      printf ("text reloc\n");
      dump_reloc1 (execp, f, N_TRELOFF (*execp), execp->a_trsize);
    }
  if (execp->a_drsize)
    {
      printf ("data reloc\n");
      dump_reloc1 (execp, f, N_DRELOFF (*execp), execp->a_drsize);
    }
}

void
dump_reloc1 (execp, f, off, size)
     struct exec *execp;
     FILE *f;
     int off;
     int size;
{
#if !defined(sun) || !defined(sparc)
  int nreloc;
  struct relocation_info reloc;
  int i;

  nreloc = size / sizeof (struct relocation_info);

  printf ("%3s: %3s %8s %4s\n", "#", "len", "adr", "sym");
  fseek (f, off, 0);
  for (i = 0; i < nreloc; i++)
    {
      if (fread ((char *) &reloc, sizeof reloc, 1, f) != 1)
	{
	  error (0, errno, "%s: cannot read relocation information",
		 filename);
	  return;
	}
      printf ("%3d: %3d %8x ", i, 1 << reloc.r_length,
	      reloc.r_address);

      if (reloc.r_extern)
	{
	  printf ("%4d ", reloc.r_symbolnum);
	  if (reloc.r_symbolnum < nsyms)
	    printf ("%s ", symtbl[reloc.r_symbolnum].n_un.n_name);
	}
      else
	{
	  printf ("     ");
	  switch (reloc.r_symbolnum & ~N_EXT)
	    {
	    case N_TEXT:
	      printf (".text ");
	      break;
	    case N_DATA:
	      printf (".data ");
	      break;
	    case N_BSS:
	      printf (".bss ");
	      break;
	    case N_ABS:
	      printf (".abs ");
	      break;
	    default:
	      printf ("base %x ", reloc.r_symbolnum);
	      break;
	    }
	}
      if (reloc.r_pcrel)
	printf ("PCREL ");
#if 0
      if (reloc.r_pad)
	printf ("PAD %x ", reloc.r_pad);
#endif
      printf ("\n");
    }
#endif
}


/* Allocate N bytes of memory dynamically, with error checking.  */

char *
xmalloc (n)
     unsigned n;
{
  char *p = malloc (n);
  if (p == 0)
    error (1, 0, "virtual memory exhausted");
  return p;
}


#if defined(USG) || defined(OLD_LINUX)
int
getpagesize ()
{
  return 4096;
}
#endif

#if defined(sun) && defined(sparc)
int
getpagesize ()
{
  return 8192;
}
#endif

void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-hnrt] [+header] [+nstuff] [+relocation] [+symbols] objfile...\n",
	   program_name);
  exit (1);
}
