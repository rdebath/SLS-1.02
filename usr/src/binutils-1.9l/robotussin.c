/* Convert COFF-format object file to BSD format.
   Used for converting the system libraries so GNU ld can link them.
   Copyright (C) 1988 Free Software Foundation, Inc.

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

/* Written by Jeff Lewis.

   BUGS:

   Should do more to verify that the input COFF file meets our
   expectations.

   On machines where the structure of the COFF data in the file does
   not match the structure of the COFF data declared (when, for
   example sizeof (struct filhdr) != FILHSZ), this program will fail.
   (Don't ask me why this is ever allowed to come about).  Accessor
   functions/ macros that painstakingly extract the data out of the
   file and stuff it in the memory struct should be written to fix
   this on such machines.

   CAVEATS:

   This program cannot claim correctness, however, it does appear to
   work on my fairly vanilla Sys5r2 machine.  Someone with the time
   and a fine tooth comb (not to mention some documentation on COFF)
   should correct this! */

#ifndef COFF_ENCAPSULATE
#define COFF_ENCAPSULATE
#endif

/* Customization for particular machines.  */

#ifdef i386
#define INPUT_MAGIC I386MAGIC
#else /* not i386 */
#if defined (m68k) || defined (mc68000)
#define INPUT_MAGIC MC68MAGIC
#endif
#endif /* not i386 */

#include <stdio.h>
#include <varargs.h>
#include <fcntl.h>

#include "a.out.encap.h"
#define N_ABSOLUTE N_ABS		/* N_ABS will be redefined in syms.h */
#undef N_ABS

#include <filehdr.h>
#include <aouthdr.h>
#include <scnhdr.h>
#include <syms.h>
#include <reloc.h>
/* Because of struct alignment on dwords sizeof (struct syment) is different
   than the syments stored in the file.  Therefore, we must kludge:  */
#define sizeof_syment (SYMESZ)
#define sizeof_reloc (RELSZ)
#define sizeof_section (SCNHSZ)
#define sizeof_coff_header (FILHSZ)
/* Without the following, compiling this is a pain on a BSD4.3 Vax.
   Though that might not seem useful, the compatibility of byte order
   between a Vax and a 386 can come in handy for cross-debugging. */
#ifdef VPRINTF_MISSING
#define vfprintf(fp, format, ap) _doprnt (format, ap, fp)
#endif

extern long lseek ();
extern void exit ();
extern char *memcpy ();
extern int errno;

void error (), sys_error ();
static void reloc_segment ();
char *mem_alloc ();

int fd_in, fd_out;		/* input and output file descriptors */

struct filehdr coff_header;	/* file header from the input file */
struct exec bsd_header;		/* file header for the output file */

struct syment *coff_sym_listp;	/* list of symbols from the input */
int *symbol_map;		/* mapping of input symbol #'s to
				   output symbol numbers */
char *text_and_data;		/* space for text & data section data */
char *relocations;		/* space for output reloc entries */
int verbose_flag;		/* flag for debugging */

struct scnhdr coff_text_header;	/* COFF text section header */
struct scnhdr coff_data_header;	/* COFF data section header */
struct scnhdr coff_bss_header;	/* COFF bss section header */
int text_sect_num;		/* COFF section # for text */
int data_sect_num;		/* COFF section # for data */
int bss_sect_num;		/* COFF section # for bss */

char *program_name;		/* Name this program was run with. */

void
main (argc, argv)
     int argc;
     char **argv;
{
  int i, j;
  char *coff_string_table, *bsd_string_table;
  register char *pc, *pc2;
  int string_table_len;
  int symbol_count;
  struct scnhdr section;
  struct nlist name;

  program_name = argv[0];

  if (argc < 3)
    error ("Usage: %s COFF-file BSD-file", argv[0]);
  if (argc > 3)
    verbose_flag = 1;

  fd_in = open (argv[1], O_RDONLY);
  if (fd_in < 0)
    sys_error ("cannot open %s", argv[1]);

  fd_out = open (argv[2], O_WRONLY | O_CREAT | O_TRUNC, 0666);
  if (fd_out < 0)
    sys_error ("cannot open %s", argv[2]);

  /* Read in the file header and all section headers searching
     for text, data and bss.  We note the section #'s of these
     sections for use when examining symbols. */

  if (read (fd_in, &coff_header, sizeof_coff_header) != sizeof_coff_header)
    error ("cannot read file header");

  if (coff_header.f_magic != INPUT_MAGIC)
    error ("bad magic number in coff file\n");

  lseek (fd_in, sizeof_coff_header + coff_header.f_opthdr, 0);

  for (i = 1; i <= coff_header.f_nscns; ++i)
    {
      if (read (fd_in, &section, sizeof_section) != sizeof_section)
	error ("cannot read section header #%d", i);
      if (strcmp (section.s_name, _TEXT) == 0)
	{
	  text_sect_num = i;
	  memcpy (&coff_text_header, &section, sizeof section);
	} 
      else if (strcmp (section.s_name, _DATA) == 0)
	{
	  data_sect_num = i;
	  memcpy (&coff_data_header, &section, sizeof section);
	} 
      else if (strcmp (section.s_name, _BSS) == 0)
	{
	  bss_sect_num = i;
	  memcpy (&coff_bss_header, &section, sizeof section);
	}
    }

#ifdef sun386
/* The purpose of the following kludge is to cope with the discrepancy
   between coff_data_header.s_vaddr and coff_text_header.s_size; in a BSD
   object file, they are assumed equal.  It should be ok to use
   coff_data_header.s_paddr, since it has no other apparent use. */
  coff_data_header.s_paddr = coff_text_header.s_size;
  coff_bss_header.s_paddr = coff_data_header.s_size + coff_text_header.s_size;
#endif

  /* Pass1 thru the symbol table - count usable symbols and map
     old symbol #'s into new ones (as used by relocation
     info).  We're only interested in keeping the kinds of symbols
     we'd expect to find in a BSD library object file: no debug
     symbols, file names, section definition symbols, etc.
     Section definition symbols are referenced by reloc entries
     in the COFF file, so we note their position with a negative
     symbol number indicating the section.  -1 is used to flag
     symbols we're not interested in, yielding an unexpected error
     if we find any reloc entries referencing them. */

  coff_sym_listp =
    (struct syment *) mem_alloc (coff_header.f_nsyms * sizeof (struct syment));
  symbol_map = (int *) mem_alloc (coff_header.f_nsyms * sizeof *symbol_map);
  if (lseek (fd_in, coff_header.f_symptr, 0) < 0L)
    sys_error ("cannot seek to COFF symbols");
  for (i = 0; i < coff_header.f_nsyms; ++i)
    {
      if (read (fd_in, coff_sym_listp + i, sizeof_syment) != sizeof_syment)
	error ("cannot read COFF symbols");
    }
  symbol_count = 0;
  for (i = 0; i < coff_header.f_nsyms; ++i)
    {
      if (coff_sym_listp[i].n_scnum != N_DEBUG
	  && coff_sym_listp[i].n_name[0] != '.'
	  && coff_sym_listp[i].n_scnum != -1)
	{
	  if (verbose_flag)
	    printf ("map %d to %d\n", i, symbol_count);
	  symbol_map[i] = symbol_count++;
#ifdef sun386
	  if (coff_sym_listp[i].n_scnum == data_sect_num)
	    coff_sym_listp[i].n_value -=
	      coff_data_header.s_vaddr - coff_data_header.s_paddr;
	  else if (coff_sym_listp[i].n_scnum == bss_sect_num)
	    coff_sym_listp[i].n_value -=
	      coff_bss_header.s_vaddr - coff_bss_header.s_paddr;
#endif
	} 
      else
	{
	  if (coff_sym_listp[i].n_sclass == C_STAT)
	    {
	      if (strcmp (coff_sym_listp[i].n_name, _TEXT) == 0)
		symbol_map[i] = -N_TEXT;
	      else if (strcmp (coff_sym_listp[i].n_name, _DATA) == 0)
		symbol_map[i] = -N_DATA;
	      else if (strcmp (coff_sym_listp[i].n_name, _BSS) == 0)
		symbol_map[i] = -N_BSS;
	      else
		symbol_map[i] = -1;
	    } 
	  else
	    {
	      symbol_map[i] = -1;
	    }
	}
      /* skip auxillary entries */
      j = coff_sym_listp[i].n_numaux;
      if (j != 0)
	{
	  if (j < 0)
	    error ("invalid numaux");
	  if (j != 1)
	    fprintf (stderr, "unlikely numaux value\n");
	  while (--j >= 0)
	    ++i;
	}
    }

  /* Now we know enough to write the output file header. */

  N_SET_MAGIC (bsd_header, OMAGIC);
  bsd_header.a_text = coff_text_header.s_size;
  bsd_header.a_data = coff_data_header.s_size;
  bsd_header.a_bss = coff_bss_header.s_size;
  bsd_header.a_syms = symbol_count * sizeof (struct nlist);
  bsd_header.a_entry = 0;
  bsd_header.a_trsize = coff_text_header.s_nreloc * sizeof (struct relocation_info);
  bsd_header.a_drsize = coff_data_header.s_nreloc * sizeof (struct relocation_info);
  if (write (fd_out, &bsd_header, sizeof bsd_header) != sizeof bsd_header)
    sys_error ("cannot write BSD header");

  /* Read in and save text and data sections - some data in
     these sections may need to be altered due to relocations. */

  text_and_data = (char *) mem_alloc (coff_text_header.s_size + coff_data_header.s_size);
  if (lseek (fd_in, coff_text_header.s_scnptr, 0) < 0L)
    sys_error ("cannot seek to text section");
  if (read (fd_in, text_and_data, coff_text_header.s_size) != coff_text_header.s_size)
    error ("cannot read text section");
  if (lseek (fd_in, coff_data_header.s_scnptr, 0) < 0L)
    sys_error ("cannot seek to data section");
  if (read (fd_in, text_and_data + coff_text_header.s_size, coff_data_header.s_size) != coff_data_header.s_size)
    error ("cannot read data section");

  /* Convert the relocation entries and do any text or data
     modifications necessary. */

  relocations = (char *) mem_alloc (bsd_header.a_trsize + bsd_header.a_drsize);
  reloc_segment (&coff_text_header, relocations);
  reloc_segment (&coff_data_header, relocations + bsd_header.a_trsize);

  if (write (fd_out, text_and_data, coff_text_header.s_size + coff_data_header.s_size)
      != coff_text_header.s_size + coff_data_header.s_size)
    sys_error ("cannot write text and data sections");
  /* ZZ - are there any alignment considerations?? */
  if ((coff_text_header.s_size & 1) || (coff_data_header.s_size & 1))
    fprintf (stderr, "non-aligned text or data section\n");
  if (write (fd_out, relocations, bsd_header.a_trsize + bsd_header.a_drsize)
      != bsd_header.a_trsize + bsd_header.a_drsize)
    sys_error ("cannot write relocation entries");

  /* Second pass thru the symbol table.  
     a COFF symbol entry may contain up to 8 chars of symbol name
     in the entry itself - symbol names > 8 go into the string table,
     whereas the BSD entry puts all symbol names into the string
     table. */

  if (lseek (fd_in, coff_header.f_symptr + coff_header.f_nsyms * sizeof_syment, 0) < 0L)
    error ("cannot seek to string table");

  i = read (fd_in, &string_table_len, sizeof string_table_len);
  if (i == sizeof string_table_len)
    {
      coff_string_table = mem_alloc (string_table_len);
      string_table_len -= sizeof string_table_len;
      i = read (fd_in, coff_string_table + sizeof string_table_len, string_table_len);
      if (i < 0)
	error ("cannot read string table");
      if (i != string_table_len)
	error ("truncated string table - expected %d, got %d",
	       string_table_len, i);
    } 
  else
    {
      string_table_len = 0;
    }
  bsd_string_table = mem_alloc (string_table_len + coff_header.f_nsyms * (SYMNMLEN + 1));
  pc = bsd_string_table + sizeof string_table_len;
  for (i = 0; i < coff_header.f_nsyms; ++i)
    {
      if (coff_sym_listp[i].n_scnum != N_DEBUG
	  && coff_sym_listp[i].n_name[0] != '.'
	  && coff_sym_listp[i].n_scnum != -1)
	{
	  if (coff_sym_listp[i].n_zeroes == 0)
	    {
	      j = pc - bsd_string_table;
#ifndef nounderscore
	      if (coff_sym_listp[i].n_sclass == C_EXT
		  || coff_sym_listp[i].n_sclass == C_STAT)
		*pc++ = '_';
#endif
	      pc2 = coff_string_table + coff_sym_listp[i].n_offset;
	      while (*pc++ = *pc2++)
		/* null */ ;
	      name.n_un.n_strx = j;
	    } 
	  else
	    {
	      pc2 = &coff_sym_listp[i].n_name[0];
	      j = pc - bsd_string_table;
#ifndef nounderscore
	      if (coff_sym_listp[i].n_sclass == C_EXT
		  || coff_sym_listp[i].n_sclass == C_STAT)
		*pc++ = '_';
#endif
	      {
		int x;
		for (x = 0; x < SYMNMLEN; x++)
		  {
		    if (*pc2 == 0)
		      break;
		    *pc++ = *pc2++;
		  }
		*pc++ = 0;
	      }
	      name.n_un.n_strx = j;
	    }
	  switch (coff_sym_listp[i].n_scnum)
	    {
	    case N_ABS:
	      name.n_type = N_ABSOLUTE;
	      break;
	    case N_UNDEF:
	      name.n_type = N_UNDF;
	      break;
	    default:
	      if (coff_sym_listp[i].n_scnum == text_sect_num)
		name.n_type = N_TEXT;
	      else if (coff_sym_listp[i].n_scnum == data_sect_num)
		name.n_type = N_DATA;
	      else if (coff_sym_listp[i].n_scnum == bss_sect_num)
		name.n_type = N_BSS;
	      break;
	    }
	  if (coff_sym_listp[i].n_sclass == C_EXT)
	    name.n_type |= N_EXT;
	  name.n_other = 0;
	  name.n_desc = 0;
	  name.n_value = coff_sym_listp[i].n_value;

	  if (write (fd_out, &name, sizeof name) != sizeof name)
	    sys_error ("cannot write symbol");
	}
      /* skip auxillary entries */
      j = coff_sym_listp[i].n_numaux;
      if (j != 0)
	{
	  while (--j >= 0)
	    ++i;
	}
    }
  i = *((int *) bsd_string_table) = pc - bsd_string_table;
  if (write (fd_out, bsd_string_table, i) != i)
    error ("cannot write string table");

  close (fd_in);
  close (fd_out);
  exit (0);
}

/* Convert the relocation entries and do any text or data
   modifications necessary. */

static void
reloc_segment (section_headerp, reloc_infop)
     struct scnhdr *section_headerp;
     struct relocation_info *reloc_infop;
{
  struct reloc coff_reloc;
  int i;

  if (lseek (fd_in, section_headerp->s_relptr, 0) < 0L)
    error ("cannot seek to relocation entries");
  for (i = 0; i < section_headerp->s_nreloc; ++i)
    {
      if (read (fd_in, &coff_reloc, sizeof_reloc) != sizeof_reloc)
	error ("cannot read relocation entry");
#ifdef sun386
      coff_reloc.r_vaddr -=
	section_headerp->s_vaddr - section_headerp->s_paddr;
#endif
      if (verbose_flag)
	printf ("vaddr = 0x%x, symndx = %d\n", coff_reloc.r_vaddr, coff_reloc.r_symndx);
      /* The reloc references a symbol declared common, thus the
	 value of the symbol holds its size (in bytes).  In COFF,
	 apparently this info is also put into the binary -
	 BSD doesn't like this, so we subtract it out. */
      if (coff_sym_listp[coff_reloc.r_symndx].n_scnum == N_UNDEF)
	{
	  if (coff_sym_listp[coff_reloc.r_symndx].n_value != 0)
	    {
	      if (verbose_flag)
		printf ("adjust common 0x%x (%d)\n",
			coff_sym_listp[coff_reloc.r_symndx].n_value,
			coff_sym_listp[coff_reloc.r_symndx].n_value);
	      switch (coff_reloc.r_type)
		{
		case R_RELBYTE:
		  *((char *) (text_and_data + coff_reloc.r_vaddr))
		    -= coff_sym_listp[coff_reloc.r_symndx].n_value;
		  break;
		case R_RELWORD:
		  *((short *) (text_and_data + coff_reloc.r_vaddr))
		    -= coff_sym_listp[coff_reloc.r_symndx].n_value;
		  break;
		case R_RELLONG:
	        case R_DIR32:	/* these are the only two that really show up */
	        case R_PCRLONG:
		  *((int *) (text_and_data + coff_reloc.r_vaddr))
		    -= coff_sym_listp[coff_reloc.r_symndx].n_value;
		  break;
	        default:
		  error ("unknown relocation type 0%o", coff_reloc.r_type);
		}
	    }
	}
      /* >= 0 means its an extern - value is the output symbol #.
	 < 0 means its an intern - value is N_TEXT, N_DATA or N_BSS. */
      if (symbol_map[coff_reloc.r_symndx] >= 0)
	{
	  reloc_infop->r_symbolnum = symbol_map[coff_reloc.r_symndx];
	  reloc_infop->r_extern = 1;
	} 
      else
	{
	  if (symbol_map[coff_reloc.r_symndx] == -1)
	    error ("Oops! possible bug - reloc reference to ignored symbol");
#ifdef sun386
	  {
	    int offset=0;
	    switch (-symbol_map[coff_reloc.r_symndx])
	      {
	      case N_DATA:
		offset = coff_data_header.s_vaddr - coff_data_header.s_paddr;
		break;
	      case N_BSS:
		offset = coff_bss_header.s_vaddr - coff_bss_header.s_paddr;
		break;
	      }
	    switch (coff_reloc.r_type)
	      {
	      case R_RELBYTE:
		*((char *) (text_and_data + coff_reloc.r_vaddr)) -= offset;
		break;
	      case R_RELWORD:
		*((short *) (text_and_data + coff_reloc.r_vaddr)) -= offset;
		break;
	      case R_RELLONG:
	      case R_DIR32:
	      case R_PCRLONG:
		*((int *) (text_and_data + coff_reloc.r_vaddr)) -= offset;
		break;
	      default:
		error ("unknown relocation type 0%o", coff_reloc.r_type);
	      }
	  }
#endif
	  reloc_infop->r_symbolnum = -symbol_map[coff_reloc.r_symndx];
	  reloc_infop->r_extern = 0;
	}
      /* COFF address includes the section address - BSD doesn't, so
	 subtract it out. */
#ifdef sun386
      coff_reloc.r_vaddr +=
	section_headerp->s_vaddr - section_headerp->s_paddr;
#endif
      reloc_infop->r_address = coff_reloc.r_vaddr - section_headerp->s_vaddr;
      switch (coff_reloc.r_type)
	{
	case R_PCRLONG:
	  reloc_infop->r_pcrel = 1;
	  reloc_infop->r_length = 2; /* 4 bytes */
	  break;
	case R_DIR32:
	case R_RELLONG:
	  reloc_infop->r_pcrel = 0;
	  reloc_infop->r_length = 2;
	  break;
	default:
	  error ("cannot handle coff reloction type 0%o", coff_reloc.r_type);
	}

      if (verbose_flag)
	printf ("reloc: addr = 0x%x, synum = %d\n",
		reloc_infop->r_address, reloc_infop->r_symbolnum);
      reloc_infop->r_pad = 0;
      ++reloc_infop;
    }
}

void
error (format, va_alist)
     char *format;
     va_dcl
{
  va_list args;

  va_start (args);
  fprintf (stderr, "%s: ", program_name);
  vfprintf (stderr, format, args);
  putc ('\n', stderr);
  va_end (args);
  exit (1);
}

extern char *sys_errlist[];
extern int errno;

void
sys_error (format, va_alist)
     char *format;
     va_dcl
{
  va_list args;

  va_start (args);
  fprintf (stderr, "%s: ", program_name);
  vfprintf (stderr, format, args);
  fprintf (stderr, ": %s\n", sys_errlist[errno]);
  va_end (args);
  exit (1);
}

extern char *malloc ();

char *
mem_alloc (size)
     int size;
{
  char *pc;

  if ((pc = malloc (size)) == NULL)
    error ("memory exhausted!");
  return pc;
}
