/* BFD ECOFF object file private structure.
   Copyright (C) 1993 Free Software Foundation, Inc.
   Written by Ian Lance Taylor, Cygnus Support.

This file is part of BFD, the Binary File Descriptor library.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* This is the target specific information kept for ECOFF files.  */

#define ecoff_data(abfd) ((abfd)->tdata.ecoff_obj_data)

typedef struct ecoff_tdata
{
  /* The reloc file position, set by
     ecoff_compute_section_file_positions.  */
  file_ptr reloc_filepos;

  /* The symbol table file position, set by ecoff_mkobject_hook.  */
  file_ptr sym_filepos;

  /* The start and end of the text segment.  Only valid for an
     existing file, not for one we are creating.  */
  unsigned long text_start;
  unsigned long text_end;

  /* The cached gp value.  This is used when relocating.  */
  bfd_vma gp;

  /* The register masks.  When linking, all the masks found in the
     input files are combined into the masks of the output file.  */
  unsigned long gprmask;
  unsigned long cprmask[4];

  /* The size of the unswapped ECOFF symbolic information.  */
  bfd_size_type raw_size;

  /* The unswapped ECOFF symbolic information.  */
  PTR raw_syments;

  /* The swapped ECOFF symbolic header.  */
  HDRR symbolic_header;

  /* Pointers to the unswapped symbolic information.  */
  unsigned char *line;
  struct dnr_ext *external_dnr;
  struct pdr_ext *external_pdr;
  struct sym_ext *external_sym;
  struct opt_ext *external_opt;
  union aux_ext *external_aux;
  char *ss;
  char *ssext;
  struct fdr_ext *external_fdr;
  struct rfd_ext *external_rfd;
  struct ext_ext *external_ext;

  /* The swapped FDR information.  */
  FDR *fdr;

  /* The FDR index.  This is set for an input BFD to a link so that
     the external symbols can set their FDR index correctly.  */
  unsigned int ifdbase;

  /* The canonical BFD symbols.  */
  struct ecoff_symbol_struct *canonical_symbols;

} ecoff_data_type;

/* Read in the ECOFF symbolic information.  FIXME: If there is ever
   another ECOFF target, this function, and the swapping functions,
   should be called via a target specific vector, as is done with the
   functions in bfd_coff_backend_data.  */

extern boolean ecoff_slurp_symbolic_info PARAMS ((bfd *));
