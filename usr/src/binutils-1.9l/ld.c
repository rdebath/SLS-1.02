/* Linker `ld' for GNU
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

/* Written by Richard Stallman with some help from Eric Albert.
   Set, indirect, and warning symbol features added by Randy Smith.  */

#ifndef AR_H
#define AR_H	<ar.h>
#endif
#ifndef A_OUT_H
#define A_OUT_H	<a.out.h>
#endif

#include AR_H
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#ifdef __STDC__
#include <stdlib.h>
#endif
#if !defined(USG) && !defined(OLD_LINUX)
#include <sys/time.h>
#include <sys/resource.h>
#endif
#ifdef linux
#include <unistd.h>
#endif
#ifndef sony_news
#include <fcntl.h>
#endif
#ifdef ns32000
struct relocation_info
{  /* Look for the default declaration in a.out.gnu.h for documentation. */
  int r_address;
  unsigned int r_symbolnum:24;
  unsigned int r_pcrel:1;
  unsigned int r_length:2;
  unsigned int r_extern:1;
  unsigned int r_bsr:1;
  unsigned int r_disp:2;
  unsigned int r_pad:1;
};
#define N_RELOCATION_INFO_DECLARED
#endif /*ns32000*/

#if !defined(A_OUT) && !defined(MACH_O)
#define A_OUT
#endif

#ifdef A_OUT
#ifdef COFF_ENCAPSULATE
#include "a.out.encap.h"
#else
#include A_OUT_H
#endif
#endif

#ifdef MACH_O
#ifndef A_OUT
#include <nlist.h>
#include <reloc.h>
#endif
#ifndef N_TEXT
#define N_TEXT 0x04
#define N_DATA 0x06
#define N_BSS 0x08
#endif
#include <sys/loader.h>
#endif

#ifndef N_SET_MAGIC
#define N_SET_MAGIC(exec, val)  ((exec).a_magic = val)
#endif

/* If compiled with GNU C, use the built-in alloca */
#ifdef __GNUC__
# ifndef alloca
#  define alloca __builtin_alloca
# endif
#else
# if defined(sun) && defined(sparc)
#  include "alloca.h"
# else
char *alloca ();
# endif
#endif

#include "getopt.h"

/* Always use the GNU version of debugging symbol type codes, if possible.  */

#include "stab.h"
#define CORE_ADDR unsigned long	/* For symseg.h */
#include "symseg.h"

#ifdef USG
#include <string.h>
#else
#include <strings.h>
#endif

/* Determine whether we should attempt to handle (minimally)
   N_BINCL and N_EINCL.  */

#if defined (__GNU_STAB__) || defined (N_BINCL)
#define HAVE_SUN_STABS
#endif

#define min(a,b) ((a) < (b) ? (a) : (b))

/* Macro to control the number of undefined references printed */
#define MAX_UREFS_PRINTED	10

/* Size of a page; obtained from the operating system.  */

int page_size;

/* Name this program was invoked by.  */

char *progname;

/* System dependencies */

/* Define this if names etext, edata and end should not start with `_'.  */
/* #define nounderscore 1 */

/* Define NON_NATIVE if using BSD or pseudo-BSD file format on a system
   whose native format is different.  */
/* #define NON_NATIVE */

/* Define this to specify the default executable format.  */

#ifdef hpux
#define DEFAULT_OUTPUT_STYLE OUTPUT_READONLY_TEXT
#endif

/* Ordinary 4.3bsd lacks these macros in a.out.h.  */

#ifndef N_TXTADDR
#if defined(vax) || defined(sony_news) || defined(hp300) || defined(pyr)
#define N_TXTADDR(X) 0
#endif
#ifdef is68k
#define N_TXTADDR(x)  (sizeof (struct exec))
#endif
#ifdef sequent
#define	N_TXTADDR(x) (N_ADDRADJ(x))
#endif
#ifdef tek4300
#define N_TXTADDR(x) ((x).a_magic == ZMAGIC ? page_size : 0)
#endif
#ifdef NeXT
#define N_TXTADDR(X) ((X).a_magic == ZMAGIC ? page_size : 0)
#endif
#endif

#ifndef N_DATADDR
#if defined(vax) || defined(sony_news) || defined(hp300) || defined(pyr)
#define N_DATADDR(x) \
	(((x).a_magic==OMAGIC)? (N_TXTADDR(x)+(x).a_text) \
	: (page_size+((N_TXTADDR(x)+(x).a_text-1) & ~(page_size-1))))
#endif
#ifdef is68k
#define SEGMENT_SIZE 0x20000
#define N_DATADDR(x) \
    (((x).a_magic==Omagic)? (N_TXTADDR(x)+(x).a_text) \
     : (SEGMENT_SIZE + ((N_TXTADDR(x)+(x).a_text-1) & ~(SEGMENT_SIZE-1))))
#endif
#ifdef sequent
#define N_DATADDR(x) \
	(((x).a_magic==OMAGIC)? (N_TXTADDR(x)+(x).a_text) \
	: (page_size+(((x).a_text-1) & ~(page_size-1))))
#endif
#ifdef tek4300
#define N_DATADDR(x) \
	(((x).a_magic==OMAGIC)? (N_TXTADDR(x)+(x).a_text) \
	: (page_size+((N_TXTADDR(x)+(x).a_text-1) & ~(page_size-1))))
#endif
#ifdef NeXT
#define N_DATADDR(X) \
	(((X).a_magic==ZMAGIC)?(N_TXTADDR(X)+(X).a_text+0xFFFF)&~0xFFFF \
	 :N_TXTADDR(X)+(X).a_text)
#endif
#endif

/* The "address" of the data segment in a relocatable file.
   The text address of a relocatable file is always
   considered to be zero (instead of the value of N_TXTADDR, which
   is what the address is in an executable), so we need to subtract
   N_TXTADDR from N_DATADDR to get the "address" for the input file.  */
#define DATA_ADDR_DOT_O(hdr) (N_DATADDR(hdr) - N_TXTADDR(hdr))

/* Define how to initialize system-dependent header fields.  */
#ifdef sun
#ifdef sparc
#define INITIALIZE_HEADER \
  {outheader.a_machtype = M_SPARC; outheader.a_toolversion = 1;}
#endif /* sparc.  */
#if defined(mc68000)
/* Set the machine type according to the machine type of the .o files.
   If they are all sun2 (68010), then the type of the executable is sun2.
   If any is sun3 (68020), then the type of the executable is sun3.
   This is consistent with the Sun loader and more useful than having
   it depend on which machine you are on when you run ld.  */
static int sun_machtype = M_68010;
#define INITIALIZE_HEADER outheader.a_machtype = sun_machtype
#define READ_HEADER_HOOK(machtype) \
  if (machtype == M_68020)           \
    {				     \
      sun_machtype = M_68020;	     \
    }
#endif /* mc68000.  */
#endif /* Sun.  */

#ifdef ALTOS
#define INITIALIZE_HEADER N_SET_MACHTYPE (outheader, M_68020)
#endif
#ifdef is68k
#ifdef M_68020
/* ISI rel 4.0D doesn't use it, and rel 3.05 doesn't have an
   a_machtype field and so won't recognize the magic number.  To keep
   binary compatibility for now, just ignore it */
#define INITIALIZE_HEADER outheader.a_machtype = 0;
#endif
#endif
#ifdef hpux
#define INITIALIZE_HEADER N_SET_MACHTYPE (outheader, HP9000S200_ID)
#endif
#if defined(i386) && !defined(sequent)
#ifndef OLD_LINUX
#define INITIALIZE_HEADER N_SET_MACHTYPE (outheader, M_386)
#endif
#endif /* Sequent symmetry.  */
#if defined(hp300)
#define INITIALIZE_HEADER outheader.a_mid = MID_HP300
/* MORE/bsd tags the filename symbol with N_FN|N_EXT not N_TEXT */
#define OFILE_FN_FLAGGED
#endif /* hp300.  */
#ifdef pyr
#define INITIALIZE_HEADER outheader.a_machid = PYR90X
#endif /* Pyramid.  */

#ifdef tek4300
#undef INITIALIZE_HEADER
#define INITIALIZE_HEADER (outheader).a_archclass = ARCHCLASS_MC68020, (outheader).a_textoff = ZMOFF
/* The alternative to the following patch is to change field "header"
   in struct file_entry, from struct exec to struct zexec; this affects
   the -A option */
#undef N_DATOFF
#define N_DATOFF(x) ( (x).a_magic==ZMAGIC		\
		     ? (ZMOFF + (x).a_text)		\
		     : (sizeof (struct exec) + (x).a_text) )
#endif

#ifdef is68k
/* This enables code to take care of an ugly hack in the ISI OS.
   If a symbol beings with _$, then the object file is included only
   if the rest of the symbol name has been referenced. */
#define DOLLAR_KLUDGE
#endif

#ifdef OLD_LINUX
#undef INITIALIZE_HEADER
#endif

/* Values for 3rd argument to lseek().  */
#ifndef L_SET
#define L_SET 0
#endif
/* This is called L_INCR in BSD, but SEEK_CUR in POSIX.  */
#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif

/*
 * Ok.  Following are the relocation information macros.  If your
 * system cannot use the default set (below), you must define all of these:

 *   relocation_info: This must be typedef'd (or #define'd) to the type
 * of structure that is stored in the relocation info section of your
 * a.out files.  Often this is defined in the a.out.h for your system.
 *
 *   RELOC_ADDRESS (rval): Offset into the current section of the
 * <whatever> to be relocated.  *Must be an lvalue*.
 *
 *   RELOC_EXTERN_P (rval):  Is this relocation entry based on an
 * external symbol (1), or was it fully resolved upon entering the
 * loader (0) in which case some combination of the value in memory
 * (if RELOC_MEMORY_ADD_P) and the extra (if RELOC_ADD_EXTRA) contains
 * what the value of the relocation actually was.  *Must be an lvalue*.
 *
 *   RELOC_TYPE (rval): For a non-external relocation, this is the
 * segment to relocate for.  *Must be an lvalue.*
 *
 *   RELOC_SYMBOL (rval): For an external relocation, this is the
 * index of its symbol in the symbol table.  *Must be an lvalue*.
 *
 *   RELOC_MEMORY_ADD_P (rval): This should be 1 if the final
 * relocation value output here should be added to memory; 0, if the
 * section of memory described should simply be set to the relocation
 * value.
 *
 *   RELOC_MEMORY_ADD_P (rval): If this is nonzero, the value previously
 * present in the memory location to be relocated is *added*
 * to the relocation value, to produce the final result.
 * Otherwise, the relocation value is stored in the memory location
 * and the value previously found there is ignored.
 * By default, this is always 1.
 *
 *   RELOC_MEMORY_SUB_P (rval): If this is nonzero, the value previously
 * present in the memory location to be relocated is *subtracted*
 * from the relocation value, to produce the final result.
 * By default, this is always 0.
 *
 *   RELOC_ADD_EXTRA (rval): (Optional) This macro, if defined, gives
 * an extra value to be added to the relocation value based on the
 * individual relocation entry.  *Must be an lvalue if defined*.
 *
 *   RELOC_PCREL_P (rval): True if the relocation value described is
 * pc relative.
 *
 *   RELOC_VALUE_RIGHTSHIFT (rval): Number of bits right to shift the
 * final relocation value before putting it where it belongs.
 *
 *   RELOC_TARGET_SIZE (rval): log to the base 2 of the number of
 * bytes of size this relocation entry describes; 1 byte == 0; 2 bytes
 * == 1; 4 bytes == 2, and etc.  This is somewhat redundant (we could
 * do everything in terms of the bit operators below), but having this
 * macro could end up producing better code on machines without fancy
 * bit twiddling.  Also, it's easier to understand/code big/little
 * endian distinctions with this macro.
 *
 *   RELOC_TARGET_BITPOS (rval): The starting bit position within the
 * object described in RELOC_TARGET_SIZE in which the relocation value
 * will go.
 *
 *   RELOC_TARGET_BITSIZE (rval): How many bits are to be replaced
 * with the bits of the relocation value.  It may be assumed by the
 * code that the relocation value will fit into this many bits.  This
 * may be larger than RELOC_TARGET_SIZE if such be useful.
 *
 *
 *		Things I haven't implemented
 *		----------------------------
 *
 *    Values for RELOC_TARGET_SIZE other than 0, 1, or 2.
 *
 *    Pc relative relocation for External references.
 *
 *
 */

#if defined(sun) && defined(sparc)
/* Sparc (Sun 4) macros */
#undef relocation_info
#define relocation_info	                reloc_info_sparc
#define RELOC_ADDRESS(r)		((r)->r_address)
#define RELOC_EXTERN_P(r)               ((r)->r_extern)
#define RELOC_TYPE(r)                   ((r)->r_index)
#define RELOC_SYMBOL(r)                 ((r)->r_index)
#define RELOC_MEMORY_SUB_P(r)		0
#define RELOC_MEMORY_ADD_P(r)           0
#define RELOC_ADD_EXTRA(r)              ((r)->r_addend)
#define RELOC_PCREL_P(r)             \
        ((r)->r_type >= RELOC_DISP8 && (r)->r_type <= RELOC_WDISP22)
#define RELOC_VALUE_RIGHTSHIFT(r)       (reloc_target_rightshift[(r)->r_type])
#define RELOC_TARGET_SIZE(r)            (reloc_target_size[(r)->r_type])
#define RELOC_TARGET_BITPOS(r)          0
#define RELOC_TARGET_BITSIZE(r)         (reloc_target_bitsize[(r)->r_type])

/* Note that these are very dependent on the order of the enums in
   enum reloc_type (in a.out.h); if they change the following must be
   changed */
/* Also note that the last few may be incorrect; I have no information */
static int reloc_target_rightshift[] = {
  0, 0, 0, 0, 0, 0, 2, 2, 10, 0, 0, 0, 0, 0, 0,
};
static int reloc_target_size[] = {
  0, 1, 2, 0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
};
static int reloc_target_bitsize[] = {
  8, 16, 32, 8, 16, 32, 30, 22, 22, 22, 13, 10, 32, 32, 16,
};

#define	MAX_ALIGNMENT	(sizeof (double))
#endif

#if defined(sequent) || defined(ns32000)
#define RELOC_ADDRESS(r)		((r)->r_address)
#define RELOC_EXTERN_P(r)		((r)->r_extern)
#define RELOC_TYPE(r)		((r)->r_symbolnum)
#define RELOC_SYMBOL(r)		((r)->r_symbolnum)
#ifdef sequent			/* I think r_bsr is needed only for compat.
				   with sequent object files, or that's
				   what Ian has done.  But might be that
				   this is needed for all 32k machines.*/
#define RELOC_MEMORY_SUB_P(r)	((r)->r_bsr)
#else
#define RELOC_MEMORY_SUB_P(r) 0
#endif
#define RELOC_MEMORY_ADD_P(r)	1
#undef RELOC_ADD_EXTRA
#ifdef sequent			/* might need to be on all 32k machines */
#define RELOC_PCREL_P(r)		((r)->r_pcrel || (r)->r_bsr)
#else
#define RELOC_PCREL_P(r)		((r)->r_pcrel)
#endif
#define RELOC_VALUE_RIGHTSHIFT(r)	0
#define RELOC_TARGET_SIZE(r)		((r)->r_length)
#define RELOC_TARGET_BITPOS(r)	0
#define RELOC_TARGET_BITSIZE(r)	32
#endif

/* Default macros */
#ifndef RELOC_ADDRESS
#define RELOC_ADDRESS(r)		((r)->r_address)
#define RELOC_EXTERN_P(r)		((r)->r_extern)
#define RELOC_TYPE(r)		((r)->r_symbolnum)
#define RELOC_SYMBOL(r)		((r)->r_symbolnum)
#define RELOC_MEMORY_SUB_P(r)	0
#define RELOC_MEMORY_ADD_P(r)	1
#undef RELOC_ADD_EXTRA
#define RELOC_PCREL_P(r)		((r)->r_pcrel)
#define RELOC_VALUE_RIGHTSHIFT(r)	0
#define RELOC_TARGET_SIZE(r)		((r)->r_length)
#define RELOC_TARGET_BITPOS(r)	0
#define RELOC_TARGET_BITSIZE(r)	32
#endif

#ifndef MAX_ALIGNMENT
#define	MAX_ALIGNMENT	(sizeof (int))
#endif

#ifdef nounderscore
#define LPREFIX '.'
#else
#define LPREFIX 'L'
#endif


/* Special global symbol types understood by GNU LD.  */

/* The following type indicates the definition of a symbol as being
   an indirect reference to another symbol.  The other symbol
   appears as an undefined reference, immediately following this symbol.

   Indirection is asymmetrical.  The other symbol's value will be used
   to satisfy requests for the indirect symbol, but not vice versa.
   If the other symbol does not have a definition, libraries will
   be searched to find a definition.

   So, for example, the following two lines placed in an assembler
   input file would result in an object file which would direct gnu ld
   to resolve all references to symbol "foo" as references to symbol
   "bar".

	.stabs "_foo",11,0,0,0
	.stabs "_bar",1,0,0,0

   Note that (11 == (N_INDR | N_EXT)) and (1 == (N_UNDF | N_EXT)).  */

#ifndef N_INDR
#define N_INDR 0xa
#endif

/* The following symbols refer to set elements.  These are expected
   only in input to the loader; they should not appear in loader
   output (unless relocatable output is requested).  To be recognized
   by the loader, the input symbols must have their N_EXT bit set.
   All the N_SET[ATDB] symbols with the same name form one set.  The
   loader collects all of these elements at load time and outputs a
   vector for each name.
   Space (an array of 32 bit words) is allocated for the set in the
   data section, and the n_value field of each set element value is
   stored into one word of the array.
   The first word of the array is the length of the set (number of
   elements).  The last word of the vector is set to zero for possible
   use by incremental loaders.  The array is ordered by the linkage
   order; the first symbols which the linker encounters will be first
   in the array.

   In C syntax this looks like:

	struct set_vector {
	  unsigned int length;
	  unsigned int vector[length];
	  unsigned int always_zero;
	};

   Before being placed into the array, each element is relocated
   according to its type.  This allows the loader to create an array
   of pointers to objects automatically.  N_SETA type symbols will not
   be relocated.

   The address of the set is made into an N_SETV symbol
   whose name is the same as the name of the set.
   This symbol acts like a N_DATA global symbol
   in that it can satisfy undefined external references.

   For the purposes of determining whether or not to load in a library
   file, set element definitions are not considered "real
   definitions"; they will not cause the loading of a library
   member.

   If relocatable output is requested, none of this processing is
   done.  The symbols are simply relocated and passed through to the
   output file.

   So, for example, the following three lines of assembler code
   (whether in one file or scattered between several different ones)
   will produce a three element vector (total length is five words;
   see above), referenced by the symbol "_xyzzy", which will have the
   addresses of the routines _init1, _init2, and _init3.

   *NOTE*: If symbolic addresses are used in the n_value field of the
   defining .stabs, those symbols must be defined in the same file as
   that containing the .stabs.

	.stabs "_xyzzy",23,0,0,_init1
	.stabs "_xyzzy",23,0,0,_init2
	.stabs "_xyzzy",23,0,0,_init3

   Note that (23 == (N_SETT | N_EXT)).  */

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
#define N_SETV	0x1C		/* Pointer to set vector in data area.  */
#endif				/* This is output from LD.  */

/* If a this type of symbol is encountered, its name is a warning
   message to print each time the symbol referenced by the next symbol
   table entry is referenced.

   This feature may be used to allow backwards compatibility with
   certain functions (eg. gets) but to discourage programmers from
   their use.

   So if, for example, you wanted to have ld print a warning whenever
   the function "gets" was used in their C program, you would add the
   following to the assembler file in which gets is defined:

	.stabs "Obsolete function \"gets\" referenced",30,0,0,0
	.stabs "_gets",1,0,0,0

   These .stabs do not necessarily have to be in the same file as the
   gets function, they simply must exist somewhere in the compilation.  */

#ifndef N_WARNING
#define N_WARNING 0x1E		/* Warning message to print if symbol
				   included */
#endif				/* This is input to ld */

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

/* Symbol table */

/* Global symbol data is recorded in these structures,
   one for each global symbol.
   They are found via hashing in 'symtab', which points to a vector of buckets.
   Each bucket is a chain of these structures through the link field.  */

typedef
  struct glosym
    {
      /* Pointer to next symbol in this symbol's hash bucket.  */
      struct glosym *link;
      /* Name of this symbol.  */
      char *name;
      /* Value of this symbol as a global symbol.  */
      long value;
      /* Chain of external 'nlist's in files for this symbol, both defs
	 and refs.  */
      struct nlist *refs;
      /* Any warning message that might be associated with this symbol
         from an N_WARNING symbol encountered. */
      char *warning;
      /* Nonzero means definitions of this symbol as common have been seen,
	 and the value here is the largest size specified by any of them.  */
      int max_common_size;
      /* For OUTPUT_RELOCATABLE, records the index of this global sym in the
	 symbol table to be written, with the first global sym given index 0.*/
      int def_count;
      /* Nonzero means a definition of this global symbol is known to exist.
	 Library members should not be loaded on its account.  */
      char defined;
      /* Nonzero means a reference to this global symbol has been seen
	 in a file that is surely being loaded.
	 A value higher than 1 is the n_type code for the symbol's
	 definition.  */
      char referenced;
      /* A count of the number of undefined references printed for a
	 specific symbol.  If a symbol is unresolved at the end of
	 digest_symbols (and the loading run is supposed to produce
	 relocatable output) do_file_warnings keeps track of how many
	 unresolved reference error messages have been printed for
	 each symbol here.  When the number hits MAX_UREFS_PRINTED,
	 messages stop. */
      unsigned char undef_refs;
      /* 1 means that this symbol has multiple definitions.  2 means
         that it has multiple definitions, and some of them are set
	 elements, one of which has been printed out already.  */
      unsigned char multiply_defined;
      /* Pointer to the file_entry for the last library that contained a
	 subfile referencing this symbol.  */
      struct file_entry *last_library_ref;
      /* Nonzero means print a message at all refs or defs of this symbol */
      char trace;
    }
  symbol;

/* Demangler for C++.  */
extern char *cplus_demangle ();

/* Demangler function to use.  We unconditionally enable the C++ demangler
   because we assume any name it successfully demangles was probably produced
   by the C++ compiler.  Enabling it only if -lg++ was specified seems too
   much of a kludge.  */
char *(*demangler)() = cplus_demangle;

/* Number of buckets in symbol hash table */
#define	TABSIZE	1009

/* The symbol hash table: a vector of TABSIZE pointers to struct glosym. */
symbol *symtab[TABSIZE];

/* Number of symbols in symbol hash table. */
int num_hash_tab_syms = 0;

/* Count the number of nlist entries that are for local symbols.
   This count and the three following counts
   are incremented as as symbols are entered in the symbol table.  */
int local_sym_count;

/* Count number of nlist entries that are for local symbols
   whose names don't start with L. */
int non_L_local_sym_count;

/* Count the number of nlist entries for debugger info.  */
int debugger_sym_count;

/* Count the number of global symbols referenced and not defined.  */
int undefined_global_sym_count;

/* Count the number of global symbols multiply defined.  */
int multiple_def_count;

/* Count the number of defined global symbols.
   Each symbol is counted only once
   regardless of how many different nlist entries refer to it,
   since the output file will need only one nlist entry for it.
   This count is computed by `digest_symbols';
   it is undefined while symbols are being loaded. */
int defined_global_sym_count;

/* Count the number of symbols defined through common declarations.
   This count is kept in symdef_library, linear_library, and
   enter_global_ref.  It is incremented when the defined flag is set
   in a symbol because of a common definition, and decremented when
   the symbol is defined "for real" (ie. by something besides a common
   definition).  */
int common_defined_global_count;

/* Count the number of set element type symbols and the number of
   separate vectors which these symbols will fit into.  See the
   GNU a.out.h for more info.
   This count is computed by 'enter_file_symbols' */
int set_symbol_count;
int set_vector_count;

/* Define a linked list of strings which define symbols which should
   be treated as set elements even though they aren't.  Any symbol
   with a prefix matching one of these should be treated as a set
   element.

   This is to make up for deficiencies in many assemblers which aren't
   willing to pass any stabs through to the loader which they don't
   understand.  */
struct string_list_element {
  char *str;
  struct string_list_element *next;
};

struct string_list_element *set_element_prefixes;

/* Count the number of definitions done indirectly (ie. done relative
   to the value of some other symbol. */
int global_indirect_count;

/* Count the number of warning symbols encountered. */
int warning_count;

/* Total number of symbols to be written in the output file.
   Computed by digest_symbols from the variables above.  */
int nsyms;


/* Nonzero means ptr to symbol entry for symbol to use as start addr.
   -e sets this.  */
symbol *entry_symbol;

/* These can be NULL if we don't actually have such a symbol.  */
symbol *edata_symbol;   /* the symbol _edata */
symbol *etext_symbol;   /* the symbol _etext */
symbol *end_symbol;	/* the symbol _end */
/* We also need __{edata,etext,end} so that they can safely
   be used from an ANSI library.  */
symbol *edata_symbol_alt;
symbol *etext_symbol_alt;
symbol *end_symbol_alt;

/* Kinds of files potentially understood by the linker. */

enum file_type { IS_UNKNOWN, IS_ARCHIVE, IS_A_OUT, IS_MACH_O };

/* Each input file, and each library member ("subfile") being loaded,
   has a `file_entry' structure for it.

   For files specified by command args, these are contained in the vector
   which `file_table' points to.

   For library members, they are dynamically allocated,
   and chained through the `chain' field.
   The chain is found in the `subfiles' field of the `file_entry'.
   The `file_entry' objects for the members have `superfile' fields pointing
   to the one for the library.  */

struct file_entry {
  /* Name of this file.  */
  char *filename;

  /* What kind of file this is. */
  enum file_type file_type;

  /* Name to use for the symbol giving address of text start */
  /* Usually the same as filename, but for a file spec'd with -l
     this is the -l switch itself rather than the filename.  */
  char *local_sym_name;

  /* Describe the layout of the contents of the file.  */

  /* The text section. */
  unsigned long int orig_text_address;
  unsigned long int text_size;
  long int text_offset;

  /* Text relocation. */
  unsigned long int text_reloc_size;
  long int text_reloc_offset;

  /* The data section. */
  unsigned long int orig_data_address;
  unsigned long int data_size;
  long int data_offset;

  /* Data relocation. */
  unsigned long int data_reloc_size;
  long int data_reloc_offset;

  /* The bss section. */
  unsigned long int orig_bss_address;
  unsigned long int bss_size;

  /* The symbol and string tables. */
  unsigned long int syms_size;
  long int syms_offset;
  unsigned long int strs_size;
  long int strs_offset;

  /* The GDB symbol segment, if any. */
  unsigned long int symseg_size;
  long int symseg_offset;

#ifdef MACH_O
  /* Section ordinals from the Mach-O load commands.  These
     are compared with the n_sect fields of symbols.  */
  int text_ordinal;
  int data_ordinal;
  int bss_ordinal;
#endif

  /* Describe data from the file loaded into core */

  /* Symbol table of the file.  */
  struct nlist *symbols;

  /* Pointer to the string table.
     The string table is not kept in core all the time,
     but when it is in core, its address is here.  */
  char *strings;

  /* Next two used only if OUTPUT_RELOCATABLE or if needed for */
  /* output of undefined reference line numbers. */

  /* Text reloc info saved by `write_text' for `coptxtrel'.  */
  struct relocation_info *textrel;
  /* Data reloc info saved by `write_data' for `copdatrel'.  */
  struct relocation_info *datarel;

  /* Relation of this file's segments to the output file */

  /* Start of this file's text seg in the output file core image.  */
  int text_start_address;
  /* Start of this file's data seg in the output file core image.  */
  int data_start_address;
  /* Start of this file's bss seg in the output file core image.  */
  int bss_start_address;
  /* Offset in bytes in the output file symbol table
     of the first local symbol for this file.  Set by `write_file_symbols'.  */
  int local_syms_offset;

  /* For library members only */

  /* For a library, points to chain of entries for the library members.  */
  struct file_entry *subfiles;
  /* For a library member, offset of the member within the archive.
     Zero for files that are not library members.  */
  int starting_offset;
  /* Size of contents of this file, if library member.  */
  int total_size;
  /* For library member, points to the library's own entry.  */
  struct file_entry *superfile;
  /* For library member, points to next entry for next member.  */
  struct file_entry *chain;

  /* 1 if file is a library. */
  char library_flag;

  /* 1 if file's header has been read into this structure.  */
  char header_read_flag;

  /* 1 means search a set of directories for this file.  */
  char search_dirs_flag;

  /* 1 means this is base file of incremental load.
     Do not load this file's text or data.
     Also default text_start to after this file's bss. */
  char just_syms_flag;
};

/* Vector of entries for input files specified by arguments.
   These are all the input files except for members of specified libraries.  */
struct file_entry *file_table;

/* Length of that vector.  */
int number_of_files;

/* When loading the text and data, we can avoid doing a close
   and another open between members of the same library.

   These two variables remember the file that is currently open.
   Both are zero if no file is open.

   See `each_file' and `file_close'.  */

struct file_entry *input_file;
int input_desc;

/* The name of the file to write; "a.out" by default.  */

char *output_filename;

/* What kind of output file to write.  */

enum file_type output_file_type;

#ifndef DEFAULT_OUTPUT_FILE_TYPE
#ifdef MACH_O
#define DEFAULT_OUTPUT_FILE_TYPE IS_MACH_O
#else
#define DEFAULT_OUTPUT_FILE_TYPE IS_A_OUT
#endif
#endif

/* What `style' of output file to write.  For BSD a.out files
   this specifies OMAGIC, NMAGIC, or ZMAGIC.  For Mach-O files
   this switches between MH_OBJECT and two flavors of MH_EXECUTE.  */

enum output_style
  {
    OUTPUT_UNSPECIFIED,
    OUTPUT_RELOCATABLE,		/* -r */
    OUTPUT_WRITABLE_TEXT,	/* -N */
    OUTPUT_READONLY_TEXT,	/* -n */
    OUTPUT_DEMAND_PAGED		/* -Z (default) */
  };

enum output_style output_style;

#ifndef DEFAULT_OUTPUT_STYLE
#define DEFAULT_OUTPUT_STYLE OUTPUT_DEMAND_PAGED
#endif

/* Descriptor for writing that file with `mywrite'.  */

int outdesc;

/* The following are computed by `digest_symbols'.  */

int text_size;			/* total size of text of all input files.  */
int text_header_size;		/* size of the file header if included in the
				   text size.  */
int data_size;			/* total size of data of all input files.  */
int bss_size;			/* total size of bss of all input files.  */
int text_reloc_size;		/* total size of text relocation of all input files.  */
int data_reloc_size;		/* total size of data relocation of all input
				   files.  */
  
/* The following are computed by write_header().  */
long int output_text_offset;	/* file offset of the text section.  */
long int output_data_offset;	/* file offset of the data section.  */
long int output_trel_offset;	/* file offset of the text relocation info.  */
long int output_drel_offset;	/* file offset of the data relocation info.  */
long int output_syms_offset;	/* file offset of the symbol table.  */
long int output_strs_offset;	/* file offset of the string table.  */

/* The following are incrementally computed by write_syms(); we keep
   them here so we can examine their values afterwards.  */
unsigned int output_syms_size;	/* total bytes of symbol table output. */
unsigned int output_strs_size;	/* total bytes of string table output. */

/* This can only be computed after the size of the string table is known.  */
long int output_symseg_offset;	/* file offset of the symbol segment (if any).  */

/* Incrementally computed by write_file_symseg().  */
unsigned int output_symseg_size;

/* Specifications of start and length of the area reserved at the end
   of the text segment for the set vectors.  Computed in 'digest_symbols' */
int set_sect_start;
int set_sect_size;

/* Pointer for in core storage for the above vectors, before they are
   written. */
unsigned long *set_vectors;

/* Amount of cleared space to leave at the end of the text segment.  */

int text_pad;

/* Amount of padding at end of data segment.  This has two parts:
   That which is before the bss segment, and that which overlaps
   with the bss segment.  */
int data_pad;

/* Format of __.SYMDEF:
   First, a longword containing the size of the 'symdef' data that follows.
   Second, zero or more 'symdef' structures.
   Third, a longword containing the length of symbol name strings.
   Fourth, zero or more symbol name strings (each followed by a null).  */

struct symdef {
  int symbol_name_string_index;
  int library_member_offset;
};

/* Record most of the command options.  */

/* Address we assume the text section will be loaded at.
   We relocate symbols and text and data for this, but we do not
   write any padding in the output file for it.  */
int text_start;

/* Address we decide the data section will be loaded at.  */
int data_start;

/* Nonzero if -T was specified in the command line.
   This prevents text_start from being set later to default values.  */
int T_flag_specified;

/* Nonzero if -Tdata was specified in the command line.
   This prevents data_start from being set later to default values.  */
int Tdata_flag_specified;

/* Size to pad data section up to.
   We simply increase the size of the data section, padding with zeros,
   and reduce the size of the bss section to match.  */
int specified_data_size;

/* Nonzero means print names of input files as processed.  */
int trace_files;

/* Which symbols should be stripped (omitted from the output):
   none, all, or debugger symbols.  */
enum { STRIP_NONE, STRIP_ALL, STRIP_DEBUGGER } strip_symbols;

/* Which local symbols should be omitted:
   none, all, or those starting with L.
   This is irrelevant if STRIP_NONE.  */
enum { DISCARD_NONE, DISCARD_ALL, DISCARD_L } discard_locals;

/* 1 => write load map.  */
int write_map;

/* 1 => assign space to common symbols even if OUTPUT_RELOCATABLE. */
int force_common_definition;

/* Standard directories to search for files specified by -l.  */
char *standard_search_dirs[] =
#ifdef STANDARD_SEARCH_DIRS
  {STANDARD_SEARCH_DIRS};
#else
#ifdef NON_NATIVE
#ifdef linux
  {"/usr2/linux/cross/lib"};
#else
  {"/usr/local/lib/gnu"};
#endif
#else
  {"/lib", "/usr/lib", "/usr/local/lib"};
#endif
#endif

/* If set STANDARD_SEARCH_DIRS is not searched.  */
int no_standard_dirs;

/* Actual vector of directories to search;
   this contains those specified with -L plus the standard ones.  */
char **search_dirs;

/* Length of the vector `search_dirs'.  */
int n_search_dirs;

/* Non zero means to create the output executable.
   Cleared by nonfatal errors.  */
int make_executable;

/* Force the executable to be output, even if there are non-fatal
   errors */
int force_executable;

/* Keep a list of any symbols referenced from the command line (so
   that error messages for these guys can be generated). This list is
   zero terminated. */
struct glosym **cmdline_references;
int cl_refs_allocated;

#ifdef linux
/* For Linux. */

int linux_library;

#define LINUX_JUMP_SHARED_LIB		0
#define LINUX_CLASSIC_SHARED_LIB	1
#define LINUX_STATIC_LIB		2

static char *linux_library_suffix [] = {
	".sa",		/* jump table shared library, default */
	".ca",		/* classic shared library */
	".a",		/* static library */
	NULL
};

#endif

#ifndef bcopy
void bcopy (), bzero ();
#endif
#ifdef __STDC__
#include <stdlib.h>
#else
char *malloc (), *realloc ();
void free ();
#endif

char *xmalloc ();
char *xrealloc ();
void usage ();
void fatal ();
void fatal_with_file ();
void perror_name ();
void perror_file ();
void error ();

int parse ();
void initialize_text_start ();
void initialize_data_start ();
void digest_symbols ();
void print_symbols ();
void load_symbols ();
void decode_command ();
void list_undefined_symbols ();
void list_unresolved_references ();
void write_output ();
void write_header ();
void write_text ();
void read_file_relocation ();
void write_data ();
void write_rel ();
void write_syms ();
void write_symsegs ();
void mywrite ();
void symtab_init ();
void padfile ();
char *concat ();
char *get_file_name ();
symbol *getsym (), *getsym_soft ();

int
main (argc, argv)
     char **argv;
     int argc;
{
  page_size = getpagesize ();
  progname = argv[0];

#ifdef RLIMIT_STACK
  /* Avoid dumping core on large .o files.  */
  {
    struct rlimit rl;

    getrlimit (RLIMIT_STACK, &rl);
    rl.rlim_cur = rl.rlim_max;
    setrlimit (RLIMIT_STACK, &rl);
  }
#endif

  /* Clear the cumulative info on the output file.  */

  text_size = 0;
  data_size = 0;
  bss_size = 0;
  text_reloc_size = 0;
  data_reloc_size = 0;

  data_pad = 0;
  text_pad = 0;

  /* Initialize the data about options.  */

  specified_data_size = 0;
  strip_symbols = STRIP_NONE;
  trace_files = 0;
  discard_locals = DISCARD_NONE;
  entry_symbol = 0;
  write_map = 0;
  force_common_definition = 0;
  T_flag_specified = 0;
  Tdata_flag_specified = 0;
  make_executable = 1;
  force_executable = 0;
  set_element_prefixes = 0;

  /* Initialize the cumulative counts of symbols.  */

  local_sym_count = 0;
  non_L_local_sym_count = 0;
  debugger_sym_count = 0;
  undefined_global_sym_count = 0;
  set_symbol_count = 0;
  set_vector_count = 0;
  global_indirect_count = 0;
  warning_count = 0;
  multiple_def_count = 0;
  common_defined_global_count = 0;

#ifdef linux
  /* For Linux. Defaut is jump table shared library. */
  linux_library = LINUX_JUMP_SHARED_LIB;
#endif

  /* Keep a list of symbols referenced from the command line */

  cl_refs_allocated = 10;
  cmdline_references
    = (struct glosym **) xmalloc (cl_refs_allocated
				  * sizeof(struct glosym *));
  *cmdline_references = 0;

  /* Completely decode ARGV.  */

  decode_command (argc, argv);

  /* Load symbols of all input files.
     Also search all libraries and decide which library members to load.  */

  load_symbols ();

  /* Create various built-in symbols.  This must occur after
     all input files are loaded so that a user program can have a
     symbol named etext (for example).  */

  if (output_style != OUTPUT_RELOCATABLE)
    symtab_init ();

  /* Compute where each file's sections go, and relocate symbols.  */

  digest_symbols ();

  /* Print error messages for any missing symbols, for any warning
     symbols, and possibly multiple definitions */

  do_warnings (stderr);

  /* Print a map, if requested.  */

  if (write_map) print_symbols (stdout);

  /* Write the output file.  */

  if (make_executable || force_executable)
    write_output ();

  exit (!make_executable);
}

void add_cmdline_ref ();

static struct option longopts[] =
{
  {"d", 0, 0, 'd'},
  {"dc", 0, 0, 'd'},		/* For Sun compatibility. */
  {"dp", 0, 0, 'd'},		/* For Sun compatibility. */
  {"e", 1, 0, 'e'},
  {"n", 0, 0, 'n'},
  {"noinhibit-exec", 0, 0, 130},
  {"nostdlib", 0, 0, 133},
  {"o", 1, 0, 'o'},
  {"r", 0, 0, 'r'},
  {"s", 0, 0, 's'},
  {"t", 0, 0, 't'},
  {"u", 1, 0, 'u'},
  {"x", 0, 0, 'x'},
  {"z", 0, 0, 'z'},
  {"A", 1, 0, 'A'},
  {"Bstatic", 0, 0, 129},	/* For Sun compatibility. */
  {"D", 1, 0, 'D'},
  {"M", 0, 0, 'M'},
  {"N", 0, 0, 'N'},
  {"S", 0, 0, 'S'},
  {"T", 1, 0, 'T'},
  {"Ttext", 1, 0, 'T'},
  {"Tdata", 1, 0, 132},
  {"V", 1, 0, 'V'},
  {"X", 0, 0, 'X'},
#ifdef linux
  {"static", 0, 0, 200},	/* For Linux. */
  {"nojump", 0, 0, 201},	/* For Linux. */
#endif
  {0, 0, 0, 0}
};

/* Since the Unix ld accepts -lfoo, -Lfoo, and -yfoo, we must also.
   This effectively prevents any long options from starting with
   one of these letters. */
#define SHORTOPTS "-l:y:L:"

/* Process the command arguments,
   setting up file_table with an entry for each input file,
   and setting variables according to the options.  */

void
decode_command (argc, argv)
     char **argv;
     int argc;
{
  int optc, longind;
  register struct file_entry *p;

  number_of_files = 0;
  output_filename = "a.out";

  n_search_dirs = 0;
  search_dirs = (char **) xmalloc (sizeof (char *));

  /* First compute number_of_files so we know how long to make file_table.
     Also process most options completely.  */

  while ((optc = getopt_long_only (argc, argv, SHORTOPTS, longopts, &longind))
	 != EOF)
    {
      if (optc == 0)
	optc = longopts[longind].val;

      switch (optc)
	{
	case '?':
	  usage (0, 0);
	  break;

	case 1:
	  /* Non-option argument. */
	  number_of_files++;
	  break;

	case 'd':
	  force_common_definition = 1;
	  break;

	case 'e':
	  entry_symbol = getsym (optarg);
	  if (!entry_symbol->defined && !entry_symbol->referenced)
	    undefined_global_sym_count++;
	  entry_symbol->referenced = 1;
	  add_cmdline_ref (entry_symbol);
	  break;

	case 'l':
	  number_of_files++;
	  break;

	case 'n':
	  if (output_style && output_style != OUTPUT_READONLY_TEXT)
	    fatal ("illegal combination of -n with -N, -r, or -z", (char *) 0);
	  output_style = OUTPUT_READONLY_TEXT;
	  break;

	case 130:		/* -noinhibit-exec */
	  force_executable = 1;
	  break;

	case 133:		/* -nostdlib */
	  no_standard_dirs = 1;
	  break;

	case 'o':
	  output_filename = optarg;
	  break;

	case 'r':
	  if (output_style && output_style != OUTPUT_RELOCATABLE)
	    fatal ("illegal combination of -r with -N, -n, or -z", (char *) 0);
	  output_style = OUTPUT_RELOCATABLE;
	  text_start = 0;
	  break;

	case 's':
	  strip_symbols = STRIP_ALL;
	  break;

	case 't':
	  trace_files = 1;
	  break;

	case 'u':
	  {
	    register symbol *sp = getsym (optarg);

	    if (!sp->defined && !sp->referenced)
	      undefined_global_sym_count++;
	    sp->referenced = 1;
	    add_cmdline_ref (sp);
	  }
	  break;

	case 'x':
	  discard_locals = DISCARD_ALL;
	  break;
	  
	case 'y':
	  {
	    register symbol *sp = getsym (optarg);

	    sp->trace = 1;
	  }
	  break;

	case 'z':
	  if (output_style && output_style != OUTPUT_DEMAND_PAGED)
	    fatal ("illegal combination of -z with -N, -n, or -r", (char *) 0);
	  output_style = OUTPUT_DEMAND_PAGED;
	  break;

	case 'A':
	  number_of_files++;
	  break;

	case 129:		/* -Bstatic. */
	  /* Ignore. */
	  break;

	case 'D':
	  specified_data_size = parse (optarg, "%x", "invalid argument to -D");
	  break;

	case 'L':
	  n_search_dirs++;
	  search_dirs = (char **)
	    xrealloc (search_dirs, n_search_dirs * sizeof (char *));
	  search_dirs[n_search_dirs - 1] = optarg;
	  break;
	  
	case 'M':
	  write_map = 1;
	  break;

	case 'N':
	  if (output_style && output_style != OUTPUT_WRITABLE_TEXT)
	    fatal ("illegal combination of -N with -n, -r, or -z", (char *) 0);
	  output_style = OUTPUT_WRITABLE_TEXT;
	  break;

	case 'S':
	  strip_symbols = STRIP_DEBUGGER;
	  break;

	case 'T':
	  text_start = parse (optarg, "%x", "invalid argument to -Ttext");
	  T_flag_specified = 1;
	  break;

	case 132:		/* -Tdata addr */
	  data_start = parse (optarg, "%x", "invalid argument to -Tdata");
	  Tdata_flag_specified = 1;
	  break;

	case 'V':
	  {
	    struct string_list_element *new
	      = (struct string_list_element *)
		xmalloc (sizeof (struct string_list_element));
	    
	    new->str = optarg;
	    new->next = set_element_prefixes;
	    set_element_prefixes = new;
	  }
	  break;

	case 'X':
	  discard_locals = DISCARD_L;
	  break;

#ifdef linux
  	/* For Linux. -static, static library*/
	case 200:
	  if (linux_library == LINUX_CLASSIC_SHARED_LIB)
	  	fatal ("illegal combination of -static with -nojump", (char *) 0);
	  linux_library = LINUX_STATIC_LIB;
	  break;

  	/* For Linux. -nojump, classic shared library*/
	case 201:
	  if (linux_library == LINUX_STATIC_LIB)
		fatal ("illegal combination of -nojump with -static", (char *) 0);
	  linux_library = LINUX_CLASSIC_SHARED_LIB;
	  break;
#endif
	}
    }

  if (!number_of_files)
    usage ("no input files", 0);

  p = file_table
    = (struct file_entry *) xmalloc (number_of_files * sizeof (struct file_entry));
  bzero (p, number_of_files * sizeof (struct file_entry));

  /* Now scan again and fill in file_table.
     All options except -A and -l are ignored here.  */

  optind = 0;			/* Reset getopt. */
  while ((optc = getopt_long_only (argc, argv, SHORTOPTS, longopts, &longind))
	 != EOF)
    {
      if (optc == 0)
	optc = longopts[longind].val;

      switch (optc)
	{
	case 1:
	  /* Non-option argument. */
	  p->filename = optarg;
	  p->local_sym_name = optarg;
	  p++;
	  break;

	case 'A':
	  if (p != file_table)
	    usage ("-A specified before an input file other than the first");
	  p->filename = optarg;
	  p->local_sym_name = optarg;
	  p->just_syms_flag = 1;
	  p++;
	  break;

	case 'l':
#ifdef linux
	  p->filename = concat ("lib", optarg, "");
#else
	  p->filename = concat ("lib", optarg, ".a");
#endif
	  p->local_sym_name = concat ("-l", optarg, "");
	  p->search_dirs_flag = 1;
	  p++;
	  break;
	}
    }

  if (!output_file_type)
    output_file_type = DEFAULT_OUTPUT_FILE_TYPE;

  if (!output_style)
    output_style = DEFAULT_OUTPUT_STYLE;

#if 0
  /* THIS CONSISTENCY CHECK BELONGS SOMEWHERE ELSE.  */
  /* Now check some option settings for consistency.  */

  if ((output_style == OUTPUT_READONLY_TEXT || output_style == OUTPUT_DEMAND_PAGED)
      && (text_start - text_start_alignment) & (page_size - 1))
    usage ("-T argument not multiple of page size, with sharable output", 0);
#endif

  /* Append the standard search directories to the user-specified ones.  */
  if (!no_standard_dirs)
    {
      int n = sizeof standard_search_dirs / sizeof standard_search_dirs[0];
      n_search_dirs += n;
      search_dirs
	= (char **) xrealloc (search_dirs, n_search_dirs * sizeof (char *));
      bcopy (standard_search_dirs, &search_dirs[n_search_dirs - n],
	     n * sizeof (char *));
    }
}


void
add_cmdline_ref (sp)
     struct glosym *sp;
{
  struct glosym **ptr;

  for (ptr = cmdline_references;
       ptr < cmdline_references + cl_refs_allocated && *ptr;
       ptr++)
    ;

  if (ptr >= cmdline_references + cl_refs_allocated - 1)
    {
      int diff = ptr - cmdline_references;

      cl_refs_allocated *= 2;
      cmdline_references = (struct glosym **)
	xrealloc (cmdline_references,
		 cl_refs_allocated * sizeof (struct glosym *));
      ptr = cmdline_references + diff;
    }

  *ptr++ = sp;
  *ptr = (struct glosym *) 0;
}

int
set_element_prefixed_p (name)
     char *name;
{
  struct string_list_element *p;
  int i;

  for (p = set_element_prefixes; p; p = p->next)
    {
      for (i = 0; p->str[i] != '\0' && (p->str[i] == name[i]); i++)
	;

      if (p->str[i] == '\0')
	return 1;
    }
  return 0;
}

/* Convenient functions for operating on one or all files being
   loaded.  */
void print_file_name ();

/* Call FUNCTION on each input file entry.
   Do not call for entries for libraries;
   instead, call once for each library member that is being loaded.

   FUNCTION receives two arguments: the entry, and ARG.  */

void
each_file (function, arg)
     register void (*function)();
     register int arg;
{
  register int i;

  for (i = 0; i < number_of_files; i++)
    {
      register struct file_entry *entry = &file_table[i];
      if (entry->library_flag)
        {
	  register struct file_entry *subentry = entry->subfiles;
	  for (; subentry; subentry = subentry->chain)
	    (*function) (subentry, arg);
	}
      else
	(*function) (entry, arg);
    }
}

/* Call FUNCTION on each input file entry until it returns a non-zero
   value.  Return this value.
   Do not call for entries for libraries;
   instead, call once for each library member that is being loaded.

   FUNCTION receives two arguments: the entry, and ARG.  It must be a
   function returning unsigned long (though this can probably be fudged). */

unsigned long
check_each_file (function, arg)
     register unsigned long (*function)();
     register int arg;
{
  register int i;
  register unsigned long return_val;

  for (i = 0; i < number_of_files; i++)
    {
      register struct file_entry *entry = &file_table[i];
      if (entry->library_flag)
        {
	  register struct file_entry *subentry = entry->subfiles;
	  for (; subentry; subentry = subentry->chain)
	    if (return_val = (*function) (subentry, arg))
	      return return_val;
	}
      else
	if (return_val = (*function) (entry, arg))
	  return return_val;
    }
  return 0;
}

/* Like `each_file' but ignore files that were just for symbol definitions.  */

void
each_full_file (function, arg)
     register void (*function)();
     register int arg;
{
  register int i;

  for (i = 0; i < number_of_files; i++)
    {
      register struct file_entry *entry = &file_table[i];
      if (entry->just_syms_flag)
	continue;
      if (entry->library_flag)
        {
	  register struct file_entry *subentry = entry->subfiles;
	  for (; subentry; subentry = subentry->chain)
	    (*function) (subentry, arg);
	}
      else
	(*function) (entry, arg);
    }
}

/* Close the input file that is now open.  */

void
file_close ()
{
  close (input_desc);
  input_desc = 0;
  input_file = 0;
}

#ifdef linux
/* We assume jump table and classic shared libraries are not
 * compatible with each other. But they are compatile with
 * classic library individually.
 */

static inline int 
next_library_suffix (int index)
{
   switch (index) {
   case LINUX_JUMP_SHARED_LIB:
   case LINUX_CLASSIC_SHARED_LIB:
      index = LINUX_STATIC_LIB;
      break;
   default:
      index++;
   }
   return index;
}

#endif

/* Open the input file specified by 'entry', and return a descriptor.
   The open file is remembered; if the same file is opened twice in a row,
   a new open is not actually done.  */

int
file_open (entry)
     register struct file_entry *entry;
{
  register int desc = -1;

  if (entry->superfile)
    return file_open (entry->superfile);

  if (entry == input_file)
    return input_desc;

  if (input_file) file_close ();

  if (entry->search_dirs_flag && n_search_dirs)
    {
#ifdef linux
      int i, j;
      int found = 0;
      char *suffix, *filename, *string;

      for (j = linux_library;
		!found && (suffix = linux_library_suffix [j]);
		j = next_library_suffix (j)) {

	/* Try each suffix */
	filename = concat (entry->filename, suffix, "");

#if 0
fprintf (stderr, "Try filename: %s\n", filename);
#endif

        for (i = 0; i < n_search_dirs; i++) {
	  string = concat (search_dirs[i], "/", filename);

#if 0
fprintf (stderr, "Try pathname: %s\n", string);
#endif

	  desc = open (string, O_RDONLY, 0);
	  if (desc >= 0) {
	      /* memory leek? */
	      free (entry->filename);

	      entry->filename = string;
	      entry->search_dirs_flag = 0;
	      found = 1;
	      break;
	  }
	  free (string);
	}

	/* we need to free the `filename'. */
	free (filename);
      }
#else
      int i;

      for (i = 0; i < n_search_dirs; i++)
	{
	  register char *string
	    = concat (search_dirs[i], "/", entry->filename);
	  desc = open (string, O_RDONLY, 0);
	  if (desc > 0)
	    {
	      entry->filename = string;
	      entry->search_dirs_flag = 0;
	      break;
	    }
	  free (string);
	}
#endif
    }
  else
    desc = open (entry->filename, O_RDONLY, 0);

  if (desc >= 0)
    {
      input_file = entry;
      input_desc = desc;
      return desc;
    }

  perror_file (entry);
  /* NOTREACHED */
}

/* Print the filename of ENTRY on OUTFILE (a stdio stream),
   and then a newline.  */

void
prline_file_name (entry, outfile)
     struct file_entry *entry;
     FILE *outfile;
{
  print_file_name (entry, outfile);
  fprintf (outfile, "\n");
}

/* Print the filename of ENTRY on OUTFILE (a stdio stream).  */

void
print_file_name (entry, outfile)
     struct file_entry *entry;
     FILE *outfile;
{
  if (entry->superfile)
    {
      print_file_name (entry->superfile, outfile);
      fprintf (outfile, "(%s)", entry->filename);
    }
  else
    fprintf (outfile, "%s", entry->filename);
}

/* Return the filename of entry as a string (malloc'd for the purpose) */

char *
get_file_name (entry)
     struct file_entry *entry;
{
  char *result, *supfile;
  if (entry->superfile)
    {
      supfile = get_file_name (entry->superfile);
      result = (char *) xmalloc (strlen (supfile)
				 + strlen (entry->filename) + 3);
      sprintf (result, "%s(%s)", supfile, entry->filename);
      free (supfile);
    }
  else
    {
      result = (char *) xmalloc (strlen (entry->filename) + 1);
      strcpy (result, entry->filename);
    }
  return result;
}

/* Medium-level input routines for rel files.  */

/* Determine whether the given ENTRY is an archive, a BSD a.out file,
   a Mach-O file, or whatever.  DESC is the descriptor on which the
   file is open.  */
void
deduce_file_type(desc, entry)
     int desc;
     struct file_entry *entry;
{
  int len;

  {
    char magic[SARMAG];
    
    lseek (desc, entry->starting_offset, 0);
    len = read (desc, magic, SARMAG);
    if (len == SARMAG && !strncmp(magic, ARMAG, SARMAG))
      {
	entry->file_type = IS_ARCHIVE;
	return;
      }
#ifdef LARMAG
    /* Odd hack for Tektronix. */
    if (len == SARMAG && !strncmp(magic, LARMAG, SARMAG))
      {
	entry->file_type = IS_ARCHIVE;
	return;
      }
#endif
  }

#ifdef A_OUT
  {
    struct exec hdr;

    lseek (desc, entry->starting_offset, 0);
#ifdef COFF_ENCAPSULATE
    if (entry->just_syms_flag)
      /* Since a file given with -A will have a coff header, unlike normal
	input files, we need to skip over it.  */
      lseek (desc, sizeof (struct coffheader), SEEK_CUR);
#endif
    len = read (desc, (char *) &hdr, sizeof (struct exec));
    if (len == sizeof (struct exec) && !N_BADMAG (hdr))
      {
	entry->file_type = IS_A_OUT;
	return;
      }
  }
#endif

#ifdef MACH_O
  {
    struct mach_header hdr;

    lseek (desc, entry->starting_offset, 0);
    len = read (desc, (char *) &hdr, sizeof (struct mach_header));
    if (len == sizeof (struct mach_header) && hdr.magic == MH_MAGIC)
      {
	entry->file_type = IS_MACH_O;
	return;
      }
  }
#endif

  fatal_with_file ("malformed input file (not rel or archive) ", entry);
}

#ifdef A_OUT
/* Read an a.out file's header and set up the fields of
   the ENTRY accordingly.  DESC is the descriptor on which
   the file is open.  */
void
read_a_out_header (desc, entry)
     int desc;
     struct file_entry *entry;
{
  register int len;
  struct exec hdr;
  struct stat st;

  lseek (desc, entry->starting_offset, 0);

#ifdef COFF_ENCAPSULATE
  if (entry->just_syms_flag)
    /* Since a file given with -A will have a coff header, unlike normal
       input files, we need to skip over it.  */
    lseek (desc, sizeof (struct coffheader), SEEK_CUR);
#endif

  read (desc, (char *) &hdr, sizeof (struct exec));

#ifdef READ_HEADER_HOOK
  READ_HEADER_HOOK(hdr.a_machtype);
#endif

  if (entry->just_syms_flag)
    entry->orig_text_address = N_TXTADDR(hdr);
  else
    entry->orig_text_address = 0;
  entry->text_size = hdr.a_text;
  entry->text_offset = N_TXTOFF(hdr);

  entry->text_reloc_size = hdr.a_trsize;
#ifdef N_TRELOFF
  entry->text_reloc_offset = N_TRELOFF(hdr);
#else
#ifdef N_DATOFF
  entry->text_reloc_offset = N_DATOFF(hdr) + hdr.a_data;
#else
  entry->text_reloc_offset = N_TXTOFF(hdr) + hdr.a_text + hdr.a_data;
#endif
#endif

  if (entry->just_syms_flag)
    entry->orig_data_address = N_DATADDR(hdr);
  else
    entry->orig_data_address = entry->text_size;
  entry->data_size = hdr.a_data;
#ifdef N_DATOFF
  entry->data_offset = N_DATOFF(hdr);
#else
  entry->data_offset = N_TXTOFF(hdr) + hdr.a_text;
#endif

#ifdef NMAGIC
  /* If this is an NMAGIC file, then even though the text and data
     are contiguous in the input file, the data has _already_ been
     relocated to the next page boundary.
     Change orig_data_address to reflect this.  */
#ifdef N_MAGIC
  if (N_MAGIC(hdr) == NMAGIC)
#else
  if (hdr.a_magic == NMAGIC)
#endif
    entry->orig_data_address += (((hdr.a_text + page_size - 1)
				  & ~(page_size - 1)) - hdr.a_text);
#endif

  entry->data_reloc_size = hdr.a_drsize;
#ifdef N_DRELOFF
  entry->data_reloc_offset = N_DRELOFF(hdr);
#else
  entry->data_reloc_offset = entry->text_reloc_offset + entry->text_reloc_size;
#endif

#ifdef N_BSSADDR
  if (entry->just_syms_flag)
    entry->orig_bss_address = N_BSSADDR(hdr);
  else
#endif
  entry->orig_bss_address = entry->orig_data_address + entry->data_size;
  entry->bss_size = hdr.a_bss;

  entry->syms_size = hdr.a_syms;
  entry->syms_offset = N_SYMOFF(hdr);
  entry->strs_offset = N_STROFF(hdr);
  lseek(desc, entry->starting_offset + entry->strs_offset, 0);
  if (read(desc, (char *) &entry->strs_size, sizeof (unsigned long int))
      != sizeof (unsigned long int))
    fatal_with_file ("failure reading string table size of ", entry);

  if (!entry->superfile)
    {
      fstat(desc, &st);
      if (st.st_size > entry->strs_offset + entry->strs_size)
	{
	  entry->symseg_size = st.st_size - (entry->strs_offset + entry->strs_size);
	  entry->symseg_offset = entry->strs_offset + entry->strs_size;
	}
    }
  else
    if (entry->total_size > entry->strs_offset + entry->strs_size)
      {
	entry->symseg_size = entry->total_size - (entry->strs_offset + entry->strs_size);
	entry->symseg_offset = entry->strs_offset + entry->strs_size;
      }
}
#endif

#ifdef MACH_O
/* Read a Mach-O file's header.  DESC is the descriptor on which the
   file is open, and ENTRY is the file's entry.  */
void
read_mach_o_header (desc, entry)
     int desc;
     struct file_entry *entry;
{
  struct mach_header mach_header;
  char *hdrbuf;
  struct load_command *load_command;
  struct segment_command *segment_command;
  struct section *section;
  struct symtab_command *symtab_command;
#ifdef LC_SYMSEG
  struct symseg_command *symseg_command;
#endif;
  int ordinal;
  int symtab_seen, symseg_seen;
  int len, cmd, seg;

  entry->text_ordinal = entry->data_ordinal = entry->bss_ordinal = 0;
  symtab_seen = symseg_seen = 0;
  ordinal = 1;

  lseek (desc, entry->starting_offset, 0);
  len = read (desc, (char *) &mach_header, sizeof (struct mach_header));
  if (len != sizeof (struct mach_header))
    fatal_with_file ("failure reading Mach-O header of ", entry);
  if (mach_header.filetype != MH_OBJECT && mach_header.filetype != MH_EXECUTE)
    fatal_with_file ("unsupported Mach-O file type (not MH_OBJECT or MH_EXECUTE) in ", entry);
  hdrbuf = xmalloc (mach_header.sizeofcmds);
  len = read (desc, hdrbuf, mach_header.sizeofcmds);
  if (len != mach_header.sizeofcmds)
    fatal_with_file ("failure reading Mach-O load commands of ", entry);
  load_command = (struct load_command *) hdrbuf;
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
		if (entry->text_ordinal)
		  fatal_with_file ("more than one __text section in ", entry);
		else
		  {
		    entry->text_ordinal = ordinal;
		    entry->orig_text_address = section->addr;
		    entry->text_size = section->size;
		    entry->text_offset = section->offset;
		    entry->text_reloc_size = section->nreloc * sizeof (struct relocation_info);
		    entry->text_reloc_offset = section->reloff;
		  }
	      else if (!strncmp(SECT_DATA, section->sectname, sizeof section->sectname))
		if (entry->data_ordinal)
		  fatal_with_file ("more than one __data section in ", entry);
		else
		  {
		    entry->data_ordinal = ordinal;
		    entry->orig_data_address = section->addr;
		    entry->data_size = section->size;
		    entry->data_offset = section->offset;
		    entry->data_reloc_size = section->nreloc * sizeof (struct relocation_info);
		    entry->data_reloc_offset = section->reloff;
		  }
	      else if (!strncmp(SECT_BSS, section->sectname, sizeof section->sectname))
		if (entry->bss_ordinal)
		  fatal_with_file ("more than one __bss section in ", entry);
		else
		  {
		    entry->bss_ordinal = ordinal;
		    entry->orig_bss_address = section->addr;
		    entry->bss_size = section->size;
		  }
	      else
		if (section->size != 0)
		  fprintf (stderr, "%s: warning: unknown section `%.*s' in %s\n",
			   progname, sizeof section->sectname, section->sectname,
			   entry->filename);
	    }
	  break;
	case LC_SYMTAB:
	  if (symtab_seen)
	      fatal_with_file ("more than one LC_SYMTAB in ", entry);
	  else
	    {
	      symtab_seen = 1;
	      symtab_command = (struct symtab_command *) load_command;
	      entry->syms_size = symtab_command->nsyms * sizeof (struct nlist);
	      entry->syms_offset = symtab_command->symoff;
	      entry->strs_size = symtab_command->strsize;
	      entry->strs_offset = symtab_command->stroff;
	    }
	  break;
#ifdef LC_SYMSEG
	case LC_SYMSEG:
	  if (symseg_seen)
	    fatal_with_file ("more than one LC_SYMSEG in ", entry);
	  else
	    {
	      symseg_seen = 1;
	      symseg_command = (struct symseg_command *) load_command;
	      entry->symseg_size = symseg_command->size;
	      entry->symseg_offset = symseg_command->offset;
	    }
	  break;
#endif
	}
      load_command = (struct load_command *)
	((char *) load_command + load_command->cmdsize);
    }

  free (hdrbuf);

  if (!symtab_seen)
    fprintf (stderr, "%s: no symbol table in %s\n", progname, entry->filename);
}
#endif

/* Read a file's header info into the proper place in the file_entry.
   DESC is the descriptor on which the file is open.
   ENTRY is the file's entry.
   Switch in the file_type to determine the appropriate actual
   header reading routine to call.  */

void
read_header (desc, entry)
     int desc;
     register struct file_entry *entry;
{
  if (!entry->file_type)
    deduce_file_type (desc, entry);

  switch (entry->file_type)
    {
    case IS_ARCHIVE:
    default:
      /* Should never happen. */
      abort ();

#ifdef A_OUT
    case IS_A_OUT:
      read_a_out_header (desc, entry);
      break;
#endif

#ifdef MACH_O
    case IS_MACH_O:
      read_mach_o_header (desc, entry);
      break;
#endif
    }

  entry->header_read_flag = 1;
}

#ifdef MACH_O
void translate_mach_o_symbols ();
#endif

/* Read the symbols of file ENTRY into core.
   Assume it is already open, on descriptor DESC.  */

void
read_entry_symbols (desc, entry)
     struct file_entry *entry;
     int desc;
{
  int str_size;

  if (!entry->header_read_flag)
    read_header (desc, entry);

  entry->symbols = (struct nlist *) xmalloc (entry->syms_size);

  lseek (desc, entry->syms_offset + entry->starting_offset, 0);
  if (entry->syms_size != read (desc, entry->symbols, entry->syms_size))
    fatal_with_file ("premature end of file in symbols of ", entry);

#ifdef MACH_O
  if (entry->file_type == IS_MACH_O)
    translate_mach_o_symbols (entry);
#endif
}

/* Read the string table of file ENTRY into core.
   Assume it is already open, on descriptor DESC.  */

void
read_entry_strings (desc, entry)
     struct file_entry *entry;
     int desc;
{
  int buffer;

  if (!entry->header_read_flag)
    read_header (desc, entry);

  lseek (desc, entry->strs_offset + entry->starting_offset, 0);
  if (entry->strs_size != read (desc, entry->strings, entry->strs_size))
    fatal_with_file ("premature end of file in strings of ", entry);
}

/* Read in the symbols of all input files.  */

void read_file_symbols (), read_entry_symbols (), read_entry_strings ();
void enter_file_symbols (), enter_global_ref (), search_library ();

void
load_symbols ()
{
  register int i;

  if (trace_files) fprintf (stderr, "Loading symbols:\n\n");

  for (i = 0; i < number_of_files; i++)
    {
      register struct file_entry *entry = &file_table[i];
      read_file_symbols (entry);
    }

  if (trace_files) fprintf (stderr, "\n");
}

/* If ENTRY is a rel file, read its symbol and string sections into core.
   If it is a library, search it and load the appropriate members
   (which means calling this function recursively on those members).  */

void
read_file_symbols (entry)
     register struct file_entry *entry;
{
  register int desc;

  desc = file_open (entry);

  if (!entry->file_type)
    deduce_file_type (desc, entry);

  if (entry->file_type == IS_ARCHIVE)
    {
      entry->library_flag = 1;
      search_library (desc, entry);
    }
  else
    {
      read_entry_symbols (desc, entry);
      entry->strings = (char *) alloca (entry->strs_size);
      read_entry_strings (desc, entry);
      enter_file_symbols (entry);
      entry->strings = 0;
    }

  file_close ();
}

/* Enter the external symbol defs and refs of ENTRY in the hash table.  */

void
enter_file_symbols (entry)
     struct file_entry *entry;
{
  register struct nlist
    *p,
    *end = entry->symbols + entry->syms_size / sizeof (struct nlist);

  if (trace_files) prline_file_name (entry, stderr);

  for (p = entry->symbols; p < end; p++)
    {
      if (p->n_type == (N_SETV | N_EXT)) continue;
      if (set_element_prefixes
	  && set_element_prefixed_p (p->n_un.n_strx + entry->strings))
	p->n_type += (N_SETA - N_ABS);

      if (SET_ELEMENT_P (p->n_type))
	{
	  set_symbol_count++;
	  if (output_style != OUTPUT_RELOCATABLE)
	    enter_global_ref (p, p->n_un.n_strx + entry->strings, entry);
	}
      else if (p->n_type == N_WARNING)
	{
	  char *name = p->n_un.n_strx + entry->strings;

	  /* Grab the next entry.  */
	  p++;
	  if (p->n_type != (N_UNDF | N_EXT))
	    {
	      fprintf (stderr, "%s: Warning symbol found in %s without external reference following.\n",
		       progname, entry->filename);
	      make_executable = 0;
	      p--;		/* Process normally.  */
	    }
	  else
	    {
	      symbol *sp;
	      char *sname = p->n_un.n_strx + entry->strings;
	      /* Deal with the warning symbol.  */
	      enter_global_ref (p, p->n_un.n_strx + entry->strings, entry);
	      sp = getsym (sname);
	      sp->warning = (char *) xmalloc (strlen(name) + 1);
	      strcpy (sp->warning, name);
	      warning_count++;
	    }
	}
      else if (p->n_type & N_EXT)
	enter_global_ref (p, p->n_un.n_strx + entry->strings, entry);
      else if (p->n_un.n_strx && !(p->n_type & (N_STAB | N_EXT)))
	{
	  if ((p->n_un.n_strx + entry->strings)[0] != LPREFIX)
	    non_L_local_sym_count++;
	  local_sym_count++;
	}
      else debugger_sym_count++;
    }

   /* Count one for the local symbol that we generate,
      whose name is the file's name (usually) and whose address
      is the start of the file's text.  */

  local_sym_count++;
  non_L_local_sym_count++;
}

/* Enter one global symbol in the hash table.
   NLIST_P points to the `struct nlist' read from the file
   that describes the global symbol.  NAME is the symbol's name.
   ENTRY is the file entry for the file the symbol comes from.

   The `struct nlist' is modified by placing it on a chain of
   all such structs that refer to the same global symbol.
   This chain starts in the `refs' field of the symbol table entry
   and is chained through the `n_name'.  */

void
enter_global_ref (nlist_p, name, entry)
     register struct nlist *nlist_p;
     char *name;
     struct file_entry *entry;
{
  register symbol *sp = getsym (name);
  register int type = nlist_p->n_type;
  int oldref = sp->referenced;
  int olddef = sp->defined;

  nlist_p->n_un.n_name = (char *) sp->refs;
  sp->refs = nlist_p;

  sp->referenced = 1;
  sp->last_library_ref = entry->superfile;

  if (type != (N_UNDF | N_EXT) || nlist_p->n_value)
    {
      if (!sp->defined || sp->defined == (N_UNDF | N_EXT))
	sp->defined = type;

      if (oldref && !olddef)
	/* It used to be undefined and we're defining it.  */
	undefined_global_sym_count--;

      if (!olddef && type == (N_UNDF | N_EXT) && nlist_p->n_value)
	{
	  /* First definition and it's common.  */
	  common_defined_global_count++;
	  sp->max_common_size = nlist_p->n_value;
	}
      else if (olddef && sp->max_common_size && type != (N_UNDF | N_EXT))
	{
	  /* It used to be common and we're defining it as
	     something else.  */
	  common_defined_global_count--;
	  sp->max_common_size = 0;
	}
      else if (olddef && sp->max_common_size && type == (N_UNDF | N_EXT)
	  && sp->max_common_size < nlist_p->n_value)
	/* It used to be common and this is a new common entry to
	   which we need to pay attention.  */
	sp->max_common_size = nlist_p->n_value;

      /* Are we defining it as a set element?  */
      if (SET_ELEMENT_P (type)
	  && (!olddef || (olddef && sp->max_common_size)))
	set_vector_count++;
      /* As an indirection?  */
      else if (type == (N_INDR | N_EXT))
	{
	  /* Indirect symbols value should be modified to point
	     a symbol being equivalenced to. */
	  nlist_p->n_value
#ifndef NeXT
	    = (unsigned int) getsym ((nlist_p + 1)->n_un.n_strx
				     + entry->strings);
#else
	    /* NeXT also has indirection but they do it weirdly. */
	    = (unsigned int) getsym (nlist_p->n_value + entry->strings);
#endif
	  if ((symbol *) nlist_p->n_value == sp)
	    {
	      /* Somebody redefined a symbol to be itself.  */
	      fprintf (stderr, "%s: Symbol %s indirected to itself.\n",
		       entry->filename, name);
	      /* Rewrite this symbol as being a global text symbol
		 with value 0.  */
	      nlist_p->n_type = sp->defined = N_TEXT | N_EXT;
	      nlist_p->n_value = 0;
	      /* Don't make the output executable.  */
	      make_executable = 0;
	    }
	  else
	    global_indirect_count++;
	}
    }
  else
    if (!oldref)
#ifndef DOLLAR_KLUDGE
      undefined_global_sym_count++;
#else
      {
	if (entry->superfile && type == (N_UNDF | N_EXT) && name[1] == '$')
	  {
	    /* This is an (ISI?) $-conditional; skip it */
	    sp->referenced = 0;
	    if (sp->trace)
	      {
		fprintf (stderr, "symbol %s is a $-conditional ignored in ", sp->name);
		print_file_name (entry, stderr);
		fprintf (stderr, "\n");
	      }
	    return;
	  }
	else
	  undefined_global_sym_count++;
      }
#endif

  if (sp == end_symbol && entry->just_syms_flag && !T_flag_specified)
    text_start = nlist_p->n_value;

  if (sp->trace)
    {
      register char *reftype;
      switch (type & ~N_EXT)
	{
	case N_UNDF:
	  if (nlist_p->n_value)
	    reftype = "defined as common";
	  else reftype = "referenced";
	  break;

	case N_ABS:
	  reftype = "defined as absolute";
	  break;

	case N_TEXT:
	  reftype = "defined in text section";
	  break;

	case N_DATA:
	  reftype = "defined in data section";
	  break;

	case N_BSS:
	  reftype = "defined in BSS section";
	  break;

	case N_SETT:
	  reftype = "is a text set element";
	  break;

	case N_SETD:
	  reftype = "is a data set element";
	  break;

	case N_SETB:
	  reftype = "is a BSS set element";
	  break;

	case N_SETA:
	  reftype = "is an absolute set element";
	  break;

	case N_SETV:
	  reftype = "defined in data section as vector";
	  break;

	case N_INDR:
	  reftype = (char *) alloca (23 + strlen (((symbol *) nlist_p->n_value)->name));
	  sprintf (reftype, "defined equivalent to %s",
		   ((symbol *) nlist_p->n_value)->name);
	  break;

#ifdef sequent
	case N_SHUNDF:
	  reftype = "shared undf";
	  break;

/* These conflict with cases above.
	case N_SHDATA:
	  reftype = "shared data";
	  break;

	case N_SHBSS:
	  reftype = "shared BSS";
	  break;
*/
#endif

	default:
	  reftype = "I don't know this type";
	  break;
	}

      fprintf (stderr, "symbol %s %s in ", sp->name, reftype);
      print_file_name (entry, stderr);
      fprintf (stderr, "\n");
    }
}

/* This return 0 if the given file entry's symbol table does *not*
   contain the nlist point entry, and it returns the files entry
   pointer (cast to unsigned long) if it does. */

unsigned long
contains_symbol (entry, n_ptr)
     struct file_entry *entry;
     register struct nlist *n_ptr;
{
  if (n_ptr >= entry->symbols &&
      n_ptr < (entry->symbols
	       + (entry->syms_size / sizeof (struct nlist))))
    return (unsigned long) entry;
  return 0;
}


/* Searching libraries */

struct file_entry *decode_library_subfile ();
void linear_library (), symdef_library ();

/* Search the library ENTRY, already open on descriptor DESC.
   This means deciding which library members to load,
   making a chain of `struct file_entry' for those members,
   and entering their global symbols in the hash table.  */

void
search_library (desc, entry)
     int desc;
     struct file_entry *entry;
{
  int member_length;
  register char *name;
  register struct file_entry *subentry;

  if (!undefined_global_sym_count) return;

  /* Examine its first member, which starts SARMAG bytes in.  */
  subentry = decode_library_subfile (desc, entry, SARMAG, &member_length);
  if (!subentry) return;

  name = subentry->filename;
  free (subentry);

  /* Search via __.SYMDEF if that exists, else linearly.  */

  if (!strcmp (name, "__.SYMDEF"))
    symdef_library (desc, entry, member_length);
  else
    linear_library (desc, entry);
}

/* Construct and return a file_entry for a library member.
   The library's file_entry is library_entry, and the library is open on DESC.
   SUBFILE_OFFSET is the byte index in the library of this member's header.
   We store the length of the member into *LENGTH_LOC.  */

struct file_entry *
decode_library_subfile (desc, library_entry, subfile_offset, length_loc)
     int desc;
     struct file_entry *library_entry;
     int subfile_offset;
     int *length_loc;
{
  int bytes_read;
  register int namelen;
  int member_length;
  register char *name;
  struct ar_hdr hdr1;
  register struct file_entry *subentry;

  lseek (desc, subfile_offset, 0);

#ifdef LARMAG
  bytes_read = getarhdr (desc, &hdr1);
#else
  bytes_read = read (desc, &hdr1, sizeof hdr1);
#endif

  if (!bytes_read)
    return 0;		/* end of archive */

#ifdef LARMAG
  if (bytes_read < 0)
#else
  if (sizeof hdr1 != bytes_read)
#endif
    fatal_with_file ("malformed library archive ", library_entry);

  if (sscanf (hdr1.ar_size, "%d", &member_length) != 1)
    fatal_with_file ("malformatted header of archive member in ", library_entry);

  subentry = (struct file_entry *) xmalloc (sizeof (struct file_entry));
  bzero (subentry, sizeof (struct file_entry));

#ifdef LARMAG
  namelen = strlen (hdr1.ar_name);
#else
  for (namelen = 0;
       namelen < sizeof hdr1.ar_name
       && hdr1.ar_name[namelen] != 0 && hdr1.ar_name[namelen] != ' '
       && hdr1.ar_name[namelen] != '/';
       namelen++);
#endif

  name = (char *) xmalloc (namelen+1);
  strncpy (name, hdr1.ar_name, namelen);
  name[namelen] = 0;

  subentry->filename = name;
  subentry->local_sym_name = name;
  subentry->symbols = 0;
  subentry->strings = 0;
  subentry->subfiles = 0;
#ifdef LARMAG
  /* The struct ar_hdr is packed in the file */
  /* The return value of getarhdr accounts for just part of it */
  bytes_read += sizeof hdr1 - sizeof hdr1.ar_name;
  subentry->starting_offset = subfile_offset + bytes_read;
#else
  subentry->starting_offset = subfile_offset + sizeof hdr1;
#endif
  subentry->superfile = library_entry;
  subentry->library_flag = 0;
  subentry->header_read_flag = 0;
  subentry->just_syms_flag = 0;
  subentry->chain = 0;
  subentry->total_size = member_length;

  (*length_loc) = member_length;

  return subentry;
}

int subfile_wanted_p ();

/* Search a library that has a __.SYMDEF member.
   DESC is a descriptor on which the library is open.
     The file pointer is assumed to point at the __.SYMDEF data.
   ENTRY is the library's file_entry.
   MEMBER_LENGTH is the length of the __.SYMDEF data.  */

void
symdef_library (desc, entry, member_length)
     int desc;
     struct file_entry *entry;
     int member_length;
{
  int *symdef_data = (int *) xmalloc (member_length);
  register struct symdef *symdef_base;
  char *sym_name_base;
  int number_of_symdefs;
  int length_of_strings;
  int not_finished;
  int bytes_read;
  register int i;
  struct file_entry *prev = 0;
  int prev_offset = 0;

  bytes_read = read (desc, symdef_data, member_length);
  if (bytes_read != member_length)
    fatal_with_file ("malformatted __.SYMDEF in ", entry);

  number_of_symdefs = *symdef_data / sizeof (struct symdef);
  if (number_of_symdefs < 0 ||
       number_of_symdefs * sizeof (struct symdef) + 2 * sizeof (int) > member_length)
    fatal_with_file ("malformatted __.SYMDEF in ", entry);

  symdef_base = (struct symdef *) (symdef_data + 1);
  length_of_strings = *(int *) (symdef_base + number_of_symdefs);

  if (length_of_strings < 0
      || number_of_symdefs * sizeof (struct symdef) + length_of_strings
	  + 2 * sizeof (int) != member_length)
    fatal_with_file ("malformatted __.SYMDEF in ", entry);

  sym_name_base = sizeof (int) + (char *) (symdef_base + number_of_symdefs);

  /* Check all the string indexes for validity.  */

  for (i = 0; i < number_of_symdefs; i++)
    {
      register int index = symdef_base[i].symbol_name_string_index;
      if (index < 0 || index >= length_of_strings
	  || (index && *(sym_name_base + index - 1)))
	fatal_with_file ("malformatted __.SYMDEF in ", entry);
    }

  /* Search the symdef data for members to load.
     Do this until one whole pass finds nothing to load.  */

  not_finished = 1;
  while (not_finished)
    {
      not_finished = 0;

      /* Scan all the symbols mentioned in the symdef for ones that we need.
	 Load the library members that contain such symbols.  */

      for (i = 0;
	   (i < number_of_symdefs
	    && (undefined_global_sym_count || common_defined_global_count));
	   i++)
	if (symdef_base[i].symbol_name_string_index >= 0)
	  {
	    register symbol *sp;

	    sp = getsym_soft (sym_name_base
			      + symdef_base[i].symbol_name_string_index);

	    /* If we find a symbol that appears to be needed, think carefully
	       about the archive member that the symbol is in.  */

	    if (sp && ((sp->referenced && !sp->defined)
		       || (sp->defined && sp->max_common_size)))
	      {
		int junk;
		register int j;
		register int offset = symdef_base[i].library_member_offset;
		struct file_entry *subentry;

		/* Don't think carefully about any archive member
		   more than once in a given pass.  */

		if (prev_offset == offset)
		  continue;
		prev_offset = offset;

		/* Read the symbol table of the archive member.  */

		subentry = decode_library_subfile (desc, entry, offset, &junk);
		if (subentry == 0)
		  fatal ("invalid offset for %s in symbol table of %s",
			 sym_name_base
			 + symdef_base[i].symbol_name_string_index,
			 entry->filename);
		read_entry_symbols (desc, subentry);
		subentry->strings = xmalloc (subentry->strs_size);
		read_entry_strings (desc, subentry);

		/* Now scan the symbol table and decide whether to load.  */

		if (!subfile_wanted_p (subentry))
		  {
		    free (subentry->symbols);
		    free (subentry->strings);
		    free (subentry);
		  }
		else
		  {
		    /* This member is needed; load it.
		       Since we are loading something on this pass,
		       we must make another pass through the symdef data.  */

		    not_finished = 1;

		    enter_file_symbols (subentry);

		    if (prev)
		      prev->chain = subentry;
		    else entry->subfiles = subentry;
		    prev = subentry;

		    /* Clear out this member's symbols from the symdef data
		       so that following passes won't waste time on them.  */

		    for (j = 0; j < number_of_symdefs; j++)
		      {
			if (symdef_base[j].library_member_offset == offset)
			  symdef_base[j].symbol_name_string_index = -1;
		      }

		    /* We'll read the strings again if we need them again.  */
		    free (subentry->strings);
		    subentry->strings = 0;
		  }
	      }
	  }
    }

  free (symdef_data);
}


/* Handle a subentry for a file with no __.SYMDEF. */

process_subentry (desc, subentry, entry, prev_addr)
     int desc;
     register struct file_entry *subentry;
     struct file_entry **prev_addr, *entry;
{
  read_entry_symbols (desc, subentry);
  subentry->strings = (char *) alloca (subentry->strs_size);
  read_entry_strings (desc, subentry);

  if (!subfile_wanted_p (subentry))
    {
      free (subentry->symbols);
      free (subentry);
    }
  else
    {
      enter_file_symbols (subentry);

      if (*prev_addr)
	(*prev_addr)->chain = subentry;
      else
	entry->subfiles = subentry;
      *prev_addr = subentry;
      subentry->strings = 0; /* Since space will dissapear on return */
    }
}

/* Search a library that has no __.SYMDEF.
   ENTRY is the library's file_entry.
   DESC is the descriptor it is open on.  */

void
linear_library (desc, entry)
     int desc;
     struct file_entry *entry;
{
  struct file_entry *prev = 0;
  register int this_subfile_offset = SARMAG;

  while (undefined_global_sym_count || common_defined_global_count)
    {
      int member_length;
      register struct file_entry *subentry;

      subentry = decode_library_subfile (desc, entry, this_subfile_offset,
					 &member_length);
      if (!subentry) return;

      process_subentry (desc, subentry, entry, &prev);
#ifdef LARMAG
      this_subfile_offset = member_length + subentry->starting_offset;
#else
      this_subfile_offset += member_length + sizeof (struct ar_hdr);
#endif
      if (this_subfile_offset & 1) this_subfile_offset++;
    }
}

/* ENTRY is an entry for a library member.
   Its symbols have been read into core, but not entered.
   Return nonzero if we ought to load this member.  */

int
subfile_wanted_p (entry)
     struct file_entry *entry;
{
  register struct nlist *p;
  register struct nlist *end
    = entry->symbols + entry->syms_size / sizeof (struct nlist);
#ifdef DOLLAR_KLUDGE
  register int dollar_cond = 0;
#endif

  for (p = entry->symbols; p < end; p++)
    {
      register int type = p->n_type;
      register char *name = p->n_un.n_strx + entry->strings;

      /* If the symbol has an interesting definition, we could
	 potentially want it.  */
      if (type & N_EXT
	  && (type != (N_UNDF | N_EXT) || p->n_value
#ifdef DOLLAR_KLUDGE
	       || name[1] == '$'
#endif
	      )
	  && !SET_ELEMENT_P (type)
	  && !set_element_prefixed_p (name))
	{
	  register symbol *sp = getsym_soft (name);

#ifdef DOLLAR_KLUDGE
	  if (name[1] == '$')
	    {
	      sp = getsym_soft (&name[2]);
	      dollar_cond = 1;
	      if (!sp) continue;
	      if (sp->referenced)
		{
		  if (write_map)
		    {
		      print_file_name (entry, stdout);
		      fprintf (stdout, " needed due to $-conditional %s\n", name);
		    }
		  return 1;
		}
	      continue;
	    }
#endif

	  /* If this symbol has not been hashed, we can't be looking for it. */

	  if (!sp) continue;

	  /* Note: There has been a lot of discussion about what to
	     when a common definition was previously seen (i.e. when
	     sp->max_common_size > 0).
	     The latest solution is to treat a previous common definition
	     (wrt to subfile_wanted_p) no differently from a real definition.
	     This has the advantage of simplicity and consistency: a common
	     definition is just like a common definition (consistent
	     with strict ANSI C) except that we allow duplicate definitions.
	     Possible disadvantage: May not be the best choice for Fortran,
	     though it is consistent with the standard.

	     An earlier solution:
	     We wanted to see a common definition in the subfile,
	     and note its size, but ignore any other definition
	     if the symbol was already defined (even as a common).
	     This meant that if there were multiple common definitions,
	     the final definition would use the largest size of any of them,
	     as it should.  But if there was a common definition and another
	     definition, like "int pipe;" in a program and "int pipe() {}"
	     in the library, only the common would be used.
	     Disadvantage: a poorly justified kludge.

	     Another previous solution:
	     If the symbol already had a definition as a common symbol,
	     we would want this subfile if some other subfile of the
	     same library that we already need anyway also used the symbol.
	     This seemed like an even more ad hoc decision.
	     It would also cause subfiles to be pulled in that would
	     then conflict with previous entries. I.e. you couldn't
	     have: ld ... start.o libc.a ... if libc.a contained start.o.

	     Other hybrid solutions were also considered.
	  */
	  if ((sp->referenced && !sp->defined))
	    {
#ifdef DOLLAR_KLUDGE
	      if (dollar_cond) continue;
#endif
	      if (type == (N_UNDF | N_EXT))
		{
		  /* Symbol being defined as common.
		     Remember this, but don't load subfile just for this.  */

		  common_defined_global_count++;
		  sp->max_common_size = p->n_value;
		  undefined_global_sym_count--;
		  sp->defined = 1;
		  continue;
		}
	      if (write_map)
		{
		  print_file_name (entry, stdout);
		  fprintf (stdout, " needed due to %s\n", sp->name);
		}
	      return 1;
	    }
	}
    }

  return 0;
}

void consider_file_section_lengths (), relocate_file_addresses ();

/* Having entered all the global symbols and found the sizes of sections
   of all files to be linked, make all appropriate deductions from this data.

   We propagate global symbol values from definitions to references.
   We compute the layout of the output file and where each input file's
   contents fit into it.  */

void
digest_symbols ()
{
  register int i;
  int setv_fill_count;

  if (trace_files)
    fprintf (stderr, "Digesting symbol information:\n\n");

  /* Initialize the text_start address; this depends on the output file formats.  */

  initialize_text_start ();

  text_size = text_header_size;

  /* Compute total size of sections */

  each_file (consider_file_section_lengths, 0);

  /* If necessary, pad text section to full page in the file.
     Include the padding in the text segment size.  */

  if (output_style == OUTPUT_READONLY_TEXT || output_style == OUTPUT_DEMAND_PAGED)
    {
#ifdef tek4300
      int page_size = ZMALIGN; /* a lie */
#endif
#ifdef is68k
      text_pad = ((text_size + SEGMENT_SIZE - 1) & (- SEGMENT_SIZE) - text_size;
#else
      text_pad = ((text_size + page_size - 1) & (- page_size)) - text_size;
#endif
      text_size += text_pad;
    }

  /* Now that the text_size is known, initialize the data start address;
     this depends on text_size as well as the output file format.  */

  initialize_data_start ();

  /* Set up the set element vector */

  if (output_style != OUTPUT_RELOCATABLE)
    {
      /* The set sector size is the number of set elements + a word
         for each symbol for the length word at the beginning of the
	 vector, plus a word for each symbol for a zero at the end of
	 the vector (for incremental linking).  */
      set_sect_size
	= (2 * set_symbol_count + set_vector_count) * sizeof (unsigned long);
      set_sect_start = data_start + data_size;
      data_size += set_sect_size;
      set_vectors = (unsigned long *) xmalloc (set_sect_size);
      setv_fill_count = 0;
    }

  /* Make sure bss starts out aligned as much as anyone can want.  */
  {
    int new_data_size = (data_size + sizeof(double) - 1) & ~(sizeof(double)-1);

    data_pad += new_data_size - data_size;
    data_size = new_data_size;
  }

  /* Compute start addresses of each file's sections and symbols.  */

  each_full_file (relocate_file_addresses, 0);

  /* Now, for each symbol, verify that it is defined globally at most once.
     Put the global value into the symbol entry.
     Common symbols are allocated here, in the BSS section.
     Each defined symbol is given a '->defined' field
      which is the correct N_ code for its definition,
      except in the case of common symbols with -r.
     Then make all the references point at the symbol entry
     instead of being chained together. */

  defined_global_sym_count = 0;

  for (i = 0; i < TABSIZE; i++)
    {
      register symbol *sp;
      for (sp = symtab[i]; sp; sp = sp->link)
	{
	  /* For each symbol */
	  register struct nlist *p, *next;
	  int defs = 0, com = sp->max_common_size;
	  struct nlist *first_definition;
	  for (p = sp->refs; p; p = next)
	    {
	      register int type = p->n_type;

	      if (SET_ELEMENT_P (type))
		{
		  if (output_style == OUTPUT_RELOCATABLE)
		    fatal ("internal: global ref to set element with -r");
		  if (!defs++)
		    {
		      sp->value = set_sect_start
			+ setv_fill_count++ * sizeof (unsigned long);
		      sp->defined = N_SETV | N_EXT;
		      first_definition = p;
		    }
		  else if ((sp->defined & ~N_EXT) != N_SETV)
		    {
		      sp->multiply_defined = 1;
		      multiple_def_count++;
		    }
		  set_vectors[setv_fill_count++] = p->n_value;
		}
	      else if ((type & N_EXT) && type != (N_UNDF | N_EXT))
		{
		  /* non-common definition */
		  if (defs++ && sp->value != p->n_value)
		    {
		      sp->multiply_defined = 1;
		      multiple_def_count++;
		    }
		  sp->value = p->n_value;
		  sp->defined = type;
		  first_definition = p;
		}
	      next = (struct nlist *) p->n_un.n_name;
	      p->n_un.n_name = (char *) sp;
	    }
	  /* Allocate as common if defined as common and not defined for real */
	  if (com && !defs)
	    {
	      if (output_style != OUTPUT_RELOCATABLE || force_common_definition)
		{
		  int align = sizeof (int);

		  /* Round up to nearest sizeof (int).  I don't know
		     whether this is necessary or not (given that
		     alignment is taken care of later), but it's
		     traditional, so I'll leave it in.  Note that if
		     this size alignment is ever removed, ALIGN above
		     will have to be initialized to 1 instead of
		     sizeof (int).  */

		  com = (com + sizeof (int) - 1) & (- sizeof (int));

		  while (!(com & align))
		    align <<= 1;

		  align = align > MAX_ALIGNMENT ? MAX_ALIGNMENT : align;

		  bss_size = ((((bss_size + data_size + data_start)
			      + (align - 1)) & (- align))
			      - data_size - data_start);

		  sp->value = data_start + data_size + bss_size;
		  sp->defined = N_BSS | N_EXT;
		  bss_size += com;
		  if (write_map)
		    printf ("Allocating common %s: %x at %x\n",
			    sp->name, com, sp->value);
		}
	      else
		{
		  sp->defined = 0;
		  undefined_global_sym_count++;
		}
	    }
	  /* Set length word at front of vector and zero byte at end.
	     Reverse the vector itself to put it in file order.  */
	  if ((sp->defined & ~N_EXT) == N_SETV)
	    {
	      unsigned long length_word_index
		= (sp->value - set_sect_start) / sizeof (unsigned long);
	      unsigned long i, tmp;

	      set_vectors[length_word_index]
		= setv_fill_count - 1 - length_word_index;

	      /* Reverse the vector.  */
	      for (i = 1;
		   i < (setv_fill_count - length_word_index - 1) / 2 + 1;
		   i++)
		{
		  tmp = set_vectors[length_word_index + i];
		  set_vectors[length_word_index + i]
		    = set_vectors[setv_fill_count - i];
		  set_vectors[setv_fill_count - i] = tmp;
		}

	      set_vectors[setv_fill_count++] = 0;
	    }
	  if (sp->defined)
	    defined_global_sym_count++;
	}
    }

  /* Make sure end of bss is aligned as much as anyone can want.  */

  bss_size = (bss_size + sizeof(double) - 1) & ~(sizeof(double)-1);

  /* Give values to _end and friends.  */
  {
    int end_value = data_start + data_size + bss_size;
    if (end_symbol)
      end_symbol->value = end_value;
    if (end_symbol_alt)
      end_symbol_alt->value = end_value;
  }

  {
    int etext_value = text_size + text_start;
    if (etext_symbol)
      etext_symbol->value = etext_value;
    if (etext_symbol_alt)
      etext_symbol_alt->value = etext_value;
  }

  {
    int edata_value = data_start + data_size;
    if (edata_symbol)
      edata_symbol->value = edata_value;
    if (edata_symbol_alt)
      edata_symbol_alt->value = edata_value;
  }

  /* Figure the data_pad now, so that it overlaps with the bss addresses.  */

  {
    /* The amount of data_pad that we are computing now.  This is the
       part which overlaps with bss.  What was computed previously
       goes before bss.  */
    int data_pad_additional = 0;
    
    if (specified_data_size && specified_data_size > data_size)
      data_pad_additional = specified_data_size - data_size;

    if (output_style == OUTPUT_DEMAND_PAGED)
      data_pad_additional =
	((data_pad_additional + data_size + page_size - 1) & (- page_size)) - data_size;

    bss_size -= data_pad_additional;
    if (bss_size < 0) bss_size = 0;

    data_size += data_pad_additional;

    data_pad += data_pad_additional;
  }
}

/* Accumulate the section sizes of input file ENTRY
   into the section sizes of the output file.  */

void
consider_file_section_lengths (entry)
     register struct file_entry *entry;
{
  if (entry->just_syms_flag)
    return;

  entry->text_start_address = text_size;
  /* If there were any vectors, we need to chop them off */
  text_size += entry->text_size;
  entry->data_start_address = data_size;
  data_size += entry->data_size;
  entry->bss_start_address = bss_size;
  bss_size += entry->bss_size;

  text_reloc_size += entry->text_reloc_size;
  data_reloc_size += entry->data_reloc_size;
}

/* Determine where the sections of ENTRY go into the output file,
   whose total section sizes are already known.
   Also relocate the addresses of the file's local and debugger symbols.  */

void
relocate_file_addresses (entry)
     register struct file_entry *entry;
{
  entry->text_start_address += text_start;

  /* Note that `data_start' and `data_size' have not yet been adjusted
     for the portion of data_pad which overlaps with bss.  If they had
     been, we would get the wrong results here.  */
  entry->data_start_address += data_start;
  entry->bss_start_address += data_start + data_size;

  {
    register struct nlist *p;
    register struct nlist *end
      = entry->symbols + entry->syms_size / sizeof (struct nlist);

    for (p = entry->symbols; p < end; p++)
      {
	/* If this belongs to a section, update it by the section's start address */
	register int type = p->n_type & N_TYPE;

	switch (type)
	  {
	  case N_TEXT:
	  case N_SETT:
	    p->n_value += entry->text_start_address - entry->orig_text_address;
	    break;
	  case N_DATA:
	  case N_SETV:
	  case N_SETD:
	    /* Data segment symbol.  Subtract the address of the
	       data segment in the input file, and add the address
	       of this input file's data segment in the output file.  */
	    p->n_value +=
	      entry->data_start_address - entry->orig_data_address;
	    break;
	  case N_BSS:
	  case N_SETB:
	    /* likewise for symbols with value in BSS.  */
	    p->n_value += entry->bss_start_address - entry->orig_bss_address;
	    break;
	  }
      }
  }
}

void describe_file_sections (), list_file_locals ();

/* Print a complete or partial map of the output file.  */

void
print_symbols (outfile)
     FILE *outfile;
{
  register int i;

  fprintf (outfile, "\nFiles:\n\n");

  each_file (describe_file_sections, outfile);

  fprintf (outfile, "\nGlobal symbols:\n\n");

  for (i = 0; i < TABSIZE; i++)
    {
      register symbol *sp;
      for (sp = symtab[i]; sp; sp = sp->link)
	{
	  if (sp->defined == 1)
	    fprintf (outfile, "  %s: common, length 0x%x\n", sp->name, sp->max_common_size);
	  if (sp->defined)
	    fprintf (outfile, "  %s: 0x%x\n", sp->name, sp->value);
	  else if (sp->referenced)
	    fprintf (outfile, "  %s: undefined\n", sp->name);
	}
    }

  each_file (list_file_locals, outfile);
}

void
describe_file_sections (entry, outfile)
     struct file_entry *entry;
     FILE *outfile;
{
  fprintf (outfile, "  ");
  print_file_name (entry, outfile);
  if (entry->just_syms_flag)
    fprintf (outfile, " symbols only\n", 0);
  else
    fprintf (outfile, " text %x(%x), data %x(%x), bss %x(%x) hex\n",
	     entry->text_start_address, entry->text_size,
	     entry->data_start_address, entry->data_size,
	     entry->bss_start_address, entry->bss_size);
}

void
list_file_locals (entry, outfile)
     struct file_entry *entry;
     FILE *outfile;
{
  register struct nlist
    *p,
    *end = entry->symbols + entry->syms_size / sizeof (struct nlist);

  entry->strings = (char *) alloca (entry->strs_size);
  read_entry_strings (file_open (entry), entry);

  fprintf (outfile, "\nLocal symbols of ");
  print_file_name (entry, outfile);
  fprintf (outfile, ":\n\n");

  for (p = entry->symbols; p < end; p++)
    /* If this is a definition,
       update it if necessary by this file's start address.  */
    if (!(p->n_type & (N_STAB | N_EXT)))
      fprintf (outfile, "  %s: 0x%x\n",
	       entry->strings + p->n_un.n_strx, p->n_value);

  entry->strings = 0;		/* All done with them.  */
}


/* Static vars for do_warnings and subroutines of it */
int list_unresolved_refs;	/* List unresolved refs */
int list_warning_symbols;	/* List warning syms */
int list_multiple_defs;		/* List multiple definitions */

/*
 * Structure for communication between do_file_warnings and it's
 * helper routines.  Will in practice be an array of three of these:
 * 0) Current line, 1) Next line, 2) Source file info.
 */
struct line_debug_entry
{
  int line;
  char *filename;
  struct nlist *sym;
};

void qsort ();
/*
 * Helper routines for do_file_warnings.
 */

/* Return an integer less than, equal to, or greater than 0 as per the
   relation between the two relocation entries.  Used by qsort.  */

int
relocation_entries_relation (rel1, rel2)
     struct relocation_info *rel1, *rel2;
{
  return RELOC_ADDRESS(rel1) - RELOC_ADDRESS(rel2);
}

/* Moves to the next debugging symbol in the file.  USE_DATA_SYMBOLS
   determines the type of the debugging symbol to look for (DSLINE or
   SLINE).  STATE_POINTER keeps track of the old and new locatiosn in
   the file.  It assumes that state_pointer[1] is valid; ie
   that it.sym points into some entry in the symbol table.  If
   state_pointer[1].sym == 0, this routine should not be called.  */

int
next_debug_entry (use_data_symbols, state_pointer)
     register int use_data_symbols;
     /* Next must be passed by reference! */
     struct line_debug_entry state_pointer[3];
{
  register struct line_debug_entry
    *current = state_pointer,
    *next = state_pointer + 1,
    /* Used to store source file */
    *source = state_pointer + 2;
  struct file_entry *entry = (struct file_entry *) source->sym;

  current->sym = next->sym;
  current->line = next->line;
  current->filename = next->filename;

  while (++(next->sym) < (entry->symbols
			  + entry->syms_size/sizeof (struct nlist)))
    {
      /* n_type is a char, and N_SOL, N_EINCL and N_BINCL are > 0x80, so
       * may look negative...therefore, must mask to low bits
       */
      switch (next->sym->n_type & 0xff)
	{
	case N_SLINE:
	  if (use_data_symbols) continue;
	  next->line = next->sym->n_desc;
	  return 1;
	case N_DSLINE:
	  if (!use_data_symbols) continue;
	  next->line = next->sym->n_desc;
	  return 1;
#ifdef HAVE_SUN_STABS
	case N_EINCL:
	  next->filename = source->filename;
	  continue;
#endif
	case N_SO:
	  source->filename = next->sym->n_un.n_strx + entry->strings;
	  source->line++;
#ifdef HAVE_SUN_STABS
	case N_BINCL:
#endif
	case N_SOL:
	  next->filename
	    = next->sym->n_un.n_strx + entry->strings;
	default:
	  continue;
	}
    }
  next->sym = (struct nlist *) 0;
  return 0;
}

/* Create a structure to save the state of a scan through the debug
   symbols.  USE_DATA_SYMBOLS is set if we should be scanning for
   DSLINE's instead of SLINE's.  entry is the file entry which points
   at the symbols to use.  */

struct line_debug_entry *
init_debug_scan (use_data_symbols, entry)
     int use_data_symbols;
     struct file_entry *entry;
{
  struct line_debug_entry
    *state_pointer
      = (struct line_debug_entry *)
	xmalloc (3 * sizeof (struct line_debug_entry));
  register struct line_debug_entry
    *current = state_pointer,
    *next = state_pointer + 1,
    *source = state_pointer + 2; /* Used to store source file */

  struct nlist *tmp;

  for (tmp = entry->symbols;
       tmp < (entry->symbols
	      + entry->syms_size/sizeof (struct nlist));
       tmp++)
    if (tmp->n_type == (int) N_SO)
      break;

  if (tmp >= (entry->symbols
	      + entry->syms_size/sizeof (struct nlist)))
    {
      /* I believe this translates to "We lose" */
      current->filename = next->filename = entry->filename;
      current->line = next->line = -1;
      current->sym = next->sym = (struct nlist *) 0;
      return state_pointer;
    }

  next->line = source->line = 0;
  next->filename = source->filename
    = (tmp->n_un.n_strx + entry->strings);
  source->sym = (struct nlist *) entry;
  next->sym = tmp;

  next_debug_entry (use_data_symbols, state_pointer); /* To setup next */

  if (!next->sym)		/* No line numbers for this section; */
				/* setup output results as appropriate */
    {
      if (source->line)
	{
	  current->filename = source->filename = entry->filename;
	  current->line = -1;	/* Don't print lineno */
	}
      else
	{
	  current->filename = source->filename;
	  current->line = 0;
	}
      return state_pointer;
    }


  next_debug_entry (use_data_symbols, state_pointer); /* To setup current */

  return state_pointer;
}

/* Takes an ADDRESS (in either text or data space) and a STATE_POINTER
   which describes the current location in the implied scan through
   the debug symbols within the file which ADDRESS is within, and
   returns the source line number which corresponds to ADDRESS.  */

int
address_to_line (address, state_pointer)
     unsigned long address;
     /* Next must be passed by reference! */
     struct line_debug_entry state_pointer[3];
{
  struct line_debug_entry
    *current = state_pointer,
    *next = state_pointer + 1;
  struct line_debug_entry *tmp_pointer;

  int use_data_symbols;

  if (next->sym)
    use_data_symbols = (next->sym->n_type & ~N_EXT) == N_DATA;
  else
    return current->line;

  /* Go back to the beginning if we've already passed it.  */
  if (current->sym->n_value > address)
    {
      tmp_pointer = init_debug_scan (use_data_symbols,
				     (struct file_entry *)
				     ((state_pointer + 2)->sym));
      state_pointer[0] = tmp_pointer[0];
      state_pointer[1] = tmp_pointer[1];
      state_pointer[2] = tmp_pointer[2];
      free (tmp_pointer);
    }

  /* If we're still in a bad way, return -1, meaning invalid line.  */
  if (current->sym->n_value > address)
    return -1;

  while (next->sym
	 && next->sym->n_value <= address
	 && next_debug_entry (use_data_symbols, state_pointer))
    ;
  return current->line;
}


/* Macros for manipulating bitvectors.  */
#define	BIT_SET_P(bv, index)	((bv)[(index) >> 3] & 1 << ((index) & 0x7))
#define	SET_BIT(bv, index)	((bv)[(index) >> 3] |= 1 << ((index) & 0x7))

/* This routine will scan through the relocation data of file ENTRY,
   printing out references to undefined symbols and references to
   symbols defined in files with N_WARNING symbols.  If DATA_SEGMENT
   is non-zero, it will scan the data relocation segment (and use
   N_DSLINE symbols to track line number); otherwise it will scan the
   text relocation segment.  Warnings will be printed on the output
   stream OUTFILE.  Eventually, every nlist symbol mapped through will
   be marked in the NLIST_BITVECTOR, so we don't repeat ourselves when
   we scan the nlists themselves.  */

do_relocation_warnings (entry, data_segment, outfile, nlist_bitvector)
     struct file_entry *entry;
     int data_segment;
     FILE *outfile;
     unsigned char *nlist_bitvector;
{
  struct relocation_info
    *reloc_start = data_segment ? entry->datarel : entry->textrel,
    *reloc;
  long syms_size = entry->syms_size / sizeof (struct nlist);
  int reloc_size
    = ((data_segment ? entry->data_reloc_size : entry->text_reloc_size)
       / sizeof (struct relocation_info));
  int start_of_segment
    = (data_segment ? entry->data_start_address : entry->text_start_address);
  struct nlist *start_of_syms = entry->symbols;
  struct line_debug_entry *state_pointer
    = init_debug_scan (data_segment != 0, entry);
  register struct line_debug_entry *current = state_pointer;
  /* Assigned to generally static values; should not be written into.  */
  char *errfmt;
  /* Assigned to alloca'd values cand copied into; should be freed
     when done.  */
  char *errmsg;
  int invalidate_line_number;

  /* We need to sort the relocation info here.  Sheesh, so much effort
     for one lousy error optimization. */

  qsort (reloc_start, reloc_size, sizeof (struct relocation_info),
	 relocation_entries_relation);

  for (reloc = reloc_start;
       reloc < (reloc_start + reloc_size);
       reloc++)
    {
      register struct nlist *s;
      register symbol *g;
      int s_index;

      /* If the relocation isn't resolved through a symbol, continue */
      if (!RELOC_EXTERN_P(reloc))
	continue;

      s_index = RELOC_SYMBOL(reloc);
      if (s_index < 0 || s_index >= syms_size)
	fatal_with_file ("bad symbol in relocation table of ", entry);
      s = &entry->symbols[s_index];

      /* Local symbols shouldn't ever be used by relocation info, so
	 the next should be safe.
	 This is, of course, wrong.  References to local BSS symbols can be
	 the targets of relocation info, and they can (must) be
	 resolved through symbols.  However, these must be defined properly,
	 (the assembler would have caught it otherwise), so we can
	 ignore these cases.  */
      if (!(s->n_type & N_EXT))
	continue;

      g = (symbol *) s->n_un.n_name;
      errmsg = 0;

      if (!g->defined && list_unresolved_refs) /* Reference */
	{
	  /* Mark as being noted by relocation warning pass.  */
	  SET_BIT (nlist_bitvector, s - start_of_syms);

	  if (g->undef_refs >= MAX_UREFS_PRINTED)    /* Listed too many */
	    continue;

	  /* Undefined symbol which we should mention */

	  if (++(g->undef_refs) == MAX_UREFS_PRINTED)
	    {
	      errfmt = "More undefined symbol %s refs follow";
	      invalidate_line_number = 1;
	    }
	  else
	    {
	      errfmt = "Undefined symbol %s referenced from %s segment";
	      invalidate_line_number = 0;
	    }
	}
      else					     /* Defined */
	{
	  /* Potential symbol warning here */
	  if (!g->warning) continue;

	  /* Mark as being noted by relocation warning pass.  */
	  SET_BIT (nlist_bitvector, s - start_of_syms);

	  errfmt = 0;
	  errmsg = g->warning;
	  invalidate_line_number = 0;
	}


      /* If errfmt == 0, errmsg has already been defined.  */
      if (errfmt != 0)
	{
	  char *nm;

	  if (!demangler || !(nm = (*demangler)(g->name)))
	    nm = g->name;
	  errmsg = xmalloc (strlen (errfmt) + strlen (nm) + 1);
	  sprintf (errmsg, errfmt, nm, data_segment ? "data" : "text");
	  if (nm != g->name)
	    free (nm);
	}

      address_to_line (RELOC_ADDRESS (reloc) + start_of_segment,
		       state_pointer);

      if (current->line >=0)
	{
	  fprintf (outfile, "%s:%d (", current->filename,
		   invalidate_line_number ? 0 : current->line);
	  print_file_name (entry, outfile);
	  fprintf (outfile, "): %s\n", errmsg);
	}
      else
	{
	  print_file_name(entry, outfile);
	  fprintf(outfile, ": %s\n", errmsg);
	}

      if (errfmt != 0)
	free (errmsg);
    }

  free (state_pointer);
}

/* Print on OUTFILE a list of all warnings generated by references
   and/or definitions in the file ENTRY.  List source file and line
   number if possible, just the .o file if not. */

void
do_file_warnings (entry, outfile)
     struct file_entry *entry;
     FILE *outfile;
{
  int number_of_syms = entry->syms_size / sizeof (struct nlist);
  unsigned char *nlist_bitvector
    = (unsigned char *) alloca ((number_of_syms >> 3) + 1);
  struct line_debug_entry *text_scan, *data_scan;
  int i;
  char *errfmt, *file_name;
  int line_number;
  int dont_allow_symbol_name;

  bzero (nlist_bitvector, (number_of_syms >> 3) + 1);

  /* Read in the files strings if they aren't available */
  if (!entry->strings)
    {
      int desc;

      entry->strings = (char *) alloca (entry->strs_size);
      desc = file_open (entry);
      read_entry_strings (desc, entry);
    }

  read_file_relocation (entry);

  /* Do text warnings based on a scan through the relocation info.  */
  do_relocation_warnings (entry, 0, outfile, nlist_bitvector);

  /* Do data warnings based on a scan through the relocation info.  */
  do_relocation_warnings (entry, 1, outfile, nlist_bitvector);

  /* Scan through all of the nlist entries in this file and pick up
     anything that the scan through the relocation stuff didn't.  */

  text_scan = init_debug_scan (0, entry);
  data_scan = init_debug_scan (1, entry);

  for (i = 0; i < number_of_syms; i++)
    {
      struct nlist *s;
      struct glosym *g;

      s = entry->symbols + i;

      if (!(s->n_type & N_EXT))
	continue;

      g = (symbol *) s->n_un.n_name;
      dont_allow_symbol_name = 0;

      if (list_multiple_defs && g->multiply_defined)
	{
	  errfmt = "Definition of symbol %s (multiply defined)";
	  switch (s->n_type)
	    {
	    case N_ABS | N_EXT:
	      line_number = -1;
	      break;
	    case N_TEXT | N_EXT:
	      line_number = address_to_line (s->n_value, text_scan);
	      file_name = text_scan[0].filename;
	      break;
	    case N_DATA | N_EXT:
	      line_number = address_to_line (s->n_value, data_scan);
	      file_name = data_scan[0].filename;
	      break;
	    case N_SETA | N_EXT:
	    case N_SETT | N_EXT:
	    case N_SETD | N_EXT:
	    case N_SETB | N_EXT:
	      if (g->multiply_defined == 2)
		continue;
	      errfmt = "First set element definition of symbol %s (multiply defined)";
	      break;
	    default:
	      continue;		/* Don't print out multiple defs
				   at references.  */
	    }
	}
      else if (BIT_SET_P (nlist_bitvector, i))
	continue;
      else if (list_unresolved_refs && !g->defined)
	{
	  if (g->undef_refs >= MAX_UREFS_PRINTED)
	    continue;

	  if (++(g->undef_refs) == MAX_UREFS_PRINTED)
	    errfmt = "More undefined \"%s\" refs follow";
	  else
	    errfmt = "Undefined symbol \"%s\" referenced";
	  line_number = -1;
	}
      else if (g->warning)
	{
	  /* There are two cases in which we don't want to
	     do this.  The first is if this is a definition instead of
	     a reference.  The second is if it's the reference used by
	     the warning stabs itself.  */
	  if (s->n_type != (N_EXT | N_UNDF)
	      || (i && (s-1)->n_type == N_WARNING))
	    continue;

	  errfmt = g->warning;
	  line_number = -1;
	  dont_allow_symbol_name = 1;
	}
      else
	continue;

      if (line_number == -1)
	{
	  print_file_name (entry, outfile);
	  fprintf (outfile, ": ");
	}
      else
	{
	  fprintf (outfile, "%s:%d (", file_name, line_number);
	  print_file_name (entry, outfile);
	  fprintf (outfile, "): ");
	}

      if (dont_allow_symbol_name)
	fprintf (outfile, "%s", errfmt);
      else
	{
	  char *nm;

	  if (!demangler || !(nm = (*demangler)(g->name)))
	    fprintf (outfile, errfmt, g->name);
	  else
	    {
	      fprintf (outfile, errfmt, nm);
	      free (nm);
	    }
	}

      fputc ('\n', outfile);
    }
  free (text_scan);
  free (data_scan);
  entry->strings = 0;		/* Since it will dissapear anyway.  */
}

do_warnings (outfile)
     FILE *outfile;
{
  list_unresolved_refs = output_style != OUTPUT_RELOCATABLE && undefined_global_sym_count;
  list_warning_symbols = warning_count;
  list_multiple_defs = multiple_def_count != 0;

  if (!(list_unresolved_refs ||
	list_warning_symbols ||
	list_multiple_defs      ))
    /* No need to run this routine */
    return;

  each_file (do_file_warnings, outfile);

  if (entry_symbol && !entry_symbol->defined) 
    fprintf(stderr, "%s: error: Entry symbol `%s' never defined.\n",
	    progname, entry_symbol->name);

  if (list_unresolved_refs || list_multiple_defs)
    make_executable = 0;
}

#ifdef A_OUT

/* Stuff pertaining to creating a.out files. */

/* The a.out header. */

#ifdef tek4300
struct zexec outheader;
#else
struct exec outheader;
#endif

#ifdef COFF_ENCAPSULATE
int need_coff_header;
struct coffheader coffheader;
#endif

/* Compute text_start and text_header_size for an a.out file.  */

void
initialize_a_out_text_start ()
{
  int magic;

  switch (output_style)
    {
    case OUTPUT_RELOCATABLE:
    case OUTPUT_WRITABLE_TEXT:
      magic = OMAGIC;
      break;
    case OUTPUT_READONLY_TEXT:
#ifdef NMAGIC
      magic = NMAGIC;
      break;
#endif
    case OUTPUT_DEMAND_PAGED:
      magic = ZMAGIC;
      break;
    default:
      fatal ("unknown output style found (bug in ld)", (char *) 0);
      break;
    }

  /* Determine whether to count the header as part of
     the text size, and initialize the text size accordingly.
     This depends on the kind of system and on the output format selected.  */
  N_SET_MAGIC (outheader, magic);
#ifdef INITIALIZE_HEADER
  INITIALIZE_HEADER;
#endif

  text_header_size = sizeof (struct exec);
#ifdef COFF_ENCAPSULATE
  /* Don't write the coff header for the output of ld -A (since
     it is not executable by the kernel anyway).  */
  if (output_style != OUTPUT_RELOCATABLE && !file_table[0].just_syms_flag)
    {
      need_coff_header = 1;
      /* set this flag now, since it will change the values of N_TXTOFF, etc */
      N_SET_FLAGS (outheader, N_FLAGS_COFF_ENCAPSULATE);
      text_header_size += sizeof (struct coffheader);
    }
#endif
  if (text_header_size <= N_TXTOFF (outheader))
    text_header_size = 0;
  else
    text_header_size -= N_TXTOFF (outheader);

#ifdef _N_BASEADDR
  /* SunOS 4.1 N_TXTADDR depends on the value of outheader.a_entry. */
  outheader.a_entry = N_PAGSIZ(outheader);
#endif

  if (!T_flag_specified && output_style != OUTPUT_RELOCATABLE)
    text_start = N_TXTADDR (outheader);
}

/* Compute data_start once text_size is known. */

void
initialize_a_out_data_start ()
{
  outheader.a_text = text_size;
#ifdef sequent
  outheader.a_text += N_ADDRADJ (outheader);
  if (entry_symbol == 0)
    entry_symbol = getsym ("start");
#endif
  if (! Tdata_flag_specified)
    data_start = N_DATADDR (outheader) + text_start - N_TXTADDR (outheader);
}

/* Compute offsets of various pieces of the a.out output file.  */

void
compute_a_out_section_offsets ()
{
  outheader.a_data = data_size;
  outheader.a_bss = bss_size;
  outheader.a_entry = (entry_symbol ? entry_symbol->value
		       : text_start + text_header_size);

#ifdef COFF_ENCAPSULATE
  if (need_coff_header)
    {
      /* We are encapsulating BSD format within COFF format.  */
      struct coffscn *tp, *dp, *bp;

      tp = &coffheader.scns[0];
      dp = &coffheader.scns[1];
      bp = &coffheader.scns[2];

      strcpy (tp->s_name, ".text");
      tp->s_paddr = text_start;
      tp->s_vaddr = text_start;
      tp->s_size = text_size;
      tp->s_scnptr = sizeof (struct coffheader) + sizeof (struct exec);
      tp->s_relptr = 0;
      tp->s_lnnoptr = 0;
      tp->s_nreloc = 0;
      tp->s_nlnno = 0;
      tp->s_flags = 0x20;
      strcpy (dp->s_name, ".data");
      dp->s_paddr = data_start;
      dp->s_vaddr = data_start;
      dp->s_size = data_size;
      dp->s_scnptr = tp->s_scnptr + tp->s_size;
      dp->s_relptr = 0;
      dp->s_lnnoptr = 0;
      dp->s_nreloc = 0;
      dp->s_nlnno = 0;
      dp->s_flags = 0x40;
      strcpy (bp->s_name, ".bss");
      bp->s_paddr = dp->s_vaddr + dp->s_size;
      bp->s_vaddr = bp->s_paddr;
      bp->s_size = bss_size;
      bp->s_scnptr = 0;
      bp->s_relptr = 0;
      bp->s_lnnoptr = 0;
      bp->s_nreloc = 0;
      bp->s_nlnno = 0;
      bp->s_flags = 0x80;

      coffheader.f_magic = COFF_MAGIC;
      coffheader.f_nscns = 3;
      /* store an unlikely time so programs can
       * tell that there is a bsd header
       */
      coffheader.f_timdat = 1;
      coffheader.f_symptr = 0;
      coffheader.f_nsyms = 0;
      coffheader.f_opthdr = 28;
      coffheader.f_flags = 0x103;
      /* aouthdr */
      coffheader.magic = ZMAGIC;
      coffheader.vstamp = 0;
      coffheader.tsize = tp->s_size;
      coffheader.dsize = dp->s_size;
      coffheader.bsize = bp->s_size;
      coffheader.entry = outheader.a_entry;
      coffheader.text_start = tp->s_vaddr;
      coffheader.data_start = dp->s_vaddr;
    }
#endif

  if (strip_symbols == STRIP_ALL)
    nsyms = 0;
  else
    {
      nsyms = (defined_global_sym_count
	       + undefined_global_sym_count);
      if (discard_locals == DISCARD_L)
	nsyms += non_L_local_sym_count;
      else if (discard_locals == DISCARD_NONE)
	nsyms += local_sym_count;
      /* One extra for following reference on indirects */
      if (output_style == OUTPUT_RELOCATABLE)
#ifndef NeXT
	nsyms += set_symbol_count + global_indirect_count;
#else
        nsyms += set_symbol_count;
#endif
    }

  if (strip_symbols == STRIP_NONE)
    nsyms += debugger_sym_count;

  outheader.a_syms = nsyms * sizeof (struct nlist);

  if (output_style == OUTPUT_RELOCATABLE)
    {
      outheader.a_trsize = text_reloc_size;
      outheader.a_drsize = data_reloc_size;
    }
  else
    {
      outheader.a_trsize = 0;
      outheader.a_drsize = 0;
#ifdef tek4300
      /* UTek has been known to panic without the following ... */
      outheader.a_textoff = ZMOFF;
      outheader.a_dataoff = ZMOFF + text_size;
      outheader.a_textaddr = text_start;
      outheader.a_dataaddr = data_start;
      outheader.a_bssaddr = data_start + data_size;
#endif
    }

  /* Initialize the various file offsets.  */

  output_text_offset = N_TXTOFF (outheader);
#ifdef N_DATOFF
#ifdef linux 
   /* if text_start and text_size are not zero or -Tdata is used, that
    * means we run ld by hand to make a fix entry point for text
    * section.
    *	H.J.
    */
  if (Tdata_flag_specified || (text_start && text_size)) {
    long int gap;

    gap = data_start - (text_start + text_size);
    fprintf (stderr, "text_start: 0x%08x\ttext_size: 0x%08x\n",
	text_start, text_size);
    fprintf (stderr, "data_start: 0x%08x\tgap: %c0x%08x\n",
	data_start, (gap >= 0 ? ' ' : '-'), (gap > 0 ? gap : -gap));
    if (gap < 0) {
      fprintf (stderr, "text_start: 0x%08x\ttext_size: 0x%08x\n",
	text_start, text_size);
      fprintf (stderr, "data_start: 0x%08x\tgap: %c0x%08x\n",
	data_start, (gap >= 0 ? ' ' : '-'), (gap > 0 ? gap : -gap));
      fatal ("no room for text section", (char *) 0);
    }
    else if (text_start && text_size) {
      /* do this only necessary */
      text_size = data_start - text_start;
      outheader.a_text = text_size;
    }
  }
#endif
  output_data_offset = N_DATOFF (outheader);
#else
  output_data_offset = output_text_offset + text_size;
#endif
#ifdef N_TRELOFF
  output_trel_offset = N_TRELOFF (outheader);
#else
  output_trel_offset = output_data_offset + data_size;
#endif
#ifdef N_DRELOFF
  output_drel_offset = N_DRELOFF (outheader);
#else
  output_drel_offset = output_trel_offset + text_reloc_size;
#endif
  output_syms_offset = N_SYMOFF (outheader);
  output_strs_offset = N_STROFF (outheader);
}

/* Compute more section offsets once the size of the string table is known.  */

void
compute_more_a_out_section_offsets ()
{
  output_symseg_offset = output_strs_offset + output_strs_size;
}

/* Write the a.out header once everything else is known.  */

void
write_a_out_header ()
{
  lseek (outdesc, 0L, 0);

#ifdef COFF_ENCAPSULATE
  if (need_coff_header)
    mywrite (&coffheader, sizeof coffheader, 1, outdesc);
#endif

#ifdef tek4300
  if (outheader.a_magic == ZMAGIC)
    mywrite (&outheader, sizeof outheader, 1, outdesc);
  else
#endif
  mywrite (&outheader, sizeof (struct exec), 1, outdesc);

  /* Output whatever padding is required in the executable file
     between the header and the start of the text.  */

#ifndef COFF_ENCAPSULATE
  padfile (N_TXTOFF (outheader) - sizeof outheader, outdesc);
#endif
}

#endif

#ifdef MACH_O

/* Stuff pertaining to creating Mach-O files. */

/* Convert the Mach-O style n_sect references into something the rest
   of the loader can understand.  */

void
translate_mach_o_symbols (entry)
     struct file_entry *entry;
{
  int i, n, g;
  struct nlist *sym;

  n = entry->syms_size / sizeof (struct nlist);
  for (i = 0; i < n; ++i)
    if (((sym = &entry->symbols[i])->n_type & ~N_EXT) == N_SECT)
      {
	if (sym->n_sect == entry->text_ordinal)
	  sym->n_type = (sym->n_type & N_EXT) | N_TEXT;
	else if (sym->n_sect == entry->data_ordinal)
	  sym->n_type = (sym->n_type & N_EXT) | N_DATA;
	else if (sym->n_sect == entry->bss_ordinal)
	  sym->n_type = (sym->n_type & N_EXT) | N_BSS;
	else
	  fatal_with_file ("unknown section referenced in symbols of ", entry);
	sym->n_sect = 0;
      }
    else if ((sym = &entry->symbols[i])->n_type == N_SLINE)
      {
	if (sym->n_sect == entry->text_ordinal)
	  sym->n_type = N_SLINE;
	else if (sym->n_sect == entry->data_ordinal)
	  sym->n_type = N_DSLINE;
	else if (sym->n_sect == entry->bss_ordinal)
	  sym->n_type = N_BSLINE;
	else
	  fatal_with_file ("unknown section referenced in debugging symbols of ", entry);
      }
}

/* Convert Mach-O style relocation info into a.out style relocation
   info internally.  */
void
translate_mach_o_relocation (entry, reloc, count)
     struct file_entry *entry;
     struct relocation_info *reloc;
     int count;
{
  int i;

  for (i = 0; i < count; ++i)
    if (!RELOC_EXTERN_P(&reloc[i]))
      if (RELOC_TYPE(&reloc[i]) == R_ABS)
	RELOC_TYPE(&reloc[i]) = N_ABS;
      else if (RELOC_TYPE(&reloc[i]) == entry->text_ordinal)
	RELOC_TYPE(&reloc[i]) = N_TEXT;
      else if (RELOC_TYPE(&reloc[i]) == entry->data_ordinal)
	RELOC_TYPE(&reloc[i]) = N_DATA;
      else if (RELOC_TYPE(&reloc[i]) == entry->bss_ordinal)
	RELOC_TYPE(&reloc[i]) = N_BSS;
      else
	fatal_with_file ("unknown section ordinal in relocation info of ", entry);
}

/* Header structure for OUTPUT_RELOCATABLE.  */

struct
{
  struct mach_header header;
  struct segment_command segment;
  struct section text;
  struct section data;
  struct section bss;
  struct symtab_command symtab;
#ifdef LC_SYMSEG
  struct symseg_command symseg;
#endif
} m_object;

#ifdef NeXT
#define CPU_TYPE CPU_TYPE_MC68030
#define CPU_SUBTYPE CPU_SUBTYPE_NeXT
#define THREAD_FLAVOR NeXT_THREAD_STATE_REGS
#define THREAD_COUNT NeXT_THREAD_STATE_REGS_COUNT
typedef struct NeXT_thread_state_regs thread_state;
#define thread_state_entry_field pc
#endif

/* Header structure for all executable output forms.  */

struct
{
  struct mach_header header;
  struct segment_command pagezero;
  struct segment_command text_segment;
  struct section text;
  struct segment_command data_segment;
  struct section data;
  struct section bss;
  struct thread_command unixthread;
  unsigned long int flavor;
  unsigned long int count;
  thread_state state;
  struct symtab_command symtab;
#ifdef LC_SYMSEG
  struct symseg_command symseg;
#endif
} m_exec;

/* Compute text_start and text_header_size for an a.out file.  */

void
initialize_mach_o_text_start ()
{
  if (output_style != OUTPUT_RELOCATABLE)
    {
      text_header_size = sizeof m_exec;
      if (!T_flag_specified && output_style != OUTPUT_RELOCATABLE)
	/* We reserve the first page of an executable to trap NULL dereferences.  */
	text_start = page_size;
    }
}

/* Compute data_start once text_size is known.  */

void
initialize_mach_o_data_start ()
{
  if (! Tdata_flag_specified)
    data_start = text_start + text_size;
}

/* Compute offsets of various pieces of the Mach-O output file.  */
void
compute_mach_o_section_offsets ()
{
  int header_size, trsize, drsize;

  switch (output_style)
    {
    case OUTPUT_RELOCATABLE:
      header_size = sizeof m_object;
      break;
    default:
      header_size = sizeof m_exec;
      break;
    }

  if (strip_symbols == STRIP_ALL)
    nsyms = 0;
  else
    {
      nsyms = (defined_global_sym_count
	       + undefined_global_sym_count);
      if (discard_locals == DISCARD_L)
	nsyms += non_L_local_sym_count;
      else if (discard_locals == DISCARD_NONE)
	nsyms += local_sym_count;
      /* One extra for following reference on indirects */
      if (output_style == OUTPUT_RELOCATABLE)
#ifndef NeXT
	nsyms += set_symbol_count + global_indirect_count;
#else
        nsyms += set_symbol_count;
#endif
    }

  if (strip_symbols == STRIP_NONE)
    nsyms += debugger_sym_count;

  output_text_offset = header_size;
  output_data_offset = output_text_offset + text_size;
  output_trel_offset = output_data_offset + data_size;
  if (output_style == OUTPUT_RELOCATABLE)
    trsize = text_reloc_size, drsize = data_reloc_size;
  else
    trsize = drsize = 0;
  output_drel_offset = output_trel_offset + trsize;
  output_syms_offset = output_drel_offset + drsize;
  output_strs_offset = output_syms_offset + nsyms * sizeof (struct nlist);
}

/* Compute more section offsets once the size of the string table is known.  */
void
compute_more_mach_o_section_offsets ()
{
  output_symseg_offset = output_strs_offset + output_strs_size;
}

/* Write the Mach-O header once everything else is known.  */

void
write_mach_o_header ()
{
  struct mach_header header;
  struct section text, data, bss;
  struct symtab_command symtab;
#ifdef LC_SYMSEG
  struct symseg_command symseg;
#endif
  thread_state state;

  lseek (outdesc, 0L, 0);


  header.magic = MH_MAGIC;
  header.cputype = CPU_TYPE;
  header.cpusubtype = CPU_SUBTYPE;
  header.filetype = output_style == OUTPUT_RELOCATABLE ? MH_OBJECT : MH_EXECUTE;
#ifdef LC_SYMSEG
  switch (output_style)
    {
    case OUTPUT_RELOCATABLE:
      header.ncmds = 3;
      header.sizeofcmds = sizeof m_object - sizeof header;
      break;
    default:
      header.ncmds = 6;
      header.sizeofcmds = sizeof m_exec - sizeof header;
      break;
    }
#else
  switch (output_style)
    {
    case OUTPUT_RELOCATABLE:
      header.ncmds = 2;
      header.sizeofcmds = sizeof m_object - sizeof header;
      break;
    default:
      header.ncmds = 5;
      header.sizeofcmds = sizeof m_exec - sizeof header;
      break;
    }
#endif
  header.flags = undefined_global_sym_count ? 0 : MH_NOUNDEFS;

  bzero((char *) &text, sizeof text);
  strncpy(text.sectname, SECT_TEXT, sizeof text.sectname);
  strncpy(text.segname, SEG_TEXT, sizeof text.segname);
  text.addr = text_start;
  text.size = text_size;
  text.offset = output_text_offset;
  text.align = text.addr % sizeof (double) ? sizeof (int) : sizeof (double);
  text.reloff = output_trel_offset;
  text.nreloc = output_style == OUTPUT_RELOCATABLE
    ? text_reloc_size / sizeof (struct relocation_info) : 0;
  text.flags = 0;

  bzero((char *) &data, sizeof data);
  strncpy(data.sectname, SECT_DATA, sizeof data.sectname);
  strncpy(data.segname, output_style == OUTPUT_WRITABLE_TEXT ? SEG_TEXT : SEG_DATA,
	  sizeof data.segname);
  data.addr = data_start;
  data.size = data_size;
  data.offset = output_data_offset;
  data.align = data.addr % sizeof (double) ? sizeof (int) : sizeof (double);
  data.reloff = output_drel_offset;
  data.nreloc = output_style == OUTPUT_RELOCATABLE
    ? data_reloc_size / sizeof (struct relocation_info) : 0;
  data.flags = 0;

  bzero((char *) &bss, sizeof bss);
  strncpy(bss.sectname, SECT_BSS, sizeof data.sectname);
  strncpy(bss.segname, output_style == OUTPUT_WRITABLE_TEXT ? SEG_TEXT : SEG_DATA,
	  sizeof bss.segname);
  bss.addr = data_start + data_size;
  bss.size = bss_size;
  bss.align = bss.addr % sizeof (double) ? sizeof (int) : sizeof (double);
  bss.reloff = 0;
  bss.nreloc = 0;
  bss.flags = S_ZEROFILL;

  symtab.cmd = LC_SYMTAB;
  symtab.cmdsize = sizeof symtab;
  symtab.symoff = output_syms_offset;
  symtab.nsyms = output_syms_size / sizeof (struct nlist);
  symtab.stroff = output_strs_offset;
  symtab.strsize = output_strs_size;

#ifdef LC_SYMSEG
  symseg.cmd = LC_SYMSEG;
  symseg.cmdsize = sizeof symseg;
  symseg.offset = output_symseg_offset;
  symseg.size = output_symseg_size;
#endif

  switch (output_style)
    {
    case OUTPUT_RELOCATABLE:
      m_object.header = header;
      m_object.segment.cmd = LC_SEGMENT;
      m_object.segment.cmdsize = sizeof (struct segment_command) + 3 * sizeof (struct section);
      strncpy(m_object.segment.segname, SEG_TEXT, sizeof m_object.segment.segname);
      m_object.segment.vmaddr = 0;
      m_object.segment.vmsize = text.size + data.size + bss.size;
      m_object.segment.fileoff = text.offset;
      m_object.segment.filesize = text.size + data.size;
      m_object.segment.maxprot = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE;
      m_object.segment.initprot = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE;
      m_object.segment.nsects = 3;
      m_object.segment.flags = 0;
      m_object.text = text;
      m_object.data = data;
      m_object.bss = bss;
      m_object.symtab = symtab;
#ifdef LC_SYMSEG
      m_object.symseg = symseg;
#endif
      mywrite((char *) &m_object, 1, sizeof m_object, outdesc);
      break;

    default:
      m_exec.header = header;
      m_exec.pagezero.cmd = LC_SEGMENT;
      m_exec.pagezero.cmdsize = sizeof (struct segment_command);
      strncpy(m_exec.pagezero.segname, SEG_PAGEZERO, sizeof m_exec.pagezero.segname);
      m_exec.pagezero.vmaddr = 0;
      m_exec.pagezero.vmsize = page_size;
      m_exec.pagezero.fileoff = 0;
      m_exec.pagezero.filesize = 0;
      m_exec.pagezero.maxprot = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE;
      m_exec.pagezero.initprot = 0;
      m_exec.pagezero.nsects = 0;
      m_exec.pagezero.flags = 0;
      m_exec.text_segment.cmd = LC_SEGMENT;
      m_exec.text_segment.cmdsize = sizeof (struct segment_command) + sizeof (struct section);
      strncpy(m_exec.text_segment.segname, SEG_TEXT, sizeof m_exec.text_segment.segname);
      m_exec.text_segment.vmaddr = text_start;
      m_exec.text_segment.vmsize = text_size;
      m_exec.text_segment.fileoff = output_text_offset;
      m_exec.text_segment.filesize = text_size;
      m_exec.text_segment.maxprot = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE;
      m_exec.text_segment.initprot = VM_PROT_READ | VM_PROT_EXECUTE;
      if (output_style == OUTPUT_WRITABLE_TEXT)
	m_exec.text_segment.initprot |= VM_PROT_WRITE;
      m_exec.text_segment.nsects = 1;
      m_exec.text_segment.flags = 0;
      m_exec.text = text;
      m_exec.data_segment.cmd = LC_SEGMENT;
      m_exec.data_segment.cmdsize = sizeof (struct segment_command) + 2 * sizeof (struct section);
      strncpy(m_exec.data_segment.segname, SEG_DATA, sizeof m_exec.data_segment.segname);
      m_exec.data_segment.vmaddr = data_start;
      m_exec.data_segment.vmsize = data_size + bss_size;
      m_exec.data_segment.fileoff = output_data_offset;
      m_exec.data_segment.filesize = data_size;
      m_exec.data_segment.maxprot = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE;
      m_exec.data_segment.initprot = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE;
      m_exec.data_segment.nsects = 2;
      m_exec.data_segment.flags = 0;
      m_exec.data = data;
      m_exec.bss = bss;
      m_exec.unixthread.cmd = LC_UNIXTHREAD;
      m_exec.unixthread.cmdsize
	= sizeof (struct thread_command) + 2 * sizeof (long int) + sizeof (thread_state);
      m_exec.flavor = THREAD_FLAVOR;
      m_exec.count = THREAD_COUNT;
      m_exec.state.thread_state_entry_field = entry_symbol
	? entry_symbol->value : text_start + text_header_size;
      m_exec.symtab = symtab;
#ifdef LC_SYMSEG
      m_exec.symseg = symseg;
#endif
      mywrite((char *) &m_exec, 1, sizeof m_exec, outdesc);
      break;
    }
}

/* Translate a.out style symbols into Mach-O style symbols.  */

void
generate_mach_o_symbols (syms, nsyms)
     struct nlist *syms;
     int nsyms;
{
  int i;

  for (i = 0; i < nsyms; ++i)
    switch (syms[i].n_type)
      {
      case N_TEXT:
      case N_TEXT | N_EXT:
	syms[i].n_type = syms[i].n_type & N_EXT | N_SECT;
	syms[i].n_sect = 1;	/* text section ordinal */
	break;
      case N_DATA:
      case N_DATA | N_EXT:
	syms[i].n_type = syms[i].n_type & N_EXT | N_SECT;
	syms[i].n_sect = 2;	/* data section ordinal */
	break;
      case N_BSS:
      case N_BSS | N_EXT:
	syms[i].n_type = syms[i].n_type & N_EXT | N_BSS;
	syms[i].n_sect = 3;	/* bss section ordinal */
	break;
      case N_SLINE:
	syms[i].n_type = N_SLINE;
	syms[i].n_sect = 1;	/* text section ordinal */
	break;
      case N_DSLINE:
	syms[i].n_type = N_SLINE;
	syms[i].n_sect = 2;	/* data section ordinal */
	break;
      case N_BSLINE:
	syms[i].n_type = N_SLINE;
	syms[i].n_sect = 3;	/* bss section ordinal */
	break;
      }
}

/* Translate a.out style relocation info into Mach-O style relocation
   info.  */

void
generate_mach_o_relocations (reloc, nreloc)
     struct relocation_info *reloc;
     int nreloc;
{
  int i;

  for (i = 0; i < nreloc; ++i)
    if (!RELOC_EXTERN_P (&reloc[i]))
      switch (RELOC_TYPE (&reloc[i]))
	{
	case N_ABS:
	case N_ABS | N_EXT:
	  RELOC_TYPE (&reloc[i]) = R_ABS;
	  break;
	case N_TEXT:
	case N_TEXT | N_EXT:
	  RELOC_TYPE (&reloc[i]) = 1; /* output text section ordinal */
	  break;
	case N_DATA:
	case N_DATA | N_EXT:
	  RELOC_TYPE (&reloc[i]) = 2; /* output data section ordinal */
	  break;
	case N_BSS:
	case N_BSS | N_EXT:
	  RELOC_TYPE (&reloc[i]) = 3; /* output bss section ordinal */
	  break;
	}
}

#endif

/* The following functions are simple switches according to the
   output style.  */

/* Compute text_start and text_header_size as appropriate for the
   output format.  */

void
initialize_text_start ()
{
#ifdef A_OUT
  if (output_file_type == IS_A_OUT)
    {
      initialize_a_out_text_start ();
      return;
    }
#endif
#ifdef MACH_O
  if (output_file_type == IS_MACH_O)
    {
      initialize_mach_o_text_start ();
      return;
    }
#endif
  fatal ("unknown output file type (enum file_type)", (char *) 0);
}

/* Initialize data_start as appropriate to the output format, once text_size
   is known.  */

void
initialize_data_start ()
{
#ifdef A_OUT
  if (output_file_type == IS_A_OUT)
    {
      initialize_a_out_data_start ();
      return;
    }
#endif
#ifdef MACH_O
  if (output_file_type == IS_MACH_O)
    {
      initialize_mach_o_data_start ();
      return;
    }
#endif
  fatal ("unknown output file type (enum file_type)", (char *) 0);
}

/* Compute offsets of the various sections within the output file.  */

void
compute_section_offsets ()
{
#ifdef A_OUT
  if (output_file_type == IS_A_OUT)
    {
      compute_a_out_section_offsets ();
      return;
    }
#endif
#ifdef MACH_O
  if (output_file_type == IS_MACH_O)
    {
      compute_mach_o_section_offsets ();
      return;
    }
#endif
  fatal ("unknown output file type (enum file_type)", (char *) 0);
}

/* Compute more section offsets, once the size of the string table
   is finalized.  */
void
compute_more_section_offsets ()
{
#ifdef A_OUT
  if (output_file_type == IS_A_OUT)
    {
      compute_more_a_out_section_offsets ();
      return;
    }
#endif
#ifdef MACH_O
  if (output_file_type == IS_MACH_O)
    {
      compute_more_mach_o_section_offsets ();
      return;
    }
#endif
  fatal ("unknown output file type (enum file_type)", (char *) 0);
}

/* Write the output file header, once everything is known.  */
void
write_header ()
{
#ifdef A_OUT
  if (output_file_type == IS_A_OUT)
    {
      write_a_out_header ();
      return;
    }
#endif
#ifdef MACH_O
  if (output_file_type == IS_MACH_O)
    {
      write_mach_o_header ();
      return;
    }
#endif
  fatal ("unknown output file type (enum file_type)", (char *) 0);
}

/* Write the output file */

void
write_output ()
{
  struct stat statbuf;
  int filemode, mask;

  /* Remove the old file in case it is owned by someone else.
     This prevents spurious "not owner" error messages.
     Don't check for errors from unlink; we don't really care
     whether it worked.

     Note that this means that if the output file is hard linked,
     the other names will still have the old contents.  This is
     the way Unix ld works; I'm going to consider it a feature.  */
  (void) unlink (output_filename);
  
  outdesc = open (output_filename, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  if (outdesc < 0) perror_name (output_filename);

  if (fstat (outdesc, &statbuf) < 0)
    perror_name (output_filename);

  filemode = statbuf.st_mode;

  chmod (output_filename, filemode & ~0111);

  /* Calculate the offsets of the various pieces of the output file.  */
  compute_section_offsets ();

  /* Output the text and data segments, relocating as we go.  */
  write_text ();
  write_data ();

  /* Output the merged relocation info, if requested with `-r'.  */
  if (output_style == OUTPUT_RELOCATABLE)
    write_rel ();

  /* Output the symbol table (both globals and locals).  */
  write_syms ();

  /* At this point the total size of the symbol table and string table
     are finalized.  */
  compute_more_section_offsets ();

  /* Copy any GDB symbol segments from input files.  */
  write_symsegs ();

  /* Now that everything is known about the output file, write its header.  */
  write_header ();

  close (outdesc);

  mask = umask (0);
  umask (mask);

  if (chmod (output_filename, filemode | (0111 & ~mask)) == -1)
    perror_name (output_filename);
}

void modify_location (), perform_relocation (), copy_text (), copy_data ();

/* Relocate the text segment of each input file
   and write to the output file.  */

void
write_text ()
{
  if (trace_files)
    fprintf (stderr, "Copying and relocating text:\n\n");

  lseek (outdesc, output_text_offset + text_header_size, 0);

  each_full_file (copy_text, 0);
  file_close ();

  if (trace_files)
    fprintf (stderr, "\n");

  padfile (text_pad, outdesc);
}

/* Read in all of the relocation information */

void
read_relocation ()
{
  each_full_file (read_file_relocation, 0);
}

/* Read in the relocation sections of ENTRY if necessary */

void
read_file_relocation (entry)
     struct file_entry *entry;
{
  register struct relocation_info *reloc;
  int desc;
  int read_return;

  desc = -1;
  if (!entry->textrel)
    {
      reloc = (struct relocation_info *) xmalloc (entry->text_reloc_size);
      desc = file_open (entry);
      lseek (desc, entry->starting_offset + entry->text_reloc_offset, L_SET);
      if (entry->text_reloc_size != (read_return = read (desc, reloc, entry->text_reloc_size)))
	{
	  fprintf (stderr, "Return from read: %d\n", read_return);
	  fatal_with_file ("premature eof in text relocation of ", entry);
	}
      entry->textrel = reloc;
    }

  if (!entry->datarel)
    {
      reloc = (struct relocation_info *) xmalloc (entry->data_reloc_size);
      if (desc == -1) desc = file_open (entry);
      lseek (desc, entry->starting_offset + entry->data_reloc_offset, L_SET);
      if (entry->data_reloc_size != read (desc, reloc, entry->data_reloc_size))
	fatal_with_file ("premature eof in data relocation of ", entry);
      entry->datarel = reloc;
    }

#ifdef MACH_O
  if (entry->file_type == IS_MACH_O)
    {
      translate_mach_o_relocation (entry, entry->textrel,
				   entry->text_reloc_size / sizeof (struct relocation_info));
      translate_mach_o_relocation (entry, entry->datarel,
				   entry->data_reloc_size / sizeof (struct relocation_info));
    }
#endif
}

/* Read the text segment contents of ENTRY, relocate them,
   and write the result to the output file.
   If `-r', save the text relocation for later reuse.  */

void
copy_text (entry)
     struct file_entry *entry;
{
  register char *bytes;
  register int desc;
  register struct relocation_info *reloc;

  if (trace_files)
    prline_file_name (entry, stderr);

  desc = file_open (entry);

  /* Allocate space for the file's text section */

  bytes = (char *) alloca (entry->text_size);

  /* Deal with relocation information however is appropriate */

  if (entry->textrel)  reloc = entry->textrel;
  else if (output_style == OUTPUT_RELOCATABLE)
    {
      read_file_relocation (entry);
      reloc = entry->textrel;
    }
  else
    {
      reloc = (struct relocation_info *) alloca (entry->text_reloc_size);
      lseek (desc, entry->starting_offset + entry->text_reloc_offset, L_SET);
      if (entry->text_reloc_size != read (desc, reloc, entry->text_reloc_size))
	fatal_with_file ("premature eof in text relocation of ", entry);
#ifdef MACH_O
      if (entry->file_type == IS_MACH_O)
	translate_mach_o_relocation (entry, reloc,
				     entry->text_reloc_size / sizeof (struct relocation_info));
#endif
    }

  /* Read the text section into core.  */

  lseek (desc, entry->starting_offset + entry->text_offset, L_SET);
  if (entry->text_size != read (desc, bytes, entry->text_size))
    fatal_with_file ("premature eof in text section of ", entry);

  /* Relocate the text according to the text relocation.  */

  perform_relocation (bytes, entry->text_start_address - entry->orig_text_address,
		      entry->text_size, reloc, entry->text_reloc_size, entry);

  /* Write the relocated text to the output file.  */

  mywrite (bytes, 1, entry->text_size, outdesc);
}

/* Relocate the data segment of each input file
   and write to the output file.  */

void
write_data ()
{
  if (trace_files)
    fprintf (stderr, "Copying and relocating data:\n\n");

  lseek (outdesc, output_data_offset, 0);

  each_full_file (copy_data, 0);
  file_close ();

  /* Write out the set element vectors.  See digest symbols for
     description of length of the set vector section.  */

  if (set_vector_count)
    mywrite (set_vectors, 2 * set_symbol_count + set_vector_count,
	     sizeof (unsigned long), outdesc);

  if (trace_files)
    fprintf (stderr, "\n");

  padfile (data_pad, outdesc);
}

/* Read the data segment contents of ENTRY, relocate them,
   and write the result to the output file.
   If `-r', save the data relocation for later reuse.
   See comments in `copy_text'.  */

void
copy_data (entry)
     struct file_entry *entry;
{
  register struct relocation_info *reloc;
  register char *bytes;
  register int desc;

  if (trace_files)
    prline_file_name (entry, stderr);

  desc = file_open (entry);

  bytes = (char *) alloca (entry->data_size);

  if (entry->datarel) reloc = entry->datarel;
  else if (output_style == OUTPUT_RELOCATABLE)	/* Will need this again */
    {
      read_file_relocation (entry);
      reloc = entry->datarel;
    }
  else
    {
      reloc = (struct relocation_info *) alloca (entry->data_reloc_size);
      lseek (desc, entry->starting_offset + entry->data_reloc_offset, L_SET);
      if (entry->data_reloc_size != read (desc, reloc, entry->data_reloc_size))
	fatal_with_file ("premature eof in data relocation of ", entry);
#ifdef MACH_O
      if (entry->file_type == IS_MACH_O)
	translate_mach_o_relocation (entry, reloc,
				     entry->data_reloc_size / sizeof (struct relocation_info));
#endif
    }

  lseek (desc, entry->starting_offset + entry->data_offset, L_SET);
  if (entry->data_size != read (desc, bytes, entry->data_size))
    fatal_with_file ("premature eof in data section of ", entry);

  perform_relocation (bytes, entry->data_start_address - entry->orig_data_address,
		      entry->data_size, reloc, entry->data_reloc_size, entry);

  mywrite (bytes, 1, entry->data_size, outdesc);
}

/* Relocate ENTRY's text or data section contents.
   DATA is the address of the contents, in core.
   DATA_SIZE is the length of the contents.
   PC_RELOCATION is the difference between the address of the contents
     in the output file and its address in the input file.
   RELOC_INFO is the address of the relocation info, in core.
   RELOC_SIZE is its length in bytes.  */
/* This version is about to be severly hacked by Randy.  Hope it
   works afterwards. */
void
perform_relocation (data, pc_relocation, data_size, reloc_info, reloc_size, entry)
     char *data;
     struct relocation_info *reloc_info;
     struct file_entry *entry;
     int pc_relocation;
     int data_size;
     int reloc_size;
{
  register struct relocation_info *p = reloc_info;
  struct relocation_info *end
    = reloc_info + reloc_size / sizeof (struct relocation_info);
  int text_relocation = entry->text_start_address - entry->orig_text_address;
  int data_relocation = entry->data_start_address - entry->orig_data_address;
  int bss_relocation = entry->bss_start_address - entry->orig_bss_address;

  for (; p < end; p++)
    {
      register int relocation = 0;
      register int addr = RELOC_ADDRESS(p);
      register unsigned int mask = 0;

      if (addr >= data_size)
	fatal_with_file ("relocation address out of range in ", entry);

      if (RELOC_EXTERN_P(p))
	{
	  int symindex = RELOC_SYMBOL (p) * sizeof (struct nlist);
	  symbol *sp = ((symbol *)
			(((struct nlist *)
			  (((char *)entry->symbols) + symindex))
			 ->n_un.n_name));

#ifdef N_INDR
	  /* Resolve indirection */
	  if ((sp->defined & ~N_EXT) == N_INDR)
	    sp = (symbol *) sp->value;
#endif

	  if (symindex >= entry->syms_size)
	    fatal_with_file ("relocation symbolnum out of range in ", entry);

	  /* If the symbol is undefined, leave it at zero.  */
	  if (! sp->defined)
	    relocation = 0;
	  else
	    relocation = sp->value;
	}
      else switch (RELOC_TYPE(p))
	{
	case N_TEXT:
	case N_TEXT | N_EXT:
	  relocation = text_relocation;
	  break;

	case N_DATA:
	case N_DATA | N_EXT:
	  relocation = data_relocation;
	  break;

	case N_BSS:
	case N_BSS | N_EXT:
	  relocation = bss_relocation;
	  break;

	case N_ABS:
	case N_ABS | N_EXT:
	  /* Don't know why this code would occur, but apparently it does.  */
	  break;

	default:
	  fatal_with_file ("nonexternal relocation code invalid in ", entry);
	}

      if (RELOC_PCREL_P(p))
	relocation -= pc_relocation;

#ifdef RELOC_ADD_EXTRA
      relocation += RELOC_ADD_EXTRA(p);
      if (output_style == OUTPUT_RELOCATABLE)
	{
	  /* If this RELOC_ADD_EXTRA is 0, it means that the
	     symbol was external and the relocation does not
	     need a fixup here.  */
	  if (RELOC_ADD_EXTRA (p))
	    {
	      if (! RELOC_PCREL_P (p))
		RELOC_ADD_EXTRA (p) = relocation;
	      else
		RELOC_ADD_EXTRA (p) -= pc_relocation;
	    }
#if 0
	  if (! RELOC_PCREL_P (p))
	    {
	      if ((int)p->r_type <= RELOC_32
		  || RELOC_EXTERN_P (p) == 0)
		RELOC_ADD_EXTRA (p) = relocation;
	    }
	  else if (RELOC_EXTERN_P (p))
	    RELOC_ADD_EXTRA (p) -= pc_relocation;
#endif
	  continue;
	}
#endif

      relocation >>= RELOC_VALUE_RIGHTSHIFT(p);

      /* Unshifted mask for relocation */
      mask = 1 << (RELOC_TARGET_BITSIZE(p) - 1);
      mask |= mask - 1;

      /* Shift everything up to where it's going to be used */
      relocation <<= RELOC_TARGET_BITPOS(p);
      mask <<= RELOC_TARGET_BITPOS(p);

#ifdef ns32000
      /* This code by Ian Dall for the ns32k displacements */
      {
	char * loc = (data + addr);
	int bytes = (1 << RELOC_TARGET_SIZE(p));
	void put_num(), put_disp(), put_imm();
	int get_num(), get_disp(), get_imm();
	switch(p->r_disp)
	  {
	  case 0:
	    if (RELOC_MEMORY_SUB_P(p))
	      put_imm(loc, relocation - get_imm(loc, bytes), bytes);
	    else if (RELOC_MEMORY_ADD_P(p))
	      put_imm(loc, relocation + get_imm(loc, bytes), bytes);
	    break;
	  case 1:
	    if (RELOC_MEMORY_SUB_P(p))
	      put_disp(loc, relocation - get_disp(loc, bytes), bytes);
	    else if (RELOC_MEMORY_ADD_P(p))
	      put_disp(loc, relocation + get_disp(loc, bytes), bytes);
	    break;
	  case 2:
	    if (RELOC_MEMORY_SUB_P(p))
	      put_num(loc, relocation - get_num(loc, bytes), bytes);
	    else if (RELOC_MEMORY_ADD_P(p))
	      put_num(loc, relocation + get_num(loc, bytes), bytes);
	    break;
	  }
      }
#else
      switch (RELOC_TARGET_SIZE(p))
	{
	case 0:
	  if (RELOC_MEMORY_SUB_P(p))
	    relocation -= *(char *) (data + addr);
	  else if (RELOC_MEMORY_ADD_P(p))
	    relocation += *(char *) (data + addr);
	  *(char *) (data + addr) &= ~mask;
	  *(char *) (data + addr) |= relocation & mask;
	  break;

	case 1:
	  if (RELOC_MEMORY_SUB_P(p))
	    relocation -= *(short *) (data + addr);
	  else if (RELOC_MEMORY_ADD_P(p))
	    relocation += *(short *) (data + addr);
	  *(short *) (data + addr) &= ~mask;
	  *(short *) (data + addr) |= relocation & mask;
	  break;

	case 2:
#ifdef CROSS_LINKER
	  /* This is necessary if the host has stricter alignment
	     than the target.  Too slow to use all the time.
	     Also doesn't deal with differing byte-order.  */
	  {
	    /* Thing to relocate.  */
	    long thing;
	    bcopy (data + addr, &thing, sizeof (thing));
	    if (RELOC_MEMORY_SUB_P (p))
	      relocation -= thing;
	    else if (RELOC_MEMORY_ADD_P (p))
	      relocation += thing;
	    thing = (thing & ~mask) | relocation & mask;
	    bcopy (&thing, data + addr, sizeof (thing));
	  }
#else /* not CROSS_LINKER */
	  if (RELOC_MEMORY_SUB_P(p))
	    relocation -= *(long *) (data + addr);
	  else if (RELOC_MEMORY_ADD_P(p))
	    relocation += *(long *) (data + addr);
	  *(long *) (data + addr) &= ~mask;
	  *(long *) (data + addr) |= relocation & mask;
#endif /* not CROSS_LINKER */
	  break;

	default:
	  fatal_with_file ("Unimplemented relocation field length in ", entry);
	}
#endif /* MINIX */
    }
}

/* For OUTPUT_RELOCATABLE only: write out the relocation,
   relocating the addresses-to-be-relocated.  */

void coptxtrel (), copdatrel ();

void
write_rel ()
{
  register int i;
  register int count = 0;

  if (trace_files)
    fprintf (stderr, "Writing text relocation:\n\n");

  /* Assign each global symbol a sequence number, giving the order
     in which `write_syms' will write it.
     This is so we can store the proper symbolnum fields
     in relocation entries we write.  */

  for (i = 0; i < TABSIZE; i++)
    {
      symbol *sp;
      for (sp = symtab[i]; sp; sp = sp->link)
	if (sp->referenced || sp->defined)
	  {
	    sp->def_count = count++;
#ifndef NeXT
	    /* Leave room for the reference required by N_INDR, if
	       necessary.  */
	    if ((sp->defined & ~N_EXT) == N_INDR)
	      count++;
#endif
	  }
    }
  /* Correct, because if (OUTPUT_RELOCATABLE), we will also be writing
     whatever indirect blocks we have.  */
#ifndef NeXT
  if (count != defined_global_sym_count
      + undefined_global_sym_count + global_indirect_count)
#else
  if (count != defined_global_sym_count
      + undefined_global_sym_count)
#endif
    fatal ("internal error");

  /* Write out the relocations of all files, remembered from copy_text.  */

  lseek (outdesc, output_trel_offset, 0);
  each_full_file (coptxtrel, 0);

  if (trace_files)
    fprintf (stderr, "\nWriting data relocation:\n\n");

  lseek (outdesc, output_drel_offset, 0);
  each_full_file (copdatrel, 0);

  if (trace_files)
    fprintf (stderr, "\n");
}

void
coptxtrel (entry)
     struct file_entry *entry;
{
  register struct relocation_info *p, *end;
  register int reloc = entry->text_start_address - text_start;

  p = entry->textrel;
  end = (struct relocation_info *) (entry->text_reloc_size + (char *) p);
  while (p < end)
    {
      RELOC_ADDRESS(p) += reloc;
      if (RELOC_EXTERN_P(p))
	{
	  register int symindex = RELOC_SYMBOL(p) * sizeof (struct nlist);
	  symbol *symptr = ((symbol *)
			    (((struct nlist *)
			      (((char *)entry->symbols) + symindex))
			     ->n_un.n_name));

	  if (symindex >= entry->syms_size)
	    fatal_with_file ("relocation symbolnum out of range in ", entry);

#ifdef N_INDR
	  /* Resolve indirection.  */
	  if ((symptr->defined & ~N_EXT) == N_INDR)
	    symptr = (symbol *) symptr->value;
#endif

	  /* If the symbol is now defined, change the external relocation
	     to an internal one.  */

	  if (symptr->defined)
	    {
	      RELOC_EXTERN_P(p) = 0;
	      RELOC_SYMBOL(p) = (symptr->defined & ~N_EXT);
#ifdef RELOC_ADD_EXTRA
	      /* If we aren't going to be adding in the value in
	         memory on the next pass of the loader, then we need
		 to add it in from the relocation entry.  Otherwise
	         the work we did in this pass is lost.  */
	      if (!RELOC_MEMORY_ADD_P(p))
		RELOC_ADD_EXTRA (p) += symptr->value;
#endif
	    }
	  else
	    /* Debugger symbols come first, so have to start this
	       after them.  */
#ifndef NeXT
	      RELOC_SYMBOL(p) = (symptr->def_count + nsyms
				 - defined_global_sym_count
				 - undefined_global_sym_count
				 - global_indirect_count);
#else
	      RELOC_SYMBOL(p) = (symptr->def_count + nsyms
				 - defined_global_sym_count
				 - undefined_global_sym_count);
#endif
	}
      p++;
    }

#ifdef MACH_O
  if (output_file_type == IS_MACH_O)
    generate_mach_o_relocations(entry->textrel,
				entry->text_reloc_size / sizeof (struct relocation_info));
#endif

  mywrite (entry->textrel, 1, entry->text_reloc_size, outdesc);
}

void
copdatrel (entry)
     struct file_entry *entry;
{
  register struct relocation_info *p, *end;
  /* Relocate the address of the relocation.
     Old address is relative to start of the input file's data section.
     New address is relative to start of the output file's data section.

     So the amount we need to relocate it by is the offset of this
     input file's data section within the output file's data section.  */
  register int reloc = entry->data_start_address - data_start;

  p = entry->datarel;
  end = (struct relocation_info *) (entry->data_reloc_size + (char *) p);
  while (p < end)
    {
      RELOC_ADDRESS(p) += reloc;
      if (RELOC_EXTERN_P(p))
	{
	  register int symindex = RELOC_SYMBOL(p) * sizeof (struct nlist);
	  symbol *symptr = ((symbol *)
			    (((struct nlist *)
			      (((char *)entry->symbols) + symindex))
			     ->n_un.n_name));
	  int symtype;

	  if (symindex >= entry->syms_size)
	    fatal_with_file ("relocation symbolnum out of range in ", entry);

#ifdef N_INDR
	  /* Resolve indirection.  */
	  if ((symptr->defined & ~N_EXT) == N_INDR)
	    symptr = (symbol *) symptr->value;
#endif

	  symtype = symptr->defined & ~N_EXT;

	  if (force_common_definition
	      || symtype == N_DATA || symtype == N_TEXT || symtype == N_ABS)
	    {
	      RELOC_EXTERN_P(p) = 0;
	      RELOC_SYMBOL(p) = symtype;
	    }
	  else
	    /* Debugger symbols come first, so have to start this
	       after them.  */
#ifndef NeXT
	    RELOC_SYMBOL(p)
	      = (((symbol *)
		  (((struct nlist *)
		    (((char *)entry->symbols) + symindex))
		   ->n_un.n_name))
		 ->def_count
		 + nsyms - defined_global_sym_count
		 - undefined_global_sym_count
		 - global_indirect_count);
#else
	    RELOC_SYMBOL(p)
	      = (((symbol *)
		  (((struct nlist *)
		    (((char *)entry->symbols) + symindex))
		   ->n_un.n_name))
		 ->def_count
		 + nsyms - defined_global_sym_count
		 - undefined_global_sym_count);
#endif
	}
      p++;
    }
#ifdef MACH_O
  if (output_file_type == IS_MACH_O)
    generate_mach_o_relocations(entry->datarel,
				entry->data_reloc_size / sizeof (struct relocation_info));
#endif

  mywrite (entry->datarel, 1, entry->data_reloc_size, outdesc);
}

void write_file_syms ();
void write_string_table ();

/* Total size of string table strings allocated so far,
   including strings in `strtab_vector'.  */
int strtab_size;

/* Vector whose elements are strings to be added to the string table.  */
char **strtab_vector;

/* Vector whose elements are the lengths of those strings.  */
int *strtab_lens;

/* Index in `strtab_vector' at which the next string will be stored.  */
int strtab_index;

/* Add the string NAME to the output file string table.
   Record it in `strtab_vector' to be output later.
   Return the index within the string table that this string will have.  */

int
assign_string_table_index (name)
     char *name;
{
  register int index = strtab_size;
  register int len = strlen (name) + 1;

  strtab_size += len;
  strtab_vector[strtab_index] = name;
  strtab_lens[strtab_index++] = len;

  return index;
}

FILE *outstream = (FILE *) 0;

/* Write the contents of `strtab_vector' into the string table.
   This is done once for each file's local&debugger symbols
   and once for the global symbols.  */

void
write_string_table ()
{
  register int i;

  lseek (outdesc, output_strs_offset + output_strs_size, 0);

  if (!outstream)
    outstream = fdopen (outdesc, "w");

  for (i = 0; i < strtab_index; i++)
    {
      fwrite (strtab_vector[i], 1, strtab_lens[i], outstream);
      output_strs_size += strtab_lens[i];
    }

  fflush (outstream);

  /* Report I/O error such as disk full.  */
  if (ferror (outstream))
    perror_name (output_filename);
}

/* Write the symbol table and string table of the output file.  */

void
write_syms ()
{
  /* Number of symbols written so far.  */
  int syms_written = 0;
  register int i;
  register symbol *sp;

  /* Buffer big enough for all the global symbols.  One
     extra struct for each indirect symbol to hold the extra reference
     following. */
  struct nlist *buf
#ifndef NeXT
    = (struct nlist *) alloca ((defined_global_sym_count
				+ undefined_global_sym_count
				+ global_indirect_count)
			       * sizeof (struct nlist));
#else
    = (struct nlist *) alloca ((defined_global_sym_count
				+ undefined_global_sym_count)
			       * sizeof (struct nlist));
#endif
  /* Pointer for storing into BUF.  */
  register struct nlist *bufp = buf;

  /* Size of string table includes the bytes that store the size.  */
  strtab_size = sizeof strtab_size;

  output_syms_size = 0;
  output_strs_size = strtab_size;

  if (strip_symbols == STRIP_ALL)
    return;

  /* Write the local symbols defined by the various files.  */

  each_file (write_file_syms, &syms_written);
  file_close ();

  /* Now write out the global symbols.  */

  /* Allocate two vectors that record the data to generate the string
     table from the global symbols written so far.  This must include
     extra space for the references following indirect outputs. */

  strtab_vector = (char **) alloca ((num_hash_tab_syms
				     + global_indirect_count) * sizeof (char *));
  strtab_lens = (int *) alloca ((num_hash_tab_syms
				 + global_indirect_count) * sizeof (int));
  strtab_index = 0;

  /* Scan the symbol hash table, bucket by bucket.  */

  for (i = 0; i < TABSIZE; i++)
    for (sp = symtab[i]; sp; sp = sp->link)
      {
	struct nlist nl;

#ifdef N_SECT
	nl.n_sect = 0;
#else
	nl.n_other = 0;
#endif
	nl.n_desc = 0;

	/* Compute a `struct nlist' for the symbol.  */

	if (sp->defined || sp->referenced)
	  {
	    /* common condition needs to be before undefined condition */
	    /* because unallocated commons are set undefined in */
	    /* digest_symbols */
	    if (sp->defined > 1) /* defined with known type */
	      {
		/* If the target of an indirect symbol has been
		   defined and we are outputting an executable,
		   resolve the indirection; it's no longer needed */
		if (output_style != OUTPUT_RELOCATABLE
		    && ((sp->defined & ~N_EXT) == N_INDR)
		    && (((symbol *) sp->value)->defined > 1))
		  {
		    symbol *newsp = (symbol *) sp->value;
		    nl.n_type = newsp->defined;
		    nl.n_value = newsp->value;
		  }
		else
		  {
		    nl.n_type = sp->defined;
		    if (sp->defined != (N_INDR | N_EXT))
		      nl.n_value = sp->value;
		    else
		      nl.n_value = 0;
		  }
	      }
	    else if (sp->max_common_size) /* defined as common but not allocated. */
	      {
		/* happens only with -r and not -d */
		/* write out a common definition */
		nl.n_type = N_UNDF | N_EXT;
		nl.n_value = sp->max_common_size;
	      }
	    else if (!sp->defined)	      /* undefined -- legit only if -r */
	      {
		nl.n_type = N_UNDF | N_EXT;
		nl.n_value = 0;
	      }
	    else
	      fatal ("internal error: %s defined in mysterious way", sp->name);

	    /* Allocate string table space for the symbol name.  */

	    nl.n_un.n_strx = assign_string_table_index (sp->name);

	    /* Output to the buffer and count it.  */

	    *bufp++ = nl;
	    syms_written++;
	    if (nl.n_type == (N_INDR | N_EXT))
#ifndef NeXT
	      {
		struct nlist xtra_ref;
		xtra_ref.n_type = N_EXT | N_UNDF;
		xtra_ref.n_un.n_strx
		  = assign_string_table_index (((symbol *) sp->value)->name);
#ifdef N_SECT
		xtra_ref.n_sect = 0;
#else
		xtra_ref.n_other = 0;
#endif
		xtra_ref.n_desc = 0;
		xtra_ref.n_value = 0;
		*bufp++ = xtra_ref;
		syms_written++;
	      }
#else
	    nl.n_value = assign_string_table_index (((symbol *) sp->value)->name);
#endif
	  }
      }

#ifdef MACH_O
  if (output_file_type == IS_MACH_O)
    generate_mach_o_symbols(buf, bufp - buf);
#endif

  /* Output the buffer full of `struct nlist's.  */

  lseek (outdesc, output_syms_offset + output_syms_size, 0);
  mywrite (buf, sizeof (struct nlist), bufp - buf, outdesc);
  output_syms_size += sizeof (struct nlist) * (bufp - buf);

  if (syms_written != nsyms)
    fatal ("internal error: wrong number of symbols written into output file", 0);

  /* Now the total string table size is known, so write it into the
     first word of the string table.  */

  lseek (outdesc, output_strs_offset, 0);
  mywrite (&strtab_size, sizeof (int), 1, outdesc);

  /* Write the strings for the global symbols.  */

  write_string_table ();
}

/* Write the local and debugger symbols of file ENTRY.
   Increment *SYMS_WRITTEN_ADDR for each symbol that is written.  */

/* Note that we do not combine identical names of local symbols.
   dbx or gdb would be confused if we did that.  */

void
write_file_syms (entry, syms_written_addr)
     struct file_entry *entry;
     int *syms_written_addr;
{
  register struct nlist *p = entry->symbols;
  register struct nlist *end = p + entry->syms_size / sizeof (struct nlist);

  /* Buffer to accumulate all the syms before writing them.
     It has one extra slot for the local symbol we generate here.  */
  struct nlist *buf
    = (struct nlist *) alloca (entry->syms_size + sizeof (struct nlist));
  register struct nlist *bufp = buf;

  /* Upper bound on number of syms to be written here.  */
  int max_syms = (entry->syms_size / sizeof (struct nlist)) + 1;

  /* Make tables that record, for each symbol, its name and its name's length.
     The elements are filled in by `assign_string_table_index'.  */

  strtab_vector = (char **) alloca (max_syms * sizeof (char *));
  strtab_lens = (int *) alloca (max_syms * sizeof (int));
  strtab_index = 0;

  /* Generate a local symbol for the start of this file's text.  */

#ifndef OFILE_FN_FLAGGED
  if (discard_locals != DISCARD_ALL)
#endif
    {
      struct nlist nl;

#ifndef OFILE_FN_FLAGGED
      nl.n_type = N_TEXT;
#else
      nl.n_type = N_FN | N_EXT;
#endif
      nl.n_un.n_strx = assign_string_table_index (entry->local_sym_name);
      nl.n_value = entry->text_start_address;
      nl.n_desc = 0;
#ifdef N_SECT
      nl.n_sect = 0;
#else
      nl.n_other = 0;
#endif
      *bufp++ = nl;
      (*syms_written_addr)++;
      entry->local_syms_offset = *syms_written_addr * sizeof (struct nlist);
    }

  /* Read the file's string table.  */

  entry->strings = (char *) alloca (entry->strs_size);
  read_entry_strings (file_open (entry), entry);

  for (; p < end; p++)
    {
      register int type = p->n_type;
      register int write = 0;

      /* WRITE gets 1 for a non-global symbol that should be written.  */


      if (SET_ELEMENT_P (type))	/* This occurs even if global.  These */
				/* types of symbols are never written */
				/* globally, though they are stored */
				/* globally.  */
        write = output_style == OUTPUT_RELOCATABLE;
      else if (!(type & (N_STAB | N_EXT)))
        /* ordinary local symbol */
	write = ((discard_locals != DISCARD_ALL)
		 && !(discard_locals == DISCARD_L &&
		      (p->n_un.n_strx + entry->strings)[0] == LPREFIX)
		 && type != N_WARNING);
      else if (!(type & N_EXT))
	/* debugger symbol */
        write = (strip_symbols == STRIP_NONE);

      if (write)
	{
	  /* If this symbol has a name,
	     allocate space for it in the output string table.  */

	  if (p->n_un.n_strx)
	    p->n_un.n_strx = assign_string_table_index (p->n_un.n_strx
							+ entry->strings);

	  /* Output this symbol to the buffer and count it.  */

	  *bufp++ = *p;
	  (*syms_written_addr)++;
	}
    }

#ifdef MACH_O
  if (output_file_type == IS_MACH_O)
    generate_mach_o_symbols(buf, bufp - buf);
#endif

  /* All the symbols are now in BUF; write them.  */

  lseek (outdesc, output_syms_offset + output_syms_size, 0);
  mywrite (buf, sizeof (struct nlist), bufp - buf, outdesc);
  output_syms_size += sizeof (struct nlist) * (bufp - buf);

  /* Write the string-table data for the symbols just written,
     using the data in vectors `strtab_vector' and `strtab_lens'.  */

  write_string_table ();
  entry->strings = 0;		/* Since it will dissapear anyway.  */
}

/* Copy any GDB symbol segments from the input files to the output file.
   The contents of the symbol segment is copied without change
   except that we store some information into the beginning of it.  */

void write_file_symseg ();

void
write_symsegs ()
{
  lseek (outdesc, output_symseg_offset, 0);
  each_file (write_file_symseg, 0);
}

void
write_file_symseg (entry)
     struct file_entry *entry;
{
  char buffer[4096];
  struct symbol_root root;
  int indesc, len, total;

  if (entry->symseg_size == 0)
    return;

  output_symseg_size += entry->symseg_size;

  /* This entry has a symbol segment.  Read the root of the segment.  */

  indesc = file_open (entry);
  lseek (indesc, entry->symseg_offset + entry->starting_offset, 0);
  if (sizeof root != read (indesc, &root, sizeof root))
    fatal_with_file ("premature end of file in symbol segment of ", entry);

  /* Store some relocation info into the root.  */

  root.ldsymoff = entry->local_syms_offset;
  root.textrel = entry->text_start_address - entry->orig_text_address;
  root.datarel = entry->data_start_address - entry->orig_data_address;
  root.bssrel = entry->bss_start_address - entry->orig_bss_address;
  root.databeg = entry->data_start_address - root.datarel;
  root.bssbeg = entry->bss_start_address - root.bssrel;

  /* Write the modified root into the output file.  */

  mywrite (&root, sizeof root, 1, outdesc);

  /* Copy the rest of the symbol segment unchanged.  */

  total = entry->symseg_size - sizeof root;

  while (total > 0)
    {
      len = read (indesc, buffer, min (sizeof buffer, total));

      if (len != min (sizeof buffer, total))
	fatal_with_file ("premature end of file in symbol segment of ", entry);
      total -= len;
      mywrite (buffer, len, 1, outdesc);
    }

  file_close ();
}

/* Define a special symbol (etext, edata, or end).  NAME is the
   name of the symbol, with a leading underscore (whether or not this
   system uses such underscores).  TYPE is its type (e.g. N_DATA | N_EXT).
   Store a symbol * for the symbol in *SYM if SYM is non-NULL.  */
static void
symbol_define (name, type, sym)
     /* const */ char *name;
     int type;
     symbol **sym;
{
  symbol *thesym;

#if defined(nounderscore)
  /* Skip the leading underscore.  */
  name++;
#endif

  thesym = getsym (name);
  if (thesym->defined)
    {
      /* The symbol is defined in some input file.  Don't mess with it.  */
      if (sym)
	*sym = 0;
    }
  else
    {
      if (thesym->referenced)
	/* The symbol was not defined, and we are defining it now.  */
	undefined_global_sym_count--;
      thesym->defined = type;
      thesym->referenced = 1;
      if (sym)
	*sym = thesym;
    }
}

/* Create the symbol table entries for `etext', `edata' and `end'.  */

void
symtab_init ()
{
  symbol_define ("_edata", N_DATA | N_EXT, &edata_symbol);
  symbol_define ("_etext", N_TEXT | N_EXT, &etext_symbol);
  symbol_define ("_end", N_BSS | N_EXT, &end_symbol);

  /* Either _edata or __edata (C names) is OK as far as ANSI is concerned
     (see section 4.1.2.1).  In general, it is best to use __foo and
     not worry about the confusing rules for the _foo namespace.
     But HPUX 7.0 uses _edata, so we might as weel be consistent.  */
  symbol_define ("__edata", N_DATA | N_EXT, &edata_symbol_alt);
  symbol_define ("__etext", N_TEXT | N_EXT, &etext_symbol_alt);
  symbol_define ("__end", N_BSS | N_EXT, &end_symbol_alt);

#ifdef sun
  {
    symbol *dynamic_symbol;
    symbol_define ("__DYNAMIC", N_ABS | N_EXT, &dynamic_symbol);
    if (dynamic_symbol)
      dynamic_symbol->value = 0;
  }
#endif
#ifdef sequent
  {
    symbol *i387_flt_symbol;
    symbol_define ("_387_flt", N_ABS | N_EXT, &i387_flt_symbol);
    if (i387_flt_symbol)
      i387_flt_symbol->value = 0;
  }
#endif
#ifdef NeXT
  {
    symbol *shlib_init_symbol;
    symbol_define ("__shared_library_initialization", N_UNDF | N_EXT, &shlib_init_symbol);
    if (shlib_init_symbol)
      shlib_init_symbol->max_common_size = sizeof (long int);
  }
#endif
}

/* Compute the hash code for symbol name KEY.  */

int
hash_string (key)
     char *key;
{
  register char *cp;
  register int k;

  cp = key;
  k = 0;
  while (*cp)
    k = (((k << 1) + (k >> 14)) ^ (*cp++)) & 0x3fff;

  return k;
}

/* Get the symbol table entry for the global symbol named KEY.
   Create one if there is none.  */

symbol *
getsym (key)
     char *key;
{
  register int hashval;
  register symbol *bp;

  /* Determine the proper bucket.  */

  hashval = hash_string (key) % TABSIZE;

  /* Search the bucket.  */

  for (bp = symtab[hashval]; bp; bp = bp->link)
    if (! strcmp (key, bp->name))
      return bp;

  /* Nothing was found; create a new symbol table entry.  */

  bp = (symbol *) xmalloc (sizeof (symbol));
  bp->refs = 0;
  bp->name = (char *) xmalloc (strlen (key) + 1);
  strcpy (bp->name, key);
  bp->defined = 0;
  bp->referenced = 0;
  bp->trace = 0;
  bp->value = 0;
  bp->max_common_size = 0;
  bp->warning = 0;
  bp->undef_refs = 0;
  bp->multiply_defined = 0;
  bp->last_library_ref = 0;

  /* Add the entry to the bucket.  */

  bp->link = symtab[hashval];
  symtab[hashval] = bp;

  ++num_hash_tab_syms;

  return bp;
}

/* Like `getsym' but return 0 if the symbol is not already known.  */

symbol *
getsym_soft (key)
     char *key;
{
  register int hashval;
  register symbol *bp;

  /* Determine which bucket.  */

  hashval = hash_string (key) % TABSIZE;

  /* Search the bucket.  */

  for (bp = symtab[hashval]; bp; bp = bp->link)
    if (! strcmp (key, bp->name))
      return bp;

  return 0;
}

/* Report a usage error.
   Like fatal except prints a usage summary.  */

void
usage (string, arg)
     char *string, *arg;
{
  if (string)
    {
      fprintf (stderr, "%s: ", progname);
      fprintf (stderr, string, arg);
      fprintf (stderr, "\n");
    }
  fprintf (stderr, "\
Usage: %s [-d] [-dc] [-dp] [-e symbol] [-l lib] [-n] [-noinhibit-exec]\n\
       [-nostdlib] [-o file] [-r] [-s] [-t] [-u symbol] [-x] [-y symbol]\n\
       [-z] [-A file] [-Bstatic] [-D size] [-L libdir] [-M] [-N]\n\
       [-S] [-T[{text,data}] addr] [-V prefix] [-X] [file...]\n",
	   progname);
  exit (1);
}

/* Report a fatal error.
   STRING is a printf format string and ARG is one arg for it.  */

void
fatal (string, arg)
     char *string, *arg;
{
  fprintf (stderr, "%s: ", progname);
  fprintf (stderr, string, arg);
  fprintf (stderr, "\n");
  exit (1);
}

/* Report a fatal error.  The error message is STRING
   followed by the filename of ENTRY.  */

void
fatal_with_file (string, entry)
     char *string;
     struct file_entry *entry;
{
  fprintf (stderr, "%s: ", progname);
  fprintf (stderr, string);
  print_file_name (entry, stderr);
  fprintf (stderr, "\n");
  exit (1);
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
  fatal (s, name);
}

/* Report a fatal error using the message for the last failed system call,
   followed by the name of file ENTRY.  */

void
perror_file (entry)
     struct file_entry *entry;
{
#if !defined(linux) || defined(NON_NATIVE)
  extern int errno, sys_nerr;
  extern char *sys_errlist[];
#endif
  char *s;

  if (errno < sys_nerr)
    s = concat ("", sys_errlist[errno], " for ");
  else
    s = "cannot open ";
  fatal_with_file (s, entry);
}

/* Report a nonfatal error.
   STRING is a format for printf, and ARG1 ... ARG3 are args for it.  */

void
error (string, arg1, arg2, arg3)
     char *string, *arg1, *arg2, *arg3;
{
  fprintf (stderr, "%s: ", progname);
  fprintf (stderr, string, arg1, arg2, arg3);
  fprintf (stderr, "\n");
}


/* Output COUNT*ELTSIZE bytes of data at BUF
   to the descriptor DESC.  */

void
mywrite (buf, count, eltsize, desc)
     char *buf;
     int count;
     int eltsize;
     int desc;
{
  register int val;
  register int bytes = count * eltsize;

  while (bytes > 0)
    {
      val = write (desc, buf, bytes);
      if (val <= 0)
	perror_name (output_filename);
      buf += val;
      bytes -= val;
    }
}

/* Output PADDING zero-bytes to descriptor OUTDESC.
   PADDING may be negative; in that case, do nothing.  */

void
padfile (padding, outdesc)
     int padding;
     int outdesc;
{
  register char *buf;
  if (padding <= 0)
    return;

  buf = (char *) alloca (padding);
  bzero (buf, padding);
  mywrite (buf, padding, 1, outdesc);
}

/* Return a newly-allocated string
   whose contents concatenate the strings S1, S2, S3.  */

char *
concat (s1, s2, s3)
     char *s1, *s2, *s3;
{
  register int len1 = strlen (s1), len2 = strlen (s2), len3 = strlen (s3);
  register char *result = (char *) xmalloc (len1 + len2 + len3 + 1);

  strcpy (result, s1);
  strcpy (result + len1, s2);
  strcpy (result + len1 + len2, s3);
  result[len1 + len2 + len3] = 0;

  return result;
}

/* Parse the string ARG using scanf format FORMAT, and return the result.
   If it does not parse, report fatal error
   generating the error message using format string ERROR and ARG as arg.  */

int
parse (arg, format, error)
     char *arg, *format;
{
  int x;
  if (1 != sscanf (arg, format, &x))
    fatal (error, arg);
  return x;
}

/* Like malloc but get fatal error if memory is exhausted.  */

char *
xmalloc (size)
     int size;
{
  register char *result;
  if (!(result = malloc (size)))
    fatal ("virtual memory exhausted", 0);
  return result;
}

/* Like realloc but get fatal error if memory is exhausted.  */

char *
xrealloc (ptr, size)
     char *ptr;
     int size;
{
  register char *result;
  if (!(result = realloc (ptr, size)))
    fatal ("virtual memory exhausted", 0);
  return result;
}


#if defined(USG) || defined (OLD_LINUX)

void
bzero (p, n)
     char *p;
{
  memset (p, 0, n);
}

void
bcopy (from, to, n)
     char *from, *to;
{
  memcpy (to, from, n);
}

getpagesize ()
{
  return (4096);
}

#endif
#ifdef ns32000
/* code by Ian Dall for the ns32k */
/* Just put out the twos complement value with arbitrary alignment could
 * just do an assignment if we know we are on a little endian. */
void put_num(buf,val,n)
     char	*buf;
     long	val;
     char       n;
{ 
  for (; n > 0; n--)
    {
      *buf++ = val & 0xff; val >>= 8;
    }
}

int get_num(buf, n)
     char *buf;
     int n;
{
  int val = 0;
  buf += (n - 1);
  for (; n > 0; n--)
    {
      val = val * 256 + (*buf-- & 0xff);
    }
  return val;
}

/* Immediate operands are bigendian */
void put_imm(buf,val,n)
     char	*buf;
     long	val;
     char       n;
{ 
  int i;
  buf += (n - 1);
  for (i = n - 1; i >= 0; i--)
    {
      *buf-- = (val & 0xff); val >>= 8;
    }
}

int get_imm(buf, n)
     char *buf;
     int n;
{
  int val = 0;
  for (; n > 0; n--)
    {
      val = (val * 256) + (*buf++ & 0xff);
    }
  return val;
}

/* This converts an integer "val" to a displacement. The reason for its'
   existence is the fact that ns32k uses Huffman coded displacements.
   This implies that the bit order is reversed in displacements and
   that they are prefixed with a size-tag.

   binary: msb -> lsb	0xxxxxxx				byte
   			10xxxxxx xxxxxxxx			word
   			11xxxxxx xxxxxxxx xxxxxxxx xxxxxxxx	double word
          
   This must be taken care of and we do it here! 		  
 */
void put_disp(buf,val,n)
     char	*buf;
     long	val;
     char       n;
{ 
  switch(n) {
  case 1:
    if (val < -64 || val > 63)
      fprintf(stderr,"Byte displacement %d, out of range.\n", val);
    val&=0x7f;
#ifdef SHOW_NUM
		printf("%x ",val & 0xff);
#endif
    *buf++=val;
    break;
  case 2:
    if (val < -8192 || val > 8191)
      fprintf(stderr,"Word displacement %d, out of range.\n", val);
    val&=0x3fff;
    val|=0x8000;
#ifdef SHOW_NUM
		printf("%x ",val>>8 & 0xff);
#endif
    *buf++=(val>>8);
#ifdef SHOW_NUM
		printf("%x ",val & 0xff);
#endif
    *buf++=val;
    break;
  case 4:
    if (val < -0x1f000000 || val >= 0x20000000)
    /* if (val < -0x20000000 || val >= 0x20000000) */
      fprintf(stderr,"Double word displacement %d, out of range\n", val);
    val|=0xc0000000;
#ifdef SHOW_NUM
		printf("%x ",val>>24 & 0xff);
#endif
    *buf++=(val>>24);
#ifdef SHOW_NUM
		printf("%x ",val>>16 & 0xff);
#endif
    *buf++=(val>>16);
#ifdef SHOW_NUM
		printf("%x ",val>>8 & 0xff);
#endif
    *buf++=(val>>8);
#ifdef SHOW_NUM
		printf("%x ",val & 0xff);
#endif
    *buf++=val;
    break;
  default:
    error("Internal logic error");
  }
}

int sign_extend (value, bits)
     int value, bits;
{
  value = value & ((1 << bits) - 1);
  return (value & (1 << (bits-1))
	  ? value | (~((1 << bits) - 1))
	  : value);
}

int get_disp (buffer, n)
     char *buffer;
     int n;
{
  int Ivalue;

  Ivalue = *buffer++ & 0xff;
  if (n == 0)
    if (Ivalue & 0x80)
      if (Ivalue & 0x40)
	n = 4;
      else
	n = 2;
    else
      n = 1;
  switch (n)
    {
    case 1:
      Ivalue = sign_extend (Ivalue, 7);
      break;
    case 2:
      Ivalue = sign_extend (Ivalue, 6);
      Ivalue = (Ivalue << 8) | (0xff & *buffer);
      break;
    case 4:
      Ivalue = sign_extend (Ivalue, 6);
      Ivalue = (Ivalue << 8) | (0xff & *buffer++);
      Ivalue = (Ivalue << 8) | (0xff & *buffer++);
      Ivalue = (Ivalue << 8) | (0xff & *buffer);
      break;
    default:
      fprintf(stderr, "get_disp: invalid argument\n");
    }
  return Ivalue;
}
#endif

#if defined(sun) && defined(sparc)
int
getpagesize ()
{
  return 8192;
}
#endif
