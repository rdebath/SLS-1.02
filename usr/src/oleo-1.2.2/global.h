#ifndef GLOBALH
#define GLOBALH

/*	Copyright (C) 1990, 1992, 1993 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "sysdef.h"
#include "utils.h"


#define RCFILE ".oleorc"

/* The most important compile-time constant.  How many bits do we want to
   allocate for cell-references?  Useful values are 8 and 16 (at the moment)
   8 allows luser to access 255*255 cells (probably big enough)
   16 allows luser to access 65535*65535, which is more than will fit in
   the avaliable virtual memory on any 32-bit machine.
 */

#ifndef BITS_PER_CELLREF
#define BITS_PER_CELLREF 16
#endif

/* The location of a cell that can never be referenced */
#define NON_ROW		0
#define NON_COL		0

#define MIN_ROW		1
#define MIN_COL 	1

#if BITS_PER_CELLREF==16
typedef unsigned short CELLREF;
#define CELLREF_MASK 0xFFFF
#define MAX_ROW 65535
#define MAX_COL 65535

/* Do *not* assume that 'name' is aligned!  It probably isn't */
#define GET_ROW(name)		((((name)[0])<<8)|(name)[1])
#define GET_COL(name)		((((name)[2])<<8)|(name)[3])
#define PUT_ROW(name,val)	((name)[0]=((val)>>8)),((name)[1]=val)
#define PUT_COL(name,val)	((name)[2]=((val)>>8)),((name)[3]=val)
#define EXP_ADD			sizeof(CELLREF)*2
#else
#if BITS_PER_CELLREF==8
typedef unsigned char CELLREF;
#define CELLREF_MASK 0xFF
#define MAX_ROW	      255
#define MAX_COL       255

#define GET_ROW(name)		((name)[0])
#define GET_COL(name)		((name)[1])
#define PUT_ROW(name,val)	((name)[0]=(val))
#define PUT_COL(name,val)	((name)[1]=(val))
#define EXP_ADD			sizeof(CELLREF)*2
#else
FOO FOO FOO You need to define the obvious macros above
#endif
#endif

/* Struct rng is used to describe a region of cells */
struct rng
{
  CELLREF lr, lc, hr, hc;
};

/* A ref_fm structure contains a list of all cells that reference some
 * value.  The value can be another cell or some global (such as the system
 * time).
 *
 * These structures are hash-consed and shared.  The hash-cons procedure
 * will re-use a particular structure if there is only one reference to it.
 */
struct ref_fm
{
  struct ref_fm *refs_next;
  unsigned short refs_refcnt;
  unsigned short refs_used;
  struct ref_array
    {
      CELLREF ref_row;
      CELLREF ref_col;
    } fm_refs[1];
};

/* refs_to is a vector of locations in a formula where the
 * cell references other cells, ranges, or variables 
 */
struct ref_to
{
  unsigned short refs_refcnt;
  struct ref_to *refs_next;
  unsigned short refs_used;
  unsigned char to_refs[1];
};

/* These macros are used to extract/store ranges in compiled formulas. */
#define GET_RNG(name,putit)	bcopy((VOIDSTAR)(name),(VOIDSTAR)(putit),sizeof(struct rng))
#define PUT_RNG(name,putit)	bcopy((VOIDSTAR)(putit),(VOIDSTAR)(name),sizeof(struct rng))
#define EXP_ADD_RNG		sizeof(struct rng)

extern struct obstack tmp_mem;
extern VOIDSTAR tmp_mem_start;

#define ERR_MAX		17
extern char *ename[];
extern char tname[];
extern char fname[];
extern char iname[];
extern char mname[];
extern char nname[];

extern VOIDSTAR parse_hash;
extern double __plinf, __neinf, __nan;

extern CELLREF curow, cucol;
extern CELLREF cur_row, cur_col;

extern int default_jst;
extern int default_fmt;
extern int default_lock;

extern unsigned short current_cycle;
extern int a0;
extern int errno;
extern const char oleo_version_string[];

#ifdef __STDC__

extern double astof (char **);
extern long astol (char **);
extern void free (void *);
extern void panic (const char *, ...);

extern VOIDSTAR init_stack (void);
extern void flush_stack (void *);
extern void push_stack (void *, void *);
extern VOIDSTAR pop_stack (void *);
extern int size_stack (void *);

extern void add_ref (CELLREF, CELLREF);
extern void add_range_ref (struct rng *);
extern void add_timer_ref (int);
extern void add_ref_to (int);

struct hash_control; /* in case it hasn't been declared yet */
extern char *hash_insert (struct hash_control *, char *, VOIDSTAR);

extern char *jst_to_str (int);
extern char *fmt_to_str (int);

#ifdef A0_REFS
extern char *col_to_str (CELLREF);
#endif

extern char *flt_to_str (double);
extern void push_refs (struct ref_fm *);
extern void no_more_cells (void);

extern char *range_name (struct rng *);
extern char *cell_name (CELLREF, CELLREF);

extern char *new_value (CELLREF, CELLREF, char *);

extern unsigned char parse_cell_or_range (char **, struct rng *);

struct var; /* in case it hasn't been declared yet */
extern void for_all_vars (void (*)(char *, struct var *));
#else

extern double astof ();
extern long astol ();
extern void free ();
extern void panic ();

extern VOIDSTAR init_stack ();
extern void flush_stack ();
extern void push_stack ();
extern VOIDSTAR pop_stack ();
extern int size_stack ();

extern void add_ref ();
extern void add_range_ref ();
extern void add_timer_ref ();
extern void add_ref_to ();

extern char *hash_insert ();
extern char *jst_to_str ();
extern char *fmt_to_str ();

#ifdef A0_REFS
extern char *col_to_str ();
#endif

extern char *flt_to_str ();
extern void push_refs ();
extern void no_more_cells ();

extern char *range_name ();
extern char *cell_name ();

extern char *new_value ();

extern unsigned char parse_cell_or_range ();

extern void for_all_vars ();
#endif

#endif
