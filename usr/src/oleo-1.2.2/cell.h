#ifndef CELLH
#define CELLH
/*	Copyright (C) 1990, 1992, 1993 Free Software Foundation, Inc.

This file is part of Oleo, the GNU Spreadsheet.

Oleo is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Oleo is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Oleo; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Various structures and stuff for the spreadsheet */

/* A union of possible values for a location in the spreadsheet
   (or a location to evaluate to:  This includes c_r, which
   a VAR, etc may evaluate to, but which no cell can ever contain */
#include "global.h"
#include "font.h"

union vals
  {
    double c_d;
    char *c_s;
    long c_l;
    int c_i;
    struct rng c_r;
  };

/* An actual cell structure.  These cannot be variable-length, since they are
   allocated as a variable-length array on a col structure. */

struct cell
  {
    /* char *cell_string; */
    short cell_flags;
    struct font_memo * cell_font;
    struct ref_fm *cell_refs_from;
    struct ref_to *cell_refs_to;
    unsigned char *cell_formula;
    unsigned short cell_cycle;
    union vals c_z;
  };

struct var
  {
    struct var *var_next;

    short var_flags;
    struct rng v_rng;

    /* This is a list of the cells that reference this variable.  If the variable
   changes, all the cells in the vars new range must be given ref_froms that
   point to these variables */
    struct ref_fm *var_ref_fm;
    /* A variable sized array that holds the var-name. */
    char var_name[1];
  };

typedef struct cell CELL;

#define VAR_UNDEF 1
#define VAR_CELL 2
#define VAR_RANGE 3

/* Shorthand for the cell union */
#define cell_flt	c_z.c_d
#define cell_str	c_z.c_s
#define cell_int	c_z.c_l
#define cell_bol	c_z.c_i
#define cell_err	c_z.c_i

/* cell_flags is a 16bit value.  These bits have the following values
15   14   13   12 . 11   10    9    8  . 7    6    5    4  . 3    2    1    0 .
 Unused | --Lock- | ----Type---- | Justify  | - Format --  | --- Precision ---
 */

#define GET_LCK(p)	((p)->cell_flags & 0x3000)
#define SET_LCK(p,x)	(((p)->cell_flags &= ~0x3000),(p)->cell_flags |= x)

#define LCK_DEF		(0x0000)
#define LCK_UNL		(0x1000)
#define LCK_LCK		(0x2000)

/* The type of a cell, or of a eval_expression() value */
#define GET_TYP(p)	((p)->cell_flags & 0x0E00)
#define SET_TYP(p,x)	((p)->cell_flags &= ~0x0E00,(p)->cell_flags|=(x))
#define TYP_FLT	0x0200
#define TYP_INT	0x0400
#define TYP_STR	0x0600
#define TYP_BOL	0x0800
#define TYP_ERR	0x0A00
/* This for the expression evaluator:  NO cell should be this type */
#define TYP_RNG 0x0E00

#define GET_JST(p)	((p)->cell_flags & 0x0180)
#define SET_JST(p,x)	((p)->cell_flags &= ~0x0180,(p)->cell_flags|=(x))
#define JST_DEF	0x0000
#define JST_LFT 0x0100
#define JST_RGT 0x0080
#define JST_CNT 0x0180

#define GET_FMT(p)	((p)->cell_flags & 0x007F)
#define SET_FMT(p,x)	((p)->cell_flags &= ~0x007F,(p)->cell_flags|=(x))
#define GET_PRC(p)	((p)&0x0F)
#define PRC_FLT	0x0F
#define FMT_DEF 0x0000

#define FMT_HID 0x000E
#define FMT_GPH 0x000F

#define FMT_DOL 0x001F
#define FMT_CMA 0x002F
#define FMT_PCT 0x003F
#define FMT_USR 0x004F

#define FMT_FXT 0x005F
#define FMT_EXP 0x006F
#define FMT_GEN 0x007F

#define FMT_MAX 0x007F

/* README README README
 *
 * The _make_ functions may cause the addresses of cells previously returned by
 * find_ functions to change.  By extention, any function that calls a make_
 * function can have that effect.  This is particularly nasty because pointers
 * to cells are stored in the global my_cell, and in various stack frames.
 * Several bugs have been traced to this idiotic design -- please be careful
 * not to add new ones.
 */

#ifdef __STDC__
extern CELL *find_cell (CELLREF, CELLREF);
extern CELL *find_or_make_cell (CELLREF, CELLREF);
extern void find_cells_in_range (struct rng *);
extern void make_cells_in_range (struct rng *);
extern CELL *next_cell_in_range (void);
extern CELL *next_row_col_in_range (CELLREF *, CELLREF *);
extern void no_more_cells (void);
extern char *decomp (CELLREF, CELLREF, CELL *);
extern void decomp_free (void);
#else
extern CELL *find_cell ();
extern CELL *find_or_make_cell ();
extern void find_cells_in_range ();
extern void make_cells_in_range ();
extern CELL *next_cell_in_range ();
extern CELL *next_row_col_in_range ();
extern void no_more_cells ();
extern char *decomp ();
extern void decomp_free ();
#endif

#endif
