#ifndef IO_UTILSH
#define IO_UTILSH

/*	Copyright (C) 1992 Free Software Foundation, Inc.

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
/*  t. lord	Thu Oct 15 18:26:51 1992	*/

#include "global.h"
#include "cell.h"

#ifdef __STDC__
extern void init_infinity (void);
extern void clear_spreadsheet (void);
extern char *print_cell (CELL *);
extern char *cell_value_string (CELLREF r, CELLREF c);
extern char *col_to_str (CELLREF);
extern char *flt_to_str (double);
char *backslash_a_string (char *, int);
extern int usr_set_fmts (void);
extern char *cell_name (CELLREF, CELLREF);
extern char *range_name (struct rng *);
extern char *new_var_value (char *, int, char *);
extern char *read_new_value (CELLREF, CELLREF, char *, char *);
extern void set_usr_stats (int, char **);
extern void get_usr_stats (int, char **);
extern char *adjust_prc (char *, CELL *, int, int, int);
#else
extern void init_infinity ();
extern void clear_spreadsheet ();
extern char *print_cell ();
extern char *cell_value_string ();
extern char *col_to_str ();
extern char *flt_to_str ();
char *backslash_a_string ();
extern int usr_set_fmts ();
extern char *cell_name ();
extern char *range_name ();
extern char *new_var_value ();
extern char *read_new_value ();
extern void set_usr_stats ();
extern void get_usr_stats ();
extern char *adjust_prc ();
#endif

extern char numb_oflo[];
extern char *bname[];
extern char print_buf[];
#endif
