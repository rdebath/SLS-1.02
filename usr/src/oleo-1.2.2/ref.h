#ifndef REFH
#define REFH

/*	Copyright (C) 1992, 1993 Free Software Foundation, Inc.

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
/*  t. lord	Sun Aug  9 20:45:14 1992	*/

#include "global.h"

#ifdef __STDC__
extern void add_var_ref (void *);
extern int eval_next_cell (void);
extern struct var *find_var (char *, int);
extern void push_cell (CELLREF, CELLREF);
extern void push_refs (struct ref_fm *);
extern struct var *find_or_make_var (char *, int);
extern void add_range_ref (struct rng *);
extern void flush_old_value (void);
extern void add_ref_to (int);
extern char *set_new_value (CELLREF, CELLREF, int, union vals *);
extern char *new_value (CELLREF, CELLREF, char *);
extern void copy_cell (CELLREF, CELLREF, CELLREF, CELLREF);
extern void move_cell (CELLREF, CELLREF, CELLREF, CELLREF);
extern char *new_var_value (char *, int, char *);
extern char *read_new_value (CELLREF, CELLREF, char *, char *);
extern void flush_variables (void);
extern void flush_all_timers (void);
extern void deal_alarm (void);
#else
extern void add_var_ref ();
extern int eval_next_cell ();
extern struct var *find_var ();
extern void push_cell ();
extern void push_refs ();
extern struct var *find_or_make_var ();
extern void add_range_ref ();
extern void flush_old_value ();
extern void add_ref_to ();
extern char *set_new_value ();
extern char *new_value ();
extern void copy_cell ();
extern void move_cell ();
extern char *new_var_value ();
extern char *read_new_value ();
extern void flush_variables ();
extern void flush_all_timers ();
extern void deal_alarm ();
#endif

extern timer_active;


#endif
