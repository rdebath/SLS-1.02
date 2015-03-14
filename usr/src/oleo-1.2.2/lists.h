#ifndef LISTSH
#define LISTSH

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
/*  t. lord	Sun Aug  9 20:38:55 1992	*/

#include "global.h"

#ifdef __STDC__
extern CELLREF highest_row (void);
extern CELLREF highest_col (void);
extern unsigned short next_width (CELLREF *);
extern unsigned short next_height (CELLREF *);
extern void list_read_file (FILE *, int);
extern void list_write_file (FILE *, struct rng *);
extern int list_set_options (int, char *);
extern void list_show_options (void);
extern int get_scaled_width (CELLREF c);
extern int get_scaled_height (CELLREF c);
extern int get_width (CELLREF c);
extern int get_height (CELLREF c);
extern int get_nodef_width (CELLREF c);
extern int get_nodef_height (CELLREF c);
extern void set_width (CELLREF, unsigned short);
extern void set_height (CELLREF, unsigned short);
extern void find_widths (CELLREF, CELLREF);
extern void find_heights (CELLREF, CELLREF);
extern void flush_everything (void);
extern void flush_all_timers (void);
extern CELLREF max_row (CELLREF);
extern CELLREF max_col (CELLREF);
extern void shift_widths (int, CELLREF, CELLREF);
extern void shift_outside (struct rng *, int, int);
#else
extern CELLREF highest_row ();
extern CELLREF highest_col ();
extern unsigned short next_width ();
extern unsigned short next_height ();
extern void list_read_file ();
extern void list_write_file ();
extern int list_set_options ();
extern void list_show_options ();
extern int get_scaled_width ();
extern int get_scaled_height ();
extern int get_width ();
extern int get_height ();
extern int get_nodef_width ();
extern int get_nodef_height ();
extern void set_width ();
extern void set_height ();
extern void find_widths ();
extern void find_heights ();
extern void flush_everything ();
extern void flush_all_timers ();
extern CELLREF max_row ();
extern CELLREF max_col ();
extern void shift_widths ();
extern void shift_outside ();
#endif

extern int height_scale;
extern int width_scale;

#endif
