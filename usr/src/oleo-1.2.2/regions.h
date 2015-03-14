#ifndef REGIONSH
#define REGIONSH

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


/*  t. lord	Sat Aug  8 15:48:59 1992	*/

#include "global.h"


struct cmp
{
  char mult;
  CELLREF row;
  CELLREF col;
};

extern struct rng sort_rng;
extern struct rng sort_ele;
extern struct cmp *sort_keys;
extern int sort_keys_num;
extern int sort_keys_alloc;

#ifdef __STDC__
extern int cmp_cells (int, int);
extern void swp_cells (int, int);
extern void rot_cells (int, int);
extern void set_rng (struct rng *, CELLREF, CELLREF, CELLREF, CELLREF);
extern void lock_region (struct rng *, int);
extern void format_region (struct rng *, int, int);
extern void sort_region (void);
#else
extern int cmp_cells ();
extern void swp_cells ();
extern void rot_cells ();
extern void set_rng ();
extern void lock_region ();
extern void format_region ();
extern void sort_region ();
#endif


extern unsigned short print_width;
extern struct rng all_rng;

#endif
