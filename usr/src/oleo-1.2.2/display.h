#ifndef DISPLAYH
#define DISPLAYH

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
/*  t. lord	Sat Oct  3 16:08:21 1992	*/

#include "global.h"
#include "font.h"
#include "cell.h"
#include "ir.h"

union cell_numeric
{
  int integer;
  double dbl;
};

struct cell_display
{
  CELLREF r, c;
  int cell_type;
  int justification;
  struct font_memo *font;

  char *unclipped;
  struct xx_sIntRectangle goal;

  char *clipped;		/* Box of clipped string, if known. */
  struct xx_sIntRectangle clip;
  union cell_numeric numeric;	/* Value from which to compute clipped str. */


  struct xx_sIntRectangle layout;	/* Space available under current layout. */

  struct cell_display *used_by;
  struct cell_display *was_used_by;
  struct cell_display *next_damaged;
};

struct display;
/* This should set the w_goal and h_goal fields of the cell_display. */
#ifdef __STDC__
typedef void (*cell_display_metric) (struct cell_display *,
				     struct display *);
#else
typedef void (*cell_display_metric) ();
#endif

struct display
{
  struct rng range;
  int *widths;
  int *heights;
  int *rowy;
  int *colx;
  void *vdata;
  cell_display_metric metric;
  struct cell_display *cells;
  struct cell_display *damaged;	/* This is a list, terminated by a pointer */
  /* to this structure rather than 0, */
  /* following the next_damage chain. */
};

#ifdef __STDC__
extern void free_display (struct display *);
extern void build_display (struct display *, struct rng *,
			   cell_display_metric, void *);
extern void build_unscaled_display (struct display *, struct rng *,
				    cell_display_metric, void *);
extern void display_test_cmd (struct rng *);
extern struct cell_display *cell_display_of (struct display *dpy,
					     CELLREF r, CELLREF c);
extern void display_range (struct rng *, struct display *dpy,
			   int x, int y, int w, int h);
extern void record_display_damage (struct display *,
				   int x, int y, int w, int h);
extern void layout (struct display *);
extern int pr_display_cell (struct display *, CELLREF, CELLREF, CELL *);
#else
extern void free_display ();
extern void build_display ();
extern void build_unscaled_display ();
extern void display_test_cmd ();
extern struct cell_display *cell_display_of ();
extern void display_range ();
extern void record_display_damage ();
extern void layout ();
extern int pr_display_cell ();
#endif

#define display_width(DPY) \
     ((DPY)->colx[(DPY)->range.hc - (DPY)->range.lc] \
      + (DPY)->widths[(DPY)->range.hc - (DPY)->range.lc])
#define display_height(DPY) \
     ((DPY)->rowy[(DPY)->range.hr - (DPY)->range.lr] \
      + (DPY)->heights[(DPY)->range.hr - (DPY)->range.lr])
#define dpy_cols(DPY) ((DPY)->range.hc - (DPY)->range.lc)
#define dpy_aref(DPY,R,C) \
   ((DPY)->cells + (((R) - (DPY)->range.lr) * dpy_cols(DPY)) \
    + ((C) - (DPY)->range.lc))
#endif
