
#ifndef WINDOWH
#define WINDOWH

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
/*  t. lord	Mon Aug 10 14:51:25 1992	*/

#include "cell.h"
#include "line.h"

/* The tty windows datastructures: */


struct window
{
  /* Do not change these directly. */
  int id;
  int win_over;			/* Where the data in this window starts */
  int win_down;			/*   on the screen.  */
  struct rng screen;		/* Cells visible. recenter_* updates this. */
  int flags;			/* You must use io_set_win_flags and perhaps */
				/*   io_recenter_cur_win */

  /* Number of lines of spreadsheet that can fit in this window.
     This only changes when the screen is resized,
     win->flags&WIN_EDGES changes, or a window is either
     created or destroyed */
  int numr;
  
  /* Number of text columns that can fit in this window.
     This changes when the screen is resized,
     win->flags&WIN_EDGES changes, a window is created or
     destoryed, or win->lh_wid changes.  In the last case
     win->numc+win->lh_wid remains a constant. */
  int numc;
  
  /* 
   * Number of columns and rows for right and bottom edges. 
   * As this changes, numc and numr change accordingly.
   */
  int bottom_edge_r;
  int right_edge_c;

  /* These values may be changed at any time. */
  /* -1 if this window isn't linked to any others, else
     contains the index into wins of the window this one is
     linked to */
  int link;

  /* Number of columns taken up by the row numbers at the
     left hand edge of the screen.  Zero if edges is
     win->flags&WIN_EDGES is off (by definition).  Seven (or
     five) if win->flags&WIN_PAG_HZ (to make things easier).
     Ranges between three "R9 " to seven "R32767 " depending on
     the number of the highest row on the screen.  */
  int lh_wid;

  
  /* Cursor row/column in this window */
  /* Note that the external variables curow, cucol are used for
     the currently active cursor position, so if you want
     cwin->curow and cwin->cucol to be accurate, you have to
     set them yourself. */
  CELLREF curow,cucol;

  VOIDSTAR *win_slops;	/* Slops in this window (tty only) */
};

struct mouse_event
{
  int seq;
  int row;
  int col;
  int button;
  int downp;
  int location;			/* See #defines, below. */
  CELLREF r;
  CELLREF c;
  struct mouse_event * next;
  struct mouse_event * prev;
};

/* Window flags:
   0x01	Locked horizontally
   0x02	Locked vertically
   0x04	Page Horizontally
   0x08	Page Vertically
   0x10	Edges disabled
   0x20	Edges standout
   */
#define WIN_LCK_HZ	0x01
#define WIN_LCK_VT	0x02
#define WIN_PAG_HZ	0x04
#define WIN_PAG_VT	0x08
#define WIN_EDGES	0x10
#define WIN_EDGE_REV	0x20

/* Do not change these directly. */
extern int scr_lines;
extern int scr_cols;
extern int input;		/* An approximation that makes sense. */
extern int status;
extern int user_input;		/* What the user actually asked for. */
extern int user_status;
extern int formulas_visible;
extern int input_rows;
extern int status_rows;
extern int label_rows;
extern int label_emcols;

extern int nwin;
extern struct window * wins;
extern struct window * cwin;

/* 
 * When the input area is active, it appears to be just another window,
 * reachable by other-window.  These values must be maintained by any
 * implementation of io_get_line.
 */
extern int window_after_input;	/* Id of the window prior to the input area. */
extern int input_active;	/* Bool: is the input area selected? */


#ifdef __STDC__
extern void io_read_window_config (char *);
extern void io_write_window_config (struct line *);
extern void io_init_windows (int sl, int sc, int ui, int us,
			     int ir, int sr, int lr, int lc);
extern void io_set_scr_size (int lines, int cols);
extern void io_set_input_status (int irows, int srows, int doredraw);
extern void io_set_formula_visability (int);
extern void io_set_input_rows (int);
extern void io_set_status_rows (int);

extern void io_win_open (int horizp, int where);
extern void io_win_close (struct window *);
extern void io_set_cwin (struct window *);
extern void io_move_cell_cursor (CELLREF, CELLREF);
extern void io_shift_cell_cursor (int dirn);
extern void io_scroll_cell_cursor (int dirn);
extern void io_pr_cell (CELLREF r, CELLREF c, CELL *);
extern void io_redo_region (struct rng * rng);
extern void io_recenter_cur_win (void);
extern void io_recenter_all_win (void);

extern int win_label_rows (struct window * win);
extern int win_label_cols (struct window * win, CELLREF last_row);

extern void io_set_win_flags (struct window *, int f);
extern int enqueue_mouse_event (int r, int c, int button, int downp);
extern void dequeue_mouse_event (struct mouse_event * out, int seq);

#else

extern void io_read_window_config ();
extern void io_write_window_config ();
extern void io_init_windows ();
extern void io_set_scr_size ();
extern void io_set_input_status ();
extern void io_set_formula_visability ();
extern void io_set_input_rows ();
extern void io_set_status_rows ();

extern void io_win_open ();
extern void io_win_close ();
extern void io_set_cwin ();
extern void io_move_cell_cursor ();
extern void io_shift_cell_cursor ();
extern void io_scroll_cell_cursor ();
extern void io_pr_cell ();
extern void io_redo_region ();

extern void io_recenter_cur_win ();
extern void io_recenter_all_win ();

extern int win_label_rows ();
extern int win_label_cols ();

extern void io_set_win_flags ();
extern int enqueue_mouse_event ();
extern void dequeue_mouse_event ();
#endif

extern struct mouse_event last_mouse_event;


/* This is stored as the button number when a dequeue failes. */
#define MOUSE_QERROR	-1
/* These are the possible mouse locations. */
#define MOUSE_ON_INPUT    -1
#define MOUSE_ON_STATUS   -2
#define MOUSE_ON_EDGE	  -3

#define MOUSE_CHAR '\034'

#endif


