#ifndef IO_ABSTRACTH
#define IO_ABSTRACTH
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
along with Oleo; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */
/*  t. lord	Fri Aug  7 12:48:16 1992	*/

#ifdef DEFINE_IO_VARS
#define EXTERN
#else
#define EXTERN extern
#endif

#include "global.h"
#include "line.h"
#include "window.h"

#ifndef VOLATILE
#ifdef __STDC__
#define VOLATILE volatile
#else
#define VOLATILE
#endif
#endif

#ifdef __STDC__
EXTERN void (*io_open_display) (void);
EXTERN void (*io_redisp) (void);	/* Refresh the existing image. */
EXTERN void (*io_repaint) (void);	/* $$$ Recompute the image. */
EXTERN void (*io_repaint_win) (struct window *);
EXTERN void (*io_close_display) (void);

/*
 * Low level input.
 */
EXTERN int (*io_input_avail) (void);/* How many chars can be read? */
EXTERN void (*io_scan_for_input) (int ok_to_block);
EXTERN void (*io_wait_for_input) (void);
EXTERN int (*io_read_kbd) (VOLATILE char *buf, int size);

/* A curses compatible interface.  These have no effect on low level
   calls except to consume characters which io_read_kbd might
   otherwise return. */
EXTERN void (*io_nodelay) (int);
EXTERN int (*io_getch) (void);

/* High level i/o. */

EXTERN void (*io_flush) (void); 

/* Input and status areas: */
EXTERN void (*io_clear_input_before) (void);
EXTERN void (*io_clear_input_after) (void);
EXTERN void (*io_update_status) (void);
EXTERN void (*io_info_msg) (char *,...);
EXTERN void (*io_error_msg) (char *,...);
EXTERN int (*io_get_chr) (char *prompt);
EXTERN int (*io_get_line) (char *prompt, struct line * line);

/* Multi-line txt messages. */
EXTERN void (*io_text_start) (void);
EXTERN void (*io_text_line) (char *,...);
EXTERN void (*io_text_finish) (void);

/* Cell values */
EXTERN void (*io_hide_cell_cursor) (void);
EXTERN void (*io_display_cell_cursor) (void);
EXTERN void (*io_pr_cell_win) (struct window *,
			       CELLREF, CELLREF,
			       CELL *);

/* The terminal's cursor may be in the current cell or the input area. */
EXTERN void (*io_cellize_cursor) (void);
EXTERN void (*io_inputize_cursor) (void);
#else
EXTERN void (*io_open_display) ();
EXTERN void (*io_redisp) ();	/* Refresh the existing image. */
EXTERN void (*io_repaint) ();	/* $$$ Recompute the image. */
EXTERN void (*io_repaint_win) ();
EXTERN void (*io_close_display) ();

/*
 * Low level input.
 */
EXTERN int (*io_input_avail) ();/* How many chars can be read? */
EXTERN void (*io_scan_for_input) ();
EXTERN void (*io_wait_for_input) ();
EXTERN int (*io_read_kbd) ();

/* A curses compatible interface.  These have no effect on low level
   calls except to consume characters which io_read_kbd might
   otherwise return. */
EXTERN void (*io_nodelay) ();
EXTERN int (*io_getch) ();

/* High level i/o. */

EXTERN void (*io_flush) (); 

/* Input and status areas: */
EXTERN void (*io_clear_input_before) ();
EXTERN void (*io_clear_input_after) ();
EXTERN void (*io_update_status) ();
EXTERN void (*io_info_msg) ();
EXTERN void (*io_error_msg) ();
EXTERN int (*io_get_chr) ();
EXTERN int (*io_get_line) ();

/* Multi-line txt messages. */
EXTERN void (*io_text_start) ();
EXTERN void (*io_text_line) ();
EXTERN void (*io_text_finish) ();

/* Cell values */
EXTERN void (*io_hide_cell_cursor) ();
EXTERN void (*io_display_cell_cursor) ();
EXTERN void (*io_pr_cell_win) ();

/* The terminal's cursor may be in the current cell or the input area. */
EXTERN void (*io_cellize_cursor) ();
EXTERN void (*io_inputize_cursor) ();
#endif


#define IO_SETUP						\
  io_open_display = _io_open_display;				\
  io_redisp = _io_redisp;					\
  io_repaint = _io_repaint;					\
  io_repaint_win = _io_repaint_win;				\
  io_close_display = _io_close_display;				\
  io_input_avail = _io_input_avail; 				\
  io_scan_for_input = _io_scan_for_input; 			\
  io_wait_for_input = _io_wait_for_input; 			\
  io_read_kbd = _io_read_kbd; 					\
  io_nodelay = _io_nodelay; 					\
  io_getch = _io_getch; 					\
  io_get_chr = _io_get_chr; 					\
  io_get_line = _io_get_line; 					\
  io_update_status = _io_update_status; 		      	\
  io_info_msg = _io_info_msg; 					\
  io_error_msg = _io_error_msg; 				\
  io_text_start = _io_text_start; 				\
  io_text_line = _io_text_line; 				\
  io_text_finish = _io_text_finish; 				\
  io_flush = _io_flush;						\
  io_clear_input_before = _io_clear_input_before;		\
  io_clear_input_after = _io_clear_input_after; 		\
  io_pr_cell_win = _io_pr_cell_win;				\
  io_hide_cell_cursor = _io_hide_cell_cursor;			\
  io_cellize_cursor = _io_cellize_cursor;			\
  io_inputize_cursor = _io_inputize_cursor;			\
  io_display_cell_cursor = _io_display_cell_cursor


#endif
