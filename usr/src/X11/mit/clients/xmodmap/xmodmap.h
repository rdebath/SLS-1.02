/*
 * xmodmap - program for loading keymap definitions into server
 *
 * $XConsortium: xmodmap.h,v 1.7 91/07/17 22:26:31 rws Exp $
 *
 * Copyright 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:  Jim Fulton, MIT X Consortium
 */

extern char *ProgramName;
extern Display *dpy;
extern int min_keycode, max_keycode;
extern Bool verbose;
extern Bool dontExecute;
extern char *inputFilename;
extern int lineno;
extern int parse_errors;

extern void initialize_map ();
extern void process_file ();
extern void process_line ();
extern void handle_line ();
extern void print_opcode ();
extern void print_work_queue ();
extern int execute_work_queue ();
extern void print_modifier_map ();
extern void print_key_table ();
extern void print_pointer_map ();
