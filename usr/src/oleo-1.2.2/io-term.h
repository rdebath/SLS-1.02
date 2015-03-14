#ifndef IO_TERMH
#define IO_TERMH

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
/*  t. lord	Sat Aug  8 15:43:52 1992	*/


#include "global.h"

extern CELLREF mkrow, mkcol;
extern CELLREF setrow, setcol;
extern unsigned int how_many;

/* The fd's that are selected on in the interact loop. */
extern fd_set read_fd_set;
extern fd_set exception_fd_set;
extern fd_set read_pending_fd_set;
extern fd_set exception_pending_fd_set;


#ifdef __STDC__
extern int global_cmd (int);
extern void execute_cmd (char *);
extern int real_get_chr (void);
extern void read_mp_usr_fmt (char *);
extern void read_mp_options (char *);
extern void write_mp_options (FILE *);
extern void sort (int, int (*)(int, int), void (*)(int, int), void (*)(int, int));
extern void map_chr (int map);
extern void nicely_goto_window (int n);
void set_options (char *);
#else
extern int global_cmd ();
extern void execute_cmd ();
extern int real_get_chr ();
extern void read_mp_usr_fmt ();
extern void read_mp_options ();
extern void write_mp_options ();
extern void sort ();
extern void map_chr ();
extern void nicely_goto_window ();
void set_options ();

#endif

#endif
