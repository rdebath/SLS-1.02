#ifndef SCH
#define SCH

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
/*  t. lord	Sun Aug  9 20:39:47 1992	*/

#include "global.h"

#ifdef __STDC__
extern void sc_read_file (FILE *,int);
extern void sc_write_file (FILE *,struct rng *);
extern  int sc_set_options (int, char *);
extern void sc_show_options (void);
#else
extern void sc_read_file ();
extern void sc_write_file ();
extern  int sc_set_options ();
extern void sc_show_options ();
#endif

#endif

