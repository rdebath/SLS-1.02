#ifndef LINEH
#define LINEH
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
/*  t. lord	Fri Aug  7 22:23:59 1992	*/

#define LINE_MIN 28

struct line
{
  int alloc;
  char *buf;
};

#ifdef __STDC__
extern void set_line (struct line *, char *);
extern void sprint_line (struct line *, char *,...);
#else
extern void set_line ();
extern void sprint_line ();
#endif
#endif
