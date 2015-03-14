#ifndef BYTE_COMPILEH
#define BYTE_COMPILEH

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

/*  t. lord	Wed Oct 28 00:16:13 1992	*/

#ifdef __STDC__
extern unsigned char *parse_and_compile (char *);
extern void byte_free (unsigned char *);
extern int is_constant (unsigned char *);
#else
extern unsigned char *parse_and_compile ();
extern void byte_free ();
extern int is_constant ();
#endif

#endif
