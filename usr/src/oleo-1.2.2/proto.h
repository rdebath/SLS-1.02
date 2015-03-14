#ifndef PROTOH
#define PROTOH
/*  t. lord	Thu Jan  2 11:31:13 1992	*/
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


#ifdef __STDC__
#define USE_PROTOS 1
#endif

#define TAIL_ARRAY 1

/*
 * Declare functions as:
 *   void foo P_((int x, float y));
 */

#if USE_PROTOS
#define	P_(X)			X
#else
#define P_(X)			()
#endif
#endif
