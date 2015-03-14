/*
  This file is part of the NetFax system.

  (c) Copyright 1989 by David M. Siegel and Sundar Narasimhan.
      All rights reserved.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation.

    This program is distributed in the hope that it will be useful, 
    but WITHOUT ANY WARRANTY; without even the implied warranty of 
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifndef INCtimeh
#define INCtimeh 1

#include <sys/types.h>
#include <sys/time.h>

/*
  Prototypes:
*/

struct timeval *tv_current(
#ifdef _PROTO
    struct timeval *tp
#endif
);

struct timeval *tv_period_to_tv(
#ifdef _PROTO
    double period,
    struct timeval *tp
#endif
);

struct timeval *tv_add(
#ifdef _PROTO
    struct timeval *tp1,
    struct timeval *tp2
#endif
);

struct timeval *tv_subtract(
#ifdef _PROTO
    struct timeval *tp1,
    struct timeval *tp2
#endif
);

struct timeval *tv_zero(
#ifdef _PROTO
    struct timeval *tp
#endif
);

int tv_iszero(
#ifdef _PROTO
    struct timeval *tp
#endif
);

int tv_cmp(
#ifdef _PROTO
    struct timeval *tp1,
    struct timeval *tp2
#endif
);

#endif INCtimeh
