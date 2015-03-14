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

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>

#include "tv.h"

struct timeval *
tv_current(tp)
struct timeval *tp;
{
    static struct timeval tpstatic;

    if (tp == NULL)
      tp = &tpstatic;

    gettimeofday(tp, NULL);

    return (tp);
}

struct timeval *
tv_period_to_tv(period, tp)
double period;
struct timeval *tp;
{
    static struct timeval tpstatic;

    if (tp == NULL)
      tp = &tpstatic;

    tp->tv_sec = (int)period;
    tp->tv_usec = (int)((period - (double)tp->tv_sec)*1000000.0);

    return (tp);
}

struct timeval *
tv_add(tp1, tp2)
struct timeval *tp1, *tp2;
{
    tp1->tv_usec += tp2->tv_usec;

    if (tp1->tv_usec > 1000000) {
	tp1->tv_usec -= 1000000;
	tp1->tv_sec += 1;
    }

    tp1->tv_sec += tp2->tv_sec;

    return (tp1);
}

struct timeval *
tv_subtract(tp1, tp2)
struct timeval *tp1, *tp2;
{
    tp1->tv_sec -= tp2->tv_sec;

    tp1->tv_usec -= tp2->tv_usec;

    if (tp1->tv_usec < 0) {
	tp1->tv_usec += 1000000;
	tp1->tv_sec -= 1;
    }

    return (tp1);
}

struct timeval *
tv_zero(tp)
struct timeval *tp;
{
    tp->tv_sec = 0;
    tp->tv_usec = 0;

    return (tp);
}

int
tv_iszero(tp)
struct timeval *tp;
{
    return (tp->tv_sec == 0 && tp->tv_usec == 0);
}

int
tv_cmp(tp1, tp2)
struct timeval *tp1, *tp2;
{
    if (tp1->tv_sec > tp2->tv_sec)
      return (1);

    if (tp1->tv_sec < tp2->tv_sec)
      return (-1);

    if (tp1->tv_usec > tp2->tv_usec)
      return (1);

    if (tp1->tv_usec < tp2->tv_usec)
      return (-1);

    return (0);
}
