/* Copyright (C) 1993  Hongjiu Lu
This file is part of the Linux C Library.

The Linux C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The Linux C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details. */

#include <errno.h>
#include <math.h>

double pow (x,y)  	
double x,y;
{
  if (x == 0.0) return 0.0;

  if (y == 0.0) return 1.0;

  if (y == 1.0) return x;

  if (x < 0.0) {
    long long tmp;
    int negative;

    tmp = (long long) y; 

    /* Even or odd */
    negative = tmp & 1;

    /* That is only valid if |y| < 2^64. */
    if (y != (double) tmp) {
       errno = EDOM;
       perror ("pow");
       return 0.0;
    }

    x = -x;
    /* x = y * log2(x) */
    __asm__ __volatile__ ("fyl2x"
    			  : "=t" (x) : "0" (x), "u" (y));
    /* x = 2 ^x */
    x = pow2 (x);

    return (negative) ? -x : x;
  }

  /* x = y * log2(x) */
  __asm__ __volatile__ ("fyl2x"
    			  : "=t" (x) : "0" (x), "u" (y));
  /* return 2 ^x */
  return pow2 (x);
}
