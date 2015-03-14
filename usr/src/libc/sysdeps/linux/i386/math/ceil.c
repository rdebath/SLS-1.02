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

#include <math.h>

double
ceil (double x)
{
  volatile short cw, cwtmp;

  __asm__ volatile ("fnstcw %0" : "=m" (cw) : );
  /* rounding up */
  cwtmp = (cw & 0xf3ff) | 0x0800;
  __asm__ volatile ("fldcw %0" : : "m" (cwtmp));
  /* x = ceil of x */
  __asm__ volatile ("frndint" : "=t" (x) : "0" (x));
  __asm__ volatile ("fldcw %0" : : "m" (cw));
  return (x);
}
