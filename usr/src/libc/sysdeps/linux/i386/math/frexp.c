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
#include <ieee754.h>

/*  Stores binary exponent of d in e, and returns whole fraction of d
 *   (with binary exponent of 0) (special case for d=0)
 */
double frexp(double d, int *e)
{
    union ieee754_double *dp = (union ieee754_double *)&d;
 
    if (d == 0.0)	/* value is zero, return exponent of 0 */
	*e = 0.0;
    else {
	*e = dp->ieee.exponent - _IEEE754_DOUBLE_BIAS + 1;
	dp->ieee.exponent = _IEEE754_DOUBLE_BIAS - 1;
    }
    return d;
}
