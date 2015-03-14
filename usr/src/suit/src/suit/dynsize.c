/*
 * This file is part of libdyn.a, the C Dynamic Object library.  It
 * contains the source code for the function DynSize().
 *
 * There are no restrictions on this code; however, if you make any
 * changes, I request that you document them so that I do not get
 * credit or blame for your modifications.
 *
 * Written by Barr3y Jaspan, Student Information Processing Board (SIPB)
 * and MIT-Project Athena, 1989.
 */

#include <stdio.h>

#include "dynpriv.h"

int DynSize(DynArray obj)
{
     if (obj->debug)
	 fprintf(stderr, "dyn: size: returning size %d.\n", obj->num_el);

     return obj->num_el;
}
