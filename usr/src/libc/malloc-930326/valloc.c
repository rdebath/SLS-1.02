/* valloc.c - Berkeley C Library Compatibility routine.
   Copyright (c) 1989, 1993  Michael J. Haertel
   You may redistribute this library under the terms of the
   GNU Library General Public License (version 2 or any later
   version) as published by the Free Software Foundation.
   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY EXPRESS OR IMPLIED
   WARRANTY.  IN PARTICULAR, THE AUTHOR MAKES NO REPRESENTATION OR
   WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS
   SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE. */

#include <limits.h>
#include <stddef.h>
#include <stdlib.h>
#include "malloc.h"

#define MAX(A,B) ((A) > (B) ? (A) : (B))

/*
 * WARNING: The definition of BLOCKSIZE (in "malloc.h")
 * must be greater than or equal to the page size of
 * your machine.  We don't do getpagesize() because I
 * want to keep weird Unix dependencies out of the code.
 */
void *
valloc(size_t size)
{
    return malloc(MAX(BLOCKSIZE, size));
}
