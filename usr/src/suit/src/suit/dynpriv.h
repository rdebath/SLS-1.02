/*
 * This file is part of libdyn.a, the C Dynamic Array library.  It
 * contains the private header file.
 *
 * There are no restrictions on this code; however, if you make any
 * changes, I request that you document them so that I do not get
 * credit or blame for your modifications.
 *
 * Written by Barr3y Jaspan, Student Information Processing Board (SIPB)
 * and MIT-Project Athena, 1989.
 */


/*
 * dynpriv.h -- private header file included by source files for libdyn.a.
 */

#ifndef _dynpriv
#define _dynpriv

#include "dynarray.h"

#if defined(SUN) && !defined(SGI_X)
#    include <malloc.h>
#elif defined(SGI_X) || defined(MACINTOSH)
#    include <stdlib.h>
#elif defined(_Windows)
#    include "srgp.h"
#define malloc     SRGP_malloc
#define realloc    SRGP_realloc
#define free       SRGP_free
#elif defined(IBM_PC)
#    include <alloc.h>
#endif

#ifndef SUN
#    include <string.h>
#endif

#include <stdio.h>

#if defined(SUN) && !defined(linux)
void printf();
void fprintf();
int free();
#endif

#endif /* _dynpriv.h */
/* DON'T ADD STUFF AFTER THIS #endif */



