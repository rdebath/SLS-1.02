/***********************************************************************\
*	Copyright (C) 1992 by Michael K. Johnson, johnsonm@stolaf.edu	*
*									*
*	This file is placed under the conditions of the GNU public	*
*	license, version 2, or any later version.  See file COPYING	*
*	for information on distribution conditions.			*
\***********************************************************************/


#include <stdlib.h>
#include <stdio.h>
#include "ps.h"

void *xcalloc(void *pointer, int size) {

  void * ret;

  if (pointer) free(pointer);
  if (!(ret = calloc(1, size))) {
    fprintf(stderr, "xcalloc: allocation error, size = %d\n", size);
    exit(1);
  } else {
    return ret;
  }
}


