/*
 * This file is part of libdyn.a, the C Dynamic Object library.  It
 * contains the source code for the functions DynGet() and DynAdd().
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


static int _DynRealloc (Int_DynObject obj, int num_incs)
{
     ARRAY temp;
     int new_size_in_bytes;
     
     new_size_in_bytes = obj->el_size*(obj->size + obj->inc*num_incs);

     if (obj->debug)
	  fprintf(stderr,
		  "dyn: alloc: Increasing object by %d bytes (%d incs).\n",
		  obj->el_size*obj->inc*num_incs, num_incs);
     
     temp = (ARRAY) realloc(obj->array, new_size_in_bytes);
     if (temp == NULL) {
	  if (obj->debug)
	       fprintf(stderr, "dyn: alloc: Out of memory.\n");
	  return DYN_NOMEM;
     }
     else {
	  obj->array = temp;
	  obj->size += obj->inc*num_incs;
     }

     if (obj->debug)
	  fprintf(stderr, "dyn: alloc: done.\n");
	  
     return DYN_OK;
}



/*
 * This function is not exported because if index is large enough to
 * cause two or more increments to be allocated the rep invariant
 * is not preserved.
 */
static int DynPut (Int_DynObject obj, void *el, int index)
{
     if (obj->debug)
	  fprintf(stderr, "dyn: put: Writing %d bytes from %x to %x + %d\n",
		  obj->el_size, (unsigned int)el, (unsigned int)obj->array, index*obj->el_size);
		  
     if (obj->size <= index) {
	  int	num_incs, ret;

	  num_incs = ((index - obj->size) / obj->inc) + 1;
	  if ((ret = _DynRealloc(obj, num_incs)) != DYN_OK)
	       return ret;
     }

     memcpy((char *)obj->array + index*obj->el_size, el, obj->el_size);

     if (obj->debug)
	  fprintf(stderr, "dyn: put: done.\n");
     
     return DYN_OK;
}



void *DynGet (DynArray obj, int num)
{
     if (num < 0) {
	  if (obj->debug)
	       fprintf(stderr, "dyn: get: bad index %d\n", num);
	  return NULL;
     }
     
     if (num >= obj->num_el) {
	  if (obj->debug)
	       fprintf(stderr, "dyn: get: highest element is %d.\n",
		       obj->num_el);
	  return NULL;
     }
     
     if (obj->debug)
	  fprintf(stderr, "dyn: get: Returning address %x + %d.\n",
		  (unsigned int)obj->array, obj->el_size*num);
     
     return ( (void *) (((char *)obj->array) + obj->el_size*num) );
}


int DynAdd (DynArray obj, void *el)
{
     int ret;

     ret = DynPut(obj, el, obj->num_el);
     if (ret != DYN_OK)
	  return ret;

     ++obj->num_el;
     obj->changed = 1;  
     obj->sortStatus = UNSORTED;
     return ret;
}



