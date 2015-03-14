/*
 * This file is part of libdyn.a, the C Dynamic Object library.  It
 * contains the source code for the function DynDelete().
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


int DynDeleteFastButWreckOrdering(DynArray obj, int index)
{
    if (index < 0) {
	if (obj->debug)
	    fprintf(stderr, "dyn: deletefast: bad index %d\n", index);
	return DYN_BADINDEX;
    }
    
    if (index >= obj->num_el) {
	if (obj->debug)
	    fprintf(stderr, "dyn: deletefast: Highest index is %d.\n",
		    obj->num_el);
	return DYN_BADINDEX;
    }
    
    if (index == obj->num_el-1) {
	if (obj->debug)
	    fprintf(stderr, "dyn: deletefast: last element, doing nothing.\n");
    }
    else {
	if (obj->debug)
	    fprintf(stderr,
		    "dyn: deletefast: copying %d bytes from %x + %d to + %d.\n",
		    obj->el_size, (unsigned int)obj->array,
		    (obj->num_el-1)*obj->el_size, index*obj->el_size);
	
	memcpy(( (char *) obj->array) + index*obj->el_size,
	       ( (char *) obj->array) + ((obj->num_el-1)*obj->el_size),
	       obj->el_size);
    }
    
    obj->sortStatus = UNSORTED;
    --obj->num_el;
    obj->changed = 1;
    
    if (obj->debug)
	fprintf(stderr, "dyn: delete: done.\n");
    
    return DYN_OK;
}




int DynDelete(DynArray obj, int index)
{
    if (index < 0) {
	if (obj->debug)
	    fprintf(stderr, "dyn: delete: bad index %d\n", index);
	return DYN_BADINDEX;
    }
    
    if (index >= obj->num_el) {
	if (obj->debug)
	    fprintf(stderr, "dyn: delete: Highest index is %d.\n",
		    obj->num_el);
	return DYN_BADINDEX;
    }
    
    if (index == obj->num_el-1) {
	if (obj->debug)
	    fprintf(stderr, "dyn: delete: last element, doing nothing.\n");
    }
    else {
	if (obj->debug)
	    fprintf(stderr,
		    "dyn: delete: copying %d bytes from %x + %d to + %d.\n",
		    obj->el_size*(obj->num_el - index), (unsigned int)obj->array,
		    (index+1)*obj->el_size, index*obj->el_size);
	
	memcpy(((char *)obj->array) + index*obj->el_size,
	       ((char *)obj->array) + (index+1)*obj->el_size,
	       obj->el_size*(obj->num_el - index - 1));
    }
    
    --obj->num_el;
    obj->changed = 1; 
    
    if (obj->debug)
	fprintf(stderr, "dyn: delete: done.\n");
    
    return DYN_OK;
}
