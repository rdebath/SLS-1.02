/*
 * This file is part of libdyn.a, the C Dynamic Object library.  It
 * contains the source code for the function DynSize().
 *
 * There are no restrictions on this code; however, if you make any
 * changes, I request that you document them so that I do not get
 * credit or blame for your modifications.
 *
 * Package by Barr3y Jaspan, Student Information Processing Board (SIPB)
 * and MIT-Project Athena, 1989.
 *
 * This modification by Jim Defay, for Randy Pausch at the University of VA.
 */

#include <stdio.h>
#ifdef _Windows
#include <search.h>
#endif

#include "dynpriv.h"

#ifdef SUN
void qsort (void *base, int numel, int width, int (*compare) (void *, void*));
#endif     

/*
#if defined(SUN) || defined(RS6000)
#include <memory.h>
#endif
*/

int DynQsort(DynArray obj, int first, int last, int (*compar)() )
{
    
    if ( DynSize(obj) == 0 )
	return(DYN_OK);
    
    if (first < DynLow(obj))
    {
	fprintf(stderr, "dyn: qsort: bad first %d.\n", first);
	return DYN_BADINDEX;
    }
    if ( last > DynHigh(obj) )
    {
	fprintf(stderr, "dyn: qsort: bad last %d.\n", last);
	return DYN_BADINDEX;
    }
    if ( first > last )
    {
	fprintf(stderr, "dyn: qsort: bad interval (%d to %d).\n", first, last);
	return DYN_BADINDEX;
    }
    
    if (obj->sortStatus != SORTED)
	obj->changed = 1;
    
    /* no need to call qsort if only one element */
    if ( first < last )
	qsort((char *) DynGet(obj,first), (last-first+1), obj->el_size, compar);
    
    if ( (first == DynLow(obj)) && (last == DynHigh(obj)) )
	obj->sortStatus = SORTED;
    
    return DYN_OK;
}



#define CHEAP_GET(OBJ,I)  (((char *)OBJ->array) + OBJ->el_size*(I))

static
    void ActuallyInsertionSort(DynArray obj, int first, int last, int (*compar)(char *, char *) )

{
  int	i;
  
  for ( i = first + 1 ; i <= last ; i++ )
    {
      if ( compar(CHEAP_GET(obj, i), CHEAP_GET(obj, i-1)) < 0 )
	{
	  int	found = 0;
	  int	newpos = i;
	  char	buf[1000];
	  memcpy(buf, CHEAP_GET(obj, i), obj->el_size);
	  do {
	    newpos -= 1;
	    /* NAT HACK: This is so we only do one chunk 'o memcpy   */

	    memcpy(CHEAP_GET(obj, newpos+1), CHEAP_GET(obj, newpos), obj->el_size); 
	    if ( newpos == first )
	       found = 1;
	     else
	       found = (compar(CHEAP_GET(obj, newpos-1), buf) <= 0);
	    
	  } while ( !found);

	  memcpy(CHEAP_GET(obj, newpos), buf, obj->el_size);
	}
    }
}

int DynInsertionSort(DynArray obj, int first, int last, int (*compar)() )
    
{
    
    if ( DynSize(obj) == 0 )
	return(DYN_OK);
    
    if (first < DynLow(obj))
    {
	fprintf(stderr, "dyn: isort: bad first %d.\n", first);
	return DYN_BADINDEX;
    }
    if ( last > DynHigh(obj) )
    {
	fprintf(stderr, "dyn: isort: bad last %d.\n", last);
	return DYN_BADINDEX;
    }
    if ( first > last )
    {
	fprintf(stderr, "dyn: isort: bad interval (%d to %d).\n", first, last);
	return DYN_BADINDEX;
    }
    
    if (obj->sortStatus != SORTED)
	obj->changed = 1;
    
    /* no need to do sort if only one element */
    if ( first < last )
	ActuallyInsertionSort(obj, first, last, compar);
    
    if ( (first == DynLow(obj)) && (last == DynHigh(obj)) )
	obj->sortStatus = SORTED;
    
    return DYN_OK;
}


int DynEqual (DynArray a, DynArray b)
{
    if (DynSize(a) != DynSize(b))
	return 0;
    if (a == NULL || b == NULL)
	return a == b;
    return !memcmp(a->array, b->array, a->el_size*a->num_el);
}









