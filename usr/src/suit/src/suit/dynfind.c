/* This file written by Randy Pausch at the University of Virginia */

#include <stdio.h>
/*
  #ifndef mips
#include <stdlib.h>
#endif
*/

#include "dynpriv.h"
#ifdef _Windows
#include <search.h>
#endif

#if defined(SUN) || defined(RS6000)
void *bsearch (void *key, void *base, unsigned, unsigned, int (*compar) (void*, void*));
#endif     


/* the passed function can be called with the given
   dynamic object (passes a void *, just like DynGet)
   and looks for the given object, using the provided comparison routine. 
   Returns either pointer to the item or NULL
   */

void	*DynFind (DynArray obj, void *key, int (*compare)() )
    
{
    
    char	*result;
    
    if (compare == NULL) {
	if (obj->debug)
	    fprintf(stderr, "dyn: dynfind: NULL compare function was received\n");
	return NULL;
    }
    
    if (key == NULL) {
	if (obj->debug)
	    fprintf(stderr, "dyn: dynfind: NULL key was received\n");
	return NULL;
    }
    
    if ( DynSize(obj) == 0 )
	return(NULL);
    
    if ( obj->sortStatus == UNSORTED )
    {
	DynInsertionSort(obj, DynLow(obj), DynHigh(obj), compare);
	obj->sortStatus = SORTED;
    }


    result = (char *) bsearch( (char *) key,
		     (char *) (obj->array),
		     (unsigned) (obj->num_el),
		     (unsigned) obj->el_size,
		     compare);

    return(result);
}

/*
   Returns either position in the array or DYN_NOT_FOUND.
*/
int DynFindIndex (DynArray obj, void *key, int (*compare)() )
    
{
    
    void	*result	= DynFind(obj, key, compare);

    if ( result == NULL)		
	return(DYN_NOT_FOUND);
    else
    {
	unsigned distance = ( (unsigned) result) - ( (unsigned) (obj->array));
	return( distance / obj->el_size );
    }

}
