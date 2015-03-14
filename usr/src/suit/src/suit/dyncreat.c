/*
 * This file is part of libdyn.a, the C Dynamic Object library.  It
 * contains the source code for the functions DynCreate() and
 * DynDestroy().
 *
 * There are no restrictions on this code; however, if you make any
 * changes, I request that you document them so that I do not get
 * credit or blame for your modifications.
 *
 * Written by Barr3y Jaspan, Student Information Processing Board (SIPB)
 * and MIT-Project Athena, 1989.
 */

#include "dynpriv.h"

#ifndef DEFAULT_INC
#define DEFAULT_INC	100
#endif

static int default_increment = DEFAULT_INC;


DynArray DynCreate(int el_size, int inc)

{
     Int_DynObject obj;

     obj = (Int_DynObject) malloc(sizeof(Int_DynObjectRec));
     if (obj == NULL)
	  return NULL;

     obj->array = (ARRAY) malloc(0);
     obj->el_size = el_size;
     obj->num_el = obj->size = obj->debug = 0;
     obj->changed = 1;
     obj->sortStatus = UNSORTED;
     obj->inc = (!! inc) ? inc : default_increment;
     return obj;
}

int DynDestroy(Int_DynObject	obj)
{
     free(obj->array);
     free(obj);
     return DYN_OK;
}


