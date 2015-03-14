/* D Y N A R R A Y */
/*
 * This file is part of libdyn.a, the C Dynamic Object library.  It
 * contains the public header file.
 *
 * There are no restrictions on this code; however, if you make any
 * changes, I request that you document them so that I do not get
 * credit or blame for your modifications.
 *
 * Written by Barr3y Jaspan, Student Information Processing Board (SIPB)
 * and MIT-Project Athena, 1989.
 */


/*
 * dynarray.h -- header file to be included by programs linking against
 * libdyn.a.
 */

#ifndef _Dyn_h
#define _Dyn_h


#ifndef SUIT_PROTOTYPE
#    ifdef __cplusplus
#        define SUIT_PROTOTYPE extern "C"
#    else
#        define SUIT_PROTOTYPE
#    endif
#endif /* SUIT_PROTOTYPE */


#ifndef ANSI_COMPILER
#    if defined(__STDC__) || defined(__cplusplus) || defined(_Windows)
#        define ANSI_COMPILER  1
#    else
#        define ANSI_COMPILER  0
#    endif
#endif

#if ANSI_COMPILER
#define CARGS(X)  X
#else
#define CARGS(X)  ()
#endif


#ifndef AVAILABLE_ARCHITECTURES
#define AVAILABLE_ARCHITECTURES

#ifdef THINK_C
#    define MACINTOSH
#elif __TURBOC__
#    define IBM_PC
#elif _AIX
#    define RS6000
#elif sgi
#    define SGI_X /* one day we'll there'll also be SGI_GL */
#elif ultrix
#    define DEC
#elif __unix__
#    define SUN /* either sparc or sun 3 */
#endif

#endif /* AVAILABLE_ARCHITECTURES */


typedef void *ARRAY;

#define	UNSORTED	   0 
#define	SORTED		   1 

#define SORT_IF_OVER	   3 
#define QUICKSORT_IF_OVER 10 

typedef struct DynObject_struct { 
     ARRAY	array; 
     unsigned short el_size, num_el, size, inc;
     unsigned int debug		: 1;
     unsigned int changed	: 1;
     unsigned int sortStatus	: 1; 
} Int_DynObjectRec, *Int_DynObject, DynArrayRec, *DynArray; 

/* Function macros */
#define DynHigh(obj)	(DynSize(obj) - 1)
#define DynLow(obj)	(0)

/* Return status codes */
#define DYN_NOT_FOUND	-1
#define DYN_OK			-1000
#define DYN_NOMEM		-1001
#define DYN_BADINDEX	-1002

/* Function declarations */
SUIT_PROTOTYPE DynArray	        DynCreate CARGS((int el_size, int inc));
SUIT_PROTOTYPE int		DynDelete CARGS((DynArray obj, int index));
SUIT_PROTOTYPE int		DynDeleteFastButWreckOrdering CARGS((DynArray obj, int index));
SUIT_PROTOTYPE int		DynAdd CARGS((DynArray obj, void *el));
SUIT_PROTOTYPE int		DynDebug CARGS((DynArray obj, int state));
SUIT_PROTOTYPE int		DynSize CARGS((DynArray obj));
SUIT_PROTOTYPE int		DynQsort CARGS((DynArray obj, int first, int last, int (*compar)() ));
SUIT_PROTOTYPE int		DynInsertionSort CARGS((DynArray obj, int first, int last, int (*compar)() ));
SUIT_PROTOTYPE void*		DynGet CARGS((DynArray obj, int num));
SUIT_PROTOTYPE void*		DynFind  CARGS((DynArray obj, void *key, int (*compare)() ));
SUIT_PROTOTYPE int		DynFindIndex  CARGS((DynArray obj, void *key, int (*compare)() ));
SUIT_PROTOTYPE int		DynDestroy CARGS((DynArray obj));
SUIT_PROTOTYPE int              DynEqual CARGS((DynArray a, DynArray b));

#endif /* _Dyn_h */
/* DO NOT ADD ANYTHING AFTER THIS #endif */
