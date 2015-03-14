/* $XConsortium: Alloc.c,v 1.46 91/07/30 11:04:41 rws Exp $ */

/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * X Toolkit Memory Allocation Routines
 *
 * Uses Xlib memory management, which is spec'd to be re-entrant.
 */

#include "IntrinsicI.h"
#undef _XBCOPYFUNC

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
char *malloc(), *realloc(), *calloc();
#endif
#if defined(macII) && !defined(__STDC__)  /* stdlib.h fails to define these */
char *malloc(), *realloc(), *calloc();
#endif /* macII */

#ifdef MALLOC_0_RETURNS_NULL
#define Xmalloc(size) malloc(((size) > 0 ? (size) : 1))
#define Xrealloc(ptr, size) realloc((ptr), ((size) > 0 ? (size) : 1))
#define Xcalloc(nelem, elsize) calloc(((nelem) > 0 ? (nelem) : 1), (elsize))
#else
#define Xmalloc(size) malloc((size))
#define Xrealloc(ptr, size) realloc((ptr), (size))
#define Xcalloc(nelem, elsize) calloc((nelem), (elsize))
#endif
#define Xfree(ptr) free(ptr)

#ifdef _XNEEDBCOPYFUNC
void _XtBCopy(b1, b2, length)
    register char *b1, *b2;
    register length;
{
    if (b1 < b2) {
	b2 += length;
	b1 += length;
	while (length--)
	    *--b2 = *--b1;
    } else {
	while (length--)
	    *b2++ = *b1++;
    }
}
#endif

void _XtAllocError(type)
    String type;
{
    Cardinal num_params = 1;
    if (type == NULL) type = "local memory allocation";
    XtErrorMsg("allocError", type, XtCXtToolkitError,
	       "Cannot perform %s", &type, &num_params);
}

void _XtHeapInit(heap)
    Heap*	heap;
{
    heap->start = NULL;
    heap->bytes_remaining = 0;
}

#ifndef XTTRACEMEMORY

char *XtMalloc(size)
    unsigned size;
{
    char *ptr;
    if ((ptr = Xmalloc(size)) == NULL)
        _XtAllocError("malloc");

    return(ptr);
}

char *XtRealloc(ptr, size)
    char     *ptr;
    unsigned size;
{
   if (ptr == NULL) return(XtMalloc(size));
   else if ((ptr = Xrealloc(ptr, size)) == NULL)
	_XtAllocError("realloc");

   return(ptr);
}

char *XtCalloc(num, size)
    unsigned num, size;
{
    char *ptr;

#ifdef MALLOC_0_RETURNS_NULL
    if (!size)
	num = size = 1;
#endif
    if ((ptr = Xcalloc(num, size)) == NULL)
	_XtAllocError("calloc");

    return(ptr);
}

void XtFree(ptr)
    char *ptr;
{
   if (ptr != NULL) Xfree(ptr);
}

#ifndef HEAP_SEGMENT_SIZE
#define HEAP_SEGMENT_SIZE 1492
#endif

char* _XtHeapAlloc(heap, bytes)
    Heap*	heap;
    Cardinal	bytes;
{
    register char* heap_loc;
    if (heap == NULL) return XtMalloc(bytes);
    if (heap->bytes_remaining < (int)bytes) {
	if ((bytes + sizeof(char*)) >= (HEAP_SEGMENT_SIZE>>1)) {
	    /* preserve current segment; insert this one in front */
#ifdef _TRACE_HEAP
	    printf( "allocating large segment (%d bytes) on heap %#x\n",
		    bytes, heap );
#endif
	    heap_loc = XtMalloc(bytes + sizeof(char*));
	    if (heap->start) {
		*(char**)heap_loc = *(char**)heap->start;
		*(char**)heap->start = heap_loc;
	    }
	    else {
		*(char**)heap_loc = NULL;
		heap->start = heap_loc;
	    }
	    return heap_loc + sizeof(char*);
	}
	/* else discard remainder of this segment */
#ifdef _TRACE_HEAP
	printf( "allocating new segment on heap %#x\n", heap );
#endif
	heap_loc = XtMalloc((unsigned)HEAP_SEGMENT_SIZE);
	*(char**)heap_loc = heap->start;
	heap->start = heap_loc;
	heap->current = heap_loc + sizeof(char*);
	heap->bytes_remaining = HEAP_SEGMENT_SIZE - sizeof(char*);
    }
#ifdef WORD64
    /* round to nearest 8-byte boundary */
    bytes = (bytes + 7) & (~7);
#else
    /* round to nearest 4-byte boundary */
    bytes = (bytes + 3) & (~3);
#endif /* WORD64 */
    heap_loc = heap->current;
    heap->current += bytes;
    heap->bytes_remaining -= bytes; /* can be negative, if rounded */
    return heap_loc;
}

void _XtHeapFree(heap)
    Heap*	heap;
{
    char* segment = heap->start;
    while (segment != NULL) {
	char* next_segment = *(char**)segment;
	XtFree(segment);
	segment = next_segment;
    }
    heap->start = NULL;
    heap->bytes_remaining = 0;
}

#else

/*
 * X Toolkit Memory Trace Allocation Routines
 */

#undef XtMalloc
#undef XtRealloc
#undef XtCalloc
#undef XtFree

typedef struct stat {
    struct stat *prev, *next;
    char *file;
    int line;
    unsigned size;
    unsigned long seq;
    XtPointer heap;
} Stats, *StatsPtr;

static StatsPtr XtMemory = (StatsPtr)NULL;
static unsigned long ActiveXtMemory = 0;
static unsigned long XtSeqId = 0;
static unsigned long XtSeqBreakpoint = ~0;

#define StatsSize(n) ((((n) + 3) & ~3) + sizeof(Stats))
#define ToStats(ptr) ((StatsPtr)(ptr - sizeof(Stats)))
#define ToMem(ptr) (((char *)ptr) + sizeof(Stats))

#define CHAIN(ptr,len,hp) \
    ptr->next = XtMemory; \
    if (XtMemory) \
        XtMemory->prev = ptr; \
    XtMemory = ptr; \
    ptr->prev = (StatsPtr)NULL; \
    ptr->file = file; \
    ptr->line = line; \
    ptr->size = len; \
    ptr->heap = hp; \
    if (file) \
	ActiveXtMemory += len; \
    ptr->seq = XtSeqId; \
    if (XtSeqId == XtSeqBreakpoint) \
	_XtBreakpoint(ptr); \
    XtSeqId++

/*ARGUSED*/
static void _XtBreakpoint(mem)
    StatsPtr mem;
{
    mem->seq = XtSeqId; /* avoid being optimized out of existence */
}

char *_XtMalloc(size, file, line)
    unsigned size;
    char *file;
    int line;
{
    StatsPtr ptr;
    unsigned newsize;

    newsize = StatsSize(size);
    if ((ptr = (StatsPtr)Xmalloc(newsize)) == NULL)
        _XtAllocError("malloc");
    CHAIN(ptr, size, NULL);
    return(ToMem(ptr));
}

char *XtMalloc(size)
    unsigned size;
{
    return _XtMalloc(size, (char *)NULL, 0);
}

char *_XtRealloc(ptr, size, file, line)
    char     *ptr;
    unsigned size;
    char *file;
    int line;
{
   char *newptr;

   newptr = _XtMalloc(size, file, line);
   if (ptr) {
       unsigned copysize = ToStats(ptr)->size;
       if (copysize > size) copysize = size;
       bcopy(ptr, newptr, copysize);
       _XtFree(ptr);
   }
   return(newptr);
}

char *XtRealloc(ptr, size)
    char     *ptr;
    unsigned size;
{
    return _XtRealloc(ptr, size, (char *)NULL, 0);
}

char *_XtCalloc(num, size, file, line)
    unsigned num, size;
    char *file;
    int line;
{
    StatsPtr ptr;
    unsigned total, newsize;

    total = num * size;
    newsize = StatsSize(total);
    if ((ptr = (StatsPtr)Xcalloc(newsize, 1)) == NULL)
        _XtAllocError("calloc");
    CHAIN(ptr, total, NULL);
    return(ToMem(ptr));
}

char *XtCalloc(num, size)
    unsigned num, size;
{
    return _XtCalloc(num, size, (char *)NULL, 0);
}

Boolean _XtIsValidPointer(ptr)
    char *ptr;
{
    register StatsPtr mem;
    register StatsPtr stp = ToStats(ptr);

    for (mem = XtMemory; mem; mem = mem->next) {
	if (mem == stp)
	    return True;
    }
    return False;
}

Boolean _XtValidateMemory = False;

void _XtFree(ptr)
    char *ptr;
{
   register StatsPtr stp;

   if (ptr) {
       if (_XtValidateMemory && !_XtIsValidPointer(ptr))
	   abort();
       stp = ToStats(ptr);
       if (stp->file)
	   ActiveXtMemory -= stp->size;
       if (stp->prev)
	   stp->prev->next = stp->next;
       else
	   XtMemory = stp->next;
       if (stp->next)
	   stp->next->prev = stp->prev;
       Xfree((char *)stp);
   }
}

void XtFree(ptr)
    char *ptr;
{
   _XtFree(ptr);
}

char *_XtHeapMalloc(heap, size, file, line)
    Heap *heap;
    Cardinal size;
    char *file;
    int line;
{
    StatsPtr ptr;
    unsigned newsize;
    XtPointer hp = (XtPointer) heap;

    newsize = StatsSize(size);
    if ((ptr = (StatsPtr)Xmalloc(newsize)) == NULL)
        _XtAllocError("malloc");
    CHAIN(ptr, size, hp);
    return(ToMem(ptr));
}

void _XtHeapFree(heap)
    register XtPointer heap;
{
    register StatsPtr mem, next;

    for (mem = XtMemory; mem; mem = next) {
	next = mem->next;
	if (mem->heap == heap) {
	    if (mem->file)
		ActiveXtMemory -= mem->size;
	    if (mem->prev)
		mem->prev->next = next;
	    else
		XtMemory = next;
	    if (next)
		next->prev = mem->prev;
	    Xfree((char *)mem);
	}
    }
}

#include <stdio.h>

void _XtPrintMemory(filename)
char * filename;
{
    register StatsPtr mem;
    FILE *f;

    if (filename == NULL)
	f = stderr;
    else 
	f = fopen(filename, "w");
    fprintf(f, "total size: %d\n", ActiveXtMemory);
    for (mem = XtMemory; mem; mem = mem->next) {
	if (mem->file)
	    fprintf(f, "size: %6d  seq: %5d  %12s(%4d)  %s\n",
		    mem->size, mem->seq,
		    mem->file, mem->line, mem->heap ? "heap" : "");
    }
    if (filename) fclose(f);
}

#endif  /* XTTRACEMEMORY */
