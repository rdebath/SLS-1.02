/* malloc.c - C standard library routine.
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
#include <string.h>
#include "malloc.h"

/* How to really get more memory. */
void *(*_morecore)(long) = _default_morecore;

/* Pointer to the base of the first block. */
char *_heapbase;

/* Block information table. */
union info *_heapinfo;

/* Number of info entries. */
static int heapsize;

/* Search index in the info table. */
int _heapindex;

/* Limit of valid info table indices. */
int _heaplimit;

/* Free lists for each fragment size. */
struct list _fraghead[BLOCKLOG];

/* Are we experienced? */
static int initialized;

/* Aligned allocation. */
static void *
align(size_t size)
{
    void *result;
    unsigned int adj;

    result = (*_morecore)(size);
    adj = (unsigned int) ((char *) result - (char *) NULL) % BLOCKSIZE;
    if (adj != 0) {
	(*_morecore)(adj = BLOCKSIZE - adj);
	result = (char *) result + adj;
    }
    return result;
}

/* Set everything up and remember that we have. */
static int
initialize()
{
    heapsize = HEAP / BLOCKSIZE;
    _heapinfo = align(heapsize * sizeof (union info));
    if (!_heapinfo)
	return 0;
    memset(_heapinfo, 0, heapsize * sizeof (union info));
    _heapinfo[0].free.size = 0;
    _heapinfo[0].free.next = _heapinfo[0].free.prev = 0;
    _heapindex = 0;
    _heapbase = (char *) _heapinfo;
    initialized = 1;
    return 1;
}

/* Get neatly aligned memory, initializing or growing the
   heap info table as necessary. */
static void *
morecore(size_t size)
{
    void *result;
    union info *newinfo, *oldinfo;
    int newsize;

    result = align(size);
    if (!result)
	return NULL;

    /* Check if we need to grow the info table. */
    if (BLOCK((char *) result + size) > heapsize) {
	newsize = heapsize;
	while (BLOCK((char *) result + size) > newsize)
	    newsize *= 2;
	newinfo = align(newsize * sizeof (union info));
	if (!newinfo) {
	    (*_morecore)(-size);
	    return NULL;
	}
	memset(newinfo, 0, newsize * sizeof (union info));
	memcpy(newinfo, _heapinfo, heapsize * sizeof (union info));
	oldinfo = _heapinfo;
	newinfo[BLOCK(oldinfo)].busy.type = 0;
	newinfo[BLOCK(oldinfo)].busy.info.size
	    = BLOCKIFY(heapsize * sizeof (union info));
	_heapinfo = newinfo;
	free(oldinfo);
	heapsize = newsize;
    }

    _heaplimit = BLOCK((char *) result + size);
    return result;
}

/* Allocate memory from the heap. */
void *
malloc(size_t size)
{
    void *result;
    int log, block, blocks, i, lastblocks, start;
    struct list *next;

    if (!initialized && !initialize())
	return NULL;

    if (size == 0)
	return NULL;

    if (size < sizeof (struct list))
	size = sizeof (struct list);

    /* Determine the allocation policy based on the request size. */
    if (size <= BLOCKSIZE / 2) {
	/* Small allocation to receive a fragment of a block. Determine
	   the logarithm to base two of the fragment size. */
	--size;
	for (log = 1; (size >>= 1) != 0; ++log)
	    ;

	/* Look in the fragment lists for a free fragment of the
	   desired size. */
	if ((next = _fraghead[log].next) != 0) {
	    /* There are free fragments of this size.  Pop a fragment
	       out of the fragment list and return it.  Update the block's
	       nfree and first counters. */
	    result = next;
	    next->prev->next = next->next;
	    if (next->next)
		next->next->prev = next->prev;
	    block = BLOCK(result);
	    if (--_heapinfo[block].busy.info.frag.nfree)
		_heapinfo[block].busy.info.frag.first
		    = (unsigned int) ((char *) next->next - (char *) NULL)
		      % BLOCKSIZE >> log;
	} else {
	    /* No free fragments of the desired size, so get a new block
	       and break it into fragments, returning the first. */
	    result = malloc(BLOCKSIZE);
	    if (!result)
		return NULL;

	    /* Link all fragments but the first into the free list. */
	    for (i = 1; i < BLOCKSIZE >> log; ++i) {
		next = (struct list *) ((char *) result + (i << log));
		next->next = _fraghead[log].next;
		next->prev = &_fraghead[log];
		next->prev->next = next;
		if (next->next)
		    next->next->prev = next;
	    }

	    /* Initialize the nfree and first counters for this block. */
	    block = BLOCK(result);
	    _heapinfo[block].busy.type = log;
	    _heapinfo[block].busy.info.frag.nfree = i - 1;
	    _heapinfo[block].busy.info.frag.first = i - 1;
	}
    } else {
	/* Large allocation to receive one or more blocks.  Search
	   the free list in a circle starting at the last place visited.
	   If we loop completely around without finding a large enough
	   space we will have to get more memory from the system. */
	blocks = BLOCKIFY(size);
	start = block = _heapindex;
	while (_heapinfo[block].free.size < blocks) {
	    block = _heapinfo[block].free.next;
	    if (block == start) {
		/* Need to get more from the system.  Check to see if
		   the new core will be contiguous with the final free
		   block; if so we don't need to get as much. */
		block = _heapinfo[0].free.prev;
		lastblocks = _heapinfo[block].free.size;
		if (_heaplimit && block + lastblocks == _heaplimit
		    && (*_morecore)(0) == ADDRESS(block + lastblocks)
		    && morecore((blocks - lastblocks) * BLOCKSIZE)) {
		    _heapinfo[block].free.size = blocks;
		    continue;
		}
		result = morecore(blocks * BLOCKSIZE);
		if (!result)
		    return NULL;
		block = BLOCK(result);
		_heapinfo[block].busy.type = 0;
		_heapinfo[block].busy.info.size = blocks;
		return result;
	    }
	}

	/* At this point we have found a suitable free list entry.
	   Figure out how to remove what we need from the list. */
	result = ADDRESS(block);
	if (_heapinfo[block].free.size > blocks) {
	    /* The block we found has a bit left over, so relink the
	       tail end back into the free list. */
	    _heapinfo[block + blocks].free.size
		= _heapinfo[block].free.size - blocks;
	    _heapinfo[block + blocks].free.next
		= _heapinfo[block].free.next;
	    _heapinfo[block + blocks].free.prev
		= _heapinfo[block].free.prev;
	    _heapinfo[_heapinfo[block].free.prev].free.next
		= _heapinfo[_heapinfo[block].free.next].free.prev
		    = _heapindex = block + blocks;
	} else {
	    /* The block exactly matches our requirements, so
	       just remove it from the list. */
	    _heapinfo[_heapinfo[block].free.next].free.prev
		= _heapinfo[block].free.prev;
	    _heapinfo[_heapinfo[block].free.prev].free.next
		= _heapindex = _heapinfo[block].free.next;
	}

	_heapinfo[block].busy.type = 0;
	_heapinfo[block].busy.info.size = blocks;
    }

    return result;
}
