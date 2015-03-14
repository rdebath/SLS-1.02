/* $XConsortium: cache.c,v 1.4 91/05/13 16:53:18 gildea Exp $ */
/*
 * Copyright 1990, 1991 Network Computing Devices;
 * Portions Copyright 1987 by Digital Equipment Corporation and the
 * Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this protoype software
 * and its documentation to Members and Affiliates of the MIT X Consortium
 * any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of Network Computing Devices, Digital or
 * MIT not be used in advertising or publicity pertaining to distribution of
 * the software without specific, written prior permission.
 *
 * NETWORK COMPUTING DEVICES, DIGITAL AND MIT DISCLAIM ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS, IN NO EVENT SHALL NETWORK COMPUTING DEVICES, DIGITAL OR MIT BE
 * LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * @(#)cache.c	4.2	91/05/02
 *
 */
#include	"cachestr.h"
#include	"misc.h"

#define INITBUCKETS 64
#define INITHASHSIZE 6
#define MAXHASHSIZE 11


#define	ENTRYOFFSET		22
#define CACHE_ENTRY_MASK	0x3FFFFF
#define	CACHE_ENTRY_BITS(id)	((id) & 0x1fc00000)
#define	CACHE_ID(id)		((int)(CACHE_ENTRY_BITS(id) >> ENTRYOFFSET))

#define	NullCacheEntry	((CacheEntryPtr) 0)

#define	MAX_NUM_CACHES	32
/* XXX make this dynamic? */
static CachePtr caches[MAX_NUM_CACHES];
static int  num_caches = 1;

/*-
 * Notes on cache implementation
 *
 * This is basically the X11 resource code, with some modifications
 * to handle aging.
 *
 * Its currently optimized for lookup & store.  Flushing old stuff
 * is a lot slower than it should probably be, but there's tradeoffs
 * in tuning.
 */

Cache
CacheInit(maxsize)
    unsigned long maxsize;
{
    Cache       id = (Cache) num_caches;
    CachePtr    cache;

    cache = (CachePtr) fsalloc(sizeof(CacheRec));
    if (!cache)
	return (Cache) 0;
    cache->entries = (CacheEntryPtr *)
	fsalloc(INITBUCKETS * sizeof(CacheEntryPtr));
    bzero((char *) cache->entries, (INITBUCKETS * sizeof(CacheEntryPtr)));
    if (!cache->entries) {
	fsfree(cache);
	return (Cache) 0;
    }
    caches[id] = cache;
    cache->elements = 0;
    cache->buckets = INITBUCKETS;
    cache->hashsize = INITHASHSIZE;
    cache->maxsize = maxsize;
    cache->nextid = id << ENTRYOFFSET;
    cache->id = id;
    num_caches++;
    return id;
}

static int
hash(cid)
    CacheID     cid;
{
    CachePtr    cache = caches[CACHE_ID(cid)];

    switch (cache->hashsize) {
    case 6:
	return ((int) (0x03F & (cid ^ (cid >> 6) ^ (cid >> 12))));
    case 7:
	return ((int) (0x07F & (cid ^ (cid >> 7) ^ (cid >> 13))));
    case 8:
	return ((int) (0x0FF & (cid ^ (cid >> 8) ^ (cid >> 16))));
    case 9:
	return ((int) (0x1FF & (cid ^ (cid >> 9))));
    case 10:
	return ((int) (0x3FF & (cid ^ (cid >> 10))));
    case 11:
	return ((int) (0x7FF & (cid ^ (cid >> 11))));
    }
    return -1;
}

static void
rebuild_cache(cache)
    CachePtr    cache;
{
    int         j;
    CacheEntryPtr cp,
                next,
              **tails,
               *entries,
              **tptr,
               *cptr;

    assert(cache);
    j = 2 * cache->buckets;
    tails = (CacheEntryPtr **) ALLOCATE_LOCAL(j * sizeof(CacheEntryPtr *));
    if (!tails)
	return;
    entries = (CacheEntryPtr *) fsalloc(j * sizeof(CacheEntryPtr));
    if (entries) {
	DEALLOCATE_LOCAL(tails);
	return;
    }
    for (cptr = entries, tptr = tails; --j >= 0; cptr++, tptr++) {
	*cptr = NullCacheEntry;
	*tptr = cptr;
    }
    cache->hashsize++;
    for (j = cache->buckets, cptr = cache->entries; --j >= 0; cptr++) {
	for (cp = *cptr; cp; cp = next) {
	    next = cp->next;
	    cp->next = NullCacheEntry;
	    tptr = &tails[hash(cp->id)];
	    **tptr = cp;
	    *tptr = &cp->next;
	}
    }
    DEALLOCATE_LOCAL(tails);
    cache->buckets *= 2;
    fsfree(cache->entries);
    cache->entries = entries;
}

/*
 * throws out all existing entries
 */
void
CacheReset()
{
    CacheEntryPtr cp;
    CachePtr    cache;
    int         i,
                j;

    for (j = 0; j < num_caches; j++) {
	cache = caches[j];
	if (!cache)
	    continue;
	for (i = 0; i < cache->buckets; i++) {
	    for (cp = cache->entries[i]; cp; cp = cp->next) {
		cache->elements--;
		cache->cursize -= cp->size;
		(*cp->free_func) (cp->id, cp->data, CacheWasReset);
		fsfree(cp);
	    }
	}
	assert(cache->cursize == 0);
    }
}

static void
flush_cache(cache, needed)
    CachePtr    cache;
    unsigned long needed;
{
    CacheEntryPtr cp,
                oldest,
               *oldprev,
               *prev;

    while ((cache->cursize + needed) > cache->maxsize) {
	oldprev = cache->entries;
	oldest = *oldprev;
	for (prev = &oldest->next; cp = *prev; prev = &cp->next) {
	    if (cp->timestamp < oldest->timestamp) {
		oldest = cp;
		oldprev = prev;
	    }
	}
	*oldprev = oldest->next;
	cache->elements--;
	cache->cursize -= oldest->size;
	(*oldest->free_func) (oldest->id, oldest->data, CacheEntryOld);
	fsfree(oldest);
    }
}

CacheID
cache_store_memory(cid, data, size, free_func)
    Cache       cid;
    pointer     data;
    unsigned long size;
    CacheFree   free_func;
{
    CacheID     id;
    CacheEntryPtr cp,
               *head;
    CachePtr    cache = caches[cid];

    if (size > cache->maxsize)	/* beyond cache limits */
	return (CacheID) 0;

    if ((cache->elements >= 4 * cache->buckets) &&
	    (cache->hashsize < MAXHASHSIZE)) {
	rebuild_cache(cache);
    }
    id = cache->nextid++;

    if ((cache->cursize + size) > cache->maxsize) {
	flush_cache(cache, size);
    }
    head = &cache->entries[hash(id)];
    cp = (CacheEntryPtr) fsalloc(sizeof(CacheEntryRec));
    if (!cp) {
	return (CacheID) 0;
    }
    cp->next = *head;
    cp->id = id;
    cp->timestamp = GetTimeInMillis();
    cp->free_func = free_func;
    cp->size = size;
    cp->data = data;
    cache->cursize += size;
    cache->elements++;
    *head = cp;

    return id;
}

pointer
CacheFetchMemory(cid, update)
    CacheID     cid;
    Bool        update;
{
    CachePtr    cache = caches[CACHE_ID(cid)];
    CacheEntryPtr cp,
               *head;

    head = &cache->entries[hash(cid)];
    for (cp = *head; cp; cp = cp->next) {
	if (cp->id == cid) {
	    if (update) {
		cp->timestamp = GetTimeInMillis();
		if (cp != *head) {	/* put it in the front */
		    cp->next = *head;
		    *head = cp;
		}
	    }
	    return cp->data;
	}
    }
    return (pointer) 0;
}

int
CacheFreeMemory(cid, notify)
    CacheID     cid;
    Bool        notify;
{
    CachePtr    cache = caches[CACHE_ID(cid)];
    CacheEntryPtr cp,
               *prev,
               *head;
    int        *elptr;
    int         elements;
    Bool        found = FALSE;

    head = &cache->entries[hash(cid)];
    elptr = &cache->elements;
    prev = head;
    while ((cp = *prev) != NullCacheEntry) {
	if (cp->id == cid) {
	    *prev = cp->next;
	    elements = --*elptr;
	    if (notify) {
		(*(cp->free_func)) (cid, cp->data, CacheEntryFreed);
	    }
	    cache->cursize -= cp->size;
	    fsfree(cp);
	    if (*elptr != elements)
		prev = head;
	    found = TRUE;
	} else {
	    prev = &cp->next;
	}
    }
    if (!found)
	FatalError("Freeing cache entry %d which isn't there\n", cid);
}

/* ARGSUSED */
void
CacheSimpleFree(cid, data, reason)
    CacheID     cid;
    pointer     data;
    int         reason;
{
    fsfree(data);
}
