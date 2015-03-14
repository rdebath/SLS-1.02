/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "hash.h"
#include <stdio.h>
#include <stdlib.h>

#ifdef MACINTOSH
#    include <string.h>
#endif

#if !defined(IBM_PC) && !defined(MACINTOSH)
#    include <memory.h>
#endif


#ifdef _Windows
#include "srgp.h"
#define SUIT_malloc   SRGP_malloc
#define SUIT_free	  SRGP_free
#else
#define SUIT_malloc   malloc
#define SUIT_free     free
#endif

/*
 ** Macros:
 */
#define HASH(HT,EL) (((HT->hashFunc)(EL)) % HT->size)

#define DEBUG 0

/*
 ** Global variables:
 */

char *HashErrorMessages[] = {
    "Hash: No Error Occurred",
    "Hash: Element already has been added",
    "Hash: Element not found",
    "Hash: Out of Memory",
    "Hash: Can't do SUIT_free()",
    "Hash: Invalid Argument",
    "Hash: Invalid Operation"
    };

hashRetVal hashError = HASH_REPLY_NOERROR;

static tableEntry sentinel;


/*
 **  Internal Routines:
 */


typedef struct RefStruct {
    unsigned short count;
    char *copy;
} ReferenceCount;

static tableEntry **searchList(tableEntry **entry, void *el, int elsize, comparFuncType *cf)
{
    sentinel.element = el;
    while (cf((*entry)->element,el) != 0) {
	entry = &((*entry)->next);
    }
    return ((*entry) == &sentinel) ? (NULL) : (entry);
}

extern int Hash(void *s);


static int growTable (hashTable ht)
{
    hashTable newht;
    int i;
    tableEntry *te, *oldte;
    
    newht = HashCreate ((ht->size)<<1, ht->elsize, (ht->maxNumCollisions)<<1,
			ht->hashFunc, ht->comparFunc);
    if (newht == NULL) {
	return 1;
    }
    /* add to the new table and delete the old table */
    for (i=0; i<ht->size; i++) {
	te = ht->table[i];
	while (te != &sentinel) {
	    oldte = te;
	    if (HashAddWithoutCopy (newht, te->element) != 0) {
		hashRetVal temperr;
		
		temperr = hashError;
		HashDestroy (newht);
		hashError = temperr;
		return 1;
	    }
	    te = te->next;
	    SUIT_free (oldte);
	}
    }
    SUIT_free (ht->table);
    
    *ht = *newht;
    
    return 0;
}


/*
 ** Public Routines:
 */


hashTable HashCreate(int entries, int elsize, int maxNumCollisions, hashFuncType *hf, comparFuncType *cf)
{
    hashTable ht;
    int i;
    
    if ((ht = (hashTable) SUIT_malloc (sizeof(struct hashTableStruct))) == NULL) {
	hashError = HASH_OUT_OF_MEMORY;
	return NULL;
    }
    ht->comparFunc = cf;
    ht->hashFunc = hf;
    ht->size = entries;
    ht->elsize = elsize;
    ht->numCollisions = 0;
    ht->maxNumCollisions = maxNumCollisions;
    ht->table = (tableEntry **) SUIT_malloc (sizeof(tableEntry *) * entries);
    for (i = 0; i < entries; i++) {
	ht->table[i] = &sentinel;
    }
    
    return ht;
}


int HashDestroy(hashTable ht)
{
    int i;
    tableEntry *te, *oldte;
    
    hashError = HASH_REPLY_NOERROR;
    for (i=0; i<ht->size; i++) {
	te = ht->table[i];
	while (te != &sentinel) {
	    oldte = te;
	    te = te->next;
	    SUIT_free (oldte->element);
	    SUIT_free (oldte);
	}
    }
    
    SUIT_free (ht->table);
    SUIT_free (ht);
    return (hashError == HASH_REPLY_NOERROR) ? (0) : (1);
}


int HashAdd(hashTable ht, void *el)
{
    void *newel;
    
    hashError = HASH_REPLY_NOERROR;
    if ((newel = (void *) SUIT_malloc (ht->elsize)) == NULL) {
	hashError = HASH_OUT_OF_MEMORY;
	return 1;
    }
    memcpy (newel, el, ht->elsize);
    if (HashAddWithoutCopy(ht, newel) != 0) {
	SUIT_free (newel);
	return 1;
    }
    return 0;
}


int HashAddWithoutCopy(hashTable ht, void *el)
{
    tableEntry **teHandle, *newte;
    int i;
    
    i = HASH(ht,el);
    teHandle = searchList (&(ht->table[i]), el, ht->elsize, ht->comparFunc);
    if (teHandle != NULL) {
	hashError = HASH_ALREADY_ADDED;
	return 1;
    }

    /* not there already, so add it */
    if ((newte = (tableEntry *) SUIT_malloc (sizeof(tableEntry))) == NULL) {
	hashError = HASH_OUT_OF_MEMORY;
	return 1;
    }
    if (ht->table[i] != &sentinel) {
	ht->numCollisions++;
    }
    newte->element = el;
    newte->next = ht->table[i];
    ht->table[i] = newte;
    hashError = HASH_REPLY_NOERROR;
    if (ht->numCollisions > ht->maxNumCollisions)
	return growTable(ht);
    
    return 0;
}


int HashDelete(hashTable ht, void *el)
{
    tableEntry **teHandle, *tempte;
    int i;
    
    i = HASH(ht,el);
    teHandle = searchList (&(ht->table[i]), el, ht->elsize, ht->comparFunc);
    if (teHandle == NULL) {
	hashError = HASH_NOT_FOUND;
	return 1;
    }
    else {
	tempte = *teHandle;
	*teHandle = (*teHandle)->next;
	SUIT_free (tempte);
	if (ht->table[i] != &sentinel)
	    ht->numCollisions--;
	hashError = HASH_REPLY_NOERROR;
	return 0;
    }
}


void *HashLookup(hashTable ht, void *el)
{
    return (HashUnusualLookup (ht, el, ht->comparFunc));
}


void *HashUnusualLookup(hashTable ht, void *el, comparFuncType *cf)
{
    tableEntry **teHandle;
    int i;
    
    i = HASH(ht,el);
    teHandle = searchList (&(ht->table[i]), el, ht->elsize, cf);
    return ((teHandle == NULL) ? (NULL) : ((*teHandle)->element));
}


void HashTraverse(hashTable ht, traverseFuncType *tf)
{
    int i;
    tableEntry *teptr;
    
    ht->inTraverse = 1;
    for (i = 0; i < ht->size; i++)
	{
	    teptr = ht->table[i];
	    while (teptr != &sentinel) {
		tf (teptr->element);
		teptr = teptr->next;
	    }
	}
    ht->inTraverse = 0;
}


elementType HashFirstEntry(hashTable ht)
{
    ht->currentIndex = 0;
    ht->currentTableEntry = ht->table[0];
    return HashNextEntry (ht);
}


elementType HashNextEntry(hashTable ht)
{
    elementType result;
    
    while ((ht->currentTableEntry == &sentinel) &&
	   (ht->currentIndex < ht->size)) {
	ht->currentIndex++;
	ht->currentTableEntry = ht->table[ht->currentIndex];
    }
    if (ht->currentIndex >= ht->size) {
	return NULL;
    }
    result = ht->currentTableEntry->element;
    ht->currentTableEntry = ht->currentTableEntry->next;
    
    return result;
}
