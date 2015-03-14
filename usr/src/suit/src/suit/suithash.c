/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "suithash.h"
#include "privsuit.h"

hashTable SUIT_HashCreate(int entries, int elsize, int maxNumCollisions, hashFuncType *hf, comparFuncType *cf)
{
    hashTable ht;
    
    ASSERT((ht = HashCreate(entries, elsize, maxNumCollisions, hf, cf)) != NULL,
	   (mes, "Couldn't create hash table with %d entries of size %d\n%s\n", entries, elsize,
	    HashErrorMessages[hashError]));
    return ht;
}

void SUIT_HashDestroy(hashTable ht)
{
    ASSERT(HashDestroy(ht) == 0,
	   (mes, "Couldn't destroy hash table:\n\t%s\n", HashErrorMessages[hashError]));
}

void SUIT_HashAdd(hashTable ht, elementType el)
{
    ASSERT(HashAdd(ht, el) == 0,
	   (mes, "Couldn't add to hash table:\n\t%s\n", HashErrorMessages[hashError]));
}

void SUIT_HashAddWithoutCopy(hashTable ht, elementType el)
{
    ASSERT(HashAddWithoutCopy(ht, el) == 0,
	   (mes, "Couldn't add without copy to hash table:\n\t%s\n", HashErrorMessages[hashError]));
}

void SUIT_HashAddAllowingDuplicates(hashTable ht, elementType el)
{
    HashAdd(ht, el);
    ASSERT(((hashError == HASH_REPLY_NOERROR) || (hashError == HASH_ALREADY_ADDED)),
	   (mes, "Couldn't add to hash table:\n\thashError = %d:\n\t%s\n", hashError, HashErrorMessages[hashError]));
}

void SUIT_HashAddWithoutCopyAllowingDuplicates(hashTable ht, elementType el)
{
    HashAddWithoutCopy(ht, el);
    ASSERT(((hashError == HASH_REPLY_NOERROR) || (hashError == HASH_ALREADY_ADDED)),
	   (mes, "Couldn't add without copy to hash table\n%s\n", HashErrorMessages[hashError]));
}

void SUIT_HashDelete(hashTable ht, elementType el)
{
    ASSERT(HashDelete(ht, el) == 0,
	   (mes, "Couldn't delete from hash table\n%s\n", HashErrorMessages[hashError]));
}
