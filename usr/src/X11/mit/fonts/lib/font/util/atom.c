/*
 * $XConsortium: atom.c,v 1.2 91/05/11 09:16:15 keith Exp $
 *
 * Copyright 1990 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

/* lame atom replacement routines for font applications */

#include "fontmisc.h"

typedef struct _AtomList {
    char		*name;
    int			len;
    int			hash;
    Atom		atom;
} AtomListRec, *AtomListPtr;

static AtomListPtr  *hashTable;

static int	    hashSize, hashUsed;
static int	    hashMask;
static int	    rehash;

static AtomListPtr  *reverseMap;
static int	    reverseMapSize;
static Atom	    lastAtom;

static
Hash(string, len)
    char    *string;
{
    int	h;

    h = 0;
    while (len--)
	h = (h << 3) ^ *string++;
    if (h < 0)
	return -h;
    return h;
}

static
ResizeHashTable ()
{
    int		newHashSize;
    int		newHashMask;
    AtomListPtr	*newHashTable;
    int		i;
    int		h;
    int		newRehash;
    int		r;

    if (hashSize == 0)
	newHashSize = 1024;
    else
	newHashSize = hashSize * 2;
    newHashTable = (AtomListPtr *) xalloc (newHashSize * sizeof (AtomListPtr));
    if (!newHashTable)
	return FALSE;
    bzero ((char *) newHashTable, newHashSize * sizeof (AtomListPtr));
    newHashMask = newHashSize - 1;
    newRehash = (newHashMask - 2);
    for (i = 0; i < hashSize; i++)
    {
	if (hashTable[i])
	{
	    h = (hashTable[i]->hash) & newHashMask;
	    if (newHashTable[h])
	    {
		r = hashTable[i]->hash % newRehash | 1;
		do {
		    h += r;
		    if (h >= newHashSize)
			h -= newHashSize;
		} while (newHashTable[h]);
	    }
	    newHashTable[h] = hashTable[i];
	}
    }
    xfree (hashTable);
    hashTable = newHashTable;
    hashSize = newHashSize;
    hashMask = newHashMask;
    rehash = newRehash;
    return TRUE;
}

ResizeReverseMap ()
{
    if (reverseMapSize == 0)
	reverseMapSize = 1000;
    else
	reverseMapSize *= 2;
    reverseMap = (AtomListPtr *) xrealloc (reverseMap, reverseMapSize * sizeof (AtomListPtr));
    if (!reverseMap)
	return FALSE;
}

static
NameEqual (a, b, l)
    char    *a, *b;
{
    while (l--)
	if (*a++ != *b++)
	    return FALSE;
    return TRUE;
}

Atom 
MakeAtom(string, len, makeit)
    char *string;
    unsigned len;
    int makeit;
{
    AtomListPtr	a;
    int		hash;
    int		h;
    int		r;

    hash = Hash (string, len);
    if (hashTable)
    {
    	h = hash & hashMask;
	if (hashTable[h])
	{
	    if (hashTable[h]->hash == hash && hashTable[h]->len == len &&
	    	NameEqual (hashTable[h]->name, string, len))
	    {
	    	return hashTable[h]->atom;
	    }
	    r = (hash % rehash) | 1;
	    for (;;)
	    {
		h += r;
		if (h >= hashSize)
		    h -= hashSize;
		if (!hashTable[h])
		    break;
		if (hashTable[h]->hash == hash && hashTable[h]->len == len &&
		    NameEqual (hashTable[h]->name, string, len))
		{
		    return hashTable[h]->atom;
		}
	    }
    	}
    }
    if (!makeit)
	return None;
    a = (AtomListPtr) xalloc (sizeof (AtomListRec) + len + 1);
    a->name = (char *) (a + 1);
    a->len = len;
    strncpy (a->name, string, len);
    a->name[len] = '\0';
    a->atom = ++lastAtom;
    a->hash = hash;
    if (hashUsed >= hashSize / 2)
    {
	ResizeHashTable ();
	h = hash & hashMask;
	if (hashTable[h])
	{
	    r = (hash % rehash) | 1;
	    do {
		h += r;
		if (h >= hashSize)
		    h -= hashSize;
	    } while (hashTable[h]);
	}
    }
    hashTable[h] = a;
    hashUsed++;
    if (reverseMapSize <= a->atom)
	ResizeReverseMap();
    reverseMap[a->atom] = a;
    return a->atom;
}

ValidAtom(atom)
    Atom atom;
{
    return (atom != None) && (atom <= lastAtom);
}

char *
NameForAtom(atom)
    Atom atom;
{
    if (atom != None && atom <= lastAtom)
	return reverseMap[atom]->name;
    return 0;
}
