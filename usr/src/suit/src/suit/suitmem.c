/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "dynpriv.h"
#include "suithash.h"
#include <string.h>

#define RC_STRING_TABLE_SIZE		251
#define RC_STRING_TABLE_MAX_COLLISIONS	200


static int HashRefStrings (void *pointer)
{
    char *str = * (char **)pointer;
    long total=0;
    int i;

    for (i=0; i < 15 && str[i] != '\0'; i++)
	total += ((unsigned char) str[i]) * (i+1);
    return total;
}



static int CompareRefStrings (void *a, void *b)
{
    char *s1 = * (char **) a;
    char *s2 = * (char **) b;
    return strcmp(s1,s2);
}


static hashTable refCountTable = NULL;

#if defined(MACINTOSH) || defined(RS6000) || defined(DEC)
char *strdup (char *s)
{
	char *retval = (char *) malloc(strlen(s)+1);
	strcpy (retval, s);
	return retval;
}
#elif defined(_Windows)
char *winStrdup (char *s)
{
	char *retval = (char *) SRGP_malloc(strlen(s)+1);
	strcpy (retval, s);
	return retval;
}
#endif




char *SUIT_createRCString (char *s)
{
    void *ref;
    
    if ((ref = SUIT_HashLookup (refCountTable, &s)) == NULL) {
	char *copy = strdup(s);
	SUIT_HashAdd (refCountTable, &copy);
	ref = SUIT_HashLookup (refCountTable, &copy);
    }
    return (* (char **) ref);
}


void si_initRCString (void)
{
    refCountTable = SUIT_HashCreate (RC_STRING_TABLE_SIZE, sizeof(char *),
				     RC_STRING_TABLE_MAX_COLLISIONS, HashRefStrings, 
				     CompareRefStrings);
}
