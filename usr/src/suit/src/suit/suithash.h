#include "hash.h"

#if defined(MACINTOSH) || defined(RS6000) || defined(DEC)
char *strdup (char *s);
#endif

char *SUIT_createRCString (char *s);
void si_initRCString (void);

hashTable SUIT_HashCreate(int entries, int elsize, int maxNumCollisions,
			  hashFuncType *hf, comparFuncType *cf);
void SUIT_HashDestroy(hashTable ht);
void SUIT_HashAdd(hashTable ht, elementType el);
void SUIT_HashAddWithoutCopy(hashTable ht, elementType el);
void SUIT_HashAddAllowingDuplicates(hashTable ht, elementType el);
void SUIT_HashAddWithoutCopyAllowingDuplicates(hashTable ht, elementType el);
void SUIT_HashDelete(hashTable ht, elementType el);
#define SUIT_HashLookup HashLookup
#define SUIT_HashUnusualLookup HashUnusualLookup

#define SUIT_HashTraverse HashTraverse
#define SUIT_HashFirstEntry HashFirstEntry
#define SUIT_HashNextEntry HashNextEntry
