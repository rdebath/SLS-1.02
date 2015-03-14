/*
** General purpose hashing library
*/


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



/* The table is increased in size once PERCENT_REHASH percent of the entries
   has more than one entry. */
#define PERCENT_REHASH 70

typedef void *elementType;

typedef struct tableEntryStruct {
  elementType element;
  struct tableEntryStruct *next;
} tableEntry;

typedef int hashFuncType(elementType element);
typedef int comparFuncType(elementType el1, elementType el2);
typedef int traverseFuncType(elementType element);

typedef struct hashTableStruct {
  int			size;
  int			elsize;
  int			numCollisions;
  int			maxNumCollisions;
  int			inTraverse; /* really is a boolean */
  int			currentIndex;
  tableEntry		*currentTableEntry;
  hashFuncType 		*hashFunc;
  comparFuncType 	*comparFunc;
  tableEntry 		**table;
} *hashTable;


/*
** Error Messages:
*/
typedef enum { 	HASH_REPLY_NOERROR 		= 0,
		HASH_ALREADY_ADDED	= 1,
		HASH_NOT_FOUND		= 2,
		HASH_OUT_OF_MEMORY	= 3,
		HASH_FREE_ERROR		= 4,
		HASH_INVALID_ARGUMENT	= 5,
		HASH_INVALID_OPERATION	= 6
} hashRetVal;

extern char *HashErrorMessages[];

extern hashRetVal hashError;

/*
HashCreate returns NULL if the table could not be created.
HashDestroy, HashAdd, and HashDelete return 0 if successful or non-zero
if an error occurred.
HashLookup returns NULL if the element was not found.
*/

/* Basic functions */

hashTable HashCreate(int entries, int elsize, int maxNumCollisions, hashFuncType *hf, comparFuncType *cf);
/* When more than maxNumCollisions collisions have occurred, the table's
   size is doubled.
   (Note that this entails rehashing all of the elements in the table.) */
int HashDestroy(hashTable ht);
int HashAdd(hashTable ht, elementType el);
int HashAddWithoutCopy(hashTable ht, elementType el);
int HashDelete(hashTable ht, elementType el);
elementType HashLookup(hashTable ht, elementType el);
elementType HashUnusualLookup(hashTable ht, elementType el, comparFuncType *cf);

/* Traversal Functions: */

void HashTraverse(hashTable ht, traverseFuncType *tf);
elementType HashFirstEntry(hashTable ht);
elementType HashNextEntry(hashTable ht);

/* 
   NOTES about traversal functions:

   The tf parameter for HashTraverse should return TRUE or FALSE
   depending on whether or not HashTraverse should continue to traverse
   the hash table.  (i.e. TRUE means continue, FALSE means stop)

   While in HashTraverse, modifications to the hash table are not
   allowed.  That is, HashDelete or HashAdd (or HashAddWithoutCopy)
   will all return HASH_INVALID_OPERATION if they are called on a hash
   table on which HashTraverse is executing at that time.

   HashFirstEntry returns the first element in the hash table, and also
   resets the internal pointer used by HashNextEntry.
   Also note that an addition or deletion could cause HashNextEntry to
   either skip or include undesired elements.  Therefore we recommend
   that after an addition or deletion HashFirstEntry be called before
   the next call to HashNextEntry.

*/
