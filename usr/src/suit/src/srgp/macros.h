/** MEMORY ALLOCATION MACROS
Handles differences between Macintosh and UNIX.
**/

#ifdef THINK_C
#define MALLOCARGTYPE   size_t
#else
#define MALLOCARGTYPE   unsigned
#endif

#define ALLOC_RECORDS(PTR,TYPE,NUM) \
   if ((PTR = (TYPE *) malloc((MALLOCARGTYPE)((NUM)*sizeof(TYPE)))) == NULL) \
      REPORT_ERROR (ERR_MALLOC); else



/** TRANSLATING BSD TO SYSTEM V **/

#if defined(THINK_C) || defined(__MSDOS__)
#define bzero(PTR,NUMBYTES)  memset(PTR,0,(size_t)NUMBYTES)
#define bcopy(SRC,DEST,NUMBYTES)  memcpy(DEST,SRC,(size_t)NUMBYTES)
#define bcmp(B1,B2,NUMBYTES)   memcmp(B1,B2,(size_t)NUMBYTES)
#define irint   /*nothing: very risky! */
#endif
