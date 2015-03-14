/*

 Copyright (C) 1990,1991 Mark Adler, Richard B. Wales, and Jean-loup Gailly.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as all of the original files are included
 unmodified, that it is not sold for profit, and that this copyright notice
 is retained.

*/

/*
 *  crypt.h by Mark Adler.
 */

/* Set up portability */
#include "tailor.h"

/* Define zfwrite() and zputc() functions */
#ifdef EXPORT
#  define zfwrite fwrite
#  define zputc putc
#else /* !EXPORT */
   extern int zfwrite OF((voidp *, extent, extent, FILE *));
   extern int zfputc OF((int, FILE *));
   extern char *key;
#  define zputc(b,f) (key!=NULL?zfputc(b,f):putc(b,f))
#endif /* ?EXPORT */

/* The implode routines now use the same temporary name generator */
char *tempname OF((int));

/* I'm sneaking this in on Rich's code to make my compiler a bit happier */
#ifdef NeXT
   extern void free(voidp *);
   extern voidp *qsort(voidp *, extent, extent, int (*)());
   extern extent strlen(char *);
   extern int unlink(char *);
#endif /* NeXT */
