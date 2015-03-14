/*

 Copyright (C) 1990,1991 Mark Adler, Richard B. Wales, and Jean-loup Gailly.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as all of the original files are included
 unmodified, that it is not sold for profit, and that this copyright notice
 is retained.

*/

/*
 *  tempf.h by Mark Adler.
 */

/* These "t" functions behave like their "f" counterparts, except that
   topen() takes one character to (possibly) be used in a temporary file
   name, twrite() can create a temporary file, and tclose() will delete
   the temporary file, if any.  tnew() is only defined for use in the
   tputc() macro.  It should not be called explicitly.  These functions
   use the type tFILE instead of FILE to point to a file descriptor. */

#if !defined(OS2) && (defined(M_I86CM) || defined(__COMPACT__))
#   define TMPSIZ  0x8000  /* memory portion of temporary files */
    /* The MSDOS compact model is to be used only for processing a large
     * number of files. In this case we try to reduce the memory requirements.
     * You can reduce TMPSIZ to 16384 or 8192 if 32K is still too large,
     * but the resulting code will be slower.
     */
#else
#   define TMPSIZ  0xe000  /* memory portion of temporary files */
#endif

typedef struct {
  char far *b;          /* memory part of file */
  unsigned p;           /* current read/write pointer for memory part */
  unsigned m;           /* bytes in memory part */
  int c;                /* character to use in spill file name */
  FILE *f;              /* spill file pointer or NULL*/
  char *n;              /* spill file name if f not NULL */
} tFILE;

tFILE *topen OF((int));
int tnew OF((tFILE *));
unsigned twrite OF((char *, unsigned, unsigned, tFILE *));
int tflush OF((tFILE *));
void trewind OF((tFILE *));
unsigned tread OF((char *, unsigned, unsigned, tFILE *));
int terror OF((tFILE *));
int teof OF((tFILE *));
int tclose OF((tFILE *));

#define tputcm(c,t) ((t)->b[(t)->p++]=(c),(t)->m<(t)->p?((t)->m=(t)->p):0,c)
#define tputcf(c,t) ((t)->f==NULL?(tnew(t)?-1:putc(c,(t)->f)):putc(c,(t)->f))
#define tputc(c,t) ((t)->p<TMPSIZ?(int)tputcm(c,t):tputcf(c,t))
