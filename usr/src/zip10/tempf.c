/*

 Copyright (C) 1990,1991 Mark Adler, Richard B. Wales, and Jean-loup Gailly.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as all of the original files are included
 unmodified, that it is not sold for profit, and that this copyright notice
 is retained.

*/

/*
 *  tempf.c by Mark Adler.
 */

#include "tailor.h"
#include "tempf.h"

extern char *tempname OF((int));

#ifdef MODERN
#  include <string.h>
#else /* !MODERN */
   voidp *malloc();
#  ifndef ZMEM
     char *memcpy();
#  endif /* !ZMEM */
#endif /* ?MODERN */
#ifdef ZMEM
   char *memcpy OF((char *, char *, unsigned int));
#endif /* ZMEM */
int unlink OF((char *));


/* Define a special memcpy for MSDOS small model */
#if defined(MSDOS) && (defined(M_I86SM) || defined(__SMALL__))
#  include <dos.h>
#  define memcpy farmemcpy
   void farmemcpy(char far *d, char far *s, unsigned n)
   {
      movedata(FP_SEG(s), FP_OFF(s), FP_SEG(d), FP_OFF(d), n);
   }
#endif


tFILE *topen(c)
int c;
/* Create a new temporary file and return its descriptor.  Save the character
   c to be used in the temporary file name, if needed. */
{
  tFILE *t;

  if ((t = (tFILE *)malloc(sizeof(tFILE))) == NULL ||
      (t->b = farmalloc(TMPSIZ)) == NULL)
    return NULL;
  t->p = t->m = 0;
  t->c = c;
  t->f = NULL;
  return t;
}


int tnew(t)
tFILE *t;               /* temporary file descriptor */
/* Create a temporary file with a unique name */
{
  return (t->n = tempname(t->c)) == NULL ||
         (t->f = fopen(t->n, FOPW)) == NULL;
}


unsigned twrite(b, s, n, t)
char *b;                /* buffer to write from */
unsigned s;             /* size of items */
unsigned n;             /* number of items */
tFILE *t;               /* temporary file descriptor */
/* Like fwrite()--will create a temporary file if needed. */
{
  unsigned j;           /* room in memory, items to write */
  long k;               /* bytes to write */
  long w;               /* bytes written to file */

  /* write to memory portion */
  j = TMPSIZ - t->p;
  k = s * (long) n;
  if (j && k)
  {
    j = (long)j > k ? (unsigned)k : j;
    memcpy(t->b + t->p, (char far *)b, j);
    t->p += j;
    if (t->m < t->p)
      t->m = t->p;
    b += j;
    k -= j;
  }
  if (k == 0)
    return n;

  /* create temporary file if needed */
  if (t->f == NULL && tnew(t))
    return 0;

  /* write to temporary file */
  j = (unsigned)(k / s);
  if (j && fwrite(b, s, j, t->f) != j)
    return 0;
  b += w = s * (long)j;
  k -= w;
  if (k && fwrite(b, (unsigned)k, 1, t->f) != 1)
    return 0;
  return n;
}


int tflush(t)
tFILE *t;               /* temporary file descriptor */
/* Like fflush() */
{
  return t->f == NULL ? 0 : fflush(t->f);
}


void trewind(t)
tFILE *t;               /* temporary file descriptor */
/* Like rewind() */
{
  t->p = 0;
  if (t->f != NULL)
    rewind(t->f);
}


unsigned tread(b, s, n, t)
char *b;                /* buffer to read into */
unsigned s;             /* size of items */
unsigned n;             /* number of items */
tFILE *t;               /* temporary file descriptor */
/* Like fread() */
{
  unsigned j;           /* bytes in memory, items to read */
  long k;               /* bytes requested */
  long r;               /* bytes read from file */

  /* read from memory */
  j = t->m - t->p;
  k = s * (long)n;
  if (j && k)
  {
    j = (long)j > k ? (unsigned)k : j;
    memcpy((char far *)b, t->b + t->p, j);
    t->p += j;
    b += j;
    k -= j;
  }

  /* read from file if more requested */
  if (k && t->f != NULL)
  {
    j = (unsigned)(k / s);
    if (j)
    {
      r = s * (long)fread(b, s, j, t->f);
      b += r;
      k -= r;
    }
    if (k && k < s)
      k -= fread(b, 1, (unsigned)k, t->f);
  }

  /* return complete items read */
  return n - (unsigned)((k + s - 1) / s);
}


int terror(t)
tFILE *t;               /* temporary file descriptor */
/* Like ferror() */
{
  return t->f == NULL ? 0 : ferror(t->f);
}


int teof(t)
tFILE *t;               /* temporary file descriptor */
/* Like feof() */
{
  return t->f == NULL ? t->p == t->m : feof(t->f);
}


int tclose(t)
tFILE *t;               /* temporary file descriptor */
/* Like fclose()--frees the memory used by the descriptor and deletes
   the temporary file, if any. */
{
  int r;

  r = 0;
  if (t->f != NULL)
  {
    r = fclose(t->f);
    unlink(t->n);
    free(t->n);
  }
  farfree(t->b);
  free(t);
  return r;
}
