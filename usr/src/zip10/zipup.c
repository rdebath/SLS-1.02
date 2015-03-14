/*

 Copyright (C) 1990,1991 Mark Adler, Richard B. Wales, and Jean-loup Gailly.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as all of the original files are included
 unmodified, that it is not sold for profit, and that this copyright notice
 is retained.

*/

/*
 *  zipup.c by Mark Adler.
 */

#define NOCPYRT         /* this is not a main module */
#include "zip.h"
#include "revision.h"

/* Use the raw functions for MSDOS and Unix to save on buffer space.
   They're not used for VMS since it doesn't work (raw is weird on VMS).
   (This sort of stuff belongs in fileio.c, but oh well.) */
#ifdef VMS
   typedef FILE *ftype;
#  define fhow FOPR
#  define fbad NULL
#  define zopen(n,p) fopen(n,p)
#  define zread(f,b,n) fread(b,1,n,f)
#  define zclose(f) fclose(f)
#  define zerr(f) ferror(f)
#  define zrew(f) rewind(f)
#else /* !VMS */
#  ifdef MSDOS
#    include <io.h>
#    include <fcntl.h>
#    define fhow (O_RDONLY|O_BINARY)
#  else /* !MSDOS */
     int open OF((char *, int));
     int read OF((int, char *, int));
     int close OF((int));
     int lseek OF((int,long,int));
#    define fhow 0
#  endif /* ?MSDOS */
   typedef int ftype;
#  define fbad (-1)
#  define zopen(n,p) open(n,p)
#  define zread(f,b,n) read(f,b,n)
#  define zclose(f) close(f)
#  define zerr(f) (k<0)
#  define zrew(f) lseek(f,0L,0)
#endif /* ?VMS */


/* Local functions */
#ifdef PROTO
#  ifndef UTIL
     local int suffixes(char *, char *);
#  endif /* !UTIL */
#endif /* PROTO */


/* Note: a zip "entry" includes a local header (which includes the file
   name), an encryption header if encrypting, and the compressed data. */


int zipcopy(z, x, y)
struct zlist far *z;    /* zip entry to copy */
FILE *x, *y;            /* source and destination files */
/* Copy the zip entry described by *z from file *x to file *y.  Return an
   error code in the ZE_ class.  */
{
  ulg n;                /* holds local header offset */

  if (fseek(x, z->off, SEEK_SET))
    return ferror(x) ? ZE_READ : ZE_EOF;
  if ((n = ftell(y)) == -1L)
    return ZE_TEMP;
  z->off = n;
  n = 4 + LOCHEAD + (long)z->nam + (long)z->ext + z->siz;
  return fcopy(x, y, n);
}


#ifndef UTIL

int percent(n, m)
ulg n, m;               /* n is the original size, m is the new size */
/* Return the percentage compression from n to m using only integer
   operations */
{
  if (n > 0xffffffL)            /* If n >= 16M */
  {                             /*  then divide n and m by 256 */
    n += 0x80;  n >>= 8;
    m += 0x80;  m >>= 8;
  }
  return n ? (int)(1 + (200 * (n - m))/n) >> 1 : 0;
}


local int suffixes(a, s)
char *a;                /* name to check suffix of */
char *s;                /* list of suffixes separated by : or ; */
/* Return true if a ends in any of the suffixes in the list s. */
{
  int m;                /* true if suffix matches so far */
  char *p;              /* pointer into special */
  char *q;              /* pointer into name a */

  m = 1;
  q = a + strlen(a) - 1;
  for (p = s + strlen(s) - 1; p >= s; p--)
    if (*p == ':' || *p == ';')
      if (m)
        return 1;
      else
      {
        m = 1;
        q = a + strlen(a) - 1;
      }
    else
#ifdef OS2
    {
      m = m && q >= a && tolower(*p) == tolower(*q);
      q--;
    }
#else /* !OS2 */
      m = m && q >= a && *p == *q--;
#endif /* ?OS2 */
  return m;
}


int zipup(z, y)
struct zlist far *z;    /* zip entry to compress */
FILE *y;                /* output file */
/* Compress the file z->name into the zip entry described by *z and write
   it to the file *y.  Determine the best method (store, shrink, or implode)
   and use it.  Encrypt if requested.  Return an error code in the
   ZE_ class. */
{
  ulg a;                /* attributes returned by filetime() */
  char *b;              /* malloc'ed file buffer */
  ulg c;                /* crc on uncompressed file data */
  ftype f;              /* file to compress */
  uch g;                /* flags returned by implode */
  int h;                /* compression method chosen (how) */
#ifndef NOIMPLODE
  ulg i;                /* size of imploded data */
#endif /* !NOIMPLODE */
  extent k;             /* result of zread */
  int l;                /* true if this file is a symbolic link */
  int m;                /* method for this entry */
  ulg n;                /* size of uncompressed file */
  long q;               /* size returned by filetime */
  int r;                /* temporary variable */
  ulg s;                /* size of shrunk or compressed data */

  /* Open file to zip up */
  if ((z->tim = filetime(z->name, &a, &q)) == 0 || q < 0)
    return ZE_OPEN;
  l = issymlnk(a);
  if (l)
    f = fbad;
  else if ((f = zopen(z->name, fhow)) == fbad)
    return ZE_OPEN;

  /* Select method based on the suffix and the global method */
  m = special != NULL && suffixes(z->name, special) ? STORE : method;

  /* Don't bother with shrink for "large" files (320 is what PKZIP uses--the
     number used here must be <= BSZ) */
  if (m == BEST && q > 512)
    m = IMPLODE;
  if (q == 0)
    m = STORE;

  /* Make first pass on the file, computing the crc and length, and running
     shrink and implode on it. */
  n = 0;
  c = updcrc((char *)NULL, 0);
  if ((b = malloc(BSZ)) == NULL)
    return ZE_MEM;
  if (m == BEST || m == SHRINK)
    if ((r = shr_setup()) != ZE_OK)
      return r;
    else
      shract = 1;
  while ((k = l ? rdsymlnk(z->name, b, BSZ) : zread(f, b, BSZ)) > 0)
  {
    n += k;
    c = updcrc(b, k);
#ifdef MINIX
    if (l)
      q = k;
#endif /* MINIX */
    if ((m == BEST || m == SHRINK) && (r = shr_p1((uch *)b, k)) != ZE_OK)
    {
      free((voidp *)b);
      shr_clear();
      shract = 0;
      return r;
    }
    if (m == BEST)              /* free up shrink data structures */
    {
      if ((!l && zread(f, b, BSZ) != 0)
#ifndef VMS
          || n != (ulg)q
#endif /* !VMS */
         )
        return ZE_READ;
      if ((r = shr_size(&s)) != ZE_OK)
      {
        shr_clear();
        shract = 0;
        return r;
      }
    }
#ifndef NOIMPLODE
    if (m == BEST || m == IMPLODE)
    {
      if (!impact)
        if ((r = imp_setup(q, level)) != ZE_OK)
          return r;
        else
          impact = 1;
      if ((r = imp_p1(b, k)) != ZE_OK)
      {
        free((voidp *)b);
        imp_clear();
        impact = 0;
        return r;
      }
    }
#endif /* !NOIMPLODE */
    if (m == BEST || l)
      break;
  }
  free((voidp *)b);
  if (!l && zerr(f))
    return ZE_READ;

  /* Determine the best method to use */
  g = 0;
  if (noisy && verbose)
    printf(" (n=%lu)", n);
  if (m == SHRINK && (r = shr_size(&s)) != ZE_OK)
  {
    shr_clear();
    shract = 0;
    return r;
  }
  if (noisy && verbose && (m == BEST || m == SHRINK))
    printf(" (s=%lu)", s);
#ifndef NOIMPLODE
  if ((m == BEST || m == IMPLODE) && (r = imp_size(&i, &g)) != ZE_OK)
  {
    imp_clear();
    impact = 0;
    return r;
  }
  if (noisy && verbose && (m == BEST || m == IMPLODE))
    printf(" (i=%lu)", i);
  if ((m == BEST || m == IMPLODE) && i < n && (m == IMPLODE || i < s))
  {
    if (noisy)
    {
      printf(" (imploded %d%%)", percent(n, i));
      fflush(stdout);
    }
    h = IMPLODE;
    s = i;
    if (m == BEST)
    {
      shr_clear();
      shract = 0;
    }
    if (!l)
      zclose(f);
  }
  else
#endif /* !NOIMPLODE */
  if ((m == BEST || m == SHRINK) && s < n)
  {
    if (noisy)
    {
      printf(" (shrunk %d%%)", percent(n, s));
      fflush(stdout);
    }
    h = SHRINK;
#ifndef NOIMPLODE
    if (m == BEST)
    {
      imp_clear();
      impact = 0;
    }
#endif /* !NOIMPLODE */
    if (!l)
      zclose(f);
  }
  else
  {
    if (noisy)
    {
      printf(" (stored 0%%)");
      fflush(stdout);
    }
    h = STORE;
    s = n;
#ifndef NOIMPLODE
    if (m == BEST || m == IMPLODE)
    {
      imp_clear();
      impact = 0;
    }
#endif /* !NOIMPLODE */
    if (m == BEST || m == SHRINK)
    {
      shr_clear();
      shract = 0;
    }
  }

#ifndef VMS
  /* Check size (but not in VMS--variable record lengths mess it up) */
  if (n != (ulg)q)
    return ZE_READ;
#endif /* !VMS */

  /* Fill in header information and write local header to zip file */
#ifdef OS2
  if ( dosify < 2 )
    dosify = IsFileSystemFAT(z->name);
#endif /* OS2 */

  /* (Assume ext, cext, com, and zname already filled in.) */
  z->vem = dosify ? 11 :                /* Made under MSDOS by PKZIP 1.1 */
#ifdef VMS
                    0x200 + REVISION;   /* Made under VMS by this Zip */
#else /* !VMS */
#ifdef OS2
                    0x600 + REVISION;   /* Made under OS/2 by this Zip */
#else /* !OS2 */
                    0x300 + REVISION;   /* Made under Unix by this Zip */
#endif /* ?OS2 */
#endif /* ?VMS */
  z->ver = 10;                          /* Need PKUNZIP 1.0 */
  z->flg = (ush)g;
  if (key != NULL)
    z->flg |= 1;
  z->lflg = z->flg;
  z->how = h;
  z->crc = c;
  z->siz = s;
  if (key != NULL)
    z->siz += 12;
  z->len = n;
  z->nam = strlen(z->zname);
  z->dsk = 0;
  z->att = 0;                           /* Assume they're all binary */
  z->atx = dosify ? a & 0xff : a;       /* Attributes from filetime() */
  if ((z->off = ftell(y)) == -1L)
    return ZE_WRITE;
  if ((r = putlocal(z, y)) != ZE_OK)
    return r;

  /* Write stored, shrunk, or imploded file to zip file */
#ifndef EXPORT
  if (key != NULL)
    crypthead(key, z->crc, y);
#endif /* !EXPORT */
  n = ftell(y);                         /* Save offset for logic check */
#ifndef NOIMPLODE
  if (h == IMPLODE)
  {
    if ((r = imp_p2(y)) != ZE_OK)
      return r;
    imp_clear();
    impact = 0;
  }
  else
#endif /* !NOIMPLODE */
  if (h == SHRINK)
  {
    if ((r = shr_p2(y)) != ZE_OK)
      return r;
    shr_clear();
    shract = 0;
  }
  else
  {
    if (!l)
      zrew(f);
    if ((b = malloc(BSZ)) == NULL)
      return ZE_MEM;
    while ((k = l ? rdsymlnk(z->name, b, BSZ) : zread(f, b, BSZ)) > 0)
    {
      if (zfwrite(b, 1, k, y) != k)
      {
        free((voidp *)b);
        return ZE_TEMP;
      }
      if (l)
        break;
    }
    free((voidp *)b);
    if (!l && zerr(f))
      return ZE_READ;
    if (!l)
      zclose(f);
  }

  /* Check length of compressed data */
  if (ftell(y) != n + s)
    return ZE_LOGIC;

  /* Done--clean up and leave */
  if (noisy)
  {
    putchar('\n');
    fflush(stdout);
  }
  return ZE_OK;
}

#endif /* !UTIL */
