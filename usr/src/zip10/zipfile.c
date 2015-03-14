/*

 Copyright (C) 1990,1991 Mark Adler, Richard B. Wales, and Jean-loup Gailly.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as all of the original files are included
 unmodified, that it is not sold for profit, and that this copyright notice
 is retained.

*/

/*
 *  zipfile.c by Mark Adler.
 */

#include "zip.h"


/* Macros for converting integers in little-endian to machine format */
#define SH(a) (((ush)(uch)(a)[0]) | (((ush)(uch)(a)[1]) << 8))
#define LG(a) ((ulg)SH(a) | ((ulg)SH((a)+2) << 16))

/* Macros for writing machine integers to little-endian format */
#define PUTSH(a,f) {putc((char)(a),(f)); putc((char)((a) >> 8),(f));}
#define PUTLG(a,f) {PUTSH(a,f) PUTSH((a) >> 16,f)}


/* -- Structure of a ZIP file -- */

/* Signatures for zip file information headers */
#define LOCSIG  0x04034b50L
#define CENSIG  0x02014b50L
#define ENDSIG  0x06054b50L

/* Offsets of values in headers */
#define LOCVER  0               /* version needed to extract */
#define LOCFLG  2               /* encrypt, implosion flags */
#define LOCHOW  4               /* compression method */
#define LOCTIM  6               /* last modified file time, DOS format */
#define LOCDAT  8               /* last modified file date, DOS format */
#define LOCCRC  10              /* uncompressed crc-32 for file */
#define LOCSIZ  14              /* compressed size in zip file */
#define LOCLEN  18              /* uncompressed size */
#define LOCNAM  22              /* length of filename */
#define LOCEXT  24              /* length of extra field */

#define CENVEM  0               /* version made by */
#define CENVER  2               /* version needed to extract */
#define CENFLG  4               /* encrypt, implosion flags */
#define CENHOW  6               /* compression method */
#define CENTIM  8               /* last modified file time, DOS format */
#define CENDAT  10              /* last modified file date, DOS format */
#define CENCRC  12              /* uncompressed crc-32 for file */
#define CENSIZ  16              /* compressed size in zip file */
#define CENLEN  20              /* uncompressed size */
#define CENNAM  24              /* length of filename */
#define CENEXT  26              /* length of extra field */
#define CENCOM  28              /* file comment length */
#define CENDSK  30              /* disk number start */
#define CENATT  32              /* internal file attributes */
#define CENATX  34              /* external file attributes */
#define CENOFF  38              /* relative offset of local header */

#define ENDDSK  0               /* number of this disk */
#define ENDBEG  2               /* number of the starting disk */
#define ENDSUB  4               /* entries on this disk */
#define ENDTOT  6               /* total number of entries */
#define ENDSIZ  8               /* size of entire central directory */
#define ENDOFF  12              /* offset of central on starting disk */
#define ENDCOM  16              /* length of zip file comment */


/* Local functions */
#ifdef PROTO
   local int zqcmp(voidp *, voidp *);
#  ifndef UTIL
     local int zbcmp(voidp *, voidp far *);
     local char *cutpath(char *);
#  endif /* !UTIL */
#endif /* PROTO */


local int zqcmp(a, b)
voidp *a, *b;           /* pointers to pointers to zip entries */
/* Used by qsort() to compare entries in the zfile list.  */
{
  return strcmp((*(struct zlist far **)a)->zname,
                (*(struct zlist far **)b)->zname);
}


#ifndef UTIL

local int zbcmp(n, z)
voidp *n;               /* string to search for */
voidp far *z;           /* pointer to a pointer to a zip entry */
/* Used by search() to compare a target to an entry in the zfile list. */
{
#ifdef OS2
  return stricmp((char *)n, ((struct zlist far *)z)->zname);
#else /* !OS2 */
  return strcmp((char *)n, ((struct zlist far *)z)->zname);
#endif /* ?OS2 */
}


struct zlist far *zsearch(n)
char *n;                /* name to find */
/* Return a pointer to the entry in zfile with the name n, or NULL if
   not found. */
{
  voidp far **p;        /* result of search() */

  if (zcount && (p = search(n, (voidp far **)zsort, zcount, zbcmp)) != NULL)
    return *(struct zlist far **)p;
  else
    return NULL;
}

#endif /* !UTIL */


#ifdef VMS
#  define PATHCUT ']'
#else /* !VMS */
#  define PATHCUT '/'
#endif /* VMS */

char *ziptyp(s)
char *s;                /* file name to force to zip */
/* If the file name *s has a dot (other than the first char), then return
   the name, otherwise append .zip to the name.  Allocate the space for
   the name in either case.  Return a pointer to the new name, or NULL
   if malloc() fails. */
{
  char *q;              /* temporary pointer */
  char *t;              /* pointer to malloc'ed string */

  if ((t = malloc(strlen(s) + 5)) == NULL)
    return NULL;
  strcpy(t, s);
#ifdef MSDOS
  for (q = t; *q; q++)
    if (*q == '\\')
      *q = '/';
#endif /* MSDOS */
  if (strrchr((q = strrchr(t, PATHCUT)) == NULL ? t : q + 1, '.') == NULL)
    strcat(t, ".zip");
#ifdef MSDOS
#ifndef OS2
  strupr(t);
#endif /* !OS2 */
#endif /* MSDOS */
  return t;
}


int readzipfile()
/*
   Make first pass through zip file, reading information from local file
   headers and then verifying that information with the central file
   headers.  Any deviation from the expected zip file format returns an
   error.  At the end, a sorted list of file names in the zip file is made
   to facilitate searching by name.

   The name of the zip file is pointed to by the global "zipfile".  The
   globals zfiles, zcount, zcomlen, zcomment, and zsort are filled in.
   Return an error code in the ZE_ class.
*/
{
  char b[CENHEAD];      /* buffer for central headers */
  FILE *f;              /* zip file */
  int m;                /* mismatch flag */
  extent n;             /* length of name */
  ulg p;                /* current file offset */
  char r;               /* holds reserved bits during memcmp() */
  ulg s;                /* size of data, start of central */
  char *t;              /* temporary variable */
  char far *u;          /* temporary variable */
  struct zlist far * far *x;    /* pointer last entry's link */
  struct zlist far *z;  /* current zip entry structure */

  /* Initialize zip file info */
  zipbeg = 0;
  zfiles = NULL;                        /* Points to first header */
  zcomlen = 0;                          /* zip file comment length */

  /* If zip file exists, read headers and check structure */
  if ((f = fopen(zipfile, FOPR)) != NULL)
  {
    x = &zfiles;                        /* first link */
    p = 0;                              /* starting file offset */
    zcount = 0;                         /* number of files */

    /* Find start of zip structures */
    while (fread(b, 4, 1, f) == 1 && (s = LG(b)) != LOCSIG && s != ENDSIG)
      if (fseek(f, -3L, SEEK_CUR))
        return ferror(f) ? ZE_READ : ZE_EOF;
      else
        p++;
    zipbeg = p;

    /* Read local headers */
    while (LG(b) == LOCSIG)
    {
      /* Read local header raw to compare later with central header 
         (this requires that the offest of ext in the zlist structure
         be greater than or equal to LOCHEAD) */
      if ((z = (struct zlist far *)farmalloc(sizeof(struct zlist))) == NULL)
        return ZE_MEM;
      if (fread(b, LOCHEAD, 1, f) != 1)
        return ferror(f) ? ZE_READ : ZE_EOF;
      t = b;  u = (char far *)z;  n = LOCHEAD;
      do {
        *u++ = *t++;
      } while (--n);

      /* Link into list */
      *x = z;
      z->nxt = NULL;
      x = &z->nxt;

      /* Read file name and extra field and skip data */
      n = SH(LOCNAM + (uch far *)z);
      z->ext = SH(LOCEXT + (uch far *)z);
      s = LG(LOCSIZ + (uch far *)z);
      if (n == 0)
      {
        sprintf(errbuf, "%d", zcount + 1);
        warn("zero-length name for entry #", errbuf);
        return ZE_FORM;
      }
      if ((z->zname = malloc(n+1)) ==  NULL ||
          (z->ext && (z->extra = malloc(z->ext)) == NULL))
        return ZE_MEM;
      if (fread(z->zname, n, 1, f) != 1 ||
          (z->ext && fread(z->extra, z->ext, 1, f) != 1) ||
          fseek(f, (long)s, SEEK_CUR))
        return ferror(f) ? ZE_READ : ZE_EOF;
      z->zname[n] = 0;                  /* terminate name */
#ifdef UTIL
      z->name = z->zname;
#else /* !UTIL */
      z->name = in2ex(z->zname);        /* convert to external name */
      if (z->name == NULL)
        return ZE_MEM;
#endif /* ?UTIL */

      /* Save offset, update for next header */
      z->off = p;
      p += 4 + LOCHEAD + n + z->ext + s;
      zcount++;

      /* Read next signature */
      if (fread(b, 4, 1, f) != 1)
        return ferror(f) ? ZE_READ : ZE_EOF;
    }

    /* Point to start of header list and read central headers */
    z = zfiles;
    s = p;                              /* save start of central */
    while (LG(b) == CENSIG)
    {
      if (z == NULL)
      {
        warn("extraneous central header signature", "");
        return ZE_FORM;
      }

      /* Read central header */
      if (fread(b, CENHEAD, 1, f) != 1)
        return ferror(f) ? ZE_READ : ZE_EOF;

      /* Compare local header with that part of central header (except
         for the reserved bits in the general purpose flags and except
         for length of extra fields--authentication can make these
         different in central and local headers) */
      z->lflg = SH(LOCFLG + (uch far *)z);      /* Save reserved bits */
      r = b[CENFLG+1];
      ((uch far *)z)[LOCFLG+1] &= 0x1f; /* Zero out reserved bits */
      b[CENFLG+1] &= 0x1f;
      for (m = 0, u = (char far *)z, n = 0; n < LOCHEAD - 2; n++)
        if (u[n] != b[n+2])
        {
          if (!m)
            warn("local and central headers differ for ", z->zname);
          m = 1;
          sprintf(errbuf, " offset %d--local = %02x, central = %02x",
                  n, (uch)u[n], (uch)b[n+2]);
          warn(errbuf, "");
        }
      if (m)
        return ZE_FORM;
      b[CENFLG+1] = r;                  /* Restore reserved bits */

      /* Overwrite local header with translated central header */
      z->vem = SH(CENVEM + b);
      z->ver = SH(CENVER + b);
      z->flg = SH(CENFLG + b);          /* may be different from z->lflg */
      z->how = SH(CENHOW + b);
      z->tim = LG(CENTIM + b);          /* time and date into one long */
      z->crc = LG(CENCRC + b);
      z->siz = LG(CENSIZ + b);
      z->len = LG(CENLEN + b);
      z->nam = SH(CENNAM + b);
      z->cext = SH(CENEXT + b);         /* may be different from z->ext */
      z->com = SH(CENCOM + b);
      z->dsk = SH(CENDSK + b);
      z->att = SH(CENATT + b);
      z->atx = LG(CENATX + b);
      if (z->off != LG(CENOFF + b))
      {
        warn("local offset in central header incorrect for ", z->zname);
        return ZE_FORM;
      }

      /* Compare name and extra fields and read comment field */
      if ((t = malloc(z->nam)) == NULL)
        return ZE_MEM;
      if (fread(t, z->nam, 1, f) != 1)
      {
        free((voidp *)t);
        return ferror(f) ? ZE_READ : ZE_EOF;
      }
      if (memcmp(t, z->zname, z->nam))
      {
        free((voidp *)t);
        warn("names in local and central differ for ", z->zname);
        return ZE_FORM;
      }
      free((voidp *)t);
      if (z->cext)
      {
        if ((z->cextra = malloc(z->cext)) == NULL)
          return ZE_MEM;
        if (fread(z->cextra, z->cext, 1, f) != 1)
        {
          free((voidp *)(z->cextra));
          return ferror(f) ? ZE_READ : ZE_EOF;
        }
        if (z->ext == z->cext && memcmp(z->extra, z->cextra, z->ext) == 0)
        {
          free((voidp *)(z->cextra));
          z->cextra = z->extra;
        }
      }
      if (z->com)
      {
        if ((z->comment = malloc(z->com)) == NULL)
          return ZE_MEM;
        if (fread(z->comment, z->com, 1, f) != 1)
        {
          free((voidp *)(z->comment));
          return ferror(f) ? ZE_READ : ZE_EOF;
        }
      }

      /* Note oddities */
      if (verbose)
      {
        if (z->vem != 10 && z->vem != 11 &&
            (n = z->vem >> 8) != 3 && n != 2 && n != 6)
        {
          sprintf(errbuf, "made by version %d.%d on system type %d: ",
                  (z->vem & 0xff) / 10, (z->vem & 0xff) % 10, z->vem >> 8);
          warn(errbuf, z->zname);
        }
        if (z->ver != 10 && z->ver != 11)
        {
          sprintf(errbuf, "needs unzip %d.%d on system type %d: ",
                  (z->ver & 0xff) / 10, (z->ver & 0xff) % 10, z->ver >> 8);
          warn(errbuf, z->zname);
        }
        if (z->flg != z->lflg)
        {
          sprintf(errbuf, "local flags = 0x%04x, central = 0x%04x: ",
                  z->lflg, z->flg);
          warn(errbuf, z->zname);
        }
        else if (z->flg & ~7)
        {
          sprintf(errbuf, "undefined bits used in flags = 0x%04x: ", z->flg);
          warn(errbuf, z->zname);
        }
        if (z->how > IMPLODE)
        {
          sprintf(errbuf, "unknown compression method %u: ", z->how);
          warn(errbuf, z->zname);
        }
        if (z->dsk)
        {
          sprintf(errbuf, "starts on disk %u: ", z->dsk);
          warn(errbuf, z->zname);
        }
        if (z->att & ~1)
        {
          sprintf(errbuf, "unknown internal attributes = 0x%04x: ", z->att);
          warn(errbuf, z->zname);
        }
        if (((n = z->vem >> 8) != 3) && n != 2 && z->atx & ~0xffL)
        {
          sprintf(errbuf, "unknown external attributes = 0x%08lx: ", z->atx);
          warn(errbuf, z->zname);
        }
        if (z->ext || z->cext)
          if (z->ext == z->cext && z->extra == z->cextra)
          {
            sprintf(errbuf, "has %d bytes of extra data: ", z->ext);
            warn(errbuf, z->zname);
          }
          else
          {
            sprintf(errbuf,
                    "local extra (%d bytes) != central extra (%d bytes): ",
                    z->ext, z->cext);
            warn(errbuf, z->zname);
          }
      }

      /* Clear actions */
      z->mark = 0;
      z->trash = 0;

      /* Update file offset */
      p += 4 + CENHEAD + z->nam + z->cext + z->com;

      /* Advance to next header structure */
      z = z->nxt;

      /* Read next signature */
      if (fread(b, 4, 1, f) != 1)
        return ferror(f) ? ZE_READ : ZE_EOF;
    }
    
    /* Read end header */
    if (z != NULL || LG(b) != ENDSIG)
    {
      warn("missing end signature--probably not a zip file (did you", "");
      warn("remember to use binary mode when you transferred it?)", "");
      return ZE_FORM;
    }
    if (fread(b, ENDHEAD, 1, f) != 1)
      return ferror(f) ? ZE_READ : ZE_EOF;
    if (SH(ENDDSK + b) || SH(ENDBEG + b) ||
        SH(ENDSUB + b) != SH(ENDTOT + b))
      warn("multiple disk information ignored", "");
    if (zcount != SH(ENDSUB + b))
    {
      warn("count in end of central directory incorrect", "");
      return ZE_FORM;
    }
    if (LG(ENDSIZ + b) != p - s)
    {
      warn("central directory size is incorrect", "");
      return ZE_FORM;
    }
    if (LG(ENDOFF + b) != s)
    {
      warn("central directory start is incorrect", "");
      return ZE_FORM;
    }
    cenbeg = s;
    zcomlen = SH(ENDCOM + b);
    if (zcomlen)
    {
      if ((zcomment = malloc(zcomlen)) == NULL)
        return ZE_MEM;
      if (fread(zcomment, zcomlen, 1, f) != 1)
      {
        free((voidp *)zcomment);
        return ferror(f) ? ZE_READ : ZE_EOF;
      }
    }
    if (zipbeg)
    {
      sprintf(errbuf, " has a preamble of %ld bytes", zipbeg);
      warn(zipfile, errbuf);
    }
    if (getc(f) != EOF)
      warn("garbage at end of zip file ignored", "");

    /* Done with zip file for now */
    fclose(f);
    
    /* If one or more files, sort by name */
    if (zcount)
    {
      if ((x = zsort =
          (struct zlist far **)malloc(zcount * sizeof(struct zlist far *))) ==
          NULL)
        return ZE_MEM;
      for (z = zfiles; z != NULL; z = z->nxt)
        *x++ = z;
      qsort((char *)zsort, zcount, sizeof(struct zlist far *), zqcmp);
    }
  }
  return ZE_OK;
}


int putlocal(z, f)
struct zlist far *z;    /* zip entry to write local header for */
FILE *f;                /* file to write to */
/* Write a local header described by *z to file *f.  Return an error code
   in the ZE_ class. */
{
  PUTLG(LOCSIG, f);
  PUTSH(z->ver, f);
  PUTSH(z->lflg, f);
  PUTSH(z->how, f);
  PUTLG(z->tim, f);
  PUTLG(z->crc, f);
  PUTLG(z->siz, f);
  PUTLG(z->len, f);
  PUTSH(z->nam, f);
  PUTSH(z->ext, f);
  if (fwrite(z->zname, 1, z->nam, f) != z->nam ||
      z->ext && fwrite(z->extra, 1, z->ext, f) != z->ext)
    return ZE_TEMP;
  return ZE_OK;
}


int putcentral(z, f)
struct zlist far *z;    /* zip entry to write central header for */
FILE *f;                /* file to write to */
/* Write a central header described by *z to file *f.  Return an error code
   in the ZE_ class. */
{
  PUTLG(CENSIG, f);
  PUTSH(z->vem, f);
  PUTSH(z->ver, f);
  PUTSH(z->flg, f);
  PUTSH(z->how, f);
  PUTLG(z->tim, f);
  PUTLG(z->crc, f);
  PUTLG(z->siz, f);
  PUTLG(z->len, f);
  PUTSH(z->nam, f);
  PUTSH(z->cext, f);
  PUTSH(z->com, f);
  PUTSH(z->dsk, f);
  PUTSH(z->att, f);
  PUTLG(z->atx, f);
  PUTLG(z->off, f);
  if (fwrite(z->zname, 1, z->nam, f) != z->nam ||
      z->cext && fwrite(z->cextra, 1, z->cext, f) != z->cext ||
      z->com && fwrite(z->comment, 1, z->com, f) != z->com)
    return ZE_TEMP;
  return ZE_OK;
}


int putend(n, s, c, m, z, f)
int n;                  /* number of entries in central directory */
ulg s, c;               /* size and offset of central directory */
extent m;               /* length of zip file comment (0 if none) */
char *z;                /* zip file comment if m != 0 */
FILE *f;                /* file to write to */
/* Write the end of central directory data to file *f.  Return an error code
   in the ZE_ class. */
{
  PUTLG(ENDSIG, f);
  PUTSH(0, f);
  PUTSH(0, f);
  PUTSH(n, f);
  PUTSH(n, f);
  PUTLG(s, f);
  PUTLG(c, f);
  PUTSH(m, f);
  if (m && fwrite(z, 1, m, f) != m)
    return ZE_TEMP;
  return ZE_OK;
}


#ifndef UTIL

local char *cutpath(p)
char *p;                /* path string */
/* Cut the last path component off the name *p in place.  Return p. */
{
  char *r;              /* pointer to last path delimiter */

#ifdef VMS                      /* change [w.x.y]z to [w.x]y.DIR */
  if ((r = strrchr(p, ']')) != NULL)
  {
    *r = 0;
    if ((r = strrchr(p, '.')) != NULL)
    {
      *r = ']';
      strcat(r, ".DIR");        /* this assumes a little padding--see PAD */
    }
    else
      *p = 0;
  }
  else
    *p = 0;
#else /* !VMS */                /* change w/x/y/z to w/x/y */
  if ((r = strrchr(p, '/')) != NULL)
    *r = 0;
  else
    *p = 0;
#endif /* ?VMS */
  return p;
}


int trash()
/* Delete the compressed files and the directories that contained the deleted
   files, if empty.  Return an error code in the ZE_ class.  Failure of
   destroy() or deletedir() is ignored. */
{
  extent i;             /* counter on deleted names */
  extent k;             /* number of deleted directories this pass */
  extent n;             /* number of deleted names left to handle */
  struct zlist far **s; /* table of zip entries to handle, sorted */
  struct zlist far *z;  /* current zip entry */

  /* Count and delete marked names */
  n = 0;
  for (z = zfiles; z != NULL; z = z->nxt)
    if (z->mark || z->trash)
    {
      z->mark = 1;
      n++;
      if (verbose)
        printf("zip diagnostic: trashing file %s\n", z->name);
      destroy(z->name);
    }

  /* Try to delete all paths that lead up to marked names */
  if (n)
  {
    if ((s = (struct zlist far **)malloc((n+1)*sizeof(struct zlist far *))) ==
        NULL ||
        (s[0] = (struct zlist far *)farmalloc(sizeof(struct zlist))) == NULL)
      return ZE_MEM;
    s[0]->name = "";
    s++;
    do {
      n = k = 0;
      for (z = zfiles; z != NULL; z = z->nxt)
        if (z->mark)
          s[n++] = z;
      qsort((char *)s, n, sizeof(struct zlist far *), zqcmp);
      for (i = 0; i < n; i++)
        if (*cutpath(s[i]->name) && strcmp(s[i]->name, s[i-1]->name))
        {
          if (verbose)
            printf("zip diagnostic: trashing directory %s\n", s[i]->name);
          deletedir(s[i]->name);
          k++;
        }
        else
          s[i]->mark = 0;
    } while (k);
    farfree((voidp far *)((--s)[0]));
    free((voidp *)s);
  }
  return ZE_OK;
}

#endif /* !UTIL */
