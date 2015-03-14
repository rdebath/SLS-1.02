/*

 Copyright (C) 1990,1991 Mark Adler, Richard B. Wales, and Jean-loup Gailly.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as all of the original files are included
 unmodified, that it is not sold for profit, and that this copyright notice
 is retained.

*/

/*
 *  zip.h by Mark Adler.
 */


/* Set up portability */
#include "tailor.h"

/* Define malloc() and string functions */
#ifdef MODERN
#  include <string.h>
#else /* !MODERN */
   voidp *malloc();
   char *getenv();
   long atol();
   char *strcpy();
   char *strcat();
   char *strchr();
   char *strrchr();
#  ifndef ZMEM
     char *memset();
     char *memcpy();
#  endif /* !ZMEM */
#endif /* ?MODERN */


/* Define fseek() commands */
#ifndef SEEK_SET
#  define SEEK_SET 0
#endif /* !SEEK_SET */

#ifndef SEEK_CUR
#  define SEEK_CUR 1
#endif /* !SEEK_CUR */


/* Forget FILENAME_MAX (incorrectly = 14 on some System V) */
#ifdef MSDOS
#  define FNMAX 256
#else /* !MSDOS */
#  define FNMAX 1024
#endif /* ?MSDOS */


/* Types centralized here for easy modification */
#define local static            /* More meaningful outside functions */
typedef unsigned char uch;      /* unsigned 8-bit value */
typedef unsigned short ush;     /* unsigned 16-bit value */
typedef unsigned long ulg;      /* unsigned 32-bit value */


/* Lengths of headers after signatures in bytes */
#define LOCHEAD 26
#define CENHEAD 42
#define ENDHEAD 18


/* Structures for in-memory file information */
struct zlist {
  /* See central header in zipfile.c for what vem..off are */
  ush vem, ver, flg, how;
  ulg tim, crc, siz, len;
  extent nam, ext, cext, com;   /* offset of ext must be >= LOCHEAD */
  ush dsk, att, lflg;           /* offset of lflg must be >= LOCHEAD */
  ulg atx, off;
  char *name;                   /* File name in zip file */
  char *extra;                  /* Extra field (set only if ext != 0) */
  char *cextra;                 /* Extra in central (set only if cext != 0) */
  char *comment;                /* Comment (set only if com != 0) */
  char *zname;                  /* Name for new zip file header */
  int mark;                     /* Marker for files to operate on */
  int trash;                    /* Marker for files to delete */
  struct zlist far *nxt;        /* Pointer to next header in list */
};
struct flist {
  char *name;                   /* Pointer to zero-delimited name */
  char *zname;                  /* Name used for zip file headers */
  struct flist far * far *lst;  /* Pointer to link pointing here */
  struct flist far *nxt;        /* Link to next name */
};


/* Error return codes and PERR macro */
#include "ziperr.h"


/* Public globals */
extern char errbuf[];           /* Handy place to build error messages */
extern int recurse;             /* Recurse into directories encountered */
extern int pathput;             /* Store path with name */
#define BEST -1                 /* Use best method */
#define STORE 0                 /* Store method */
#define SHRINK 1                /* Use shrink or store only */
#define IMPLODE 6               /* Use implode or store only */
extern int method;              /* Restriction on compression method */
extern int dosify;              /* Make new entries look like MSDOS */
extern char *special;           /* Don't compress special suffixes */
extern int verbose;             /* Report oddities in zip file structure */
extern int level;               /* Compression level */
#ifdef VMS
   extern int vmsver;           /* Append VMS version number to file names */
#endif /* VMS */
extern int linkput;             /* Store symbolic links as such */
extern int noisy;               /* False for quiet operation */
extern char *key;               /* Scramble password or NULL */
extern char *tempath;           /* Path for temporary files */
extern char *zipfile;           /* New or existing zip archive (zip file) */
extern ulg zipbeg;              /* Starting offset of zip structures */
extern ulg cenbeg;              /* Starting offset of central directory */
extern struct zlist far *zfiles;/* Pointer to list of files in zip file */
extern extent zcount;           /* Number of files in zip file */
extern extent zcomlen;          /* Length of zip file comment */
extern char *zcomment;          /* Zip file comment (not zero-terminated) */
extern struct zlist far **zsort;/* List of files sorted by name */
extern struct flist far *found; /* List of names found */
extern struct flist far * far *fnxt;    /* Where to put next in found list */
extern extent fcount;           /* Count of names in found list */
extern int shract;              /* Shrink active */
extern int impact;              /* Implosion active */


/* Diagnostic function */
#ifdef DEBUG
#  define diag(where) fprintf(stderr, "zip diagnostic: %s\n", where)
#else /* !DEBUG */
#  define diag(where)
#endif /* ?DEBUG */


/* Public function prototypes */

        /* in zip.c, zipcloak.c, or zipsplit.c */
void warn OF((char *, char *));

        /* in zipup.c */
int zipcopy OF((struct zlist far *, FILE *, FILE *));
#ifndef UTIL
   int percent OF((ulg, ulg));
   int zipup OF((struct zlist far *, FILE *));
#endif /* !UTIL */

        /* in zipfile.c */
#ifndef UTIL
   struct zlist far *zsearch OF((char *));
   int trash OF((void));
#endif /* !UTIL */
char *ziptyp OF((char *));
int readzipfile OF((void));
int putlocal OF((struct zlist far *, FILE *));
int putcentral OF((struct zlist far *, FILE *));
int putend OF((int, ulg, ulg, extent, char *, FILE *));

        /* in fileio.c */
#ifndef UTIL
#  ifdef MSDOS
     int wild OF((char *));
#  endif /* MSDOS */
   char *getnam OF((char *));
   struct flist far *fexpel OF((struct flist far *));
   char *in2ex OF((char *));
   int exclude OF((void));
   int procname OF((char *));
   void stamp OF((char *, ulg));
   ulg dostime OF((int, int, int, int, int, int));
   ulg filetime OF((char *, ulg *, long *));
   int issymlnk OF((ulg a));
#  ifdef S_IFLNK
#    define rdsymlnk(p,b,n) readlink(p,b,n)
     extern int readlink OF((char *, char *, int));
#  else /* !S_IFLNK */
#    define rdsymlnk(p,b,n) (0)
#  endif /* !S_IFLNK */
   int deletedir OF((char *));
#endif /* !UTIL */
int destroy OF((char *));
int replace OF((char *, char *));
int getfileattr OF((char *));
int setfileattr OF((char *, int));
char *tempname OF((int));
int fcopy OF((FILE *, FILE *, ulg));
#ifndef EXPORT
#  ifndef MSVMS
     void echoff OF((int));
     void echon OF((void));
#  endif /* !MSVMS */
   char *getp OF((char *, char *, int));
#endif /* !EXPORT */
#ifdef ZMEM
   char *memset OF((char *, int, unsigned int));
   char *memcpy OF((char *, char *, unsigned int));
   int memcmp OF((char *, char *, unsigned int));
#endif /* ZMEM */

        /* in crypt.c */
#ifdef EXPORT
#  define zfwrite fwrite
#else /* !EXPORT */
   void crypthead OF((char *, ulg, FILE *));
#  ifdef UTIL
     int zipcloak OF ((struct zlist far *, FILE *, FILE *, char *));
     int zipbare OF ((struct zlist far *, FILE *, FILE *, char *));
#  else /* !UTIL */
     int zfwrite OF((voidp *, extent, extent, FILE *));
     int zfputc OF((int, FILE *));
#  endif /* ?UTIL */
#endif /* ?EXPORT */

        /* in util.c */
char *isshexp OF((char *));
int shmatch OF((char *, char *));
#ifdef MSDOS
   int dosmatch OF((char *, char *));
#endif /* MSDOS */
voidp far **search OF((voidp *, voidp far **, extent,
                       int (*)(voidp *, voidp far *)));
ulg crc32 OF((ulg, int));
ulg updcrc OF((char *, extent));

        /* in shrink.c */
#ifndef UTIL
   int shr_setup OF((void));
   int shr_p1 OF((uch *, extent));
   int shr_size OF((ulg *));
   int shr_p2 OF((FILE *));
   int shr_clear OF((void));
  
        /* in implode.c */
#  ifndef NOIMPLODE
     int imp_setup OF((long, int));
     int imp_p1 OF((char *, int));
     int imp_size OF((ulg *, uch *));
     int imp_p2 OF((FILE *));
     int imp_clear OF((void));
#  endif /* !NOIMPLODE */
#endif /* !UTIL */


/* end of zip.h */
