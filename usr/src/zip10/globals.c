/*

 Copyright (C) 1990,1991 Mark Adler, Richard B. Wales, and Jean-loup Gailly.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as all of the original files are included
 unmodified, that it is not sold for profit, and that this copyright notice
 is retained.

*/

/*
 *  globals.c by Mark Adler.
 */

#define GLOBALS         /* include definition of errors[] in zip.h */
#include "zip.h"


/* Handy place to build error messages */
char errbuf[FNMAX+81];

/* Argument processing globals */
int recurse = 0;        /* 1=recurse into directories encountered */
int pathput = 1;        /* 1=store path with name */
int method = BEST;      /* one of BEST, SHRINK (only), or IMPLODE (only) */
int dosify = 0;         /* 1=make new entries look like MSDOS */
int verbose = 0;        /* 1=report oddities in zip file structure */
int level = 5;          /* 0=fastest compression, 9=best compression */
#ifdef VMS
   int vmsver = 0;      /* 1=append VMS version number to file names */
#endif /* VMS */
int linkput = 0;        /* 1=store symbolic links as such */
int noisy = 1;          /* 0=quiet operation */
char *special = NULL;   /* List of special suffixes */
char *key = NULL;       /* Scramble password if scrambling */
char *tempath = NULL;   /* Path for temporary files */

/* Zip file globals */
char *zipfile;          /* New or existing zip archive (zip file) */
ulg zipbeg;             /* Starting offset of zip structures */
ulg cenbeg;             /* Starting offset of central directory */
struct zlist far *zfiles = NULL;  /* Pointer to list of files in zip file */
extent zcount;          /* Number of files in zip file */
extent zcomlen;         /* Length of zip file comment */
char *zcomment;         /* Zip file comment (not zero-terminated) */
struct zlist far **zsort;       /* List of files sorted by name */

/* Files to operate on that are not in zip file */
struct flist far *found = NULL; /* List of names found */
struct flist far * far *fnxt;   /* Where to put next name in found list */
extent fcount;          /* Count of files in list */

/* Flags for cleaning up after compression routines */
int shract = 0;         /* Shrink active */
int impact = 0;         /* Implosion active */
