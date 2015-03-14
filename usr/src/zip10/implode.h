/*

 Copyright (C) 1990,1991 Mark Adler, Richard B. Wales, and Jean-loup Gailly.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as all of the original files are included
 unmodified, that it is not sold for profit, and that this copyright notice
 is retained.

*/

/*
 * implode.h by Richard B. Wales.
 */

#include "crypt.h"
#include "tempf.h"
#include <errno.h>


/***********************************************************************
 *
 * Type definitions.
 */


typedef long  L_INT;
typedef int   INT;
typedef short S_INT;

typedef unsigned long  UL_INT;
typedef unsigned int   U_INT;
typedef unsigned short US_INT;

typedef unsigned char  U_CHAR;

typedef unsigned long  CRC;

#define VOID void

#define local static            /* More meaningful outside functions */
/* #define local */

#define TRUE  1
#define FALSE 0

/* Error return codes. */
typedef
enum
    {   IM_OK,                  /* all OK */
        IM_EOF,                 /* end of file on input */
        IM_IOERR,               /* I/O error */
        IM_BADARG,              /* invalid procedure argument */
        IM_NOMEM,               /* out of memory */
        IM_LOGICERR,            /* logic error */
        IM_NOCTBLS              /* no more code tables */
    }
    ImpErr;

/* The different possible compression methods. */
typedef
enum
    {   NO_LITERAL_TREE,        /* use only two trees */
        LITERAL_TREE            /* use all three trees */
    }
    Method;

/* File data structure. */
typedef
struct  fdata
    {   L_INT    fd_len;        /* # of bytes in file */
        L_INT    fd_clen;       /* compressed length */
        tFILE    *fd_temp;      /* temporary file stream pointer */
        U_INT    fd_bufsize;    /* size of sliding dictionary */
        U_INT    fd_strsize;    /* max string match length */
        U_INT    fd_nbits;      /* # distance bits to write literally */
        Method   fd_method;     /* compression method */
    }
    FDATA;

/* Data structure for string matches. */
typedef
struct  match
    {   S_INT       ma_dist;    /* distance back into buffer */
        union {
           US_INT   ma_length;  /* length of matched string */
           U_CHAR   ma_litc[2]; /* literal characters matched */
        } l;
        /* l is ma_litc if ma_dist <= 0. If ma_dist < 0, the length is
         * 2 and the distance is -ma_dist.
         */
    }
    MATCH;


/***********************************************************************
 *
 * External variable declarations.
 */

extern FDATA fd;                /* file data */
#ifndef MSDOS
extern int errno;               /* system error code */
#endif  /* MSDOS */

extern MATCH *ma_buf;           /* match info buffer */
#define MA_BUFSIZE 512
/* MA_BUFSIZE must be such that
 *     256*sizeof(TRDATA) <= MA_BUFSIZE*sizeof(MATCH)
 * since the same buffer is used for both purposes at different times.
 */

/***********************************************************************
 *
 * External procedure declarations.
 */


#ifdef  MODERN
#include <string.h>
#else
voidp *malloc();
char  *strcpy();
char  *strcat();
#endif  /* MODERN */


/***********************************************************************
 *
 * Routines in "im_lmat.c" source file.
 */


ImpErr  lm_init
        OF ((int pack_level));

ImpErr  lm_input
        OF ((U_CHAR *block, U_INT count));

ImpErr  lm_windup
        OF ((void));


/***********************************************************************
 *
 * Routines in "im_ctree.c" source file.
 */

ImpErr ct_init
        OF ((void));

ImpErr ct_tally
        OF ((MATCH *ma));

ImpErr ct_mktrees
        OF ((void));

ImpErr ct_wrtrees
        OF ((FILE *outfp));

ImpErr ct_wrdata
        OF ((FILE *outfp));

ImpErr ct_windup
        OF ((void));


/***********************************************************************
 *
 * Routines in "im_bits.c" source file.
 */

ImpErr bi_init
        OF ((FILE *fp));

ImpErr bi_rlout
        OF ((int value, int length));

int bi_reverse
        OF ((int value, int length));

ImpErr bi_windup
        OF ((void));


/***********************************************************************
 *
 * Routines in "implode.c" source file.
 */

int imp_setup
        OF ((long filesize, int pack_level));

int imp_p1
        OF ((char *buf, int count));

int imp_size
        OF ((long *size, char *opts));

int imp_p2
        OF ((FILE *outfp));

int imp_clear
        OF ((void));


/**********************************************************************/
