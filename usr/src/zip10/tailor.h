/* tailor.h -- Not copyrighted 1991 Mark Adler */

/* const's are inconsistently used across ANSI libraries--kill for all
   header files. */
#define const


/* Use prototypes and ANSI libraries if __STDC__ */
#ifdef __STDC__
#  ifndef PROTO
#    define PROTO
#  endif /* !PROTO */
#  define MODERN
#endif /* __STDC__ */


/* Use prototypes and ANSI libraries if Silicon Graphics */
#ifdef sgi
#  ifndef PROTO
#    define PROTO
#  endif /* !PROTO */
#  define MODERN
#endif /* sgi */


/* Define MSDOS for Turbo C as well as Microsoft C */
#ifdef __POWERC                 /* For Power C too */
#  define __TURBOC__
#endif /* __POWERC */
#ifdef __TURBOC__
#  ifndef MSDOS
#    define MSDOS
#  endif /* !MSDOS */
#endif /* __TURBOC__ */


/* Use prototypes and ANSI libraries if Microsoft or Borland C */
#ifdef MSDOS
#  ifndef PROTO
#    define PROTO
#  endif /* !PROTO */
#  define MODERN
#endif /* MSDOS */


/* Turn off prototypes if requested */
#ifdef NOPROTO
#  ifdef PROTO
#    undef PROTO
#  endif /* PROTO */
#endif /* NOPROT */


/* Used to remove arguments in function prototypes for non-ANSI C */
#ifdef PROTO
#  define OF(a) a
#else /* !PROTO */
#  define OF(a) ()
#endif /* ?PROTO */


/* Allow far and huge allocation for small model (Microsoft C or Turbo C) */
#ifdef MSDOS
#  ifdef __TURBOC__
#    include <alloc.h>
#  else /* !__TURBOC__ */
#    include <malloc.h>
#    define farmalloc _fmalloc
#    define farfree   _ffree
#  endif /* ?__TURBOC__ */
#else /* !MSDOS */
#  define huge
#  define far
#  define near
#  define farmalloc malloc
#  define farfree   free
#endif /* ?MSDOS */


/* Define MSVMS if either MSDOS or VMS defined */
#ifdef MSDOS
#  define MSVMS
#else /* !MSDOS */
#  ifdef VMS
#    define MSVMS
#  endif /* VMS */
#endif /* ?MSDOS */


/* Define void, voidp, and extent (size_t) */
#include <stdio.h>
#ifdef MODERN
#  ifndef M_XENIX
#    include <stddef.h>
#  endif /* !M_XENIX */
#  include <stdlib.h>
   typedef size_t extent;
   typedef void voidp;
#else /* !MODERN */
   typedef unsigned int extent;
#  define void int
   typedef char voidp;
#endif /* ?MODERN */

/* Get types and stat */
#ifdef VMS
#  include <types.h>
#  include <stat.h>
#else /* !VMS */
#  include <sys/types.h>
#  include <sys/stat.h>
#endif /* ?VMS */


/* Cheap fix for unlink on VMS */
#ifdef VMS
#  define unlink delete
#endif /* VMS */


/* For Pyramid */
#ifdef pyr
#  define strrchr rindex
#  define ZMEM
#endif /* pyr */


/* File operations--use "b" for binary if allowed */
#ifdef MODERN
#  define FOPR "rb"
#  define FOPM "r+b"
#  define FOPW "w+b"
#else /* !MODERN */
#  define FOPR "r"
#  define FOPM "r+"
#  define FOPW "w+"
#endif /* ?MODERN */


/* Fine tuning */
#ifndef MSDOS
#   define BSZ 8192   /* Buffer size for files */
#else /* !MSDOS */
#   define BSZ 4096   /* Keep precious NEAR space */
    /* BSZ can't be 8192 even for compact model because of 64K limitation
     * in im_lmat.c. If you run out of memory when processing a large number
     * files, use the compact model and reduce BSZ to 2048 here and in
     * im_lm.asm.
     */
#endif /* ?MSDOS */

/* end of tailor.h */
