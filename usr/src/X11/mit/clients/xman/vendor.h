/*
 * $Header: /home/x_cvs/mit/clients/xman/vendor.h,v 1.4 1992/09/16 15:09:38 dawes Exp $
 * $XConsortium: vendor.h,v 1.7 91/08/20 14:27:57 gildea Exp $
 *
 * Copyright 1991 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 */

/* Vendor-specific definitions */

#define SUFFIX "suffix"
#define FOLD "fold"
#define FOLDSUFFIX "foldsuffix"
#define MNULL 0
#define MSUFFIX 1
#define MFOLD 2
#define MFOLDSUFFIX 3

/*
 * The directories to search.  Assume that the manual directories are more
 * complete than the cat directories.
 */

#if ( defined(UTEK) || defined(apollo) )
#  define SEARCHDIR  CAT
#else
#  define SEARCHDIR  MAN
#endif

#if ( defined(sgi) || defined(SYSV386) || defined(__386BSD__) || defined(linux) )
# define SEARCHOTHER CAT
#endif

#if defined(__386BSD__)
# define MANCONF "/etc/man.conf"
#endif

/*
 * The default manual page directory.
 *
 * The MANPATH enviornment variable will override this.
 */

#ifndef SYSMANPATH

#ifdef macII
#  define SYSMANPATH "/usr/catman/u_man:/usr/catman/a_man"
#endif /* macII */
#if defined(SVR4) || defined(__386BSD__)
#  define SYSMANPATH "/usr/share/man"
#endif /* SVR4 || __386BSD__ */
#ifdef hcx
#  define SYSMANPATH "/usr/catman/local_man:/usr/catman/u_man:/usr/catman/a_man:/usr/catman/p_man:/usr/catman/ada_man"
#endif /* hcx */
#if defined(SYSV) && defined(SYSV386)
#  define SYSMANPATH "/usr/catman/u_man:/usr/catman/p_man"
#endif /* SYSV386 */
#ifdef sgi
#  define SYSMANPATH "/usr/catman/a_man:/usr/catman/g_man:/usr/catman/p_man:/usr/catman/u_man:/usr/man/p_man:/usr/man/u_man:/usr/man"
#endif /* sgi */

#ifndef SYSMANPATH
#  define SYSMANPATH "/usr/man"
#endif

#endif

/*
 * Compression Definitions.
 */

#if defined( macII ) || defined( hcx ) || (defined(SYSV) && defined(SYSV386) && !defined(ISC)) || defined(sgi)
#  define COMPRESSION_EXTENSION   "z"
#  define UNCOMPRESS_FORMAT       "pcat %s > %s"
#  define NO_COMPRESS		/* mac can't handle using pack as a filter and
				   xman needs it to be done that way. */
#else
#  if defined ( UTEK )
#    define COMPRESSION_EXTENSION "C"
#    define UNCOMPRESS_FORMAT     "ccat < %s > %s"
#    define COMPRESS              "compact"
#  else
#    define COMPRESSION_EXTENSION "Z"
#    define UNCOMPRESS_FORMAT     "zcat < %s > %s"
#    define COMPRESS              "compress"
#  endif /* UTEK */
#endif /* macII, hcx, SYSV386, sgi */



/*
 * The command filters for the manual and apropos searches.
 */

#if ( defined(hpux) || defined(macII) || defined(CRAY) || defined(ultrix) || defined(hcx) )
#  define NO_MANPATH_SUPPORT
#endif

#ifdef NO_MANPATH_SUPPORT
#  define APROPOS_FORMAT ("man -k %s | pr -h Apropos >> %s")
#else
#  define APROPOS_FORMAT ("man -M %s -k %s | pr -h Apropos > %s")
#endif

#if defined( ultrix )
#  define FORMAT "| nroff -man"             /* The format command. */
#else
#  ifdef __386BSD__
#    define FORMAT "| eqn | tbl | nroff -man"
#  else
#    define FORMAT "| neqn | nroff -man"      /* The format command. */
#  endif
#endif

/*
 * Names of the man and cat dirs.
 */

#define MAN "man"

#if ( defined(macII) || defined(CRAY) || defined(hcx) || (defined(SYSV) && defined(SYSV386)) )
/*
 * The Apple, Cray,, SYSV386, and HCX folks put the preformatted pages in the
 * "man" directories.
 */
#  define CAT MAN
#else
#  define CAT "cat"
#endif

extern void AddStandardSections();
extern void AddNewSection();

typedef struct _SectionList {
  struct _SectionList * next;
  char * label;			/* section label */
  char * directory;		/* section directory */
  int flags;
} SectionList;
