/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : config.h
 *  Author    : I.Lea
 *  Created   : 03-09-92
 *  Updated   : 04-12-92
 *  Notes     : #defines to determine different OS capabilites
 *  Copyright : (c) Copyright 1991-92 by Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#ifdef M_XENIX
#	define	HAVE_PROTOTYPES_H
#endif

#ifdef PTX
#	define	HAVE_TERMIO_H
#endif

#if defined(SVR4) || defined(UMAXV)
#	define	HAVE_TERMIOS_H
#endif

#if defined(_POSIX_SOURCE) || defined(DGUX) || defined(SVR4) || defined(UMAXV)
#	define	HAVE_POSIX_JC
#endif

#ifdef SVR4
#	define	HAVE_LOCALE_H
#	define	HAVE_SETLOCALE
#	undef	sinix		/* SNI Sinix (nsc32000) */
#endif

#if defined(SIGCHLD) && !defined(apollo)
#	define	HAVE_SYS_WAIT_H
#endif

#if !defined(__NeXT__) && !defined(gould) && !defined(mips) && !defined(MACH)
#	define	HAVE_UNISTD_H
#endif

#if !defined(AMIGA) && !defined(MINIX)
#	define	HAVE_SYS_IOCTL_H
#endif

#if !defined(AMIGA) && !defined(BSD) && !defined(sinix) && !defined(RS6000)
#	define	HAVE_SYS_UTSNAME_H
#endif

#if !defined(apollo) && !defined(sequent) && !defined(sysV68) && !defined(UTS)
#	define	HAVE_STDLIB_H
#endif

#ifdef BSD
#	define	HAVE_STRINGS_H
#else
#	define	HAVE_STRING_H
#endif

#if defined(BSD) || defined(UMAXV)
#	define	HAVE_FCNTL_H
#endif

#ifndef __hpux
#	define	HAVE_SYS_STREAM_H
#endif

#if !defined(apollo) && !defined(__hpux) && !defined(sinix) && !defined(UMAXV)
#	define	HAVE_SYS_PTEM_H
#endif

#if !defined(apollo) && !defined(SCO_UNIX) && !defined(sinix) && !defined(SVR4)
#	define	HAVE_SYS_PTY_H
#endif

#if defined(BSD) || defined(__hpux) || defined(RS6000) || defined(sinix) || \
    defined(UMAXV)
#	define	HAVE_NETDB_H
#endif

#if !defined(apollo)
#	define	HAVE_SYS_TIME_H
#endif

#ifdef RS6000
#	define	HAVE_SYS_SELECT_H
#endif

#if defined(__GNUC__) || defined(HAVE_POSIX_JC)
#	define	HAVE_SIGTYPE_VOID
#else	
#	if defined(sony)
#		define	HAVE_SIGTYPE_INT
#	else
#		if __STDC__ || defined(atthcx) || defined(__hpux) || \
		   defined(PTX) || defined(RS6000) || defined(sgi) || \
		   defined(sinix) || defined(sysV68) || defined(sun) || \
		   defined(SVR4) || defined(ultrix)
#			define	HAVE_SIGTYPE_VOID
#		else
#			define	HAVE_SIGTYPE_INT
#		endif
#	endif
#endif

#if defined(apollo) || defined(AUX) || defined(BSD) || defined(__hpux) || \
    defined(PTX) || defined(RS6000) || defined(sinix) || defined(SVR4) || \
    defined(UMAXV)
#	define	HAVE_LONG_FILENAMES
#endif

#if defined(BSD) || defined(__hpux) || defined(RS6000) || defined(sinix) || \
    defined(UMAXV) 
#	define	HAVE_GETHOSTBYNAME
#endif

#if defined(AMIGA) || defined(apollo) || defined(BSD) || defined(MINIX)
#	define	HAVE_CR_AS_CHAR
#endif

/*
 * Used in tin.h
 */

#if __STDC__ || defined(SVR4) 
#	if !defined(apollo) && !defined(__hpux) 
#		define	HAVE_ANSI_ASSERT
#	endif
#endif

#if defined(__NeXT__) || defined(MACH)
#	define	DONT_HAVE_SIGWINCH
#endif

#if defined(BSD) && ! defined(sinix)
#	define	DONT_HAVE_GETCWD
#endif

#if defined(sequent) || defined(pyr)
#	define	DONT_HAVE_MEMCMP
#endif

#if defined(__arm) || defined(sequent)
#	define	DONT_HAVE_TZSET
#endif

/*
 * Used in parsedate.y
 */

#if defined(__arm) || defined(DGUX) || defined(sequent) || !defined(BSD)
#	define	DONT_HAVE_TM_GMTOFF
#endif

/*
 * Used in art.c
 */

#if defined(apollo)
#	define	DONT_HAVE_SELECT
#endif

#if defined(__hpux)
#	define	DONT_PROTOTYPE_PTR_TO_FUNC
#	define	DONT_HAVE_SYS_BSDTYPES_H
#	define	HAVE_KEYPAD
#endif

#ifdef sinix
#	undef	HAVE_SYS_STREAM_H
#	define	DONT_HAVE_MKDIR
#endif

/*
 * Hack used to try and get a compile on Sun i386 & old SunOS 4.0.2
 */
 
#if defined(sun) && defined(i386)
#	undef	HAVE_STDLIB_H
#endif

#ifndef STDIN_FILENO
#	define	STDIN_FILENO	0
#endif

#if defined(BSD) || defined(_POSIX_SOURCE)
#	define	HAVE_REWINDDIR
#endif
