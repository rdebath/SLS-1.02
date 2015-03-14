#ifndef SYSDEFH
#define SYSDEFH
/*	Copyright (C) 1992, 1993 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */
/*  t. lord	Sun Aug  9 22:03:36 1992	*/



#ifdef STDC_HEADERS
#include <stddef.h>
#endif

#include <sys/types.h>
#include <sys/time.h>
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
  
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#else
#ifdef __STDC__
extern int access (char *, int);
extern int getuid (void);
#endif
#endif /* HAVE_UNISTD_H */

#if defined(STDC_HEADERS) || defined(HAVE_STRING_H)
#include <string.h>
/* An ANSI string.h and pre-ANSI memory.h might conflict.  */
#if !defined(STDC_HEADERS) && HAVE_MEMORY_H
#include <memory.h>
#endif /* not STDC_HEADERS and HAVE_MEMORY_H */
#ifndef index
#define index strchr
#endif
#ifndef rindex
#define rindex strrchr
#endif
#ifndef bcopy
#define bcopy(s, d, n) memcpy ((d), (s), (n))
#endif
#ifndef bcmp
#define bcmp(s1, s2, n) memcmp ((s1), (s2), (n))
#endif
#ifndef bzero
#define bzero(s, n) memset ((s), 0, (n))
#endif
#else /* not STDC_HEADERS and not HAVE_STRING_H */
#include <strings.h>
/* memory.h and strings.h conflict on some systems.  */
#endif /* not STDC_HEADERS and not HAVE_STRING_H */

#include <math.h>
#include <time.h>
#include <signal.h>


#ifdef __STDC__
#ifndef linux
extern int gethostname (char *, int);
#endif /* linux */
extern int atoi (const char *);
extern char *getenv (const char *);

#ifndef HAVE_STRDUP
extern char *strdup (const char *);
#endif
#ifndef HAVE_STRICMP
extern int stricmp (const char *, const char *);
#endif
#ifndef HAVE_STRINCMP
extern int strincmp (const char *, const char *, size_t);
#endif
#ifndef HAVE_STRSTR
extern const char *strstr (const char *, const char *);
#endif

#else  /* !defined(__STDC__) */

#ifndef HAVE_STRDUP
extern char *strdup ();
#endif
#ifndef HAVE_STRSTR
extern char *strstr ();
#endif

#endif /* !defined(__STDC__) */

extern char *getenv ();

#ifndef RETSIGTYPE
#define RETSIGTYPE void
#endif /* RETSIGTYPE */

#ifndef VOIDSTAR
#define VOIDSTAR void *
#endif

#ifndef __STDC__
#define const
#endif

#if defined(USE_DLD) && 0
extern int dld_errno;
extern int dld_nerr;
extern char *dld_errlst[];
extern dld_undefined_sym_count;
extern char *dld_search_path;
extern void (*dld_get_func())();
extern int dld_unlink_by_file EXT2(char *,int);
extern int dld_link EXT1(char *);
extern int dld_function_executable_p EXT1(char *);
extern int dld_init EXT1(char *);
extern char *dld_find_executable EXT1(char *);
extern char ** dld_list_undefined_sym EXT0();
#endif


#endif
