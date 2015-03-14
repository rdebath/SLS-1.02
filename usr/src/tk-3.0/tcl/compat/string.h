/*
 * string.h --
 *
 *	Declarations of ANSI C library procedures for string handling.
 *
 * Copyright 1991 Regents of the University of California
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appears in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 * $Header: /user6/ouster/tcl/compat/RCS/string.h,v 1.5 92/10/14 16:06:50 ouster Exp $ SPRITE (Berkeley)
 */

#ifndef _STRING
#define _STRING

#include <tcl.h>
#include <tclInt.h>

/*
 * The following #include is needed to define size_t.
 */

#ifdef sun
#include <sys/stdtypes.h>
#endif

extern char *		memchr _ANSI_ARGS_((CONST VOID *s, int c, size_t n));
extern int		memcmp _ANSI_ARGS_((CONST VOID *s1, CONST VOID *s2,
			    size_t n));
extern char *		memcpy _ANSI_ARGS_((VOID *t, CONST VOID *f, size_t n));
extern char *		memmove _ANSI_ARGS_((VOID *t, CONST VOID *f,
			    size_t n));
extern char *		memset _ANSI_ARGS_((VOID *s, int c, size_t n));

extern int		strcasecmp _ANSI_ARGS_((CONST char *s1,
			    CONST char *s2));
extern char *		strcat _ANSI_ARGS_((char *dst, CONST char *src));
extern char *		strchr _ANSI_ARGS_((CONST char *string, int c));
extern int		strcmp _ANSI_ARGS_((CONST char *s1, CONST char *s2));
extern char *		strcpy _ANSI_ARGS_((char *dst, CONST char *src));
extern size_t		strcspn _ANSI_ARGS_((CONST char *string,
			    CONST char *chars));
extern char *		strdup _ANSI_ARGS_((CONST char *string));
extern char *		strerror _ANSI_ARGS_((int error));
extern size_t		strlen _ANSI_ARGS_((CONST char *string));
extern int		strncasecmp _ANSI_ARGS_((CONST char *s1,
			    CONST char *s2, size_t n));
extern char *		strncat _ANSI_ARGS_((char *dst, CONST char *src,
			    size_t numChars));
extern int		strncmp _ANSI_ARGS_((CONST char *s1, CONST char *s2,
			    size_t nChars));
extern char *		strncpy _ANSI_ARGS_((char *dst, CONST char *src,
			    size_t numChars));
extern char *		strpbrk _ANSI_ARGS_((CONST char *string, char *chars));
extern char *		strrchr _ANSI_ARGS_((CONST char *string, int c));
extern size_t		strspn _ANSI_ARGS_((CONST char *string,
			    CONST char *chars));
extern char *		strstr _ANSI_ARGS_((CONST char *string,
			    CONST char *substring));
extern char *		strtok _ANSI_ARGS_((CONST char *s, CONST char *delim));

#endif /* _STRING */
