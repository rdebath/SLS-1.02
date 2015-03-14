/* $XConsortium: error.c,v 1.4 91/12/09 16:49:23 converse Exp $ */
/*
 * error message handling
 */
/*
 * Copyright 1991 Network Computing Devices;
 * Portions Copyright 1987 by Digital Equipment Corporation and the
 * Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this protoype software
 * and its documentation to Members and Affiliates of the MIT X Consortium
 * any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of Network Computing Devices, Digital or
 * MIT not be used in advertising or publicity pertaining to distribution of
 * the software without specific, written prior permission.
 *
 * NETWORK COMPUTING DEVICES, DIGITAL AND MIT DISCLAIM ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS, IN NO EVENT SHALL NETWORK COMPUTING DEVICES, DIGITAL OR MIT BE
 * LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * $NCDId: @(#)error.c,v 4.1 1991/07/08 18:22:51 lemke Exp $
 *
 */

#include	<stdio.h>
#include	<X11/Xos.h>
#ifndef X_NOT_POSIX
#ifdef _POSIX_SOURCE
#include <limits.h>
#else
#define _POSIX_SOURCE
#include <limits.h>
#undef _POSIX_SOURCE
#endif
#endif
#ifndef PATH_MAX
#include <sys/param.h>
#ifndef PATH_MAX
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else
#define PATH_MAX 1024
#endif
#endif
#endif

#ifdef USE_SYSLOG
#include	<syslog.h>
#endif

#include	"misc.h"

Bool        UseSyslog;
char        ErrorFile[PATH_MAX];

static Bool log_open;

static void
abort_server()
{
    fflush(stderr);

#ifdef SABER
    saber_stop();
#else
    abort();
#endif
}

void
InitErrors()
{
    int         i;

#ifdef USE_SYSLOG
    if (UseSyslog && !log_open) {
	openlog("Font Server", LOG_PID, LOG_LOCAL0);
	log_open = TRUE;
	return;
    }
#endif

    if (ErrorFile[0]) {
	i = creat(ErrorFile, 0666);
	if (i != -1) {
	    dup2(i, 2);
	    close(i);
	} else {
	    ErrorF("Can't open error file \"%s\"\n", ErrorFile);
	}
    }
}

void
CloseErrors()
{
#ifdef USE_SYSLOG
    if (UseSyslog) {
	closelog();
	log_open = FALSE;
	return;
    }
#endif
}

void
Error(str)
    char       *str;
{
    /* XXX this should also go to syslog() */
    perror(str);
}

/*
 * used for informational messages
 */
/* VARARGS1 */
void
NoticeF(f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9)	/* limit of 10 args */
    char       *f;
    char       *s0,
               *s1,
               *s2,
               *s3,
               *s4,
               *s5,
               *s6,
               *s7,
               *s8,
               *s9;
{

#ifdef USE_SYSLOG
    if (UseSyslog) {
	syslog(LOG_NOTICE, f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9);
	return;
    }
#endif

    /* XXX should Notices just be ignored if not using syslog? */
    fprintf(stderr, "Notice: ");
    fprintf(stderr, f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9);
}

/*
 * used for non-fatal error messages
 */
/* VARARGS1 */
void
ErrorF(f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9)	/* limit of 10 args */
    char       *f;
    char       *s0,
               *s1,
               *s2,
               *s3,
               *s4,
               *s5,
               *s6,
               *s7,
               *s8,
               *s9;
{

#ifdef USE_SYSLOG
    if (UseSyslog) {
	syslog(LOG_ERR, f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9);
	return;
    }
#endif

    fprintf(stderr, "Error: ");
    fprintf(stderr, f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9);
}

/* VARARGS1 */
void
FatalError(f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9)	/* limit of 10 args */
    char       *f;
    char       *s0,
               *s1,
               *s2,
               *s3,
               *s4,
               *s5,
               *s6,
               *s7,
               *s8,
               *s9;
{
    ErrorF("\nFatal server error!\n");
    ErrorF(f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9);
    ErrorF("\n");
    abort_server();
    /* NOTREACHED */
}
