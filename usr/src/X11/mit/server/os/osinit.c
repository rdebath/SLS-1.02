/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
/* $XConsortium: osinit.c,v 1.40 91/11/29 15:21:07 rws Exp $ */
#include "X.h"
#include "os.h"
#include "osdep.h"
#undef NULL
#include <stdio.h>
#include "Xos.h"

#ifndef PATH_MAX
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else
#define PATH_MAX 1024
#endif
#endif

#ifndef SYSV
#include <sys/resource.h>
#endif

#ifndef ADMPATH
#define ADMPATH "/usr/adm/X%smsgs"
#endif

extern char *display;
#ifdef RLIMIT_DATA
int limitDataSpace = -1;
#endif
#ifdef RLIMIT_STACK
int limitStackSpace = -1;
#endif
#ifdef RLIMIT_NOFILE
int limitNoFile = -1;
#endif

OsInit()
{
    static Bool been_here = FALSE;
    char fname[PATH_MAX];

#ifdef macII
    set42sig();
#endif

    if (!been_here) {
	fclose(stdin);
	fclose(stdout);
	/* hack test to decide where to log errors */
	if (write (2, fname, 0)) 
	{
	    FILE *err;
	    sprintf (fname, ADMPATH, display);
	    /*
	     * uses stdio to avoid os dependencies here,
	     * a real os would use
 	     *  open (fname, O_WRONLY|O_APPEND|O_CREAT, 0666)
	     */
	    if (!(err = fopen (fname, "a+")))
		err = fopen ("/dev/null", "w");
	    if (err && (fileno(err) != 2)) {
		dup2 (fileno (err), 2);
		fclose (err);
	    }
#if defined(SYSV) || defined(SVR4)
	    {
	    static char buf[BUFSIZ];
	    setvbuf (stderr, buf, _IOLBF, BUFSIZ);
	    }
#else
	    setlinebuf(stderr);
#endif
	}

#ifndef X_NOT_POSIX
	if (getpgrp () == 0)
	    setpgid (0, 0);
#else
#ifndef SYSV
	if (getpgrp (0) == 0)
	    setpgrp (0, getpid ());
#endif
#endif

#ifdef RLIMIT_DATA
	if (limitDataSpace >= 0)
	{
	    struct rlimit	rlim;

	    if (!getrlimit(RLIMIT_DATA, &rlim))
	    {
		if ((limitDataSpace > 0) && (limitDataSpace < rlim.rlim_max))
		    rlim.rlim_cur = limitDataSpace;
		else
		    rlim.rlim_cur = rlim.rlim_max;
		(void)setrlimit(RLIMIT_DATA, &rlim);
	    }
	}
#endif
#ifdef RLIMIT_STACK
	if (limitStackSpace >= 0)
	{
	    struct rlimit	rlim;

	    if (!getrlimit(RLIMIT_STACK, &rlim))
	    {
		if ((limitStackSpace > 0) && (limitStackSpace < rlim.rlim_max))
		    rlim.rlim_cur = limitStackSpace;
		else
		    rlim.rlim_cur = rlim.rlim_max;
		(void)setrlimit(RLIMIT_STACK, &rlim);
	    }
	}
#endif
#ifdef RLIMIT_NOFILE
	if (limitNoFile >= 0)
	{
	    struct rlimit	rlim;

	    if (!getrlimit(RLIMIT_NOFILE, &rlim))
	    {
		if ((limitNoFile > 0) && (limitNoFile < rlim.rlim_max))
		    rlim.rlim_cur = limitNoFile;
		else
		    rlim.rlim_cur = rlim.rlim_max;
		if (rlim.rlim_cur > MAXSOCKS)
		    rlim.rlim_cur = MAXSOCKS;
		(void)setrlimit(RLIMIT_NOFILE, &rlim);
	    }
	}
#endif
	been_here = TRUE;
    }

    OsInitColors();
}
