#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)xv_util.c 1.3 91/09/14";
#endif
#endif

/*
 *	(c) Copyright 1990 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

#include <stdio.h>
#include <xview/xview.h>

#ifdef hpux
#define USE_NAME
#endif
#ifdef SVR4
#define USE_NAME
#endif

#ifdef USE_UNAME
#include <sys/utsname.h>
#endif

/*
 * xv_get_hostname - emulates gethostname() on non-bsd systems.
 */

Xv_private int
xv_get_hostname (buf, maxlen)
    char *buf;
    int maxlen;
{
    int len;

#ifdef USE_UNAME
    struct utsname name;
 
    uname (&name);
    len = strlen (name.nodename);
    if (len >= maxlen) len = maxlen - 1;
    (void) strncpy (buf, name.nodename, len);
    buf[len] = '\0';
#else
    buf[0] = '\0';
    (void) gethostname (buf, maxlen);
    buf [maxlen - 1] = '\0';
    len = strlen(buf);
#endif
    return len;
}
