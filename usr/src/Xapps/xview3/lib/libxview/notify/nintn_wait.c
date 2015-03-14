#ifndef	lint
#ifdef sccs
static char     sccsid[] = "@(#)nintn_wait.c 20.12 91/09/14 Copyr 1985 Sun Micro";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

/*
 * Nint_n_wait.c - Implement the notify_next_wait3_func interface.
 */

#include <xview_private/ntfy.h>
#include <xview_private/ndet.h>
#include <xview_private/nint.h>

extern          Notify_value
notify_next_wait3_func(nclient, pid, status, rusage)
    Notify_client   nclient;
    int             pid;
#ifndef SVR4
    union wait     *status;
#else SVR4
    int *status;
#endif SVR4
    struct rusage  *rusage;
{
    Notify_func     func;

    /* Don't check pid because may be exiting */
    if ((func = nint_next_callout(nclient, NTFY_WAIT3)) == NOTIFY_FUNC_NULL)
	return (NOTIFY_UNEXPECTED);
    return (func(nclient, pid, status, rusage));
}
