/*
 * Xau - X Authorization Database Library
 *
 * $XConsortium: AuUnlock.c,v 1.5 91/04/17 11:00:11 rws Exp $
 *
 * Copyright 1988 Massachusetts Institute of Technology
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
 *
 * Author:  Keith Packard, MIT X Consortium
 */

#include <X11/Xauth.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifndef X_NOT_POSIX
#include <errno.h>
#else
#include <sys/errno.h>
#endif

#if NeedFunctionPrototypes
XauUnlockAuth (
_Xconst char *file_name)
#else
XauUnlockAuth (file_name)
char	*file_name;
#endif
{
    char	creat_name[1025], link_name[1025];
    char	*strcpy (), *strcat ();

    if (strlen (file_name) > 1022)
	return;
    (void) strcpy (creat_name, file_name);
    (void) strcat (creat_name, "-c");
    (void) strcpy (link_name, file_name);
    (void) strcat (link_name, "-l");
    /*
     * I think this is the correct order
     */
    (void) unlink (creat_name);
    (void) unlink (link_name);
}
