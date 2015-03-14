/*
 * MIT-MAGIC-COOKIE-1 authorization scheme
 *
 * $XConsortium: mitauth.c,v 1.3 89/03/14 15:53:36 rws Exp $
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

#include "X.h"
#include "os.h"

static struct auth {
    struct auth	*next;
    unsigned short	len;
    char	*data;
    XID		id;
} *mit_auth;

int
MitAddCookie (data_length, data, id)
unsigned short	data_length;
char	*data;
XID	id;
{
    struct auth	*new;

    new = (struct auth *) xalloc (sizeof (struct auth));
    if (!new)
	return 0;
    new->data = (char *) xalloc ((unsigned) data_length);
    if (!new->data) {
	xfree(new);
	return 0;
    }
    new->next = mit_auth;
    mit_auth = new;
    bcopy (data, new->data, (int) data_length);
    new->len = data_length;
    new->id = id;
    return 1;
}

XID
MitCheckCookie (data_length, data)
unsigned short	data_length;
char	*data;
{
    struct auth	*auth;

    for (auth = mit_auth; auth; auth=auth->next) {
        if (data_length == auth->len &&
	   bcmp (data, auth->data, (int) data_length) == 0)
	    return auth->id;
    }
    return (XID) -1;
}

int
MitResetCookie ()
{
    struct auth	*auth, *next;

    for (auth = mit_auth; auth; auth=next) {
	next = auth->next;
	xfree (auth->data);
	xfree (auth);
    }
    mit_auth = 0;
}

XID
MitToID (data_length, data)
unsigned short	data_length;
char	*data;
{
    struct auth	*auth;

    for (auth = mit_auth; auth; auth=auth->next) {
	if (data_length == auth->len &&
	    bcmp (data, auth->data, data_length) == 0)
	    return auth->id;
    }
    return (XID) -1;
}

MitFromID (id, data_lenp, datap)
XID id;
unsigned short	*data_lenp;
char	**datap;
{
    struct auth	*auth;

    for (auth = mit_auth; auth; auth=auth->next) {
	if (id == auth->id) {
	    *data_lenp = auth->len;
	    *datap = auth->data;
	    return 1;
	}
    }
    return 0;
}

MitRemoveCookie (data_length, data)
unsigned short	data_length;
char	*data;
{
    struct auth	*auth, *prev;

    prev = 0;
    for (auth = mit_auth; auth; auth=auth->next) {
	if (data_length == auth->len &&
	    bcmp (data, auth->data, data_length) == 0)
 	{
	    if (prev)
		prev->next = auth->next;
	    else
		mit_auth = auth->next;
	    xfree (auth->data);
	    xfree (auth);
	    return 1;
	}
    }
    return 0;
}
