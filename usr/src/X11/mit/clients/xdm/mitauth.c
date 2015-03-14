/*
 * xdm - display manager daemon
 *
 * $XConsortium: mitauth.c,v 1.9 91/07/24 00:06:43 keith Exp $
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

/*
 * mitauth
 *
 * generate authorization keys
 * for MIT-MAGIC-COOKIE-1 type authorization
 */

# include   <X11/Xos.h>
# include   "dm.h"

# define AUTH_DATA_LEN	16	/* bytes of authorization data */
static char	auth_name[256];
static int	auth_name_len;

MitInitAuth (name_len, name)
    unsigned short  name_len;
    char	    *name;
{
    if (name_len > 256)
	name_len = 256;
    auth_name_len = name_len;
    bcopy (name, auth_name, name_len);
}

Xauth *
MitGetAuth (namelen, name)
    unsigned short  namelen;
    char	    *name;
{
    Xauth   *new;
    new = (Xauth *) malloc (sizeof (Xauth));

    if (!new)
	return (Xauth *) 0;
    new->family = FamilyWild;
    new->address_length = 0;
    new->address = 0;
    new->number_length = 0;
    new->number = 0;

    new->data = (char *) malloc (AUTH_DATA_LEN);
    if (!new->data)
    {
	free ((char *) new);
	return (Xauth *) 0;
    }
    new->name = (char *) malloc (namelen);
    if (!new->name)
    {
	free ((char *) new->data);
	free ((char *) new);
	return (Xauth *) 0;
    }
    bcopy (name, (char *)new->name, namelen);
    new->name_length = namelen;
    GenerateAuthorization (new->data, AUTH_DATA_LEN);
    new->data_length = AUTH_DATA_LEN;
    return new;
}
