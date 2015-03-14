/*
 * xdm - display manager daemon
 *
 * $XConsortium: xdmauth.c,v 1.1 89/12/13 15:23:06 keith Exp $
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
 * xdmauth
 *
 * generate cryptographically secure authorization keys
 * for XDM-AUTHORIZATION-1
 */

#include "dm.h"

#ifdef HASDES

static char	auth_name[256];
static int	auth_name_len;
#define AUTH_DATA_LEN	8

XdmInitAuth (name_len, name)
    unsigned short  name_len;
    char	    *name;
{
    InitCryptoKey ();
    if (name_len > 256)
	name_len = 256;
    auth_name_len = name_len;
    bcopy (name, auth_name, name_len);
}

Xauth *
XdmGetAuth (namelen, name)
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
    bcopy (name, new->name, namelen);
    new->name_length = namelen;
    GenerateCryptoKey (new->data, AUTH_DATA_LEN);
    new->data_length = AUTH_DATA_LEN;
    return new;
}

XdmGetXdmcpAuth (pdpy,authorizationNameLen, authorizationName)
    struct protoDisplay	*pdpy;
    unsigned short	authorizationNameLen;
    char		*authorizationName;
{
    Xauth   *fileauth, *xdmcpauth;

    if (pdpy->fileAuthorization && pdpy->xdmcpAuthorization)
	return;
    xdmcpauth = XdmGetAuth (authorizationNameLen, authorizationName);
    if (!xdmcpauth)
	return;
    fileauth = (Xauth *) malloc (sizeof (Xauth));
    if (!fileauth)
    {
	XauDisposeAuth(xdmcpauth);
	return;
    }
    *fileauth = *xdmcpauth;
    fileauth->name = malloc (xdmcpauth->name_length);
    fileauth->data = malloc (16);
    fileauth->data_length = 16;
    if (!fileauth->name || !fileauth->data)
    {
	XauDisposeAuth (xdmcpauth);
	if (fileauth->name)
	    free ((char *) fileauth->name);
	if (fileauth->data)
	    free ((char *) fileauth->data);
	free ((char *) fileauth);
	return;
    }
    bcopy (xdmcpauth->name, fileauth->name, xdmcpauth->name_length);
    bcopy (pdpy->authenticationData.data, fileauth->data, 8);
    bcopy (xdmcpauth->data, fileauth->data + 8, 8);
    XdmcpEncrypt (xdmcpauth->data, &pdpy->key, xdmcpauth->data, 8);
    pdpy->fileAuthorization = fileauth;
    pdpy->xdmcpAuthorization = xdmcpauth;
}

XdmGetKey (pdpy, displayID)
    struct protoDisplay	*pdpy;
    ARRAY8Ptr		displayID;
{
    FILE    *keys;
    char    line[1024], id[1024], key[1024];
    int	    keylen;

    keys = fopen (keyFile, "r");
    if (!keys)
	return FALSE;
    while (fgets (line, sizeof (line) -  1, keys))
    {
	if (line[0] == '#' || sscanf (line, "%s %s", id, key) != 2)
	    continue;
	if (strlen (id) == displayID->length &&
	    !strncmp (id, displayID->data, displayID->length))
	{
	    keylen = strlen (key);
	    while (keylen < 8)
		key[keylen++] = '\0';
	    bcopy (key, pdpy->key.data, 8);
	    fclose (keys);
	    return TRUE;
	}
    }
    fclose (keys);
    return FALSE;
}

XdmCheckAuthentication (pdpy, displayID, authenticationName, authenticationData)
    struct protoDisplay	*pdpy;
    ARRAY8Ptr		displayID, authenticationName, authenticationData;
{
    int		    newlen;
    XdmAuthKeyPtr   incoming;

    if (!XdmGetKey (pdpy, displayID))
	return FALSE;
    if (authenticationData->length != 8)
	return FALSE;
    XdmcpDecrypt (authenticationData->data, &pdpy->key,
		  authenticationData->data, 8);
    if (!XdmcpCopyARRAY8(authenticationData, &pdpy->authenticationData))
	return FALSE;
    incoming = (XdmAuthKeyPtr) authenticationData->data;
    XdmcpIncrementKey (incoming);
    XdmcpEncrypt (authenticationData->data, &pdpy->key,
		  authenticationData->data, 8);
    return TRUE;
}

#endif
