/*
 * xdm - display manager daemon
 *
 * $XConsortium: policy.c,v 1.3 89/12/13 15:25:16 keith Exp $
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
 * policy.c.  Implement site-dependent policy for XDMCP connections
 */

# include "dm.h"

static ARRAY8 noAuthentication = { (CARD16) 0, (CARD8Ptr) 0 };

typedef struct _XdmAuth {
    ARRAY8  authentication;
    ARRAY8  authorization;
} XdmAuthRec, *XdmAuthPtr;

XdmAuthRec auth[] = {
#ifdef HASDES
{ {(CARD16) 20, (CARD8 *) "XDM-AUTHENTICATION-1"},
  {(CARD16) 19, (CARD8 *) "XDM-AUTHORIZATION-1"},
},
#endif
{ {(CARD16) 0, (CARD8 *) 0},
  {(CARD16) 0, (CARD8 *) 0},
}
};

#define NumAuth	(sizeof auth / sizeof auth[0])

ARRAY8Ptr
ChooseAuthentication (authenticationNames)
    ARRAYofARRAY8Ptr	authenticationNames;
{
    int	i, j;

    for (i = 0; i < authenticationNames->length; i++)
	for (j = 0; j < NumAuth; j++)
	    if (XdmcpARRAY8Equal (&authenticationNames->data[i],
				  &auth[j].authentication))
		return &authenticationNames->data[i];
    return &noAuthentication;
}

CheckAuthentication (pdpy, displayID, name, data)
    struct protoDisplay	*pdpy;
    ARRAY8Ptr		displayID, name, data;
{
#ifdef HASDES
    if (name->length && !strncmp (name->data, "XDM-AUTHENTICATION-1", 20))
	return XdmCheckAuthentication (pdpy, displayID, name, data);
#endif
    return TRUE;
}

int
SelectAuthorizationTypeIndex (authenticationName, authorizationNames)
    ARRAY8Ptr		authenticationName;
    ARRAYofARRAY8Ptr	authorizationNames;
{
    int	i, j;

    for (j = 0; j < NumAuth; j++)
	if (XdmcpARRAY8Equal (authenticationName,
			      &auth[j].authentication))
	    break;
    if (j < NumAuth)
    {
    	for (i = 0; i < authorizationNames->length; i++)
	    if (XdmcpARRAY8Equal (&authorizationNames->data[i],
				  &auth[j].authorization))
	    	return i;
    }
    for (i = 0; i < authorizationNames->length; i++)
	if (ValidAuthorization (authorizationNames->data[i].length,
				authorizationNames->data[i].data))
	    return i;
    return -1;
}

int
Willing (addr, addrlen, authenticationName, status)
    struct sockaddr *addr;
    int		    addrlen;
    ARRAY8Ptr	    authenticationName;
    ARRAY8Ptr	    status;
{
    static char	statusBuf[256];
    extern char	*localHostname ();

    sprintf (statusBuf, "host %s", localHostname());
    status->length = strlen (statusBuf);
    status->data = (CARD8Ptr) malloc (status->length);
    if (!status->data)
	status->length = 0;
    else
	bcopy (statusBuf, status->data, status->length);
    return 1;
}

ARRAY8Ptr
Accept (from, fromlen, displayNumber)
    struct sockaddr *from;
    int		    fromlen;
    CARD16	    displayNumber;
{
    return 0;
}

int
SelectConnectionTypeIndex (connectionTypes, connectionAddresses)
    ARRAY16Ptr	     connectionTypes;
    ARRAYofARRAY8Ptr connectionAddresses;
{
    return 0;
}
