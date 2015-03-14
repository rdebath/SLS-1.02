/*
 * xdm - display manager daemon
 *
 * $XConsortium: policy.c,v 1.10 91/07/24 00:06:48 keith Exp $
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

#ifdef XDMCP

# include <X11/X.h>
# include <sys/socket.h>
#ifdef AF_INET
# include <netinet/in.h>
#endif

static ARRAY8 noAuthentication = { (CARD16) 0, (CARD8Ptr) 0 };

typedef struct _XdmAuth {
    ARRAY8  authentication;
    ARRAY8  authorization;
} XdmAuthRec, *XdmAuthPtr;

static XdmAuthRec auth[] = {
#ifdef HASXDMAUTH
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

    for (i = 0; i < (int)authenticationNames->length; i++)
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
#ifdef HASXDMAUTH
    if (name->length && !strncmp ((char *)name->data, "XDM-AUTHENTICATION-1", 20))
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
    	for (i = 0; i < (int)authorizationNames->length; i++)
	    if (XdmcpARRAY8Equal (&authorizationNames->data[i],
				  &auth[j].authorization))
	    	return i;
    }
    for (i = 0; i < (int)authorizationNames->length; i++)
	if (ValidAuthorization (authorizationNames->data[i].length,
				(char *) authorizationNames->data[i].data))
	    return i;
    return -1;
}

/*ARGSUSED*/
int
Willing (addr, connectionType, authenticationName, status, type)
    ARRAY8Ptr	    addr;
    CARD16	    connectionType;
    ARRAY8Ptr	    authenticationName;
    ARRAY8Ptr	    status;
    xdmOpCode	    type;
{
    char	statusBuf[256];
    int		ret;
    
    ret = AcceptableDisplayAddress (addr, connectionType, type);
    if (!ret)
	sprintf (statusBuf, "Display not authorized to connect");
    else
	sprintf (statusBuf, "Willing to manage");
    status->length = strlen (statusBuf);
    status->data = (CARD8Ptr) malloc (status->length);
    if (!status->data)
	status->length = 0;
    else
	bcopy (statusBuf, status->data, status->length);
    return ret;
}

/*ARGSUSED*/
ARRAY8Ptr
Accept (from, fromlen, displayNumber)
    struct sockaddr *from;
    int		    fromlen;
    CARD16	    displayNumber;
{
    return 0;
}

/*ARGSUSED*/
int
SelectConnectionTypeIndex (connectionTypes, connectionAddresses)
    ARRAY16Ptr	     connectionTypes;
    ARRAYofARRAY8Ptr connectionAddresses;
{
    return 0;
}

#endif /* XDMCP */
