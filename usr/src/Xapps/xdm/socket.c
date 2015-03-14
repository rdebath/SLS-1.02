/*
 * xdm - display manager daemon
 *
 * $XConsortium: socket.c,v 1.15 89/12/13 15:24:29 keith Exp $
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
 * socket.c.  Support for XDMCP
 */

# include "dm.h"

# include	<sys/types.h>
# include	<sys/socket.h>
# include	<netinet/in.h>
# include	<sys/un.h>
# include	<X11/X.h>
# include	<netdb.h>

/*
 * interface to policy routines
 */

extern ARRAY8Ptr	ChooseAuthentication ();
extern int		Willing ();
extern ARRAY8Ptr	Accept ();
extern int		SelectConnectionTypeIndex ();

int	socketFd = -1;

FD_TYPE	WellKnownSocketsMask;
int	WellKnownSocketsMax;

#define pS(s)	((s) ? ((char *) (s)) : "empty string")

CreateWellKnownSockets ()
{
    struct sockaddr_in	sock_addr;

    if (request_port == 0)
	    return;
    Debug ("creating socket %d\n", request_port);
    socketFd = socket (AF_INET, SOCK_DGRAM, 0);
    if (socketFd == -1) {
	LogError ("socket creation failed\n");
	return;
    }
    RegisterCloseOnFork (socketFd);
    sock_addr.sin_family = AF_INET;
    sock_addr.sin_port = htons ((short) request_port);
    sock_addr.sin_addr.s_addr = htonl (INADDR_ANY);
    if (bind (socketFd, &sock_addr, sizeof (sock_addr)) == -1)
    {
	LogError ("error binding socket address %d\n", request_port);
	close (socketFd);
	socketFd = -1;
    }
    else {
	WellKnownSocketsMax = socketFd;
	FD_SET (socketFd, &WellKnownSocketsMask);
    }
}

DestroyWellKnownSockets ()
{
    if (socketFd != -1)
    {
	close (socketFd);
	socketFd = -1;
    }
}

AnyWellKnownSockets ()
{
    return socketFd != -1;
}

WaitForSomething ()
{
    FD_TYPE	reads;
    int	nready;
    extern int Rescan, ChildReady;

    Debug ("WaitForSomething\n");
    if (socketFd != -1) {
	reads = WellKnownSocketsMask;
	nready = select (WellKnownSocketsMax + 1, &reads, 0, 0, 0);
	Debug ("select returns %d.  Rescan: %d  ChildReady: %d\n",
		nready, Rescan, ChildReady);
	if (nready > 0 && FD_ISSET (socketFd, &reads))
	    ProcessRequestSocket ();
	if (ChildReady)
	{
	    WaitForChild ();
	}
    } else
	WaitForChild ();
}

/*
 * respond to a request on the UDP socket.
 */

static ARRAY8	Hostname;

registerHostname (name, namelen)
    char    *name;
    int	    namelen;
{
    int	i;

    if (!XdmcpReallocARRAY8 (&Hostname, namelen))
	return;
    for (i = 0; i < namelen; i++)
	Hostname.data[i] = name[i];
}

typedef struct _forwardHost {
    struct _forwardHost	*next;
    struct sockaddr	*addr;
    int			addrlen;
} forwardHost, *forwardHostPtr;

forwardHostPtr	forwardHosts;

registerForwardHost (addr, addrlen)
    struct sockaddr *addr;
    int		    addrlen;
{
    forwardHostPtr  new;

    new = (forwardHostPtr) malloc (sizeof (forwardHost));
    if (!new)
	return;
    new->addr = (struct sockaddr *) malloc ((unsigned) addrlen);
    if (!new->addr)
    {
	free ((char *) new);
	return;
    }
    bcopy (addr, new->addr, addrlen);
    new->addrlen = addrlen;
    new->next = forwardHosts;
    forwardHosts = new;
}

static XdmcpBuffer	buffer;

ProcessRequestSocket ()
{
    XdmcpHeader		header;
    struct sockaddr_in	addr;
    int			addrlen = sizeof addr;

    Debug ("ProcessRequestSocket\n");
    if (!XdmcpFill (socketFd, &buffer, &addr, &addrlen))
	return;
    if (!XdmcpReadHeader (&buffer, &header))
	return;
    if (header.version != XDM_PROTOCOL_VERSION)
	return;
    Debug ("header: %d %d %d\n", header.version, header.opcode, header.length);
    switch (header.opcode)
    {
    case BROADCAST_QUERY:
	broadcast_respond (&addr, addrlen, header.length);
	break;
    case QUERY:
	query_respond (&addr, addrlen, header.length);
	break;
    case INDIRECT_QUERY:
	indirect_respond (&addr, addrlen, header.length);
	break;
    case FORWARD_QUERY:
	forward_respond (&addr, addrlen, header.length);
	break;
    case REQUEST:
	request_respond (&addr, addrlen, header.length);
	break;
    case MANAGE:
	manage (&addr, addrlen, header.length);
	break;
    case KEEPALIVE:
	send_alive (&addr, addrlen, header.length);
	break;
    }
}

query_respond (from, fromlen, length)
    struct sockaddr *from;
    int		    fromlen;
    int		    length;
{
    Debug ("Query respond %d\n", length);
    direct_query_respond (from, fromlen, length, QUERY);
}

broadcast_respond (from, fromlen, length)
    struct sockaddr *from;
    int		    fromlen;
    int		    length;
{
    direct_query_respond (from, fromlen, length, BROADCAST_QUERY);
}

direct_query_respond (from, fromlen, length, type)
    struct sockaddr *from;
    int		    fromlen;
    int		    length;
    xdmOpCode	    type;
{
    ARRAYofARRAY8   queryAuthenticationNames;
    int		    expectedLen;
    int		    i;
    
    if (!XdmcpReadARRAYofARRAY8 (&buffer, &queryAuthenticationNames))
	return;
    expectedLen = 1;
    for (i = 0; i < queryAuthenticationNames.length; i++)
	expectedLen += 2 + queryAuthenticationNames.data[i].length;
    if (length == expectedLen)
	all_query_respond (from, fromlen, &queryAuthenticationNames, type);
    XdmcpDisposeARRAYofARRAY8 (&queryAuthenticationNames);
}

indirect_respond (from, fromlen, length)
    struct sockaddr *from;
    int		    fromlen;
    int		    length;
{
    forwardHostPtr  forward;
    ARRAYofARRAY8   queryAuthenticationNames;
    ARRAY8	    clientAddress;
    ARRAY8	    clientPort;
    int		    expectedLen;
    int		    i;
    XdmcpHeader	    header;
    
    Debug ("Indirect respond %d\n", length);
    if (!XdmcpReadARRAYofARRAY8 (&buffer, &queryAuthenticationNames))
	return;
    expectedLen = 1;
    for (i = 0; i < queryAuthenticationNames.length; i++)
	expectedLen += 2 + queryAuthenticationNames.data[i].length;
    if (length == expectedLen)
    {
    	clientAddress.length = 0;
	clientAddress.data = 0;
    	clientPort.length = 0;
	clientPort.data = 0;
    	switch (from->sa_family) {
#ifdef AF_UNIX
    	case AF_UNIX:
    	    {
	    	struct sockaddr_un	*un_addr;
	    	int			length;
	    	int			i;
    	
	    	un_addr = (struct sockaddr_un *) from;
	    	length = strlen (un_addr->sun_path);
	    	if (XdmcpAllocARRAY8 (&clientAddress, length))
	    	{
	    	    for (i = 0; i < length; i++)
		    	clientAddress.data[i] = un_addr->sun_path[i];
	    	}
	    	break;
    	    }
#endif
#ifdef AF_INET
    	case AF_INET:
    	    {
	    	struct sockaddr_in	*in_addr;
    	
	    	in_addr = (struct sockaddr_in *) from;
	    	if (XdmcpAllocARRAY8 (&clientAddress, 4) &&
	    	    XdmcpAllocARRAY8 (&clientPort, 2))
	    	{
	    	    bcopy (&in_addr->sin_addr, clientAddress.data, 4);
	    	    bcopy (&in_addr->sin_port, clientPort.data, 2);
	    	}
    	    }
    	    break;
#endif
#ifdef AF_CHAOS
    	case AF_CHAOS:
    	    break;
#endif
#ifdef AF_DECnet
    	case AF_DECnet:
    	    break;
#endif
    	}

	/*
	 * set up the forward query packet
	 */
    	header.version = XDM_PROTOCOL_VERSION;
    	header.opcode = (CARD16) FORWARD_QUERY;
    	header.length = 0;
    	header.length += 2 + clientAddress.length;
    	header.length += 2 + clientPort.length;
    	header.length += 1;
    	for (i = 0; i < queryAuthenticationNames.length; i++)
	    header.length += 2 + queryAuthenticationNames.data[i].length;
    	XdmcpWriteHeader (&buffer, &header);
    	XdmcpWriteARRAY8 (&buffer, &clientAddress);
    	XdmcpWriteARRAY8 (&buffer, &clientPort);
    	XdmcpWriteARRAYofARRAY8 (&buffer, &queryAuthenticationNames);

	for (forward = forwardHosts; forward; forward = forward->next)
	{
	    /*
	     * only forward to servers advertising matching
	     * addressing styles
	     */
	    if (forward->addr->sa_family == from->sa_family)
	    {
		XdmcpFlush (socketFd, &buffer,
			    forward->addr, forward->addrlen);
	    }
    	}
	XdmcpDisposeARRAY8 (&clientAddress);
	XdmcpDisposeARRAY8 (&clientPort);
    	all_query_respond (from, fromlen, &queryAuthenticationNames,
			   INDIRECT_QUERY);
    }
    else
    {
	Debug ("Indirect length error got %d expect %d\n", length, expectedLen);
    }
    XdmcpDisposeARRAYofARRAY8 (&queryAuthenticationNames);
}

forward_respond (from, fromlen, length)
    struct sockaddr	*from;
    int			fromlen;
    int			length;
{
    ARRAY8	    clientAddress;
    ARRAY8	    clientPort;
    ARRAYofARRAY8   authenticationNames;
    struct sockaddr *client;
    int		    clientlen;
    int		    expectedLen;
    int		    i;
    
    Debug ("Forward respond %d\n", length);
    clientAddress.length = 0;
    clientAddress.data = 0;
    clientPort.length = 0;
    clientPort.data = 0;
    authenticationNames.length = 0;
    authenticationNames.data = 0;
    if (XdmcpReadARRAY8 (&buffer, &clientAddress) &&
	XdmcpReadARRAY8 (&buffer, &clientPort) &&
	XdmcpReadARRAYofARRAY8 (&buffer, &authenticationNames))
    {
	expectedLen = 0;
	expectedLen += 2 + clientAddress.length;
	expectedLen += 2 + clientPort.length;
	for (i = 0; i < authenticationNames.length; i++)
	    expectedLen += 2 + authenticationNames.data[i].length;
	if (length == expectedLen)
	{
    	    switch (from->sa_family)
    	    {
#ifdef AF_INET
	    case AF_INET:
		{
		    struct sockaddr_in	in_addr;

		    if (clientAddress.length != 4 ||
		        clientPort.length != 2)
		    {
			goto badAddress;
		    }
		    in_addr.sin_family = AF_INET;
		    bcopy (clientAddress.data, &in_addr.sin_addr, 4);
		    bcopy (clientPort.data, &in_addr.sin_port, 2);
		    client = (struct sockaddr *) &in_addr;
		    clientlen = sizeof (in_addr);
		}
		break;
#endif
#ifdef AF_UNIX
	    case AF_UNIX:
		{
		    struct sockaddr_un	un_addr;

		    if (clientAddress.length >= sizeof (un_addr.sun_path))
			goto badAddress;
		    un_addr.sun_family = AF_UNIX;
		    bcopy (clientAddress.data, un_addr.sun_path, clientAddress.length);
		    un_addr.sun_path[clientAddress.length] = '\0';
		    client = (struct sockaddr *) &un_addr;
		    clientlen = sizeof (un_addr);
		}
		break;
#endif
#ifdef AF_CHAOS
	    case AF_CHAOS:
		goto badAddress;
		break;
#endif
#ifdef AF_DECnet
	    case AF_DECnet:
		goto badAddress;
		break;
#endif
    	    }
	    all_query_respond (client, clientlen, &authenticationNames,
			       FORWARD_QUERY);
	}
	else
	{
	    Debug ("Forward length error got %d expect %d\n", length, expectedLen);
	}
    }
badAddress:
    XdmcpDisposeARRAY8 (&clientAddress);
    XdmcpDisposeARRAY8 (&clientPort);
    XdmcpDisposeARRAYofARRAY8 (&authenticationNames);
}

all_query_respond (from, fromlen, authenticationNames, type)
    struct sockaddr	*from;
    int			fromlen;
    ARRAYofARRAY8Ptr	authenticationNames;
    xdmOpCode		type;
{
    ARRAY8Ptr	authenticationName;
    ARRAY8	status;

    authenticationName = ChooseAuthentication (authenticationNames);
    if (Willing (from, fromlen, authenticationName, &status))
	send_willing (from, fromlen, authenticationName, &status);
    else
	if (type == QUERY)
	    send_unwilling (from, fromlen, authenticationName, &status);
    XdmcpDisposeARRAY8 (&status);
}

send_willing (from, fromlen, authenticationName, status)
    struct sockaddr *from;
    int		    fromlen;
    ARRAY8Ptr	    authenticationName;
    ARRAY8Ptr	    status;
{
    XdmcpHeader	header;

    Debug ("Send willing %*.*s %*.*s\n", authenticationName->length,
					 authenticationName->length,
					 pS(authenticationName->data),
					 status->length,
					 status->length,
					 pS(status->data));
    header.version = XDM_PROTOCOL_VERSION;
    header.opcode = (CARD16) WILLING;
    header.length = 6 + authenticationName->length +
		    Hostname.length + status->length;
    XdmcpWriteHeader (&buffer, &header);
    XdmcpWriteARRAY8 (&buffer, authenticationName);
    XdmcpWriteARRAY8 (&buffer, &Hostname);
    XdmcpWriteARRAY8 (&buffer, status);
    XdmcpFlush (socketFd, &buffer, from, fromlen);
}

send_unwilling (from, fromlen, authenticationName, status)
    struct sockaddr *from;
    int		    fromlen;
    ARRAY8Ptr	    authenticationName;
    ARRAY8Ptr	    status;
{
    XdmcpHeader	header;

    Debug ("Send unwilling %*.*s %*.*s\n", authenticationName->length,
					 authenticationName->length,
					 pS(authenticationName->data),
					 status->length,
					 status->length,
					 pS(status->data));
    header.version = XDM_PROTOCOL_VERSION;
    header.opcode = (CARD16) UNWILLING;
    header.length = 4 + Hostname.length + status->length;
    XdmcpWriteHeader (&buffer, &header);
    XdmcpWriteARRAY8 (&buffer, &Hostname);
    XdmcpWriteARRAY8 (&buffer, status);
    XdmcpFlush (socketFd, &buffer, from, fromlen);
}

static unsigned long	globalSessionID;

#define NextSessionID()    (++globalSessionID)
    
static ARRAY8 outOfMemory = { (CARD16) 13, (CARD8Ptr) "Out of memory" };
static ARRAY8 noValidAddr = { (CARD16) 16, (CARD8Ptr) "No valid address" };
static ARRAY8 noValidAuth = { (CARD16) 22, (CARD8Ptr) "No valid authorization" };
static ARRAY8 noAuthentic = { (CARD16) 29, (CARD8Ptr) "XDM has no authentication key" };

request_respond (from, fromlen, length)
    struct sockaddr *from;
    int		    fromlen;
    int		    length;
{
    CARD16	    displayNumber;
    ARRAY16	    connectionTypes;
    ARRAYofARRAY8   connectionAddresses;
    ARRAY8	    authenticationName;
    ARRAY8	    authenticationData;
    ARRAYofARRAY8   authorizationNames;
    ARRAY8	    manufacturerDisplayID;
    ARRAY8Ptr	    reason;
    int		    expectlen;
    int		    i, j;
    struct protoDisplay  *pdpy;
    ARRAY8	    authorizationName, authorizationData;
    ARRAY8Ptr	    connectionAddress;

    Debug ("Request respond %d\n", length);
    connectionTypes.data = 0;
    connectionAddresses.data = 0;
    authenticationName.data = 0;
    authenticationData.data = 0;
    authorizationNames.data = 0;
    authorizationName.length = 0;
    authorizationData.length = 0;
    manufacturerDisplayID.data = 0;
    if (XdmcpReadCARD16 (&buffer, &displayNumber) &&
	XdmcpReadARRAY16 (&buffer, &connectionTypes) &&
	XdmcpReadARRAYofARRAY8 (&buffer, &connectionAddresses) &&
	XdmcpReadARRAY8 (&buffer, &authenticationName) &&
	XdmcpReadARRAY8 (&buffer, &authenticationData) &&
	XdmcpReadARRAYofARRAY8 (&buffer, &authorizationNames) &&
	XdmcpReadARRAY8 (&buffer, &manufacturerDisplayID))
    {
	expectlen = 0;
	expectlen += 2;				    /* displayNumber */
	expectlen += 1 + 2*connectionTypes.length;  /* connectionTypes */
	expectlen += 1;				    /* connectionAddresses */
	for (i = 0; i < connectionAddresses.length; i++)
	    expectlen += 2 + connectionAddresses.data[i].length;
	expectlen += 2 + authenticationName.length; /* authenticationName */
	expectlen += 2 + authenticationData.length; /* authenticationData */
	expectlen += 1;				    /* authoriationNames */
	for (i = 0; i < authorizationNames.length; i++)
	    expectlen += 2 + authorizationNames.data[i].length;
	expectlen += 2 + manufacturerDisplayID.length;	/* displayID */
	if (expectlen != length)
	{
	    Debug ("Request length error got %d expect %d\n", length, expectlen);
	    goto abort;
	}
	if (connectionTypes.length == 0 ||
	    connectionAddresses.length != connectionTypes.length)
	{
	    reason = &noValidAddr;
	    pdpy = 0;
	    goto decline;
	}
	if (pdpy = FindProtoDisplay (from, fromlen, displayNumber))
	    goto accept;
	reason = Accept (from, fromlen, displayNumber);
	if (reason)
	    goto decline;
	i = SelectConnectionTypeIndex (&connectionTypes,
				       &connectionAddresses);
	if (i < 0)
	{
	    reason = &noValidAddr;
	    goto decline;
	}
	if (authorizationNames.length == 0)
	    j = 0;
	else
	    j = SelectAuthorizationTypeIndex (&authenticationName,
					      &authorizationNames);
	if (j < 0)
	{
	    reason = &noValidAuth;
	    goto decline;
	}
	connectionAddress = &connectionAddresses.data[i];
	pdpy = NewProtoDisplay (from, fromlen,
				displayNumber,
				connectionTypes.data[i],
				connectionAddress,
				NextSessionID());
	if (!pdpy)
	{
	    reason = &outOfMemory;
	    goto decline;
	}
	if (!CheckAuthentication (pdpy,
				  &manufacturerDisplayID,
				  &authenticationName,
				  &authenticationData))
	{
	    reason = &noAuthentic;
	    goto decline;
	}
	if (j < authorizationNames.length)
	{
	    Xauth   *auth;
	    SetProtoDisplayAuthorization (pdpy,
		(unsigned short) authorizationNames.data[j].length,
		(char *) authorizationNames.data[j].data);
	    auth = pdpy->xdmcpAuthorization;
	    if (!auth)
		auth = pdpy->fileAuthorization;
	    if (auth)
	    {
		authorizationName.length = auth->name_length;
		authorizationName.data = (CARD8Ptr) auth->name;
		authorizationData.length = auth->data_length;
		authorizationData.data = (CARD8Ptr) auth->data;
	    }
	}
	if (pdpy)
	{
accept:	    ;
	    send_accept (from, fromlen, pdpy->sessionID,
				        &authenticationName,
					&authenticationData,
					&authorizationName,
					&authorizationData);
	}
	else
	{
decline:    ;
	    send_decline (from, fromlen, &authenticationName,
				 &authenticationData,
				 reason);
	}
    }
abort:
    XdmcpDisposeARRAY16 (&connectionTypes);
    XdmcpDisposeARRAYofARRAY8 (&connectionAddresses);
    XdmcpDisposeARRAY8 (&authenticationName);
    XdmcpDisposeARRAY8 (&authenticationData);
    XdmcpDisposeARRAYofARRAY8 (&authorizationNames);
    XdmcpDisposeARRAY8 (&manufacturerDisplayID);
}

send_accept (to, tolen, sessionID,
	     authenticationName, authenticationData,
	     authorizationName, authorizationData)
    struct sockaddr *to;
    int		    tolen;
    CARD32	    sessionID;
    ARRAY8Ptr	    authenticationName, authenticationData;
    ARRAY8Ptr	    authorizationName, authorizationData;
{
    XdmcpHeader	header;

    Debug ("Accept %d\n", sessionID);
    header.version = XDM_PROTOCOL_VERSION;
    header.opcode = (CARD16) ACCEPT;
    header.length = 4;			    /* session ID */
    header.length += 2 + authenticationName->length;
    header.length += 2 + authenticationData->length;
    header.length += 2 + authorizationName->length;
    header.length += 2 + authorizationData->length;
    XdmcpWriteHeader (&buffer, &header);
    XdmcpWriteCARD32 (&buffer, sessionID);
    XdmcpWriteARRAY8 (&buffer, authenticationName);
    XdmcpWriteARRAY8 (&buffer, authenticationData);
    XdmcpWriteARRAY8 (&buffer, authorizationName);
    XdmcpWriteARRAY8 (&buffer, authorizationData);
    XdmcpFlush (socketFd, &buffer, to, tolen);
}
   
send_decline (to, tolen, authenticationName, authenticationData, status)
    struct sockaddr *to;
    int		    tolen;
    ARRAY8Ptr	    authenticationName, authenticationData;
    ARRAY8Ptr	    status;
{
    XdmcpHeader	header;

    Debug ("Decline %*.*s\n", status->length, status->length, pS(status->data));
    header.version = XDM_PROTOCOL_VERSION;
    header.opcode = (CARD16) DECLINE;
    header.length = 0;
    header.length += 2 + status->length;
    header.length += 2 + authenticationName->length;
    header.length += 2 + authenticationData->length;
    XdmcpWriteHeader (&buffer, &header);
    XdmcpWriteARRAY8 (&buffer, status);
    XdmcpWriteARRAY8 (&buffer, authenticationName);
    XdmcpWriteARRAY8 (&buffer, authenticationData);
    XdmcpFlush (socketFd, &buffer, to, tolen);
}

manage (from, fromlen, length)
    struct sockaddr *from;
    int		    fromlen;
    int		    length;
{
    CARD32		sessionID;
    CARD16		displayNumber;
    ARRAY8		displayClass;
    int			expectlen;
    struct protoDisplay	*pdpy;
    struct display	*d;
    char		*name;
    char		*class;
    struct sockaddr	*from_save;

    Debug ("Manage %d\n", length);
    displayClass.data = 0;
    displayClass.length = 0;
    if (XdmcpReadCARD32 (&buffer, &sessionID) &&
	XdmcpReadCARD16 (&buffer, &displayNumber) &&
	XdmcpReadARRAY8 (&buffer, &displayClass))
    {
	expectlen = 4 +				/* session ID */
		    2 +				/* displayNumber */
		    2 + displayClass.length;	/* displayClass */
	if (expectlen != length)
	{
	    Debug ("Manage length error got %d expect %d\n", length, expectlen);
	    goto abort;
	}
	pdpy = FindProtoDisplay (from, fromlen, displayNumber);
	if (!pdpy || pdpy->sessionID != sessionID)
	{
	    /*
	     * We may have already started a session for this display
	     * but it hasn't seen the response in the form of an
	     * XOpenDisplay() yet. So check if it is in the list of active
	     * displays, and if so check that the session id's match.
	     * If all this is true, then we have a duplicate request that
	     * can be ignored.
	     */
	    if (!pdpy 
		&& (d = FindDisplayByAddress(from, fromlen, displayNumber))
		&& d->sessionID == sessionID) {
		     Debug("manage: got duplicate pkt, ignoring\n");
		     goto abort;
	    }
	    send_refuse (from, fromlen, sessionID);
	}
	else
	{
	    char	*NetworkAddressToName ();

	    name = NetworkAddressToName (pdpy->connectionType,
					 &pdpy->connectionAddress,
					 pdpy->displayNumber);
	    Debug ("Computed display name: %s\n", name);
	    if (!name)
	    {
		send_failed (from, fromlen, "(no name)", sessionID, "out of memory");
		goto abort;
	    }
	    d = FindDisplayByName (name);
	    if (d)
	    {
		Debug ("Terminating active session for %s\n", d->name);
		StopDisplay (d);
	    }
	    class = malloc (displayClass.length + 1);
	    if (!class)
	    {
		send_failed (from, fromlen, name, sessionID, "out of memory");
		goto abort;
	    }
	    if (displayClass.length)
	    {
		bcopy (displayClass.data, class, displayClass.length);
		class[displayClass.length] = '\0';
	    }
	    else
		class = (char *) 0;
	    from_save = (struct sockaddr *) malloc (fromlen);
	    if (!from_save)
	    {
		send_failed (from, fromlen, name, sessionID, "out of memory");
		goto abort;
	    }
	    bcopy (from, from_save, fromlen);
	    d = NewDisplay (name, class);
	    d->displayType.location = Foreign;
	    d->displayType.lifetime = Transient;
	    d->displayType.origin = FromXDMCP;
	    d->sessionID = pdpy->sessionID;
	    d->from = from_save;
	    d->fromlen = fromlen;
	    d->displayNumber = pdpy->displayNumber;
	    d->authorization = pdpy->fileAuthorization;
	    if (d->authorization)
		d->authorize = TRUE;
	    if (remoteAuthDir)
	    {
		int len;

		len = strlen (remoteAuthDir) + 12;
		if (d->authFile)
		    free (d->authFile);
		d->authFile = malloc ((unsigned) len);
		sprintf (d->authFile, "%s/AuthXXXXXX", remoteAuthDir);
		(void) mktemp (d->authFile);
	    }
	    pdpy->fileAuthorization = 0;
	    DisposeProtoDisplay (pdpy);
	    Debug ("Starting display %s,%s\n", d->name, d->class);
	    StartDisplay (d);
	}
    }
abort:
    XdmcpDisposeARRAY8 (&displayClass);
}

SendFailed (d, reason)
    struct display  *d;
    char	    *reason;
{
    Debug ("Display start failed, sending Failed\n");
    send_failed (d->from, d->fromlen, d->name, d->sessionID, reason);
}

send_failed (from, fromlen, name, sessionID, reason)
    struct sockaddr *from;
    int		    fromlen;
    char	    *name;
    CARD32	    sessionID;
    char	    *reason;
{
    static char	buf[256];
    XdmcpHeader	header;
    ARRAY8	status;

    sprintf (buf, "Session %d failed for display %s: %s",
	     sessionID, name, reason);
    Debug ("Send failed %d %s\n", sessionID, buf);
    status.length = strlen (buf);
    status.data = (CARD8Ptr) buf;
    header.version = XDM_PROTOCOL_VERSION;
    header.opcode = (CARD16) FAILED;
    header.length = 6 + status.length;
    XdmcpWriteHeader (&buffer, &header);
    XdmcpWriteCARD32 (&buffer, sessionID);
    XdmcpWriteARRAY8 (&buffer, &status);
    XdmcpFlush (socketFd, &buffer, from, fromlen);
}

send_refuse (from, fromlen, sessionID)
    struct sockaddr *from;
    int		    fromlen;
    CARD32	    sessionID;
{
    XdmcpHeader	header;

    Debug ("Send refuse %d\n", sessionID);
    header.version = XDM_PROTOCOL_VERSION;
    header.opcode = (CARD16) REFUSE;
    header.length = 4;
    XdmcpWriteHeader (&buffer, &header);
    XdmcpWriteCARD32 (&buffer, sessionID);
    XdmcpFlush (socketFd, &buffer, from, fromlen);
}

send_alive (from, fromlen, length)
    struct sockaddr *from;
    int		    fromlen;
    int		    length;
{
    CARD32		sessionID;
    CARD16		displayNumber;
    struct display	*d;
    XdmcpHeader		header;
    CARD8		sendRunning;
    CARD32		sendSessionID;

    Debug ("Send alive\n");
    if (XdmcpReadCARD16 (&buffer, &displayNumber) &&
	XdmcpReadCARD32 (&buffer, &sessionID))
    {
	if (length == 6)
	{
	    d = FindDisplayBySessionID (sessionID);
	    if (!d) {
		d = FindDisplayByAddress (from, fromlen, displayNumber);
	    }
	    sendRunning = 0;
	    sendSessionID = 0;
	    if (d && d->status == running)
 	    {
		if (d->sessionID == sessionID)
		    sendRunning = 1;
		sendSessionID = d->sessionID;
	    }
	    header.version = XDM_PROTOCOL_VERSION;
	    header.opcode = (CARD16) ALIVE;
	    header.length = 5;
	    Debug ("alive: %d %d\n", sendRunning, sendSessionID);
	    XdmcpWriteHeader (&buffer, &header);
	    XdmcpWriteCARD8 (&buffer, sendRunning);
	    XdmcpWriteCARD32 (&buffer, sendSessionID);
	    XdmcpFlush (socketFd, &buffer, from, fromlen);
	}
    }
}

char *
NetworkAddressToName(connectionType, connectionAddress, displayNumber)
CARD16	    connectionType;
ARRAY8Ptr   connectionAddress;
CARD16	    displayNumber;
{
    switch (connectionType)
    {
    case FamilyInternet:
	{
	    CARD8		*data;
	    struct hostent	*hostent;
	    char		*name;
	    char		*localhost, *localHostname();

	    data = connectionAddress->data;
	    hostent = gethostbyaddr (data,
				     connectionAddress->length, AF_INET);

	    localhost = localHostname ();

#define getString(name,len)	((name = malloc (len + 1)) ? 1 : 0)

	    if (hostent)
	    {
		if (!strcmp (localhost, hostent->h_name))
		{
		    if (!getString (name, 10))
			return 0;
		    sprintf (name, ":%d", displayNumber);
		}
		else
		{
		    if (removeDomainname)
		    {
		    	char    *localDot, *remoteDot;
    
			/* check for a common domain name.  This
			 * could reduce names by recognising common
			 * super-domain names as well, but I don't think
			 * this is as useful, and will confuse more
			 * people
 			 */
		    	if ((localDot = index (localhost, '.')) &&
		            (remoteDot = index (hostent->h_name, '.')))
			{
			    /* smash the name in place; it won't
			     * be needed later.
			     */
			    if (!strcmp (localDot+1, remoteDot+1))
				*remoteDot = '\0';
			}
		    }

		    if (!getString (name, strlen (hostent->h_name) + 10))
			return 0;
		    sprintf (name, "%s:%d", hostent->h_name, displayNumber);
		}
	    }
	    else
	    {
		if (!getString (name, 25))
		    return 0;
		sprintf(name, "%d.%d.%d.%d:%d",
			data[0], data[1], data[2], data[3], displayNumber);
	    }
	    return name;
	}
#ifdef DNET
    case FamilyDECnet:
	return NULL;
#endif DNET
    default:
	return NULL;
    }
}

static char localHostbuf[256];
static int  gotLocalHostname;

char *
localHostname ()
{
    if (!gotLocalHostname)
    {
	XmuGetHostname (localHostbuf, sizeof (localHostbuf) - 1);
	gotLocalHostname = 1;
    }
    return localHostbuf;
}
