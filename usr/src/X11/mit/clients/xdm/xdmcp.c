/* $Header: /home/x_cvs/mit/clients/xdm/xdmcp.c,v 1.4 1992/06/18 11:21:39 dawes Exp $ */
/*
 * xdm - display manager daemon
 *
 * $XConsortium: xdmcp.c,v 1.6 92/03/24 10:30:37 gildea Exp $
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
 * xdmcp.c - Support for XDMCP
 */

# include "dm.h"

#ifdef XDMCP

# include	<X11/X.h>
# include	<X11/Xfuncs.h>
# include	<sys/types.h>
# include	<ctype.h>

#include	<sys/socket.h>
#include	<netinet/in.h>
#ifndef ISC
#include	<sys/un.h>
#endif
#include	<netdb.h>

#define getString(name,len)	((name = malloc (len + 1)) ? 1 : 0)

/*
 * interface to policy routines
 */

extern ARRAY8Ptr	ChooseAuthentication ();
extern int		Willing ();
extern ARRAY8Ptr	Accept ();
extern int		SelectConnectionTypeIndex ();

int	xdmcpFd = -1;
int	chooserFd = -1;

FD_TYPE	WellKnownSocketsMask;
int	WellKnownSocketsMax;

#define pS(s)	((s) ? ((char *) (s)) : "empty string")

DestroyWellKnownSockets ()
{
    if (xdmcpFd != -1)
    {
	close (xdmcpFd);
	xdmcpFd = -1;
    }
    if (chooserFd != -1)
    {
	close (chooserFd);
	chooserFd = -1;
    }
}

AnyWellKnownSockets ()
{
    return xdmcpFd != -1 || chooserFd != -1;
}

WaitForSomething ()
{
    FD_TYPE	reads;
    int	nready;
    extern int Rescan, ChildReady;

    Debug ("WaitForSomething\n");
    if (AnyWellKnownSockets () && !ChildReady) {
	reads = WellKnownSocketsMask;
	nready = select (WellKnownSocketsMax + 1, &reads, 0, 0, 0);
	Debug ("select returns %d.  Rescan: %d  ChildReady: %d\n",
		nready, Rescan, ChildReady);
	if (nready > 0)
	{
	    if (xdmcpFd >= 0 && FD_ISSET (xdmcpFd, &reads))
		ProcessRequestSocket ();
	    if (chooserFd >= 0 && FD_ISSET (chooserFd, &reads))
		ProcessChooserSocket (chooserFd);
	}
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

static XdmcpBuffer	buffer;

ProcessRequestSocket ()
{
    XdmcpHeader		header;
    struct sockaddr_in	addr;
    int			addrlen = sizeof addr;

    Debug ("ProcessRequestSocket\n");
    bzero ((char *) &addr, sizeof (addr));
    if (!XdmcpFill (xdmcpFd, &buffer, &addr, &addrlen)) {
	Debug ("XdmcpFill failed\n");
	return;
    }
    if (!XdmcpReadHeader (&buffer, &header)) {
	Debug ("XdmcpReadHeader failed\n");
	return;
    }
    if (header.version != XDM_PROTOCOL_VERSION) {
	Debug ("XDMCP header version read was %d, expected %d\n",
	       header.version, XDM_PROTOCOL_VERSION);
	return;
    }
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
    for (i = 0; i < (int)queryAuthenticationNames.length; i++)
	expectedLen += 2 + queryAuthenticationNames.data[i].length;
    if (length == expectedLen)
	all_query_respond (from, fromlen, &queryAuthenticationNames, type);
    XdmcpDisposeARRAYofARRAY8 (&queryAuthenticationNames);
}

/*ARGSUSED*/
static int
sendForward (connectionType, address, closure)
    CARD16	connectionType;
    ARRAY8Ptr	address;
    char	*closure;
{
#ifdef AF_INET
    struct sockaddr_in	    in_addr;
#endif
#ifdef AF_DECnet
#endif
    struct sockaddr	    *addr;
    int			    addrlen;

    switch (connectionType)
    {
#ifdef AF_INET
    case FamilyInternet:
	addr = (struct sockaddr *) &in_addr;
	bzero ((char *) &in_addr, sizeof (in_addr));
	in_addr.sin_family = AF_INET;
	in_addr.sin_port = htons ((short) XDM_UDP_PORT);
	if (address->length != 4)
	    return;
	bcopy (address->data, (char *) &in_addr.sin_addr, address->length);
	addrlen = sizeof (struct sockaddr_in);
	break;
#endif
#ifdef AF_DECnet
    case FamilyDECnet:
#endif
    default:
	return;
    }
    XdmcpFlush (xdmcpFd, &buffer, addr, addrlen);
}

extern char *NetaddrAddress();
extern char *NetaddrPort();

static void
ClientAddress (from, addr, port, type)
    struct sockaddr *from;
    ARRAY8Ptr	    addr, port;	/* return */
    CARD16	    *type;	/* return */
{
    int length, family;
    char *data;

    data = NetaddrPort(from, &length);
    XdmcpAllocARRAY8 (port, length);
    bcopy (data, port->data, length);
    port->length = length;

    family = ConvertAddr(from, &length, &data);
    XdmcpAllocARRAY8 (addr, length);
    bcopy (data, addr->data, length);
    addr->length = length;

    *type = family;
}

/* computes an X display name */

static char *
NetworkAddressToName(connectionType, connectionAddress, displayNumber)
    CARD16	connectionType;
    ARRAY8Ptr   connectionAddress;
    CARD16	displayNumber;
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
	    hostent = gethostbyaddr ((char *)data,
				     connectionAddress->length, AF_INET);

	    localhost = localHostname ();

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
#endif /* DNET */
    default:
	return NULL;
    }
}

indirect_respond (from, fromlen, length)
    struct sockaddr *from;
    int		    fromlen;
    int		    length;
{
    ARRAYofARRAY8   queryAuthenticationNames;
    ARRAY8	    clientAddress;
    ARRAY8	    clientPort;
    CARD16	    connectionType;
    int		    expectedLen;
    int		    i;
    XdmcpHeader	    header;
    int		    localHostAsWell;
    
    Debug ("Indirect respond %d\n", length);
    if (!XdmcpReadARRAYofARRAY8 (&buffer, &queryAuthenticationNames))
	return;
    expectedLen = 1;
    for (i = 0; i < (int)queryAuthenticationNames.length; i++)
	expectedLen += 2 + queryAuthenticationNames.data[i].length;
    if (length == expectedLen)
    {
	ClientAddress (from, &clientAddress, &clientPort, &connectionType);
	/*
	 * set up the forward query packet
	 */
    	header.version = XDM_PROTOCOL_VERSION;
    	header.opcode = (CARD16) FORWARD_QUERY;
    	header.length = 0;
    	header.length += 2 + clientAddress.length;
    	header.length += 2 + clientPort.length;
    	header.length += 1;
    	for (i = 0; i < (int)queryAuthenticationNames.length; i++)
	    header.length += 2 + queryAuthenticationNames.data[i].length;
    	XdmcpWriteHeader (&buffer, &header);
    	XdmcpWriteARRAY8 (&buffer, &clientAddress);
    	XdmcpWriteARRAY8 (&buffer, &clientPort);
    	XdmcpWriteARRAYofARRAY8 (&buffer, &queryAuthenticationNames);

	localHostAsWell = ForEachMatchingIndirectHost (&clientAddress, connectionType, sendForward, (char *) 0);
	
	XdmcpDisposeARRAY8 (&clientAddress);
	XdmcpDisposeARRAY8 (&clientPort);
	if (localHostAsWell)
	    all_query_respond (from, fromlen, &queryAuthenticationNames,
			   INDIRECT_QUERY);
    }
    else
    {
	Debug ("Indirect length error got %d expect %d\n", length, expectedLen);
    }
    XdmcpDisposeARRAYofARRAY8 (&queryAuthenticationNames);
}

/*ARGSUSED*/
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
	expectedLen += 1;	    /* authenticationNames */
	for (i = 0; i < (int)authenticationNames.length; i++)
	    expectedLen += 2 + authenticationNames.data[i].length;
	if (length == expectedLen)
	{
	    int	j;

	    j = 0;
	    for (i = 0; i < (int)clientPort.length; i++)
		j = j * 256 + clientPort.data[i];
	    Debug ("Forward client address (port %d)", j);
	    for (i = 0; i < (int)clientAddress.length; i++)
		Debug (" %d", clientAddress.data[i]);
	    Debug ("\n");
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
		    bzero ((char *) &in_addr, sizeof (in_addr));
		    in_addr.sin_family = AF_INET;
		    bcopy (clientAddress.data, &in_addr.sin_addr, 4);
		    bcopy (clientPort.data, (char *) &in_addr.sin_port, 2);
		    client = (struct sockaddr *) &in_addr;
		    clientlen = sizeof (in_addr);
		}
		break;
#endif
#ifndef ISC
#ifdef AF_UNIX
	    case AF_UNIX:
		{
		    struct sockaddr_un	un_addr;

		    if (clientAddress.length >= sizeof (un_addr.sun_path))
			goto badAddress;
		    bzero ((char *) &un_addr, sizeof (un_addr));
		    un_addr.sun_family = AF_UNIX;
		    bcopy (clientAddress.data, un_addr.sun_path, clientAddress.length);
		    un_addr.sun_path[clientAddress.length] = '\0';
		    client = (struct sockaddr *) &un_addr;
		    clientlen = sizeof (un_addr);
		}
		break;
#endif
#endif /* ISC */
#ifdef AF_CHAOS
	    case AF_CHAOS:
		goto badAddress;
#endif
#ifdef AF_DECnet
	    case AF_DECnet:
		goto badAddress;
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
    ARRAY8	addr;
    CARD16	connectionType;
    int		length;

    connectionType = ConvertAddr(from, &length, &(addr.data));
    addr.length = length;	/* convert int to short */
    Debug ("all_query_respond: conntype=%d, addr=%lx, len=%d\n",
	   connectionType, *(addr.data), addr.length);
    if (connectionType < 0)
	return;

    if (type == INDIRECT_QUERY)
	RememberIndirectClient (&addr, connectionType);
    else
	ForgetIndirectClient (&addr, connectionType);

    authenticationName = ChooseAuthentication (authenticationNames);
    if (Willing (&addr, connectionType, authenticationName, &status, type))
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
    XdmcpFlush (xdmcpFd, &buffer, from, fromlen);
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
    XdmcpFlush (xdmcpFd, &buffer, from, fromlen);
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
	for (i = 0; i < (int)connectionAddresses.length; i++)
	    expectlen += 2 + connectionAddresses.data[i].length;
	expectlen += 2 + authenticationName.length; /* authenticationName */
	expectlen += 2 + authenticationData.length; /* authenticationData */
	expectlen += 1;				    /* authoriationNames */
	for (i = 0; i < (int)authorizationNames.length; i++)
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
	Debug ("NewProtoDisplay 0x%x\n", pdpy);
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
	if (j < (int)authorizationNames.length)
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

    Debug ("Accept Session ID %d\n", sessionID);
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
    XdmcpFlush (xdmcpFd, &buffer, to, tolen);
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
    XdmcpFlush (xdmcpFd, &buffer, to, tolen);
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
    XdmcpNetaddr	from_save;
    ARRAY8		clientAddress, clientPort;
    CARD16		connectionType;

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
	Debug ("Manage Session ID %d, pdpy 0x%x\n", sessionID, pdpy);
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
	    Debug ("Session ID %d refused\n", sessionID);
	    if (pdpy)
		Debug ("Existing Session ID %d\n", pdpy->sessionID);
	    send_refuse (from, fromlen, sessionID);
	}
	else
	{
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
		extern void StopDisplay ();

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
	    from_save = (XdmcpNetaddr) malloc (fromlen);
	    if (!from_save)
	    {
		send_failed (from, fromlen, name, sessionID, "out of memory");
		goto abort;
	    }
	    bcopy (from, from_save, fromlen);
	    d = NewDisplay (name, class);
	    if (!d)
	    {
		free ((char *) from_save);
		send_failed (from, fromlen, name, sessionID, "out of memory");
		goto abort;
	    }
	    d->displayType.location = Foreign;
	    d->displayType.lifetime = Transient;
	    d->displayType.origin = FromXDMCP;
	    d->sessionID = pdpy->sessionID;
	    d->from = from_save;
	    d->fromlen = fromlen;
	    d->displayNumber = pdpy->displayNumber;
	    ClientAddress (from, &clientAddress, &clientPort, &connectionType);
	    d->useChooser = 0;
	    if (IsIndirectClient (&clientAddress, connectionType))
	    {
		Debug ("IsIndirectClient\n");
		ForgetIndirectClient (&clientAddress, connectionType);
		if (UseChooser (&clientAddress, connectionType))
		{
		    d->useChooser = 1;
		    Debug ("Use chooser for %s\n", d->name);
		}
	    }
	    d->clientAddr = clientAddress;
	    d->connectionType = connectionType;
	    XdmcpDisposeARRAY8 (&clientPort);
	    if (pdpy->fileAuthorization)
	    {
		d->authorizations = (Xauth **) malloc (sizeof (Xauth *));
		if (!d->authorizations)
		{
		    free ((char *) from_save);
		    free ((char *) d);
		    send_failed (from, fromlen, name, sessionID, "out of memory");
		    goto abort;
		}
		d->authorizations[0] = pdpy->fileAuthorization;
		d->authNum = 1;
		pdpy->fileAuthorization = 0;
	    }
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
    XdmcpFlush (xdmcpFd, &buffer, from, fromlen);
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
    XdmcpFlush (xdmcpFd, &buffer, from, fromlen);
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
	    XdmcpFlush (xdmcpFd, &buffer, from, fromlen);
	}
    }
}

char *
NetworkAddressToHostname (connectionType, connectionAddress)
    CARD16	connectionType;
    ARRAY8Ptr   connectionAddress;
{
    char    *name = 0;

    switch (connectionType)
    {
    case FamilyInternet:
	{
	    struct hostent	*hostent;
	    char dotted[20];
	    char *local_name;

	    hostent = gethostbyaddr ((char *)connectionAddress->data,
				     connectionAddress->length, AF_INET);

	    if (hostent)
		local_name = hostent->h_name;
	    else {
		/* can't get name, so use emergency fallback */
		sprintf(dotted, "%d.%d.%d.%d",
			connectionAddress->data[0],
			connectionAddress->data[1],
			connectionAddress->data[2],
			connectionAddress->data[3]);
		local_name = dotted;
	    }
	    if (!getString (name, strlen (local_name)))
		break;
	    strcpy (name, local_name);
	    break;
	}
#ifdef DNET
    case FamilyDECnet:
	break;
#endif /* DNET */
    default:
	break;
    }
    return name;
}

static
HostnameToNetworkAddress (name, connectionType, connectionAddress)
char	    *name;
CARD16	    connectionType;
ARRAY8Ptr   connectionAddress;
{
    switch (connectionType)
    {
    case FamilyInternet:
	{
	    struct hostent	*hostent;

	    hostent = gethostbyname (name);
	    if (!hostent)
		return FALSE;
	    if (!XdmcpAllocARRAY8 (connectionAddress, hostent->h_length))
		return FALSE;
	    bcopy (hostent->h_addr, connectionAddress->data, hostent->h_length);
	    return TRUE;
	}
#ifdef DNET
    case FamilyDECnet:
	return FALSE;
#endif
    }
    return FALSE;
}

/*
 * converts a display name into a network address, using
 * the same rules as XOpenDisplay (algorithm cribbed from there)
 */

static
NameToNetworkAddress(name, connectionTypep, connectionAddress, displayNumber)
char	    *name;
CARD16Ptr   connectionTypep;
ARRAY8Ptr   connectionAddress;
CARD16Ptr   displayNumber;
{
    char    *colon, *display_number;
    char    hostname[1024];
    int	    dnet = FALSE;
    CARD16  number;
    CARD16  connectionType;

    colon = index (name, ':');
    if (!colon)
	return FALSE;
    if (colon != name)
    {
	if (colon - name > sizeof (hostname))
	    return FALSE;
	strncpy (hostname, name, colon - name);
	hostname[colon - name] = '\0';
    }
    else
    {
	strcpy (hostname, localHostname ());
    }
    if (colon[1] == ':')
    {
	dnet = TRUE;
	colon++;
    }
#ifndef DNETCONN
    if (dnet)
	return FALSE;
#endif
    display_number = colon + 1;
    while (*display_number && *display_number != '.')
    {
	if (!isascii (*display_number) || !isdigit(*display_number))
	    return FALSE;
    }
    if (display_number == colon + 1)
	return FALSE;
    number = atoi (colon + 1);
#ifdef DNETCONN
    if (dnet)
	connectionType = FamilyDECnet;
    else
#endif
	connectionType = FamilyInternet;
    if (!HostnameToNetworkAddress (hostname, connectionType, connectionAddress))
	return FALSE;
    *displayNumber = number;
    *connectionTypep = connectionType;
    return TRUE;
}

#endif /* XDMCP */
