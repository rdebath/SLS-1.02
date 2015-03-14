/*
 * $XConsortium: protodpy.c,v 1.4 89/12/13 15:23:33 keith Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

/*
 * protodpy.c
 *
 * manage a collection of proto-displays.  These are displays for
 * which sessionID's have been generated, but no session has been
 * started.
 */

#include "dm.h"

struct protoDisplay	*protoDisplays;

extern void	free();

addressEqual (a1, len1, a2, len2)
struct sockaddr	*a1, *a2;
int		len1, len2;
{
    char    *c1, *c2;

    if (len1 != len2)
	return FALSE;
    c1 = (char *) a1;
    c2 = (char *) a2;
    while (len1--)
	if (*c1++ != *c2++)
	    return FALSE;
    return TRUE;
}

struct protoDisplay *
FindProtoDisplay (address, addrlen, displayNumber)
    struct sockaddr *address;
    int		    addrlen;
    CARD16	    displayNumber;
{
    struct protoDisplay	*pdpy;

    for (pdpy = protoDisplays; pdpy; pdpy=pdpy->next)
	if (pdpy->displayNumber == displayNumber &&
	    addressEqual (address, addrlen, pdpy->address, pdpy->addrlen))
	{
	    return pdpy;
	}
    return (struct protoDisplay *) 0;
}

TimeoutProtoDisplays (now)
    long    now;
{
    struct protoDisplay	*pdpy, *next;

    for (pdpy = protoDisplays; pdpy; pdpy = next)
    {
	next = pdpy->next;
	if (pdpy->date < now - PROTO_TIMEOUT)
	    DisposeProtoDisplay (pdpy);
    }
}

struct protoDisplay *
NewProtoDisplay (address, addrlen, displayNumber,
		 connectionType, connectionAddress, sessionID)
    struct sockaddr *address;
    int		    addrlen;
    CARD16	    displayNumber;
    CARD16	    connectionType;
    ARRAY8Ptr	    connectionAddress;
    CARD32	    sessionID;
{
    struct protoDisplay	*pdpy;
    long    date;

    time (&date);
    TimeoutProtoDisplays (date);
    pdpy = (struct protoDisplay *) malloc (sizeof *pdpy);
    if (!pdpy)
	return NULL;
    pdpy->address = (struct sockaddr *) malloc (addrlen);
    if (!pdpy->address)
    {
	free ((char *) pdpy);
	return NULL;
    }
    pdpy->addrlen = addrlen;
    bcopy (address, pdpy->address, addrlen);
    pdpy->displayNumber = displayNumber;
    pdpy->connectionType = connectionType;
    pdpy->date = date;
    if (!XdmcpCopyARRAY8 (connectionAddress, &pdpy->connectionAddress))
    {
	free ((char *) pdpy->address);
	free ((char *) pdpy);
	return NULL;
    }
    pdpy->sessionID = sessionID;
    pdpy->fileAuthorization = (Xauth *) NULL;
    pdpy->xdmcpAuthorization = (Xauth *) NULL;
    pdpy->next = protoDisplays;
    protoDisplays = pdpy;
    return pdpy;
}

DisposeProtoDisplay (pdpy)
    struct protoDisplay	*pdpy;
{
    struct protoDisplay	*p, *prev;

    prev = 0;
    for (p = protoDisplays; p; p=p->next)
    {
	if (p == pdpy)
	    break;
	prev = p;
    }
    if (!p)
	return;
    if (prev)
	prev->next = pdpy->next;
    else
	protoDisplays = pdpy->next;
    XdmcpDisposeARRAY8 (&pdpy->connectionAddress);
    if (pdpy->fileAuthorization)
	XauDisposeAuth (pdpy->fileAuthorization);
    if (pdpy->xdmcpAuthorization)
	XauDisposeAuth (pdpy->xdmcpAuthorization);
    free ((char *) pdpy->address);
    free ((char *) pdpy);
}
