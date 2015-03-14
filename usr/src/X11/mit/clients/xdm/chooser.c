/*
 * $XConsortium: chooser.c,v 1.10 91/08/25 10:49:47 keith Exp $
 *
 * Copyright 1990 Massachusetts Institute of Technology
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
 * Chooser - display a menu of names and let the user select one
 */

/*
 * Layout:
 *
 *  +--------------------------------------------------+
 *  |             +------------------+                 |
 *  |             |      Label       |                 |
 *  |             +------------------+                 |
 *  |    +-+--------------+                            |
 *  |    |^| name-1       |                            |
 *  |    ||| name-2       |                            |
 *  |    |v| name-3       |                            |
 *  |    | | name-4       |                            |
 *  |    | | name-5       |                            |
 *  |    | | name-6       |                            |
 *  |    +----------------+                            |
 *  |    cancel  accept  ping                          |
 *  +--------------------------------------------------+
 */

#include    <X11/Intrinsic.h>
#include    <X11/StringDefs.h>
#include    <X11/Xatom.h>

#include    <X11/Xaw/Paned.h>
#include    <X11/Xaw/Label.h>
#include    <X11/Xaw/Viewport.h>
#include    <X11/Xaw/List.h>
#include    <X11/Xaw/Box.h>
#include    <X11/Xaw/Command.h>

#include    "dm.h"

#include    <X11/Xdmcp.h>

#include    <sys/types.h>
#include    <stdio.h>
#include    <ctype.h>

#ifdef SVR4
#include    <sys/sockio.h>
#endif
#include    <sys/socket.h>
#include    <netinet/in.h>
#include    <sys/ioctl.h>

#define BROADCAST_HOSTNAME  "BROADCAST"

#ifdef hpux
# include <sys/utsname.h>
# ifdef HAS_IFREQ
#  include <net/if.h>
# endif
#else
#ifdef __convex__
# include <sync/queue.h>
# include <sync/sema.h>
#endif
# include <net/if.h>
#endif /* hpux */

#include    <netdb.h>

Widget	    toplevel, label, viewport, paned, list, box, cancel, acceptit, ping;

static void	CvtStringToARRAY8();

static struct _app_resources {
    ARRAY8Ptr   xdmAddress;
    ARRAY8Ptr	clientAddress;
    int		connectionType;
} app_resources;

#define offset(field) XtOffsetOf(struct _app_resources, field)

#define XtRARRAY8   "ARRAY8"

static XtResource  resources[] = {
    {"xdmAddress",	"XdmAddress",  XtRARRAY8,	sizeof (ARRAY8Ptr),
	offset (xdmAddress),	    XtRString,	NULL },
    {"clientAddress",	"ClientAddress",  XtRARRAY8,	sizeof (ARRAY8Ptr),
	offset (clientAddress),	    XtRString,	NULL },
    {"connectionType",	"ConnectionType",   XtRInt,	sizeof (int),
	offset (connectionType),    XtRImmediate,	(XtPointer) 0 }
};
#undef offset

static XrmOptionDescRec options[] = {
    "-xdmaddress",	"*xdmAddress",	    XrmoptionSepArg,	NULL,
    "-clientaddress",	"*clientAddress",   XrmoptionSepArg,	NULL,
    "-connectionType",	"*connectionType",  XrmoptionSepArg,	NULL,
};

typedef struct _hostAddr {
    struct _hostAddr	*next;
    struct sockaddr	*addr;
    int			addrlen;
    xdmOpCode		type;
} HostAddr;

static HostAddr    *hostAddrdb;

typedef struct _hostName {
    struct _hostName	*next;
    char		*fullname;
    int			willing;
    ARRAY8		hostname, status;
    CARD16		connectionType;
    ARRAY8		hostaddr;
} HostName;

static HostName    *hostNamedb;

static int  socketFD;

static int  pingTry;

#define PING_INTERVAL	2000
#define TRIES		3

static XdmcpBuffer	directBuffer, broadcastBuffer;
static XdmcpBuffer	buffer;

/* ARGSUSED */
static void
PingHosts (closure, id)
    XtPointer closure;
    XtIntervalId *id;
{
    HostAddr	*hosts;

    for (hosts = hostAddrdb; hosts; hosts = hosts->next)
    {
	if (hosts->type == QUERY)
	    XdmcpFlush (socketFD, &directBuffer, hosts->addr, hosts->addrlen);
	else
	    XdmcpFlush (socketFD, &broadcastBuffer, hosts->addr, hosts->addrlen);
    }
    if (++pingTry < TRIES)
	XtAddTimeOut (PING_INTERVAL, PingHosts, (XtPointer) 0);
}

char	**NameTable;
int	NameTableSize;

static int
HostnameCompare (a, b)
#if __STDC__
    const void *a, *b;
#else
    char *a, *b;
#endif
{
    return strcmp (*(char **)a, *(char **)b);
}

static void
RebuildTable (size)
{
    char	**newTable = 0;
    HostName	*names;
    int		i;

    if (size)
    {
	newTable = (char **) malloc (size * sizeof (char *));
	if (!newTable)
	    return;
	for (names = hostNamedb, i = 0; names; names = names->next, i++)
	    newTable[i] = names->fullname;
	qsort (newTable, size, sizeof (char *), HostnameCompare);
    }
    XawListChange (list, newTable, size, 0, TRUE);
    if (NameTable)
	free ((char *) NameTable);
    NameTable = newTable;
    NameTableSize = size;
}

static int
AddHostname (hostname, status, addr, willing)
    ARRAY8Ptr	    hostname, status;
    struct sockaddr *addr;
    int		    willing;
{
    HostName	*new, **names, *name;
    ARRAY8	hostAddr;
    CARD16	connectionType;
    int		fulllen;

    switch (addr->sa_family)
    {
    case AF_INET:
	hostAddr.data = (CARD8 *) &((struct sockaddr_in *) addr)->sin_addr;
	hostAddr.length = 4;
	connectionType = FamilyInternet;
	break;
    default:
	hostAddr.data = (CARD8 *) "";
	hostAddr.length = 0;
	connectionType = FamilyLocal;
	break;
    }
    for (names = &hostNamedb; *names; names = & (*names)->next)
    {
	name = *names;
	if (connectionType == name->connectionType &&
	    XdmcpARRAY8Equal (&hostAddr, &name->hostaddr))
	{
	    if (XdmcpARRAY8Equal (status, &name->status))
	    {
		return 0;
	    }
	    break;
	}
    }
    if (!*names)
    {
	new = (HostName *) malloc (sizeof (HostName));
    	if (!new)
	    return 0;
	if (hostname->length)
	{
	    switch (addr->sa_family)
	    {
	    case AF_INET:
	    	{
	    	    struct hostent  *hostent;
		    char	    *host;
    	
	    	    hostent = gethostbyaddr (hostAddr.data, hostAddr.length, AF_INET);
	    	    if (hostent)
	    	    {
			XdmcpDisposeARRAY8 (hostname);
		    	host = hostent->h_name;
			XdmcpAllocARRAY8 (hostname, strlen (host));
			bcopy (host, hostname->data, hostname->length);
	    	    }
	    	}
	    }
	}
    	if (!XdmcpAllocARRAY8 (&new->hostaddr, hostAddr.length))
    	{
	    free ((char *) new->fullname);
	    free ((char *) new);
	    return 0;
    	}
    	bcopy (hostAddr.data, new->hostaddr.data, hostAddr.length);
	new->connectionType = connectionType;
	new->hostname = *hostname;

    	*names = new;
    	new->next = 0;
	NameTableSize++;
    }
    else
    {
	new = *names;
	free (new->fullname);
	XdmcpDisposeARRAY8 (&new->status);
	XdmcpDisposeARRAY8 (hostname);
    }
    new->willing = willing;
    new->status = *status;

    hostname = &new->hostname;
    fulllen = hostname->length;
    if (fulllen < 30)
	fulllen = 30;
    new->fullname = malloc (fulllen + status->length + 10);
    if (!new->fullname)
    {
	new->fullname = "Unknown";
    }
    else
    {
	sprintf (new->fullname, "%-30.*s %*.*s",
		 hostname->length, hostname->data,
		 status->length, status->length, status->data);
    }
    RebuildTable (NameTableSize);
    return 1;
}

static
DisposeHostname (host)
    HostName	*host;
{
    XdmcpDisposeARRAY8 (&host->hostname);
    XdmcpDisposeARRAY8 (&host->hostaddr);
    XdmcpDisposeARRAY8 (&host->status);
    free ((char *) host->fullname);
    free ((char *) host);
}

static
RemoveHostname (host)
    HostName	*host;
{
    HostName	**prev, *hosts;

    prev = &hostNamedb;;
    for (hosts = hostNamedb; hosts; hosts = hosts->next)
    {
	if (hosts == host)
	    break;
	prev = &hosts->next;
    }
    if (!hosts)
	return;
    *prev = host->next;
    DisposeHostname (host);
    NameTableSize--;
    RebuildTable (NameTableSize);
}

static
EmptyHostnames ()
{
    HostName	*hosts, *next;

    for (hosts = hostNamedb; hosts; hosts = next)
    {
	next = hosts->next;
	DisposeHostname (hosts);
    }
    NameTableSize = 0;
    hostNamedb = 0;
    RebuildTable (NameTableSize);
}

/* ARGSUSED */
static void
ReceivePacket (closure, source, id)
    XtPointer	closure;
    int		*source;
    XtInputId	*id;
{
    XdmcpHeader	    header;
    ARRAY8	    authenticationName;
    ARRAY8	    hostname;
    ARRAY8	    status;
    int		    saveHostname = 0;
    struct sockaddr addr;
    int		    addrlen;

    addrlen = sizeof (addr);
    if (!XdmcpFill (socketFD, &buffer, &addr, &addrlen))
	return;
    if (!XdmcpReadHeader (&buffer, &header))
	return;
    if (header.version != XDM_PROTOCOL_VERSION)
	return;
    hostname.data = 0;
    status.data = 0;
    authenticationName.data = 0;
    switch (header.opcode) {
    case WILLING:
    	if (XdmcpReadARRAY8 (&buffer, &authenticationName) &&
	    XdmcpReadARRAY8 (&buffer, &hostname) &&
	    XdmcpReadARRAY8 (&buffer, &status))
    	{
	    if (header.length == 6 + authenticationName.length +
	    	hostname.length + status.length)
	    {
		if (AddHostname (&hostname, &status, &addr, header.opcode == (int) WILLING))
		    saveHostname = 1;
	    }
    	}
	XdmcpDisposeARRAY8 (&authenticationName);
	break;
    case UNWILLING:
    	if (XdmcpReadARRAY8 (&buffer, &hostname) &&
	    XdmcpReadARRAY8 (&buffer, &status))
    	{
	    if (header.length == 4 + hostname.length + status.length)
	    {
		if (AddHostname (&hostname, &status, &addr, header.opcode == (int) WILLING))
		    saveHostname = 1;

	    }
    	}
	break;
    default:
	break;
    }
    if (!saveHostname)
    {
    	XdmcpDisposeARRAY8 (&hostname);
    	XdmcpDisposeARRAY8 (&status);
    }
}

RegisterHostaddr (addr, len, type)
    struct sockaddr *addr;
    int		    len;
    xdmOpCode	    type;
{
    HostAddr		*host, **prev;

    host = (HostAddr *) malloc (sizeof (HostAddr));
    if (!host)
	return;
    host->addr = (struct sockaddr *) malloc (len);
    if (!host->addr)
    {
	free ((char *) host);
	return;
    }
    bcopy ((char *) addr, (char *) host->addr, len);
    host->addrlen = len;
    host->type = type;
    for (prev = &hostAddrdb; *prev; prev = &(*prev)->next)
	;
    *prev = host;
    host->next = NULL;
}

/*
 * Register the address for this host.
 * Called with each of the names on the command line.
 * The special name "BROADCAST" looks up all the broadcast
 *  addresses on the local host.
 */

RegisterHostname (name)
    char    *name;
{
    struct hostent	*hostent;
    struct sockaddr_in	in_addr;
    struct ifconf	ifc;
    register struct ifreq *ifr;
    struct sockaddr	broad_addr;
    char		buf[2048];
    int			n;

    if (!strcmp (name, BROADCAST_HOSTNAME))
    {
	ifc.ifc_len = sizeof (buf);
	ifc.ifc_buf = buf;
	if (ioctl (socketFD, (int) SIOCGIFCONF, (char *) &ifc) < 0)
	    return;
	for (ifr = ifc.ifc_req, n = ifc.ifc_len / sizeof (struct ifreq); --n >= 0;
	    ifr++)
	{
	    if (ifr->ifr_addr.sa_family != AF_INET)
		continue;

	    broad_addr = ifr->ifr_addr;
	    ((struct sockaddr_in *) &broad_addr)->sin_addr.s_addr =
		htonl (INADDR_BROADCAST);
#ifdef SIOCGIFBRDADDR
	    {
		struct ifreq    broad_req;
    
		broad_req = *ifr;
		if (ioctl (socketFD, SIOCGIFFLAGS, (char *) &broad_req) != -1 &&
		    (broad_req.ifr_flags & IFF_BROADCAST) &&
		    (broad_req.ifr_flags & IFF_UP)
		    )
		{
		    broad_req = *ifr;
		    if (ioctl (socketFD, SIOCGIFBRDADDR, &broad_req) != -1)
			broad_addr = broad_req.ifr_addr;
		    else
			continue;
		}
		else
		    continue;
	    }
#endif
	    in_addr = *((struct sockaddr_in *) &broad_addr);
	    in_addr.sin_port = htons (XDM_UDP_PORT);
	    RegisterHostaddr ((struct sockaddr *)&in_addr, sizeof (in_addr),
			      BROADCAST_QUERY);
	}
    }
    else
    {
#ifndef ishexdigit
#define ishexdigit(c)	(isdigit(c) || 'a' <= (c) && (c) <= 'f')
#endif

	if (isascii (name[0]) && ishexdigit (name[0]))
	{
	    if (!index (name, '.'))
		FromHex (name, &in_addr.sin_addr.s_addr, strlen (name));
	    else
		in_addr.sin_addr.s_addr = inet_addr (name);
	    if (in_addr.sin_addr.s_addr == -1)
		return;
	    in_addr.sin_family = AF_INET;
	}
	else
	{
	    hostent = gethostbyname (name);
	    if (!hostent)
		return;
	    if (hostent->h_addrtype != AF_INET || hostent->h_length != 4)
	    	return;
	    in_addr.sin_family = hostent->h_addrtype;
	    bcopy (hostent->h_addr, &in_addr.sin_addr, 4);
	}
	in_addr.sin_port = htons (XDM_UDP_PORT);
	RegisterHostaddr ((struct sockaddr *)&in_addr, sizeof (in_addr),
			  QUERY);
    }
}

static ARRAYofARRAY8	AuthenticationNames;

RegisterAuthenticationName (name, namelen)
    char    *name;
    int	    namelen;
{
    ARRAY8Ptr	authName;
    if (!XdmcpReallocARRAYofARRAY8 (&AuthenticationNames,
				    AuthenticationNames.length + 1))
	return;
    authName = &AuthenticationNames.data[AuthenticationNames.length-1];
    if (!XdmcpAllocARRAY8 (authName, namelen))
	return;
    bcopy (name, authName->data, namelen);
}

InitXDMCP (argv)
    char    **argv;
{
    int	soopts = 1;
    XdmcpHeader	header;
    int	i;
    int optlen;

    header.version = XDM_PROTOCOL_VERSION;
    header.opcode = (CARD16) BROADCAST_QUERY;
    header.length = 1;
    for (i = 0; i < (int)AuthenticationNames.length; i++)
	header.length += 2 + AuthenticationNames.data[i].length;
    XdmcpWriteHeader (&broadcastBuffer, &header);
    XdmcpWriteARRAYofARRAY8 (&broadcastBuffer, &AuthenticationNames);

    header.version = XDM_PROTOCOL_VERSION;
    header.opcode = (CARD16) QUERY;
    header.length = 1;
    for (i = 0; i < (int)AuthenticationNames.length; i++)
	header.length += 2 + AuthenticationNames.data[i].length;
    XdmcpWriteHeader (&directBuffer, &header);
    XdmcpWriteARRAYofARRAY8 (&directBuffer, &AuthenticationNames);
    if ((socketFD = socket (AF_INET, SOCK_DGRAM, 0)) < 0)
	return 0;
#ifdef SO_BROADCAST
    soopts = 1;
    if (setsockopt (socketFD, SOL_SOCKET, SO_BROADCAST, &soopts, sizeof (soopts)) < 0)
	perror ("setsockopt");
#endif
    
    XtAddInput (socketFD, (XtPointer) XtInputReadMask, ReceivePacket,
		(XtPointer) 0);
    while (*argv)
    {
	RegisterHostname (*argv);
	++argv;
    }
    pingTry = 0;
    PingHosts ((XtPointer)NULL, (XtIntervalId *)NULL);
    return 1;
}

Boolean
Choose (h)
    HostName	*h;
{
    if (app_resources.xdmAddress)
    {
	struct sockaddr_in  in_addr;
	struct sockaddr	*addr;
	int		family;
	int		len;
	int		fd;
	char		buf[1024];
	XdmcpBuffer	buffer;
	char		*xdm;

	xdm = (char *) app_resources.xdmAddress->data;
	family = (xdm[0] << 8) + xdm[1];
	switch (family) {
	case AF_INET:
	    in_addr.sin_family = family;
	    bcopy (xdm + 2, &in_addr.sin_port, 2);
	    bcopy (xdm + 4, &in_addr.sin_addr.s_addr, 4);
	    addr = (struct sockaddr *) &in_addr;
	    len = sizeof (in_addr);
	    break;
	}
	if ((fd = socket (family, SOCK_STREAM, 0)) == -1)
	{
	    fprintf (stderr, "Cannot create response socket\n");
	    exit (REMANAGE_DISPLAY);
	}
	if (connect (fd, addr, len) == -1)
	{
	    fprintf (stderr, "Cannot connect to xdm\n");
	    exit (REMANAGE_DISPLAY);
	}
	buffer.data = (BYTE *) buf;
	buffer.size = sizeof (buf);
	buffer.pointer = 0;
	buffer.count = 0;
	XdmcpWriteARRAY8 (&buffer, app_resources.clientAddress);
	XdmcpWriteCARD16 (&buffer, (CARD16) app_resources.connectionType);
	XdmcpWriteARRAY8 (&buffer, &h->hostaddr);
	write (fd, buffer.data, buffer.pointer);
	close (fd);
    }
    else
    {
	int i;

    	printf ("%u\n", h->connectionType);
    	for (i = 0; i < (int)h->hostaddr.length; i++)
	    printf ("%u%s", h->hostaddr.data[i],
		    i == h->hostaddr.length - 1 ? "\n" : " ");
    }
}

/* ARGSUSED */
static void
DoAccept (w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    XawListReturnStruct	*r;
    HostName		*h;

    r = XawListShowCurrent (list);
    if (r->list_index == XAW_LIST_NONE)
	XBell (XtDisplay (toplevel), 0);
    else
    {
	for (h = hostNamedb; h; h = h->next)
	    if (!strcmp (r->string, h->fullname))
	    {
		Choose (h);
	    }
	exit (OBEYSESS_DISPLAY);
    }
}

/* ARGSUSED */
static void
DoCheckWilling (w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    XawListReturnStruct	*r;
    HostName		*h;
    
    r = XawListShowCurrent (list);
    if (r->list_index == XAW_LIST_NONE)
	return;
    for (h = hostNamedb; h; h = h->next)
	if (!strcmp (r->string, h->fullname))
	    if (!h->willing)
		XawListUnhighlight (list);
}

/* ARGSUSED */
static void
DoCancel (w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    exit (OBEYSESS_DISPLAY);
}

/* ARGSUSED */
static void
DoPing (w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    EmptyHostnames ();
    pingTry = 0;
    PingHosts ((XtPointer)NULL, (XtIntervalId *)NULL);
}

static XtActionsRec app_actions[] = {
    "Accept",	    DoAccept,
    "Cancel",	    DoCancel,
    "CheckWilling", DoCheckWilling,
    "Ping",	    DoPing,
};

main (argc, argv)
    char    **argv;
{
    Arg		position[3];
    Dimension   width, height;
    Position	x, y;

    toplevel = XtInitialize (argv[0], "Chooser", options, XtNumber(options), &argc, argv);

    XtAddConverter(XtRString, XtRARRAY8, CvtStringToARRAY8, NULL, 0);

    XtGetApplicationResources (toplevel, (XtPointer) &app_resources, resources,
			       XtNumber (resources), NULL, (Cardinal) 0);

    XtAddActions (app_actions, XtNumber (app_actions));
    paned = XtCreateManagedWidget ("paned", panedWidgetClass, toplevel, NULL, 0);
    label = XtCreateManagedWidget ("label", labelWidgetClass, paned, NULL, 0);
    viewport = XtCreateManagedWidget ("viewport", viewportWidgetClass, paned, NULL, 0);
    list = XtCreateManagedWidget ("list", listWidgetClass, viewport, NULL, 0);
    box = XtCreateManagedWidget ("box", boxWidgetClass, paned, NULL, 0);
    cancel = XtCreateManagedWidget ("cancel", commandWidgetClass, box, NULL, 0);
    acceptit = XtCreateManagedWidget ("accept", commandWidgetClass, box, NULL, 0);
    ping = XtCreateManagedWidget ("ping", commandWidgetClass, box, NULL, 0);

    /*
     * center ourselves on the screen
     */
    XtSetMappedWhenManaged(toplevel, FALSE);
    XtRealizeWidget (toplevel);

    XtSetArg (position[0], XtNwidth, &width);
    XtSetArg (position[1], XtNheight, &height);
    XtGetValues (toplevel, position, (Cardinal) 2);
    x = (Position)(WidthOfScreen (XtScreen (toplevel)) - width) / 2;
    y = (Position)(HeightOfScreen (XtScreen (toplevel)) - height) / 3;
    XtSetArg (position[0], XtNx, x);
    XtSetArg (position[1], XtNy, y);
    XtSetValues (toplevel, position, (Cardinal) 2);

    /*
     * Run
     */
    XtMapWidget(toplevel);
    InitXDMCP (argv + 1);
    XtMainLoop ();
    exit(0);
    /*NOTREACHED*/
}

FromHex (s, d, len)
    char    *s, *d;
    int	    len;
{
    int	t;
    while (len >= 2)
    {
#define HexChar(c)  ('0' <= (c) && (c) <= '9' ? (c) - '0' : (c) - 'a' + 10)
	t = HexChar (*s) << 4;
	s++;
	t += HexChar (*s);
	s++;
	*d++ = t;
	len -= 2;
    }
}

/*ARGSUSED*/
static void
CvtStringToARRAY8 (args, num_args, fromVal, toVal)
    XrmValuePtr	args;
    Cardinal	*num_args;
    XrmValuePtr	fromVal;
    XrmValuePtr	toVal;
{
    static ARRAY8Ptr	dest;
    char	*s;
    int		len;

    dest = (ARRAY8Ptr) XtMalloc (sizeof (ARRAY8));
    len = fromVal->size;
    s = (char *) fromVal->addr;
    if (!XdmcpAllocARRAY8 (dest, len >> 1))
	XtStringConversionWarning ((char *) fromVal->addr, XtRARRAY8);
    else
    {
	FromHex (s, (char *) dest->data, len);
    }
    toVal->addr = (caddr_t) &dest;
    toVal->size = sizeof (ARRAY8Ptr);
}
