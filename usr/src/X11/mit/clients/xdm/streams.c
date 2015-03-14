/*
 * xdm - display manager daemon
 *
 * $XConsortium: streams.c,v 1.2 91/08/25 10:50:29 keith Exp $
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
 * streams.c - Support for STREAMS
 */

#include "dm.h"

#ifdef XDMCP
#ifdef STREAMSCONN

#include <fcntl.h>
#include <tiuser.h>
#include <netconfig.h>
#include <netdir.h>

extern int	xdmcpFd;
extern int	chooserFd;

extern FD_TYPE	WellKnownSocketsMask;
extern int	WellKnownSocketsMax;

CreateWellKnownSockets ()
{
    struct t_bind bind_addr;
    struct netconfig *nconf;
    struct nd_hostserv service;
    struct nd_addrlist *servaddrs;
    char *name, *localHostname();
    char bindbuf[15];
    int it;

    if (request_port == 0)
	return;
    Debug ("creating UDP stream %d\n", request_port);

    nconf = getnetconfigent("udp");
    if (!nconf) {
	t_error("getnetconfigent udp");
	return;
    }

    xdmcpFd = t_open(nconf->nc_device, O_RDWR, NULL);
    if (xdmcpFd == -1) {
	LogError ("XDMCP stream creation failed\n");
	t_error ("t_open");
	return;
    }
    name = localHostname ();
    registerHostname (name, strlen (name));
    RegisterCloseOnFork (xdmcpFd);

    service.h_host = HOST_SELF;
    sprintf(bindbuf, "%d", request_port);
    service.h_serv = bindbuf;
    netdir_getbyname(nconf, &service, &servaddrs);
    freenetconfigent(nconf);

    bind_addr.qlen = 5;
    bind_addr.addr.buf = servaddrs->n_addrs[0].buf;
    bind_addr.addr.len = servaddrs->n_addrs[0].len;
    it = t_bind(xdmcpFd, &bind_addr, &bind_addr);
    netdir_free(servaddrs, ND_ADDRLIST);
    if (it < 0)
    {
	LogError ("error binding STREAMS address %d\n", request_port);
	t_error("t_bind");	/* also goes to log file */
	t_close (xdmcpFd);
	xdmcpFd = -1;
	return;
    }
    WellKnownSocketsMax = xdmcpFd;
    FD_SET (xdmcpFd, &WellKnownSocketsMask);

    chooserFd = t_open ("/dev/tcp", O_RDWR, NULL);
    Debug ("Created chooser fd %d\n", chooserFd);
    if (chooserFd == -1)
    {
	LogError ("chooser stream creation failed\n");
	t_error("t_open chooser");
	return;
    }
    if (chooserFd > WellKnownSocketsMax)
	WellKnownSocketsMax = chooserFd;
    FD_SET (chooserFd, &WellKnownSocketsMask);
}

GetChooserAddr (addr, lenp)
    char	*addr;		/* return */
    int		*lenp;		/* size of addr, returned as amt used */
{
    struct netbuf nbuf;
    int retval;

    nbuf.buf = addr;
    nbuf.maxlen = *lenp;
    retval = t_getname (chooserFd, nbuf, LOCALNAME);
    if (retval < 0) {
	if (debugLevel > 0)
	    t_error("t_getname on chooser fd");
    }
    *lenp = nbuf.len;
    return retval;
}

#endif /* STREAMSCONN */
#endif /* XDMCP */
