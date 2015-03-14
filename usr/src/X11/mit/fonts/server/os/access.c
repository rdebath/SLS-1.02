/* $XConsortium: access.c,v 1.6 92/06/01 17:07:58 gildea Exp $ */
/*
 * Copyright 1990, 1991 Network Computing Devices;
 * Portions Copyright 1987 by Digital Equipment Corporation and the
 * Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of Network Computing Devices, Digital or
 * M.I.T. not be used in advertising or publicity pertaining to distribution
 * of the software without specific, written prior permission.
 *
 * NETWORK COMPUTING DEVICES, DIGITAL AND M.I.T. DISCLAIM ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL NETWORK COMPUTING DEVICES,
 * DIGITAL OR M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
 * THIS SOFTWARE.
 */

#include        <sys/param.h>
#include	<sys/types.h>
#include	<sys/socket.h>
#include	<netdb.h>
#include	<netinet/in.h>
#include	"clientstr.h"
#include	"misc.h"
#include	"site.h"
#include	"accstr.h"
#include	"osdep.h"

long        MaxClients = DEFAULT_CLIENT_LIMIT;

void
AccessSetConnectionLimit(num)
    int         num;
{
    num++;	/* take serverClient into account */
    if (num > MAXSOCKS) {
	ErrorF("Client limit of %d too high; using default of %d\n",
	       num, DEFAULT_CLIENT_LIMIT);
	return;
    }
    MaxClients = num;
}

/*
 * XXX
 *
 * needs massive amounts of OS-dependent work (big surprise)
 */
int
GetHostAddress(addr)
    HostAddress *addr;
{
    char        hname[64];
    struct hostent *hp;

    addr->addr_len = sizeof(struct in_addr);
    addr->address = (pointer) fsalloc(addr->addr_len);
    if (!addr->address)
	return FSBadAlloc;
    addr->type = HOST_AF_INET;
    gethostname(hname, sizeof(hname));
    hp = gethostbyname(hname);
    if (hp) {
	bcopy((char *) hp->h_addr, (char *) addr->address, addr->addr_len);
    } else {
	fsfree((char *) addr->address);
	return FSBadName;
    }
    return FSSuccess;
}

/* ARGSUSED */
int
CheckClientAuthorization(client, client_auth, accept, index, size, auth_data)
    ClientPtr   client;
    AuthPtr     client_auth;
    int        *accept;
    int        *index;
    int        *size;
    char      **auth_data;
{
    OsCommPtr	oc;

    /* now that it's connected, zero the connect time
       so it doesn't get killed */
    oc = (OsCommPtr)client->osPrivate;
    oc->conn_time = 0;

    *size = 0;
    *accept = AuthSuccess;
    *index = 0;			/* we support no authorization protocols */
    return FSSuccess;
}
