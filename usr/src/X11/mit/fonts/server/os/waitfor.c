/* $XConsortium: waitfor.c,v 1.8 91/09/11 11:59:39 rws Exp $ */
/*
 * waits for input
 */
/*
 * Copyright 1990, 1991 Network Computing Devices;
 * Portions Copyright 1987 by Digital Equipment Corporation and the
 * Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this protoype software
 * and its documentation to Members and Affiliates of the MIT X Consortium
 * any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of Network Computing Devices, Digital or
 * MIT not be used in advertising or publicity pertaining to distribution of
 * the software without specific, written prior permission.
 *
 * NETWORK COMPUTING DEVICES, DIGITAL AND MIT DISCLAIM ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS, IN NO EVENT SHALL NETWORK COMPUTING DEVICES, DIGITAL OR MIT BE
 * LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * $NCDId: @(#)waitfor.c,v 4.5 1991/06/24 11:59:20 lemke Exp $
 *
 */

#include	<stdio.h>
#include	<errno.h>
#include	<sys/param.h>

#include	<X11/Xos.h>	/* strings, time, etc */

#include	"clientstr.h"
#include	"globals.h"
#include	"osdep.h"

extern WorkQueuePtr workQueue;

extern int  errno;

extern void MakeNewConnections();
extern void FlushAllOutput();

extern long WellKnownConnections;
extern long LastSelectMask[];
extern long WriteMask[];
extern long ClientsWithInput[];
extern long ClientsWriteBlocked[];
extern long AllSockets[];
extern long AllClients[];
extern long OutputPending[];

extern Bool AnyClientsWriteBlocked;
extern Bool NewOutputPending;

extern int  ConnectionTranslation[];

long        LastReapTime;

/*
 * wait_for_something
 *
 * server suspends until
 * - data from clients
 * - new client connects
 * - room to write data to clients
 */

WaitForSomething(pClientsReady)
    int        *pClientsReady;
{
    struct timeval *wt,
                waittime;
    long        clientsReadable[mskcnt];
    long        clientsWriteable[mskcnt];
    long        curclient;
    int         selecterr;
    long        current_time = 0;
    long        timeout;
    int         nready,
                i;

    while (1) {
	/* handle the work Q */
	if (workQueue)
	    ProcessWorkQueue();

	if (ANYSET(ClientsWithInput)) {
	    COPYBITS(ClientsWithInput, clientsReadable);
	    break;
	}
	/*
	 * deal with KeepAlive timeouts.  if this seems to costly, SIGALRM
	 * could be used, but its more dangerous since some it could catch us
	 * at an inopportune moment (like inside un-reentrant malloc()).
	 */
	current_time = GetTimeInMillis();
	timeout = current_time - LastReapTime;
	if (timeout > ReapClientTime) {
	    ReapAnyOldClients();
	    LastReapTime = current_time;
	    timeout = ReapClientTime;
	}
	timeout = ReapClientTime - timeout;
	waittime.tv_sec = timeout / MILLI_PER_SECOND;
	waittime.tv_usec = (timeout % MILLI_PER_SECOND) *
	    (1000000 / MILLI_PER_SECOND);
	wt = &waittime;

	COPYBITS(AllSockets, LastSelectMask);

	BlockHandler((pointer) &wt, (pointer) LastSelectMask);
	if (NewOutputPending)
	    FlushAllOutput();

	if (AnyClientsWriteBlocked) {
	    COPYBITS(ClientsWriteBlocked, clientsWriteable);
	    i = select(MAXSOCKS, (int *) LastSelectMask,
		       (int *) clientsWriteable, (int *) NULL, wt);
	} else {
	    i = select(MAXSOCKS, (int *) LastSelectMask, (int *) NULL,
		       (int *) NULL, wt);
	}
	selecterr = errno;

	WakeupHandler((unsigned long) i, (pointer) LastSelectMask);
	if (i <= 0) {		/* error or timeout */
	    CLEARBITS(clientsWriteable);
	    if (i < 0) {
		if (selecterr == EBADF) {	/* somebody disconnected */
		    CheckConnections();
		} else if (selecterr != EINTR) {
		    ErrorF("WaitForSomething: select(): errno %d\n", selecterr);
		} else {
		    /*
		     * must have been broken by a signal.  go deal with any
		     * exception flags
		     */
		    return 0;
		}
	    } else {		/* must have timed out */
		ReapAnyOldClients();
		LastReapTime = GetTimeInMillis();
	    }
	} else {
	    if (AnyClientsWriteBlocked && ANYSET(clientsWriteable)) {
		NewOutputPending = TRUE;
		ORBITS(OutputPending, clientsWriteable, OutputPending);
		UNSETBITS(ClientsWriteBlocked, clientsWriteable);
		if (!ANYSET(ClientsWriteBlocked))
		    AnyClientsWriteBlocked = FALSE;
	    }
	    MASKANDSETBITS(clientsReadable, LastSelectMask, AllClients);
	    if (LastSelectMask[0] & WellKnownConnections)
		MakeNewConnections();
	    if (ANYSET(clientsReadable))
		break;

	}
    }
    nready = 0;

    if (ANYSET(clientsReadable)) {
	ClientPtr   client;
	int         conn;

	if (current_time)	/* may not have been set */
	    current_time = GetTimeInMillis();
	for (i = 0; i < mskcnt; i++) {
	    while (clientsReadable[i]) {
		curclient = ffs(clientsReadable[i]) - 1;
		conn = ConnectionTranslation[curclient + (i << 5)];
		clientsReadable[i] &= ~(1 << curclient);
		client = clients[conn];
		if (!client)
		    continue;
		pClientsReady[nready++] = conn;
		client->last_request_time = current_time;
		client->clientGone = CLIENT_ALIVE;
	    }
	}
    }
    return nready;
}

#ifndef ANYSET
/*
 * This is not always a macro
  */
ANYSET(src)
    long       *src;
{
    int         i;

    for (i = 0; i < mskcnt; i++)
	if (src[i])
	    return (1);
    return (0);
}

#endif
