/* $XConsortium: events.c,v 1.3 91/05/13 16:55:02 gildea Exp $ */
/*
 * event handling stuff
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
 * @(#)events.c	4.4	5/7/91
 *
 */

#include	"clientstr.h"
#include	"FSproto.h"
#include	"globals.h"
#include	"events.h"

extern void (*EventSwapVector[NUM_EVENT_VECTORS]) ();

static Mask lastEventMask = FontChangeNotifyMask;

#define	AllEventMasks	(lastEventMask | (lastEventMask - 1))

void
WriteErrorToClient(client, error)
    ClientPtr   client;
    fsError    *error;
{
    if (client->swapped) {
	fsError     errorTo;

	SErrorEvent(error, &errorTo);
	(void) WriteToClient(client, sizeof(fsError), (char *) &errorTo);
    } else {
	(void) WriteToClient(client, sizeof(fsError),
			     (char *) error);
    }
}

int
ProcSetEventMask(client)
    ClientPtr   client;
{
    REQUEST(fsSetEventMaskReq);
    REQUEST_AT_LEAST_SIZE(fsSetEventMaskReq);

    if (stuff->event_mask & ~AllEventMasks) {
	SendErrToClient(client, FSBadEventMask, (pointer) stuff->event_mask);
	return FSBadEventMask;
    }
    client->eventmask = stuff->event_mask;
    return client->noClientException;
}

int
ProcGetEventMask(client)
    ClientPtr   client;
{
    fsGetEventMaskReply rep;

    REQUEST(fsGetEventMaskReq);
    REQUEST_AT_LEAST_SIZE(fsGetEventMaskReq);

    rep.type = FS_Reply;
    rep.sequenceNumber = client->sequence;
    rep.length = sizeof(fsGetEventMaskReply) >> 2;
    rep.event_mask = client->eventmask;

    return client->noClientException;
}

void
SendKeepAliveEvent(client)
    ClientPtr   client;
{
    fsKeepAliveEvent ev;

    ev.type = FS_Event;
    ev.event_code = KeepAlive;
    ev.sequenceNumber = client->sequence;
    ev.length = sizeof(fsKeepAliveEvent) >> 2;
    ev.timestamp = GetTimeInMillis();

#ifdef DEBUG
    fprintf(stderr, "client #%d is getting a KeepAlive\n", client->index);
#endif

    if (client->swapped) {
	fsKeepAliveEvent evTo;

	SErrorEvent((fsError *) & ev, (fsError *) & evTo);
	(void) WriteToClient(client, sizeof(fsKeepAliveEvent), (char *) &evTo);
    } else {
	(void) WriteToClient(client, sizeof(fsKeepAliveEvent), (char *) &ev);
    }
}
