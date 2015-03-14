/* $XConsortium: xsendexev.c,v 1.7 90/05/18 11:32:30 rws Exp $ */

/************************************************************
Copyright (c) 1989 by Hewlett-Packard Company, Palo Alto, California, and the 
Massachusetts Institute of Technology, Cambridge, Massachusetts.

			All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the names of Hewlett-Packard or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

HEWLETT-PACKARD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
HEWLETT-PACKARD BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

********************************************************/

/***********************************************************************
 *
 * Request to send an extension event.
 *
 */

#define EXTENSION_EVENT_BASE  64
#define	 NEED_EVENTS
#define	 NEED_REPLIES
#include "X.h"				/* for inputstr.h    */
#include "Xproto.h"			/* Request macro     */
#include "inputstr.h"			/* DeviceIntPtr	     */
#include "windowstr.h"			/* Window      	     */
#include "XI.h"
#include "XIproto.h"

extern	int 		IReqCode;
extern	int 		BadDevice;
extern	void		(* ReplySwapVector[256]) ();
extern	void		(* EventSwapVector[128]) ();
DeviceIntPtr		LookupDeviceIntRec();

/***********************************************************************
 *
 * Handle requests from clients with a different byte order than us.
 *
 */

int
SProcXSendExtensionEvent(client)
    register ClientPtr client;
    {
    register char n;
    register long *p;
    register int i;
    xEvent eventT;
    xEvent *eventP;
    void (*proc)(), NotImplemented();

    REQUEST(xSendExtensionEventReq);
    swaps(&stuff->length, n);
    swapl(&stuff->destination, n);
    swaps(&stuff->count, n);
    eventP = (xEvent *) &stuff[1];
    for (i=0; i<stuff->num_events; i++,eventP++)
        {
	proc = EventSwapVector[eventP->u.u.type & 0177];
 	if (proc == NotImplemented)   /* no swapping proc; invalid event type? */
	    return (BadValue);
	(*proc)(eventP, &eventT);
	*eventP = eventT;
	}

    p = (long *) (((xEvent *) &stuff[1]) + stuff->num_events);
    for (i=0; i<stuff->count; i++)
        {
        swapl(p, n);
	p++;
        }
    return(ProcXSendExtensionEvent(client));
    }

/***********************************************************************
 *
 * Send an event to some client, as if it had come from an extension input 
 * device.
 *
 */

ProcXSendExtensionEvent (client)
    register ClientPtr client;
    {
    int			ret;
    extern int 		lastEvent; 		/* Defined in extension.c */
    DeviceIntPtr	dev;
    xEvent		*first;
    XEventClass		*list;
    struct tmask	tmp[EMASKSIZE];

    REQUEST(xSendExtensionEventReq);
    REQUEST_AT_LEAST_SIZE(xSendExtensionEventReq);

    if (stuff->length !=(sizeof(xSendExtensionEventReq)>>2) + stuff->count +
	(stuff->num_events * (sizeof (xEvent) >> 2)))
	{
	SendErrorToClient (client, IReqCode, X_SendExtensionEvent, 0, 
		BadLength);
	return Success;
	}

    dev = LookupDeviceIntRec (stuff->deviceid);
    if (dev == NULL)
	{
	SendErrorToClient(client, IReqCode, X_SendExtensionEvent, 0, 
		BadDevice);
	return Success;
	}

    /* The client's event type must be one defined by an extension. */

    first = ((xEvent *) &stuff[1]);
    if ( ! ((EXTENSION_EVENT_BASE  <= first->u.u.type) &&
	(first->u.u.type < lastEvent)) )
	{
	client->errorValue = first->u.u.type;
	SendErrorToClient(client, IReqCode, X_SendExtensionEvent, 0, 
		BadValue);
	return Success;
	}

    list = (XEventClass *) (first + stuff->num_events);
    if ((ret = CreateMaskFromList (client, list, stuff->count, tmp, dev, 
	X_SendExtensionEvent)) != Success)
	return Success;

    ret =  (SendEvent (client, dev, stuff->destination,
	stuff->propagate, &stuff[1], tmp[stuff->deviceid].mask, 
	stuff->num_events));

    if (ret != Success)
	SendErrorToClient(client, IReqCode, X_SendExtensionEvent, 0, ret);

    return Success;
    }
