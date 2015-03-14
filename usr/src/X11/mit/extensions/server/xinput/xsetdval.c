/* $XConsortium: xsetdval.c,v 1.1 91/02/22 15:34:21 rws Exp $ */

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
 * Request to change the mode of an extension input device.
 *
 */

#define	 NEED_EVENTS
#define	 NEED_REPLIES
#include "X.h"				/* for inputstr.h    */
#include "Xproto.h"			/* Request macro     */
#include "XI.h"
#include "XIproto.h"
#include "inputstr.h"			/* DeviceIntPtr	     */

extern	int 		IReqCode;
extern	int		BadDevice;
extern	void		(* ReplySwapVector[256]) ();
DeviceIntPtr		LookupDeviceIntRec();

/***********************************************************************
 *
 * Handle a request from a client with a different byte order.
 *
 */

int
SProcXSetDeviceValuators(client)
    register ClientPtr client;
    {
    register char n;

    REQUEST(xSetDeviceValuatorsReq);
    swaps(&stuff->length, n);
    return(ProcXSetDeviceValuators(client));
    }

/***********************************************************************
 *
 * This procedure sets the value of valuators on an extension input device.
 *
 */

int
ProcXSetDeviceValuators(client)
    register ClientPtr client;
    {
    DeviceIntPtr dev;
    xSetDeviceValuatorsReply	rep;

    REQUEST(xSetDeviceValuatorsReq);
    REQUEST_AT_LEAST_SIZE(xSetDeviceValuatorsReq);

    rep.repType = X_Reply;
    rep.RepType = X_SetDeviceValuators;
    rep.length = 0;
    rep.status = Success;
    rep.sequenceNumber = client->sequence;

    if (stuff->length !=(sizeof(xSetDeviceValuatorsReq)>>2) + 
	stuff->num_valuators)
	{
	SendErrorToClient (client, IReqCode, X_SetDeviceValuators, 0, 
		BadLength);
	return Success;
	}
    dev = LookupDeviceIntRec (stuff->deviceid);
    if (dev == NULL)
	{
	SendErrorToClient (client, IReqCode, X_SetDeviceValuators, 0, 
	    BadDevice);
	return Success;
	}
    if (dev->valuator == NULL)
	{
	SendErrorToClient(client, IReqCode, X_SetDeviceValuators, 0, 
		BadMatch);
	return Success;
	}

    if (stuff->first_valuator + stuff->num_valuators > dev->valuator->numAxes)
	{
	SendErrorToClient(client, IReqCode, X_SetDeviceValuators, 0, 
		BadValue);
	return Success;
	}

    if ((dev->grab) && !SameClient(dev->grab, client))
	rep.status = AlreadyGrabbed;
    else
	{
	rep.status = SetDeviceValuators (client, dev, (int *) &stuff[1],
	    stuff->first_valuator, stuff->num_valuators);
	if (rep.status != Success)
	    SendErrorToClient(client, IReqCode, X_SetDeviceValuators, 0, 
		rep.status);
	else
	    WriteReplyToClient (client, sizeof (xSetDeviceValuatorsReply), &rep);
	}

    return Success;
    }

/***********************************************************************
 *
 * This procedure writes the reply for the XSetDeviceValuators function,
 * if the client and server have a different byte ordering.
 *
 */

SRepXSetDeviceValuators (client, size, rep)
    ClientPtr	client;
    int		size;
    xSetDeviceValuatorsReply	*rep;
    {
    register char n;

    swaps(&rep->sequenceNumber, n);
    swapl(&rep->length, n);
    WriteToClient(client, size, rep);
    }
