/* $XConsortium: xsetmode.c,v 1.8 91/02/22 15:33:34 rws Exp $ */

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
SProcXSetDeviceMode(client)
    register ClientPtr client;
    {
    register char n;

    REQUEST(xSetDeviceModeReq);
    swaps(&stuff->length, n);
    return(ProcXSetDeviceMode(client));
    }

/***********************************************************************
 *
 * This procedure sets the mode of a device.
 *
 */

int
ProcXSetDeviceMode(client)
    register ClientPtr client;
    {
    DeviceIntPtr dev;
    xSetDeviceModeReply	rep;

    REQUEST(xSetDeviceModeReq);
    REQUEST_SIZE_MATCH(xSetDeviceModeReq);

    rep.repType = X_Reply;
    rep.RepType = X_SetDeviceMode;
    rep.length = 0;
    rep.sequenceNumber = client->sequence;

    dev = LookupDeviceIntRec (stuff->deviceid);
    if (dev == NULL)
	{
	SendErrorToClient (client, IReqCode, X_SetDeviceMode, 0, BadDevice);
	return Success;
	}
    if (dev->valuator == NULL)
	{
	SendErrorToClient(client, IReqCode, X_SetDeviceValuators, 0, 
		BadMatch);
	return Success;
	}
    if ((dev->grab) && !SameClient(dev->grab, client))
	rep.status = AlreadyGrabbed;
    else
	{
	rep.status = SetDeviceMode (client, dev, stuff->mode);
	if (rep.status != Success)
	    SendErrorToClient(client, IReqCode, X_SetDeviceMode, 0, rep.status);
	else
	    {
  	    dev->valuator->mode = stuff->mode;
	    WriteReplyToClient (client, sizeof (xSetDeviceModeReply), &rep);
	    }
	}


    return Success;
    }

/***********************************************************************
 *
 * This procedure writes the reply for the XSetDeviceMode function,
 * if the client and server have a different byte ordering.
 *
 */

SRepXSetDeviceMode (client, size, rep)
    ClientPtr	client;
    int		size;
    xSetDeviceModeReply	*rep;
    {
    register char n;

    swaps(&rep->sequenceNumber, n);
    swapl(&rep->length, n);
    WriteToClient(client, size, rep);
    }
