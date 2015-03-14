/* $XConsortium: xchgdctl.c,v 1.1 91/07/24 15:51:26 rws Exp $ */

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

/********************************************************************
 *
 *  Change Device control attributes for an extension device.
 *
 */

#define	 NEED_EVENTS			/* for inputstr.h    */
#define	 NEED_REPLIES
#include "X.h"				/* for inputstr.h    */
#include "Xproto.h"			/* Request macro     */
#include "inputstr.h"			/* DeviceIntPtr	     */
#include "XI.h"
#include "XIproto.h"			/* control constants */

extern	int 	IReqCode;
extern	int	BadDevice, DeviceBusy;
extern	void	(* ReplySwapVector[256]) ();
DeviceIntPtr	LookupDeviceIntRec();

/***********************************************************************
 *
 * This procedure changes the control attributes for an extension device,
 * for clients on machines with a different byte ordering than the server.
 *
 */

int
SProcXChangeDeviceControl(client)
    register ClientPtr client;
    {
    register char n;

    REQUEST(xChangeDeviceControlReq);
    swaps(&stuff->length, n);
    swaps(&stuff->control, n);
    return(ProcXChangeDeviceControl(client));
    }

/***********************************************************************
 *
 * Change the control attributes.
 *
 */

ProcXChangeDeviceControl(client)
    ClientPtr client;
    {
    int i, len, status;
    DeviceIntPtr dev;
    xDeviceResolutionCtl *r;
    xChangeDeviceControlReply rep;
    AxisInfoPtr a;
    CARD32 *resolution;

    REQUEST(xChangeDeviceControlReq);
    REQUEST_AT_LEAST_SIZE(xChangeDeviceControlReq);

    len = stuff->length - (sizeof(xChangeDeviceControlReq) >>2);
    dev = LookupDeviceIntRec (stuff->deviceid);
    if (dev == NULL)
	{
	SendErrorToClient (client, IReqCode, X_ChangeDeviceControl, 0, 
		BadDevice);
	return Success;
	}

    rep.repType = X_Reply;
    rep.RepType = X_ChangeDeviceControl;
    rep.length = 0;
    rep.sequenceNumber = client->sequence;

    switch (stuff->control) 
	{
	case DEVICE_RESOLUTION:
	    if (!dev->valuator)
		{
		SendErrorToClient (client, IReqCode, X_ChangeDeviceControl, 0, 
		    BadMatch);
		return Success;
		}
	    if ((dev->grab) && !SameClient(dev->grab, client))
		{
		rep.status = AlreadyGrabbed;
		WriteReplyToClient(client, sizeof(xChangeDeviceControlReply), 
		    &rep);
		return Success;
		}
    	    r = (xDeviceResolutionCtl *) &stuff[1];
	    resolution = (CARD32 *) (r + 1);
	    if (r->first_valuator + r->num_valuators > dev->valuator->numAxes)
		{
		SendErrorToClient (client, IReqCode, X_ChangeDeviceControl, 0, 
		    BadValue);
		return Success;
		}
	    status = ChangeDeviceControl(client, dev, r);
	    if (status == Success)
		{
	        a = &dev->valuator->axes[r->first_valuator];
		for (i=0; i<r->num_valuators; i++)
		    if (*(resolution+i) < (a+i)->min_resolution ||
		        *(resolution+i) > (a+i)->max_resolution)
			{
			SendErrorToClient (client, IReqCode, 
			    X_ChangeDeviceControl, 0, BadValue);
			return Success;
			}
		for (i=0; i<r->num_valuators; i++)
		    (a++)->resolution = *resolution++; 
		}
	    else if (status == DeviceBusy)
		{
		rep.status = DeviceBusy;
		WriteReplyToClient(client, sizeof(xChangeDeviceControlReply), 
		    &rep);
		return Success;
		}
	    else 
		{
		SendErrorToClient (client, IReqCode, X_ChangeDeviceControl, 0, 
		    BadMatch);
		return Success;
		}
	    break;
	default:
	    SendErrorToClient (client, IReqCode, X_ChangeDeviceControl, 0, 
		BadValue);
	    return Success;
	}
    WriteReplyToClient(client, sizeof(xChangeDeviceControlReply), &rep);
    return Success;
    }

/***********************************************************************
 *
 * This procedure writes the reply for the xChangeDeviceControl function,
 * if the client and server have a different byte ordering.
 *
 */

SRepXChangeDeviceControl (client, size, rep)
    ClientPtr	client;
    int		size;
    xChangeDeviceControlReply	*rep;
    {
    register char n;

    swaps(&rep->sequenceNumber, n);
    swapl(&rep->length, n);
    WriteToClient(client, size, rep);
    }

