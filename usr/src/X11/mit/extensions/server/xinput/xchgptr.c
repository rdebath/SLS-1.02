/* $Header: xchgptr.c,v 1.14 91/05/05 17:36:57 rws Exp $ */

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
 * Extension function to change the pointer device.
 *
 */

#define	 NEED_EVENTS
#define	 NEED_REPLIES
#include "X.h"				/* for inputstr.h    */
#include "Xproto.h"			/* Request macro     */
#include "XI.h"
#include "XIproto.h"
#include "inputstr.h"			/* DeviceIntPtr	     */
#include "windowstr.h"			/* window structure  */
#include "scrnintstr.h"			/* screen structure  */

extern	int 		IReqCode;
extern	int 		BadDevice;
extern	int 		ChangeDeviceNotify;
extern	Mask		ChangeDeviceNotifyMask;
extern	InputInfo	inputInfo;
extern	ScreenInfo	screenInfo;
extern	WindowPtr	*WindowTable;
extern	void		(* ReplySwapVector[256]) ();
DeviceIntPtr		LookupDeviceIntRec();

/***********************************************************************
 *
 * This procedure is invoked to swap the request bytes if the server and
 * client have a different byte order.
 *
 */

int
SProcXChangePointerDevice(client)
    register ClientPtr client;
    {
    register char n;

    REQUEST(xChangePointerDeviceReq);
    swaps(&stuff->length, n);
    return(ProcXChangePointerDevice(client));
    }

/***********************************************************************
 *
 * This procedure changes the device used as the X pointer.
 *
 */

ProcXChangePointerDevice (client)
    register ClientPtr client;
    {
    DeviceIntPtr 	xptr = inputInfo.pointer;
    DeviceIntPtr 	dev;
    ValuatorClassPtr 	v;
    xChangePointerDeviceReply	rep;
    changeDeviceNotify	ev;

    REQUEST(xChangePointerDeviceReq);
    REQUEST_SIZE_MATCH(xChangePointerDeviceReq);

    rep.repType = X_Reply;
    rep.RepType = X_ChangePointerDevice;
    rep.length = 0;
    rep.sequenceNumber = client->sequence;

    dev = LookupDeviceIntRec (stuff->deviceid);
    if (dev == NULL)
	{
	rep.status = -1;
	SendErrorToClient(client, IReqCode, X_ChangePointerDevice, 0, 
	    BadDevice);
	return Success;
	}

    v = dev->valuator;
    if (v == NULL || v->numAxes < 2 || 
	stuff->xaxis >= v->numAxes ||
	stuff->yaxis >= v->numAxes)
	{
	rep.status = -1;
	SendErrorToClient(client, IReqCode, X_ChangePointerDevice, 0, BadMatch);
	return Success;
	}

    if (((dev->grab) && !SameClient(dev->grab, client)) ||
        ((xptr->grab) && !SameClient(xptr->grab, client)))
	rep.status = AlreadyGrabbed;
    else if ((dev->sync.frozen &&
	     dev->sync.other && !SameClient(dev->sync.other, client)) ||
	     (xptr->sync.frozen &&
	      xptr->sync.other && !SameClient(xptr->sync.other, client)))
	rep.status = GrabFrozen;
    else
	{
	if (ChangePointerDevice (
	    xptr, dev, stuff->xaxis, stuff->yaxis) != Success)
	    {
	    SendErrorToClient(client, IReqCode, X_ChangePointerDevice, 0, 
		BadDevice);
	    return Success;
	    }
	if (dev->focus)
	    DeleteFocusClassDeviceStruct(dev);
	RegisterOtherDevice (xptr);
	RegisterPointerDevice (dev);

	ev.type = ChangeDeviceNotify;
	ev.deviceid = stuff->deviceid;
	ev.time = currentTime.milliseconds;
	ev.request = NewPointer;

	SendEventToAllWindows (dev, ChangeDeviceNotifyMask, &ev, 1);
	SendMappingNotify (MappingPointer, 0, 0);

	rep.status = 0;
	}

    WriteReplyToClient (client, sizeof (xChangePointerDeviceReply), 
	&rep);
    return Success;
    }

DeleteFocusClassDeviceStruct(dev)
    DeviceIntPtr dev;
    {
    xfree(dev->focus->trace);
    xfree(dev->focus);
    dev->focus = NULL;
    }

/***********************************************************************
 *
 * Send an event to interested clients in all windows on all screens.
 *
 */

SendEventToAllWindows (dev, mask, ev, count)
    DeviceIntPtr dev;
    Mask mask;
    xEvent *ev;
    int count;
    {
    int i;
    WindowPtr pWin, p1;

    for (i=0; i<screenInfo.numScreens; i++)
	{
	pWin = WindowTable[i];
	(void)DeliverEventsToWindow(pWin, ev, count, mask, NullGrab, dev->id);
	p1 = pWin->firstChild;
	FindInterestedChildren (dev, p1, mask, ev, count);
	}
    }

/***********************************************************************
 *
 * Walk through the window tree, finding all clients that want to know
 * about the ChangeDeviceNotify Event.
 *
 */

FindInterestedChildren (dev, p1, mask, ev, count)
    DeviceIntPtr	dev;
    WindowPtr 		p1;
    Mask		mask;
    xEvent		*ev;
    int			count;
    {
    WindowPtr p2;

    while (p1)
        {
        p2 = p1->firstChild;
	(void)DeliverEventsToWindow(p1, ev, count, mask, NullGrab, dev->id);
	FindInterestedChildren (dev, p2, mask, ev, count);
	p1 = p1->nextSib;
        }
    }

/***********************************************************************
 *
 * This procedure writes the reply for the XChangePointerDevice 
 * function, if the client and server have a different byte ordering.
 *
 */

SRepXChangePointerDevice (client, size, rep)
    ClientPtr	client;
    int		size;
    xChangePointerDeviceReply	*rep;
    {
    register char n;

    swaps(&rep->sequenceNumber, n);
    swapl(&rep->length, n);
    WriteToClient(client, size, rep);
    }
