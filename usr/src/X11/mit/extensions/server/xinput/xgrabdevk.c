/* $Header: xgrabdevk.c,v 1.8 91/05/05 18:29:28 rws Exp $ */

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
 * Extension function to grab a key on an extension device.
 *
 */

#define	 NEED_EVENTS
#define	 NEED_REPLIES
#include "X.h"				/* for inputstr.h    */
#include "Xproto.h"			/* Request macro     */
#include "inputstr.h"			/* DeviceIntPtr	     */
#include "windowstr.h"			/* window structure  */
#include "XI.h"
#include "XIproto.h"

extern	int 		IReqCode;
extern	int		BadClass;
extern	int		BadDevice;
extern	InputInfo	inputInfo;
extern	void		(* ReplySwapVector[256]) ();
DeviceIntPtr		LookupDeviceIntRec();

/***********************************************************************
 *
 * Handle requests from clients with a different byte order.
 *
 */

int
SProcXGrabDeviceKey(client)
    register ClientPtr client;
    {
    register char n;
    register long *p;
    register int i;

    REQUEST(xGrabDeviceKeyReq);
    swaps(&stuff->length, n);
    swapl(&stuff->grabWindow, n);
    swaps(&stuff->modifiers, n);
    swaps(&stuff->event_count, n);
    p = (long *) &stuff[1];
    for (i=0; i<stuff->event_count; i++)
        {
        swapl(p, n);
	p++;
        }
    return(ProcXGrabDeviceKey(client));
    }

/***********************************************************************
 *
 * Grab a key on an extension device.
 *
 */

int
ProcXGrabDeviceKey(client)
    ClientPtr client;
    {
    int			ret;
    DeviceIntPtr 	dev;
    DeviceIntPtr 	mdev;
    XEventClass		*class;
    struct tmask	tmp[EMASKSIZE];

    REQUEST(xGrabDeviceKeyReq);
    REQUEST_AT_LEAST_SIZE(xGrabDeviceKeyReq);

    if (stuff->length !=(sizeof(xGrabDeviceKeyReq)>>2) + stuff->event_count)
	{
	SendErrorToClient (client, IReqCode, X_GrabDeviceKey, 0, BadLength);
	return Success;
	}

    dev = LookupDeviceIntRec (stuff->grabbed_device);
    if (dev == NULL)
	{
	SendErrorToClient(client, IReqCode, X_GrabDeviceKey, 0, 
	    BadDevice);
	return Success;
	}

    if (stuff->modifier_device != UseXKeyboard)
	{
	mdev = LookupDeviceIntRec (stuff->modifier_device);
	if (mdev == NULL)
	    {
	    SendErrorToClient(client, IReqCode, X_GrabDeviceKey, 0, 
	        BadDevice);
	    return Success;
	    }
	if (mdev->key == NULL)
	    {
	    SendErrorToClient(client, IReqCode, X_GrabDeviceKey, 0, 
		BadMatch);
	    return Success;
	    }
	}
    else
	mdev = (DeviceIntPtr) LookupKeyboardDevice();

    class = (XEventClass *) (&stuff[1]);	/* first word of values */

    if ((ret = CreateMaskFromList (client, class, 
	stuff->event_count, tmp, dev, X_GrabDeviceKey)) != Success)
	    return Success;

    ret = GrabKey(client, dev, stuff->this_device_mode, 
	stuff->other_devices_mode, stuff->modifiers, mdev, stuff->key, 
	stuff->grabWindow, stuff->ownerEvents, tmp[stuff->grabbed_device].mask);

    if (ret != Success)
        {
	SendErrorToClient(client, IReqCode, X_GrabDeviceKey, 0, ret);
        return Success;
        }

    return Success;
    }
