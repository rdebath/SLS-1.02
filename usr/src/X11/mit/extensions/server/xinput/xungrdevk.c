/* $Header: xungrdevk.c,v 1.7 91/05/05 18:29:40 rws Exp $ */

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
 * Request to release a grab of a key on an extension device.
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

extern	int 	IReqCode;
extern	int	BadDevice;
extern	void	(* ReplySwapVector[256]) ();
extern	int	DeviceKeyPress;
DeviceIntPtr	LookupDeviceIntRec();

/***********************************************************************
 *
 * Handle requests from a client with a different byte order.
 *
 */

int
SProcXUngrabDeviceKey(client)
    register ClientPtr client;
    {
    register char n;

    REQUEST(xUngrabDeviceKeyReq);
    swaps(&stuff->length, n);
    swapl(&stuff->grabWindow, n);
    swaps(&stuff->modifiers, n);
    return(ProcXUngrabDeviceKey(client));
    }

/***********************************************************************
 *
 * Release a grab of a key on an extension device.
 *
 */

int
ProcXUngrabDeviceKey(client)
    ClientPtr client;
    {
    DeviceIntPtr	dev;
    DeviceIntPtr	mdev;
    WindowPtr 		pWin;
    GrabRec 		temporaryGrab;

    REQUEST(xUngrabDeviceKeyReq);
    REQUEST_SIZE_MATCH(xUngrabDeviceKeyReq);

    dev = LookupDeviceIntRec (stuff->grabbed_device);
    if (dev == NULL)
	{
	SendErrorToClient(client, IReqCode, X_UngrabDeviceKey, 0, 
	    BadDevice);
	return Success;
	}
    if (dev->key == NULL)
	{
	SendErrorToClient(client, IReqCode, X_UngrabDeviceKey, 0, BadMatch);
	return Success;
	}

    if (stuff->modifier_device != UseXKeyboard)
	{
	mdev = LookupDeviceIntRec (stuff->modifier_device);
	if (mdev == NULL)
	    {
	    SendErrorToClient(client, IReqCode, X_UngrabDeviceKey, 0, 
	        BadDevice);
	    return Success;
	    }
	if (mdev->key == NULL)
	    {
	    SendErrorToClient(client, IReqCode, X_UngrabDeviceKey, 0, 
		BadMatch);
	    return Success;
	    }
	}
    else
	mdev = (DeviceIntPtr) LookupKeyboardDevice();

    pWin = LookupWindow(stuff->grabWindow, client);
    if (!pWin)
	{
	SendErrorToClient(client, IReqCode, X_UngrabDeviceKey, 0, 
	    BadWindow);
	return Success;
	}

    temporaryGrab.resource = client->clientAsMask;
    temporaryGrab.device = dev;
    temporaryGrab.window = pWin;
    temporaryGrab.type  = DeviceKeyPress;
    temporaryGrab.modifierDevice = mdev;
    temporaryGrab.modifiersDetail.exact = stuff->modifiers;
    temporaryGrab.modifiersDetail.pMask = NULL;
    temporaryGrab.detail.exact = stuff->key;
    temporaryGrab.detail.pMask = NULL;

    DeletePassiveGrabFromList(&temporaryGrab);
    return Success;
    }
