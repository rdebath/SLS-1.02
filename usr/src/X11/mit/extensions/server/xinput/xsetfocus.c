/* $XConsortium: xsetfocus.c,v 1.5 90/05/18 14:15:11 rws Exp $ */

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
 * Request to set the focus for an extension device.
 *
 */

#define	 NEED_EVENTS
#define	 NEED_REPLIES
#include "X.h"				/* for inputstr.h    */
#include "Xproto.h"			/* Request macro     */
#include "windowstr.h"			/* focus struct      */
#include "inputstr.h"			/* DeviceIntPtr	     */
#include "XI.h"
#include "XIproto.h"

extern	int 		IReqCode;
extern	int		BadDevice;
extern	InputInfo	inputInfo;
extern	void		(* ReplySwapVector[256]) ();
DeviceIntPtr		LookupDeviceIntRec();

/***********************************************************************
 *
 * This procedure sets the focus for a device.
 *
 */

int
SProcXSetDeviceFocus(client)
    register ClientPtr client;
    {
    register char n;

    REQUEST(xSetDeviceFocusReq);
    swaps(&stuff->length, n);
    swapl(&stuff->focus, n);
    swapl(&stuff->time, n);
    return(ProcXSetDeviceFocus(client));
    }

/***********************************************************************
 *
 * This procedure sets the focus for a device.
 *
 */

int
ProcXSetDeviceFocus(client)
    register ClientPtr client;
    {
    int				ret;
    register DeviceIntPtr	dev;

    REQUEST(xSetDeviceFocusReq);
    REQUEST_SIZE_MATCH(xSetDeviceFocusReq);

    dev = LookupDeviceIntRec (stuff->device);
    if (dev==NULL || !dev->focus)
	{
	SendErrorToClient(client, IReqCode, X_SetDeviceFocus, 0, BadDevice);
	return Success;
	}

    ret = SetInputFocus (client, dev, stuff->focus, stuff->revertTo, 
	stuff->time, TRUE);
    if (ret != Success)
	SendErrorToClient(client, IReqCode, X_SetDeviceFocus, 0, ret);

    return Success;
    }
