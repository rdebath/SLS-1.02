/* $XConsortium: xgetfocus.c,v 1.5 90/05/18 14:14:43 rws Exp $ */

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
 * Extension function to get the focus for an extension device.
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
extern	void		(* ReplySwapVector[256]) ();
DeviceIntPtr		LookupDeviceIntRec();

/***********************************************************************
 *
 * This procedure gets the focus for a device.
 *
 */

int
SProcXGetDeviceFocus(client)
    register ClientPtr client;
    {
    register char n;

    REQUEST(xGetDeviceFocusReq);
    swaps(&stuff->length, n);
    return(ProcXGetDeviceFocus(client));
    }

/***********************************************************************
 *
 * This procedure gets the focus for a device.
 *
 */

int
ProcXGetDeviceFocus(client)
    ClientPtr client;
    {
    DeviceIntPtr	dev;
    FocusClassPtr 	focus;
    xGetDeviceFocusReply rep;

    REQUEST(xGetDeviceFocusReq);
    REQUEST_SIZE_MATCH(xGetDeviceFocusReq);

    dev = LookupDeviceIntRec (stuff->deviceid);
    if (dev == NULL || !dev->focus)
	{
	SendErrorToClient(client, IReqCode, X_GetDeviceFocus, 0, BadDevice);
	return Success;
	}

    rep.repType = X_Reply;
    rep.RepType = X_GetDeviceFocus;
    rep.length = 0;
    rep.sequenceNumber = client->sequence;

    focus = dev->focus;

    if (focus->win == NoneWin)
	rep.focus = None;
    else if (focus->win == PointerRootWin)
	rep.focus = PointerRoot;
    else if (focus->win == FollowKeyboardWin)
	rep.focus = FollowKeyboard;
    else 
	rep.focus = focus->win->drawable.id;

    rep.time = focus->time.milliseconds;
    rep.revertTo = focus->revert;
    WriteReplyToClient (client, sizeof(xGetDeviceFocusReply), &rep);
    return Success;
    }

/***********************************************************************
 *
 * This procedure writes the reply for the GetDeviceFocus function,
 * if the client and server have a different byte ordering.
 *
 */

SRepXGetDeviceFocus (client, size, rep)
    ClientPtr	client;
    int		size;
    xGetDeviceFocusReply	*rep;
    {
    register char n;

    swaps(&rep->sequenceNumber, n);
    swapl(&rep->length, n);
    swapl(&rep->focus, n);
    swapl(&rep->time, n);
    WriteToClient(client, size, rep);
    }
