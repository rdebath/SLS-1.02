/* $XConsortium: xgetbmap.c,v 1.4 89/12/02 15:20:53 rws Exp $ */

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
 * Extension function to return the version of the extension.
 *
 */

#define	 NEED_EVENTS
#define	 NEED_REPLIES
#include "X.h"				/* for inputstr.h    */
#include "Xproto.h"			/* Request macro     */
#include "inputstr.h"			/* DeviceIntPtr	     */
#include "XI.h"
#include "XIproto.h"

extern	int 		IReqCode;
extern	int 		BadDevice;
extern	void		(* ReplySwapVector[256]) ();
DeviceIntPtr		LookupDeviceIntRec();

/***********************************************************************
 *
 * This procedure gets the button mapping for the specified device.
 *
 */

int
SProcXGetDeviceButtonMapping(client)
    register ClientPtr client;
    {
    register char n;

    REQUEST(xGetDeviceButtonMappingReq);
    swaps(&stuff->length, n);
    return(ProcXGetDeviceButtonMapping(client));
    }

/***********************************************************************
 *
 * This procedure gets the button mapping for the specified device.
 *
 */

ProcXGetDeviceButtonMapping (client)
    register ClientPtr client;
    {
    DeviceIntPtr	dev;
    xGetDeviceButtonMappingReply	rep;
    ButtonClassPtr	b;

    REQUEST(xGetDeviceButtonMappingReq);
    REQUEST_SIZE_MATCH(xGetDeviceButtonMappingReq);

    rep.repType = X_Reply;
    rep.RepType = X_GetDeviceButtonMapping;
    rep.nElts = 0;
    rep.length = 0;
    rep.sequenceNumber = client->sequence;

    dev = LookupDeviceIntRec (stuff->deviceid);
    if (dev == NULL)
	{
	SendErrorToClient(client, IReqCode, X_GetDeviceButtonMapping, 0, 
		BadDevice);
	return Success;
	}

    b = dev->button;
    if (b == NULL)
	{
	SendErrorToClient(client, IReqCode, X_GetDeviceButtonMapping, 0, 
		BadMatch);
	return Success;
	}
    rep.nElts = b->numButtons;
    rep.length = (rep.nElts + (4-1))/4;
    WriteReplyToClient (client, sizeof (xGetDeviceButtonMappingReply), &rep);
    (void)WriteToClient(client, rep.nElts,
			(char *)&b->map[1]);
    return Success;
    }

/***********************************************************************
 *
 * This procedure writes the reply for the XGetDeviceButtonMapping function,
 * if the client and server have a different byte ordering.
 *
 */

SRepXGetDeviceButtonMapping (client, size, rep)
    ClientPtr	client;
    int		size;
    xGetDeviceButtonMappingReply	*rep;
    {
    register char n;

    swaps(&rep->sequenceNumber, n);
    swapl(&rep->length, n);
    WriteToClient(client, size, rep);
    }
