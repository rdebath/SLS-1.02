/* $XConsortium: xgetmmap.c,v 1.4 89/12/02 15:21:02 rws Exp $ */

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
 *  Get the modifier mapping for an extension device.
 *
 */

#define	 NEED_EVENTS			/* for inputstr.h    */
#define	 NEED_REPLIES
#include "X.h"				/* for inputstr.h    */
#include "Xproto.h"			/* Request macro     */
#include "inputstr.h"			/* DeviceIntPtr	     */
#include "XI.h"
#include "XIproto.h"			/* Request macro     */

extern	int 	IReqCode;
extern	int	BadDevice;
extern	void	(* ReplySwapVector[256]) ();
DeviceIntPtr	LookupDeviceIntRec();

/***********************************************************************
 *
 * This procedure gets the modifier mapping for an extension device,
 * for clients on machines with a different byte ordering than the server.
 *
 */

int
SProcXGetDeviceModifierMapping(client)
    register ClientPtr client;
    {
    register char n;

    REQUEST(xGetDeviceModifierMappingReq);
    swaps(&stuff->length, n);
    return(ProcXGetDeviceModifierMapping(client));
    }

/***********************************************************************
 *
 * Get the device Modifier mapping.
 *
 */

ProcXGetDeviceModifierMapping(client)
    ClientPtr client;
    {
    CARD8				maxkeys;
    DeviceIntPtr			dev;
    xGetDeviceModifierMappingReply 	rep;
    KeyClassPtr 			kp;
    
    REQUEST(xGetDeviceModifierMappingReq);
    REQUEST_SIZE_MATCH(xGetDeviceModifierMappingReq);

    dev = LookupDeviceIntRec (stuff->deviceid);
    if (dev == NULL)
	{
	SendErrorToClient (client, IReqCode, X_GetDeviceModifierMapping, 0, 
		BadDevice);
	return Success;
	}

    kp = dev->key;
    if (kp == NULL)
	{
	SendErrorToClient (client, IReqCode, X_GetDeviceModifierMapping, 0, 
		BadMatch);
	return Success;
	}
    maxkeys =  kp->maxKeysPerModifier;

    rep.repType = X_Reply;
    rep.RepType = X_GetDeviceModifierMapping;
    rep.numKeyPerModifier = maxkeys;
    rep.sequenceNumber = client->sequence;
    /* length counts 4 byte quantities - there are 8 modifiers 1 byte big */
    rep.length = 2*maxkeys;

    WriteReplyToClient(client, sizeof(xGetDeviceModifierMappingReply), &rep);

    /* Reply with the (modified by DDX) map that SetModifierMapping passed in */
    WriteToClient(client, 8*maxkeys, (char *)kp->modifierKeyMap);
    return Success;
    }

/***********************************************************************
 *
 * This procedure writes the reply for the XGetDeviceModifierMapping function,
 * if the client and server have a different byte ordering.
 *
 */

SRepXGetDeviceModifierMapping (client, size, rep)
    ClientPtr	client;
    int		size;
    xGetDeviceModifierMappingReply	*rep;
    {
    register char n;

    swaps(&rep->sequenceNumber, n);
    swapl(&rep->length, n);
    WriteToClient(client, size, rep);
    }
