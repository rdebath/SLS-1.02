/* $XConsortium: xgetkmap.c,v 1.4 89/12/02 15:21:00 rws Exp $ */

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
 *  Get the key mapping for an extension device.
 *
 */

#define	 NEED_EVENTS			/* for inputstr.h    */
#define	 NEED_REPLIES
#include "X.h"				/* for inputstr.h    */
#include "Xproto.h"			/* Request macro     */
#include "inputstr.h"			/* DeviceIntPtr	     */
#include "XI.h"
#include "XIproto.h"

extern	int 	IReqCode;
extern	int	BadDevice;
extern	void	(* ReplySwapVector[256]) ();
DeviceIntPtr	LookupDeviceIntRec();

/***********************************************************************
 *
 * This procedure gets the key mapping for an extension device,
 * for clients on machines with a different byte ordering than the server.
 *
 */

int
SProcXGetDeviceKeyMapping(client)
    register ClientPtr client;
    {
    register char n;

    REQUEST(xGetDeviceKeyMappingReq);
    swaps(&stuff->length, n);
    return(ProcXGetDeviceKeyMapping(client));
    }

/***********************************************************************
 *
 * Get the device key mapping.
 *
 */

ProcXGetDeviceKeyMapping(client)
    register ClientPtr client;
    {
    extern	void	CopySwap32Write();
    xGetDeviceKeyMappingReply rep;
    DeviceIntPtr dev;
    KeySymsPtr	k;

    REQUEST(xGetDeviceKeyMappingReq);
    REQUEST_SIZE_MATCH(xGetDeviceKeyMappingReq);

    dev = LookupDeviceIntRec (stuff->deviceid);
    if (dev == NULL)
	{
	SendErrorToClient (client, IReqCode, X_GetDeviceKeyMapping, 0, 
		BadDevice);
	return Success;
	}

    if (dev->key == NULL)
	{
	SendErrorToClient (client, IReqCode, X_GetDeviceKeyMapping, 0, 
		BadMatch);
	return Success;
	}
    k =  &dev->key->curKeySyms;

    if ((stuff->firstKeyCode < k->minKeyCode) ||
        (stuff->firstKeyCode > k->maxKeyCode))
        {
	client->errorValue = stuff->firstKeyCode;
	SendErrorToClient (client, IReqCode, X_GetDeviceKeyMapping, 0, 
		BadValue);
	return Success;
        }

    if (stuff->firstKeyCode + stuff->count > k->maxKeyCode + 1)
        {
	client->errorValue = stuff->count;
	SendErrorToClient (client, IReqCode, X_GetDeviceKeyMapping, 0, 
		BadValue);
	return Success;
        }

    rep.repType = X_Reply;
    rep.RepType = X_GetDeviceKeyMapping;
    rep.sequenceNumber = client->sequence;
    rep.keySymsPerKeyCode = k->mapWidth;
    rep.length = (k->mapWidth * stuff->count); /* KeySyms are 4 bytes */
    WriteReplyToClient(client, sizeof(xGetDeviceKeyMappingReply), &rep);

    client->pSwapReplyFunc = CopySwap32Write;
    WriteSwappedDataToClient(
	client,
	k->mapWidth * stuff->count * sizeof(KeySym),
	&k->map[stuff->firstKeyCode - k->minKeyCode]);

    return Success;
    }

/***********************************************************************
 *
 * This procedure writes the reply for the XGetDeviceKeyMapping function,
 * if the client and server have a different byte ordering.
 *
 */

SRepXGetDeviceKeyMapping (client, size, rep)
    ClientPtr	client;
    int		size;
    xGetDeviceKeyMappingReply	*rep;
    {
    register char n;

    swaps(&rep->sequenceNumber, n);
    swapl(&rep->length, n);
    WriteToClient(client, size, rep);
    }

