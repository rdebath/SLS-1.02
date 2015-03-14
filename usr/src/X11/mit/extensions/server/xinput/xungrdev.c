/* $XConsortium: xungrdev.c,v 1.6 89/12/02 15:21:45 rws Exp $ */

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
 * Request to release a grab of an extension device.
 *
 */

#define	 NEED_EVENTS
#define	 NEED_REPLIES
#include "X.h"				/* for inputstr.h    */
#include "Xproto.h"			/* Request macro     */
#include "inputstr.h"			/* DeviceIntPtr	     */
#include "windowstr.h"			/* window structure  */
#include "XIproto.h"

extern	int 	IReqCode;
extern	int	BadDevice;
extern	void	(* ReplySwapVector[256]) ();
DeviceIntPtr	LookupDeviceIntRec();

/***********************************************************************
 *
 * Handle requests from a client with a different byte order.
 *
 */

int
SProcXUngrabDevice(client)
register ClientPtr client;
    {
    register char n;

    REQUEST(xUngrabDeviceReq);
    swaps(&stuff->length, n);
    swapl(&stuff->time, n);
    return(ProcXUngrabDevice(client));
    }

/***********************************************************************
 *
 * Release a grab of an extension device.
 *
 */

int
ProcXUngrabDevice(client)
register ClientPtr client;
    {
    DeviceIntPtr 	dev;
    GrabPtr 		grab;
    TimeStamp 		time;

    REQUEST(xUngrabDeviceReq);
    REQUEST_SIZE_MATCH(xUngrabDeviceReq);

    dev = LookupDeviceIntRec (stuff->deviceid);
    if (dev == NULL)
	{
	SendErrorToClient(client, IReqCode, X_UngrabDevice, 0, BadDevice);
	return Success;
	}
    grab =  dev->grab;

    time = ClientTimeToServerTime(stuff->time);
    if ((CompareTimeStamps(time, currentTime) != LATER) &&
	(CompareTimeStamps(time, dev->grabTime) != EARLIER) &&
	(grab) && SameClient(grab, client))
	(*dev->DeactivateGrab)(dev);
    return Success;
    }
