/* $XConsortium: xchgkmap.c,v 1.5 89/12/02 15:20:34 rws Exp $ */

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
 *  Change key mapping for an extension device.
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
extern	int	DeviceMappingNotify;
DeviceIntPtr	LookupDeviceIntRec();

/***********************************************************************
 *
 * This procedure swaps the request when the client and
 * server have different byte orderings.
 *
 */

int
SProcXChangeDeviceKeyMapping(client)
    register ClientPtr client;
    {
    register char n;
    register long *p;
    register int i, count;

    REQUEST(xChangeDeviceKeyMappingReq);
    swaps(&stuff->length, n);
    p = (long *) &stuff[1];
    count = stuff->keyCodes * stuff->keySymsPerKeyCode;
    for (i = 0; i < count; i++)
        {
        swapl(p, n);
	p++;
        }
    return(ProcXChangeDeviceKeyMapping(client));
    }

/***********************************************************************
 *
 * Change the device key mapping.
 *
 */

ProcXChangeDeviceKeyMapping(client)
    register ClientPtr client;
    {
    int	ret;
    unsigned len;
    DeviceIntPtr dev;

    REQUEST(xChangeDeviceKeyMappingReq);
    REQUEST_AT_LEAST_SIZE(xChangeDeviceKeyMappingReq);

    dev = LookupDeviceIntRec (stuff->deviceid);
    if (dev == NULL)
	{
	SendErrorToClient (client, IReqCode, X_ChangeDeviceKeyMapping, 0, 
		BadDevice);
	return Success;
	}
    len = stuff->length - (sizeof(xChangeDeviceKeyMappingReq) >> 2);  

    ret = ChangeKeyMapping (client, dev, len, DeviceMappingNotify, 
	stuff->firstKeyCode, stuff->keyCodes, stuff->keySymsPerKeyCode, 
	&stuff[1]);

    if (ret != Success)
	SendErrorToClient (client, IReqCode, X_ChangeDeviceKeyMapping, 0, 
		ret);
    return Success;
    }
