/* $XConsortium: xgetvers.c,v 1.5 90/05/18 15:35:29 rws Exp $ */

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
extern	void		(* ReplySwapVector[256]) ();
DeviceIntPtr		LookupDeviceIntRec();
XExtensionVersion	AllExtensionVersions[128];

/***********************************************************************
 *
 * Handle a request from a client with a different byte order than us.
 *
 */

int
SProcXGetExtensionVersion(client)
    register ClientPtr client;
    {
    register char n;

    REQUEST(xGetExtensionVersionReq);
    swaps(&stuff->length, n);
    swaps(&stuff->nbytes, n);
    return(ProcXGetExtensionVersion(client));
    }

/***********************************************************************
 *
 * This procedure lists the input devices available to the server.
 *
 */

ProcXGetExtensionVersion (client)
    register ClientPtr client;
    {
    xGetExtensionVersionReply	rep;

    REQUEST(xGetExtensionVersionReq);
    REQUEST_AT_LEAST_SIZE(xGetExtensionVersionReq);


    rep.repType = X_Reply;
    rep.RepType = X_GetExtensionVersion;
    rep.length = 0;
    rep.sequenceNumber = client->sequence;
    rep.major_version = 0;
    rep.minor_version = 0;

    rep.present = TRUE;
    if (rep.present)
	{
	rep.major_version = 
	    AllExtensionVersions[IReqCode-128].major_version;
	rep.minor_version = 
	    AllExtensionVersions[IReqCode-128].minor_version;
	}
    WriteReplyToClient (client, sizeof (xGetExtensionVersionReply), &rep);

    return Success;
    }

/***********************************************************************
 *
 * This procedure writes the reply for the XGetExtensionVersion function,
 * if the client and server have a different byte ordering.
 *
 */

SRepXGetExtensionVersion (client, size, rep)
    ClientPtr	client;
    int		size;
    xGetExtensionVersionReply	*rep;
    {
    register char n;

    swaps(&rep->sequenceNumber, n);
    swapl(&rep->length, n);
    swaps(&rep->major_version, n);
    swaps(&rep->minor_version, n);
    WriteToClient(client, size, rep);
    }
