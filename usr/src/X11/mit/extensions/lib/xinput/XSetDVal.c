/* $XConsortium: XSetDVal.c,v 1.1 91/02/22 15:26:57 rws Exp $ */

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
 * XSetDeviceValuators - Set the value of valuators on an extension input
 * device.
 *
 */

#include "XI.h"
#include "XIproto.h"
#include "Xlibint.h"
#include "XInput.h"
#include "extutil.h"

int
XSetDeviceValuators (dpy, dev, valuators, first_valuator, num_valuators)
    register Display 	*dpy;
    XDevice 		*dev;
    int			*valuators;
    int			first_valuator;
    int			num_valuators;
    {       
    xSetDeviceValuatorsReq 		*req;
    xSetDeviceValuatorsReply 	rep;
    XExtDisplayInfo *info = (XExtDisplayInfo *) XInput_find_display (dpy);

    LockDisplay (dpy);
    if (CheckExtInit(dpy, XInput_Add_XSetDeviceValuators) == -1)
	return (NoSuchExtension);

    GetReq(SetDeviceValuators,req);		
    req->reqType = info->codes->major_opcode;
    req->ReqType = X_SetDeviceValuators;
    req->deviceid = dev->device_id;
    req->first_valuator = first_valuator;
    req->num_valuators = num_valuators;
    req->length += num_valuators;

    /* note: Data is a macro that uses its arguments multiple
       times, so "nvalues" is changed in a separate assignment
       statement */

    num_valuators <<= 2;
    Data (dpy, (char *) valuators, num_valuators);

    (void) _XReply (dpy, (xReply *) &rep, 0, xTrue);
    UnlockDisplay(dpy);
    SyncHandle();
    return (rep.status);
    }

