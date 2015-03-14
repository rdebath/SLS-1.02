/* $XConsortium: XChgDCtl.c,v 1.2 91/07/24 16:13:07 rws Exp $ */

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
 * XChangeDeviceControl - Change the control attributes of an extension
 * input device.
 *
 */

#include "XI.h"
#include "XIproto.h"
#include "Xlibint.h"
#include "XInput.h"
#include "extutil.h"

int
XChangeDeviceControl (dpy, dev, control, d)
    register	Display 	*dpy;
    XDevice			*dev;
    int				control;
    XDeviceControl		*d;
    {
    int length;
    xChangeDeviceControlReq	*req;
    xChangeDeviceControlReply	rep;
    XExtDisplayInfo *info = (XExtDisplayInfo *) XInput_find_display (dpy);

    LockDisplay (dpy);
    if (CheckExtInit(dpy, XInput_Add_XChangeDeviceControl) == -1)
	return (NoSuchExtension);

    GetReq(ChangeDeviceControl,req);
    req->reqType = info->codes->major_opcode;
    req->ReqType = X_ChangeDeviceControl;
    req->deviceid = dev->device_id;
    req->control = control;

    switch (control)
	{
	case DEVICE_RESOLUTION:
	    {
	    XDeviceResolutionControl	*R;
	    xDeviceResolutionCtl	r;

	    R = (XDeviceResolutionControl *) d;
	    r.control = DEVICE_RESOLUTION;
	    r.length = sizeof (xDeviceResolutionCtl) + 
		R->num_valuators * sizeof(int);
	    r.first_valuator = R->first_valuator;
	    r.num_valuators = R->num_valuators;
	    req->length += ((unsigned)(r.length + 3) >> 2);
	    length = sizeof (xDeviceResolutionCtl);
	    Data (dpy, (char *) &r, length);
	    length  = r.num_valuators * sizeof(int);
	    Data (dpy, (char *) R->resolutions, length);
	    if (! _XReply (dpy, (xReply *) &rep, 0, xTrue)) 
		{
		UnlockDisplay(dpy);
		SyncHandle();
		return (NoSuchExtension);
		}
	    else
		return (rep.status);
	    }
	default:
	    {
	    xDeviceCtl		u;

	    u.control = d->control;
	    u.length = d->length - sizeof (int);
	    length = ((unsigned)(u.length + 3) >> 2);
	    req->length += length;
	    length <<= 2;
	    Data (dpy, (char *) &u, length);
	    }
	}

    UnlockDisplay(dpy);
    SyncHandle();
    return (Success);
    }

