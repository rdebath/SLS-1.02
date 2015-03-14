/* $Consortium: XGetDCtl.c,v 1.2 90/11/13 13:16:52 gms Exp $ */

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
 * XGetDeviceControl - get the Device control state of an extension device.
 *
 */

#include "XI.h"
#include "XIproto.h"
#include "Xlibint.h"
#include "Xlib.h"
#include "XInput.h"
#include "extutil.h"

XDeviceControl
*XGetDeviceControl (dpy, dev, control)
    register	Display 	*dpy;
    XDevice			*dev;
    int				control;
    {
    int	size = 0;
    int	nbytes, i;
    XDeviceControl *Device = NULL;
    XDeviceControl *Sav = NULL;
    xDeviceState *d = NULL;
    xDeviceState *sav = NULL;
    xGetDeviceControlReq *req;
    xGetDeviceControlReply rep;
    XExtDisplayInfo *info = (XExtDisplayInfo *) XInput_find_display (dpy);

    LockDisplay (dpy);
    if (CheckExtInit(dpy, XInput_Add_XChangeDeviceControl) == -1)
	return ((XDeviceControl *) NoSuchExtension);

    GetReq(GetDeviceControl,req);
    req->reqType = info->codes->major_opcode;
    req->ReqType = X_GetDeviceControl;
    req->deviceid = dev->device_id;
    req->control = control;

    if (! _XReply (dpy, (xReply *) &rep, 0, xFalse)) 
	{
	UnlockDisplay(dpy);
	SyncHandle();
	return (XDeviceControl *) NULL;
	}
    if (rep.length > 0) 
	{
	nbytes = (long)rep.length << 2;
	d = (xDeviceState *) Xmalloc((unsigned) nbytes);
        if (!d)
	    {
	    _XEatData (dpy, (unsigned long) nbytes);
	    UnlockDisplay(dpy);
	    SyncHandle();
	    return (XDeviceControl *) NULL;
	    }
	sav = d;
	_XRead (dpy, (char *) d, nbytes);

	switch (d->control)
	    {
	    case DEVICE_RESOLUTION:
		{
		xDeviceResolutionState *r;

		r = (xDeviceResolutionState *) d;
		size += sizeof (XDeviceResolutionState) + 
			(3 * sizeof(int) * r->num_valuators);
		break;
		}
	    default:
		size += d->length;
		break;
	    }

	Device = (XDeviceControl *) Xmalloc((unsigned) size);
        if (!Device)
	    {
	    UnlockDisplay(dpy);
	    SyncHandle();
	    return (XDeviceControl *) NULL;
	    }
	Sav = Device;

	d = sav;
	switch (control)
	    {
	    case DEVICE_RESOLUTION:
		{
		int *iptr, *iptr2;
		xDeviceResolutionState *r;
		XDeviceResolutionState *R;
		r = (xDeviceResolutionState *) d;
		R = (XDeviceResolutionState *) Device;

		R->control = DEVICE_RESOLUTION;
		R->length = sizeof (XDeviceResolutionState);
		R->num_valuators = r->num_valuators;
		iptr = (int *) (R+1);
		iptr2 = (int *) (r+1);
		R->resolutions = iptr;
		R->min_resolutions = iptr + R->num_valuators;
		R->max_resolutions = iptr + (2 * R->num_valuators);
		for (i=0; i < (3 * R->num_valuators); i++)
		    *iptr++ = *iptr2++;
		break;
		}
	    default:
		break;
	    }
	XFree (sav);
	}

    UnlockDisplay(dpy);
    SyncHandle();
    return (Sav);
    }

XFreeDeviceControl (control)
    XDeviceControl *control;
    {
    XFree (control);
    }
