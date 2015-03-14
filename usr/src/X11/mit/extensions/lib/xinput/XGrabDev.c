/* $XConsortium: XGrabDev.c,v 1.5 91/07/23 12:27:52 rws Exp $ */

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
 * XGrabDevice - grab an extension input device.
 *
 */

#include "XI.h"
#include "XIproto.h"
#include "Xlibint.h"
#include "XInput.h"
#include "extutil.h"

int 
XGrabDevice (dpy, dev, grab_window, ownerEvents, event_count, event_list,
	this_device_mode, other_devices_mode, time)
    register Display 	*dpy;
    XDevice		*dev;
    Window 		grab_window;
    Bool 		ownerEvents;
    int			event_count;
    XEventClass		*event_list;
    int 		this_device_mode;
    int 		other_devices_mode;
    Time 		time;
    {
    xGrabDeviceReply rep;
    register xGrabDeviceReq *req;
    XExtDisplayInfo *info = (XExtDisplayInfo *) XInput_find_display (dpy);

    LockDisplay (dpy);
    if (CheckExtInit(dpy, XInput_Initial_Release) == -1)
	return (NoSuchExtension);

    GetReq(GrabDevice,req);		
    req->reqType = info->codes->major_opcode;
    req->ReqType = X_GrabDevice;
    
    req->deviceid = dev->device_id;
    req->grabWindow = grab_window;
    req->ownerEvents = ownerEvents;
    req->event_count = event_count;
    req->this_device_mode = this_device_mode;
    req->other_devices_mode = other_devices_mode;
    req->time = time;
    req->length += event_count;

    /* note: Data is a macro that uses its arguments multiple
       times, so "nvalues" is changed in a separate assignment
       statement */

    event_count <<= 2;
    Data (dpy, (char *) event_list, event_count);

    if (_XReply (dpy, (xReply *) &rep, 0, xTrue) == 0) 
	rep.status = GrabSuccess;
    UnlockDisplay(dpy);
    SyncHandle();
    return (rep.status);
    }
