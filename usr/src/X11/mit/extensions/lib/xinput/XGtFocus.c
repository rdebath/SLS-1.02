/* $XConsortium: XGtFocus.c,v 1.4 89/12/06 20:38:40 rws Exp $ */

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
 * XGetDeviceFocus - Get the focus of an input device.
 *
 */

#include "XI.h"
#include "XIproto.h"
#include "Xlibint.h"
#include "XInput.h"
#include "extutil.h"

int
XGetDeviceFocus (dpy, dev, focus, revert_to, time)
    register Display *dpy;
    XDevice *dev;
    Window *focus;
    int *revert_to;
    int *time;
    {       
    xGetDeviceFocusReq 	*req;
    xGetDeviceFocusReply 	rep;
    XExtDisplayInfo *info = (XExtDisplayInfo *) XInput_find_display (dpy);

    LockDisplay (dpy);
    if (CheckExtInit(dpy, XInput_Initial_Release) == -1)
	return (NoSuchExtension);

    GetReq(GetDeviceFocus,req);		
    req->reqType = info->codes->major_opcode;
    req->ReqType = X_GetDeviceFocus;
    req->deviceid = dev->device_id;

    (void) _XReply (dpy, (xReply *) &rep, 0, xTrue);
    *focus = rep.focus;
    *revert_to = rep.revertTo;
    *time = rep.time;
    UnlockDisplay(dpy);
    SyncHandle();
    return (Success);
    }

