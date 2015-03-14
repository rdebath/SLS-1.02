/* $XConsortium: XGetKMap.c,v 1.5 90/05/18 11:23:24 rws Exp $ */

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
 * XGetDeviceKeyMapping - get the keymap of an extension device.
 *
 */

#include "XI.h"
#include "XIproto.h"
#include "Xlibint.h"
#include "XInput.h"
#include "extutil.h"

KeySym 
*XGetDeviceKeyMapping (dpy, dev, first, keycount, syms_per_code)
    register	Display 	*dpy;
    XDevice			*dev;
    KeyCode			first;
    int				keycount;
    int				*syms_per_code;		/* RETURN */
    {
    long nbytes;
    register KeySym *mapping = NULL;
    xGetDeviceKeyMappingReq *req;
    xGetDeviceKeyMappingReply rep;
    XExtDisplayInfo *info = (XExtDisplayInfo *) XInput_find_display (dpy);

    LockDisplay (dpy);
    if (CheckExtInit(dpy, XInput_Initial_Release) == -1)
	return ((KeySym *) NoSuchExtension);

    GetReq(GetDeviceKeyMapping,req);
    req->reqType = info->codes->major_opcode;
    req->ReqType = X_GetDeviceKeyMapping;
    req->deviceid = dev->device_id;
    req->firstKeyCode = first;
    req->count = keycount;

    if (! _XReply (dpy, (xReply *) &rep, 0, xFalse)) 
	{
	UnlockDisplay(dpy);
	SyncHandle();
	return (KeySym *) NULL;
	}
    if (rep.length > 0) {
        *syms_per_code = rep.keySymsPerKeyCode;
	nbytes = (long)rep.length << 2;
	mapping = (KeySym *) Xmalloc((unsigned) nbytes);
	if (mapping)
	    _XRead (dpy, (char *)mapping, nbytes);
	else
	    _XEatData (dpy, (unsigned long) nbytes);
      }

    UnlockDisplay(dpy);
    SyncHandle();
    return (mapping);
    }
