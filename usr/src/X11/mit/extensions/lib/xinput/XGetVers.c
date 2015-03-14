/* $XConsortium: XGetVers.c,v 1.7 91/02/09 17:50:57 rws Exp $ */

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
 * XGetExtensionVersion - Get the version of the input extension.
 *
 */

#include "XIproto.h"
#include "Xlibint.h"
#include "XI.h"
#include "XInput.h"
#include "extutil.h"

XExtensionVersion
*XGetExtensionVersion (dpy, name)
    register Display 	*dpy;
    char		*name;
    {       
    xGetExtensionVersionReq 	*req;
    xGetExtensionVersionReply 	rep;
    XExtensionVersion		*ext;
    XExtDisplayInfo *info = (XExtDisplayInfo *) XInput_find_display (dpy);

    LockDisplay (dpy);
    if (CheckExtInit(dpy, Dont_Check) == -1)
	return ((XExtensionVersion *) NoSuchExtension);

    GetReq(GetExtensionVersion,req);		
    req->reqType = info->codes->major_opcode;
    req->ReqType = X_GetExtensionVersion;
    req->nbytes = name ? strlen(name) : 0;
    req->length += (unsigned)(req->nbytes+3)>>2;
    _XSend(dpy, name, (long)req->nbytes);

    if (! _XReply (dpy, (xReply *) &rep, 0, xTrue)) 
	{
	UnlockDisplay(dpy);
	SyncHandle();
	return (XExtensionVersion *) NULL;
	}
    ext = (XExtensionVersion *) Xmalloc (sizeof (XExtensionVersion));
    if (ext)
	{
	ext->present = rep.present;
	if (ext->present)
	    {
	    ext->major_version = rep.major_version;
	    ext->minor_version = rep.minor_version;
	    }
	}
    UnlockDisplay(dpy);
    SyncHandle();
    return (ext);
    }

