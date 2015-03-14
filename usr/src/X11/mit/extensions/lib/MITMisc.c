/*
 * $XConsortium: MITMisc.c,v 1.4 91/01/05 14:46:12 rws Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

/* RANDOM CRUFT! THIS HAS NO OFFICIAL X CONSORTIUM BLESSING */

#define NEED_REPLIES
#include "Xlibint.h"
#include "MITMisc.h"
#include "mitmiscstr.h"
#include "Xext.h"
#include "extutil.h"

static XExtensionInfo _mit_info_data;
static XExtensionInfo *mit_info = &_mit_info_data;
static /* const */ char *mit_extension_name = MITMISCNAME;

#define MITCheckExtension(dpy,i,val) \
  XextCheckExtension (dpy, i, mit_extension_name, val)

/*****************************************************************************
 *                                                                           *
 *			   private utility routines                          *
 *                                                                           *
 *****************************************************************************/

static int close_display();
static /* const */ XExtensionHooks mit_extension_hooks = {
    NULL,				/* create_gc */
    NULL,				/* copy_gc */
    NULL,				/* flush_gc */
    NULL,				/* free_gc */
    NULL,				/* create_font */
    NULL,				/* free_font */
    close_display,			/* close_display */
    NULL,				/* wire_to_event */
    NULL,				/* event_to_wire */
    NULL,				/* error */
    NULL				/* error_string */
};

static XEXT_GENERATE_FIND_DISPLAY (find_display, mit_info, mit_extension_name, 
				   &mit_extension_hooks, MITMiscNumberEvents,
				   NULL)

static XEXT_GENERATE_CLOSE_DISPLAY (close_display, mit_info)


/*****************************************************************************
 *                                                                           *
 *		    public Shared Memory Extension routines                  *
 *                                                                           *
 *****************************************************************************/

Bool XMITMiscQueryExtension (dpy, event_basep, error_basep)
    Display *dpy;
    int *event_basep, *error_basep;
{
    XExtDisplayInfo *info = find_display (dpy);

    if (XextHasExtension(info)) {
	*event_basep = info->codes->first_event;
	*error_basep = info->codes->first_error;
	return True;
    } else {
	return False;
    }
}


Status XMITMiscSetBugMode(dpy, onOff)
    Display *dpy;
    Bool onOff;
{
    XExtDisplayInfo *info = find_display (dpy);
    register xMITSetBugModeReq *req;

    MITCheckExtension (dpy, info, 0);

    LockDisplay(dpy);
    GetReq(MITSetBugMode, req);
    req->reqType = info->codes->major_opcode;
    req->mitReqType = X_MITSetBugMode;
    req->onOff = onOff;
    UnlockDisplay(dpy);
    SyncHandle();
    return 1;
}

Bool XMITMiscGetBugMode(dpy)
    Display *dpy;
{
    XExtDisplayInfo *info = find_display (dpy);
    register xMITGetBugModeReq *req;
    xMITGetBugModeReply rep;

    MITCheckExtension (dpy, info, 0);

    LockDisplay(dpy);
    GetReq(MITGetBugMode, req);
    req->reqType = info->codes->major_opcode;
    req->mitReqType = X_MITGetBugMode;
    if (!_XReply(dpy, (xReply *)&rep, 0, xFalse)) {
	UnlockDisplay(dpy);
	SyncHandle();
	return False;
    }
    UnlockDisplay(dpy);
    SyncHandle();
    return rep.onOff;
}
