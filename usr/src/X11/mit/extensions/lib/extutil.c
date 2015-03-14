/*
 * $XConsortium: extutil.c,v 1.10 91/07/12 10:28:36 rws Exp $
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
 * Author:  Jim Fulton, MIT X Consortium
 *
 * 
 * 		       Xlib Extension-Writing Utilities
 * 
 * This package contains utilities for writing the client API for various
 * protocol extensions.  THESE INTERFACES ARE NOT PART OF THE X STANDARD AND
 * ARE SUBJECT TO CHANGE!
 * 
 *  Routines include:
 * 
 *         XextCreateExtension		called once per extension
 *         XextDestroyExtension		if no longer using extension
 *         XextAddDisplay		add another display
 *         XextRemoveDisplay		remove a display
 *         XextFindDisplay		is a display open
 * 
 * In addition, the following Xlib-style interfaces are provided:
 * 
 *         XSetExtensionErrorHandler	establish an extension error handler
 *         XMissingExtension		raise an error about missing ext
 */

#include <stdio.h>
#include "Xlibint.h"
#include "../include/Xext.h"
#include "../include/extutil.h"


/*
 * XextCreateExtension - return an extension descriptor containing context
 * information for this extension.  This object is passed to all Xext 
 * routines.
 */
XExtensionInfo *XextCreateExtension ()
{
    register XExtensionInfo *info =
      (XExtensionInfo *) Xmalloc (sizeof (XExtensionInfo));

    if (info) {
	info->head = NULL;
	info->cur = NULL;
	info->ndisplays = 0;
    }
    return info;
}


/*
 * XextDestroyExtension - free memory the given extension descriptor
 */
void XextDestroyExtension (info)
    XExtensionInfo *info;
{
    info->head = NULL;			/* to catch refs after this */
    info->cur = NULL;
    info->ndisplays = 0;
    XFree ((char *) info);
}



/*
 * XextAddDisplay - add a display to this extension
 */
XExtDisplayInfo *XextAddDisplay (extinfo, dpy, ext_name, hooks, nevents, data)
    XExtensionInfo *extinfo;
    Display *dpy;
    char *ext_name;
    XExtensionHooks *hooks;
    int nevents;
    caddr_t data;
{
    XExtDisplayInfo *dpyinfo;

    dpyinfo = (XExtDisplayInfo *) Xmalloc (sizeof (XExtDisplayInfo));
    if (!dpyinfo) return NULL;
    dpyinfo->next = extinfo->head;
    dpyinfo->display = dpy;
    dpyinfo->data = data;
    dpyinfo->codes = XInitExtension (dpy, ext_name);

    /*
     * if the server has the extension, then we can initialize the 
     * appropriate function vectors
     */
    if (dpyinfo->codes) {
	int i, j;

	for (i = 0, j = dpyinfo->codes->first_event; i < nevents; i++, j++) {
	    XESetWireToEvent (dpy, j, hooks->wire_to_event);
	    XESetEventToWire (dpy, j, hooks->event_to_wire);
	}
	if (hooks->create_gc)
	  XESetCreateGC (dpy, dpyinfo->codes->extension, hooks->create_gc);
	if (hooks->copy_gc)
	  XESetCopyGC (dpy, dpyinfo->codes->extension, hooks->copy_gc);
	if (hooks->flush_gc)
	  XESetFlushGC (dpy, dpyinfo->codes->extension, hooks->flush_gc);
	if (hooks->free_gc)
	  XESetFreeGC (dpy, dpyinfo->codes->extension, hooks->free_gc);
	if (hooks->create_font)
	  XESetCreateFont (dpy, dpyinfo->codes->extension, hooks->create_font);
	if (hooks->free_font)
	  XESetFreeFont (dpy, dpyinfo->codes->extension, hooks->free_font);
	if (hooks->close_display)
	  XESetCloseDisplay (dpy, dpyinfo->codes->extension, 
			     hooks->close_display);
	if (hooks->error)
	  XESetError (dpy, dpyinfo->codes->extension, hooks->error);
	if (hooks->error_string)
	  XESetErrorString (dpy, dpyinfo->codes->extension,
			    hooks->error_string);
    }

    /*
     * now, chain it onto the list
     */
    extinfo->head = dpyinfo;
    extinfo->cur = dpyinfo;
    extinfo->ndisplays++;
    return dpyinfo;
}


/*
 * XextRemoveDisplay - remove the indicated display from the extension object
 */
int XextRemoveDisplay (extinfo, dpy)
    XExtensionInfo *extinfo;
    Display *dpy;
{
    XExtDisplayInfo *dpyinfo, *prev;

    /*
     * locate this display and its back link so that it can be removed
     */
    prev = NULL;
    for (dpyinfo = extinfo->head; dpyinfo; dpyinfo = dpyinfo->next) {
	if (dpyinfo->display == dpy) break;
	prev = dpyinfo;
    }
    if (!dpyinfo) return 0;		/* hmm, actually an error */

    /*
     * remove the display from the list; handles going to zero
     */
    if (prev)
	prev->next = dpyinfo->next;
    else
	extinfo->head = dpyinfo->next;

    extinfo->ndisplays--;
    if (dpyinfo == extinfo->cur) extinfo->cur = NULL;  /* flush cache */

    Xfree ((char *) dpyinfo);
    return 1;
}


/*
 * XextFindDisplay - look for a display in this extension; keeps a cache
 * of the most-recently used for efficiency.
 */
XExtDisplayInfo *XextFindDisplay (extinfo, dpy)
    XExtensionInfo *extinfo;
    Display *dpy;
{
    register XExtDisplayInfo *dpyinfo;

    /*
     * see if this was the most recently accessed display
     */
    if ((dpyinfo = extinfo->cur)&& dpyinfo->display == dpy) return dpyinfo;


    /*
     * look for display in list
     */
    for (dpyinfo = extinfo->head; dpyinfo; dpyinfo = dpyinfo->next) {
	if (dpyinfo->display == dpy) {
	    extinfo->cur = dpyinfo;	/* cache most recently used */
	    return dpyinfo;
	}
    }

    return NULL;
}



static int _default_exterror (dpy, ext_name, reason)
    Display *dpy;
    char *ext_name;
    char *reason;
{
    fprintf (stderr, "Xlib:  extension \"%s\" %s on display \"%s\".\n",
	     ext_name, reason, DisplayString(dpy));
    return 0;
}


/*
 * XSetExtensionErrorHandler - sets the handler that gets called when a 
 * requested extension is referenced.  This should eventually move into Xlib.
 */

extern int (*_XExtensionErrorFunction)();

int (*XSetExtensionErrorHandler(handler))()
    int (*handler)();
{
    int (*oldhandler)() = _XExtensionErrorFunction;

    _XExtensionErrorFunction = (handler ? handler :
				_default_exterror);
    return oldhandler;
}


/*
 * XMissingExtension - call the extension error handler
 */
#if NeedFunctionPrototypes
int XMissingExtension (
    Display *dpy,
    _Xconst char *ext_name)
#else
int XMissingExtension (dpy, ext_name)
    Display *dpy;
    char *ext_name;
#endif
{
    int (*func)() = (_XExtensionErrorFunction ?
		     _XExtensionErrorFunction : _default_exterror);

    if (!ext_name) ext_name = X_EXTENSION_UNKNOWN;
    return (*func) (dpy, ext_name, X_EXTENSION_MISSING);
}
