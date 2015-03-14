/*
 * $XConsortium: extutil.h,v 1.11 89/12/09 21:12:33 rws Exp $
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
 *                     Xlib Extension-Writing Utilities
 *
 * This package contains utilities for writing the client API for various
 * protocol extensions.  THESE INTERFACES ARE NOT PART OF THE X STANDARD AND
 * ARE SUBJECT TO CHANGE!
 */

#ifndef _EXTUTIL_H_
#define _EXTUTIL_H_

/*
 * We need to keep a list of open displays since the Xlib display list isn't
 * public.  We also have to per-display info in a separate block since it isn't
 * stored directly in the Display structure.
 */
typedef struct _XExtDisplayInfo {
    struct _XExtDisplayInfo *next;	/* keep a linked list */
    Display *display;			/* which display this is */
    XExtCodes *codes;			/* the extension protocol codes */
    caddr_t data;			/* extra data for extension to use */
} XExtDisplayInfo;

typedef struct _XExtensionInfo {
    XExtDisplayInfo *head;		/* start of list */
    XExtDisplayInfo *cur;		/* most recently used */
    int ndisplays;			/* number of displays */
} XExtensionInfo;

typedef struct _XExtensionHooks {
    int (*create_gc)();
    int (*copy_gc)();
    int (*flush_gc)();
    int (*free_gc)();
    int (*create_font)();
    int (*free_font)();
    int (*close_display)();
    Bool (*wire_to_event)();
    Status (*event_to_wire)();
    int (*error)();
    char *(*error_string)();
} XExtensionHooks;

extern XExtensionInfo *XextCreateExtension();
extern void XextDestroyExtension();
extern XExtDisplayInfo *XextAddDisplay();
extern int XextRemoveDisplay();
extern XExtDisplayInfo *XextFindDisplay();

#define XextHasExtension(i) ((i) && ((i)->codes))
#define XextCheckExtension(dpy,i,name,val) \
  if (!XextHasExtension(i)) { XMissingExtension (dpy, name); return val; }
#define XextSimpleCheckExtension(dpy,i,name) \
  if (!XextHasExtension(i)) { XMissingExtension (dpy, name); return; }


/*
 * helper macros to generate code that is common to all extensions; caller
 * should prefix it with static if extension source is in one file; this
 * could be a utility function, but have to stack 6 unused arguments for 
 * something that is called many, many times would be bad.
 */
#define XEXT_GENERATE_FIND_DISPLAY(proc,extinfo,extname,hooks,nev,data) \
XExtDisplayInfo *proc (dpy) \
    register Display *dpy; \
{ \
    XExtDisplayInfo *dpyinfo; \
    if (!extinfo) { if (!(extinfo = XextCreateExtension())) return NULL; } \
    if (!(dpyinfo = XextFindDisplay (extinfo, dpy))) \
      dpyinfo = XextAddDisplay (extinfo,dpy,extname,hooks,nev,data); \
    return dpyinfo; \
}

#define XEXT_GENERATE_CLOSE_DISPLAY(proc,extinfo) \
int proc (dpy, codes) \
    Display *dpy; \
    XExtCodes *codes; \
{ \
    return XextRemoveDisplay (extinfo, dpy); \
}

#define XEXT_GENERATE_ERROR_STRING(proc,extname,nerr,errl) \
char *proc (dpy, code, codes, buf, n) \
    Display  *dpy; \
    int code; \
    XExtCodes *codes; \
    char *buf; \
    int n; \
{  \
    code -= codes->first_error;  \
    if (code >= 0 && code < nerr) { \
	char tmp[256]; \
	sprintf (tmp, "%s.%d", extname, code); \
	XGetErrorDatabaseText (dpy, "XProtoError", tmp, errl[code], buf, n); \
	return buf; \
    } \
    return (char *)0; \
}

#endif
