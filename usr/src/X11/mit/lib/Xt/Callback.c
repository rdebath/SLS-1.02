/* $XConsortium: Callback.c,v 1.35 91/11/21 14:36:30 converse Exp $ */

/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#include "IntrinsicI.h"

static String XtNinvalidCallbackList = "invalidCallbackList";
static String XtNxtAddCallback = "xtAddCallback";
static String XtNxtRemoveCallback = "xtRemoveCallback";
static String XtNxtRemoveAllCallback = "xtRemoveAllCallback";
static String XtNxtCallCallback = "xtCallCallback";

/* However it doesn't contain a final NULL record */
#define ToList(p) ((XtCallbackList) ((p)+1))

static InternalCallbackList* FetchInternalList(widget, name)
    Widget	widget;
    String	name;
{
    register XrmQuark		quark;
    register int 		n;
    register CallbackTable	offsets;

    quark = StringToQuark(name);
    offsets = (CallbackTable) 
	widget->core.widget_class->core_class.callback_private;

    for (n = (int) *(offsets++); --n >= 0; offsets++)
	if (quark == (*offsets)->xrm_name)
	    return (InternalCallbackList *) 
		((char *) widget - (*offsets)->xrm_offset - 1);
    return NULL;
}


void _XtAddCallback(callbacks, callback, closure)
    InternalCallbackList*   callbacks;
    XtCallbackProc	    callback;
    XtPointer		    closure;
{
    register InternalCallbackList icl;
    register XtCallbackList cl;
    register int count;
    
    icl = *callbacks;
    count = icl ? icl->count : 0;

    if (icl && icl->call_state) {
	icl->call_state |= _XtCBFreeAfterCalling;
	icl = (InternalCallbackList)
	    XtMalloc(sizeof(InternalCallbackRec) +
		     sizeof(XtCallbackRec) * (count + 1));
	bcopy((char *)ToList(*callbacks), (char *)ToList(icl),
	      sizeof(XtCallbackRec) * count);
    } else {
	icl = (InternalCallbackList)
	    XtRealloc((char *) icl, sizeof(InternalCallbackRec) +
		      sizeof(XtCallbackRec) * (count + 1));
    }
    *callbacks = icl;
    icl->count = count + 1;
    icl->is_padded = 0;
    icl->call_state = 0;
    cl = ToList(icl) + count;
    cl->callback = callback;
    cl->closure = closure;
} /* _XtAddCallback */

void _XtAddCallbackOnce(callbacks, callback, closure)
    register InternalCallbackList*callbacks;
    XtCallbackProc	    callback;
    XtPointer		    closure;
{
    register XtCallbackList cl = ToList(*callbacks);
    register int i;
    
    for (i=(*callbacks)->count; --i >= 0; cl++)
	if (cl->callback == callback && cl->closure == closure)
	    return;

    _XtAddCallback(callbacks, callback, closure);
} /* _XtAddCallbackOnce */

#if NeedFunctionPrototypes
void XtAddCallback(
    Widget	    widget,
    _Xconst char*   name,
    XtCallbackProc  callback,
    XtPointer	    closure
    )
#else
void XtAddCallback(widget, name, callback, closure)
    Widget	    widget;
    String	    name;
    XtCallbackProc  callback;
    XtPointer	    closure;
#endif
{
    InternalCallbackList *callbacks;

    callbacks = FetchInternalList(widget, name);
    if (!callbacks) {
       XtAppWarningMsg(XtWidgetToApplicationContext(widget),
	       XtNinvalidCallbackList,XtNxtAddCallback,XtCXtToolkitError,
              "Cannot find callback list in XtAddCallback",
	      (String *)NULL, (Cardinal *)NULL);
       return;
    }
    _XtAddCallback(callbacks, callback, closure);
} /* XtAddCallback */

/* ARGSUSED */
static void AddCallbacks(widget, callbacks, newcallbacks)
    Widget		    widget;
    InternalCallbackList   *callbacks;
    XtCallbackList	    newcallbacks;
{
    register InternalCallbackList icl;
    register int i, j;
    register XtCallbackList cl;

    icl = *callbacks;
    i = icl ? icl->count : 0;
    for (j=0, cl = newcallbacks; cl->callback; cl++, j++);
    if (icl && icl->call_state) {
	icl->call_state |= _XtCBFreeAfterCalling;
	icl = (InternalCallbackList) XtMalloc(sizeof(InternalCallbackRec) +
					      sizeof(XtCallbackRec) * (i+j));
	bcopy((char *)ToList(*callbacks), (char *)ToList(icl),
	      sizeof(XtCallbackRec) * i);
    } else {
	icl = (InternalCallbackList) XtRealloc((char *) icl,
					       sizeof(InternalCallbackRec) + 
					       sizeof(XtCallbackRec) * (i+j));
    }
    *callbacks = icl;
    icl->count = i+j;
    icl->is_padded = 0;
    icl->call_state = 0;
    for (cl = ToList(icl) + i; --j >= 0; )
	*cl++ = *newcallbacks++;
} /* AddCallbacks */

#if NeedFunctionPrototypes
void XtAddCallbacks(
    Widget	    widget,
    _Xconst char*   name,
    XtCallbackList  xtcallbacks
    )
#else
void XtAddCallbacks(widget, name, xtcallbacks)
    Widget	    widget;
    String	    name;
    XtCallbackList  xtcallbacks;
#endif
{
    InternalCallbackList* callbacks;

    callbacks = FetchInternalList(widget, name);
    if (!callbacks) {
       XtAppWarningMsg(XtWidgetToApplicationContext(widget),
	       XtNinvalidCallbackList,XtNxtAddCallback,XtCXtToolkitError,
              "Cannot find callback list in XtAddCallbacks",
	      (String *)NULL, (Cardinal *)NULL);
       return;
    }
    AddCallbacks(widget, callbacks, xtcallbacks);
} /* XtAddCallbacks */

void _XtRemoveCallback (callbacks, callback, closure)
    InternalCallbackList   *callbacks;
    XtCallbackProc	    callback;
    XtPointer		    closure;

{
    register InternalCallbackList icl;
    register int i, j;
    register XtCallbackList cl, ncl, ocl;

    icl = *callbacks;
    if (!icl) return;

    cl = ToList(icl);
    for (i=icl->count; --i >= 0; cl++) {
	if (cl->callback == callback && cl->closure == closure) {
	    if (icl->call_state) {
		icl->call_state |= _XtCBFreeAfterCalling;
		if (icl->count == 1) {
		    *callbacks = NULL;
		} else {
		    j = icl->count - i - 1;
		    ocl = ToList(icl);
		    icl = (InternalCallbackList)
			XtMalloc(sizeof(InternalCallbackRec) +
				 sizeof(XtCallbackRec) * (i + j));
		    icl->count = i + j;
		    icl->is_padded = 0;
		    icl->call_state = 0;
		    ncl = ToList(icl);
		    while (--j >= 0)
			*ncl++ = *ocl++;
		    while (--i >= 0)
			*ncl++ = *++cl;
		    *callbacks = icl;
		}
	    } else {
		if (--icl->count) {
		    ncl = cl + 1;
		    while (--i >= 0)
			*cl++ = *ncl++;
		    icl = (InternalCallbackList)
			XtRealloc((char *) icl, sizeof(InternalCallbackRec)
				  + sizeof(XtCallbackRec) * icl->count);
		    icl->is_padded = 0;
		    *callbacks = icl;
		} else {
		    XtFree((char *) icl);
		    *callbacks = NULL;
		}
	    }
	    return;
	}
    }
} /* _XtRemoveCallback */

#if NeedFunctionPrototypes
void XtRemoveCallback (
    Widget	    widget,
    _Xconst char*   name,
    XtCallbackProc  callback,
    XtPointer	    closure
    )
#else
void XtRemoveCallback (widget, name, callback, closure)
    Widget	    widget;
    String	    name;
    XtCallbackProc  callback;
    XtPointer	    closure;
#endif
{
    InternalCallbackList *callbacks;

    callbacks = FetchInternalList(widget, name);
    if (!callbacks) {
       XtAppWarningMsg(XtWidgetToApplicationContext(widget),
	       XtNinvalidCallbackList,XtNxtRemoveCallback,XtCXtToolkitError,
              "Cannot find callback list in XtRemoveCallbacks",
	      (String *)NULL, (Cardinal *)NULL);
	return;
    }

    _XtRemoveCallback(callbacks, callback, closure);
} /* XtRemoveCallback */


#if NeedFunctionPrototypes
void XtRemoveCallbacks (widget, name, xtcallbacks)
    Widget	    widget;
    _Xconst char*   name;
    XtCallbackList  xtcallbacks;
#else
void XtRemoveCallbacks (widget, name, xtcallbacks)
    Widget	    widget;
    String	    name;
    XtCallbackList  xtcallbacks;
#endif
{
    InternalCallbackList *callbacks;
    register int i;
    register InternalCallbackList icl;
    register XtCallbackList cl, ccl, rcl;

    callbacks = FetchInternalList(widget, name);
    if (!callbacks) {
       XtAppWarningMsg(XtWidgetToApplicationContext(widget),
	       XtNinvalidCallbackList,XtNxtRemoveCallback,XtCXtToolkitError,
              "Cannot find callback list in XtRemoveCallbacks",
	      (String *)NULL, (Cardinal *)NULL);
	return;
    }

    icl = *callbacks;
    if (!icl) return;

    i = icl->count;
    cl = ToList(icl);
    if (icl->call_state) {
	icl->call_state |= _XtCBFreeAfterCalling;
	icl = (InternalCallbackList)XtMalloc(sizeof(InternalCallbackRec) +
					     sizeof(XtCallbackRec) * i);
	icl->count = i;
	icl->call_state = 0;
    }
    ccl = ToList(icl);
    while (--i >= 0) {
	*ccl++ = *cl;
	for (rcl=xtcallbacks; rcl->callback; rcl++) {
	    if (cl->callback == rcl->callback && cl->closure == rcl->closure) {
		ccl--;
		icl->count--;
		break;
	    }
	}
	cl++;
    }
    if (icl->count) {
	icl = (InternalCallbackList)
	    XtRealloc((char *)icl, (sizeof(InternalCallbackRec) +
				    sizeof(XtCallbackRec) * icl->count));
	icl->is_padded = 0;
	*callbacks = icl;
    } else {
	XtFree((char *)icl);
	*callbacks = NULL;
    }
} /* XtRemoveCallbacks */


void _XtRemoveAllCallbacks (callbacks)
    InternalCallbackList *callbacks;
{
    register InternalCallbackList icl = *callbacks;

    if (icl) {
	if (icl->call_state)
	    icl->call_state |= _XtCBFreeAfterCalling;
	else
	    XtFree((char *) icl);
	*callbacks = NULL;
    }
} /* _XtRemoveAllCallbacks */

#if NeedFunctionPrototypes
void XtRemoveAllCallbacks(widget, name)
    Widget widget;
    _Xconst char* name;
#else
void XtRemoveAllCallbacks(widget, name)
    Widget widget;
    String name;
#endif
{
    InternalCallbackList *callbacks;

    callbacks = FetchInternalList(widget, name);
    if (!callbacks) {
       XtAppWarningMsg(XtWidgetToApplicationContext(widget),
	       XtNinvalidCallbackList,XtNxtRemoveAllCallback,XtCXtToolkitError,
              "Cannot find callback list in XtRemoveAllCallbacks",
	      (String *)NULL, (Cardinal *)NULL);

	return;
    }
    _XtRemoveAllCallbacks(callbacks);
} /* XtRemoveAllCallbacks */

InternalCallbackList _XtCompileCallbackList(xtcallbacks)
    XtCallbackList xtcallbacks;
{
    register int n;
    register XtCallbackList xtcl, cl;
    register InternalCallbackList callbacks;

    for (n=0, xtcl=xtcallbacks; xtcl->callback; n++, xtcl++) {};
    if (n == 0) return (InternalCallbackList) NULL;

    callbacks = (InternalCallbackList) XtMalloc(sizeof(InternalCallbackRec) +
						sizeof(XtCallbackRec) * n);
    callbacks->count = n;
    callbacks->is_padded = 0;
    callbacks->call_state = 0;
    cl = ToList(callbacks);
    while (--n >= 0)
	*cl++ = *xtcallbacks++;
    return(callbacks);
} /* _XtCompileCallbackList */


XtCallbackList _XtGetCallbackList(callbacks)
    InternalCallbackList *callbacks;
{
    register int i;
    register InternalCallbackList icl;
    register XtCallbackList cl, ocl;

    icl = *callbacks;
    if (!icl) {
	static XtCallbackRec emptyList[1] = { {NULL, NULL} };
	return (XtCallbackList)emptyList;
    }
    if (icl->is_padded)
	return ToList(icl);
    i = icl->count;
    if (icl->call_state) {
	icl->call_state |= _XtCBFreeAfterCalling;
	ocl = ToList(icl);
	icl = (InternalCallbackList) XtMalloc(sizeof(InternalCallbackRec) +
					      sizeof(XtCallbackRec) * (i+1));
	icl->count = i;
	icl->call_state = 0;
	cl = ToList(icl);
	while (--i >= 0)
	    *cl++ = *ocl++;
    } else {
	icl = (InternalCallbackList) XtRealloc((char *)icl,
					       sizeof(InternalCallbackRec) +
					       sizeof(XtCallbackRec) * (i+1));
	cl = ToList(icl) + i;
    }
    icl->is_padded = 1;
    cl->callback = (XtCallbackProc) NULL;
    cl->closure = NULL;
    *callbacks = icl;
    return ToList(icl);
}

#if NeedFunctionPrototypes
void XtCallCallbacks(
    Widget   widget,
    _Xconst char* name,
    XtPointer call_data
    )
#else
void XtCallCallbacks(widget, name, call_data)
    Widget   widget;
    String   name;
    XtPointer call_data;
#endif
{
    InternalCallbackList *callbacks;
    register InternalCallbackList icl;
    register XtCallbackList cl;
    register int i;
    char ostate;

    callbacks = FetchInternalList(widget, name);
    if (!callbacks) {
       XtAppWarningMsg(XtWidgetToApplicationContext(widget),
	       XtNinvalidCallbackList,XtNxtCallCallback,XtCXtToolkitError,
              "Cannot find callback list in XtCallCallbacks",
	      (String *)NULL, (Cardinal *)NULL);
	return;
    }

    icl = *callbacks;
    if (!icl) return;
    cl = ToList(icl);
    if (icl->count == 1) {
	(*cl->callback) (widget, cl->closure, call_data);
	return;
    }
    ostate = icl->call_state;
    icl->call_state = _XtCBCalling;
    for (i = icl->count; --i >= 0; cl++)
	(*cl->callback) (widget, cl->closure, call_data);
    if (ostate)
	icl->call_state |= ostate;
    else if (icl->call_state & _XtCBFreeAfterCalling)
	XtFree((char *)icl);
    else
	icl->call_state = ostate;
} /* XtCallCallbacks */


#if NeedFunctionPrototypes
XtCallbackStatus XtHasCallbacks(
     Widget		widget,
     _Xconst char*	callback_name
     )
#else
XtCallbackStatus XtHasCallbacks(widget, callback_name)
     Widget		widget;
     String		callback_name;
#endif
{
    InternalCallbackList *callbacks;
    callbacks = FetchInternalList(widget, callback_name);
    if (!callbacks)
	return XtCallbackNoList;
    else if (!*callbacks)
	return XtCallbackHasNone;
    return XtCallbackHasSome;
} /* XtHasCallbacks */


void XtCallCallbackList(widget, callbacks, call_data)
    Widget widget;
    XtCallbackList callbacks;
    XtPointer call_data;
{
    register InternalCallbackList icl;
    register XtCallbackList cl;
    register int i;
    char ostate;

    if (!callbacks) return;
    icl = (InternalCallbackList)callbacks;
    cl = ToList(icl);
    if (icl->count == 1) {
	(*cl->callback) (widget, cl->closure, call_data);
	return;
    }
    ostate = icl->call_state;
    icl->call_state = _XtCBCalling;
    for (i = icl->count; --i >= 0; cl++)
	(*cl->callback) (widget, cl->closure, call_data);
    if (ostate)
	icl->call_state |= ostate;
    else if (icl->call_state & _XtCBFreeAfterCalling)
	XtFree((char *)icl);
    else
	icl->call_state = 0;
} /* XtCallCallbackList */
