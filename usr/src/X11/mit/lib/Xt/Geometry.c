/* $XConsortium: Geometry.c,v 1.55 92/02/11 17:13:18 converse Exp $ */

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
#include "ShellP.h"

static void ClearRectObjAreas(r, old)
    RectObj r;
    XWindowChanges* old;
{
    Widget pw = _XtWindowedAncestor((Widget)r);
    int bw2;

    bw2 = old->border_width << 1;
    XClearArea( XtDisplay(pw), XtWindow(pw),
		old->x, old->y,
		old->width + bw2, old->height + bw2,
		TRUE );

    bw2 = r->rectangle.border_width << 1;
    XClearArea( XtDisplay(pw), XtWindow(pw),
		(int)r->rectangle.x, (int)r->rectangle.y,
		(unsigned int)(r->rectangle.width + bw2),
	        (unsigned int)(r->rectangle.height + bw2),
		TRUE );
}

/*
 * Internal function used by XtMakeGeometryRequest and XtSetValues.
 * Returns more data than the public interface.  Does not convert
 * XtGeometryDone to XtGeometryYes.
 *
 * clear_rect_obj - *** RETURNED ***  
 *		    TRUE if the rect obj has been cleared, false otherwise.
 */

XtGeometryResult 
_XtMakeGeometryRequest (widget, request, reply, clear_rect_obj)
    Widget widget;
    XtWidgetGeometry *request, *reply;
    Boolean * clear_rect_obj;
{
    XtWidgetGeometry    junk;
    XtGeometryHandler manager;
    XtGeometryResult returnCode;
    Widget parent = widget->core.parent;
    XtGeometryMask	changeMask;
    Boolean managed, parentRealized, rgm = False;
    XWindowChanges changes;

    *clear_rect_obj = FALSE;

    if (XtIsShell(widget)) {
	ShellClassExtension ext;
	for (ext = (ShellClassExtension)((ShellWidgetClass)XtClass(widget))
		   ->shell_class.extension;
	     ext != NULL && ext->record_type != NULLQUARK;
	     ext = (ShellClassExtension)ext->next_extension);

	if (ext != NULL) {
	    if (  ext->version == XtShellExtensionVersion
		  && ext->record_size == sizeof(ShellClassExtensionRec)) {
		manager = ext->root_geometry_manager;
		rgm = True;
	    } else {
		String params[1];
		Cardinal num_params = 1;
		params[0] = XtClass(widget)->core_class.class_name;
		XtAppErrorMsg(XtWidgetToApplicationContext(widget),
		     "invalidExtension", "xtMakeGeometryRequest",
		     XtCXtToolkitError,
		     "widget class %s has invalid ShellClassExtension record",
		     params, &num_params);
	    }
	} else {
	    XtAppErrorMsg(XtWidgetToApplicationContext(widget),
			  "internalError", "xtMakeGeometryRequest",
			  XtCXtToolkitError,
			  "internal error; ShellClassExtension is NULL",
			  NULL, NULL);
	}
	managed = True;
	parentRealized = TRUE;
    } else if (parent == NULL) {
	XtAppErrorMsg(XtWidgetToApplicationContext(widget),
		      "invalidParent","xtMakeGeometryRequest",XtCXtToolkitError,
		      "non-shell has no parent in XtMakeGeometryRequest",
		      (String *)NULL, (Cardinal *)NULL);
    } else /* not shell */ {
	managed = XtIsManaged(widget);

	if (XtIsComposite(parent)) {
	    parentRealized = XtIsRealized(parent);
	    manager = ((CompositeWidgetClass) (parent->core.widget_class))
		    ->composite_class.geometry_manager;
	} else if (managed) {
	    /* Should never happen - XtManageChildren should have checked */
	    XtAppErrorMsg(XtWidgetToApplicationContext(widget),
			  "invalidParent", "xtMakeGeometryRequest",
			  XtCXtToolkitError,
			  "XtMakeGeometryRequest - parent not composite",
			  (String *)NULL, (Cardinal *)NULL);
	} else {
	    /* no need to waste time checking if parent is actually realized
	     * at this point; since the child is unmanaged we need to perform
	     * the configure iff the child is realized, so we dummy the
	     * parentRealized checks below.
	     */
	    parentRealized = TRUE;
	}
    }

    if (managed && manager == (XtGeometryHandler) NULL) {
	XtErrorMsg("invalidGeometryManager","xtMakeGeometryRequest",
                 XtCXtToolkitError,
                 "XtMakeGeometryRequest - parent has no geometry manager",
                  (String *)NULL, (Cardinal *)NULL);
    }

    if (widget->core.being_destroyed) return XtGeometryNo;

    /* see if requesting anything to change */
    changeMask = 0;
    if (request->request_mode & CWStackMode
	&& request->stack_mode != XtSMDontChange) {
	    changeMask |= CWStackMode;
	    if (request->request_mode & CWSibling) {
		XtCheckSubclass(request->sibling, rectObjClass,
				"XtMakeGeometryRequest");
		changeMask |= CWSibling;
	    }
    }
    if (request->request_mode & CWX
	&& widget->core.x != request->x) changeMask |= CWX;
    if (request->request_mode & CWY
	&& widget->core.y != request->y) changeMask |= CWY;
    if (request->request_mode & CWWidth
	&& widget->core.width != request->width) changeMask |= CWWidth;
    if (request->request_mode & CWHeight
	&& widget->core.height != request->height) changeMask |= CWHeight;
    if (request->request_mode & CWBorderWidth
	&& widget->core.border_width != request->border_width)
	changeMask |= CWBorderWidth;
    if (! changeMask) return XtGeometryYes;
    changeMask |= (request->request_mode & XtCWQueryOnly);

    if ( !(changeMask & XtCWQueryOnly) && XtIsRealized(widget) ) {
	/* keep record of the current geometry so we know what's changed */
	changes.x = widget->core.x ;
	changes.y = widget->core.y ;
	changes.width = widget->core.width ;
	changes.height = widget->core.height ;
	changes.border_width = widget->core.border_width ;
    }

    if (!managed || !parentRealized) {
	/* Don't get parent's manager involved--assume the answer is yes */
	if (changeMask & XtCWQueryOnly) {
	    /* He was just asking, don't change anything, just tell him yes */
	    return XtGeometryYes;
	} else {
	    /* copy values from request to widget */
	    if (request->request_mode & CWX)
		widget->core.x = request->x;
	    if (request->request_mode & CWY)
		widget->core.y = request->y;
	    if (request->request_mode & CWWidth)
		widget->core.width = request->width;
	    if (request->request_mode & CWHeight)
		widget->core.height = request->height;
	    if (request->request_mode & CWBorderWidth)
		widget->core.border_width = request->border_width;
	    if (!parentRealized) return XtGeometryYes;
	    else returnCode = XtGeometryYes;
	}
    } else {
	/* go ask the widget's geometry manager */
	if (reply == (XtWidgetGeometry *) NULL) {
	    returnCode = (*manager)(widget, request, &junk);
	} else {
	    returnCode = (*manager)(widget, request, reply);
	}
    }

    /*
     * If Unrealized, not a XtGeometryYes, or a query-only then we are done.
     */

    if ((returnCode != XtGeometryYes) || 
	(changeMask & XtCWQueryOnly) || !XtIsRealized(widget)) {

	return returnCode;
    }

    if (XtIsWidget(widget)) {	/* reconfigure the window (if needed) */

	if (rgm) return returnCode;

	if (changes.x != widget->core.x) {
 	    changeMask |= CWX;
 	    changes.x = widget->core.x;
 	}
 	if (changes.y != widget->core.y) {
 	    changeMask |= CWY;
 	    changes.y = widget->core.y;
 	}
 	if (changes.width != widget->core.width) {
 	    changeMask |= CWWidth;
 	    changes.width = widget->core.width;
 	}
 	if (changes.height != widget->core.height) {
 	    changeMask |= CWHeight;
 	    changes.height = widget->core.height;
 	}
 	if (changes.border_width != widget->core.border_width) {
 	    changeMask |= CWBorderWidth;
 	    changes.border_width = widget->core.border_width;
 	}
	if (changeMask & CWStackMode) {
	    changes.stack_mode = request->stack_mode;
	    if (changeMask & CWSibling)
		changes.sibling = XtWindow(request->sibling);
	}

	XConfigureWindow(XtDisplay(widget), XtWindow(widget),
			 changeMask, &changes);
    }
    else {			/* RectObj child of realized Widget */
	*clear_rect_obj = TRUE;
	ClearRectObjAreas((RectObj)widget, &changes);
    }

    return returnCode;
} /* _XtMakeGeometryRequest */


/* Public routines */

XtGeometryResult XtMakeGeometryRequest (widget, request, reply)
    Widget         widget;
    XtWidgetGeometry *request, *reply;
{
    Boolean junk;
    XtGeometryResult returnCode;

    returnCode = _XtMakeGeometryRequest(widget, request, reply, &junk);

    return ((returnCode == XtGeometryDone) ? XtGeometryYes : returnCode);
}

#if NeedFunctionPrototypes
XtGeometryResult XtMakeResizeRequest(
    Widget	widget,
    _XtDimension width,
    _XtDimension height,
    Dimension	*replyWidth,
    Dimension	*replyHeight
    )
#else
XtGeometryResult XtMakeResizeRequest
	(widget, width, height, replyWidth, replyHeight)
    Widget	widget;
    Dimension	width, height;
    Dimension	*replyWidth, *replyHeight;
#endif
{
    XtWidgetGeometry request, reply;
    XtGeometryResult r;

    request.request_mode = CWWidth | CWHeight;
    request.width = width;
    request.height = height;
    r = XtMakeGeometryRequest(widget, &request, &reply);
    if (replyWidth != NULL)
	if (r == XtGeometryAlmost && reply.request_mode & CWWidth)
	    *replyWidth = reply.width;
	else
	    *replyWidth = width;
    if (replyHeight != NULL)
	if (r == XtGeometryAlmost && reply.request_mode & CWHeight)
	    *replyHeight = reply.height;
	else
	    *replyHeight = height;
    return r;
} /* XtMakeResizeRequest */

void XtResizeWindow(w)
    Widget w;
{
    if (XtIsRealized(w)) {
	XWindowChanges changes;
	changes.width = w->core.width;
	changes.height = w->core.height;
	changes.border_width = w->core.border_width;
	XConfigureWindow(XtDisplay(w), XtWindow(w),
	    (unsigned) CWWidth | CWHeight | CWBorderWidth, &changes);
    }
} /* XtResizeWindow */


#if NeedFunctionPrototypes
void XtResizeWidget(
    Widget w,
    _XtDimension width,
    _XtDimension height,
    _XtDimension borderWidth
    )
#else
void XtResizeWidget(w, width, height, borderWidth)
    Widget w;
    Dimension width, height, borderWidth;
#endif
{
    XWindowChanges changes;
    Dimension old_width, old_height, old_borderWidth;
    Cardinal mask = 0;

    if ((old_width = w->core.width) != width) {
	changes.width = w->core.width = width;
	mask |= CWWidth;
    }

    if ((old_height = w->core.height) != height) {
	changes.height = w->core.height = height;
	mask |= CWHeight;
    }

    if ((old_borderWidth = w->core.border_width) != borderWidth) {
	changes.border_width = w->core.border_width = borderWidth;
	mask |= CWBorderWidth;
    }

    if (mask != 0) {
	if (XtIsRealized(w)) {
	    if (XtIsWidget(w))
		XConfigureWindow(XtDisplay(w), XtWindow(w), mask, &changes);
	    else {
		Widget pw = _XtWindowedAncestor(w);
		old_width += (old_borderWidth << 1);
		old_height += (old_borderWidth << 1);
		if ((Dimension)(width + (borderWidth << 1)) > old_width)
		    old_width = width + (borderWidth << 1);
		if ((Dimension)(height + (borderWidth << 1)) > old_height)
		    old_height = height + (borderWidth << 1);
		XClearArea( XtDisplay(pw), XtWindow(pw),
			    (int)w->core.x, (int)w->core.y,
			    (unsigned int)old_width, (unsigned int)old_height,
			    TRUE );
	    }
	}
	if ((mask & (CWWidth | CWHeight)) &&
	      XtClass(w)->core_class.resize != (XtWidgetProc) NULL)
	    (*(w->core.widget_class->core_class.resize))(w);
    }
} /* XtResizeWidget */

#if NeedFunctionPrototypes
void XtConfigureWidget(
    Widget w,
    _XtPosition x,
    _XtPosition y,
    _XtDimension width,
    _XtDimension height,
    _XtDimension borderWidth
    )
#else
void XtConfigureWidget(w, x, y, width, height, borderWidth)
    Widget w;
    Position x, y;
    Dimension width, height, borderWidth;
#endif
{
    XWindowChanges changes, old;
    Cardinal mask = 0;

    if ((old.x = w->core.x) != x) {
	changes.x = w->core.x = x;
	mask |= CWX;
    }

    if ((old.y = w->core.y) != y) {
	changes.y = w->core.y = y;
	mask |= CWY;
    }

    if ((old.width = w->core.width) != width) {
	changes.width = w->core.width = width;
	mask |= CWWidth;
    }

    if ((old.height = w->core.height) != height) {
	changes.height = w->core.height = height;
	mask |= CWHeight;
    }

    if ((old.border_width = w->core.border_width) != borderWidth) {
	changes.border_width = w->core.border_width = borderWidth;
	mask |= CWBorderWidth;
    }

    if (mask != 0) {
	if (XtIsRealized(w)) {
	    if (XtIsWidget(w))
		XConfigureWindow(XtDisplay(w), XtWindow(w), mask, &changes);
	    else
		ClearRectObjAreas((RectObj)w, &old);
	}
	if ((mask & (CWWidth | CWHeight)) &&
	      XtClass(w)->core_class.resize != (XtWidgetProc) NULL)
	    (*(w->core.widget_class->core_class.resize))(w);
    }
} /* XtConfigureWidget */

#if NeedFunctionPrototypes
void XtMoveWidget(
    Widget w,
    _XtPosition x,
    _XtPosition y
    )
#else
void XtMoveWidget(w, x, y)
    Widget w;
    Position x, y;
#endif
{
    if ((w->core.x != x) || (w->core.y != y)) {
	XWindowChanges old;
	old.x = w->core.x;
	old.y = w->core.y;
	w->core.x = x;
	w->core.y = y;
	if (XtIsRealized(w)) {
	    if (XtIsWidget(w))
		XMoveWindow(XtDisplay(w), XtWindow(w), w->core.x, w->core.y);
	    else {
		old.width = w->core.width;
		old.height = w->core.height;
		old.border_width = w->core.border_width;
		ClearRectObjAreas((RectObj)w, &old);
	    }
        }
    }
} /* XtMoveWidget */

#if NeedFunctionPrototypes
void XtTranslateCoords(
    register Widget w,
    _XtPosition x,
    _XtPosition y,
    register Position *rootx,	/* return */
    register Position *rooty	/* return */
    )
#else
void XtTranslateCoords(w, x, y, rootx, rooty)
    register Widget w;
    Position x, y;
    register Position *rootx, *rooty;	/* return */
#endif
{
    Position garbagex, garbagey;
    Widget passed = w;

    if (rootx == NULL) rootx = &garbagex;
    if (rooty == NULL) rooty = &garbagey;

    *rootx = x;
    *rooty = y;

    for (; w != NULL && ! XtIsShell(w); w = w->core.parent) {
	*rootx += w->core.x + w->core.border_width;
	*rooty += w->core.y + w->core.border_width;
    }

    if (w == NULL)
        XtAppWarningMsg(XtWidgetToApplicationContext(passed),
		"invalidShell","xtTranslateCoords",XtCXtToolkitError,
                "Widget has no shell ancestor",
		(String *)NULL, (Cardinal *)NULL);
    else {
	Position x, y;
	extern void _XtShellGetCoordinates();
	_XtShellGetCoordinates( w, &x, &y );
	*rootx += x + w->core.border_width;
	*rooty += y + w->core.border_width;
    }
}

XtGeometryResult XtQueryGeometry(widget, intended, reply)
    Widget widget;
    register XtWidgetGeometry *intended; /* parent's changes; may be NULL */
    XtWidgetGeometry *reply;	/* child's preferred geometry; never NULL */
{
    XtWidgetGeometry null_intended;
    XtGeometryHandler query = XtClass(widget)->core_class.query_geometry;
    XtGeometryResult result;

    reply->request_mode = 0;
    if (query != NULL) {
	if (intended == NULL) {
	    null_intended.request_mode = 0;
	    intended = &null_intended;
	}
	result = (*query) (widget, intended, reply);
    }
    else {
	result = XtGeometryYes;
    }

#define FillIn(mask, field) \
	if (!(reply->request_mode & mask)) reply->field = widget->core.field;

    FillIn(CWX, x);
    FillIn(CWY, y);
    FillIn(CWWidth, width);
    FillIn(CWHeight, height);
    FillIn(CWBorderWidth, border_width);
#undef FillIn

    if (!reply->request_mode & CWStackMode) reply->stack_mode =
XtSMDontChange;

    return result;
  }
