/* $XConsortium: SetSens.c,v 1.3 91/01/06 13:32:42 rws Exp $ */

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
#include "StringDefs.h"

/*
 *	XtSetSensitive()
 */

static void SetAncestorSensitive(widget, ancestor_sensitive)
    register Widget widget;
    Boolean	    ancestor_sensitive;
{
    Arg			args[1];
    register Cardinal   i;
    register WidgetList children;

    if (widget->core.ancestor_sensitive == ancestor_sensitive) return;

    XtSetArg(args[0], XtNancestorSensitive, ancestor_sensitive);
    XtSetValues(widget, args, XtNumber(args));

    /* If widget's sensitive is TRUE, propagate new ancestor_sensitive to
       children's ancestor_sensitive; else do nothing as children's
       ancestor_sensitive is already FALSE */
    
    if (widget->core.sensitive && XtIsComposite(widget)) {
	children = ((CompositeWidget) widget)->composite.children;
	for (i=0; i < ((CompositeWidget)widget)->composite.num_children; i++) {
	    SetAncestorSensitive (children[i], ancestor_sensitive);
	}
    }
} /* SetAncestorSensitive */


#if NeedFunctionPrototypes
void XtSetSensitive(
    register Widget widget,
    _XtBoolean	    sensitive
    )
#else
void XtSetSensitive(widget, sensitive)
    register Widget widget;
    Boolean	    sensitive;
#endif
{
    Arg			args[1];
    register Cardinal   i;
    register WidgetList children;

    if (widget->core.sensitive == sensitive) return;

    XtSetArg(args[0], XtNsensitive, sensitive);
    XtSetValues(widget, args, XtNumber(args));

    /* If widget's ancestor_sensitive is TRUE, propagate new sensitive to
       children's ancestor_sensitive; else do nothing as children's
       ancestor_sensitive is already FALSE */
    
    if (widget->core.ancestor_sensitive && XtIsComposite (widget)) {
	children = ((CompositeWidget) widget)->composite.children;
	for (i = 0; i < ((CompositeWidget)widget)->composite.num_children; i++){
	    SetAncestorSensitive (children[i], sensitive);
	}
    }
} /* XtSetSensitive */
