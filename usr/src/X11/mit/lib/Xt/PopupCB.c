/* $XConsortium: PopupCB.c,v 1.2 91/01/06 13:32:35 rws Exp $ */

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

/* ARGSUSED */
void XtCallbackNone(widget, closure, call_data)
    Widget  widget;
    XtPointer closure;
    XtPointer call_data;
{
    XtSetSensitive(widget, FALSE);
    _XtPopup((Widget) closure, XtGrabNone, FALSE);
} /* XtCallbackNone */

/* ARGSUSED */
void XtCallbackNonexclusive(widget, closure, call_data)
    Widget  widget;
    XtPointer closure;
    XtPointer call_data;
{

    XtSetSensitive(widget, FALSE);
    _XtPopup((Widget) closure, XtGrabNonexclusive, FALSE);
} /* XtCallbackNonexclusive */

/* ARGSUSED */
void XtCallbackExclusive(widget, closure, call_data)
    Widget  widget;
    XtPointer closure;
    XtPointer call_data;
{
    XtSetSensitive(widget, FALSE);
    _XtPopup((Widget) closure, XtGrabExclusive, FALSE);
} /* XtCallbackExclusive */
