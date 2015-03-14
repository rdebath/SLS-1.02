/* $XConsortium: SelectionI.h,v 1.32 91/05/06 17:21:00 converse Exp $ */

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

#ifndef _XtselectionI_h
#define _XtselectionI_h

#include "Intrinsic.h"

#define BYTELENGTH(length, format) ((length)*((format)>>3))
#define NUMELEM(bytelength, format) ((bytelength) / ((format)>>3))

typedef struct _RequestRec *Request;
typedef struct _SelectRec *Select;

typedef struct _RequestRec {
   Select ctx;		      /* logical owner */
   Widget widget;	      /* widget actually receiving Selection events */
   Window requestor;
   Atom property;
   Atom target;
   Atom type;
   int format;
   XtPointer value;
   unsigned long bytelength;
   int offset;
   XtIntervalId timeout;
   XSelectionRequestEvent event; /* for XtGetSelectionRequest */
   Boolean allSent;
} RequestRec;

typedef struct {
  Atom prop;
  Boolean avail;
} SelectionPropRec, *SelectionProp;

typedef struct {
  Display *dpy;
  Atom incr_atom, indirect_atom, timestamp_atom;
#ifdef DRAFT_ICCCM_COMPATIBILITY
  Atom incremental_atom;	/* X11R2 ICCCM Draft 25 February 1988 */
#endif
  int propCount;
  SelectionProp list;
} PropListRec, *PropList;

typedef struct _SelectRec {
    Atom selection; 			/* constant */
    Display *dpy; 			/* constant */
    Widget widget;
    Time time;
    XtConvertSelectionProc convert;
    XtLoseSelectionProc loses;
    XtSelectionDoneProc notify;
    XtCancelConvertSelectionProc owner_cancel;
    XtPointer owner_closure;
    PropList prop_list;
    Request req;			/* state for local non-incr xfer */
    int ref_count;			/* of active transfers */
    unsigned int incremental:1;
    unsigned int free_when_done:1;
    unsigned int was_disowned:1;
} SelectRec;

typedef struct {
    XtSelectionCallbackProc callback;
    XtPointer *req_closure;
    Atom property;
    Atom *target;
    Atom type;
    int format;
    char *value;
    int bytelength;
    int offset;
    XtIntervalId timeout;
    XtEventHandler proc;
    Widget widget;
    Time time;
    Select ctx;
    Boolean incremental;
} CallBackInfoRec, *CallBackInfo;

typedef struct {
  Atom target;
  Atom property;
} IndirectPair;

#define IndirectPairWordSize 2

typedef struct {
  int active_transfer_count;
} RequestWindowRec;

#define MAX_SELECTION_INCR(dpy) (((65536 < XMaxRequestSize(dpy)) ? \
	(65536 << 2)  : (XMaxRequestSize(dpy) << 2))-100)

#ifdef DRAFT_ICCCM_COMPATIBILITY
#define MATCH_SELECT(event, info) ((event->time == info->time) && \
	    (event->requestor == XtWindow(info->widget)) && \
	    (event->selection == info->ctx->selection) && \
	    ((event->target == *info->target) || \
	     ((event->target == info->ctx->prop_list->incr_atom) && \
	      (event->property == info->property)) || \
	     ((event->target == info->ctx->prop_list->incremental_atom) && \
	      (event->property == info->property))))
#else
#define MATCH_SELECT(event, info) ((event->time == info->time) && \
	    (event->requestor == XtWindow(info->widget)) && \
	    (event->selection == info->ctx->selection) && \
	    (event->target == *info->target))
#endif

#endif /* _XtselectionI_h */
/* DON'T ADD STUFF AFTER THIS #endif */
