/* $XConsortium: Event.c,v 1.136 92/02/21 15:54:06 converse Exp $ */

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
#include "Shell.h"
#include "StringDefs.h"

#if __STDC__
#define Const const
#else
#define Const /**/
#endif

#define NonMaskableMask ((EventMask)0x80000000L)

/*
 * These are definitions to make the code that handles exposure compresssion
 * easier to read.
 *
 * COMP_EXPOSE      - The compression exposure field of "widget"
 * COMP_EXPOSE_TYPE - The type of compression (lower 4 bits of COMP_EXPOSE.
 * GRAPHICS_EXPOSE  - TRUE if the widget wants graphics expose events
 *                    dispatched.
 * NO_EXPOSE        - TRUE if the widget wants No expose events dispatched.
 */

#define COMP_EXPOSE   (widget->core.widget_class->core_class.compress_exposure)
#define COMP_EXPOSE_TYPE (COMP_EXPOSE & 0x0f)
#define GRAPHICS_EXPOSE  ((XtExposeGraphicsExpose & COMP_EXPOSE) || \
			  (XtExposeGraphicsExposeMerged & COMP_EXPOSE))
#define NO_EXPOSE        (XtExposeNoExpose & COMP_EXPOSE)

EventMask XtBuildEventMask(widget)
    Widget widget;
{
    XtEventTable ev;
    EventMask	mask = 0L;

    for (ev = widget->core.event_table; ev != NULL; ev = ev->next)
	if (ev->select) mask |= ev->mask;
    if (widget->core.widget_class->core_class.expose != NULL)
	mask |= ExposureMask;
    if (widget->core.widget_class->core_class.visible_interest) 
	mask |= VisibilityChangeMask;
    if (widget->core.tm.translations)
	mask |= widget->core.tm.translations->eventMask;

    return mask & ~NonMaskableMask;
}

static void
RemoveEventHandler(widget, eventMask, other, proc, closure, raw)
Widget	        widget;
EventMask       eventMask;
Boolean	        other;
XtEventHandler  proc;
XtPointer	closure;
Boolean	        raw;
{
    XtEventRec *p, **pp;
    EventMask oldMask = XtBuildEventMask(widget);

    if (raw) raw = 1;
    pp = &widget->core.event_table;
    while ((p = *pp) &&
	   (p->proc != proc || p->closure != closure || p->select == raw))
	pp = &p->next;
    if (!p) return;

    /* un-register it */
    eventMask &= ~NonMaskableMask;
    if (other)
	eventMask |= NonMaskableMask;
    p->mask &= ~eventMask;

    if (!p->mask) {        /* delete it entirely */
        *pp = p->next;
	XtFree((char *)p);	
    }

    /* Reset select mask if realized and not raw. */
    if ( !raw && XtIsRealized(widget) && !widget->core.being_destroyed) {
	EventMask mask = XtBuildEventMask(widget);

	if (oldMask != mask) 
	    XSelectInput(XtDisplay(widget), XtWindow(widget), mask);
    }
}

/*	Function Name: AddEventHandler
 *	Description: An Internal routine that does the actual work of
 *                   adding the event handlers.
 *	Arguments: widget - widget to register an event handler for.
 *                 eventMask - events to mask for.
 *                 other - pass non maskable events to this proceedure.
 *                 proc - proceedure to register.
 *                 closure - data to pass to the event hander.
 *                 position - where to add this event handler.
 *                 force_new_position - If the element is already in the
 *                                      list, this will force it to the 
 *                                      beginning or end depending on position.
 *                 raw - If FALSE call XSelectInput for events in mask.
 *	Returns: none
 */

static void 
AddEventHandler(widget, eventMask, other, proc, 
		closure, position, force_new_position, raw)
Widget	        widget;
EventMask       eventMask;
Boolean         other, force_new_position, raw;
XtEventHandler  proc;
XtPointer	closure;
XtListPosition  position;
{
    register XtEventRec *p, **pp;
    EventMask oldMask;
    
    eventMask &= ~NonMaskableMask;
    if (other)
	eventMask |= NonMaskableMask;
    if (!eventMask) return;
    
    if (XtIsRealized(widget) && !raw) oldMask = XtBuildEventMask(widget);
    
    if (raw) raw = 1;
    pp = &widget->core.event_table;
    while ((p = *pp) &&
	   (p->proc != proc || p->closure != closure || p->select == raw))
	pp = &p->next;

    if (!p) {		                /* New proc to add to list */
	p = XtNew(XtEventRec);
	p->proc = proc;
	p->closure = closure;
	p->mask = eventMask;
	p->select = ! raw;
	
	if (position == XtListHead) {
	    p->next = widget->core.event_table;
	    widget->core.event_table = p;
	} else {
	    *pp = p;
	    p->next = NULL;
	}
    } 
    else {
	if (force_new_position) {
	    *pp = p->next;

	    if (position == XtListHead) {
		p->next = widget->core.event_table;
		widget->core.event_table = p;
	    } else {
	       	/*
		 * Find the last element in the list.
		 */
		while (*pp)
		    pp = &(*pp)->next;
		*pp = p;
		p->next = NULL;
	    }
	}

	/* update existing proc */
	p->mask |= eventMask;
    }

    if (XtIsRealized(widget) && !raw) {
	EventMask mask = XtBuildEventMask(widget);

	if (oldMask != mask) 
	    XSelectInput(XtDisplay(widget), XtWindow(widget), mask);
    }

}

#if NeedFunctionPrototypes
void XtRemoveEventHandler(
    Widget	    widget,
    EventMask       eventMask,
    _XtBoolean	    other,
    XtEventHandler  proc,
    XtPointer	    closure
    )
#else
void XtRemoveEventHandler(widget, eventMask, other, proc, closure)
    Widget	    widget;
    EventMask       eventMask;
    Boolean	    other;
    XtEventHandler  proc;
    XtPointer	    closure;
#endif
{
    RemoveEventHandler(widget, eventMask, other, proc, closure, FALSE);
}

#if NeedFunctionPrototypes
void XtAddEventHandler(
    Widget	    widget,
    EventMask       eventMask,
    _XtBoolean      other,
    XtEventHandler  proc,
    XtPointer	    closure
    )
#else
void XtAddEventHandler(widget, eventMask, other, proc, closure)
    Widget	    widget;
    EventMask       eventMask;
    Boolean         other;
    XtEventHandler  proc;
    XtPointer	    closure;
#endif
{
    AddEventHandler(widget, eventMask, other, 
		    proc, closure, XtListTail, FALSE, FALSE);
}

#if NeedFunctionPrototypes
void XtInsertEventHandler(
    Widget	    widget,
    EventMask       eventMask,
    _XtBoolean      other,
    XtEventHandler  proc,
    XtPointer	    closure,
    XtListPosition  position
    )
#else
void XtInsertEventHandler(widget, eventMask, other, proc, closure, position)
    Widget	    widget;
    EventMask       eventMask;
    Boolean         other;
    XtEventHandler  proc;
    XtPointer	    closure;
    XtListPosition  position;
#endif
{
    AddEventHandler(widget, eventMask, other, 
		    proc, closure, position, TRUE, FALSE);
}

#if NeedFunctionPrototypes
void XtRemoveRawEventHandler(
    Widget	    widget,
    EventMask       eventMask,
    _XtBoolean	    other,
    XtEventHandler  proc,
    XtPointer	    closure
    )
#else
void XtRemoveRawEventHandler(widget, eventMask, other, proc, closure)
    Widget	    widget;
    EventMask       eventMask;
    Boolean	    other;
    XtEventHandler  proc;
    XtPointer	    closure;
#endif
{
    RemoveEventHandler(widget, eventMask, other, proc, closure, TRUE);
}

#if NeedFunctionPrototypes
void XtInsertRawEventHandler(
    Widget	    widget,
    EventMask       eventMask,
    _XtBoolean	    other,
    XtEventHandler  proc,
    XtPointer	    closure,
    XtListPosition  position
    )
#else
void XtInsertRawEventHandler(widget, eventMask, other, proc, closure, position)
    Widget	    widget;
    EventMask       eventMask;
    Boolean	    other;
    XtEventHandler  proc;
    XtPointer	    closure;
    XtListPosition  position;
#endif
{
    AddEventHandler(widget, eventMask, other, 
		    proc, closure, position, TRUE, TRUE);
}

#if NeedFunctionPrototypes
void XtAddRawEventHandler(
    Widget	    widget,
    EventMask       eventMask,
    _XtBoolean      other,
    XtEventHandler  proc,
    XtPointer	    closure
    )
#else
void XtAddRawEventHandler(widget, eventMask, other, proc, closure)
    Widget	    widget;
    EventMask       eventMask;
    Boolean         other;
    XtEventHandler  proc;
    XtPointer	    closure;
#endif
{
    AddEventHandler(widget, eventMask, other, 
		    proc, closure, XtListTail, FALSE, TRUE);
}

typedef struct _WWPair {
    struct _WWPair *next;
    Window window;
    Widget widget;
} *WWPair;

typedef struct _WWTable {
    unsigned int mask;		/* size of hash table - 1 */
    unsigned int rehash;	/* mask - 2 */
    unsigned int occupied;	/* number of occupied entries */
    unsigned int fakes;		/* number occupied by WWfake */
    Widget *entries;		/* the entries */
    WWPair pairs;		/* bogus entries */
} *WWTable;

static Const WidgetRec WWfake;	/* placeholder for deletions */

#define WWHASH(tab,win) ((win) & tab->mask)
#define WWREHASHVAL(tab,win) ((((win) % tab->rehash) + 2) | 1)
#define WWREHASH(tab,idx,rehash) ((idx + rehash) & tab->mask)
#define WWTABLE(display) (_XtGetPerDisplay(display)->WWtable)

static void ExpandWWTable();

void _XtRegisterWindow(window, widget)
    register Window window;
    register Widget widget;
{
    register WWTable tab;
    register int idx, rehash;
    register Widget entry;

    tab = WWTABLE(XtDisplay(widget));
    if (window != XtWindow(widget)) {
	WWPair pair;
	pair = XtNew(struct _WWPair);
	pair->next = tab->pairs;
	pair->window = window;
	pair->widget = widget;
	tab->pairs = pair;
	return;
    }
    if ((tab->occupied + (tab->occupied >> 2)) > tab->mask)
	ExpandWWTable(tab);

    idx = WWHASH(tab, window);
    if ((entry = tab->entries[idx]) && entry != &WWfake) {
	rehash = WWREHASHVAL(tab, window);
	do {
	    idx = WWREHASH(tab, idx, rehash);
	} while ((entry = tab->entries[idx]) && entry != &WWfake);
    }
    if (!entry)
	tab->occupied++;
    else if (entry == &WWfake)
	tab->fakes--;
    tab->entries[idx] = widget;
}

void _XtUnregisterWindow(window, widget)
    register Window window;
    register Widget widget;
{
    register WWTable tab;
    register int idx, rehash;
    register Widget entry;

    tab = WWTABLE(XtDisplay(widget));
    if (window != XtWindow(widget)) {
	WWPair *prev, pair;

	prev = &tab->pairs;
	while ((pair = *prev) && pair->window != window)
	    prev = &pair->next;
	if (pair) {
	    *prev = pair->next;
	    XtFree((char *)pair);
	}
	return;
    }
    idx = WWHASH(tab, window);
    if (entry = tab->entries[idx]) {
	if (entry != widget) {
	    rehash = WWREHASHVAL(tab, window);
	    do {
		idx = WWREHASH(tab, idx, rehash);
		if (!(entry = tab->entries[idx]))
		    return;
	    } while (entry != widget);
	}
	tab->entries[idx] = (Widget)&WWfake;
	tab->fakes++;
    }
}

static void ExpandWWTable(tab)
    register WWTable tab;
{
    unsigned int oldmask;
    register Widget *oldentries, *entries;
    register int oldidx, newidx, rehash;
    register Widget entry;

    oldmask = tab->mask;
    oldentries = tab->entries;
    tab->occupied -= tab->fakes;
    tab->fakes = 0;
    if ((tab->occupied + (tab->occupied >> 2)) > tab->mask) {
	tab->mask = (tab->mask << 1) + 1;
	tab->rehash = tab->mask - 2;
    }
    entries = tab->entries = (Widget *) XtCalloc(tab->mask+1, sizeof(Widget));
    for (oldidx = 0; oldidx <= oldmask; oldidx++) {
	if ((entry = oldentries[oldidx]) && entry != &WWfake) {
	    newidx = WWHASH(tab, XtWindow(entry));
	    if (entries[newidx]) {
		rehash = WWREHASHVAL(tab, XtWindow(entry));
		do {
		    newidx = WWREHASH(tab, newidx, rehash);
		} while (entries[newidx]);
	    }
	    entries[newidx] = entry;
	}
    }
    XtFree((char *)oldentries);
}

Widget XtWindowToWidget(display, window)
    register Display *display;
    register Window window;
{
    register WWTable tab;
    register int idx, rehash;
    register Widget entry;
    WWPair pair;

    if (!window) return NULL;
    tab = WWTABLE(display);
    idx = WWHASH(tab, window);
    if ((entry = tab->entries[idx]) && XtWindow(entry) != window) {
	rehash = WWREHASHVAL(tab, window);
	do {
	    idx = WWREHASH(tab, idx, rehash);
	} while ((entry = tab->entries[idx]) && XtWindow(entry) != window);
    }
    if (entry)
	return entry;
    for (pair = tab->pairs; pair; pair = pair->next) {
	if (pair->window == window)
	    return pair->widget;
    }
    return NULL;
}

void _XtAllocWWTable(pd)
    XtPerDisplay pd;
{
    register WWTable tab;

    tab = (WWTable) XtMalloc(sizeof(struct _WWTable));
    tab->mask = 0x7f;
    tab->rehash = tab->mask - 2;
    tab->entries = (Widget *) XtCalloc(tab->mask+1, sizeof(Widget));
    tab->occupied = 0;
    tab->fakes = 0;
    tab->pairs = NULL;
    pd->WWtable = tab;
}

void _XtFreeWWTable(pd)
    register XtPerDisplay pd;
{
    register WWPair pair, next;

    for (pair = pd->WWtable->pairs; pair; pair = next) {
	next = pair->next;
	XtFree((char *)pair);
    }
    XtFree((char *)pd->WWtable->entries);
    XtFree((char *)pd->WWtable);
}

#define EHMAXSIZE 25 /* do not make whopping big */

static Boolean CallEventHandlers(widget, event, mask)
    Widget     widget;
    XEvent    *event;
    EventMask  mask;
{
    register XtEventRec *p;   
    XtEventHandler *proc;
    XtPointer *closure;
    XtEventHandler procs[EHMAXSIZE];
    XtPointer closures[EHMAXSIZE];
    Boolean cont_to_disp = True;
    int i, numprocs;

    /* Have to copy the procs into an array, because one of them might
     * call XtRemoveEventHandler, which would break our linked list. */

    numprocs = 0;
    for (p=widget->core.event_table; p; p = p->next) {
	if (mask & p->mask)
	    numprocs++;
    }
    if (numprocs > EHMAXSIZE) {
	proc = (XtEventHandler *)XtMalloc(numprocs * (sizeof(XtEventHandler) +
						      sizeof(XtPointer)));
	closure = (XtPointer *)(proc + numprocs);
    } else {
	proc = procs;
	closure = closures;
    }
    numprocs = 0;
    for (p=widget->core.event_table; p; p = p->next) {
	if (mask & p->mask) {
	    proc[numprocs] = p->proc;
	    closure[numprocs] = p->closure;
	    numprocs++;
	}
    }
    for (i = 0; i < numprocs && cont_to_disp; i++)
	(*(proc[i]))(widget, closure[i], event, &cont_to_disp);
    if (numprocs > EHMAXSIZE)
	XtFree((char *)proc);
    return cont_to_disp;
}

static Region nullRegion;
static void CompressExposures();

/* keep this SMALL to avoid blowing stack cache! */
/* because some compilers allocate all local locals on procedure entry */
#define EHSIZE 4

#define XtDidNothing	False
#define XtDidDispatch	True
#define XtDidFilter	2

static Boolean DispatchEvent(event, widget, mask, pd)
    register XEvent    *event;
    Widget    widget;
    EventMask mask;
    XtPerDisplay pd;
{
    register XtEventRec *p;   
    XEvent nextEvent;
    Boolean was_dispatched = XtDidNothing;
    Boolean call_tm = XtDidNothing;
    Boolean cont_to_disp;

    if (XFilterEvent(event, XtWindow(widget)))
	return XtDidFilter;
	
    if ( (mask == ExposureMask) ||
	 ((event->type == NoExpose) && NO_EXPOSE) ||
	 ((event->type == GraphicsExpose) && GRAPHICS_EXPOSE) ) {

	if (widget->core.widget_class->core_class.expose != NULL ) {

	    /* We need to mask off the bits that could contain the information
	     * about whether or not we desire Graphics and NoExpose events.  */

	    if ( (COMP_EXPOSE_TYPE == XtExposeNoCompress) || 
		 (event->type == NoExpose) )

		(*widget->core.widget_class->core_class.expose)
		    (widget, event, (Region)NULL);
	    else {
		CompressExposures(event, widget, pd);
	    }
	    was_dispatched = XtDidDispatch;
	}
    }

    if (mask == EnterWindowMask &&
	    widget->core.widget_class->core_class.compress_enterleave) {
	if (XPending(event->xcrossing.display)) {
	    XPeekEvent(event->xcrossing.display, &nextEvent);
	    if (nextEvent.type == LeaveNotify &&
		  event->xcrossing.window == nextEvent.xcrossing.window &&
		  event->xcrossing.subwindow == nextEvent.xcrossing.subwindow){
		/* skip the enter/leave pair */
		XNextEvent(event->xcrossing.display, &nextEvent);
		return XtDidNothing;
	    }
	}
    }

    if (event->type == MotionNotify &&
	    widget->core.widget_class->core_class.compress_motion) {
	while (XPending(event->xmotion.display)) {
	    XPeekEvent(event->xmotion.display, &nextEvent);
	    if (nextEvent.type == MotionNotify &&
		    event->xmotion.window == nextEvent.xmotion.window &&
		    event->xmotion.subwindow == nextEvent.xmotion.subwindow) {
		/* replace the current event with the next one */
		XNextEvent(event->xmotion.display, event);
	    } else break;
	}
    }

    if ((mask == VisibilityChangeMask) &&
        XtClass(widget)->core_class.visible_interest) {
	    was_dispatched = XtDidDispatch;
	    /* our visibility just changed... */
	    switch (((XVisibilityEvent *)event)->state) {
		case VisibilityUnobscured:
		    widget->core.visible = TRUE;
		    break;

		case VisibilityPartiallyObscured:
		    /* what do we want to say here? */
		    /* well... some of us is visible */
		    widget->core.visible = TRUE;
		    break;

		case VisibilityFullyObscured:
		    widget->core.visible = FALSE;
		    /* do we want to mark our children obscured? */
		    break;
	    }
	}

    /* to maintain "copy" semantics we check TM now but call later */
    if (widget->core.tm.translations &&
	(mask & widget->core.tm.translations->eventMask))
	call_tm = XtDidDispatch;

    cont_to_disp = True;
    p=widget->core.event_table;
    if (p) {
	if (p->next) {
	    XtEventHandler proc[EHSIZE];
	    XtPointer closure[EHSIZE];
	    int numprocs = 0;

	    /* Have to copy the procs into an array, because one of them might
	     * call XtRemoveEventHandler, which would break our linked list. */

	    for (; p; p = p->next) {
		if (mask & p->mask) {
		    if (numprocs >= EHSIZE)
			break;
		    proc[numprocs] = p->proc;
		    closure[numprocs] = p->closure;
		    numprocs++;
		}
	    }
	    if (numprocs) {
		if (p) {
		    cont_to_disp = CallEventHandlers(widget, event, mask);
		} else {
		    int i;
		    for (i = 0; i < numprocs && cont_to_disp; i++)
			(*(proc[i]))(widget, closure[i], event, &cont_to_disp);
		}
		was_dispatched = XtDidDispatch;
	    }
	} else if (mask & p->mask) {
	    (*p->proc)(widget, p->closure, event, &cont_to_disp);
	    was_dispatched = XtDidDispatch;
	}
    }
    if (call_tm && cont_to_disp)
	_XtTranslateEvent(widget, event);
    return (was_dispatched|call_tm);
}

/*
 * This structure is passed into the check exposure proc.
 */

typedef struct _CheckExposeInfo {
    int type1, type2;		/* Types of events to check for. */
    Boolean maximal;		/* Ignore non-exposure events? */
    Boolean non_matching;	/* Was there an event that did not 
				   match eighter type? */
    Window window;		/* Window to match. */
} CheckExposeInfo;

#define GetCount(ev) ( ((ev)->type == GraphicsExpose) ? \
		       (ev)->xgraphicsexpose.count : (ev)->xexpose.count)

/*	Function Name: CompressExposures
 *	Description: Handles all exposure compression
 *	Arguments: event - the xevent that is to be dispatched
 *                 widget - the widget that this event occured in.
 *	Returns: none.
 *
 *      NOTE: Event must be of type Expose or GraphicsExpose.
 */

static void SendExposureEvent();
static Bool CheckExposureEvent();

static void
CompressExposures(event, widget, pd)
Widget widget;
XEvent * event;
XtPerDisplay pd;
{
    CheckExposeInfo info;
    int count;

    XtAddExposureToRegion(event, pd->region);

    if ( GetCount(event) != 0 )
	return;

    if ( (COMP_EXPOSE_TYPE == XtExposeCompressSeries) ||
	 (XEventsQueued(XtDisplay(widget), QueuedAfterReading) == 0) ) {
	SendExposureEvent(event, widget, pd);
	return;
    }

    if (COMP_EXPOSE & XtExposeGraphicsExposeMerged) {
	info.type1 = Expose;
	info.type2 = GraphicsExpose;
    }
    else {
	info.type1 = event->type;
	info.type2 = 0;
    }
    info.maximal =(COMP_EXPOSE_TYPE == XtExposeCompressMaximal) ? True : False;
    info.non_matching = FALSE;
    info.window = XtWindow(widget);

/*
 * We have to be very careful here not to hose down the processor
 * when blocking until count gets to zero.
 *
 * First, check to see if there are any events in the queue for this
 * widget, and of the correct type.
 *
 * Once we cannot find any more events, check to see that count is zero. 
 * If it is not then block until we get another exposure event.
 *
 * If we find no more events, and count on the last one we saw was zero we
 * we can be sure that all events have been processed.
 *
 * Unfortunately, we wind up having to look at the entire queue
 * event if we're not doing Maximal compression, due to the
 * semantics of XCheckIfEvent (we can't abort without re-ordering
 * the event queue as a side-effect).
 */

    count = 0;
    while (TRUE) {
	XEvent event_return;

	if (XCheckIfEvent(XtDisplay(widget), &event_return, 
			  CheckExposureEvent, (char *) &info)) {

	    count = GetCount(&event_return);
	    XtAddExposureToRegion(&event_return, pd->region);
	}
	else if (count != 0) {
	    XIfEvent(XtDisplay(widget), &event_return,
		     CheckExposureEvent, (char *) &info);
	    count = GetCount(&event_return);
	    XtAddExposureToRegion(&event_return, pd->region);
	}
	else /* count == 0 && XCheckIfEvent Failed. */
	    break;
    }

    SendExposureEvent(event, widget, pd);
}

/*	Function Name: SendExposureEvent
 *	Description: Sets the x, y, width, and height of the event
 *                   to be the clip box of Expose Region.
 *	Arguments: event  - the X Event to mangle; Expose or GraphicsExpose.
 *                 widget - the widget that this event occured in.
 *                 pd     - the per display information for this widget.
 *	Returns: none.
 */

static void
SendExposureEvent(event, widget, pd)
XEvent * event;
Widget widget;
XtPerDisplay pd;
{
    XtExposeProc expose = widget->core.widget_class->core_class.expose;
    XRectangle rect;

    XClipBox(pd->region, &rect);
    if (event->type == Expose) {
	event->xexpose.x = rect.x;
	event->xexpose.y = rect.y;
	event->xexpose.width = rect.width;
	event->xexpose.height = rect.height;
    }
    else {			/* Graphics Expose Event. */
	event->xgraphicsexpose.x = rect.x;
	event->xgraphicsexpose.y = rect.y;
	event->xgraphicsexpose.width = rect.width;
	event->xgraphicsexpose.height = rect.height;
    }
    (*expose)(widget, event, pd->region);
    (void) XIntersectRegion(nullRegion, pd->region, pd->region);
}

/*	Function Name: CheckExposureEvent
 *	Description: Checks the event cue for an expose event
 *	Arguments: display - the display connection.
 *                 event - the event to check.
 *                 arg - a pointer to the exposure info structure.
 *	Returns: TRUE if we found an event of the correct type
 *               with the right window.
 *
 * NOTE: The only valid types (info.type1 and info.type2) are Expose
 *       and GraphicsExpose.
 */

/* ARGSUSED */
static Bool
CheckExposureEvent(disp, event, arg)
Display * disp;
XEvent * event;
char * arg;
{
    CheckExposeInfo * info = ((CheckExposeInfo *) arg);

    if ( (info->type1 == event->type) || (info->type2 == event->type)) {
	if (!info->maximal && info->non_matching) return FALSE;
	if (event->type == GraphicsExpose)
	    return(event->xgraphicsexpose.drawable == info->window);
	return(event->xexpose.window == info->window);
    }
    info->non_matching = TRUE;
    return(FALSE);
}

static EventMask Const masks[] = {
	0,			    /* Error, should never see  */
	0,			    /* Reply, should never see  */
	KeyPressMask,		    /* KeyPress			*/
	KeyReleaseMask,		    /* KeyRelease		*/
	ButtonPressMask,	    /* ButtonPress		*/
	ButtonReleaseMask,	    /* ButtonRelease		*/
	PointerMotionMask	    /* MotionNotify		*/
		| ButtonMotionMask,
	EnterWindowMask,	    /* EnterNotify		*/
	LeaveWindowMask,	    /* LeaveNotify		*/
	FocusChangeMask,	    /* FocusIn			*/
	FocusChangeMask,	    /* FocusOut			*/
	KeymapStateMask,	    /* KeymapNotify		*/
	ExposureMask,		    /* Expose			*/
	NonMaskableMask,	    /* GraphicsExpose, in GC    */
	NonMaskableMask,	    /* NoExpose, in GC		*/
	VisibilityChangeMask,       /* VisibilityNotify		*/
	SubstructureNotifyMask,     /* CreateNotify		*/
	StructureNotifyMask	    /* DestroyNotify		*/
		| SubstructureNotifyMask,
	StructureNotifyMask	    /* UnmapNotify		*/
		| SubstructureNotifyMask,
	StructureNotifyMask	    /* MapNotify		*/
		| SubstructureNotifyMask,
	SubstructureRedirectMask,   /* MapRequest		*/
	StructureNotifyMask	    /* ReparentNotify		*/
		| SubstructureNotifyMask,
	StructureNotifyMask	    /* ConfigureNotify		*/
		| SubstructureNotifyMask,
	SubstructureRedirectMask,   /* ConfigureRequest		*/
	StructureNotifyMask	    /* GravityNotify		*/
		| SubstructureNotifyMask,
	ResizeRedirectMask,	    /* ResizeRequest		*/
	StructureNotifyMask	    /* CirculateNotify		*/
		| SubstructureNotifyMask,
	SubstructureRedirectMask,   /* CirculateRequest		*/
	PropertyChangeMask,	    /* PropertyNotify		*/
	NonMaskableMask,	    /* SelectionClear		*/
	NonMaskableMask,	    /* SelectionRequest		*/
	NonMaskableMask,	    /* SelectionNotify		*/
	ColormapChangeMask,	    /* ColormapNotify		*/
	NonMaskableMask,	    /* ClientMessage		*/
	NonMaskableMask		    /* MappingNotify		*/
};

EventMask _XtConvertTypeToMask (eventType)
    int		eventType;
{
    eventType &= 0x7f;	/* Events sent with XSendEvent have high bit set. */
    if (eventType < XtNumber(masks))
	return masks[eventType];
    else
	return 0;
}

Boolean _XtOnGrabList(widget, grabList)
    register Widget widget;
    XtGrabRec  *grabList;
{
    register XtGrabRec* gl;
    for (; widget != NULL; widget = (Widget)widget->core.parent) {
	for (gl = grabList; gl != NULL; gl = gl->next) {
	    if (gl->widget == widget) return TRUE;
	    if (gl->exclusive) break;
	}
    }
    return FALSE;
}

static Widget LookupSpringLoaded(grabList)
    XtGrabList	grabList;
{
    XtGrabList	gl;

    for (gl = grabList; gl != NULL; gl = gl->next) {
	if (gl->spring_loaded)
	  if (XtIsSensitive(gl->widget))
	    return gl->widget;
	  else
	    return NULL;
	if (gl->exclusive) break;
    }
    return NULL;
}

typedef enum _GrabType {pass, ignore, remap} GrabType;

static Boolean DecideToDispatch(event)
    XEvent  *event;
{
    register    Widget widget;
    EventMask   mask;
    GrabType    grabType;
    Widget	dspWidget;
    Time	time = 0;
    XtPerDisplay pd;
    XtPerDisplayInput pdi;
    XtGrabList  grabList;

    widget = XtWindowToWidget (event->xany.display, event->xany.window);
    pd = _XtGetPerDisplay(event->xany.display);
    pdi = _XtGetPerDisplayInput(event->xany.display);
    grabList = *_XtGetGrabList(pdi);
    
    mask = _XtConvertTypeToMask(event->xany.type);

    grabType = pass;
    switch (event->xany.type & 0x7f) {

      case KeyPress:
      case KeyRelease:		grabType = remap; time = event->xkey.time;
				break;

      case ButtonPress:
      case ButtonRelease:	grabType = remap; time = event->xbutton.time;
				break;

      case MotionNotify:	grabType = ignore; time = event->xmotion.time;
#define XKnownButtons (Button1MotionMask|Button2MotionMask|Button3MotionMask|\
                       Button4MotionMask|Button5MotionMask)
	                        mask |= (event->xmotion.state & XKnownButtons);
#undef XKnownButtons
				break;

      case EnterNotify:
				grabType = ignore; /* fall through */
      case LeaveNotify:		time = event->xcrossing.time; break;

      case PropertyNotify:	time = event->xproperty.time; break;

      case SelectionClear:	time = event->xselectionclear.time; break;
    }

    if (time) pd->last_timestamp = time;

    if (widget == NULL) {
	if (grabType != remap)
	    return XFilterEvent(event, None);
	/* event occurred in a non-widget window, but we've promised also
	   to dispatch it to the nearest accessible spring_loaded widget */
	else if ((widget = LookupSpringLoaded(grabList)) != NULL)
	    return DispatchEvent(event, widget, mask, pd);
	return XFilterEvent(event, None);
    }

    switch(grabType) {
	case pass:
	    return DispatchEvent(event, widget, mask, pd);

	case ignore:
	    if ((grabList == NULL || _XtOnGrabList(widget,grabList))
		&& XtIsSensitive(widget)) {
		return DispatchEvent(event, widget, mask, pd);
	    }
	    return XtDidNothing;

	case remap:
	    
	    {
		Boolean was_dispatched = XtDidNothing;
		extern Widget _XtFindRemapWidget();
		extern void _XtUngrabBadGrabs();

		dspWidget = _XtFindRemapWidget(event, widget, mask, pdi);
		
		if ((grabList == NULL || 
		     _XtOnGrabList(dspWidget, grabList)) &&
		    XtIsSensitive(dspWidget)) {
		    was_dispatched = DispatchEvent(event, dspWidget,
						   mask, pd);
		    if (was_dispatched & XtDidFilter)
			return was_dispatched;
		}
		else _XtUngrabBadGrabs(event, widget, mask, pdi);
		
		/* Also dispatch to nearest accessible spring_loaded. */
		/* Fetch this afterward to reflect modal list changes */
		grabList = *_XtGetGrabList(pdi);
		widget = LookupSpringLoaded(grabList);
		if (widget != NULL && widget != dspWidget) {
		    was_dispatched |= DispatchEvent(event, widget,
						    mask, pd);
		}
		
		return was_dispatched;
	    }
    }
    /* should never reach here */
    return XtDidNothing;
}

Boolean XtDispatchEvent (event)
    XEvent  *event;
{
    Boolean was_dispatched;
    XtAppContext app = XtDisplayToApplicationContext(event->xany.display);
    int dispatch_level = ++app->dispatch_level;
    int starting_count = app->destroy_count;
    void _XtRefreshMapping();

    if (event->xany.type == MappingNotify)
	_XtRefreshMapping(event, True);

    /*
     * To make recursive XtDispatchEvent work, we need to do phase 2 destroys
     * only on those widgets destroyed by this particular dispatch.
     *
     */

    was_dispatched = DecideToDispatch(event);

    if (app->destroy_count > starting_count)
	_XtDoPhase2Destroy(app, dispatch_level);

    app->dispatch_level = dispatch_level - 1;

    if (_XtSafeToDestroy(app)) {
	if (_XtAppDestroyCount != 0) _XtDestroyAppContexts();
	if (_XtDpyDestroyCount != 0) _XtCloseDisplays();
    }
    
    return (was_dispatched != XtDidNothing);
}

/* ARGSUSED */
static void GrabDestroyCallback(widget, closure, call_data)
    Widget  widget;
    XtPointer closure;
    XtPointer call_data;
{
    /* Remove widget from grab list if it destroyed */
    XtRemoveGrab(widget);
}

static XtGrabRec *NewGrabRec(widget, exclusive, spring_loaded)
    Widget  widget;
    Boolean exclusive;
    Boolean spring_loaded;
{
    register XtGrabList    gl;

    gl		      = XtNew(XtGrabRec);
    gl->next	      = NULL;
    gl->widget        = widget;
    gl->exclusive     = exclusive;
    gl->spring_loaded = spring_loaded;

    return gl;
}

#if NeedFunctionPrototypes
void XtAddGrab(
    Widget  widget,
    _XtBoolean exclusive,
    _XtBoolean spring_loaded
    )
#else
void XtAddGrab(widget, exclusive, spring_loaded)
    Widget  widget;
    Boolean exclusive;
    Boolean spring_loaded;
#endif
{
    register    XtGrabList gl;
    XtGrabList	*grabListPtr;

    grabListPtr = 
      _XtGetGrabList(_XtGetPerDisplayInput(XtDisplay(widget)));
      
					 

    if (spring_loaded && !exclusive) {
	XtAppWarningMsg(XtWidgetToApplicationContext(widget),
		"grabError", "xtAddGrab", XtCXtToolkitError,
		"XtAddGrab requires exclusive grab if spring_loaded is TRUE",
		(String *) NULL, (Cardinal *) NULL);
	exclusive = TRUE;
    }

    gl = NewGrabRec(widget, exclusive, spring_loaded);
    gl->next = *grabListPtr;
    *grabListPtr = gl;

    XtAddCallback (widget, XtNdestroyCallback, 
	    GrabDestroyCallback, (XtPointer) NULL);
}

void XtRemoveGrab(widget)
    Widget  widget;
{
    register XtGrabList gl;
    register Boolean done;
    XtGrabList	*grabListPtr;

    grabListPtr = 
      _XtGetGrabList(_XtGetPerDisplayInput(XtDisplay(widget)));
      
    for (gl = *grabListPtr; gl != NULL; gl = gl->next) {
	if (gl->widget == widget) break;
    }

    if (gl == NULL) {
	    XtAppWarningMsg(XtWidgetToApplicationContext(widget),
		       "grabError","xtRemoveGrab",XtCXtToolkitError,
		       "XtRemoveGrab asked to remove a widget not on the list",
		       (String *)NULL, (Cardinal *)NULL);
	    return;
	}	

    do {
	gl = *grabListPtr;
	done = (gl->widget == widget);
	*grabListPtr = gl->next;
	XtRemoveCallback(gl->widget, XtNdestroyCallback,
		GrabDestroyCallback, (XtPointer)NULL);
	XtFree((char *)gl);
    } while (! done);
    return;
}

void XtMainLoop()
{
	XtAppMainLoop(_XtDefaultAppContext());
}

void XtAppMainLoop(app)
	XtAppContext app;
{
    XEvent event;

    for (;;) {
    	XtAppNextEvent(app, &event);
	XtDispatchEvent(&event);
    }
}


void _XtEventInitialize()
{
    static Boolean initialized = FALSE;
    if (initialized) return;
    initialized = TRUE;

    nullRegion = XCreateRegion();
}

void XtAddExposureToRegion(event, region)
    XEvent   *event;
    Region   region;
{
    XRectangle rect;

    switch (event->type) {
	case Expose:
		rect.x = event->xexpose.x;
		rect.y = event->xexpose.y;
		rect.width = event->xexpose.width;
		rect.height = event->xexpose.height;
		break;
	case GraphicsExpose:
		rect.x = event->xgraphicsexpose.x;
		rect.y = event->xgraphicsexpose.y;
		rect.width = event->xgraphicsexpose.width;
		rect.height = event->xgraphicsexpose.height;
		break;
	default:
		return;
    }

    XUnionRectWithRegion(&rect, region, region);
}


void _XtFreeEventTable(event_table)
    XtEventTable *event_table;
{
    register XtEventTable event;

    event = *event_table;
    while (event != NULL) {
	register XtEventTable next = event->next;
	XtFree((char *) event);
	event = next;
    }
}

Time XtLastTimestampProcessed(dpy)
    Display *dpy;
{
    return _XtGetPerDisplay(dpy)->last_timestamp;
}
      

void _XtSendFocusEvent(child, type)
    Widget child;
    int type;
{

    child = XtIsWidget(child) ? child : _XtWindowedAncestor(child);
    if (XtIsSensitive(child) && !child->core.being_destroyed
	&& XtIsRealized(child)
	&& (XtBuildEventMask(child) & FocusChangeMask))
    {
	XFocusChangeEvent event;

	event.type = type;
	event.serial = LastKnownRequestProcessed(XtDisplay(child));
	event.send_event = True;
	event.display = XtDisplay(child);
	event.window = XtWindow(child);
	event.mode = NotifyNormal;
	event.detail = NotifyAncestor;
	DispatchEvent((XEvent*)&event, child, _XtConvertTypeToMask(type),
		      _XtGetPerDisplay(XtDisplay(child)));
    }
}
