/*
 * $XConsortium: mieq.c,v 1.4 91/07/19 23:20:26 keith Exp $
 *
 * Copyright 1990 Massachusetts Institute of Technology
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
 * Author:  Keith Packard, MIT X Consortium
 */

/*
 * mieq.c
 *
 * Machine independent event queue
 *
 */

# define NEED_EVENTS
# include   "X.h"
# include   "Xmd.h"
# include   "Xproto.h"
# include   "misc.h"
# include   "windowstr.h"
# include   "pixmapstr.h"
# include   "inputstr.h"
# include   "mi.h"
# include   "scrnintstr.h"

#define QUEUE_SIZE  256

typedef struct _Event {
    xEvent	event;
    ScreenPtr	pScreen;
} EventRec, *EventPtr;

typedef struct _EventQueue {
    long	head, tail;	    /* long for SetInputCheck */
    CARD32	lastEventTime;	    /* to avoid time running backwards */
    Bool	lastMotion;
    EventRec	events[QUEUE_SIZE]; /* static allocation for signals */
    DevicePtr	pKbd, pPtr;	    /* device pointer, to get funcs */
    ScreenPtr	pEnqueueScreen;	    /* screen events are being delivered to */
    ScreenPtr	pDequeueScreen;	    /* screen events are being dispatched to */
} EventQueueRec, *EventQueuePtr;

static EventQueueRec miEventQueue;

Bool
mieqInit (pKbd, pPtr)
    DevicePtr	pKbd, pPtr;
{
    miEventQueue.head = miEventQueue.tail = 0;
    miEventQueue.lastEventTime = GetTimeInMillis ();
    miEventQueue.pKbd = pKbd;
    miEventQueue.pPtr = pPtr;
    miEventQueue.lastMotion = FALSE;
    miEventQueue.pEnqueueScreen = screenInfo.screens[0];
    miEventQueue.pDequeueScreen = miEventQueue.pEnqueueScreen;
    SetInputCheck (&miEventQueue.head, &miEventQueue.tail);
    return TRUE;
}

/*
 * Must be reentrant with ProcessInputEvents.  Assumption: mieqEnqueue
 * will never be interrupted.  If this is called from both signal
 * handlers and regular code, make sure the signal is suspended when
 * called from regular code.
 */

void
mieqEnqueue (e)
    xEvent	*e;
{
    int	oldtail, newtail, prevtail;
    Bool    isMotion;

    oldtail = miEventQueue.tail;
    isMotion = e->u.u.type == MotionNotify;
    if (isMotion && miEventQueue.lastMotion && oldtail != miEventQueue.head)
    {
	if (oldtail == 0)
	    oldtail = QUEUE_SIZE;
	oldtail = oldtail - 1;
    }
    else
    {
    	newtail = oldtail + 1;
    	if (newtail == QUEUE_SIZE)
	    newtail = 0;
    	/* Toss events which come in late */
    	if (newtail == miEventQueue.head)
	    return;
	miEventQueue.tail = newtail;
    }
    miEventQueue.lastMotion = isMotion;
    miEventQueue.events[oldtail].event = *e;
    /*
     * Make sure that event times don't go backwards - this
     * is "unnecessary", but very useful
     */
    if (e->u.keyButtonPointer.time < miEventQueue.lastEventTime &&
	miEventQueue.lastEventTime - e->u.keyButtonPointer.time < 10000)
    {
	miEventQueue.events[oldtail].event.u.keyButtonPointer.time =
	    miEventQueue.lastEventTime;
    }
    miEventQueue.events[oldtail].pScreen = miEventQueue.pEnqueueScreen;
}

void
mieqSwitchScreen (pScreen)
    ScreenPtr	pScreen;
{
    miEventQueue.pEnqueueScreen = pScreen;
}

/*
 * Call this from ProcessInputEvents()
 */

mieqProcessInputEvents ()
{
    EventRec	*e;
    int		x, y;
    xEvent	xe;

    while (miEventQueue.head != miEventQueue.tail)
    {
	extern int  screenIsSaved;

	if (screenIsSaved == SCREEN_SAVER_ON)
	    SaveScreens (SCREEN_SAVER_OFF, ScreenSaverReset);

	e = &miEventQueue.events[miEventQueue.head];
	/*
	 * Assumption - screen switching can only occur on motion events
	 */
	if (e->pScreen != miEventQueue.pDequeueScreen)
	{
	    miEventQueue.pDequeueScreen = e->pScreen;
	    x = e->event.u.keyButtonPointer.rootX;
	    y = e->event.u.keyButtonPointer.rootY;
	    if (miEventQueue.head == QUEUE_SIZE - 1)
	    	miEventQueue.head = 0;
	    else
	    	++miEventQueue.head;
	    NewCurrentScreen (miEventQueue.pDequeueScreen, x, y);
	}
	else
	{
	    xe = e->event;
	    if (miEventQueue.head == QUEUE_SIZE - 1)
	    	miEventQueue.head = 0;
	    else
	    	++miEventQueue.head;
	    switch (xe.u.u.type) 
	    {
	    case KeyPress:
	    case KeyRelease:
	    	(*miEventQueue.pKbd->processInputProc)
					    (&xe, miEventQueue.pKbd, 1);
	    	break;
	    default:
	    	(*miEventQueue.pPtr->processInputProc)
					    (&xe, miEventQueue.pPtr, 1);
	    	break;
	    }
	}
    }
}
