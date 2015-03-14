/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
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


/* $XConsortium: dixutils.c,v 1.39 91/07/16 20:24:03 keith Exp $ */

#include "X.h"
#include "Xmd.h"
#include "misc.h"
#include "window.h"
#include "dixstruct.h"
#include "pixmapstr.h"
#include "scrnintstr.h"
#define  XK_LATIN1
#include "keysymdef.h"

/*
 * CompareTimeStamps returns -1, 0, or +1 depending on if the first
 * argument is less than, equal to or greater than the second argument.
 */

int
CompareTimeStamps(a, b)
    TimeStamp a, b;
{
    if (a.months < b.months)
	return EARLIER;
    if (a.months > b.months)
	return LATER;
    if (a.milliseconds < b.milliseconds)
	return EARLIER;
    if (a.milliseconds > b.milliseconds)
	return LATER;
    return SAMETIME;
}

/*
 * convert client times to server TimeStamps
 */

#define HALFMONTH ((unsigned long) 1<<31)
TimeStamp
ClientTimeToServerTime(c)
     CARD32 c;
{
    TimeStamp ts;
    if (c == CurrentTime)
	return currentTime;
    ts.months = currentTime.months;
    ts.milliseconds = c;
    if (c > currentTime.milliseconds)
    {
	if (((unsigned long) c - currentTime.milliseconds) > HALFMONTH)
	    ts.months -= 1;
    }
    else if (c < currentTime.milliseconds)
    {
	if (((unsigned long)currentTime.milliseconds - c) > HALFMONTH)
	    ts.months += 1;
    }
    return ts;
}

/*
 * ISO Latin-1 case conversion routine
 *
 * this routine always null-terminates the result, so
 * beware of too-small buffers
 */

void
CopyISOLatin1Lowered(dest, source, length)
    register unsigned char *dest, *source;
    int length;
{
    register int i;

    for (i = 0; i < length; i++, source++, dest++)
    {
	if ((*source >= XK_A) && (*source <= XK_Z))
	    *dest = *source + (XK_a - XK_A);
	else if ((*source >= XK_Agrave) && (*source <= XK_Odiaeresis))
	    *dest = *source + (XK_agrave - XK_Agrave);
	else if ((*source >= XK_Ooblique) && (*source <= XK_Thorn))
	    *dest = *source + (XK_oslash - XK_Ooblique);
	else
	    *dest = *source;
    }
    *dest = '\0';
}

WindowPtr
LookupWindow(rid, client)
    XID rid;
    ClientPtr client;
{
    client->errorValue = rid;
    if(rid == INVALID)
	return NULL;
    if (client->lastDrawableID == rid)
    {
        if (client->lastDrawable->type != DRAWABLE_PIXMAP)
            return ((WindowPtr) client->lastDrawable);
        return (WindowPtr) NULL;
    }
    return (WindowPtr)LookupIDByType(rid, RT_WINDOW);
}


pointer
LookupDrawable(rid, client)
    XID rid;
    ClientPtr client;
{
    register DrawablePtr pDraw;

    if(rid == INVALID)
	return (pointer) NULL;
    if (client->lastDrawableID == rid)
	return ((pointer) client->lastDrawable);
    pDraw = (DrawablePtr)LookupIDByClass(rid, RC_DRAWABLE);
    if (pDraw && (pDraw->type != UNDRAWABLE_WINDOW))
        return (pointer)pDraw;		
    return (pointer)NULL;
}


int
AlterSaveSetForClient(client, pWin, mode)
    ClientPtr client;
    WindowPtr pWin;
    unsigned mode;
{
    int numnow;
    pointer *pTmp;
    int j;

    numnow = client->numSaved;
    j = 0;
    if (numnow)
    {
	pTmp = client->saveSet;
	while ((j < numnow) && (pTmp[j] != (pointer)pWin))
	    j++;
    }
    if (mode == SetModeInsert)
    {
	if (j < numnow)         /* duplicate */
	   return(Success);
	numnow++;
	pTmp = (pointer *)xrealloc(client->saveSet, sizeof(pointer) * numnow);
	if (!pTmp)
	    return(BadAlloc);
	client->saveSet = pTmp;
       	client->numSaved = numnow;
	client->saveSet[numnow - 1] = (pointer)pWin;
	return(Success);
    }
    else if ((mode == SetModeDelete) && (j < numnow))
    {
	while (j < numnow-1)
	{
           pTmp[j] = pTmp[j+1];
	   j++;
	}
	numnow--;
        if (numnow)
	{
    	    pTmp = (pointer *)xrealloc(client->saveSet,
				       sizeof(pointer) * numnow);
	    if (pTmp)
		client->saveSet = pTmp;
	}
        else
        {
            xfree(client->saveSet);
	    client->saveSet = (pointer *)NULL;
	}
	client->numSaved = numnow;
	return(Success);
    }
    return(Success);
}


DeleteWindowFromAnySaveSet(pWin)
    WindowPtr pWin;
{
    register int i;
    register ClientPtr client;
    
    for (i = 0; i< currentMaxClients; i++)
    {    
	client = clients[i];
	if (client && client->numSaved)
	    (void)AlterSaveSetForClient(client, pWin, SetModeDelete);
    }
}

/* No-op Don't Do Anything : sometimes we need to be able to call a procedure
 * that doesn't do anything.  For example, on screen with only static
 * colormaps, if someone calls install colormap, it's easier to have a dummy
 * procedure to call than to check if there's a procedure 
 */
void
NoopDDA()
{
}

typedef struct _BlockHandler {
    void    (*BlockHandler)();
    void    (*WakeupHandler)();
    pointer blockData;
    Bool    deleted;
} BlockHandlerRec, *BlockHandlerPtr;

static BlockHandlerPtr	handlers;
static int		numHandlers;
static int		sizeHandlers;
static Bool		inHandler;
static Bool		handlerDeleted;

/* called from the OS layer */
void
BlockHandler(pTimeout, pReadmask)
pointer	pTimeout;	/* DIX doesn't want to know how OS represents time */
pointer pReadmask;	/* nor how it represents the set of descriptors */
{
    register int i, j;
    
    ++inHandler;
    for (i = 0; i < screenInfo.numScreens; i++)
	(* screenInfo.screens[i]->BlockHandler)(i, 
				screenInfo.screens[i]->blockData,
				pTimeout, pReadmask);
    for (i = 0; i < numHandlers; i++)
	(*handlers[i].BlockHandler) (handlers[i].blockData,
				     pTimeout, pReadmask);
    if (handlerDeleted)
    {
	for (i = 0; i < numHandlers;)
	    if (handlers[i].deleted)
	    {
	    	for (j = i; j < numHandlers - 1; j++)
		    handlers[j] = handlers[j+1];
	    	numHandlers--;
	    }
	    else
		i++;
	handlerDeleted = FALSE;
    }
    --inHandler;
}

void
WakeupHandler(result, pReadmask)
unsigned long	result;	/* 32 bits of undefined result from the wait */
pointer pReadmask;	/* the resulting descriptor mask */
{
    register int i, j;

    ++inHandler;
    for (i = numHandlers - 1; i >= 0; i--)
	(*handlers[i].WakeupHandler) (handlers[i].blockData,
				      result, pReadmask);
    for (i = 0; i < screenInfo.numScreens; i++)
	(* screenInfo.screens[i]->WakeupHandler)(i, 
				screenInfo.screens[i]->wakeupData,
				result, pReadmask);
    if (handlerDeleted)
    {
	for (i = 0; i < numHandlers;)
	    if (handlers[i].deleted)
	    {
	    	for (j = i; j < numHandlers - 1; j++)
		    handlers[j] = handlers[j+1];
	    	numHandlers--;
	    }
	    else
		i++;
	handlerDeleted = FALSE;
    }
    --inHandler;
}

/* Reentrant with BlockHandler and WakeupHandler, except wakeup won't
 * get called until next time
 */

Bool
RegisterBlockAndWakeupHandlers (blockHandler, wakeupHandler, blockData)
    void    (*blockHandler)();
    void    (*wakeupHandler)();
    pointer blockData;
{
    BlockHandlerPtr new;

    if (numHandlers >= sizeHandlers)
    {
    	new = (BlockHandlerPtr) xrealloc (handlers, (numHandlers + 1) *
				      	  sizeof (BlockHandlerRec));
    	if (!new)
	    return FALSE;
    	handlers = new;
	sizeHandlers = numHandlers + 1;
    }
    handlers[numHandlers].BlockHandler = blockHandler;
    handlers[numHandlers].WakeupHandler = wakeupHandler;
    handlers[numHandlers].blockData = blockData;
    handlers[numHandlers].deleted = FALSE;
    numHandlers = numHandlers + 1;
    return TRUE;
}

void
RemoveBlockAndWakeupHandlers (blockHandler, wakeupHandler, blockData)
    void    (*blockHandler)();
    void    (*wakeupHandler)();
    pointer blockData;
{
    int	    i;

    for (i = 0; i < numHandlers; i++)
	if (handlers[i].BlockHandler == blockHandler &&
	    handlers[i].WakeupHandler == wakeupHandler &&
	    handlers[i].blockData == blockData)
	{
	    if (inHandler)
	    {
		handlerDeleted = TRUE;
		handlers[i].deleted = TRUE;
	    }
	    else
	    {
	    	for (; i < numHandlers - 1; i++)
		    handlers[i] = handlers[i+1];
	    	numHandlers--;
	    }
	    break;
	}
}

InitBlockAndWakeupHandlers ()
{
    xfree (handlers);
    handlers = (BlockHandlerPtr) 0;
    numHandlers = 0;
    sizeHandlers = 0;
}

/*
 * A general work queue.  Perform some task before the server
 * sleeps for input.
 */

WorkQueuePtr		workQueue;
static WorkQueuePtr	*workQueueLast = &workQueue;
static Bool		WorkBlockHandlerRegistered;

/* ARGSUSED */
void
ProcessWorkQueue()
{
    WorkQueuePtr    q, n, p;

    p = NULL;
    /*
     * Scan the work queue once, calling each function.  Those
     * which return TRUE are removed from the queue, otherwise
     * they will be called again.  This must be reentrant with
     * QueueWorkProc, hence the crufty usage of variables.
     */
    for (q = workQueue; q; q = n)
    {
	if ((*q->function) (q->client, q->closure))
	{
	    /* remove q from the list */
	    n = q->next;    /* don't fetch until after func called */
	    if (p)
		p->next = n;
	    else
		workQueue = n;
	    xfree (q);
	}
	else
	{
	    n = q->next;    /* don't fetch until after func called */
	    p = q;
	}
    }
    if (p)
	workQueueLast = &p->next;
    else
    {
	workQueueLast = &workQueue;
    }
}

Bool
QueueWorkProc (function, client, closure)
    Bool	(*function)();
    ClientPtr	client;
    pointer	closure;
{
    WorkQueuePtr    q;

    q = (WorkQueuePtr) xalloc (sizeof *q);
    if (!q)
	return FALSE;
    q->function = function;
    q->client = client;
    q->closure = closure;
    q->next = NULL;
    *workQueueLast = q;
    workQueueLast = &q->next;
    return TRUE;
}

/*
 * Manage a queue of sleeping clients, awakening them
 * when requested, by using the OS functions IgnoreClient
 * and AttendClient.  Note that this *ignores* the troubles
 * with request data interleaving itself with events, but
 * we'll leave that until a later time.
 */

typedef struct _SleepQueue {
    struct _SleepQueue	*next;
    ClientPtr		client;
    Bool		(*function)();
    pointer		closure;
} SleepQueueRec, *SleepQueuePtr;

static SleepQueuePtr	sleepQueue = NULL;

Bool
ClientSleep (client, function, closure)
    ClientPtr	client;
    Bool	(*function)();
    pointer	closure;
{
    SleepQueuePtr   q;

    q = (SleepQueuePtr) xalloc (sizeof *q);
    if (!q)
	return FALSE;

    IgnoreClient (client);
    q->next = sleepQueue;
    q->client = client;
    q->function = function;
    q->closure = closure;
    sleepQueue = q;
    return TRUE;
}

Bool
ClientSignal (client)
    ClientPtr	client;
{
    SleepQueuePtr   q;

    for (q = sleepQueue; q; q = q->next)
	if (q->client == client)
	{
	    return QueueWorkProc (q->function, q->client, q->closure);
	}
    return FALSE;
}

ClientWakeup (client)
    ClientPtr	client;
{
    SleepQueuePtr   q, *prev;

    prev = &sleepQueue;
    while (q = *prev)
    {
	if (q->client == client)
	{
	    *prev = q->next;
	    xfree (q);
	    if (!client->clientGone)
		AttendClient (client);
	    break;
	}
	prev = &q->next;
    }
}

Bool
ClientIsAsleep (client)
    ClientPtr	client;
{
    SleepQueuePtr   q;

    for (q = sleepQueue; q; q = q->next)
	if (q->client == client)
	    return TRUE;
    return FALSE;
}
