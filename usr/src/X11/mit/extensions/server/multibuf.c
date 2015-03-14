/************************************************************
Copyright 1989 by The Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
no- tice appear in all copies and that both that copyright
no- tice and this permission notice appear in supporting
docu- mentation, and that the name of MIT not be used in
advertising or publicity pertaining to distribution of the
software without specific prior written permission.
M.I.T. makes no representation about the suitability of
this software for any purpose. It is provided "as is"
without any express or implied warranty.

MIT DISCLAIMS ALL WARRANTIES WITH REGARD TO  THIS  SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FIT-
NESS FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL MIT BE  LI-
ABLE  FOR  ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,  DATA  OR
PROFITS,  WHETHER  IN  AN  ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION  WITH
THE USE OR PERFORMANCE OF THIS SOFTWARE.

********************************************************/

/* $XConsortium: multibuf.c,v 1.11 91/06/01 13:25:50 rws Exp $ */
#define NEED_REPLIES
#define NEED_EVENTS
#include <stdio.h>
#include "X.h"
#include "Xproto.h"
#include "misc.h"
#include "os.h"
#include "windowstr.h"
#include "scrnintstr.h"
#include "pixmapstr.h"
#include "extnsionst.h"
#include "dixstruct.h"
#include "resource.h"
#include "opaque.h"
#define _MULTIBUF_SERVER_	/* don't want Xlib structures */
#include "multibufst.h"
#include "regionstr.h"
#include "gcstruct.h"
#include "inputstr.h"
#include <sys/time.h>

/*
 * per-Multibuffer data
 */
 
typedef struct _Multibuffers	*MultibuffersPtr;

#define SameClient(obj,client) \
	(CLIENT_BITS((obj)->resource) == (client)->clientAsMask)
#define rClient(obj) (clients[CLIENT_ID((obj)->resource)])
#define bClient(b)   (clients[CLIENT_ID(b->pPixmap->drawable.id)])

#define ValidEventMasks (ExposureMask|MultibufferClobberNotifyMask|MultibufferUpdateNotifyMask)

typedef struct _Multibuffer {
    MultibuffersPtr pMultibuffers;  /* associated window data */
    Mask	    eventMask;	    /* MultibufferClobberNotifyMask|ExposureMask|MultibufferUpdateNotifyMask */
    Mask	    otherEventMask; /* mask of all other clients event masks */
    OtherClients    *otherClients;
    int		    number;	    /* index into array */
    int		    side;	    /* alwys Mono */
    int		    clobber;	    /* Unclobbered, PartiallyClobbered, FullClobbered */
    PixmapPtr	    pPixmap;	    /* associated pixmap */
} MultibufferRec, *MultibufferPtr;

/*
 * per-window data
 */

typedef struct _Multibuffers {
    WindowPtr	pWindow;		/* associated window */
    int		numMultibuffer;		/* count of buffers */
    int		displayedMultibuffer;	/* currently active buffer */
    int		updateAction;		/* Undefined, Background, Untouched, Copied */
    int		updateHint;		/* Frequent, Intermittent, Static */
    int		windowMode;		/* always Mono */

    TimeStamp	lastUpdate;		/* time of last update */

    unsigned short	width, height;	/* last known window size */
    short		x, y;		/* for static gravity */

    MultibufferPtr	buffers;
} MultibuffersRec;

/*
 * per-screen data
 */
typedef struct _MultibufferScreen {
    Bool	(*PositionWindow)();
} MultibufferScreenRec, *MultibufferScreenPtr;

/*
 * per display-image-buffers request data.
 */

typedef struct _DisplayRequest {
    struct _DisplayRequest	*next;
    TimeStamp			activateTime;
    ClientPtr			pClient;
    XID				id;
} DisplayRequestRec, *DisplayRequestPtr;

static unsigned char	MultibufferReqCode;
static int		MultibufferEventBase;
static int		MultibufferErrorBase;
static int		MultibufferScreenIndex = -1;
static int		MultibufferWindowIndex = -1;

static int		BlockHandlerRegistered;
static void		MultibufferBlockHandler(), MultibufferWakeupHandler();

static void		PerformDisplayRequest ();
static void		DisposeDisplayRequest ();
static Bool		QueueDisplayRequest ();

static void		BumpTimeStamp ();

static void		MultibufferExpose ();
static void		MultibufferUpdate ();
static void		AliasMultibuffer ();
static void		DisposeMultibuffers ();
static void		RecalculateMultibufferOtherEvents ();
static int		EventSelectForMultibuffer();

/*
 * The Pixmap associated with a buffer can be found as a resource
 * with this type
 */
static RESTYPE		MultibufferDrawableResType;
static void		MultibufferDrawableDelete ();
/*
 * The per-buffer data can be found as a resource with this type.
 * the resource id of the per-buffer data is the same as the resource
 * id of the pixmap
 */
static RESTYPE		MultibufferResType;
static void		MultibufferDelete ();
/*
 * The per-window data can be found as a resource with this type,
 * using the window resource id
 */
static RESTYPE		MultibuffersResType;
static void		MultibuffersDelete ();
/*
 * Per display-buffers request is attached to a resource so that
 * it will disappear if the client dies before the request should
 * be processed
 */
static RESTYPE		DisplayRequestResType;
static void		DisplayRequestDelete ();
/*
 * Clients other than the buffer creator attach event masks in
 * OtherClient structures; each has a resource of this type.
 */
static RESTYPE		OtherClientResType;
static void		OtherClientDelete ();

/****************
 * MultibufferExtensionInit
 *
 * Called from InitExtensions in main()
 *
 ****************/

static int		ProcMultibufferDispatch(), SProcMultibufferDispatch();
static void		MultibufferResetProc();
static void		SClobberNotifyEvent(), SUpdateNotifyEvent();
static Bool		MultibufferPositionWindow();

void
MultibufferExtensionInit()
{
    ExtensionEntry	    *extEntry;
    int			    i, j;
    ScreenPtr		    pScreen;
    MultibufferScreenPtr    pMultibufferScreen;

    /*
     * allocate private pointers in windows and screens.  Allocating
     * window privates may seem like an unnecessary expense, but every
     * PositionWindow call must check to see if the window is
     * multi-buffered; a resource lookup is too expensive.
     */
    MultibufferScreenIndex = AllocateScreenPrivateIndex ();
    if (MultibufferScreenIndex < 0)
	return;
    MultibufferWindowIndex = AllocateWindowPrivateIndex ();
    for (i = 0; i < screenInfo.numScreens; i++)
    {
	pScreen = screenInfo.screens[i];
	if (!AllocateWindowPrivate (pScreen, MultibufferWindowIndex, 0) ||
	    !(pMultibufferScreen = (MultibufferScreenPtr) xalloc (sizeof (MultibufferScreenRec))))
	{
	    for (j = 0; j < i; j++)
		xfree (screenInfo.screens[j]->devPrivates[MultibufferScreenIndex].ptr);
	    return;
	}
	pScreen->devPrivates[MultibufferScreenIndex].ptr = (pointer) pMultibufferScreen;
	/*
 	 * wrap PositionWindow to resize the pixmap when the window
	 * changes size
 	 */
	pMultibufferScreen->PositionWindow = pScreen->PositionWindow;
	pScreen->PositionWindow = MultibufferPositionWindow;
    }
    /*
     * create the resource types
     */
    MultibufferDrawableResType =
	CreateNewResourceType(MultibufferDrawableDelete)|RC_CACHED|RC_DRAWABLE;
    MultibufferResType = CreateNewResourceType(MultibufferDelete);
    MultibuffersResType = CreateNewResourceType(MultibuffersDelete);
    DisplayRequestResType = CreateNewResourceType(DisplayRequestDelete);
    OtherClientResType = CreateNewResourceType(OtherClientDelete);
    if (MultibufferDrawableResType && MultibufferResType &&
	MultibuffersResType && DisplayRequestResType &&
	OtherClientResType &&
	(extEntry = AddExtension(MULTIBUFFER_PROTOCOL_NAME,
				 MultibufferNumberEvents, 
				 MultibufferNumberErrors,
				 ProcMultibufferDispatch, SProcMultibufferDispatch,
				 MultibufferResetProc, StandardMinorOpcode)))
    {
	MultibufferReqCode = (unsigned char)extEntry->base;
	MultibufferEventBase = extEntry->eventBase;
	MultibufferErrorBase = extEntry->errorBase;
	EventSwapVector[MultibufferEventBase + MultibufferClobberNotify] = SClobberNotifyEvent;
	EventSwapVector[MultibufferEventBase + MultibufferUpdateNotify] = SUpdateNotifyEvent;
    }
}

/*ARGSUSED*/
static void
MultibufferResetProc (extEntry)
ExtensionEntry	*extEntry;
{
    int			    i;
    ScreenPtr		    pScreen;
    MultibufferScreenPtr    pMultibufferScreen;
    
    if (MultibufferScreenIndex < 0)
	return;
    for (i = 0; i < screenInfo.numScreens; i++)
    {
	pScreen = screenInfo.screens[i];
	if (pScreen->devPrivates[MultibufferScreenIndex].ptr)
	{
	    pMultibufferScreen = (MultibufferScreenPtr) pScreen->devPrivates[MultibufferScreenIndex].ptr;
	    pScreen->PositionWindow = pMultibufferScreen->PositionWindow;
	    xfree (pMultibufferScreen);
	}
    }
}

static int
ProcGetBufferVersion (client)
    register ClientPtr	client;
{
    REQUEST(xMbufGetBufferVersionReq);
    xMbufGetBufferVersionReply	rep;
    register int		n;

    REQUEST_SIZE_MATCH (xMbufGetBufferVersionReq);
    rep.type = X_Reply;
    rep.length = 0;
    rep.sequenceNumber = client->sequence;
    rep.majorVersion = MULTIBUFFER_MAJOR_VERSION;
    rep.minorVersion = MULTIBUFFER_MINOR_VERSION;
    if (client->swapped) {
    	swaps(&rep.sequenceNumber, n);
    	swapl(&rep.length, n);
    }
    WriteToClient(client, sizeof (xMbufGetBufferVersionReply), (char *)&rep);
    return (client->noClientException);
}

static int
ProcCreateImageBuffers (client)
    register ClientPtr	client;
{
    REQUEST(xMbufCreateImageBuffersReq);
    xMbufCreateImageBuffersReply	rep;
    register int		n;
    WindowPtr			pWin;
    XID				*ids;
    int				len, nbuf;
    int				i;
    MultibuffersPtr			pMultibuffers;
    MultibufferPtr			pMultibuffer;
    ScreenPtr			pScreen;
    int				width, height, depth;

    REQUEST_AT_LEAST_SIZE (xMbufCreateImageBuffersReq);
    len = stuff->length - (sizeof(xMbufCreateImageBuffersReq) >> 2);
    if (len == 0)
	return BadLength;
    if (!(pWin = LookupWindow (stuff->window, client)))
	return BadWindow;
    if (pWin->drawable.class == InputOnly)
	return BadMatch;
    switch (stuff->updateAction)
    {
    case MultibufferUpdateActionUndefined:
    case MultibufferUpdateActionBackground:
    case MultibufferUpdateActionUntouched:
    case MultibufferUpdateActionCopied:
	break;
    default:
	client->errorValue = stuff->updateAction;
	return BadValue;
    }
    switch (stuff->updateHint)
    {
    case MultibufferUpdateHintFrequent:
    case MultibufferUpdateHintIntermittent:
    case MultibufferUpdateHintStatic:
	break;
    default:
	client->errorValue = stuff->updateHint;
	return BadValue;
    }
    nbuf = len;
    ids = (XID *) &stuff[1];
    for (i = 0; i < nbuf; i++)
    {
	LEGAL_NEW_RESOURCE(ids[i], client);
    }
    pMultibuffers = (MultibuffersPtr) xalloc (sizeof (MultibuffersRec));
    if (!pMultibuffers)
	return BadAlloc;
    pMultibuffers->pWindow = pWin;
    pMultibuffers->buffers = (MultibufferPtr) xalloc (nbuf * sizeof (MultibufferRec));
    if (!pMultibuffers->buffers)
    {
	xfree (pMultibuffers);
	return BadAlloc;
    }
    if (!AddResource (pWin->drawable.id, MultibuffersResType, (pointer) pMultibuffers))
    {
	xfree (pMultibuffers->buffers);
	xfree (pMultibuffers);
	return BadAlloc;
    }
    width = pWin->drawable.width;
    height = pWin->drawable.height;
    depth = pWin->drawable.depth;
    pScreen = pWin->drawable.pScreen;
    for (i = 0; i < nbuf; i++)
    {
	pMultibuffer = &pMultibuffers->buffers[i];
	pMultibuffer->eventMask = 0L;
	pMultibuffer->otherEventMask = 0L;
	pMultibuffer->otherClients = (OtherClientsPtr) NULL;
	pMultibuffer->number = i;
	pMultibuffer->side = MultibufferSideMono;
	pMultibuffer->clobber = MultibufferUnclobbered;
	pMultibuffer->pMultibuffers = pMultibuffers;
	if (!AddResource (ids[i], MultibufferResType, (pointer) pMultibuffer))
	    break;
	pMultibuffer->pPixmap = (*pScreen->CreatePixmap) (pScreen, width, height, depth);
	if (!pMultibuffer->pPixmap)
	    break;
	if (!AddResource (ids[i], MultibufferDrawableResType, (pointer) pMultibuffer->pPixmap))
	{
	    FreeResource (ids[i], MultibufferResType);
	    (*pScreen->DestroyPixmap) (pMultibuffer->pPixmap);
	    break;
	}
	pMultibuffer->pPixmap->drawable.id = ids[i];
    }
    pMultibuffers->numMultibuffer = i;
    pMultibuffers->displayedMultibuffer = -1;
    if (i > 0)
	AliasMultibuffer (pMultibuffers, 0);
    pMultibuffers->updateAction = stuff->updateAction;
    pMultibuffers->updateHint = stuff->updateHint;
    pMultibuffers->windowMode = MultibufferModeMono;
    pMultibuffers->lastUpdate.months = 0;
    pMultibuffers->lastUpdate.milliseconds = 0;
    pMultibuffers->width = width;
    pMultibuffers->height = height;
    pWin->devPrivates[MultibufferWindowIndex].ptr = (pointer) pMultibuffers;
    rep.type = X_Reply;
    rep.length = 0;
    rep.sequenceNumber = client->sequence;
    rep.numberBuffer = pMultibuffers->numMultibuffer;
    if (client->swapped)
    {
    	swaps(&rep.sequenceNumber, n);
    	swapl(&rep.length, n);
	swaps(&rep.numberBuffer, n);
    }
    WriteToClient(client, sizeof (xMbufCreateImageBuffersReply), (char *)&rep);
    return (client->noClientException);
}

static int
ProcDisplayImageBuffers (client)
    register ClientPtr	client;
{
    REQUEST(xMbufDisplayImageBuffersReq);
    MultibufferPtr	    *pMultibuffer;
    MultibuffersPtr	    *ppMultibuffers;
    int		    nbuf;
    XID		    *ids;
    int		    i, j;
    CARD32	    minDelay, maxDelay;
    TimeStamp	    activateTime, bufferTime;
    
    REQUEST_AT_LEAST_SIZE (xMbufDisplayImageBuffersReq);
    nbuf = stuff->length - (sizeof (xMbufDisplayImageBuffersReq) >> 2);
    if (!nbuf)
	return Success;
    minDelay = stuff->minDelay;
    maxDelay = stuff->maxDelay;
    ids = (XID *) &stuff[1];
    ppMultibuffers = (MultibuffersPtr *) xalloc (nbuf * sizeof (MultibuffersPtr));
    pMultibuffer = (MultibufferPtr *) xalloc (nbuf * sizeof (MultibufferPtr));
    if (!ppMultibuffers || !pMultibuffer)
    {
	xfree (ppMultibuffers);
	xfree (pMultibuffer);
	client->errorValue = 0;
	return BadAlloc;
    }
    activateTime.months = 0;
    activateTime.milliseconds = 0;
    for (i = 0; i < nbuf; i++)
    {
	pMultibuffer[i] = (MultibufferPtr) LookupIDByType (ids[i], MultibufferResType);
	if (!pMultibuffer[i])
	{
	    xfree (ppMultibuffers);
	    xfree (pMultibuffer);
	    client->errorValue = ids[i];
	    return MultibufferErrorBase + MultibufferBadBuffer;
	}
	ppMultibuffers[i] = pMultibuffer[i]->pMultibuffers;
	for (j = 0; j < i; j++)
	{
	    if (ppMultibuffers[i] == ppMultibuffers[j])
	    {
	    	xfree (ppMultibuffers);
	    	xfree (pMultibuffer);
		client->errorValue = ids[i];
	    	return BadMatch;
	    }
	}
	bufferTime = ppMultibuffers[i]->lastUpdate;
	BumpTimeStamp (&bufferTime, minDelay);
	if (CompareTimeStamps (bufferTime, activateTime) == LATER)
	    activateTime = bufferTime;
    }
    UpdateCurrentTime ();
    if (CompareTimeStamps (activateTime, currentTime) == LATER &&
	QueueDisplayRequest (client, activateTime))
    {
	;
    }
    else
	PerformDisplayRequest (ppMultibuffers, pMultibuffer, nbuf);
    xfree (ppMultibuffers);
    xfree (pMultibuffer);
    return Success;
}

static int
ProcDestroyImageBuffers (client)
    register ClientPtr	client;
{
    REQUEST (xMbufDestroyImageBuffersReq);
    WindowPtr	pWin;

    REQUEST_SIZE_MATCH (xMbufDestroyImageBuffersReq);
    if (!(pWin = LookupWindow (stuff->window, client)))
	return BadWindow;
    DisposeMultibuffers (pWin);
    return Success;
}

static int
ProcSetMBufferAttributes (client)
    register ClientPtr	client;
{
    REQUEST (xMbufSetMBufferAttributesReq);
    WindowPtr	pWin;
    MultibuffersPtr	pMultibuffers;
    int		len;
    Mask	vmask;
    Mask	index;
    CARD32	updateHint;
    XID		*vlist;

    REQUEST_AT_LEAST_SIZE (xMbufSetMBufferAttributesReq);
    pWin = LookupWindow (stuff->window, client);
    if (!pWin)
	return BadWindow;
    pMultibuffers = (MultibuffersPtr)LookupIDByType (pWin->drawable.id, MultibuffersResType);
    if (!pMultibuffers)
	return BadMatch;
    len = stuff->length - (sizeof (xMbufSetMBufferAttributesReq) >> 2);
    vmask = stuff->valueMask;
    if (len != Ones (vmask))
	return BadLength;
    vlist = (XID *) &stuff[1];
    while (vmask)
    {
	index = (Mask) lowbit (vmask);
	vmask &= ~index;
	switch (index)
	{
	case MultibufferWindowUpdateHint:
	    updateHint = (CARD32) *vlist;
	    switch (updateHint)
	    {
	    case MultibufferUpdateHintFrequent:
	    case MultibufferUpdateHintIntermittent:
	    case MultibufferUpdateHintStatic:
		pMultibuffers->updateHint = updateHint;
		break;
	    default:
		client->errorValue = updateHint;
		return BadValue;
	    }
	    vlist++;
	    break;
	default:
	    client->errorValue = stuff->valueMask;
	    return BadValue;
	}
    }
    return Success;
}

static int
ProcGetMBufferAttributes (client)
    ClientPtr	client;
{
    REQUEST (xMbufGetMBufferAttributesReq);
    WindowPtr	pWin;
    MultibuffersPtr	pMultibuffers;
    XID		*ids;
    xMbufGetMBufferAttributesReply  rep;
    int		i, n;

    REQUEST_SIZE_MATCH (xMbufGetMBufferAttributesReq);
    pWin = LookupWindow (stuff->window, client);
    if (!pWin)
	return BadWindow;
    pMultibuffers = (MultibuffersPtr)LookupIDByType (pWin->drawable.id, MultibuffersResType);
    if (!pMultibuffers)
	return BadAccess;
    ids = (XID *) ALLOCATE_LOCAL (pMultibuffers->numMultibuffer * sizeof (XID));
    if (!ids)
	return BadAlloc;
    for (i = 0; i < pMultibuffers->numMultibuffer; i++)
	ids[i] = pMultibuffers->buffers[i].pPixmap->drawable.id;
    rep.type = X_Reply;
    rep.sequenceNumber = client->sequence;
    rep.length = pMultibuffers->numMultibuffer;
    rep.displayedBuffer = pMultibuffers->displayedMultibuffer;
    rep.updateAction = pMultibuffers->updateAction;
    rep.updateHint = pMultibuffers->updateHint;
    rep.windowMode = pMultibuffers->windowMode;
    if (client->swapped)
    {
    	swaps(&rep.sequenceNumber, n);
    	swapl(&rep.length, n);
	swaps(&rep.displayedBuffer, n);
	SwapLongs (ids, pMultibuffers->numMultibuffer);
    }
    WriteToClient (client, sizeof (xMbufGetMBufferAttributesReply), &rep);
    WriteToClient (client, (int) (pMultibuffers->numMultibuffer * sizeof (XID)), ids);
    DEALLOCATE_LOCAL((pointer) ids);
    return client->noClientException;
}

static int
ProcSetBufferAttributes (client)
    register ClientPtr	client;
{
    REQUEST(xMbufSetBufferAttributesReq);
    MultibufferPtr	pMultibuffer;
    int		len;
    Mask	vmask, index;
    XID		*vlist;
    Mask	eventMask;
    int		result;

    REQUEST_AT_LEAST_SIZE (xMbufSetBufferAttributesReq);
    pMultibuffer = (MultibufferPtr) LookupIDByType (stuff->buffer, MultibufferResType);
    if (!pMultibuffer)
	return MultibufferErrorBase + MultibufferBadBuffer;
    len = stuff->length - (sizeof (xMbufSetBufferAttributesReq) >> 2);
    vmask = stuff->valueMask;
    if (len != Ones (vmask))
	return BadLength;
    vlist = (XID *) &stuff[1];
    while (vmask)
    {
	index = (Mask) lowbit (vmask);
	vmask &= ~index;
	switch (index)
	{
	case MultibufferBufferEventMask:
	    eventMask = (Mask) *vlist;
	    vlist++;
	    result = EventSelectForMultibuffer (pMultibuffer, client, eventMask);
	    if (result != Success)
		return result;
	    break;
	default:
	    client->errorValue = stuff->valueMask;
	    return BadValue;
	}
    }
    return Success;
}

ProcGetBufferAttributes (client)
    register ClientPtr	client;
{
    REQUEST(xMbufGetBufferAttributesReq);
    MultibufferPtr	pMultibuffer;
    xMbufGetBufferAttributesReply	rep;
    OtherClientsPtr		other;
    int				n;

    REQUEST_SIZE_MATCH (xMbufGetBufferAttributesReq);
    pMultibuffer = (MultibufferPtr) LookupIDByType (stuff->buffer, MultibufferResType);
    if (!pMultibuffer)
	return MultibufferErrorBase + MultibufferBadBuffer;
    rep.type = X_Reply;
    rep.sequenceNumber = client->sequence;
    rep.length = 0;
    rep.window = pMultibuffer->pMultibuffers->pWindow->drawable.id;
    if (bClient (pMultibuffer) == client)
	rep.eventMask = pMultibuffer->eventMask;
    else
    {
	rep.eventMask = (Mask) 0L;
	for (other = pMultibuffer->otherClients; other; other = other->next)
	    if (SameClient (other, client))
	    {
		rep.eventMask = other->mask;
		break;
	    }
    }
    rep.bufferIndex = pMultibuffer->number;
    rep.side = pMultibuffer->side;
    if (client->swapped)
    {
    	swaps(&rep.sequenceNumber, n);
    	swapl(&rep.length, n);
	swapl(&rep.window, n);
	swapl(&rep.eventMask, n);
	swaps(&rep.bufferIndex, n);
    }
    WriteToClient(client, sizeof (xMbufGetBufferAttributesReply), (char *)&rep);
    return (client->noClientException);
}

static int
ProcGetBufferInfo (client)
    register ClientPtr	client;
{
    REQUEST (xMbufGetBufferInfoReq);
    DrawablePtr		    pDrawable;
    xMbufGetBufferInfoReply rep;
    ScreenPtr		    pScreen;
    int			    i, j, k;
    int			    n;
    xMbufBufferInfo	    *pInfo;
    int			    nInfo;
    DepthPtr		    pDepth;

    pDrawable = (DrawablePtr) LookupDrawable (stuff->drawable, client);
    if (!pDrawable)
	return BadDrawable;
    pScreen = pDrawable->pScreen;
    nInfo = 0;
    for (i = 0; i < pScreen->numDepths; i++)
    {
	pDepth = &pScreen->allowedDepths[i];
	nInfo += pDepth->numVids;
    }
    pInfo = (xMbufBufferInfo *)
	    ALLOCATE_LOCAL (pScreen->numVisuals * sizeof (xMbufBufferInfo));
    if (!pInfo)
	return BadAlloc;

    rep.type = X_Reply;
    rep.sequenceNumber = client->sequence;
    rep.length = nInfo * (sizeof (xMbufBufferInfo) >> 2);
    rep.normalInfo = nInfo;
    rep.stereoInfo = 0;
    if (client->swapped)
    {
	swaps(&rep.sequenceNumber, n);
	swapl(&rep.length, n);
	swaps(&rep.normalInfo, n);
	swaps(&rep.stereoInfo, n);
    }

    k = 0;
    for (i = 0; i < pScreen->numDepths; i++)
    {
	pDepth = &pScreen->allowedDepths[i];
	for (j = 0; j < pDepth->numVids; j++)
	{
	    pInfo[k].visualID = pDepth->vids[j];
	    pInfo[k].maxBuffers = 0;
	    pInfo[k].depth = pDepth->depth;
	    if (client->swapped)
	    {
		swapl (&pInfo[k].visualID, n);
		swaps (&pInfo[k].maxBuffers, n);
	    }
	    k++;
	}
    }
    WriteToClient (client, sizeof (xMbufGetBufferInfoReply), (pointer) &rep);
    WriteToClient (client, (int) nInfo * sizeof (xMbufBufferInfo), (pointer) pInfo);
    DEALLOCATE_LOCAL ((pointer) pInfo);
    return client->noClientException;
}

static int
ProcMultibufferDispatch (client)
    register ClientPtr	client;
{
    REQUEST(xReq);
    switch (stuff->data) {
    case X_MbufGetBufferVersion:
	return ProcGetBufferVersion (client);
    case X_MbufCreateImageBuffers:
	return ProcCreateImageBuffers (client);
    case X_MbufDisplayImageBuffers:
	return ProcDisplayImageBuffers (client);
    case X_MbufDestroyImageBuffers:
	return ProcDestroyImageBuffers (client);
    case X_MbufSetMBufferAttributes:
	return ProcSetMBufferAttributes (client);
    case X_MbufGetMBufferAttributes:
	return ProcGetMBufferAttributes (client);
    case X_MbufSetBufferAttributes:
	return ProcSetBufferAttributes (client);
    case X_MbufGetBufferAttributes:
	return ProcGetBufferAttributes (client);
    case X_MbufGetBufferInfo:
	return ProcGetBufferInfo (client);
    default:
	return BadRequest;
    }
}

static int
SProcGetBufferVersion (client)
    register ClientPtr	client;
{
    register int    n;
    REQUEST (xMbufGetBufferVersionReq);

    swaps (&stuff->length, n);
    return ProcGetBufferVersion (client);
}

static int
SProcCreateImageBuffers (client)
    register ClientPtr	client;
{
    register int    n;
    REQUEST (xMbufCreateImageBuffersReq);

    swaps (&stuff->length, n);
    REQUEST_AT_LEAST_SIZE (xMbufCreateImageBuffersReq);
    swapl (&stuff->window, n);
    SwapRestL(stuff);
    return ProcCreateImageBuffers (client);
}

static int
SProcDisplayImageBuffers (client)
    register ClientPtr	client;
{
    register int    n;
    REQUEST (xMbufDisplayImageBuffersReq);
    
    swaps (&stuff->length, n);
    REQUEST_AT_LEAST_SIZE (xMbufDisplayImageBuffersReq);
    swaps (&stuff->minDelay, n);
    swaps (&stuff->maxDelay, n);
    SwapRestL(stuff);
    return ProcDisplayImageBuffers (client);
}

static int
SProcDestroyImageBuffers (client)
    register ClientPtr	client;
{
    register int    n;
    REQUEST (xMbufDestroyImageBuffersReq);
    
    swaps (&stuff->length, n);
    REQUEST_SIZE_MATCH (xMbufDestroyImageBuffersReq);
    swapl (&stuff->window, n);
    return ProcDestroyImageBuffers (client);
}

static int
SProcSetMBufferAttributes (client)
    register ClientPtr	client;
{
    register int    n;
    REQUEST (xMbufSetMBufferAttributesReq);

    swaps (&stuff->length, n);
    REQUEST_AT_LEAST_SIZE(xMbufSetMBufferAttributesReq);
    swapl (&stuff->window, n);
    swapl (&stuff->valueMask, n);
    SwapRestL(stuff);
    return ProcSetMBufferAttributes (client);
}

static int
SProcGetMBufferAttributes (client)
    register ClientPtr	client;
{
    register int    n;
    REQUEST (xMbufGetMBufferAttributesReq);

    swaps (&stuff->length, n);
    REQUEST_AT_LEAST_SIZE(xMbufGetMBufferAttributesReq);
    swapl (&stuff->window, n);
    return ProcGetMBufferAttributes (client);
}

static int
SProcSetBufferAttributes (client)
    register ClientPtr	client;
{
    register int    n;
    REQUEST (xMbufSetBufferAttributesReq);

    swaps (&stuff->length, n);
    REQUEST_AT_LEAST_SIZE(xMbufSetBufferAttributesReq);
    swapl (&stuff->buffer, n);
    swapl (&stuff->valueMask, n);
    SwapRestL(stuff);
    return ProcSetBufferAttributes (client);
}

static int
SProcGetBufferAttributes (client)
    register ClientPtr	client;
{
    register int    n;
    REQUEST (xMbufGetBufferAttributesReq);

    swaps (&stuff->length, n);
    REQUEST_AT_LEAST_SIZE(xMbufGetBufferAttributesReq);
    swapl (&stuff->buffer, n);
    return ProcGetBufferAttributes (client);
}

static int
SProcGetBufferInfo (client)
    register ClientPtr	client;
{
    register int    n;
    REQUEST (xMbufGetBufferInfoReq);

    swaps (&stuff->length, n);
    REQUEST_SIZE_MATCH (xMbufGetBufferInfoReq);
    swapl (&stuff->drawable, n);
    return ProcGetBufferInfo (client);
}

static int
SProcMultibufferDispatch (client)
    register ClientPtr	client;
{
    REQUEST(xReq);
    switch (stuff->data) {
    case X_MbufGetBufferVersion:
	return SProcGetBufferVersion (client);
    case X_MbufCreateImageBuffers:
	return SProcCreateImageBuffers (client);
    case X_MbufDisplayImageBuffers:
	return SProcDisplayImageBuffers (client);
    case X_MbufDestroyImageBuffers:
	return SProcDestroyImageBuffers (client);
    case X_MbufSetMBufferAttributes:
	return SProcSetMBufferAttributes (client);
    case X_MbufGetMBufferAttributes:
	return SProcGetMBufferAttributes (client);
    case X_MbufSetBufferAttributes:
	return SProcSetBufferAttributes (client);
    case X_MbufGetBufferAttributes:
	return SProcGetBufferAttributes (client);
    case X_MbufGetBufferInfo:
	return SProcGetBufferInfo (client);
    default:
	return BadRequest;
    }
}

static void
SUpdateNotifyEvent (from, to)
    xMbufUpdateNotifyEvent	*from, *to;
{
    to->type = from->type;
    cpswaps (from->sequenceNumber, to->sequenceNumber);
    cpswapl (from->buffer, to->buffer);
    cpswapl (from->timeStamp, to->timeStamp);
}

static void
SClobberNotifyEvent (from, to)
    xMbufClobberNotifyEvent	*from, *to;
{
    to->type = from->type;
    cpswaps (from->sequenceNumber, to->sequenceNumber);
    cpswapl (from->buffer, to->buffer);
    to->state = from->state;
}

static void
SetupBackgroundPainter (pWin, pGC)
    WindowPtr	pWin;
    GCPtr	pGC;
{
    XID		    gcvalues[4];
    int		    ts_x_origin, ts_y_origin;
    PixUnion	    background;
    int		    backgroundState;
    Mask	    gcmask;

    /*
     * set up the gc to clear the pixmaps;
     */
    ts_x_origin = ts_y_origin = 0;

    backgroundState = pWin->backgroundState;
    background = pWin->background;
    if (backgroundState == ParentRelative) {
	WindowPtr	pParent;

	pParent = pWin;
	while (pParent->backgroundState == ParentRelative) {
	    ts_x_origin -= pParent->origin.x;
	    ts_y_origin -= pParent->origin.y;
	    pParent = pParent->parent;
	}
	backgroundState = pParent->backgroundState;
	background = pParent->background;
    }

    /*
     * First take care of any ParentRelative stuff by altering the
     * tile/stipple origin to match the coordinates of the upper-left
     * corner of the first ancestor without a ParentRelative background.
     * This coordinate is, of course, negative.
     */

    if (backgroundState == BackgroundPixel)
    {
	gcvalues[0] = (XID) background.pixel;
	gcvalues[1] = FillSolid;
	gcmask = GCForeground|GCFillStyle;
    }
    else
    {
	gcvalues[0] = FillTiled;
	gcvalues[1] = (XID) background.pixmap;
	gcvalues[2] = ts_x_origin;
	gcvalues[3] = ts_y_origin;
	gcmask = GCFillStyle|GCTile|GCTileStipXOrigin|GCTileStipYOrigin;
    }
    DoChangeGC(pGC, gcmask, gcvalues, TRUE);
}

static void
PerformDisplayRequest (ppMultibuffers, pMultibuffer, nbuf)
    MultibufferPtr	    *pMultibuffer;
    MultibuffersPtr	    *ppMultibuffers;
    int		    nbuf;
{
    GCPtr	    pGC;
    PixmapPtr	    pPrevPixmap, pNewPixmap;
    xRectangle	    clearRect;
    WindowPtr	    pWin;
    RegionPtr	    pExposed;
    int		    i;
    MultibufferPtr	    pPrevMultibuffer;
    XID		    bool;

    UpdateCurrentTime ();
    for (i = 0; i < nbuf; i++)
    {
	pWin = ppMultibuffers[i]->pWindow;
	pGC = GetScratchGC (pWin->drawable.depth, pWin->drawable.pScreen);
	pPrevMultibuffer = &ppMultibuffers[i]->buffers[ppMultibuffers[i]->displayedMultibuffer];
	pPrevPixmap = pPrevMultibuffer->pPixmap;
	pNewPixmap = pMultibuffer[i]->pPixmap;
	switch (ppMultibuffers[i]->updateAction)
	{
	case MultibufferUpdateActionUndefined:
	    break;
	case MultibufferUpdateActionBackground:
	    SetupBackgroundPainter (pWin, pGC);
	    ValidateGC (pPrevPixmap, pGC);
	    clearRect.x = 0;
	    clearRect.y = 0;
	    clearRect.width = pPrevPixmap->drawable.width;
	    clearRect.height = pPrevPixmap->drawable.height;
	    (*pGC->ops->PolyFillRect) (pPrevPixmap, pGC, 1, &clearRect);
	    break;
	case MultibufferUpdateActionUntouched:
	    if (pPrevMultibuffer->eventMask & ExposureMask)
	    {
	    	bool = TRUE;
	    	DoChangeGC (pGC, GCGraphicsExposures, &bool, FALSE);
	    }
	    ValidateGC (pPrevPixmap, pGC);
	    pExposed = (*pGC->ops->CopyArea)
			    ((DrawablePtr) pWin,
			     (DrawablePtr) pPrevPixmap,
			     pGC,
			     0, 0,
			     pWin->drawable.width, pWin->drawable.height,
			     0, 0);
	    if (pPrevMultibuffer->eventMask & ExposureMask)
	    {
	    	if (pExposed)
	    	{
		    RegionPtr	pWinSize;
		    extern RegionPtr	CreateUnclippedWinSize();

		    pWinSize = CreateUnclippedWinSize (pWin);
		    (*pWin->drawable.pScreen->Intersect) (pExposed,
							  pExposed, pWinSize);
		    (*pWin->drawable.pScreen->RegionDestroy) (pWinSize);
	    	    MultibufferExpose (pPrevMultibuffer, pExposed);
	    	    (*pWin->drawable.pScreen->RegionDestroy) (pExposed);
	    	}
	    	bool = FALSE;
	    	DoChangeGC (pGC, GCGraphicsExposures, &bool, FALSE);
	    }
	    break;
	case MultibufferUpdateActionCopied:
	    ValidateGC (pPrevPixmap, pGC);
	    (*pGC->ops->CopyArea) (pNewPixmap, pPrevPixmap, pGC,
				   0, 0, pWin->drawable.width, pWin->drawable.height,
				   0, 0);
	    break;
	}
	ValidateGC (pWin, pGC);
	(*pGC->ops->CopyArea) (pNewPixmap, pWin, pGC,
			       0, 0, pWin->drawable.width, pWin->drawable.height,
			       0, 0);
	ppMultibuffers[i]->lastUpdate = currentTime;
	MultibufferUpdate (pMultibuffer[i], ppMultibuffers[i]->lastUpdate.milliseconds);
	AliasMultibuffer (ppMultibuffers[i], pMultibuffer[i] - ppMultibuffers[i]->buffers);
	FreeScratchGC (pGC);
    }
    return;
}

static DisplayRequestPtr    pPendingRequests;

static void
DisposeDisplayRequest (pRequest)
    DisplayRequestPtr	pRequest;
{
    DisplayRequestPtr	pReq, pPrev;

    pPrev = 0;
    for (pReq = pPendingRequests; pReq; pReq = pReq->next)
	if (pReq == pRequest)
	{
	    if (pPrev)
		pPrev->next = pReq->next;
	    else
		pPendingRequests = pReq->next;
	    xfree (pReq);
	    break;
	}
}

static Bool
QueueDisplayRequest (client, activateTime)
    ClientPtr	    client;
    TimeStamp	    activateTime;
{
    DisplayRequestPtr	pRequest, pReq, pPrev;

    if (!BlockHandlerRegistered)
    {
	if (!RegisterBlockAndWakeupHandlers (MultibufferBlockHandler,
					     MultibufferWakeupHandler,
					     (pointer) 0))
	{
	    return FALSE;
	}
	BlockHandlerRegistered = TRUE;
    }
    pRequest = (DisplayRequestPtr) xalloc (sizeof (DisplayRequestRec));
    if (!pRequest)
	return FALSE;
    pRequest->pClient = client;
    pRequest->activateTime = activateTime;
    pRequest->id = FakeClientID (client->index);
    if (!AddResource (pRequest->id, DisplayRequestResType, (pointer) pRequest))
    {
	xfree (pRequest);
	return FALSE;
    }
    pPrev = 0;
    for (pReq = pPendingRequests; pReq; pReq = pReq->next)
    {
	if (CompareTimeStamps (pReq->activateTime, activateTime) == LATER)
	    break;
	pPrev = pReq;
    }
    if (pPrev)
	pPrev->next = pRequest;
    else
	pPendingRequests = pRequest;
    pRequest->next = pReq;
    if (client->swapped)
    {
    	register int    n;
    	REQUEST (xMbufDisplayImageBuffersReq);
    	
    	SwapRestL(stuff);
    	swaps (&stuff->length, n);
    	swaps (&stuff->minDelay, n);
    	swaps (&stuff->maxDelay, n);
    }
    client->sequence--;
    ResetCurrentRequest (client);
    IgnoreClient (client);
    return TRUE;
}

static void
MultibufferBlockHandler (data, wt, LastSelectMask)
    pointer	    data;		/* unused */
    struct timeval  **wt;		/* wait time */
    long	    *LastSelectMask;
{
    DisplayRequestPtr	    pReq, pNext;
    unsigned long	    newdelay, olddelay;
    static struct timeval   delay_val;

    if (!pPendingRequests)
	return;
    UpdateCurrentTimeIf ();
    for (pReq = pPendingRequests; pReq; pReq = pNext)
    {
	pNext = pReq->next;
	if (CompareTimeStamps (pReq->activateTime, currentTime) == LATER)
	    break;
	AttendClient (pReq->pClient);
	FreeResource (pReq->id, 0);
    }
    pReq = pPendingRequests;
    if (!pReq)
	return;
    newdelay = pReq->activateTime.milliseconds - currentTime.milliseconds;
    if (*wt == NULL)
    {
	delay_val.tv_sec = newdelay / 1000;
	delay_val.tv_usec = 1000 * (newdelay % 1000);
	*wt = &delay_val;
    }
    else
    {
	olddelay = (*wt)->tv_sec * 1000 + (*wt)->tv_usec / 1000;
	if (newdelay < olddelay)
	{
	    (*wt)->tv_sec = newdelay / 1000;
	    (*wt)->tv_usec = 1000 * (newdelay % 1000);
	}
    }
}

static void
MultibufferWakeupHandler (data, i, LastSelectMask)
    pointer	    data;
    int		    i;
    long	    *LastSelectMask;
{
    DisplayRequestPtr	pReq, pNext;

    if (!pPendingRequests)
    {
	RemoveBlockAndWakeupHandlers (MultibufferBlockHandler,
				      MultibufferWakeupHandler,
				      (pointer) 0);
	BlockHandlerRegistered = 0;
	return;
    }
    UpdateCurrentTimeIf ();
    for (pReq = pPendingRequests; pReq; pReq = pNext)
    {
	pNext = pReq->next;
	if (CompareTimeStamps (pReq->activateTime, currentTime) == LATER)
	    break;
	AttendClient (pReq->pClient);
	FreeResource (pReq->id, 0);
    }
}

/*
 * Deliver events to a buffer
 */

static int
DeliverEventsToMultibuffer (pMultibuffer, pEvents, count, filter)
    MultibufferPtr	pMultibuffer;
    xEvent	*pEvents;
    int		count;
{
    int deliveries = 0, nondeliveries = 0;
    int attempt;
    OtherClients *other;

    if (!((pMultibuffer->otherEventMask|pMultibuffer->eventMask) & filter))
	return 0;
    if (attempt = TryClientEvents(
	bClient(pMultibuffer), pEvents, count, pMultibuffer->eventMask, filter, (GrabPtr) 0))
    {
	if (attempt > 0)
	    deliveries++;
	else
	    nondeliveries--;
    }
    for (other = pMultibuffer->otherClients; other; other=other->next)
    {
	if (attempt = TryClientEvents(
	      rClient(other), pEvents, count, other->mask, filter, (GrabPtr) 0))
	{
	    if (attempt > 0)
		deliveries++;
	    else
		nondeliveries--;
	}
    }
    if (deliveries)
	return deliveries;
    return nondeliveries;
}

/*
 * Send Expose events to interested clients
 */

static void
MultibufferExpose (pMultibuffer, pRegion)
    MultibufferPtr	pMultibuffer;
    RegionPtr	pRegion;
{
    if (pRegion && !REGION_NIL(pRegion))
    {
	xEvent *pEvent;
	PixmapPtr   pPixmap;
	register xEvent *pe;
	register BoxPtr pBox;
	register int i;
	int numRects;

	pPixmap = pMultibuffer->pPixmap;
	(* pPixmap->drawable.pScreen->TranslateRegion)(pRegion,
		    -pPixmap->drawable.x, -pPixmap->drawable.y);
	/* XXX MultibufferExpose "knows" the region representation */
	numRects = REGION_NUM_RECTS(pRegion);
	pBox = REGION_RECTS(pRegion);

	pEvent = (xEvent *) ALLOCATE_LOCAL(numRects * sizeof(xEvent));
	if (pEvent) {
	    pe = pEvent;

	    for (i=1; i<=numRects; i++, pe++, pBox++)
	    {
		pe->u.u.type = Expose;
		pe->u.expose.window = pPixmap->drawable.id;
		pe->u.expose.x = pBox->x1;
		pe->u.expose.y = pBox->y1;
		pe->u.expose.width = pBox->x2 - pBox->x1;
		pe->u.expose.height = pBox->y2 - pBox->y1;
		pe->u.expose.count = (numRects - i);
	    }
	    (void) DeliverEventsToMultibuffer (pMultibuffer, pEvent, numRects, ExposureMask);
	    DEALLOCATE_LOCAL(pEvent);
	}
    }
}

static void
MultibufferUpdate (pMultibuffer, time)
    MultibufferPtr	pMultibuffer;
    CARD32	time;
{
    xMbufUpdateNotifyEvent	event;

    event.type = MultibufferEventBase + MultibufferUpdateNotify;
    event.buffer = pMultibuffer->pPixmap->drawable.id;
    event.timeStamp = time;
    (void) DeliverEventsToMultibuffer (pMultibuffer, (xEvent *)&event,
				       1, MultibufferUpdateNotifyMask);
}

/*
 * The sample implementation will never generate MultibufferClobberNotify
 * events
 */

static void
MultibufferClobber (pMultibuffer)
    MultibufferPtr	pMultibuffer;
{
    xMbufClobberNotifyEvent	event;

    event.type = MultibufferEventBase + MultibufferClobberNotify;
    event.buffer = pMultibuffer->pPixmap->drawable.id;
    event.state = pMultibuffer->clobber;
    (void) DeliverEventsToMultibuffer (pMultibuffer, (xEvent *)&event,
				       1, MultibufferClobberNotifyMask);
}

/*
 * make the resource id for buffer i refer to the window
 * drawable instead of the pixmap;
 */

static void
AliasMultibuffer (pMultibuffers, i)
    MultibuffersPtr	pMultibuffers;
    int		i;
{
    MultibufferPtr	pMultibuffer;

    if (i == pMultibuffers->displayedMultibuffer)
	return;
    /*
     * remove the old association
     */
    if (pMultibuffers->displayedMultibuffer >= 0)
    {
	pMultibuffer = &pMultibuffers->buffers[pMultibuffers->displayedMultibuffer];
	ChangeResourceValue (pMultibuffer->pPixmap->drawable.id,
			     MultibufferDrawableResType,
 			     (pointer) pMultibuffer->pPixmap);
    }
    /*
     * make the new association
     */
    pMultibuffer = &pMultibuffers->buffers[i];
    ChangeResourceValue (pMultibuffer->pPixmap->drawable.id,
			 MultibufferDrawableResType,
			 (pointer) pMultibuffers->pWindow);
    pMultibuffers->displayedMultibuffer = i;
}

/*
 * free everything associated with multibuffering for this
 * window
 */

static void
DisposeMultibuffers (pWin)
    WindowPtr	pWin;
{
    FreeResourceByType (pWin->drawable.id, MultibuffersResType, FALSE);
    /* Zero out the window's pointer to the buffers so they won't be reused */
    pWin->devPrivates[MultibufferWindowIndex].ptr = NULL;
}

/*
 * resize the buffers when the window is resized
 */ 

static Bool
MultibufferPositionWindow (pWin, x, y)
    WindowPtr	pWin;
    int		x, y;
{
    ScreenPtr	    pScreen;
    MultibufferScreenPtr pMultibufferScreen;
    MultibuffersPtr	    pMultibuffers;
    MultibufferPtr	    pMultibuffer;
    int		    width, height;
    int		    i;
    int		    dx, dy, dw, dh;
    int		    sourcex, sourcey;
    int		    destx, desty;
    PixmapPtr	    pPixmap;
    GCPtr	    pGC;
    int		    savewidth, saveheight;
    xRectangle	    clearRect;
    Bool	    clear;

    pScreen = pWin->drawable.pScreen;
    pMultibufferScreen = (MultibufferScreenPtr) pScreen->devPrivates[MultibufferScreenIndex].ptr;
    (*pMultibufferScreen->PositionWindow) (pWin, x, y);
    if (!(pMultibuffers = (MultibuffersPtr) pWin->devPrivates[MultibufferWindowIndex].ptr))
	return TRUE;
    if (pMultibuffers->width == pWin->drawable.width &&
        pMultibuffers->height == pWin->drawable.height)
	return TRUE;
    width = pWin->drawable.width;
    height = pWin->drawable.height;
    dx = pWin->drawable.x - pMultibuffers->x;
    dy = pWin->drawable.x - pMultibuffers->y;
    dw = width - pMultibuffers->width;
    dh = height - pMultibuffers->height;
    GravityTranslate (0, 0, -dx, -dy, dw, dh,
		      pWin->bitGravity, &destx, &desty);
    clear = pMultibuffers->width < width || pMultibuffers->height < height ||
	    pWin->bitGravity == ForgetGravity;

    sourcex = 0;
    sourcey = 0;
    savewidth = pMultibuffers->width;
    saveheight = pMultibuffers->height;
    /* clip rectangle to source and destination */
    if (destx < 0)
    {
	savewidth += destx;
	sourcex -= destx;
	destx = 0;
    }
    if (destx + savewidth > width)
	savewidth = width - destx;
    if (desty < 0)
    {
	saveheight += desty;
	sourcey -= desty;
	desty = 0;
    }
    if (desty + saveheight > height)
	saveheight = height - desty;

    pMultibuffers->width = width;
    pMultibuffers->height = height;
    pMultibuffers->x = pWin->drawable.x;
    pMultibuffers->y = pWin->drawable.y;

    pGC = GetScratchGC (pWin->drawable.depth, pScreen);
    if (clear)
    {
	SetupBackgroundPainter (pWin, pGC);
	clearRect.x = 0;
	clearRect.y = 0;
	clearRect.width = width;
	clearRect.height = height;
    }
    for (i = 0; i < pMultibuffers->numMultibuffer; i++)
    {
	pMultibuffer = &pMultibuffers->buffers[i];
	pPixmap = (*pScreen->CreatePixmap) (pScreen, width, height, pWin->drawable.depth);
	if (!pPixmap)
	{
	    DisposeMultibuffers (pWin);
	    break;
	}
	ValidateGC (pPixmap, pGC);
	/*
	 * I suppose this could avoid quite a bit of work if
	 * it computed the minimal area required.
	 */
	if (clear)
	    (*pGC->ops->PolyFillRect) (pPixmap, pGC, 1, &clearRect);
	if (pWin->bitGravity != ForgetGravity)
	{
	    (*pGC->ops->CopyArea) (pMultibuffer->pPixmap, pPixmap, pGC,
				    sourcex, sourcey, savewidth, saveheight,
				    destx, desty);
	}
	pPixmap->drawable.id = pMultibuffer->pPixmap->drawable.id;
	(*pScreen->DestroyPixmap) (pMultibuffer->pPixmap);
	pMultibuffer->pPixmap = pPixmap;
	if (i != pMultibuffers->displayedMultibuffer)
	{
	    ChangeResourceValue (pPixmap->drawable.id,
				 MultibufferDrawableResType,
				 (pointer) pPixmap);
	}
    }
    FreeScratchGC (pGC);
    return TRUE;
}

/* Resource delete func for MultibufferDrawableResType */
static void
MultibufferDrawableDelete (pDrawable, id)
    DrawablePtr	pDrawable;
    XID		id;
{
    WindowPtr	pWin;
    MultibuffersPtr	pMultibuffers;
    PixmapPtr	pPixmap;

    if (pDrawable->type == DRAWABLE_WINDOW)
    {
	pWin = (WindowPtr) pDrawable;
	pMultibuffers = (MultibuffersPtr) pWin->devPrivates[MultibufferWindowIndex].ptr;
	pPixmap = pMultibuffers->buffers[pMultibuffers->displayedMultibuffer].pPixmap;
    }
    else
    {
	pPixmap = (PixmapPtr) pDrawable;
    }
    (*pPixmap->drawable.pScreen->DestroyPixmap) (pPixmap);
}

/* Resource delete func for MultibufferResType */
static void
MultibufferDelete (pMultibuffer, id)
    MultibufferPtr	pMultibuffer;
    XID		id;
{
    return;
}

/* Resource delete func for MultibuffersResType */
static void
MultibuffersDelete (pMultibuffers, id)
    MultibuffersPtr	pMultibuffers;
    XID		id;
{
    int	i;

    for (i = 0; i < pMultibuffers->numMultibuffer; i++)
	FreeResource (pMultibuffers->buffers[i].pPixmap->drawable.id, 0);
    xfree (pMultibuffers->buffers);
    xfree (pMultibuffers);
}

/* Resource delete func for DisplayRequestResType */
static void
DisplayRequestDelete (pRequest, id)
    DisplayRequestPtr	pRequest;
    XID			id;
{
    DisposeDisplayRequest (pRequest);
}

/* Resource delete func for OtherClientResType */
static void
OtherClientDelete (pMultibuffer, id)
    MultibufferPtr	pMultibuffer;
    XID		id;
{
    register OtherClientsPtr	other, prev;

    prev = 0;
    for (other = pMultibuffer->otherClients; other; other = other->next)
    {
	if (other->resource == id)
	{
	    if (prev)
		prev->next = other->next;
	    else
		pMultibuffer->otherClients = other->next;
	    xfree (other);
	    RecalculateMultibufferOtherEvents (pMultibuffer);
	    break;
	}
	prev = other;
    }
}

static int
EventSelectForMultibuffer (pMultibuffer, client, mask)
    MultibufferPtr	pMultibuffer;
    ClientPtr	client;
    Mask	mask;
{
    OtherClientsPtr	other;

    if (mask & ~ValidEventMasks)
    {
	client->errorValue = mask;
	return BadValue;
    }
    if (bClient (pMultibuffer) == client)
    {
	pMultibuffer->eventMask = mask;
    }
    else
    {
	for (other = pMultibuffer->otherClients; other; other = other->next)
	{
	    if (SameClient (other, client))
	    {
		if (mask == 0)
		{
		    FreeResource (other->resource, RT_NONE);
		    break;
		}
		other->mask = mask;
		break;
	    }
	}
	if (!other)
	{
	    other = (OtherClients *) xalloc (sizeof (OtherClients));
	    if (!other)
		return BadAlloc;
	    other->mask = mask;
	    other->resource = FakeClientID (client->index);
	    if (!AddResource (other->resource, OtherClientResType, (pointer) pMultibuffer))
	    {
		xfree (other);
		return BadAlloc;
	    }
	    other->next = pMultibuffer->otherClients;
	    pMultibuffer->otherClients = other;
	}
	RecalculateMultibufferOtherEvents (pMultibuffer);
    }
    return (client->noClientException);
}

static void
RecalculateMultibufferOtherEvents (pMultibuffer)
    MultibufferPtr	pMultibuffer;
{
    Mask	    otherEventMask;
    OtherClients    *other;

    otherEventMask = 0L;
    for (other = pMultibuffer->otherClients; other; other = other->next)
	otherEventMask |= other->mask;
    pMultibuffer->otherEventMask = otherEventMask;
}

/* add milliseconds to a timestamp */
static void
BumpTimeStamp (ts, inc)
TimeStamp   *ts;
CARD32	    inc;
{
    CARD32  newms;

    newms = ts->milliseconds + inc;
    if (newms < ts->milliseconds)
	ts->months++;
    ts->milliseconds = newms;
}
