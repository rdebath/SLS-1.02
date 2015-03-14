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

/* $XConsortium: window.c,v 5.77.1.1 92/08/21 15:49:07 rws Exp $ */

#include "X.h"
#define NEED_REPLIES
#define NEED_EVENTS
#include "Xproto.h"
#include "misc.h"
#include "scrnintstr.h"
#include "os.h"
#include "regionstr.h"
#include "validate.h"
#include "windowstr.h"
#include "input.h"
#include "resource.h"
#include "colormapst.h"
#include "cursorstr.h"
#include "dixstruct.h"
#include "gcstruct.h"
#include "servermd.h"

extern Bool permitOldBugs;

/******
 * Window stuff for server 
 *
 *    CreateRootWindow, CreateWindow, ChangeWindowAttributes,
 *    GetWindowAttributes, DeleteWindow, DestroySubWindows,
 *    HandleSaveSet, ReparentWindow, MapWindow, MapSubWindows,
 *    UnmapWindow, UnmapSubWindows, ConfigureWindow, CirculateWindow,
 *
 ******/

static unsigned char _back_lsb[4] = {0x88, 0x22, 0x44, 0x11};
static unsigned char _back_msb[4] = {0x11, 0x44, 0x22, 0x88};

typedef struct _ScreenSaverStuff {
    WindowPtr pWindow;
    XID       wid;
    BYTE      blanked;
} ScreenSaverStuffRec;

#define SCREEN_IS_BLANKED   0
#define SCREEN_ISNT_SAVED   1
#define SCREEN_IS_TILED     2
#define SCREEN_IS_BLACK	    3

#define HasSaverWindow(i)   (savedScreenInfo[i].pWindow != NullWindow)

extern int ScreenSaverBlanking, ScreenSaverAllowExposures;
int screenIsSaved = SCREEN_SAVER_OFF;

static ScreenSaverStuffRec savedScreenInfo[MAXSCREENS];

extern WindowPtr *WindowTable;
extern void (* ReplySwapVector[256]) ();

static void ResizeChildrenWinSize();
extern void CheckCursorConfinement();
extern void DeleteWindowFromAnySelections();
extern void DeleteWindowFromAnyEvents();
extern Mask EventMaskForClient();
extern void WindowHasNewCursor();
extern void RecalculateDeliverableEvents();
extern int rand();
static Bool MarkOverlappedWindows();
static void SetWinSize(), SetBorderSize();
static Bool TileScreenSaver();

#define INPUTONLY_LEGAL_MASK (CWWinGravity | CWEventMask | \
			      CWDontPropagate | CWOverrideRedirect | CWCursor )

#define BOXES_OVERLAP(b1, b2) \
      (!( ((b1)->x2 <= (b2)->x1)  || \
        ( ((b1)->x1 >= (b2)->x2)) || \
        ( ((b1)->y2 <= (b2)->y1)) || \
        ( ((b1)->y1 >= (b2)->y2)) ) )

#define RedirectSend(pWin) \
    ((pWin->eventMask|wOtherEventMasks(pWin)) & SubstructureRedirectMask)

#define SubSend(pWin) \
    ((pWin->eventMask|wOtherEventMasks(pWin)) & SubstructureNotifyMask)

#define StrSend(pWin) \
    ((pWin->eventMask|wOtherEventMasks(pWin)) & StructureNotifyMask)

#define SubStrSend(pWin,pParent) (StrSend(pWin) || SubSend(pParent))

/*
 * For SaveUnders using backing-store. The idea is that when a window is mapped
 * with saveUnder set TRUE, any windows it obscures will have its backing
 * store turned on setting the DIXsaveUnder bit,
 * The backing-store code must be written to allow for this
 */

/*
 * this is the configuration parameter "NO_BACK_SAVE"
 * it means that any existant backing store should not 
 * be used to implement save unders.
 */

#ifndef NO_BACK_SAVE

#define DO_SAVE_UNDERS(pWin)	((pWin)->drawable.pScreen->saveUnderSupport ==\
				 USE_DIX_SAVE_UNDERS)

/*
 * saveUnderSupport is set to this magic value when using DIXsaveUnders
 */

#define USE_DIX_SAVE_UNDERS	0x40

static int numSaveUndersViewable = 0;
static int deltaSaveUndersViewable = 0;

/*-
 *-----------------------------------------------------------------------
 * CheckSubSaveUnder --
 *	Check all the inferiors of a window for coverage by saveUnder
 *	windows. Called from ChangeSaveUnder and CheckSaveUnder.
 *	This code is very inefficient.
 *
 * Results:
 *	TRUE if any windows need to have backing-store removed.
 *
 * Side Effects:
 *	Windows may have backing-store turned on or off.
 *
 *-----------------------------------------------------------------------
 */
Bool
CheckSubSaveUnder(pParent, pFirst, pRegion)
    register WindowPtr 	pParent;    	/* Parent to check */
    WindowPtr		pFirst;		/* first reconfigured window */
    RegionPtr	  	pRegion;    	/* Initial area obscured by saveUnder */
{
    register WindowPtr	pChild;	    	/* Current child */
    register ScreenPtr 	pScreen;    	/* Screen to use */
    RegionRec	  	SubRegion; 	/* Area of children obscured */
    Bool		res = FALSE;	/* result */
    Bool		subInited=FALSE;/* SubRegion initialized */

    pScreen = pParent->drawable.pScreen;
    if (pChild = pParent->firstChild)
    {
	/*
	 * build region above first changed window
	 */

	for (; pChild != pFirst; pChild = pChild->nextSib)
	    if (pChild->viewable && pChild->saveUnder)
		(* pScreen->Union) (pRegion, pRegion, &pChild->borderSize);
	
	/*
	 * check region below and including first changed window
	 */

	for (; pChild; pChild = pChild->nextSib)
	{
	    if (pChild->viewable)
	    {
		/*
		 * don't save under nephew/niece windows;
		 * use a separate region
		 */

		if (pChild->firstChild)
		{
		    if (!subInited)
		    {
			(*pScreen->RegionInit)(&SubRegion, NullBox, 0);
			subInited = TRUE;
		    }
		    (* pScreen->RegionCopy) (&SubRegion, pRegion);
		    res |= CheckSubSaveUnder(pChild, pChild->firstChild,
					     &SubRegion);
		}
		else
		{
		    res |= CheckSubSaveUnder(pChild, pChild->firstChild,
					     pRegion);
		}

		if (pChild->saveUnder)
		    (* pScreen->Union) (pRegion, pRegion, &pChild->borderSize);
	    }
	}

	if (subInited)
	    (* pScreen->RegionUninit) (&SubRegion);
    }

    /*
     * Check the state of this window.  DIX save unders are
     * enabled for viewable windows with some client expressing
     * exposure interest and which intersect the save under region
     */

    if (pParent->viewable && 
	((pParent->eventMask | wOtherEventMasks(pParent)) & ExposureMask) &&
	(*pScreen->RectIn) (pRegion, (*pScreen->RegionExtents)
					(&pParent->borderSize)) != rgnOUT)
    {
	if (!pParent->DIXsaveUnder)
	{
	    pParent->DIXsaveUnder = TRUE;
	    (* pScreen->ChangeWindowAttributes) (pParent, CWBackingStore);
	}
    }
    else
    {
	if (pParent->DIXsaveUnder)
	{
	    res = TRUE;
	    pParent->DIXsaveUnder = FALSE;
	}
    }
    return res;
}

/*-
 *-----------------------------------------------------------------------
 * CheckSaveUnder --
 *	See if a window's backing-store state should be changed because
 *	it is or is not obscured by a sibling or child window with saveUnder.
 *
 * Results:
 *	TRUE if any windows need to have backing-store removed.
 *
 * Side Effects:
 *	If the window's state should be changed, it is.
 *
 *-----------------------------------------------------------------------
 */
Bool
CheckSaveUnder (pWin)
    register WindowPtr pWin;   	/* Window to check */
{
    RegionRec	rgn;    	/* Extent of siblings with saveUnder */
    Bool	res;

    if (!deltaSaveUndersViewable && !numSaveUndersViewable)
	return FALSE;
    numSaveUndersViewable += deltaSaveUndersViewable;
    deltaSaveUndersViewable = 0;
    (* pWin->drawable.pScreen->RegionInit) (&rgn, NullBox, 1);
    res = CheckSubSaveUnder (pWin->parent, pWin->nextSib, &rgn);
    (*pWin->drawable.pScreen->RegionUninit) (&rgn);
    return res;
}


/*-
 *-----------------------------------------------------------------------
 * ChangeSaveUnder --
 *	Change the save-under state of a tree of windows. Called when
 *	a window with saveUnder TRUE is mapped/unmapped/reconfigured.
 *	
 * Results:
 *	TRUE if any windows need to have backing-store removed.
 *
 * Side Effects:
 *	Windows may have backing-store turned on or off.
 *
 *-----------------------------------------------------------------------
 */
Bool
ChangeSaveUnder(pWin, first)
    register WindowPtr 	pWin;
    WindowPtr  	  	first; 	    	/* First window to check.
					 * Used when pWin was restacked */
{
    RegionRec	rgn;  	/* Area obscured by saveUnder windows */
    register ScreenPtr pScreen;
    Bool	res;

    if (!deltaSaveUndersViewable && !numSaveUndersViewable)
	return FALSE;
    numSaveUndersViewable += deltaSaveUndersViewable;
    deltaSaveUndersViewable = 0;
    pScreen = pWin->drawable.pScreen;
    (* pScreen->RegionInit) (&rgn, NullBox, 1);
    res = CheckSubSaveUnder (pWin->parent, first, &rgn);
    (* pScreen->RegionUninit) (&rgn);
    return res;
}

/*-
 *-----------------------------------------------------------------------
 * DoChangeSaveUnder --
 *	Actually turn backing-store off for those windows that no longer
 *	need to have it on.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Backing-store and SAVE_UNDER_CHANGE_BIT are turned off for those
 *	windows affected.
 *
 *-----------------------------------------------------------------------
 */
void
DoChangeSaveUnder(pWin, pFirst)
    WindowPtr	  	pWin;
    WindowPtr		pFirst;
{
    register WindowPtr pChild;
    Bool (* ChangeWindowAttributes)();

    ChangeWindowAttributes = pWin->drawable.pScreen->ChangeWindowAttributes;
    if (!pWin->DIXsaveUnder &&
	(pWin->backingStore == NotUseful) && pWin->backStorage)
	(*ChangeWindowAttributes)(pWin, CWBackingStore);
    if (!(pChild = pFirst))
	return;
    while (1)
    {
	if (!pChild->DIXsaveUnder &&
	    (pChild->backingStore == NotUseful) && pChild->backStorage)
	    (*ChangeWindowAttributes)(pChild, CWBackingStore);
	if (pChild->firstChild)
	{
	    pChild = pChild->firstChild;
	    continue;
	}
	while (!pChild->nextSib)
	{
	    pChild = pChild->parent;
	    if (pChild == pWin)
		return;
	}
	pChild = pChild->nextSib;
    }
}
#endif /* DO_SAVE_UNDER */

#ifdef DEBUG
/******
 * PrintWindowTree
 *    For debugging only
 ******/

int
PrintChildren(p1, indent)
    WindowPtr p1;
    int indent;
{
    WindowPtr p2;
    int i;

    while (p1)
    {
        p2 = p1->firstChild;
        for (i=0; i<indent; i++) ErrorF( " ");
	ErrorF( "%x\n", p1->drawable.id);
        miPrintRegion(&p1->clipList);
	PrintChildren(p2, indent+4);
	p1 = p1->nextSib;
    }
}

PrintWindowTree()
{
    int i;
    WindowPtr pWin, p1;

    for (i=0; i<screenInfo.numScreens; i++)
    {
	ErrorF( "WINDOW %d\n", i);
	pWin = WindowTable[i];
        miPrintRegion(&pWin->clipList);
	p1 = pWin->firstChild;
	PrintChildren(p1, 4);
    }
}
#endif

/*
 * allocate an entry in the per-window private data structures
 */

static int  windowPrivateCount;

void
ResetWindowPrivates()
{
    windowPrivateCount = 0;
}

int
AllocateWindowPrivateIndex()
{
    return windowPrivateCount++;
}

int
TraverseTree(pWin, func, data)
    register WindowPtr pWin;
    int (*func)();
    pointer data;
{
    register int result;
    register WindowPtr pChild;

    if (!(pChild = pWin))
       return(WT_NOMATCH);
    while (1)
    {
	result = (* func)(pChild, data);
	if (result == WT_STOPWALKING)
	    return(WT_STOPWALKING);
	if ((result == WT_WALKCHILDREN) && pChild->firstChild)
	{
	    pChild = pChild->firstChild;
	    continue;
	}
	while (!pChild->nextSib && (pChild != pWin))
	    pChild = pChild->parent;
	if (pChild == pWin)
	    break;
	pChild = pChild->nextSib;
    }
    return(WT_NOMATCH);
}

/*****
 * WalkTree
 *   Walk the window tree, for SCREEN, preforming FUNC(pWin, data) on
 *   each window.  If FUNC returns WT_WALKCHILDREN, traverse the children,
 *   if it returns WT_DONTWALKCHILDREN, dont.  If it returns WT_STOPWALKING
 *   exit WalkTree.  Does depth-first traverse.
 *****/

int
WalkTree(pScreen, func, data)
    ScreenPtr pScreen;
    int (* func)();
    pointer data;
{
    return(TraverseTree(WindowTable[pScreen->myNum], func, data));
}

/*****
 *  HandleExposures(pWin)
 *    starting at pWin, draw background in any windows that have exposure
 *    regions, translate the regions, restore any backing store,
 *    and then send any regions stille xposed to the client
 *****/

static void
HandleExposures(pWin)
    WindowPtr pWin;
{
    register WindowPtr pChild;
    register ValidatePtr val;
    Bool (* RegionNotEmpty)();
    void (* RegionUninit)();
    void (* WindowExposures)();

    pChild = pWin;
    RegionNotEmpty = pChild->drawable.pScreen->RegionNotEmpty;
    RegionUninit = pChild->drawable.pScreen->RegionUninit;
    WindowExposures = pChild->drawable.pScreen->WindowExposures;
    while (1)
    {
	if (val = pChild->valdata)
	{
	    if ((*RegionNotEmpty)(&val->after.borderExposed))
		(*pChild->drawable.pScreen->PaintWindowBorder)(pChild,
						    &val->after.borderExposed,
						    PW_BORDER);
	    (*RegionUninit)(&val->after.borderExposed);
	    (*WindowExposures)(pChild, &val->after.exposed, NullRegion);
	    (*RegionUninit)(&val->after.exposed);
	    xfree(val);
	    pChild->valdata = (ValidatePtr)NULL;
	    if (pChild->firstChild)
	    {
		pChild = pChild->firstChild;
		continue;
	    }
	}
	while (!pChild->nextSib && (pChild != pWin))
	    pChild = pChild->parent;
	if (pChild == pWin)
	    break;
	pChild = pChild->nextSib;
    }
}

/* hack for forcing backing store on all windows */
int	defaultBackingStore = NotUseful;
/* hack to force no backing store */
Bool	disableBackingStore = FALSE;
/* hack to force no save unders */
Bool	disableSaveUnders = FALSE;

static void
SetWindowToDefaults(pWin)
    register WindowPtr pWin;
{
    pWin->prevSib = NullWindow;
    pWin->firstChild = NullWindow;
    pWin->lastChild = NullWindow;

    pWin->valdata = (ValidatePtr)NULL;
    pWin->optional = (WindowOptPtr)NULL;
    pWin->cursorIsNone = TRUE;

    pWin->backingStore = NotUseful;
    pWin->DIXsaveUnder = FALSE;
    pWin->backStorage = (pointer) NULL;

    pWin->mapped = FALSE;           /* off */
    pWin->realized = FALSE;     /* off */
    pWin->viewable = FALSE;
    pWin->visibility = VisibilityNotViewable;
    pWin->overrideRedirect = FALSE;
    pWin->saveUnder = FALSE;

    pWin->bitGravity = ForgetGravity;
    pWin->winGravity = NorthWestGravity;

    pWin->eventMask = 0;
    pWin->deliverableEvents = 0;
    pWin->dontPropagate = 0;
}

static void
MakeRootTile(pWin)
    WindowPtr pWin;
{
    ScreenPtr pScreen = pWin->drawable.pScreen;
    GCPtr pGC;
    unsigned char back[128];
    int len = PixmapBytePad(4, 1);
    register unsigned char *from, *to;
    register int i, j;

    pWin->background.pixmap = (*pScreen->CreatePixmap)(pScreen, 4, 4,
						    pScreen->rootDepth);

    pWin->backgroundState = BackgroundPixmap;
    pGC = GetScratchGC(pScreen->rootDepth, pScreen);
    if (!pWin->background.pixmap || !pGC)
	FatalError("cound not create root tile");

    {
	CARD32 attributes[2];

	attributes[0] = pScreen->whitePixel;
	attributes[1] = pScreen->blackPixel;

	(void)ChangeGC(pGC, GCForeground | GCBackground, attributes);
    }

   ValidateGC((DrawablePtr)pWin->background.pixmap, pGC);

   from = (screenInfo.bitmapBitOrder == LSBFirst) ? _back_lsb : _back_msb;
   to = back;

   for (i = 4; i > 0; i--, from++)
	for (j = len; j > 0; j--)
	    *to++ = *from;

   (*pGC->ops->PutImage)(pWin->background.pixmap, pGC, 1,
	            0, 0, 4, 4, 0, XYBitmap, back);

   FreeScratchGC(pGC);

}

static WindowPtr
AllocateWindow(pScreen)
    ScreenPtr pScreen;
{
    WindowPtr pWin;
    register char *ptr;
    register DevUnion *ppriv;
    register unsigned *sizes;
    register unsigned size;
    register int i;

    pWin = (WindowPtr)xalloc(pScreen->totalWindowSize);
    if (pWin)
    {
	ppriv = (DevUnion *)(pWin + 1);
	pWin->devPrivates = ppriv;
	sizes = pScreen->WindowPrivateSizes;
	ptr = (char *)(ppriv + pScreen->WindowPrivateLen);
	for (i = pScreen->WindowPrivateLen; --i >= 0; ppriv++, sizes++)
	{
	    if (size = *sizes)
	    {
		ppriv->ptr = (pointer)ptr;
		ptr += size;
	    }
	    else
		ppriv->ptr = (pointer)NULL;
	}
    }
    return pWin;
}

/*****
 * CreateRootWindow
 *    Makes a window at initialization time for specified screen
 *****/

Bool
CreateRootWindow(pScreen)
    ScreenPtr	pScreen;
{
    WindowPtr	pWin;
    BoxRec	box;
    PixmapFormatRec *format;

    pWin = AllocateWindow(pScreen);
    if (!pWin)
	return FALSE;

    savedScreenInfo[pScreen->myNum].pWindow = NULL;
    savedScreenInfo[pScreen->myNum].wid = FakeClientID(0);
    screenIsSaved = SCREEN_SAVER_OFF;

    WindowTable[pScreen->myNum] = pWin;

    pWin->drawable.pScreen = pScreen;
    pWin->drawable.type = DRAWABLE_WINDOW;

    pWin->drawable.depth = pScreen->rootDepth;
    for (format = screenInfo.formats;
	 format->depth != pScreen->rootDepth;
	 format++)
	;
    pWin->drawable.bitsPerPixel = format->bitsPerPixel;

    pWin->drawable.serialNumber = NEXT_SERIAL_NUMBER;

    pWin->parent = NullWindow;
    SetWindowToDefaults(pWin);

    pWin->optional = (WindowOptRec *) xalloc (sizeof (WindowOptRec));

    pWin->optional->dontPropagateMask = 0;
    pWin->optional->otherEventMasks = 0;
    pWin->optional->otherClients = NULL;
    pWin->optional->passiveGrabs = NULL;
    pWin->optional->userProps = NULL;
    pWin->optional->backingBitPlanes = ~0L;
    pWin->optional->backingPixel = 0;
#ifdef SHAPE
    pWin->optional->boundingShape = NULL;
    pWin->optional->clipShape = NULL;
#endif
#ifdef XINPUT
    pWin->optional->inputMasks = NULL;
#endif
    pWin->optional->colormap = pScreen->defColormap;
    pWin->optional->visual = pScreen->rootVisual;

    pWin->nextSib = NullWindow;

    pWin->drawable.id = FakeClientID(0);

    pWin->origin.x = pWin->origin.y = 0;
    pWin->drawable.height = pScreen->height;
    pWin->drawable.width = pScreen->width;
    pWin->drawable.x = pWin->drawable.y = 0;

    box.x1 = 0;
    box.y1 = 0;
    box.x2 = pScreen->width;
    box.y2 = pScreen->height;
    (* pScreen->RegionInit)(&pWin->clipList, &box, 1);
    (* pScreen->RegionInit)(&pWin->winSize, &box, 1);
    (* pScreen->RegionInit)(&pWin->borderSize, &box, 1);
    (* pScreen->RegionInit)(&pWin->borderClip, &box, 1);

    pWin->drawable.class = InputOutput;
    pWin->optional->visual = pScreen->rootVisual;

    pWin->backgroundState = BackgroundPixel;
    pWin->background.pixel = pScreen->whitePixel;

    pWin->borderIsPixel = TRUE;
    pWin->border.pixel = pScreen->blackPixel;
    pWin->borderWidth = 0;

    if (!AddResource(pWin->drawable.id, RT_WINDOW, (pointer)pWin))
	return FALSE;

    if (disableBackingStore)
	pScreen->backingStoreSupport = NotUseful;

#ifdef DO_SAVE_UNDERS
    if ((pScreen->backingStoreSupport != NotUseful) &&
	(pScreen->saveUnderSupport == NotUseful))
    {
	/*
	 * If the screen has backing-store but no save-unders, let the
	 * clients know we can support save-unders using backing-store.
	 */
	pScreen->saveUnderSupport = USE_DIX_SAVE_UNDERS;
    }
#endif /* DO_SAVE_UNDERS */
		
    if (disableSaveUnders)
	pScreen->saveUnderSupport = NotUseful;

    return TRUE;
}

InitRootWindow(pWin)
    WindowPtr pWin;
{
    ScreenPtr pScreen;

    pScreen = pWin->drawable.pScreen;
    if (!(*pScreen->CreateWindow)(pWin))
	return; /* XXX */
    (*pScreen->PositionWindow)(pWin, 0, 0);

    pWin->cursorIsNone = FALSE;
    pWin->optional->cursor = rootCursor;
    rootCursor->refcnt++;
    MakeRootTile(pWin);
    pWin->backingStore = defaultBackingStore;
    /* We SHOULD check for an error value here XXX */
    (*pScreen->ChangeWindowAttributes)(pWin,
		       CWBackPixmap|CWBorderPixel|CWCursor|CWBackingStore);

    MapWindow(pWin, serverClient);
}

/* Set the region to the intersection of the rectangle and the
 * window's winSize.  The window is typically the parent of the
 * window from which the region came.
 */

ClippedRegionFromBox(pWin, Rgn, x, y, w, h)
    register WindowPtr pWin;
    RegionPtr Rgn;
    register int x, y;
    int w, h;
{
    register ScreenPtr pScreen = pWin->drawable.pScreen;
    BoxRec box;

    box = *((* pScreen->RegionExtents)(&pWin->winSize));
    /* we do these calculations to avoid overflows */
    if (x > box.x1)
	box.x1 = x;
    if (y > box.y1)
	box.y1 = y;
    x += w;
    if (x < box.x2)
	box.x2 = x;
    y += h;
    if (y < box.y2)
	box.y2 = y;
    if (box.x1 > box.x2)
	box.x2 = box.x1;
    if (box.y1 > box.y2)
	box.y2 = box.y1;
    (* pScreen->RegionReset)(Rgn, &box);
    (* pScreen->Intersect)(Rgn, Rgn, &pWin->winSize);
}

WindowPtr
RealChildHead(pWin)
    register WindowPtr pWin;
{
    if (!pWin->parent &&
	(screenIsSaved == SCREEN_SAVER_ON) &&
	(HasSaverWindow (pWin->drawable.pScreen->myNum)))
	return (pWin->firstChild);
    else
	return (NullWindow);
}

/*****
 * CreateWindow
 *    Makes a window in response to client request 
 *****/

WindowPtr
CreateWindow(wid, pParent, x, y, w, h, bw, class, vmask, vlist,
	     depth, client, visual, error)
    Window wid;
    register WindowPtr pParent;
    short x,y;
    unsigned short w, h, bw;
    unsigned short class;
    register Mask vmask;
    XID *vlist;
    int depth;
    ClientPtr client;
    VisualID visual;
    int *error;
{
    register WindowPtr pWin;
    WindowPtr pHead;
    register ScreenPtr pScreen;
    xEvent event;
    int idepth, ivisual;
    Bool fOK;
    DepthPtr pDepth;
    PixmapFormatRec *format;
    register WindowOptPtr ancwopt;

    if (class == CopyFromParent)
	class = pParent->drawable.class;

    if ((class != InputOutput) && (class != InputOnly))
    {
	*error = BadValue;
	client->errorValue = class;
	return NullWindow;
    }

    if ((class != InputOnly) && (pParent->drawable.class == InputOnly))
    {
        *error = BadMatch;
	return NullWindow;
    }

    if ((class == InputOnly) && ((bw != 0) || (depth != 0)))
    {
        *error = BadMatch;
	return NullWindow;
    }

    pScreen = pParent->drawable.pScreen;

    if ((class == InputOutput) && (depth == 0))
        depth = pParent->drawable.depth;
    ancwopt = pParent->optional;
    if (!ancwopt)
	ancwopt = FindWindowWithOptional(pParent)->optional;
    if (visual == CopyFromParent)
	visual = ancwopt->visual;

    /* Find out if the depth and visual are acceptable for this Screen */
    if ((visual != ancwopt->visual) || (depth != pParent->drawable.depth))
    {
	fOK = FALSE;
	for(idepth = 0; idepth < pScreen->numDepths; idepth++)
	{
	    pDepth = (DepthPtr) &pScreen->allowedDepths[idepth];
	    if ((depth == pDepth->depth) || (depth == 0))
	    {
		for (ivisual = 0; ivisual < pDepth->numVids; ivisual++)
		{
		    if (visual == pDepth->vids[ivisual])
		    {
			fOK = TRUE;
			break;
		    }
		}
	    }
	}
	if (fOK == FALSE)
	{
	    *error = BadMatch;
	    return NullWindow;
	}
    }

    if (((vmask & (CWBorderPixmap | CWBorderPixel)) == 0) &&
	(class != InputOnly) &&
	(depth != pParent->drawable.depth))
    {
        *error = BadMatch;
        return NullWindow;
    }

    if (((vmask & CWColormap) == 0) &&
	(class != InputOnly) &&
	((visual != ancwopt->visual) || (ancwopt->colormap == None)))
    {
	*error = BadMatch;
        return NullWindow;
    }

    pWin = AllocateWindow(pScreen);
    if (!pWin)
    {
	*error = BadAlloc;
        return NullWindow;
    }
    pWin->drawable = pParent->drawable;
    pWin->drawable.depth = depth;
    if (depth == pParent->drawable.depth)
	pWin->drawable.bitsPerPixel = pParent->drawable.bitsPerPixel;
    else
    {
	for (format = screenInfo.formats; format->depth != depth; format++)
	    ;
	pWin->drawable.bitsPerPixel = format->bitsPerPixel;
    }
    if (class == InputOnly)
        pWin->drawable.type = (short) UNDRAWABLE_WINDOW;
    pWin->drawable.serialNumber = NEXT_SERIAL_NUMBER;

    pWin->drawable.id = wid;
    pWin->drawable.class = class;

    pWin->parent = pParent;
    SetWindowToDefaults(pWin);

    if (visual != ancwopt->visual)
    {
	if (!MakeWindowOptional (pWin))
	{
	    xfree (pWin);
	    *error = BadAlloc;
	    return NullWindow;
	}
	pWin->optional->visual = visual;
	pWin->optional->colormap = None;
    }

    pWin->borderWidth = bw;
    pWin->backgroundState = None;

    pWin->borderIsPixel = pParent->borderIsPixel;
    pWin->border = pParent->border;
    if (pWin->borderIsPixel == FALSE)
	pWin->border.pixmap->refcnt++;
		
    pWin->origin.x = x + (int)bw;
    pWin->origin.y = y + (int)bw;
    pWin->drawable.width = w;
    pWin->drawable.height = h;
    pWin->drawable.x = pParent->drawable.x + x + (int)bw;
    pWin->drawable.y = pParent->drawable.y + y + (int)bw;

        /* set up clip list correctly for unobscured WindowPtr */
    (* pScreen->RegionInit)(&pWin->clipList, NullBox, 1);
    (* pScreen->RegionInit)(&pWin->borderClip, NullBox, 1);
    (* pScreen->RegionInit)(&pWin->winSize, NullBox, 1);
    (* pScreen->RegionInit)(&pWin->borderSize, NullBox, 1);

    pHead = RealChildHead(pParent);
    if (pHead)
    {
	pWin->nextSib = pHead->nextSib;
        if (pHead->nextSib)
    	    pHead->nextSib->prevSib = pWin;
	else
	    pParent->lastChild = pWin;
        pHead->nextSib = pWin;
	pWin->prevSib = pHead;
    }
    else
    {
        pWin->nextSib = pParent->firstChild;
        if (pParent->firstChild)
	    pParent->firstChild->prevSib = pWin;
        else
            pParent->lastChild = pWin;
	pParent->firstChild = pWin;
    }

    SetWinSize (pWin);
    SetBorderSize (pWin);

    /* We SHOULD check for an error value here XXX */
    if (!(*pScreen->CreateWindow)(pWin))
    {
	*error = BadAlloc;
	DeleteWindow(pWin, wid);
	return NullWindow;
    }
    /* We SHOULD check for an error value here XXX */
    (*pScreen->PositionWindow)(pWin, pWin->drawable.x, pWin->drawable.y);

    if (!(vmask & CWEventMask))
	RecalculateDeliverableEvents(pWin);

    if (vmask)
        *error = ChangeWindowAttributes(pWin, vmask, vlist, wClient (pWin));
    else
	*error = Success;

    if (*error != Success)
    {
        (void)EventSelectForWindow(pWin, client, (Mask)0); /* can't fail */
	DeleteWindow(pWin, wid);
	return NullWindow;
    }
    if (!(vmask & CWBackingStore) && (defaultBackingStore != NotUseful))
    {
        XID value = defaultBackingStore;
	(void)ChangeWindowAttributes(pWin, CWBackingStore, &value, wClient (pWin));
    }

    if (SubSend(pParent))
    {
	event.u.u.type = CreateNotify;
	event.u.createNotify.window = wid;
	event.u.createNotify.parent = pParent->drawable.id;
	event.u.createNotify.x = x;
	event.u.createNotify.y = y;
	event.u.createNotify.width = w;
	event.u.createNotify.height = h;
	event.u.createNotify.borderWidth = bw;
	event.u.createNotify.override = pWin->overrideRedirect;
	DeliverEvents(pParent, &event, 1, NullWindow);		
    }

    return pWin;
}

static void
FreeWindowResources(pWin)
    register WindowPtr pWin;
{
    register ScreenPtr pScreen;
    void (* proc)();

    pScreen = pWin->drawable.pScreen;

    DeleteWindowFromAnySaveSet(pWin);
    DeleteWindowFromAnySelections(pWin);
    DeleteWindowFromAnyEvents(pWin, TRUE);
    proc = pScreen->RegionUninit;
    (* proc)(&pWin->clipList);
    (* proc)(&pWin->winSize);
    (* proc)(&pWin->borderClip);
    (* proc)(&pWin->borderSize);
#ifdef SHAPE
    if (wBoundingShape (pWin))
	(* pScreen->RegionDestroy)(wBoundingShape (pWin));
    if (wClipShape (pWin))
	(* pScreen->RegionDestroy)(wClipShape (pWin));
#endif
    if (pWin->borderIsPixel == FALSE)
	(* pScreen->DestroyPixmap)(pWin->border.pixmap);
    if (pWin->backgroundState == BackgroundPixmap)
	(* pScreen->DestroyPixmap)(pWin->background.pixmap);

    DeleteAllWindowProperties(pWin);
    /* We SHOULD check for an error value here XXX */
    (* pScreen->DestroyWindow)(pWin);
    DisposeWindowOptional (pWin);
}

static void
CrushTree(pWin)
    WindowPtr pWin;
{
    register WindowPtr pChild, pSib, pParent;
    Bool (* UnrealizeWindow)();
    xEvent event;

    if (!(pChild = pWin->firstChild))
        return;
    UnrealizeWindow = pWin->drawable.pScreen->UnrealizeWindow;
    while (1)
    {
	if (pChild->firstChild)
	{
	    pChild = pChild->firstChild;
	    continue;
	}
	while (1)
	{
	    pParent = pChild->parent;
	    if (SubStrSend(pChild, pParent))
	    {
		event.u.u.type = DestroyNotify;
		event.u.destroyNotify.window = pChild->drawable.id;
		DeliverEvents(pChild, &event, 1, NullWindow);		
	    }
	    FreeResource(pChild->drawable.id, RT_WINDOW);
	    pSib = pChild->nextSib;
#ifdef DO_SAVE_UNDERS
	    if (pChild->saveUnder && pChild->viewable)
		deltaSaveUndersViewable--;
#endif
	    pChild->viewable = FALSE;
	    if (pChild->realized)
	    {
		pChild->realized = FALSE;
		(*UnrealizeWindow)(pChild);
	    }
	    FreeWindowResources(pChild);
	    xfree(pChild);
	    if (pChild = pSib)
		break;
	    pChild = pParent;
	    pChild->firstChild = NullWindow;
	    pChild->lastChild = NullWindow;
	    if (pChild == pWin)
		return;
	}
    }
}
	
/*****
 *  DeleteWindow
 *       Deletes child of window then window itself
 *****/

/*ARGSUSED*/
DeleteWindow(pWin, wid)
    register WindowPtr pWin;
    Window wid;
{
    register WindowPtr pParent;
    xEvent event;

    UnmapWindow(pWin, FALSE);

    CrushTree(pWin);

    pParent = pWin->parent;
    if (pParent && SubStrSend(pWin, pParent))
    {
	event.u.u.type = DestroyNotify;
	event.u.destroyNotify.window = pWin->drawable.id;
	DeliverEvents(pWin, &event, 1, NullWindow);		
    }

    FreeWindowResources(pWin);
    if (pParent)
    {
	if (pParent->firstChild == pWin)
            pParent->firstChild = pWin->nextSib;
	if (pParent->lastChild == pWin)
            pParent->lastChild = pWin->prevSib;
        if (pWin->nextSib)
            pWin->nextSib->prevSib = pWin->prevSib;
        if (pWin->prevSib)
            pWin->prevSib->nextSib = pWin->nextSib;
    }
    xfree(pWin);
}

/*ARGSUSED*/
DestroySubwindows(pWin, client)
    register WindowPtr pWin;
    ClientPtr client;
{
    /* XXX
     * The protocol is quite clear that each window should be
     * destroyed in turn, however, unmapping all of the first
     * eliminates most of the calls to ValidateTree.  So,
     * this implementation is incorrect in that all of the
     * UnmapNotifies occur before all of the DestroyNotifies.
     * If you care, simply delete the call to UnmapSubwindows.
     */
    UnmapSubwindows(pWin);
    while (pWin->lastChild)
	FreeResource(pWin->lastChild->drawable.id, RT_NONE);
}

/*****
 *  ChangeWindowAttributes
 *   
 *  The value-mask specifies which attributes are to be changed; the
 *  value-list contains one value for each one bit in the mask, from least
 *  to most significant bit in the mask.  
 *****/
 
int
ChangeWindowAttributes(pWin, vmask, vlist, client)
    register WindowPtr pWin;
    Mask vmask;
    XID *vlist;
    ClientPtr client;
{
    register Mask index;
    register XID *pVlist;
    PixmapPtr pPixmap;
    Pixmap pixID;
    CursorPtr pCursor, pOldCursor;
    Cursor cursorID;
    WindowPtr pChild;
    Colormap cmap;
    ColormapPtr	pCmap;
    xEvent xE;
    int result;
    register ScreenPtr pScreen;
    Mask vmaskCopy = 0;
    register Mask tmask;
    unsigned int val;
    int error;
    Bool checkOptional = FALSE;

    if ((pWin->drawable.class == InputOnly) && (vmask & (~INPUTONLY_LEGAL_MASK)))
        return BadMatch;

    error = Success;
    pScreen = pWin->drawable.pScreen;
    pVlist = vlist;
    tmask = vmask;
    while (tmask)
    {
	index = (Mask) lowbit (tmask);
	tmask &= ~index;
	switch (index)
        {
	  case CWBackPixmap:
	    pixID = (Pixmap )*pVlist;
	    pVlist++;
	    if (pixID == None)
	    {
		if (pWin->backgroundState == BackgroundPixmap)
		    (* pScreen->DestroyPixmap)(pWin->background.pixmap);
		if (!pWin->parent)
                    MakeRootTile(pWin);
                else
                    pWin->backgroundState = None;
	    }
	    else if (pixID == ParentRelative)
	    {
		if (pWin->parent &&
		    pWin->drawable.depth != pWin->parent->drawable.depth)
		{
		    error = BadMatch;
		    goto PatchUp;
		}
		if (pWin->backgroundState == BackgroundPixmap)
		    (* pScreen->DestroyPixmap)(pWin->background.pixmap);
		if (!pWin->parent)
		    MakeRootTile(pWin);
		else
	            pWin->backgroundState = ParentRelative;
		/* Note that the parent's backgroundTile's refcnt is NOT
		 * incremented. */
	    }
            else
	    {	
                pPixmap = (PixmapPtr)LookupIDByType(pixID, RT_PIXMAP);
                if (pPixmap != (PixmapPtr) NULL)
		{
                    if  ((pPixmap->drawable.depth != pWin->drawable.depth) ||
			 (pPixmap->drawable.pScreen != pScreen))
		    {
                        error = BadMatch;
			goto PatchUp;
		    }
		    if (pWin->backgroundState == BackgroundPixmap)
			(* pScreen->DestroyPixmap)(pWin->background.pixmap);
		    pWin->backgroundState = BackgroundPixmap;
		    pWin->background.pixmap = pPixmap;
		    pPixmap->refcnt++;
		}
	        else
		{
		    error = BadPixmap;
		    client->errorValue = pixID;
		    goto PatchUp;
		}
	    }
	    break;
	  case CWBackPixel:
	    if (pWin->backgroundState == BackgroundPixmap)
		(* pScreen->DestroyPixmap)(pWin->background.pixmap);
	    pWin->backgroundState = BackgroundPixel;
	    pWin->background.pixel = (CARD32 ) *pVlist;
	           /* background pixel overrides background pixmap,
		      so don't let the ddx layer see both bits */
            vmaskCopy &= ~CWBackPixmap;
	    pVlist++;
	    break;
	  case CWBorderPixmap:
	    pixID = (Pixmap ) *pVlist;
	    pVlist++;
	    if (pixID == CopyFromParent)
	    {
		if (!pWin->parent ||
		    (pWin->drawable.depth != pWin->parent->drawable.depth))
		{
		    error = BadMatch;
		    goto PatchUp;
		}
		if (pWin->borderIsPixel == FALSE)
		    (* pScreen->DestroyPixmap)(pWin->border.pixmap);
		pWin->border = pWin->parent->border;
		if ((pWin->borderIsPixel = pWin->parent->borderIsPixel) == TRUE)
		{
		    index = CWBorderPixel;
		}
                else
		{
		    pWin->parent->border.pixmap->refcnt++;
		}
	    }
	    else
	    {	
		pPixmap = (PixmapPtr)LookupIDByType(pixID, RT_PIXMAP);
		if (pPixmap)
		{
                    if  ((pPixmap->drawable.depth != pWin->drawable.depth) ||
			 (pPixmap->drawable.pScreen != pScreen))
		    {
			error = BadMatch;
			goto PatchUp;
		    }
		    if (pWin->borderIsPixel == FALSE)
			(* pScreen->DestroyPixmap)(pWin->border.pixmap);
		    pWin->borderIsPixel = FALSE;
		    pWin->border.pixmap = pPixmap;
		    pPixmap->refcnt++;
		}
    	        else
		{
		    error = BadPixmap;
		    client->errorValue = pixID;
		    goto PatchUp;
		}
	    }
	    break;
	  case CWBorderPixel:
	    if (pWin->borderIsPixel == FALSE)
		(* pScreen->DestroyPixmap)(pWin->border.pixmap);
	    pWin->borderIsPixel = TRUE;
            pWin->border.pixel = (CARD32) *pVlist;
		    /* border pixel overrides border pixmap,
		       so don't let the ddx layer see both bits */
	    vmaskCopy &= ~CWBorderPixmap;
	    pVlist++;
            break;
	  case CWBitGravity:
	    val = (CARD8 )*pVlist;
	    pVlist++;
	    if (val > StaticGravity)
	    {
		error = BadValue;
		client->errorValue = val;
		goto PatchUp;
	    }
	    pWin->bitGravity = val;
	    break;
	  case CWWinGravity:
	    val = (CARD8 )*pVlist;
	    pVlist++;
	    if (val > StaticGravity)
	    {
		error = BadValue;
		client->errorValue = val;
		goto PatchUp;
	    }
	    pWin->winGravity = val;
	    break;
	  case CWBackingStore:
	    val = (CARD8 )*pVlist;
	    pVlist++;
	    if ((val != NotUseful) && (val != WhenMapped) && (val != Always))
	    {
		error = BadValue;
		client->errorValue = val;
		goto PatchUp;
	    }
	    pWin->backingStore = val;
	    break;
	  case CWBackingPlanes:
	    if (pWin->optional || ((CARD32)*pVlist != ~0L)) {
		if (!pWin->optional && !MakeWindowOptional (pWin))
		{
		    error = BadAlloc;
		    goto PatchUp;
		}
		pWin->optional->backingBitPlanes = (CARD32) *pVlist;
		if ((CARD32)*pVlist == ~0L)
		    checkOptional = TRUE;
	    }
	    pVlist++;
	    break;
	  case CWBackingPixel:
	    if (pWin->optional || (CARD32) *pVlist) {
		if (!pWin->optional && !MakeWindowOptional (pWin))
		{
		    error = BadAlloc;
		    goto PatchUp;
		}
		pWin->optional->backingPixel = (CARD32) *pVlist;
		if (!*pVlist)
		    checkOptional = TRUE;
	    }
	    pVlist++;
	    break;
	  case CWSaveUnder:
	    val = (BOOL) *pVlist;
	    pVlist++;
	    if ((val != xTrue) && (val != xFalse))
	    {
		error = BadValue;
		client->errorValue = val;
		goto PatchUp;
	    }
#ifdef DO_SAVE_UNDERS
	    if ((pWin->saveUnder != val) && (pWin->viewable) &&
		DO_SAVE_UNDERS(pWin))
	    {
		/*
		 * Re-check all siblings and inferiors for obscurity or
		 * exposition (hee hee).
		 */
		if (pWin->saveUnder)
		    deltaSaveUndersViewable--;
		else
		    deltaSaveUndersViewable++;
		pWin->saveUnder = val;
		if (ChangeSaveUnder(pWin, pWin->nextSib))
		    DoChangeSaveUnder(pWin->parent, pWin->nextSib);
	    }
	    else
	    {
		pWin->saveUnder = val;
	    }
#else
	    pWin->saveUnder = val;
#endif /* DO_SAVE_UNDERS */
	    break;
	  case CWEventMask:
	    result = EventSelectForWindow(pWin, client, (Mask )*pVlist);
	    if (result)
	    {
		error = result;
		goto PatchUp;
	    }
	    pVlist++;
	    break;
	  case CWDontPropagate:
	    result = EventSuppressForWindow(pWin, client, (Mask )*pVlist,
					    &checkOptional);
	    if (result)
	    {
		error = result;
		goto PatchUp;
	    }
	    pVlist++;
	    break;
	  case CWOverrideRedirect:
	    val = (BOOL ) *pVlist;
	    pVlist++;
	    if ((val != xTrue) && (val != xFalse))
	    {
		error = BadValue;
		client->errorValue = val;
		goto PatchUp;
	    }
	    pWin->overrideRedirect = val;
	    break;
	  case CWColormap:
	    cmap = (Colormap) *pVlist;
	    pVlist++;
	    if (cmap == CopyFromParent)
	    {
		if (pWin->parent &&
		    (!pWin->optional ||
 		     pWin->optional->visual == wVisual (pWin->parent)))
		{
		    cmap = wColormap (pWin->parent);
		}
		else
		    cmap = None;
	    }
	    if (cmap == None)
	    {
		error = BadMatch;
		goto PatchUp;
	    }
	    pCmap = (ColormapPtr)LookupIDByType(cmap, RT_COLORMAP);
	    if (!pCmap)
	    {
		error = BadColor;
		client->errorValue = cmap;
		goto PatchUp;
	    }
	    if (pCmap->pVisual->vid != wVisual (pWin) ||
		pCmap->pScreen != pScreen)
	    {
		error = BadMatch;
		goto PatchUp;
	    }
	    if (cmap != wColormap (pWin))
	    {
		if (!pWin->optional)
		{
		    if (!MakeWindowOptional (pWin))
		    {
			error = BadAlloc;
			goto PatchUp;
		    }
		}
		else if (pWin->parent && cmap == wColormap (pWin->parent))
		    checkOptional = TRUE;

		/*
		 * propagate the original colormap to any children
		 * inheriting it
		 */

		for (pChild = pWin->firstChild; pChild; pChild=pChild->nextSib)
		{
		    if (!pChild->optional && !MakeWindowOptional (pChild))
		    {
			error = BadAlloc;
			goto PatchUp;
		    }
		}

		pWin->optional->colormap = cmap;

		/*
		 * check on any children now matching the new colormap
		 */

		for (pChild = pWin->firstChild; pChild; pChild=pChild->nextSib)
		{
		    if (pChild->optional->colormap == cmap)
			CheckWindowOptionalNeed (pChild);
		}

		xE.u.u.type = ColormapNotify;
		xE.u.colormap.window = pWin->drawable.id;
		xE.u.colormap.colormap = cmap;
		xE.u.colormap.new = xTrue;
		xE.u.colormap.state = IsMapInstalled(cmap, pWin);
		DeliverEvents(pWin, &xE, 1, NullWindow);
	    }
	    break;
	  case CWCursor:
	    cursorID = (Cursor ) *pVlist;
	    pVlist++;
	    /*
	     * install the new
	     */
	    if ( cursorID == None)
	    {
	    	if (pWin == WindowTable[pWin->drawable.pScreen->myNum])
		    pCursor = rootCursor;
	    	else
		    pCursor = (CursorPtr) None;
	    }
	    else
	    {
	    	pCursor = (CursorPtr)LookupIDByType(cursorID, RT_CURSOR);
	    	if (!pCursor)
	    	{
		    error = BadCursor;
		    client->errorValue = cursorID;
		    goto PatchUp;
	    	}
	    }

	    if (pCursor != wCursor (pWin))
	    {
	    	/*
	     	 * patch up child windows so they don't lose cursors.
	     	 */

	    	for (pChild = pWin->firstChild; pChild; pChild=pChild->nextSib)
		{
		    if (!pChild->optional && !pChild->cursorIsNone &&
			!MakeWindowOptional (pChild))
		    {
			error = BadAlloc;
			goto PatchUp;
		    }
	    	}

		pOldCursor = 0;
		if (pCursor == (CursorPtr) None)
		{
		    pWin->cursorIsNone = TRUE;
		    if (pWin->optional)
		    {
			pOldCursor = pWin->optional->cursor;
			pWin->optional->cursor = (CursorPtr) None;
			checkOptional = TRUE;
		    }
		} else {
		    if (!pWin->optional)
		    {
			if (!MakeWindowOptional (pWin))
			{
			    error = BadAlloc;
			    goto PatchUp;
			}
		    }
		    else if (pWin->parent && pCursor == wCursor (pWin->parent))
			checkOptional = TRUE;
		    pOldCursor = pWin->optional->cursor;
		    pWin->optional->cursor = pCursor;
		    pCursor->refcnt++;
		    pWin->cursorIsNone = FALSE;
		    /*
		     * check on any children now matching the new cursor
		     */

		    for (pChild=pWin->firstChild; pChild; pChild=pChild->nextSib)
		    {
			if (pChild->optional &&
			    (pChild->optional->cursor == pCursor))
			    CheckWindowOptionalNeed (pChild);
		    }
		}

		if (pWin->realized)
		    WindowHasNewCursor( pWin);

		/* Can't free cursor until here - old cursor
		 * is needed in WindowHasNewCursor
		 */
		if (pOldCursor)
		    FreeCursor (pOldCursor, (Cursor)0);
	    }
	    break;
     	 default:
	    error = BadValue;
	    client->errorValue = vmask;
	    goto PatchUp;
      }
      vmaskCopy |= index;
    }
PatchUp:
    if (checkOptional)
	CheckWindowOptionalNeed (pWin);

    	/* We SHOULD check for an error value here XXX */
    (*pScreen->ChangeWindowAttributes)(pWin, vmaskCopy);

    /* 
        If the border contents have changed, redraw the border. 
	Note that this has to be done AFTER pScreen->ChangeWindowAttributes
        for the tile to be rotated, and the correct function selected.
    */
    if ((vmaskCopy & (CWBorderPixel | CWBorderPixmap))
	&& pWin->viewable && HasBorder (pWin))
    {
	RegionRec exposed;

	(* pScreen->RegionInit)(&exposed, NullBox, 0);
        (* pScreen->Subtract)(&exposed, &pWin->borderClip, &pWin->winSize);
	(*pWin->drawable.pScreen->PaintWindowBorder)(pWin, &exposed, PW_BORDER);
        (* pScreen->RegionUninit)(&exposed);
    }
    return error;
}


/*****
 * GetWindowAttributes
 *    Notice that this is different than ChangeWindowAttributes
 *****/

GetWindowAttributes(pWin, client)
    register WindowPtr pWin;
    ClientPtr client;
{
    xGetWindowAttributesReply wa;

    wa.type = X_Reply;
    wa.bitGravity = pWin->bitGravity;
    wa.winGravity = pWin->winGravity;
    wa.backingStore  = pWin->backingStore;
    wa.length = (sizeof(xGetWindowAttributesReply) -
		 sizeof(xGenericReply)) >> 2;
    wa.sequenceNumber = client->sequence;
    wa.backingBitPlanes =  wBackingBitPlanes (pWin);
    wa.backingPixel =  wBackingPixel (pWin);
    wa.saveUnder = (BOOL)pWin->saveUnder;
    wa.override = pWin->overrideRedirect;
    if (!pWin->mapped)
        wa.mapState = IsUnmapped;
    else if (pWin->realized)
        wa.mapState = IsViewable;
    else
        wa.mapState = IsUnviewable;

    wa.colormap =  wColormap (pWin);
    wa.mapInstalled = (wa.colormap == None) ? xFalse
					    : IsMapInstalled(wa.colormap, pWin);

    wa.yourEventMask = EventMaskForClient(pWin, client);
    wa.allEventMasks = pWin->eventMask | wOtherEventMasks (pWin);
    wa.doNotPropagateMask = wDontPropagateMask (pWin);
    wa.class = pWin->drawable.class;
    wa.visualID = wVisual (pWin);

    WriteReplyToClient(client, sizeof(xGetWindowAttributesReply), &wa);
}


static WindowPtr
MoveWindowInStack(pWin, pNextSib)
    register WindowPtr pWin, pNextSib;
{
    register WindowPtr pParent = pWin->parent;
    WindowPtr pFirstChange = pWin; /* highest window where list changes */

    if (pWin->nextSib != pNextSib)
    {
        if (!pNextSib)        /* move to bottom */
	{
            if (pParent->firstChild == pWin)
                pParent->firstChild = pWin->nextSib;
	    /* if (pWin->nextSib) */	 /* is always True: pNextSib == NULL
				          * and pWin->nextSib != pNextSib
					  * therefore pWin->nextSib != NULL */
	    pFirstChange = pWin->nextSib;
	    pWin->nextSib->prevSib = pWin->prevSib;
	    if (pWin->prevSib)
                pWin->prevSib->nextSib = pWin->nextSib;
            pParent->lastChild->nextSib = pWin;
            pWin->prevSib = pParent->lastChild;
            pWin->nextSib = NullWindow;
            pParent->lastChild = pWin;
	}
        else if (pParent->firstChild == pNextSib) /* move to top */
        {
	    pFirstChange = pWin;
	    if (pParent->lastChild == pWin)
    	       pParent->lastChild = pWin->prevSib;
	    if (pWin->nextSib)
		pWin->nextSib->prevSib = pWin->prevSib;
	    if (pWin->prevSib)
                pWin->prevSib->nextSib = pWin->nextSib;
	    pWin->nextSib = pParent->firstChild;
	    pWin->prevSib = (WindowPtr ) NULL;
	    pNextSib->prevSib = pWin;
	    pParent->firstChild = pWin;
	}
        else			/* move in middle of list */
        {
	    WindowPtr pOldNext = pWin->nextSib;

	    pFirstChange = NullWindow;
            if (pParent->firstChild == pWin)
                pFirstChange = pParent->firstChild = pWin->nextSib;
	    if (pParent->lastChild == pWin) {
	       pFirstChange = pWin;
    	       pParent->lastChild = pWin->prevSib;
	    }
	    if (pWin->nextSib)
		pWin->nextSib->prevSib = pWin->prevSib;
	    if (pWin->prevSib)
                pWin->prevSib->nextSib = pWin->nextSib;
            pWin->nextSib = pNextSib;
            pWin->prevSib = pNextSib->prevSib;
	    if (pNextSib->prevSib)
                pNextSib->prevSib->nextSib = pWin;
            pNextSib->prevSib = pWin;
	    if (!pFirstChange) {		     /* do we know it yet? */
	        pFirstChange = pParent->firstChild;  /* no, search from top */
	        while ((pFirstChange != pWin) && (pFirstChange != pOldNext))
		     pFirstChange = pFirstChange->nextSib;
	    }
	}
    }

    return( pFirstChange );
}

RegionPtr
CreateUnclippedWinSize (pWin)
    register WindowPtr   pWin;
{
    RegionPtr	pRgn;
    BoxRec	box;

    box.x1 = pWin->drawable.x;
    box.y1 = pWin->drawable.y;
    box.x2 = pWin->drawable.x + (int) pWin->drawable.width;
    box.y2 = pWin->drawable.y + (int) pWin->drawable.height;
    pRgn = (*pWin->drawable.pScreen->RegionCreate) (&box, 1);
#ifdef SHAPE
    if (wBoundingShape (pWin) || wClipShape (pWin)) {
        ScreenPtr	pScreen = pWin->drawable.pScreen;

	(*pScreen->TranslateRegion)
	    (pRgn, - pWin->drawable.x, - pWin->drawable.y);
	if (wBoundingShape (pWin))
	    (*pScreen->Intersect)
		(pRgn, pRgn, wBoundingShape (pWin));
	if (wClipShape (pWin))
	    (*pScreen->Intersect)
		(pRgn, pRgn, wClipShape (pWin));
	(*pScreen->TranslateRegion)
	    (pRgn, pWin->drawable.x, pWin->drawable.y);
    }
#endif
    return pRgn;
}

static void
SetWinSize (pWin)
    register WindowPtr pWin;
{
    ClippedRegionFromBox(pWin->parent, &pWin->winSize,
			 pWin->drawable.x, pWin->drawable.y,
 			 (int)pWin->drawable.width,
 			 (int)pWin->drawable.height);
#ifdef SHAPE
    if (wBoundingShape (pWin) || wClipShape (pWin)) {
        ScreenPtr	pScreen = pWin->drawable.pScreen;

	(*pScreen->TranslateRegion)
	    (&pWin->winSize, - pWin->drawable.x, - pWin->drawable.y);
	if (wBoundingShape (pWin))
	    (*pScreen->Intersect)
		(&pWin->winSize, &pWin->winSize, wBoundingShape (pWin));
	if (wClipShape (pWin))
	    (*pScreen->Intersect)
		(&pWin->winSize, &pWin->winSize, wClipShape (pWin));
	(*pScreen->TranslateRegion)
	    (&pWin->winSize, pWin->drawable.x, pWin->drawable.y);
    }
#endif
}

static void
SetBorderSize (pWin)
    register WindowPtr pWin;
{
    int	bw;

    if (HasBorder (pWin)) {
	bw = wBorderWidth (pWin);
	ClippedRegionFromBox(pWin->parent, &pWin->borderSize,
		pWin->drawable.x - bw, pWin->drawable.y - bw,
		(int)(pWin->drawable.width + (bw<<1)),
 		(int)(pWin->drawable.height + (bw<<1)));
#ifdef SHAPE
    	if (wBoundingShape (pWin)) {
            ScreenPtr	pScreen = pWin->drawable.pScreen;

	    (*pScreen->TranslateRegion)
	    	(&pWin->borderSize, - pWin->drawable.x, - pWin->drawable.y);
	    (*pScreen->Intersect)
	    	(&pWin->borderSize, &pWin->borderSize, wBoundingShape (pWin));
	    (*pScreen->TranslateRegion)
	    	(&pWin->borderSize, pWin->drawable.x, pWin->drawable.y);
	    (*pScreen->Union) (&pWin->borderSize,
			       &pWin->borderSize, &pWin->winSize);
    	}
#endif
    } else {
	(* pWin->drawable.pScreen->RegionCopy)(&pWin->borderSize,
					       &pWin->winSize);
    }
}

static void
MoveWindow(pWin, x, y, pNextSib, kind)
    register WindowPtr pWin;
    short x,y;
    WindowPtr pNextSib;
    VTKind kind;
{
    WindowPtr pParent;
    Bool WasViewable = (Bool)(pWin->viewable);
    short bw;
    RegionPtr oldRegion;
    DDXPointRec oldpt;
    Bool anyMarked;
    register ScreenPtr pScreen;
    WindowPtr windowToValidate;
#ifdef DO_SAVE_UNDERS
    Bool dosave = FALSE;
#endif

    /* if this is a root window, can't be moved */
    if (!(pParent = pWin->parent))
       return ;
    pScreen = pWin->drawable.pScreen;
    bw = wBorderWidth (pWin);

    oldpt.x = pWin->drawable.x;
    oldpt.y = pWin->drawable.y;
    if (WasViewable)
    {
        oldRegion = (* pScreen->RegionCreate)(NullBox, 1);
        (* pScreen->RegionCopy)(oldRegion, &pWin->borderClip);
	anyMarked = MarkOverlappedWindows(pWin, pWin);
    }
    pWin->origin.x = x + (int)bw;
    pWin->origin.y = y + (int)bw;
    x = pWin->drawable.x = pParent->drawable.x + x + (int)bw;
    y = pWin->drawable.y = pParent->drawable.y + y + (int)bw;

    SetWinSize (pWin);
    SetBorderSize (pWin);

    (* pScreen->PositionWindow)(pWin, x, y);

    windowToValidate = MoveWindowInStack(pWin, pNextSib);

    ResizeChildrenWinSize(pWin, x - oldpt.x, y - oldpt.y, 0, 0);

    if (WasViewable)
    {

        anyMarked |= MarkOverlappedWindows(pWin, windowToValidate);
#ifdef DO_SAVE_UNDERS
	if (DO_SAVE_UNDERS(pWin))
	{
	    if (pWin->saveUnder)
		dosave = ChangeSaveUnder(pWin, windowToValidate);
	    else
		dosave = CheckSaveUnder(pWin);
	}
#endif /* DO_SAVE_UNDERS */

	if (anyMarked)
	{
	    (* pScreen->ValidateTree)(pParent, NullWindow, kind);
	    (* pWin->drawable.pScreen->CopyWindow)(pWin, oldpt, oldRegion);
	    (* pScreen->RegionDestroy)(oldRegion);
	    /* XXX need to retile border if ParentRelative origin */
	    HandleExposures(pParent);
	}
#ifdef DO_SAVE_UNDERS
	if (dosave)
	    DoChangeSaveUnder(pWin->parent, windowToValidate);
#endif /* DO_SAVE_UNDERS */
	if (anyMarked && pScreen->PostValidateTree)
	    (* pScreen->PostValidateTree)(pParent, NullWindow, kind);
    }
    if (pWin->realized)
	WindowsRestructured ();
}

void
GravityTranslate (x, y, oldx, oldy, dw, dh, gravity, destx, desty)
    register int x, y;		/* new window position */
    int		oldx, oldy;	/* old window position */
    int		dw, dh;
    unsigned	gravity;
    register int *destx, *desty;	/* position relative to gravity */
{
    switch (gravity) {
    case NorthGravity:
	*destx = x + dw / 2;
	*desty = y;
	break;
    case NorthEastGravity:
	*destx = x + dw;
	*desty = y;
	break;
    case WestGravity:
	*destx = x;
	*desty = y + dh / 2;
	break;
    case CenterGravity:
	*destx = x + dw / 2;
	*desty = y + dh / 2;
	break;
    case EastGravity:
	*destx = x + dw;
	*desty = y + dh / 2;
	break;
    case SouthWestGravity:
	*destx = x;
	*desty = y + dh;
	break;
    case SouthGravity:
	*destx = x + dw / 2;
	*desty = y + dh;
	break;
    case SouthEastGravity:
	*destx = x + dw;
	*desty = y + dh;
	break;
    case StaticGravity:
	*destx = oldx;
	*desty = oldy;
	break;
    default:
	*destx = x;
	*desty = y;
	break;
    }
}

/* XXX need to retile border on each window with ParentRelative origin */
static void
ResizeChildrenWinSize(pWin, dx, dy, dw, dh)
    register WindowPtr pWin;
    int dx, dy, dw, dh;
{
    register ScreenPtr pScreen;
    register WindowPtr pSib, pChild;
    Bool resized = (dw || dh);

    pScreen = pWin->drawable.pScreen;

    for (pSib = pWin->firstChild; pSib; pSib = pSib->nextSib)
    {
	if (resized && (pSib->winGravity > NorthWestGravity))
	{
	    int cwsx, cwsy;

	    cwsx = pSib->origin.x;
	    cwsy = pSib->origin.y;
	    GravityTranslate (cwsx, cwsy, cwsx - dx, cwsy - dy, dw, dh,
			pSib->winGravity, &cwsx, &cwsy);
	    if (cwsx != pSib->origin.x || cwsy != pSib->origin.y)
	    {
		xEvent event;

		event.u.u.type = GravityNotify;
		event.u.gravity.window = pSib->drawable.id;
		event.u.gravity.x = cwsx - wBorderWidth (pSib);
		event.u.gravity.y = cwsy - wBorderWidth (pSib);
		DeliverEvents (pSib, &event, 1, NullWindow);
		pSib->origin.x = cwsx;
		pSib->origin.y = cwsy;
	    }
	}
	pSib->drawable.x = pWin->drawable.x + pSib->origin.x;
	pSib->drawable.y = pWin->drawable.y + pSib->origin.y;
	SetWinSize (pSib);
	SetBorderSize (pSib);
	(* pScreen->PositionWindow)(pSib, pSib->drawable.x, pSib->drawable.y);
	if (pChild = pSib->firstChild)
	{
	    while (1)
	    {
		pChild->drawable.x = pChild->parent->drawable.x +
				     pChild->origin.x;
		pChild->drawable.y = pChild->parent->drawable.y +
				     pChild->origin.y;
		SetWinSize (pChild);
		SetBorderSize (pChild);
		(* pScreen->PositionWindow)(pChild,
				    pChild->drawable.x, pChild->drawable.y);
		if (pChild->firstChild)
		{
		    pChild = pChild->firstChild;
		    continue;
		}
		while (!pChild->nextSib && (pChild != pSib))
		    pChild = pChild->parent;
		if (pChild == pSib)
		    break;
		pChild = pChild->nextSib;
	    }
	}
    }
}

/*
 * pValid is a region of the screen which has been
 * successfully copied -- recomputed exposed regions for affected windows
 */

static int
RecomputeExposures (pWin, pValid)
    register WindowPtr	pWin;
    RegionPtr	pValid;
{
    register ScreenPtr	pScreen;

    if (pWin->valdata)
    {
	pScreen = pWin->drawable.pScreen;
	/*
	 * compute exposed regions of this window
	 */
	(*pScreen->Subtract)(&pWin->valdata->after.exposed, &pWin->clipList, pValid);
	/*
	 * compute exposed regions of the border
	 */
	(*pScreen->Subtract)(&pWin->valdata->after.borderExposed,
			     &pWin->borderClip, &pWin->winSize);
	(*pScreen->Subtract)(&pWin->valdata->after.borderExposed,
			     &pWin->valdata->after.borderExposed, pValid);
	return WT_WALKCHILDREN;
    }
    return WT_NOMATCH;
}


static void
SlideAndSizeWindow(pWin, x, y, w, h, pSib)
    register WindowPtr pWin;
    short x,y;
    unsigned short w, h;
    WindowPtr pSib;
{
    WindowPtr pParent;
    Bool WasViewable = (Bool)(pWin->viewable);
    unsigned short width = pWin->drawable.width,
                   height = pWin->drawable.height;
    short oldx = pWin->drawable.x,
          oldy = pWin->drawable.y;
    int bw = wBorderWidth (pWin);
    short dw, dh;
    DDXPointRec oldpt;
    RegionPtr oldRegion;
    Bool anyMarked;
    register ScreenPtr pScreen;
    WindowPtr pFirstChange;
    register WindowPtr pChild;
    RegionPtr	gravitate[StaticGravity + 1];
    register unsigned g;
    int		nx, ny;		/* destination x,y */
    int		newx, newy;	/* new inner window position */
    RegionPtr	pRegion;
    RegionPtr	destClip;	/* portions of destination already written */
    RegionPtr	oldWinClip;	/* old clip list for window */
    RegionPtr	borderVisible = NullRegion; /* visible area of the border */
    RegionPtr	bsExposed = NullRegion;	    /* backing store exposures */
    Bool	shrunk = FALSE; /* shrunk in an inner dimension */
    Bool	moved = FALSE;	/* window position changed */
#ifdef DO_SAVE_UNDERS
    Bool	dosave = FALSE;
#endif

    /* if this is a root window, can't be resized */
    if (!(pParent = pWin->parent))
        return ;

    pScreen = pWin->drawable.pScreen;
    newx = pParent->drawable.x + x + bw;
    newy = pParent->drawable.y + y + bw;
    if (WasViewable)
    {
	anyMarked = FALSE;
	/*
	 * save the visible region of the window
	 */
	oldRegion = (*pScreen->RegionCreate) (NullBox, 1);
	(*pScreen->RegionCopy) (oldRegion, &pWin->winSize);

	/*
	 * catagorize child windows into regions to be moved
	 */
	for (g = 0; g <= StaticGravity; g++)
	    gravitate[g] = (RegionPtr) NULL;
	for (pChild = pWin->firstChild; pChild; pChild = pChild->nextSib)
	{
	    g = pChild->winGravity;
	    if (g != UnmapGravity)
	    {
		if (!gravitate[g])
		    gravitate[g] = (*pScreen->RegionCreate) (NullBox, 1);
		(*pScreen->Union) (gravitate[g],
				   gravitate[g], &pChild->borderClip);
	    }
	    else
	    {
		UnmapWindow(pChild, TRUE);
		anyMarked = TRUE;
	    }
	}
	anyMarked |= MarkOverlappedWindows(pWin, pWin);

	oldWinClip = NULL;
	if (pWin->bitGravity != ForgetGravity)
	{
	    oldWinClip = (*pScreen->RegionCreate) (NullBox, 1);
	    (*pScreen->RegionCopy) (oldWinClip, &pWin->clipList);
	}
    	/*
     	 * if the window is changing size, borderExposed
     	 * can't be computed correctly without some help.
     	 */
    	if (pWin->drawable.height > h || pWin->drawable.width > w)
	    shrunk = TRUE;

	if (newx != oldx || newy != oldy)
	    moved = TRUE;

	if ((pWin->drawable.height != h || pWin->drawable.width != w) &&
	    HasBorder (pWin))
	{
	    borderVisible = (*pScreen->RegionCreate) (NullBox, 1);
	    /* for tiled borders, we punt and draw the whole thing */
	    if (pWin->borderIsPixel || !moved)
	    {
	    	if (shrunk || moved)
		    (*pScreen->Subtract) (borderVisible,
					  &pWin->borderClip,
				      	  &pWin->winSize);
	    	else
		    (*pScreen->RegionCopy) (borderVisible,
					    &pWin->borderClip);
	    }
	}
    }
    pWin->origin.x = x + bw;
    pWin->origin.y = y + bw;
    pWin->drawable.height = h;
    pWin->drawable.width = w;

    x = pWin->drawable.x = newx;
    y = pWin->drawable.y = newy;

    SetWinSize (pWin);
    SetBorderSize (pWin);

    dw = (int)w - (int)width;
    dh = (int)h - (int)height;
    ResizeChildrenWinSize(pWin, x - oldx, y - oldy, dw, dh);

    /* let the hardware adjust background and border pixmaps, if any */
    (* pScreen->PositionWindow)(pWin, x, y);

    pFirstChange = MoveWindowInStack(pWin, pSib);

    if (WasViewable)
    {
	pRegion = (*pScreen->RegionCreate) (NullBox, 1);
	if (pWin->backStorage)
	    (*pScreen->RegionCopy) (pRegion, &pWin->clipList);

	anyMarked |= MarkOverlappedWindows(pWin, pFirstChange);

	if (pWin->valdata)
 	{
	    pWin->valdata->before.resized = TRUE;
	    pWin->valdata->before.borderVisible = borderVisible;
	}

#ifdef DO_SAVE_UNDERS
	if (DO_SAVE_UNDERS(pWin))
	{
	    if (pWin->saveUnder)
		dosave = ChangeSaveUnder(pWin, pFirstChange);
	    else
		dosave = CheckSaveUnder(pWin);
	}
#endif /* DO_SAVE_UNDERS */

	if (anyMarked)
	    (* pScreen->ValidateTree)(pParent, pFirstChange, VTOther);
	/*
	 * the entire window is trashed unless bitGravity
	 * recovers portions of it
	 */
	(*pScreen->RegionCopy) (&pWin->valdata->after.exposed, &pWin->clipList);
    }

    GravityTranslate (x, y, oldx, oldy, dw, dh, pWin->bitGravity, &nx, &ny);

    if (pWin->backStorage &&
	((pWin->backingStore == Always) || WasViewable))
    {
	if (!WasViewable)
	    pRegion = &pWin->clipList; /* a convenient empty region */
	if (pWin->bitGravity == ForgetGravity)
	    bsExposed = (* pScreen->TranslateBackingStore)
				(pWin, 0, 0, NullRegion, oldx, oldy);
	else
	{
	    bsExposed = (* pScreen->TranslateBackingStore)
			     (pWin, nx - x, ny - y, pRegion, oldx, oldy);
	}
    }

    if (WasViewable)
    {
	/* avoid the border */
	if (HasBorder (pWin))
	{
	    int	offx, offy, dx, dy;
	    offx = 0;
	    offy = 0;
	    for (g = 0; g <= StaticGravity; g++)
	    {
		if (!gravitate[g])
		    continue;

		/* align winSize to gravitate[g] */
		GravityTranslate (x, y, oldx, oldy, dw, dh, g, &nx, &ny);
		
		dx = (nx - oldx) - offx;
		dy = (ny - oldy) - offy;
		if (dx || dy)
		{
		    (*pScreen->TranslateRegion) (&pWin->winSize, dx, dy);
		    offx += dx;
		    offy += dy;
		}
		(*pScreen->Intersect) (gravitate[g], gravitate[g], &pWin->winSize);
	    }
	    /* get winSize back where it belongs */
	    if (offx || offy)
		(*pScreen->TranslateRegion) (&pWin->winSize, -offx, -offy);
	}
	/*
	 * add screen bits to the appropriate bucket
	 */

	if (oldWinClip)
	{
	    /*
	     * clip to new clipList
	     */
	    (*pScreen->RegionCopy) (pRegion, oldWinClip);
	    (*pScreen->TranslateRegion) (pRegion, nx - oldx, ny - oldy);
	    (*pScreen->Intersect) (oldWinClip, pRegion, &pWin->clipList);
	    /*
	     * don't step on any gravity bits which will be copied after this
	     * region.  Note -- this assumes that the regions will be copied
	     * in gravity order.
	     */
	    for (g = pWin->bitGravity + 1; g <= StaticGravity; g++)
	    {
		if (gravitate[g])
		    (*pScreen->Subtract) (oldWinClip, oldWinClip, gravitate[g]);
	    }
	    (*pScreen->TranslateRegion) (oldWinClip, oldx - nx, oldy - ny);
	    g = pWin->bitGravity;
	    if (!gravitate[g])
		gravitate[g] = oldWinClip;
	    else
	    {
		(*pScreen->Union) (gravitate[g], gravitate[g], oldWinClip);
		(*pScreen->RegionDestroy) (oldWinClip);
	    }
	}

	/*
	 * move the bits on the screen
	 */

	destClip = NULL;

	for (g = 0; g <= StaticGravity; g++)
	{
	    if (!gravitate[g])
	    	continue;

	    GravityTranslate (x, y, oldx, oldy, dw, dh, g, &nx, &ny);

            oldpt.x = oldx + (x - nx);
	    oldpt.y = oldy + (y - ny);

	    /* Note that gravitate[g] is *translated* by CopyWindow */

	    /* only copy the remaining useful bits */

	    (*pScreen->Intersect) (gravitate[g], gravitate[g], oldRegion);

	    /* clip to not overwrite already copied areas */

	    if (destClip) {
		(*pScreen->TranslateRegion) (destClip, oldpt.x - x, oldpt.y - y);
		(*pScreen->Subtract) (gravitate[g], gravitate[g], destClip);
		(*pScreen->TranslateRegion) (destClip, x - oldpt.x, y - oldpt.y);
	    }

	    /* and move those bits */

	    if (oldpt.x != x || oldpt.y != y)
		(*pWin->drawable.pScreen->CopyWindow)(pWin, oldpt, gravitate[g]);

	    /* remove any overwritten bits from the remaining useful bits */

	    (*pScreen->Subtract) (oldRegion, oldRegion, gravitate[g]);

	    /*
	     * recompute exposed regions of child windows
	     */
	
	    for (pChild = pWin->firstChild; pChild; pChild = pChild->nextSib)
	    {
		if (pChild->winGravity != g)
		    continue;
		(*pScreen->Intersect) (pRegion,
				       &pChild->borderClip, gravitate[g]);
		TraverseTree (pChild, RecomputeExposures, (pointer)pRegion);
	    }

	    /*
	     * remove the successfully copied regions of the
	     * window from its exposed region
	     */

	    if (g == pWin->bitGravity)
		(*pScreen->Subtract)(&pWin->valdata->after.exposed,
				     &pWin->valdata->after.exposed, gravitate[g]);
	    if (!destClip)
	    	destClip = gravitate[g];
	    else
	    {
		(*pScreen->Union) (destClip, destClip, gravitate[g]);
		(*pScreen->RegionDestroy) (gravitate[g]);
	    }
	}

	(*pScreen->RegionDestroy) (oldRegion);
	(*pScreen->RegionDestroy) (pRegion);
	if (destClip)
	    (*pScreen->RegionDestroy) (destClip);
	if (bsExposed)
	{
	    RegionPtr	valExposed = NullRegion;

	    if (pWin->valdata)
		valExposed = &pWin->valdata->after.exposed;
	    (*pScreen->WindowExposures) (pWin, valExposed, bsExposed);
	    if (valExposed)
		(*pScreen->RegionEmpty) (valExposed);
	    (*pScreen->RegionDestroy) (bsExposed);
	}
	if (anyMarked)
	    HandleExposures(pParent);
#ifdef DO_SAVE_UNDERS
	if (dosave)
	    DoChangeSaveUnder(pParent, pFirstChange);
#endif /* DO_SAVE_UNDERS */
	if (anyMarked && pScreen->PostValidateTree)
	    (* pScreen->PostValidateTree)(pParent, pFirstChange, VTOther);
    }
    else if (bsExposed)
    {
	(*pScreen->WindowExposures) (pWin, NullRegion, bsExposed);
	(*pScreen->RegionDestroy) (bsExposed);
    }
    if (pWin->realized)
	WindowsRestructured ();
}

/* Keeps the same inside(!) origin */

static void
ChangeBorderWidth(pWin, width)
    register WindowPtr pWin;
    unsigned short width;
{
    WindowPtr pParent;
    int oldwidth;
    Bool anyMarked;
    register ScreenPtr pScreen;
    Bool WasViewable = (Bool)(pWin->viewable);
    Bool HadBorder;
#ifdef DO_SAVE_UNDERS
    Bool	dosave = FALSE;
#endif

    oldwidth = wBorderWidth (pWin);
    if (oldwidth == width)
        return;
    HadBorder = HasBorder(pWin);
    pScreen = pWin->drawable.pScreen;
    pParent = pWin->parent;
    if (WasViewable && width < oldwidth)
	anyMarked = MarkOverlappedWindows(pWin, pWin);

    pWin->borderWidth = width;
    SetBorderSize (pWin);

    if (WasViewable)
    {
        if (width > oldwidth)
	{
	    anyMarked = MarkOverlappedWindows(pWin, pWin);
    	    /*
     	     * save the old border visible region to correctly compute
     	     * borderExposed.
     	     */
	    if (pWin->valdata && HadBorder)
	    {
	    	RegionPtr   borderVisible;
	    	borderVisible = (*pScreen->RegionCreate) (NULL, 1);
	    	(*pScreen->Subtract) (borderVisible,
				      &pWin->borderClip, &pWin->winSize);
	    	pWin->valdata->before.borderVisible = borderVisible;
	    }
	}
#ifdef DO_SAVE_UNDERS
	if (DO_SAVE_UNDERS(pWin))
	{
	    if (pWin->saveUnder)
		dosave = ChangeSaveUnder(pWin, pWin->nextSib);
	    else
		dosave = CheckSaveUnder(pWin);
	}
#endif /* DO_SAVE_UNDERS */

	if (anyMarked)
	{
	    (* pScreen->ValidateTree)(pParent, pWin, VTOther);
	    HandleExposures(pParent);
	}
#ifdef DO_SAVE_UNDERS
	if (dosave)
	    DoChangeSaveUnder(pParent, pWin->nextSib);
#endif /* DO_SAVE_UNDERS */
	if (anyMarked && pScreen->PostValidateTree)
	    (* pScreen->PostValidateTree)(pParent, pWin, VTOther);
    }
    if (pWin->realized)
	WindowsRestructured ();
}


#define GET_INT16(m, f) \
  	if (m & mask) \
          { \
             f = (INT16) *pVlist;\
 	    pVlist++; \
         }
#define GET_CARD16(m, f) \
 	if (m & mask) \
         { \
            f = (CARD16) *pVlist;\
 	    pVlist++;\
         }

#define GET_CARD8(m, f) \
 	if (m & mask) \
         { \
            f = (CARD8) *pVlist;\
 	    pVlist++;\
         }

#define ChangeMask ((Mask)(CWX | CWY | CWWidth | CWHeight))

#define IllegalInputOnlyConfigureMask (CWBorderWidth)

/*
 * IsSiblingAboveMe
 *     returns Above if pSib above pMe in stack or Below otherwise 
 */

static int
IsSiblingAboveMe(pMe, pSib)
    register WindowPtr pMe, pSib;
{
    register WindowPtr pWin;

    pWin = pMe->parent->firstChild;
    while (pWin)
    {
        if (pWin == pSib)
            return(Above);
        else if (pWin == pMe)
            return(Below);
        pWin = pWin->nextSib;
    }
    return(Below);
}

static BoxPtr
WindowExtents(pWin, pBox)
    register WindowPtr pWin;
    register BoxPtr pBox;
{
    pBox->x1 = pWin->origin.x - wBorderWidth (pWin);
    pBox->y1 = pWin->origin.y - wBorderWidth (pWin);
    pBox->x2 = pWin->origin.x + (int)pWin->drawable.width
	       + wBorderWidth (pWin);
    pBox->y2 = pWin->origin.y + (int)pWin->drawable.height
	       + wBorderWidth (pWin);
    return(pBox);
}

#ifdef SHAPE
#define IS_SHAPED(pWin)	(wBoundingShape (pWin) != (RegionPtr) NULL)

static RegionPtr
MakeBoundingRegion (pWin, pBox)
    register WindowPtr	pWin;
    BoxPtr	pBox;
{
    RegionPtr	pRgn;
    register ScreenPtr pScreen = pWin->drawable.pScreen;

    pRgn = (*pScreen->RegionCreate) (pBox, 1);
    if (wBoundingShape (pWin)) {
	    (*pScreen->TranslateRegion) (pRgn, -pWin->origin.x,
					          -pWin->origin.y);
	    (*pScreen->Intersect) (pRgn, pRgn, wBoundingShape (pWin));
	    (*pScreen->TranslateRegion) (pRgn, pWin->origin.x,
					          pWin->origin.y);
    }
    return pRgn;
}

static Bool
ShapeOverlap (pWin, pWinBox, pSib, pSibBox)
    WindowPtr	pWin, pSib;
    BoxPtr	pWinBox, pSibBox;
{
    RegionPtr	pWinRgn, pSibRgn;
    register ScreenPtr	pScreen;
    Bool	ret;

    if (!IS_SHAPED(pWin) && !IS_SHAPED(pSib))
	return TRUE;
    pScreen = pWin->drawable.pScreen;
    pWinRgn = MakeBoundingRegion (pWin, pWinBox);
    pSibRgn = MakeBoundingRegion (pSib, pSibBox);
    (*pScreen->Intersect) (pWinRgn, pWinRgn, pSibRgn);
    ret = (*pScreen->RegionNotEmpty) (pWinRgn);
    (*pScreen->RegionDestroy) (pWinRgn);
    (*pScreen->RegionDestroy) (pSibRgn);
    return ret;
}
#endif

static Bool
AnyWindowOverlapsMe(pWin, pHead, box)
    WindowPtr pWin, pHead;
    register BoxPtr box;
{
    register WindowPtr pSib;
    BoxRec sboxrec;
    register BoxPtr sbox;

    for (pSib = pWin->prevSib; pSib != pHead; pSib = pSib->prevSib)
    {
	if (pSib->mapped)
	{
	    sbox = WindowExtents(pSib, &sboxrec);
	    if (BOXES_OVERLAP(sbox, box)
#ifdef SHAPE
	    && ShapeOverlap (pWin, box, pSib, sbox)
#endif
	    )
		return(TRUE);
	}
    }
    return(FALSE);
}

static Bool
IOverlapAnyWindow(pWin, box)
    WindowPtr pWin;
    register BoxPtr box;
{
    register WindowPtr pSib;
    BoxRec sboxrec;
    register BoxPtr sbox;

    for (pSib = pWin->nextSib; pSib; pSib = pSib->nextSib)
    {
	if (pSib->mapped)
	{
	    sbox = WindowExtents(pSib, &sboxrec);
	    if (BOXES_OVERLAP(sbox, box)
#ifdef SHAPE
	    && ShapeOverlap (pWin, box, pSib, sbox)
#endif
	    )
		return(TRUE);
	}
    }
    return(FALSE);
}

/*
 *   WhereDoIGoInTheStack() 
 *        Given pWin and pSib and the relationshipe smode, return
 *        the window that pWin should go ABOVE.
 *        If a pSib is specified:
 *            Above:  pWin is placed just above pSib
 *            Below:  pWin is placed just below pSib
 *            TopIf:  if pSib occludes pWin, then pWin is placed
 *                    at the top of the stack
 *            BottomIf:  if pWin occludes pSib, then pWin is 
 *                       placed at the bottom of the stack
 *            Opposite: if pSib occludes pWin, then pWin is placed at the
 *                      top of the stack, else if pWin occludes pSib, then
 *                      pWin is placed at the bottom of the stack
 *
 *        If pSib is NULL:
 *            Above:  pWin is placed at the top of the stack
 *            Below:  pWin is placed at the bottom of the stack
 *            TopIf:  if any sibling occludes pWin, then pWin is placed at
 *                    the top of the stack
 *            BottomIf: if pWin occludes any sibline, then pWin is placed at
 *                      the bottom of the stack
 *            Opposite: if any sibling occludes pWin, then pWin is placed at
 *                      the top of the stack, else if pWin occludes any
 *                      sibling, then pWin is placed at the bottom of the stack
 *
 */

static WindowPtr
WhereDoIGoInTheStack(pWin, pSib, x, y, w, h, smode)
    register WindowPtr pWin, pSib;
    short x, y;
    unsigned short w, h;
    int smode;
{
    BoxRec box;
    register ScreenPtr pScreen;
    WindowPtr pHead, pFirst;

    if ((pWin == pWin->parent->firstChild) &&
	(pWin == pWin->parent->lastChild))
        return((WindowPtr ) NULL);
    pHead = RealChildHead(pWin->parent);
    pFirst = pHead ? pHead->nextSib : pWin->parent->firstChild;
    pScreen = pWin->drawable.pScreen;
    box.x1 = x;
    box.y1 = y;
    box.x2 = x + (int)w;
    box.y2 = y + (int)h;
    switch (smode)
    {
      case Above:
        if (pSib)
           return(pSib);
        else if (pWin == pFirst)
            return(pWin->nextSib);
        else
            return(pFirst);
      case Below:
        if (pSib)
	    if (pSib->nextSib != pWin)
	        return(pSib->nextSib);
	    else
	        return(pWin->nextSib);
        else
            return NullWindow;
      case TopIf:
	if ((!pWin->mapped || (pSib && !pSib->mapped)) && !permitOldBugs)
	    return(pWin->nextSib);
	else if (pSib)
	{
            if ((IsSiblingAboveMe(pWin, pSib) == Above) &&
                ((* pScreen->RectIn)(&pSib->borderSize, &box) != rgnOUT))
                return(pFirst);
            else
                return(pWin->nextSib);
	}
        else if (AnyWindowOverlapsMe(pWin, pHead, &box))
            return(pFirst);
        else
            return(pWin->nextSib);
      case BottomIf:
	if ((!pWin->mapped || (pSib && !pSib->mapped)) && !permitOldBugs)
	    return(pWin->nextSib);
	else if (pSib)
	{
            if ((IsSiblingAboveMe(pWin, pSib) == Below) &&
                ((* pScreen->RectIn)(&pSib->borderSize, &box) != rgnOUT))
                return NullWindow;
            else
                return(pWin->nextSib);
	}
        else if (IOverlapAnyWindow(pWin, &box))
            return NullWindow;
        else
            return(pWin->nextSib);
      case Opposite:
	if ((!pWin->mapped || (pSib && !pSib->mapped)) && !permitOldBugs)
	    return(pWin->nextSib);
	else if (pSib)
	{
	    if ((* pScreen->RectIn)(&pSib->borderSize, &box) != rgnOUT)
            {
                if (IsSiblingAboveMe(pWin, pSib) == Above)
                    return(pFirst);
                else
                    return NullWindow;
            }
            else
                return(pWin->nextSib);
	}
        else if (AnyWindowOverlapsMe(pWin, pHead, &box))
	{
	    /* If I'm occluded, I can't possibly be the first child
             * if (pWin == pWin->parent->firstChild)
             *    return pWin->nextSib;
	     */
            return(pFirst);
	}
        else if (IOverlapAnyWindow(pWin, &box))
            return NullWindow;
        else
            return pWin->nextSib;
      default:
      {
        ErrorF("Internal error in ConfigureWindow, smode == %d\n",smode );
        return pWin->nextSib;
      }
    }
}

static void
ReflectStackChange(pWin, pSib, kind)
    register WindowPtr pWin, pSib;
    VTKind  kind;
{
/* Note that pSib might be NULL */

    Bool WasViewable = (Bool)pWin->viewable;
    WindowPtr pParent;
    Bool anyMarked;
    WindowPtr pFirstChange;
#ifdef DO_SAVE_UNDERS
    Bool	dosave = FALSE;
#endif

    /* if this is a root window, can't be restacked */
    if (!(pParent = pWin->parent))
        return ;

    pFirstChange = MoveWindowInStack(pWin, pSib);

    if (WasViewable)
    {
	anyMarked = MarkOverlappedWindows(pWin, pFirstChange);
#ifdef DO_SAVE_UNDERS
	if (DO_SAVE_UNDERS(pWin))
	{
	    if (pWin->saveUnder)
		dosave = ChangeSaveUnder(pWin, pFirstChange);
	    else
		dosave = CheckSaveUnder(pWin);
	}
#endif /* DO_SAVE_UNDERS */
	if (anyMarked)
	{
	    (* pWin->drawable.pScreen->ValidateTree)(pParent, pFirstChange,
						     kind);
	    HandleExposures(pParent);
	}
#ifdef DO_SAVE_UNDERS
	if (dosave)
	    DoChangeSaveUnder(pParent, pFirstChange);
#endif /* DO_SAVE_UNDERS */
	if (anyMarked && pWin->drawable.pScreen->PostValidateTree)
	    (* pWin->drawable.pScreen->PostValidateTree)(pParent,
							 pFirstChange, kind);
    }
    if (pWin->realized)
	WindowsRestructured ();
}

/*****
 * ConfigureWindow
 *****/


int
ConfigureWindow(pWin, mask, vlist, client)
    register WindowPtr pWin;
    register Mask mask;
    XID *vlist;
    ClientPtr client;
{
#define RESTACK_WIN    0
#define MOVE_WIN       1
#define RESIZE_WIN     2
#define REBORDER_WIN   3
    register WindowPtr pSib = NullWindow;
    register WindowPtr pParent = pWin->parent;
    Window sibwid;
    Mask index, tmask;
    register XID *pVlist;
    short x,   y, beforeX, beforeY;
    unsigned short w = pWin->drawable.width,
                   h = pWin->drawable.height,
	           bw = pWin->borderWidth;
    int action,
        smode = Above;
    xEvent event;

    if ((pWin->drawable.class == InputOnly) && (mask & IllegalInputOnlyConfigureMask))
        return(BadMatch);

    if ((mask & CWSibling) && !(mask & CWStackMode))
        return(BadMatch);

    pVlist = vlist;

    if (pParent)
    {
        x = pWin->drawable.x - pParent->drawable.x - (int)bw;
        y = pWin->drawable.y - pParent->drawable.y - (int)bw;
    }
    else
    {
        x = pWin->drawable.x;
        y = pWin->drawable.y;
    }
    beforeX = x;
    beforeY = y;
    action = RESTACK_WIN;	
    if ((mask & (CWX | CWY)) && (!(mask & (CWHeight | CWWidth))))
    {
	GET_INT16(CWX, x);
 	GET_INT16(CWY, y);
	action = MOVE_WIN;
    }
	/* or should be resized */
    else if (mask & (CWX |  CWY | CWWidth | CWHeight))
    {
	GET_INT16(CWX, x);
	GET_INT16(CWY, y);
	GET_CARD16(CWWidth, w);
	GET_CARD16 (CWHeight, h);
	if (!w || !h)
	{
	    client->errorValue = 0;
            return BadValue;
	}
        action = RESIZE_WIN;
    }
    tmask = mask & ~ChangeMask;
    while (tmask)
    {
	index = (Mask)lowbit (tmask);
	tmask &= ~index;
	switch (index)
        {
          case CWBorderWidth:
	    GET_CARD16(CWBorderWidth, bw);
	    break;
          case CWSibling:
	    sibwid = (Window ) *pVlist;
	    pVlist++;
            pSib = (WindowPtr )LookupIDByType(sibwid, RT_WINDOW);
            if (!pSib)
	    {
		client->errorValue = sibwid;
                return(BadWindow);
	    }
            if (pSib->parent != pParent)
		return(BadMatch);
	    if (pSib == pWin)
	        return(BadMatch);
	    break;
          case CWStackMode:
	    GET_CARD8(CWStackMode, smode);
	    if ((smode != TopIf) && (smode != BottomIf) &&
 		(smode != Opposite) && (smode != Above) && (smode != Below))
                   return(BadMatch);
	    break;
	  default:
	    client->errorValue = mask;
	    return(BadValue);
	}
    }
	/* root really can't be reconfigured, so just return */
    if (!pParent)
	return Success;

        /* Figure out if the window should be moved.  Doesnt
           make the changes to the window if event sent */

    if (mask & CWStackMode)
        pSib = WhereDoIGoInTheStack(pWin, pSib, pParent->drawable.x + x,
				    pParent->drawable.y + y,
				    w + (bw << 1), h + (bw << 1), smode);
    else
        pSib = pWin->nextSib;

    if ((!pWin->overrideRedirect) && RedirectSend(pParent))
    {
	event.u.u.type = ConfigureRequest;
	event.u.configureRequest.window = pWin->drawable.id;
	event.u.configureRequest.parent = pParent->drawable.id;
        if (mask & CWSibling)
	   event.u.configureRequest.sibling = sibwid;
        else
       	    event.u.configureRequest.sibling = None;
        if (mask & CWStackMode)
	   event.u.u.detail = smode;
        else
       	    event.u.u.detail = Above;
	event.u.configureRequest.x = x;
	event.u.configureRequest.y = y;
	event.u.configureRequest.width = w;
	event.u.configureRequest.height = h;
	event.u.configureRequest.borderWidth = bw;
	event.u.configureRequest.valueMask = mask;
	if (MaybeDeliverEventsToClient(pParent, &event, 1,
	        SubstructureRedirectMask, client) == 1)
    	    return(Success);
    }
    if (action == RESIZE_WIN)
    {
        Bool size_change = (w != pWin->drawable.width)
                        || (h != pWin->drawable.height);
	if (size_change && ((pWin->eventMask|wOtherEventMasks(pWin)) & ResizeRedirectMask))
	{
	    xEvent eventT;
    	    eventT.u.u.type = ResizeRequest;
    	    eventT.u.resizeRequest.window = pWin->drawable.id;
	    eventT.u.resizeRequest.width = w;
	    eventT.u.resizeRequest.height = h;
	    if (MaybeDeliverEventsToClient(pWin, &eventT, 1,
				       ResizeRedirectMask, client) == 1)
	    {
                /* if event is delivered, leave the actual size alone. */
	        w = pWin->drawable.width;
	        h = pWin->drawable.height;
                size_change = FALSE;
	    }
	}
        if (!size_change)
	{
	    if (mask & (CWX | CWY))
    	        action = MOVE_WIN;
	    else if (mask & (CWStackMode | CWBorderWidth))
	        action = RESTACK_WIN;
            else   /* really nothing to do */
                return(Success) ;
	}
    }

    if (action == RESIZE_WIN)
            /* we've already checked whether there's really a size change */
            goto ActuallyDoSomething;
    if ((mask & CWX) && (x != beforeX))
            goto ActuallyDoSomething;
    if ((mask & CWY) && (y != beforeY))
            goto ActuallyDoSomething;
    if ((mask & CWBorderWidth) && (bw != wBorderWidth (pWin)))
            goto ActuallyDoSomething;
    if (mask & CWStackMode)
    {
        if (pWin->nextSib != pSib)
            goto ActuallyDoSomething;
    }
    return(Success);

ActuallyDoSomething:
    if (SubStrSend(pWin, pParent))
    {
	event.u.u.type = ConfigureNotify;
	event.u.configureNotify.window = pWin->drawable.id;
	if (pSib)
	    event.u.configureNotify.aboveSibling = pSib->drawable.id;
	else
	    event.u.configureNotify.aboveSibling = None;
	event.u.configureNotify.x = x;
	event.u.configureNotify.y = y;
	event.u.configureNotify.width = w;
	event.u.configureNotify.height = h;
	event.u.configureNotify.borderWidth = bw;
	event.u.configureNotify.override = pWin->overrideRedirect;
	DeliverEvents(pWin, &event, 1, NullWindow);
    }
    if (mask & CWBorderWidth)
    {
	if (action == RESTACK_WIN)
	{
	    action = MOVE_WIN;
	    pWin->borderWidth = bw;
	}
	else if ((action == MOVE_WIN) &&
		 (beforeX + wBorderWidth (pWin) == x + (int)bw) &&
		 (beforeY + wBorderWidth (pWin) == y + (int)bw))
	{
	    action = REBORDER_WIN;
            ChangeBorderWidth(pWin, bw);
	}
        else
	    pWin->borderWidth = bw;
    }
    if (action == MOVE_WIN)
        MoveWindow(pWin, x, y, pSib,
		   (mask & CWBorderWidth) ? VTOther : VTMove);
    else if (action == RESIZE_WIN)
        SlideAndSizeWindow(pWin, x, y, w, h, pSib);
    else if (mask & CWStackMode)
        ReflectStackChange(pWin, pSib, VTOther);

    if (action != RESTACK_WIN)
	CheckCursorConfinement(pWin);

    return(Success);
#undef RESTACK_WIN
#undef MOVE_WIN
#undef RESIZE_WIN
#undef REBORDER_WIN
}

#ifdef SHAPE
/******
 *
 * SetShape
 *    The border/window shape has changed.  Recompute winSize/borderSize
 *    and send appropriate exposure events
 */

SetShape(pWin)
    register WindowPtr	pWin;
{
    Bool	WasViewable = (Bool)(pWin->viewable);
    register ScreenPtr pScreen = pWin->drawable.pScreen;
    Bool	anyMarked;
    WindowPtr	pParent = pWin->parent;
    RegionPtr	pOldClip, bsExposed;
#ifdef DO_SAVE_UNDERS
    Bool	dosave = FALSE;
#endif

    if (WasViewable)
    {
	anyMarked = MarkOverlappedWindows(pWin, pWin);
	if (pWin->valdata)
	{
	    if (HasBorder (pWin))
	    {
		RegionPtr	borderVisible;

	    	borderVisible = (*pScreen->RegionCreate) (NullBox, 1);
	    	(*pScreen->Subtract) (borderVisible,
				      &pWin->borderClip, &pWin->winSize);
	    	pWin->valdata->before.borderVisible = borderVisible;
	    }
	    pWin->valdata->before.resized = TRUE;
	}
    }

    SetWinSize (pWin);
    SetBorderSize (pWin);

    ResizeChildrenWinSize(pWin, 0, 0, 0, 0);

    if (WasViewable)
    {
	if (pWin->backStorage)
	{
	    pOldClip = (*pScreen->RegionCreate) (NullBox, 1);
	    (*pScreen->RegionCopy) (pOldClip, &pWin->clipList);
	}

	anyMarked |= MarkOverlappedWindows(pWin, pWin);

#ifdef DO_SAVE_UNDERS
	if (DO_SAVE_UNDERS(pWin))
	{
	    if (pWin->saveUnder)
		dosave = ChangeSaveUnder(pWin, pWin);
	    else
		dosave = CheckSaveUnder(pWin);
	}
#endif /* DO_SAVE_UNDERS */

	if (anyMarked)
	    (* pScreen->ValidateTree)(pParent, NullWindow, VTOther);
    }

    if (pWin->backStorage &&
	((pWin->backingStore == Always) || WasViewable))
    {
	if (!WasViewable)
	    pOldClip = &pWin->clipList; /* a convenient empty region */
	bsExposed = (* pScreen->TranslateBackingStore)
			     (pWin, 0, 0, pOldClip,
 			      pWin->drawable.x, pWin->drawable.y);
    	if (bsExposed)
    	{
	    RegionPtr	valExposed = NullRegion;
    
	    if (pWin->valdata)
	    	valExposed = &pWin->valdata->after.exposed;
	    (*pScreen->WindowExposures) (pWin, valExposed, bsExposed);
	    if (valExposed)
	    	(*pScreen->RegionEmpty) (valExposed);
	    (*pScreen->RegionDestroy) (bsExposed);
    	}
    }
    if (WasViewable)
    {
	if (anyMarked)
	    HandleExposures(pParent);
#ifdef DO_SAVE_UNDERS
	if (dosave)
	    DoChangeSaveUnder(pParent, pWin);
#endif /* DO_SAVE_UNDERS */
	if (anyMarked && pScreen->PostValidateTree)
	    (* pScreen->PostValidateTree)(pParent, NullWindow, VTOther);
    }
    if (pWin->realized)
	WindowsRestructured ();
    CheckCursorConfinement(pWin);
}
#endif

/******
 *
 * CirculateWindow
 *    For RaiseLowest, raises the lowest mapped child (if any) that is
 *    obscured by another child to the top of the stack.  For LowerHighest,
 *    lowers the highest mapped child (if any) that is obscuring another
 *    child to the bottom of the stack.  Exposure processing is performed 
 *
 ******/

int
CirculateWindow(pParent, direction, client)
    WindowPtr pParent;
    int direction;
    ClientPtr client;
{
    register WindowPtr pWin, pHead, pFirst;
    xEvent event;
    BoxRec box;

    pHead = RealChildHead(pParent);
    pFirst = pHead ? pHead->nextSib : pParent->firstChild;
    if (direction == RaiseLowest)
    {
	for (pWin = pParent->lastChild;
	     (pWin != pHead) &&
	     !(pWin->mapped &&
	       AnyWindowOverlapsMe(pWin, pHead, WindowExtents(pWin, &box)));
	     pWin = pWin->prevSib) ;
	if (pWin == pHead)
	    return Success;
    }
    else
    {
	for (pWin = pFirst;
	     pWin &&
	     !(pWin->mapped &&
	       IOverlapAnyWindow(pWin, WindowExtents(pWin, &box)));
	     pWin = pWin->nextSib) ;
	if (!pWin)
	    return Success;
    }

    event.u.circulate.window = pWin->drawable.id;
    event.u.circulate.parent = pParent->drawable.id;
    event.u.circulate.event = pParent->drawable.id;
    if (direction == RaiseLowest)
	event.u.circulate.place = PlaceOnTop;
    else
        event.u.circulate.place = PlaceOnBottom;

    if (RedirectSend(pParent))
    {
	event.u.u.type = CirculateRequest;
	if (MaybeDeliverEventsToClient(pParent, &event, 1,
	        SubstructureRedirectMask, client) == 1)
    	    return(Success);
    }

    event.u.u.type = CirculateNotify;
    DeliverEvents(pWin, &event, 1, NullWindow);
    ReflectStackChange(pWin,
		       (direction == RaiseLowest) ? pFirst : NullWindow,
		       VTStack);

    return(Success);
}

static int
CompareWIDs(pWin, wid)
    WindowPtr pWin;
    Window *wid;
{
    if (pWin->drawable.id == *wid)
       return(WT_STOPWALKING);
    else
       return(WT_WALKCHILDREN);
}

/*****
 *  ReparentWindow
 *****/

int
ReparentWindow(pWin, pParent, x, y, client)
    register WindowPtr pWin, pParent;
    short x,y;
    ClientPtr client;
{
    WindowPtr pPrev;
    Bool WasMapped = (Bool)(pWin->mapped);
    xEvent event;
    int bw = wBorderWidth (pWin);
    register ScreenPtr pScreen;

    pScreen = pWin->drawable.pScreen;
    if (TraverseTree(pWin, CompareWIDs, (pointer)&pParent->drawable.id) == WT_STOPWALKING)
        return(BadMatch);		
    if (!MakeWindowOptional(pWin))
	return(BadAlloc);

    if (WasMapped)
       UnmapWindow(pWin, FALSE);

    event.u.u.type = ReparentNotify;
    event.u.reparent.window = pWin->drawable.id;
    event.u.reparent.parent = pParent->drawable.id;
    event.u.reparent.x = x;
    event.u.reparent.y = y;
    event.u.reparent.override = pWin->overrideRedirect;
    DeliverEvents(pWin, &event, 1, pParent);

    /* take out of sibling chain */

    pPrev = pWin->parent;
    if (pPrev->firstChild == pWin)
        pPrev->firstChild = pWin->nextSib;
    if (pPrev->lastChild == pWin)
        pPrev->lastChild = pWin->prevSib;

    if (pWin->nextSib)
        pWin->nextSib->prevSib = pWin->prevSib;
    if (pWin->prevSib)
        pWin->prevSib->nextSib = pWin->nextSib;

    /* insert at begining of pParent */
    pWin->parent = pParent;
    pPrev = RealChildHead(pParent);
    if (pPrev)
    {
	pWin->nextSib = pPrev->nextSib;
        if (pPrev->nextSib)
    	    pPrev->nextSib->prevSib = pWin;
	else
	    pParent->lastChild = pWin;
        pPrev->nextSib = pWin;
	pWin->prevSib = pPrev;
    }
    else
    {
        pWin->nextSib = pParent->firstChild;
	pWin->prevSib = NullWindow;
        if (pParent->firstChild)
	    pParent->firstChild->prevSib = pWin;
        else
            pParent->lastChild = pWin;
	pParent->firstChild = pWin;
    }

    pWin->origin.x = x + bw;
    pWin->origin.y = y + bw;
    pWin->drawable.x = x + bw + pParent->drawable.x;
    pWin->drawable.y = y + bw + pParent->drawable.y;

    /* clip to parent */
    SetWinSize (pWin);
    SetBorderSize (pWin);

    (* pScreen->PositionWindow)(pWin, pWin->drawable.x, pWin->drawable.y);
    ResizeChildrenWinSize(pWin, 0, 0, 0, 0);

    CheckWindowOptionalNeed(pWin);

    if (WasMapped)
        MapWindow(pWin, client);
    RecalculateDeliverableEvents(pWin);
    return(Success);
}

static void
MarkWindow(pWin)
    register WindowPtr pWin;
{
    register ValidatePtr val;
    extern Bool Must_have_memory;

    if (pWin->valdata)
	return;
    Must_have_memory = TRUE; /* XXX */
    val = (ValidatePtr)xalloc(sizeof(ValidateRec));
    Must_have_memory = FALSE; /* XXX */
    val->before.oldAbsCorner.x = pWin->drawable.x;
    val->before.oldAbsCorner.y = pWin->drawable.y;
    val->before.borderVisible = NullRegion;
    val->before.resized = FALSE;
    pWin->valdata = val;
}

static Bool
MarkOverlappedWindows(pWin, pFirst)
    WindowPtr pWin;
    WindowPtr pFirst;
{
    register BoxPtr box;
    register WindowPtr pChild, pLast;
    int (* RectIn)();
    Bool anyMarked = FALSE;

    if (pWin == pFirst)
    {
	/* Blindly mark pWin and all of it's inferiors.  This is a slight
	 * overkill if there are mapped windows that outside pWin's border,
	 * but it's better than wasting time on RectIn checks.
	 */
	pChild = pWin;
	while (1)
	{
	    if (pChild->viewable)
	    {
		MarkWindow(pChild);
		if (pChild->firstChild)
		{
		    pChild = pChild->firstChild;
		    continue;
		}
	    }
	    while (!pChild->nextSib && (pChild != pWin))
		pChild = pChild->parent;
	    if (pChild == pWin)
		break;
	    pChild = pChild->nextSib;
	}
	anyMarked = TRUE;
	pFirst = pFirst->nextSib;
    }
    if (pChild = pFirst)
    {
	box = (* pChild->drawable.pScreen->RegionExtents)(&pWin->borderSize);
	RectIn = pChild->drawable.pScreen->RectIn;
	pLast = pChild->parent->lastChild;
	while (1)
	{
	    if (pChild->viewable && (*RectIn)(&pChild->borderSize, box))
	    {
		MarkWindow(pChild);
		anyMarked = TRUE;
		if (pChild->firstChild)
		{
		    pChild = pChild->firstChild;
		    continue;
		}
	    }
	    while (!pChild->nextSib && (pChild != pLast))
		pChild = pChild->parent;
	    if (pChild == pLast)
		break;
	    pChild = pChild->nextSib;
	}
    }
    if (anyMarked)
	MarkWindow(pWin->parent);
    return anyMarked;
}

static void
RealizeTree(pWin)
    WindowPtr pWin;
{
    register WindowPtr pChild;
    Bool (* Realize)();

    Realize = pWin->drawable.pScreen->RealizeWindow;
    pChild = pWin;
    while (1)
    {
	if (pChild->mapped)
	{
	    pChild->realized = TRUE;
#ifdef DO_SAVE_UNDERS
	    if (pChild->saveUnder)
		deltaSaveUndersViewable++;
#endif
	    pChild->viewable = (pChild->drawable.class == InputOutput);
	    (* Realize)(pChild);
	    if (pChild->firstChild)
	    {
		pChild = pChild->firstChild;
		continue;
	    }
	}
	while (!pChild->nextSib && (pChild != pWin))
	    pChild = pChild->parent;
	if (pChild == pWin)
	    return;
	pChild = pChild->nextSib;
    }
}

/*****
 * MapWindow
 *    If some other client has selected SubStructureReDirect on the parent
 *    and override-redirect is xFalse, then a MapRequest event is generated,
 *    but the window remains unmapped.  Otherwise, the window is mapped and a
 *    MapNotify event is generated.
 *****/

int
MapWindow(pWin, client)
    register WindowPtr pWin;
    ClientPtr client;
{
    register ScreenPtr pScreen;

    register WindowPtr pParent;
#ifdef DO_SAVE_UNDERS
    Bool	dosave = FALSE;
#endif

    if (pWin->mapped)
        return(Success);
    pScreen = pWin->drawable.pScreen;
    if (pParent = pWin->parent)
    {
        xEvent event;
	Bool anyMarked;

        if ((!pWin->overrideRedirect) && RedirectSend(pParent))
	{
	    event.u.u.type = MapRequest;
	    event.u.mapRequest.window = pWin->drawable.id;
	    event.u.mapRequest.parent = pParent->drawable.id;

	    if (MaybeDeliverEventsToClient(pParent, &event, 1,
	        SubstructureRedirectMask, client) == 1)
    	        return(Success);
	}

	pWin->mapped = TRUE;
	if (SubStrSend(pWin, pParent))
	{
	    event.u.u.type = MapNotify;
	    event.u.mapNotify.window = pWin->drawable.id;
	    event.u.mapNotify.override = pWin->overrideRedirect;
	    DeliverEvents(pWin, &event, 1, NullWindow);
	}

        if (!pParent->realized)
            return(Success);
	RealizeTree(pWin);
	if (pWin->viewable)
	{
	    anyMarked = MarkOverlappedWindows(pWin, pWin);
#ifdef DO_SAVE_UNDERS
	    if (DO_SAVE_UNDERS(pWin))
	    {
		if (pWin->saveUnder)
		    dosave = ChangeSaveUnder(pWin, pWin->nextSib);
		else
		    dosave = CheckSaveUnder(pWin);
	    }
#endif /* DO_SAVE_UNDERS */
	    if (anyMarked)
	    {
		(* pScreen->ValidateTree)(pParent, pWin, VTMap);
		HandleExposures(pParent);
	    }
#ifdef DO_SAVE_UNDERS
	    if (dosave)
		DoChangeSaveUnder(pParent, pWin->nextSib);
#endif /* DO_SAVE_UNDERS */
	if (anyMarked && pScreen->PostValidateTree)
	    (* pScreen->PostValidateTree)(pParent, pWin, VTMap);
	}
	WindowsRestructured ();
    }
    else
    {
	RegionRec   temp;

	pWin->mapped = TRUE;
        pWin->realized = TRUE;     /* for roots */
        pWin->viewable = pWin->drawable.class == InputOutput;
    	/* We SHOULD check for an error value here XXX */
        (* pScreen->RealizeWindow)(pWin);
	(* pScreen->RegionInit) (&temp, NullBox, 0);
	(* pScreen->RegionCopy) (&temp, &pWin->clipList);
	(*pScreen->WindowExposures) (pWin, &temp, NullRegion);
	(* pScreen->RegionUninit) (&temp);
    }

    return(Success);
}


/*****
 * MapSubwindows
 *    Performs a MapWindow all unmapped children of the window, in top
 *    to bottom stacking order.
 *****/

MapSubwindows(pParent, client)
    register WindowPtr pParent;
    ClientPtr client;
{
    register WindowPtr	pWin;
    WindowPtr		pFirstMapped = NullWindow;
#ifdef DO_SAVE_UNDERS
    WindowPtr		pFirstSaveUndered = NullWindow;
#endif
    register ScreenPtr	pScreen;
    register Mask	parentRedirect;
    register Mask	parentNotify;
    xEvent		event;
    Bool		anyMarked;
#ifdef DO_SAVE_UNDERS
    Bool	dosave = FALSE;
#endif

    pScreen = pParent->drawable.pScreen;
    parentRedirect = RedirectSend(pParent);
    parentNotify = SubSend(pParent);
    anyMarked = FALSE;
    for (pWin = pParent->firstChild; pWin; pWin = pWin->nextSib)
    {
	if (!pWin->mapped)
	{
            if (parentRedirect && !pWin->overrideRedirect)
	    {
	    	event.u.u.type = MapRequest;
	    	event.u.mapRequest.window = pWin->drawable.id;
	    	event.u.mapRequest.parent = pParent->drawable.id;
    
	    	if (MaybeDeliverEventsToClient(pParent, &event, 1,
	            SubstructureRedirectMask, client) == 1)
    	            continue;
	    }
    
	    pWin->mapped = TRUE;
	    if (parentNotify || StrSend(pWin))
	    {
		event.u.u.type = MapNotify;
		event.u.mapNotify.window = pWin->drawable.id;
		event.u.mapNotify.override = pWin->overrideRedirect;
		DeliverEvents(pWin, &event, 1, NullWindow);
	    }
    
	    if (!pFirstMapped)
		pFirstMapped = pWin;
            if (pParent->realized)
	    {
	    	RealizeTree(pWin);
	    	if (pWin->viewable)
	    	{
	    	    anyMarked |= MarkOverlappedWindows(pWin, pWin);
#ifdef DO_SAVE_UNDERS
	    	    if (DO_SAVE_UNDERS(pWin))
	    	    {
		    	if (pWin->saveUnder)
		    	    dosave |= ChangeSaveUnder(pWin, pWin->nextSib);
		    	else
		    	    dosave |= CheckSaveUnder(pWin);
		    	if (dosave && !pFirstSaveUndered)
			    pFirstSaveUndered = pWin;
	    	    }
#endif /* DO_SAVE_UNDERS */
	    	}
	    }
	}
    }

    if (pFirstMapped)
    {
    	if (anyMarked)
    	{
	    (* pScreen->ValidateTree)(pParent, pFirstMapped, VTMap);
	    HandleExposures(pParent);
    	}
#ifdef DO_SAVE_UNDERS
	if (dosave)
	    DoChangeSaveUnder(pParent, pFirstSaveUndered->nextSib);
#endif /* DO_SAVE_UNDERS */
	if (anyMarked && pScreen->PostValidateTree)
	    (* pScreen->PostValidateTree)(pParent, pFirstMapped, VTMap);
    	WindowsRestructured ();
    }
}

static void
UnrealizeTree(pWin, fromConfigure)
    WindowPtr pWin;
    Bool fromConfigure;
{
    register WindowPtr pChild;
    void (*RegionEmpty)();
    Bool (*Unrealize)();
    void (*ClipNotify)();

    RegionEmpty = pWin->drawable.pScreen->RegionEmpty;
    Unrealize = pWin->drawable.pScreen->UnrealizeWindow;
    ClipNotify = pWin->drawable.pScreen->ClipNotify;
    pChild = pWin;
    while (1)
    {
	if (pChild->realized)
	{
	    pChild->realized = FALSE;
	    pChild->visibility = VisibilityNotViewable;
	    (* Unrealize)(pChild);
	    DeleteWindowFromAnyEvents(pChild, FALSE);
	    if (pChild->viewable)
	    {
#ifdef DO_SAVE_UNDERS
		if (pChild->saveUnder)
		    deltaSaveUndersViewable--;
#endif
		pChild->viewable = FALSE;
		if (pChild->backStorage)
		    (*pChild->drawable.pScreen->SaveDoomedAreas)(
					    pChild, &pChild->clipList, 0, 0);
		if ((pChild != pWin) || fromConfigure)
		{
		    (* RegionEmpty)(&pChild->clipList);
		    if (ClipNotify)
			(* ClipNotify)(pChild, 0, 0);
		    (* RegionEmpty)(&pChild->borderClip);
		}
		pChild->drawable.serialNumber = NEXT_SERIAL_NUMBER;
	    }
	    if (pChild->firstChild)
	    {
		pChild = pChild->firstChild;
		continue;
	    }
	}
	while (!pChild->nextSib && (pChild != pWin))
	    pChild = pChild->parent;
	if (pChild == pWin)
	    return;
	pChild = pChild->nextSib;
    }
}

/*****
 * UnmapWindow
 *    If the window is already unmapped, this request has no effect.
 *    Otherwise, the window is unmapped and an UnMapNotify event is
 *    generated.  Cannot unmap a root window.
 *****/

UnmapWindow(pWin, fromConfigure)
    register WindowPtr pWin;
    Bool fromConfigure;
{
    register WindowPtr pParent;
    xEvent event;
    Bool wasRealized = (Bool)pWin->realized;
    Bool wasViewable = (Bool)pWin->viewable;

    if ((!pWin->mapped) || (!(pParent = pWin->parent)))
        return(Success);
    if (SubStrSend(pWin, pParent))
    {
	event.u.u.type = UnmapNotify;
	event.u.unmapNotify.window = pWin->drawable.id;
	event.u.unmapNotify.fromConfigure = fromConfigure;
	DeliverEvents(pWin, &event, 1, NullWindow);
    }
    if (wasViewable && !fromConfigure)
    {
	pWin->valdata = UnmapValData;
	MarkOverlappedWindows(pWin, pWin->nextSib);
	MarkWindow(pWin->parent);
    }
    pWin->mapped = FALSE;
    if (wasRealized)
	UnrealizeTree(pWin, fromConfigure);
    if (wasViewable)
    {
	if (!fromConfigure)
	{
	    (* pWin->drawable.pScreen->ValidateTree)(pParent, pWin, VTUnmap);
	    HandleExposures(pParent);
	}
#ifdef DO_SAVE_UNDERS
	if (DO_SAVE_UNDERS(pWin))
	{
	    if (pWin->saveUnder ? ChangeSaveUnder(pWin, pWin->nextSib) :
				  CheckSaveUnder(pWin))
	    {
		DoChangeSaveUnder(pParent, pWin->nextSib);
	    }
	}
	pWin->DIXsaveUnder = FALSE;
#endif /* DO_SAVE_UNDERS */
	if (!fromConfigure && pWin->drawable.pScreen->PostValidateTree)
	    (* pWin->drawable.pScreen->PostValidateTree)(pParent,
							 pWin, VTUnmap);
    }
    if (wasRealized && !fromConfigure)
	WindowsRestructured ();
    return(Success);
}

/*****
 * UnmapSubwindows
 *    Performs an UnmapWindow request with the specified mode on all mapped
 *    children of the window, in bottom to top stacking order.
 *****/

UnmapSubwindows(pWin)
    register WindowPtr pWin;
{
    register WindowPtr pChild, pHead;
    xEvent event;
    Bool wasRealized = (Bool)pWin->realized;
    Bool wasViewable = (Bool)pWin->viewable;
    Bool anyMarked = FALSE;
    Mask parentNotify;

    if (!pWin->firstChild)
	return;
    parentNotify = SubSend(pWin);
    pHead = RealChildHead(pWin);
    for (pChild = pWin->lastChild; pChild != pHead; pChild = pChild->prevSib)
    {
	if (pChild->mapped)
        {
	    if (parentNotify || StrSend(pChild))
	    {
		event.u.u.type = UnmapNotify;
		event.u.unmapNotify.window = pChild->drawable.id;
		event.u.unmapNotify.fromConfigure = xFalse;
		DeliverEvents(pChild, &event, 1, NullWindow);
	    }
	    if (pChild->viewable)
	    {
		pChild->valdata = UnmapValData;
		anyMarked = TRUE;
	    }
	    pChild->mapped = FALSE;
            if (pChild->realized)
		UnrealizeTree(pChild, FALSE);
	    if (wasViewable)
	    {
#ifdef DO_SAVE_UNDERS
		pChild->DIXsaveUnder = FALSE;
#endif /* DO_SAVE_UNDERS */
		if (pChild->backStorage)
		    (*pChild->drawable.pScreen->SaveDoomedAreas)(
					    pChild, &pChild->clipList, 0, 0);
	    }
	}
    }
    if (wasViewable)
    {
	if (anyMarked)
	{
	    MarkWindow(pWin);
	    (* pWin->drawable.pScreen->ValidateTree)(pWin, pHead, VTUnmap);
	    HandleExposures(pWin);
	}
#ifdef DO_SAVE_UNDERS
	if (DO_SAVE_UNDERS(pWin))
	{
	    if (CheckSaveUnder(pWin->firstChild))
		DoChangeSaveUnder(pWin, pWin->firstChild);
	}
#endif /* DO_SAVE_UNDERS */
	if (anyMarked && pWin->drawable.pScreen->PostValidateTree)
	    (* pWin->drawable.pScreen->PostValidateTree)(pWin, pHead, VTUnmap);
    }
    if (wasRealized)
	WindowsRestructured ();
}


void
HandleSaveSet(client)
    register ClientPtr client;
{
    register WindowPtr pParent, pWin;
    register int j;

    for (j=0; j<client->numSaved; j++)
    {
        pWin = (WindowPtr)client->saveSet[j];
        pParent = pWin->parent;
        while (pParent && (wClient (pParent) == client))
            pParent = pParent->parent;
        if (pParent)
	{
            ReparentWindow(pWin, pParent, pWin->drawable.x - wBorderWidth (pWin),
			   pWin->drawable.y - wBorderWidth (pWin), client);
	    if(!pWin->realized && pWin->mapped)
		pWin->mapped = FALSE;
            MapWindow(pWin, client);
	}
    }
    xfree(client->saveSet);
    client->numSaved = 0;
    client->saveSet = (pointer *)NULL;
}

Bool
VisibleBoundingBoxFromPoint(pWin, x, y, box)
    register WindowPtr pWin;
    int x, y;   /* in root */
    BoxPtr box;   /* "return" value */
{
    if (!pWin->realized)
	return (FALSE);
    if ((* pWin->drawable.pScreen->PointInRegion)(&pWin->clipList, x, y, box))
        return(TRUE);
    return(FALSE);
}

Bool
PointInWindowIsVisible(pWin, x, y)
    register WindowPtr pWin;
    int x, y;	/* in root */
{
    BoxRec box;

    if (!pWin->realized)
	return (FALSE);
    if ((* pWin->drawable.pScreen->PointInRegion)(&pWin->borderClip,
						  x, y, &box))
        return(TRUE);
    return(FALSE);
}


RegionPtr
NotClippedByChildren(pWin)
    register WindowPtr pWin;
{
    register ScreenPtr pScreen;
    RegionPtr pReg;

    pScreen = pWin->drawable.pScreen;
    pReg = (* pScreen->RegionCreate)(NullBox, 1);
    if (pWin->parent ||
	screenIsSaved != SCREEN_SAVER_ON ||
	!HasSaverWindow (pWin->drawable.pScreen->myNum))
    {
	(* pScreen->Intersect) (pReg, &pWin->borderClip, &pWin->winSize);
    }
    return(pReg);
}


void
SendVisibilityNotify(pWin)
    WindowPtr pWin;
{
    xEvent event;
    event.u.u.type = VisibilityNotify;
    event.u.visibility.window = pWin->drawable.id;
    event.u.visibility.state = pWin->visibility;
    DeliverEvents(pWin, &event, 1, NullWindow);
}


#define RANDOM_WIDTH 32

#ifndef NOLOGOHACK
extern int logoScreenSaver;
static DrawLogo();
#endif

void
SaveScreens(on, mode)
    int on;
    int mode;
{
    int i;
    int what;

    if (on == SCREEN_SAVER_FORCER)
    {
	UpdateCurrentTimeIf();
	lastDeviceEventTime = currentTime;
        if (mode == ScreenSaverReset)
            what = SCREEN_SAVER_OFF;
        else
           what = SCREEN_SAVER_ON;
	if (what == screenIsSaved)
            return ;
    }
    else
        what = on;
    for (i = 0; i < screenInfo.numScreens; i++)
    {
        if (on == SCREEN_SAVER_FORCER)
        {
           (* screenInfo.screens[i]->SaveScreen) (screenInfo.screens[i], on);
        }
        if (what == SCREEN_SAVER_OFF)
        {
	    if (savedScreenInfo[i].blanked == SCREEN_IS_BLANKED)
	    {
	       (* screenInfo.screens[i]->SaveScreen) (screenInfo.screens[i],
						      what);
	    }
            else if (HasSaverWindow (i))
	    {
                savedScreenInfo[i].pWindow = NullWindow;
    	        FreeResource(savedScreenInfo[i].wid, RT_NONE);
	    }
	    continue;
        }
        else if (what == SCREEN_SAVER_ON)
        {
            if (screenIsSaved == SCREEN_SAVER_ON)  /* rotate pattern */
            {
		if (savedScreenInfo[i].blanked == SCREEN_IS_TILED)
	        {
		    WindowPtr pWin = savedScreenInfo[i].pWindow;
#ifndef NOLOGOHACK
		    if (logoScreenSaver)
			(*pWin->drawable.pScreen->ClearToBackground)(pWin, 0, 0, 0, 0, FALSE);
#endif
	            MoveWindow(pWin,
			       (short)(-(rand() % RANDOM_WIDTH)),
			       (short)(-(rand() % RANDOM_WIDTH)),
		               pWin->nextSib, VTMove);
#ifndef NOLOGOHACK
		    if (logoScreenSaver)
			DrawLogo(pWin);
#endif
		}
		continue;
	    }
            if (ScreenSaverBlanking != DontPreferBlanking)
	    {
    	    	if ((* screenInfo.screens[i]->SaveScreen)
       	       	   (screenInfo.screens[i], what))
    	    	{
       	       	   savedScreenInfo[i].blanked = SCREEN_IS_BLANKED;
       	       	   continue;
    	    	}
    	    	if ((ScreenSaverAllowExposures != DontAllowExposures) &&
		    TileScreenSaver(i, SCREEN_IS_BLACK))
		{
		    savedScreenInfo[i].blanked = SCREEN_IS_BLACK;
		    continue;
    	    	}
	    }
	    if ((ScreenSaverAllowExposures != DontAllowExposures) &&
		TileScreenSaver(i, SCREEN_IS_TILED))
            {
	        savedScreenInfo[i].blanked = SCREEN_IS_TILED;
	    }
            else
	        savedScreenInfo[i].blanked = SCREEN_ISNT_SAVED;
	}
    }
    screenIsSaved = what;
}

static Bool
TileScreenSaver(i, kind)
    int i;
    int	kind;
{
    int j;
    int result;
    XID attributes[3];
    Mask mask;
    WindowPtr pWin;		
    CursorMetricRec cm;
    unsigned char *srcbits, *mskbits;
    CursorPtr cursor;
    XID	    cursorID;
    int	attri;

    mask = 0;
    attri = 0;
    switch (kind) {
    case SCREEN_IS_TILED:
    	switch (WindowTable[i]->backgroundState) {
    	case BackgroundPixel:
	    attributes[attri++] = WindowTable[i]->background.pixel;
	    mask |= CWBackPixel;
	    break;
    	case BackgroundPixmap:
	    attributes[attri++] = None;
	    mask |= CWBackPixmap;
	    break;
    	default:
	    break;
    	}
	break;
    case SCREEN_IS_BLACK:
	attributes[attri++] = WindowTable[i]->drawable.pScreen->blackPixel;
	mask |= CWBackPixel;
	break;
    }
    mask |= CWOverrideRedirect;
    attributes[attri++] = xTrue;

    /*
     * create a blank cursor
     */

    cm.width=16;
    cm.height=16;
    cm.xhot=8;
    cm.yhot=8;
    srcbits = (unsigned char *)xalloc( PixmapBytePad(32, 1)*16);
    mskbits = (unsigned char *)xalloc( PixmapBytePad(32, 1)*16);
    if (!srcbits || !mskbits)
    {
	xfree(srcbits);
	xfree(mskbits);
	cursor = 0;
    }
    else
    {
	for (j=0; j<PixmapBytePad(32, 1)*16; j++)
	    srcbits[j] = mskbits[j] = 0x0;
	cursor = AllocCursor(srcbits, mskbits, &cm, 0, 0, 0, 0, 0, 0);
	if (cursor)
	{
	    cursorID = FakeClientID(0);
	    if (AddResource (cursorID, RT_CURSOR, (pointer) cursor))
	    {
	    	attributes[attri] = cursorID;
	    	mask |= CWCursor;
	    }
	    else
		cursor = 0;
	}
	else
	{
	    xfree (srcbits);
	    xfree (mskbits);
	}
    }

    pWin = savedScreenInfo[i].pWindow =
	 CreateWindow(savedScreenInfo[i].wid,
	      WindowTable[i],
	      -RANDOM_WIDTH, -RANDOM_WIDTH,
	      (unsigned short)screenInfo.screens[i]->width + RANDOM_WIDTH,
	      (unsigned short)screenInfo.screens[i]->height + RANDOM_WIDTH,
	      0, InputOutput, mask, attributes, 0, serverClient,
	      wVisual (WindowTable[i]), &result);

    if (cursor)
	FreeResource (cursorID, RT_NONE);

    if (!pWin)
	return FALSE;

    if (!AddResource(pWin->drawable.id, RT_WINDOW,
		     (pointer)savedScreenInfo[i].pWindow))
	return FALSE;

    if (mask & CWBackPixmap)
    {
	pWin->backgroundState = BackgroundPixmap;
	pWin->background.pixmap = pWin->parent->background.pixmap;
	pWin->background.pixmap->refcnt++;
	(*pWin->drawable.pScreen->ChangeWindowAttributes)(pWin, CWBackPixmap);
    }

    MapWindow(pWin, serverClient);
#ifndef NOLOGOHACK
    if (kind == SCREEN_IS_TILED && logoScreenSaver)
	DrawLogo(pWin);
#endif
    return TRUE;
}

/*
 * FindWindowWithOptional
 *
 * search ancestors of the given window for an entry containing
 * a WindowOpt structure.  Assumptions:  some parent will
 * contain the structure.
 */

WindowPtr
FindWindowWithOptional (w)
    register WindowPtr w;
{
    do
	w = w->parent;
    while (!w->optional);
    return w;
}

/*
 * CheckWindowOptionalNeed
 *
 * check each optional entry in the given window to see if
 * the value is satisfied by the default rules.  If so,
 * release the optional record
 */

CheckWindowOptionalNeed (w)
    register WindowPtr w;
{
    register WindowOptPtr optional;
    register WindowOptPtr parentOptional;

    if (!w->parent)
	return;
    optional = w->optional;
    if (optional->dontPropagateMask != DontPropagateMasks[w->dontPropagate])
	return;
    if (optional->otherEventMasks != 0)
	return;
    if (optional->otherClients != NULL)
	return;
    if (optional->passiveGrabs != NULL)
	return;
    if (optional->userProps != NULL)
	return;
    if (optional->backingBitPlanes != ~0L)
	return;
    if (optional->backingPixel != 0)
	return;
#ifdef SHAPE
    if (optional->boundingShape != NULL)
	return;
    if (optional->clipShape != NULL)
	return;
#endif
#ifdef XINPUT
    if (optional->inputMasks != NULL)
	return;
#endif
    parentOptional = FindWindowWithOptional(w)->optional;
    if (optional->visual != parentOptional->visual)
	return;
    if (optional->cursor != None &&
	(optional->cursor != parentOptional->cursor ||
	 w->parent->cursorIsNone))
	return;
    if (optional->colormap != parentOptional->colormap)
	return;
    DisposeWindowOptional (w);
}

/*
 * MakeWindowOptional
 *
 * create an optional record and initialize it with the default
 * values.
 */

Bool
MakeWindowOptional (pWin)
    register WindowPtr pWin;
{
    register WindowOptPtr optional;
    register WindowOptPtr parentOptional;

    if (pWin->optional)
	return TRUE;
    optional = (WindowOptPtr) xalloc (sizeof (WindowOptRec));
    if (!optional)
	return FALSE;
    optional->dontPropagateMask = DontPropagateMasks[pWin->dontPropagate];
    optional->otherEventMasks = 0;
    optional->otherClients = NULL;
    optional->passiveGrabs = NULL;
    optional->userProps = NULL;
    optional->backingBitPlanes = ~0L;
    optional->backingPixel = 0;
#ifdef SHAPE
    optional->boundingShape = NULL;
    optional->clipShape = NULL;
#endif
#ifdef XINPUT
    optional->inputMasks = NULL;
#endif
    parentOptional = FindWindowWithOptional(pWin)->optional;
    optional->visual = parentOptional->visual;
    if (!pWin->cursorIsNone)
    {
	optional->cursor = parentOptional->cursor;
	optional->cursor->refcnt++;
    }
    else
    {
	optional->cursor = None;
    }
    optional->colormap = parentOptional->colormap;
    pWin->optional = optional;
    return TRUE;
}

DisposeWindowOptional (pWin)
    register WindowPtr pWin;
{
    if (!pWin->optional)
	return;
    /*
     * everything is peachy.  Delete the optional record
     * and clean up
     */
    if (pWin->optional->cursor)
    {
	FreeCursor (pWin->optional->cursor, (Cursor)0);
	pWin->cursorIsNone = FALSE;
    }
    else
	pWin->cursorIsNone = TRUE;
    xfree (pWin->optional);
    pWin->optional = NULL;
}

#ifndef NOLOGOHACK
static
DrawLogo(pWin)
    WindowPtr pWin;
{
    DrawablePtr pDraw;
    ScreenPtr pScreen;
    int x, y;
    unsigned int width, height, size;
    GC *pGC;
    int d11, d21, d31;
    xPoint poly[4];
    XID fore[2], back[2];
    xrgb rgb[2];
    BITS32 fmask, bmask;
    ColormapPtr cmap;

    pDraw = (DrawablePtr)pWin;
    pScreen = pDraw->pScreen;
    x = -pWin->origin.x;
    y = -pWin->origin.y;
    width = pScreen->width;
    height = pScreen->height;
    pGC = GetScratchGC(pScreen->rootDepth, pScreen);
    if (!pGC)
	return;

    if ((rand() % 100) <= 17) /* make the probability for white fairly low */
	fore[0] = pScreen->whitePixel;
    else
	fore[0] = pScreen->blackPixel;
    if ((pWin->backgroundState == BackgroundPixel) &&
	(cmap = (ColormapPtr)LookupIDByType(wColormap (pWin), RT_COLORMAP))) {
	fore[1] = pWin->background.pixel;
	QueryColors(cmap, 2, fore, rgb);
	if ((rgb[0].red == rgb[1].red) &&
	    (rgb[0].green == rgb[1].green) &&
	    (rgb[0].blue == rgb[1].blue)) {
	    if (fore[0] == pScreen->blackPixel)
		fore[0] = pScreen->whitePixel;
	    else
		fore[0] = pScreen->blackPixel;
	}
    }
    fore[1] = FillSolid;
    fmask = GCForeground|GCFillStyle;
    if (pWin->backgroundState == BackgroundPixel) {
	back[0] = pWin->background.pixel;
	back[1] = FillSolid;
	bmask = GCForeground|GCFillStyle;
    } else {
	back[0] = 0;
	back[1] = 0;
	(void)DoChangeGC(pGC, GCTileStipXOrigin|GCTileStipYOrigin, back, 0);
	back[0] = FillTiled;
	back[1] = (XID)pWin->background.pixmap;
	bmask = GCFillStyle|GCTile;
    }

    size = width;
    if (height < width)
	 size = height;
    size = RANDOM_WIDTH + rand() % (size - RANDOM_WIDTH);
    size &= ~1;
    x += rand() % (width - size);
    y += rand() % (height - size);

/*
 *           -----
 *          /    /
 *         /    /
 *        /    /
 *       /    /
 *      /____/
 */

    d11 = (size / 11);
    if (d11 < 1) d11 = 1;
    d21 = (d11+3) / 4;
    d31 = d11 + d11 + d21;
    poly[0].x = x + size;              poly[0].y = y;
    poly[1].x = x + size-d31;          poly[1].y = y;
    poly[2].x = x + 0;                 poly[2].y = y + size;
    poly[3].x = x + d31;               poly[3].y = y + size;
    (void)DoChangeGC(pGC, fmask, fore, 1);
    ValidateGC(pDraw, pGC);
    (*pGC->ops->FillPolygon)(pDraw, pGC, Convex, CoordModeOrigin, 4, poly);

/*
 *           ------
 *          /     /
 *         /  __ /
 *        /  /  /
 *       /  /  /
 *      /__/__/
 */

    poly[0].x = x + d31/2;                       poly[0].y = y + size;
    poly[1].x = x + size / 2;                    poly[1].y = y + size/2;
    poly[2].x = x + (size/2)+(d31-(d31/2));      poly[2].y = y + size/2;
    poly[3].x = x + d31;                         poly[3].y = y + size;
    (void)DoChangeGC(pGC, bmask, back, 1);
    ValidateGC(pDraw, pGC);
    (*pGC->ops->FillPolygon)(pDraw, pGC, Convex, CoordModeOrigin, 4, poly);

/*
 *           ------
 *          /  /  /
 *         /--/  /
 *        /     /
 *       /     /
 *      /_____/
 */

    poly[0].x = x + size - d31/2;                poly[0].y = y;
    poly[1].x = x + size / 2;                    poly[1].y = y + size/2;
    poly[2].x = x + (size/2)-(d31-(d31/2));      poly[2].y = y + size/2;
    poly[3].x = x + size - d31;                  poly[3].y = y;
    ValidateGC(pDraw, pGC);
    (*pGC->ops->FillPolygon)(pDraw, pGC, Convex, CoordModeOrigin, 4, poly);

/*
 * -----
 * \    \
 *  \    \
 *   \    \
 *    \    \
 *     \____\
 */

    poly[0].x = x;                     poly[0].y = y;
    poly[1].x = x + size/4;            poly[1].y = y;
    poly[2].x = x + size;              poly[2].y = y + size;
    poly[3].x = x + size - size/4;     poly[3].y = y + size;
    (void)DoChangeGC(pGC, fmask, fore, 1);
    ValidateGC(pDraw, pGC);
    (*pGC->ops->FillPolygon)(pDraw, pGC, Convex, CoordModeOrigin, 4, poly);

/*
 *          /
 *         /
 *        /
 *       /
 *      /
 */

    poly[0].x = x + size- d11;        poly[0].y = y;
    poly[1].x = x + size-( d11+d21);  poly[1].y = y;
    poly[2].x = x + d11;              poly[2].y = y + size;
    poly[3].x = x + d11 + d21;        poly[3].y = y + size;
    (void)DoChangeGC(pGC, bmask, back, 1);
    ValidateGC(pDraw, pGC);
    (*pGC->ops->FillPolygon)(pDraw, pGC, Convex, CoordModeOrigin, 4, poly);

    FreeScratchGC(pGC);
}

#endif
