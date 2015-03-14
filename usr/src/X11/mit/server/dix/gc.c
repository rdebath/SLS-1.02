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

/* $XConsortium: gc.c,v 5.17 91/05/10 17:40:06 keith Exp $ */

#include "X.h"
#include "Xmd.h"
#include "Xproto.h"
#include "misc.h"
#include "resource.h"
#include "gcstruct.h"
#include "pixmapstr.h"
#include "dixfontstr.h"
#include "scrnintstr.h"
#include "region.h"

#include "dix.h"

extern XID clientErrorValue;

static Bool CreateDefaultTile();

unsigned char DefaultDash[2] = {4, 4};

void
ValidateGC(pDraw, pGC)
    DrawablePtr	pDraw;
    GC		*pGC;
{
    (*pGC->funcs->ValidateGC) (pGC, pGC->stateChanges, pDraw);
    pGC->stateChanges = 0;
    pGC->serialNumber = pDraw->serialNumber;
}



/* Publically defined entry to ChangeGC.  Just calls DoChangeGC and tells
 * it that all of the entries are constants or IDs */
int
ChangeGC(pGC, mask, pval)
    register GC 	*pGC;
    register BITS32	mask;
    XID			*pval;
{
    return (DoChangeGC(pGC, mask, pval, 0));
}
/* DoChangeGC(pGC, mask, pval, fPointer)
   mask is a set of bits indicating which values to change.
   pval contains an appropriate value for each mask.
   fPointer is true if the values for tiles, stipples, fonts or clipmasks
   are pointers instead of IDs.  
   if there is an error, the value is marked as changed 
   anyway, which is probably wrong, but infrequent.

NOTE:
	all values sent over the protocol for ChangeGC requests are
32 bits long
*/

int
DoChangeGC(pGC, mask, pval, fPointer)
    register GC 	*pGC;
    register BITS32	mask;
    XID			*pval;
    int			fPointer;
{
    register BITS32 	index;
    register int 	error = 0;
    PixmapPtr 		pPixmap;
    BITS32		maskQ;

    pGC->serialNumber |= GC_CHANGE_SERIAL_BIT;

    maskQ = mask;	/* save these for when we walk the GCque */
    while (mask && !error) 
    {
	index = (BITS32) lowbit (mask);
	mask &= ~index;
	pGC->stateChanges |= index;
	switch (index)
	{
	    case GCFunction:
		if (((CARD8)*pval >= GXclear) && ((CARD8)*pval <= GXset))
		    pGC->alu = (CARD8)*pval;
		else
		{
		    clientErrorValue = (CARD8)*pval;
		    error = BadValue;
		}
		pval++;
		break;
	    case GCPlaneMask:
		pGC->planemask = *pval++;
		break;
	    case GCForeground:
		pGC->fgPixel = *pval++;
		/*
		 * this is for CreateGC
		 */
		if (!pGC->tileIsPixel && !pGC->tile.pixmap)
		{
		    pGC->tileIsPixel = TRUE;
		    pGC->tile.pixel = pGC->fgPixel;
		}
		break;
	    case GCBackground:
		pGC->bgPixel = *pval++;
		break;
	    case GCLineWidth:		/* ??? line width is a CARD16 */
		pGC->lineWidth = (CARD16)*pval;
                pval++;
		break;
	    case GCLineStyle:
		if (((CARD8)*pval >= LineSolid) 
		    && ((CARD8)*pval <= LineDoubleDash))
		    pGC->lineStyle = (CARD8)*pval;
		else
		{
		    clientErrorValue = (CARD8)*pval;
		    error = BadValue;
		}
		pval++;
		break;
	    case GCCapStyle:
		if (((CARD8)*pval >= CapNotLast) 
		    && ((CARD8)*pval <= CapProjecting))
		    pGC->capStyle = (CARD8)*pval;
		else
		{
		    clientErrorValue = (CARD8)*pval;
		    error = BadValue;
		}
		pval++;
		break;
	    case GCJoinStyle:
		if (((CARD8)*pval >= JoinMiter) && ((CARD8)*pval <= JoinBevel))
		    pGC->joinStyle = (CARD8)*pval;
		else
		{
		    clientErrorValue = (CARD8)*pval;
		    error = BadValue;
		}
		pval++;
		break;
	    case GCFillStyle:
		if (((CARD8)*pval >= FillSolid) 
		    && ((CARD8)*pval <= FillOpaqueStippled))
		    pGC->fillStyle = (CARD8)*pval;
		else
		{
		    clientErrorValue = (CARD8)*pval;
		    error = BadValue;
		}
		pval++;
		break;
	    case GCFillRule:
		if (((CARD8)*pval >= EvenOddRule) && 
		    ((CARD8)*pval <= WindingRule))
		    pGC->fillRule = (CARD8)*pval;
		else
		{
		    clientErrorValue = (CARD8)*pval;
		    error = BadValue;
		}
		pval++;
		break;
	    case GCTile:
		if(fPointer)
		    pPixmap = (PixmapPtr) *pval;
		else
		    pPixmap = (PixmapPtr)LookupIDByType((CARD32)*pval, 
							RT_PIXMAP);
		if (pPixmap)
		{
		    if ((pPixmap->drawable.depth != pGC->depth) ||
			(pPixmap->drawable.pScreen != pGC->pScreen))
		    {
			error = BadMatch;
		    }
		    else
		    {
			pPixmap->refcnt++;
			if (!pGC->tileIsPixel)
			    (* pGC->pScreen->DestroyPixmap)(pGC->tile.pixmap);
			pGC->tileIsPixel = FALSE;
			pGC->tile.pixmap = pPixmap;
		    }
		}
		else
		{
		    clientErrorValue = (CARD32)*pval;
		    error = BadPixmap;
		}
		pval++;
		break;
	    case GCStipple:
		if(fPointer)
		    pPixmap = (PixmapPtr) *pval;
		else
		    pPixmap = (PixmapPtr)LookupIDByType((CARD32)*pval, 
							RT_PIXMAP);
		if (pPixmap)
		{
		    if ((pPixmap->drawable.depth != 1) ||
			(pPixmap->drawable.pScreen != pGC->pScreen))
		    {
			error = BadMatch;
		    }
		    else
		    {
			pPixmap->refcnt++;
			if (pGC->stipple)
			    (* pGC->pScreen->DestroyPixmap)(pGC->stipple);
			pGC->stipple = pPixmap;
		    }
		}
		else
		{
		    clientErrorValue = (CARD32)*pval;
		    error = BadPixmap;
		}
		pval++;
		break;
	    case GCTileStipXOrigin:
		pGC->patOrg.x = (INT16)*pval;
                pval++;
		break;
	    case GCTileStipYOrigin:
		pGC->patOrg.y = (INT16)*pval;
		pval++;
		break;
	    case GCFont:
              {
		FontPtr	pFont;


		if(fPointer)
		    pFont = (FontPtr) *pval;
		else
		    pFont = (FontPtr)LookupIDByType((CARD32)*pval, RT_FONT);

		if (pFont)
		{
		    pFont->refcnt++;
		    if (pGC->font)
    		        CloseFont(pGC->font, (Font)0);
		    pGC->font = pFont;
		 }
		else
		{
		    clientErrorValue = *pval;
		    error = BadFont;
		}
		pval++;
		break;
	      }
	    case GCSubwindowMode:
		if (((CARD8)*pval == ClipByChildren) ||
		    ((CARD8)*pval == IncludeInferiors))
		    pGC->subWindowMode = (CARD8)*pval;
		else
		{
		    clientErrorValue = (CARD8)*pval;
		    error = BadValue;
		}
		pval++;
		break;
	    case GCGraphicsExposures:
		if ((Bool)*pval == xFalse)
		    pGC->graphicsExposures = FALSE;
		else if ((Bool)*pval == xTrue)
		    pGC->graphicsExposures = TRUE;
		else
		{
		    clientErrorValue = (Bool)*pval;
		    error = BadValue;
		}
		pval++;
		break;
	    case GCClipXOrigin:
		pGC->clipOrg.x = (INT16)(*pval);
		pval++;
		break;
	    case GCClipYOrigin:
		pGC->clipOrg.y = (INT16)(*pval);
		pval++;
		break;
	    case GCClipMask:
	      {
		Pixmap pid;
		int	clipType;

		pid = (Pixmap) *pval;
		if (pid == None)
		{
		    clipType = CT_NONE;
		}
		else
		{
		    if(fPointer)
			pPixmap = (PixmapPtr) *pval;
		    else
		        pPixmap = (PixmapPtr)LookupIDByType(pid, RT_PIXMAP);
		    if (pPixmap)
  		    {
			if ((pPixmap->drawable.depth != 1) ||
			    (pPixmap->drawable.pScreen != pGC->pScreen))
			{
			    error = BadMatch;
			}
			else
			{
			    clipType = CT_PIXMAP;
			    pPixmap->refcnt++;
			}
		    }
		    else
		    {
			clientErrorValue = pid;
			error = BadPixmap;
		    }
		}
		pval++;
		if(error == Success)
		{
		    (*pGC->funcs->ChangeClip)(pGC, clipType, pPixmap, 0);
		}
		break;
	      }
	    case GCDashOffset:
		pGC->dashOffset = (CARD16)*pval;
		pval++;
		break;
	    case GCDashList:
		if ((CARD8) (*pval) == 4)
		{
		    if (pGC->dash != DefaultDash)
		    {
			xfree(pGC->dash);
			pGC->numInDashList = 2;
			pGC->dash = DefaultDash;
		    }
		}
		else if ((CARD8) (*pval) != 0)
 		{
		    unsigned char *dash;

		    dash = (unsigned char *)xalloc(2 * sizeof(unsigned char));
		    if (dash)
		    {
			if (pGC->dash != DefaultDash)
			    xfree(pGC->dash);
			pGC->numInDashList = 2;
			pGC->dash = dash;
			dash[0] = (CARD8)(*pval);
			dash[1] = (CARD8)(*pval);
		    }
		    else
			error = BadAlloc;
		}
 		else
		{
		   clientErrorValue = (CARD8)*pval;
		   error = BadValue;
		}
		pval++;
		break;
	    case GCArcMode:
		if (((CARD8)*pval >= ArcChord) 
		    && ((CARD8)*pval <= ArcPieSlice))
		    pGC->arcMode = (CARD8)*pval;
		else
		{
		    clientErrorValue = (CARD8)*pval;
		    error = BadValue;
		}
		pval++;
		break;
	    default:
		clientErrorValue = maskQ;
		error = BadValue;
		pval++;
		break;
	}
    }
    if (pGC->fillStyle == FillTiled && pGC->tileIsPixel)
    {
	if (!CreateDefaultTile (pGC))
	{
	    pGC->fillStyle = FillSolid;
	    error = BadAlloc;
	}
    }
    (*pGC->funcs->ChangeGC)(pGC, maskQ);
    return error;
}

/* CreateGC(pDrawable, mask, pval, pStatus)
   creates a default GC for the given drawable, using mask to fill
   in any non-default values.
   Returns a pointer to the new GC on success, NULL otherwise.
   returns status of non-default fields in pStatus
BUG:
   should check for failure to create default tile

*/

static int  gcPrivateCount;

void
ResetGCPrivates()
{
    gcPrivateCount = 0;
}

int
AllocateGCPrivateIndex()
{
    return gcPrivateCount++;
}

static GCPtr
AllocateGC(pScreen)
    ScreenPtr pScreen;
{
    GCPtr pGC;
    register char *ptr;
    register DevUnion *ppriv;
    register unsigned *sizes;
    register unsigned size;
    register int i;

    pGC = (GCPtr)xalloc(pScreen->totalGCSize);
    if (pGC)
    {
	ppriv = (DevUnion *)(pGC + 1);
	pGC->devPrivates = ppriv;
	sizes = pScreen->GCPrivateSizes;
	ptr = (char *)(ppriv + pScreen->GCPrivateLen);
	for (i = pScreen->GCPrivateLen; --i >= 0; ppriv++, sizes++)
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
    return pGC;
}

GCPtr
CreateGC(pDrawable, mask, pval, pStatus)
    DrawablePtr	pDrawable;
    BITS32	mask;
    XID		*pval;
    int		*pStatus;
{
    register GCPtr pGC;
    extern FontPtr defaultFont;

    pGC = AllocateGC(pDrawable->pScreen);
    if (!pGC)
    {
	*pStatus = BadAlloc;
	return (GCPtr)NULL;
    }

    pGC->pScreen = pDrawable->pScreen;
    pGC->depth = pDrawable->depth;
    pGC->alu = GXcopy; /* dst <- src */
    pGC->planemask = ~0;
    pGC->serialNumber = GC_CHANGE_SERIAL_BIT;
    pGC->funcs = 0;

    pGC->fgPixel = 0;
    pGC->bgPixel = 1;
    pGC->lineWidth = 0;
    pGC->lineStyle = LineSolid;
    pGC->capStyle = CapButt;
    pGC->joinStyle = JoinMiter;
    pGC->fillStyle = FillSolid;
    pGC->fillRule = EvenOddRule;
    pGC->arcMode = ArcPieSlice;
    if (mask & GCForeground)
    {
	/*
	 * magic special case -- ChangeGC checks for this condition
	 * and snags the Foreground value to create a pseudo default-tile
	 */
	pGC->tileIsPixel = FALSE;
	pGC->tile.pixmap = NullPixmap;
    }
    else
    {
	pGC->tileIsPixel = TRUE;
	pGC->tile.pixel = 0;
    }

    pGC->patOrg.x = 0;
    pGC->patOrg.y = 0;
    pGC->subWindowMode = ClipByChildren;
    pGC->graphicsExposures = TRUE;
    pGC->clipOrg.x = 0;
    pGC->clipOrg.y = 0;
    pGC->clientClipType = CT_NONE;
    pGC->clientClip = (pointer)NULL;
    pGC->numInDashList = 2;
    pGC->dash = DefaultDash;
    pGC->dashOffset = 0;
    pGC->lastWinOrg.x = 0;
    pGC->lastWinOrg.y = 0;

    /* use the default font and stipple */
    pGC->font = defaultFont;
    defaultFont->refcnt++;
    pGC->stipple = pGC->pScreen->PixmapPerDepth[0];
    pGC->stipple->refcnt++;

    pGC->stateChanges = (1 << GCLastBit+1) - 1;
    if (!(*pGC->pScreen->CreateGC)(pGC))
	*pStatus = BadAlloc;
    else if (mask)
        *pStatus = ChangeGC(pGC, mask, pval);
    else
	*pStatus = Success;
    if (*pStatus != Success)
    {
	if (!pGC->tileIsPixel && !pGC->tile.pixmap)
	    pGC->tileIsPixel = TRUE; /* undo special case */
	FreeGC(pGC, (GContext)0);
	pGC = (GCPtr)NULL;
    }

    return (pGC);
}

static Bool
CreateDefaultTile (pGC)
    GCPtr   pGC;
{
    XID		tmpval[3];
    PixmapPtr 	pTile;
    GCPtr	pgcScratch;
    xRectangle	rect;
    short	w, h;

    w = 1;
    h = 1;
    (*pGC->pScreen->QueryBestSize)(TileShape, &w, &h, pGC->pScreen);
    pTile = (PixmapPtr)
	    (*pGC->pScreen->CreatePixmap)(pGC->pScreen,
					  w, h, pGC->depth);
    pgcScratch = GetScratchGC(pGC->depth, pGC->pScreen);
    if (!pTile || !pgcScratch)
    {
	if (pTile)
	    (*pTile->drawable.pScreen->DestroyPixmap)(pTile);
	if (pgcScratch)
	    FreeScratchGC(pgcScratch);
	return FALSE;
    }
    tmpval[0] = GXcopy;
    tmpval[1] = pGC->tile.pixel;
    tmpval[2] = FillSolid;
    (void)ChangeGC(pgcScratch, GCFunction | GCForeground | GCFillStyle, 
		   tmpval);
    ValidateGC((DrawablePtr)pTile, pgcScratch);
    rect.x = 0;
    rect.y = 0;
    rect.width = w;
    rect.height = h;
    (*pgcScratch->ops->PolyFillRect)(pTile, pgcScratch, 1, &rect);
    /* Always remember to free the scratch graphics context after use. */
    FreeScratchGC(pgcScratch);

    pGC->tileIsPixel = FALSE;
    pGC->tile.pixmap = pTile;
    return TRUE;
}

int
CopyGC(pgcSrc, pgcDst, mask)
    register GC		*pgcSrc;
    register GC		*pgcDst;
    register BITS32	mask;
{
    register BITS32	index;
    BITS32		maskQ;
    int i;
    int 		error = 0;

    if (pgcSrc == pgcDst)
	return Success;
    pgcDst->serialNumber |= GC_CHANGE_SERIAL_BIT;
    pgcDst->stateChanges |= mask;
    maskQ = mask;
    while (mask)
    {
	index = (BITS32) lowbit (mask);
	mask &= ~index;
	switch (index)
	{
	    case GCFunction:
		pgcDst->alu = pgcSrc->alu;
		break;
	    case GCPlaneMask:
		pgcDst->planemask = pgcSrc->planemask;
		break;
	    case GCForeground:
		pgcDst->fgPixel = pgcSrc->fgPixel;
		break;
	    case GCBackground:
		pgcDst->bgPixel = pgcSrc->bgPixel;
		break;
	    case GCLineWidth:
		pgcDst->lineWidth = pgcSrc->lineWidth;
		break;
	    case GCLineStyle:
		pgcDst->lineStyle = pgcSrc->lineStyle;
		break;
	    case GCCapStyle:
		pgcDst->capStyle = pgcSrc->capStyle;
		break;
	    case GCJoinStyle:
		pgcDst->joinStyle = pgcSrc->joinStyle;
		break;
	    case GCFillStyle:
		pgcDst->fillStyle = pgcSrc->fillStyle;
		break;
	    case GCFillRule:
		pgcDst->fillRule = pgcSrc->fillRule;
		break;
	    case GCTile:
		{
		    if (EqualPixUnion(pgcDst->tileIsPixel,
				      pgcDst->tile,
				      pgcSrc->tileIsPixel,
				      pgcSrc->tile))
		    {
			break;
		    }
		    if (!pgcDst->tileIsPixel)
			(* pgcDst->pScreen->DestroyPixmap)(pgcDst->tile.pixmap);
		    pgcDst->tileIsPixel = pgcSrc->tileIsPixel;
		    pgcDst->tile = pgcSrc->tile;
		    if (!pgcDst->tileIsPixel)
		       pgcDst->tile.pixmap->refcnt++;
		    break;
		}
	    case GCStipple:
		{
		    if (pgcDst->stipple == pgcSrc->stipple)
			break;
		    if (pgcDst->stipple)
			(* pgcDst->pScreen->DestroyPixmap)(pgcDst->stipple);
		    pgcDst->stipple = pgcSrc->stipple;
		    if (pgcDst->stipple)
			pgcDst->stipple->refcnt ++;
		    break;
		}
	    case GCTileStipXOrigin:
		pgcDst->patOrg.x = pgcSrc->patOrg.x;
		break;
	    case GCTileStipYOrigin:
		pgcDst->patOrg.y = pgcSrc->patOrg.y;
		break;
	    case GCFont:
		if (pgcDst->font == pgcSrc->font)
		    break;
		if (pgcDst->font)
		    CloseFont(pgcDst->font, (Font)0);
		if ((pgcDst->font = pgcSrc->font) != NullFont)
		    (pgcDst->font)->refcnt++;
		break;
	    case GCSubwindowMode:
		pgcDst->subWindowMode = pgcSrc->subWindowMode;
		break;
	    case GCGraphicsExposures:
		pgcDst->graphicsExposures = pgcSrc->graphicsExposures;
		break;
	    case GCClipXOrigin:
		pgcDst->clipOrg.x = pgcSrc->clipOrg.x;
		break;
	    case GCClipYOrigin:
		pgcDst->clipOrg.y = pgcSrc->clipOrg.y;
		break;
	    case GCClipMask:
		(* pgcDst->funcs->CopyClip)(pgcDst, pgcSrc);
		break;
	    case GCDashOffset:
		pgcDst->dashOffset = pgcSrc->dashOffset;
		break;
	    case GCDashList:
		if (pgcSrc->dash == DefaultDash)
		{
		    if (pgcDst->dash != DefaultDash)
		    {
			xfree(pgcDst->dash);
			pgcDst->numInDashList = pgcSrc->numInDashList;
			pgcDst->dash = pgcSrc->dash;
		    }
		}
		else
		{
		    unsigned char *dash;

		    dash = (unsigned char *)xalloc(pgcSrc->numInDashList *
						   sizeof(unsigned char));
		    if (dash)
		    {
			if (pgcDst->dash != DefaultDash)
			    xfree(pgcDst->dash);
			pgcDst->numInDashList = pgcSrc->numInDashList;
			pgcDst->dash = dash;
			for (i=0; i<pgcSrc->numInDashList; i++)
			    dash[i] = pgcSrc->dash[i];
		    }
		    else
			error = BadAlloc;
		}
		break;
	    case GCArcMode:
		pgcDst->arcMode = pgcSrc->arcMode;
		break;
	    default:
		clientErrorValue = maskQ;
		error = BadValue;
		break;
	}
    }
    if (pgcDst->fillStyle == FillTiled && pgcDst->tileIsPixel)
    {
	if (!CreateDefaultTile (pgcDst))
	{
	    pgcDst->fillStyle = FillSolid;
	    error = BadAlloc;
	}
    }
    (*pgcDst->funcs->CopyGC) (pgcSrc, maskQ, pgcDst);
    return error;
}

/*****************
 * FreeGC 
 *   does the diX part of freeing the characteristics in the GC 
 ***************/

/*ARGSUSED*/
int
FreeGC(pGC, gid)
    GCPtr pGC;
    GContext gid;
{
    CloseFont(pGC->font, (Font)0);
    (* pGC->funcs->DestroyClip)(pGC);

    if (!pGC->tileIsPixel)
	(* pGC->pScreen->DestroyPixmap)(pGC->tile.pixmap);
    if (pGC->stipple)
	(* pGC->pScreen->DestroyPixmap)(pGC->stipple);

    (*pGC->funcs->DestroyGC) (pGC);
    if (pGC->dash != DefaultDash)
	xfree(pGC->dash);
    xfree(pGC);
    return(Success);
}

void
SetGCMask(pGC, selectMask, newDataMask)
    GCPtr pGC;
    Mask selectMask;
    Mask newDataMask;
{
    pGC->stateChanges = (~selectMask & pGC->stateChanges) |
		        (selectMask & newDataMask);
    if (selectMask & newDataMask)
        pGC->serialNumber |= GC_CHANGE_SERIAL_BIT;        
}



/* CreateScratchGC(pScreen, depth)
    like CreateGC, but doesn't do the default tile or stipple,
since we can't create them without already having a GC.  any code
using the tile or stipple has to set them explicitly anyway,
since the state of the scratch gc is unknown.  This is OK
because ChangeGC() has to be able to deal with NULL tiles and
stipples anyway (in case the CreateGC() call has provided a 
value for them -- we can't set the default tile until the
client-supplied attributes are installed, since the fgPixel
is what fills the default tile.  (maybe this comment should
go with CreateGC() or ChangeGC().)
*/

GCPtr
CreateScratchGC(pScreen, depth)
    ScreenPtr pScreen;
    unsigned depth;
{
    register GCPtr pGC;
    extern FontPtr defaultFont;

    pGC = AllocateGC(pScreen);
    if (!pGC)
	return (GCPtr)NULL;

    pGC->pScreen = pScreen;
    pGC->depth = depth;
    pGC->alu = GXcopy; /* dst <- src */
    pGC->planemask = ~0;
    pGC->serialNumber = 0;

    pGC->fgPixel = 0;
    pGC->bgPixel = 1;
    pGC->lineWidth = 0;
    pGC->lineStyle = LineSolid;
    pGC->capStyle = CapButt;
    pGC->joinStyle = JoinMiter;
    pGC->fillStyle = FillSolid;
    pGC->fillRule = EvenOddRule;
    pGC->arcMode = ArcPieSlice;
    pGC->font = defaultFont;
    if ( pGC->font)  /* necessary, because open of default font could fail */
	pGC->font->refcnt++;
    pGC->tileIsPixel = TRUE;
    pGC->tile.pixel = 0;
    pGC->stipple = NullPixmap;
    pGC->patOrg.x = 0;
    pGC->patOrg.y = 0;
    pGC->subWindowMode = ClipByChildren;
    pGC->graphicsExposures = TRUE;
    pGC->clipOrg.x = 0;
    pGC->clipOrg.y = 0;
    pGC->clientClipType = CT_NONE;
    pGC->dashOffset = 0;
    pGC->numInDashList = 2;
    pGC->dash = DefaultDash;
    pGC->lastWinOrg.x = 0;
    pGC->lastWinOrg.y = 0;

    pGC->stateChanges = (1 << GCLastBit+1) - 1;
    if (!(*pScreen->CreateGC)(pGC))
    {
	FreeGC(pGC, (GContext)0);
	pGC = (GCPtr)NULL;
    }
    return pGC;
}


FreeGCperDepth(screenNum)
    int screenNum;
{
    register int i;
    register ScreenPtr pScreen;
    GCPtr *ppGC;

    pScreen = screenInfo.screens[screenNum];
    ppGC = pScreen->GCperDepth;

    for (i = 0; i <= pScreen->numDepths; i++)
	(void)FreeGC(ppGC[i], (GContext)0);
    pScreen->rgf = ~0L;
}


Bool
CreateGCperDepth(screenNum)
    int screenNum;
{
    register int i;
    register ScreenPtr pScreen;
    DepthPtr pDepth;
    GCPtr *ppGC;

    pScreen = screenInfo.screens[screenNum];
    pScreen->rgf = 0;
    ppGC = pScreen->GCperDepth;
    /* do depth 1 separately because it's not included in list */
    if (!(ppGC[0] = CreateScratchGC(pScreen, 1)))
	return FALSE;
    ppGC[0]->graphicsExposures = FALSE;

    pDepth = pScreen->allowedDepths;
    for (i=0; i<pScreen->numDepths; i++, pDepth++)
    {
	if (!(ppGC[i+1] = CreateScratchGC(pScreen, pDepth->depth)))
	{
	    for (; i >= 0; i--)
		(void)FreeGC(ppGC[i], (GContext)0);
	    return FALSE;
	}
	ppGC[i+1]->graphicsExposures = FALSE;
    }
    return TRUE;
}

Bool
CreateDefaultStipple(screenNum)
    int screenNum;
{
    register ScreenPtr pScreen;
    XID tmpval[3];
    xRectangle rect;
    short w, h;
    GCPtr pgcScratch;

    pScreen = screenInfo.screens[screenNum];

    w = 16;
    h = 16;
    (* pScreen->QueryBestSize)(StippleShape, &w, &h, pScreen);
    if (!(pScreen->PixmapPerDepth[0] =
			(*pScreen->CreatePixmap)(pScreen, w, h, 1)))
	return FALSE;
    /* fill stipple with 1 */
    tmpval[0] = GXcopy; tmpval[1] = 1; tmpval[2] = FillSolid;
    pgcScratch = GetScratchGC(1, pScreen);
    if (!pgcScratch)
    {
	(*pScreen->DestroyPixmap)(pScreen->PixmapPerDepth[0]);
	return FALSE;
    }
    (void)ChangeGC(pgcScratch, GCFunction|GCForeground|GCFillStyle, tmpval);
    ValidateGC((DrawablePtr)pScreen->PixmapPerDepth[0], pgcScratch);
    rect.x = 0;
    rect.y = 0;
    rect.width = w;
    rect.height = h;
    (*pgcScratch->ops->PolyFillRect)(pScreen->PixmapPerDepth[0], 
				pgcScratch, 1, &rect);
    FreeScratchGC(pgcScratch);
    return TRUE;
}

FreeDefaultStipple(screenNum)
    int screenNum;
{
    ScreenPtr pScreen = screenInfo.screens[screenNum];
    (*pScreen->DestroyPixmap)(pScreen->PixmapPerDepth[0]);
}


SetDashes(pGC, offset, ndash, pdash)
register GCPtr pGC;
unsigned offset;
register unsigned ndash;
register unsigned char *pdash;
{
    register long i;
    register unsigned char *p, *indash;
    BITS32 maskQ = 0;

    i = ndash;
    p = pdash;
    while (i--)
    {
	if (!*p++)
	{
	    /* dash segment must be > 0 */
	    clientErrorValue = 0;
	    return BadValue;
	}
    }

    if (ndash & 1)
	p = (unsigned char *)xalloc(2 * ndash * sizeof(unsigned char));
    else
	p = (unsigned char *)xalloc(ndash * sizeof(unsigned char));
    if (!p)
	return BadAlloc;

    pGC->serialNumber |= GC_CHANGE_SERIAL_BIT;
    if (offset != pGC->dashOffset)
    {
	pGC->dashOffset = offset;
	pGC->stateChanges |= GCDashOffset;
	maskQ |= GCDashOffset;
    }

    if (pGC->dash != DefaultDash)
	xfree(pGC->dash);
    pGC->numInDashList = ndash;
    pGC->dash = p;
    if (ndash & 1)
    {
	pGC->numInDashList += ndash;
	indash = pdash;
	i = ndash;
	while (i--)
	    *p++ = *indash++;
    }
    while(ndash--)
	*p++ = *pdash++;
    pGC->stateChanges |= GCDashList;
    maskQ |= GCDashList;

    if (pGC->funcs->ChangeGC)
	(*pGC->funcs->ChangeGC) (pGC, maskQ);
    return Success;
}

int
VerifyRectOrder(nrects, prects, ordering)
    int			nrects;
    xRectangle		*prects;
    int			ordering;
{
    register xRectangle	*prectP, *prectN;
    register int	i;

    switch(ordering)
    {
      case Unsorted:
	  return CT_UNSORTED;
      case YSorted:
	  if(nrects > 1)
	  {
	      for(i = 1, prectP = prects, prectN = prects + 1;
		  i < nrects;
		  i++, prectP++, prectN++)
		  if(prectN->y < prectP->y)
		      return -1;
	  }
	  return CT_YSORTED;
      case YXSorted:
	  if(nrects > 1)
	  {
	      for(i = 1, prectP = prects, prectN = prects + 1;
		  i < nrects;
		  i++, prectP++, prectN++)
		  if((prectN->y < prectP->y) ||
		      ( (prectN->y == prectP->y) &&
		        (prectN->x < prectP->x) ) )
		      return -1;
	  }
	  return CT_YXSORTED;
      case YXBanded:
	  if(nrects > 1)
	  {
	      for(i = 1, prectP = prects, prectN = prects + 1;
		  i < nrects;
		  i++, prectP++, prectN++)
		  if((prectN->y != prectP->y &&
 		      prectN->y < prectP->y + (int) prectP->height) ||
		     ((prectN->y == prectP->y) &&
		      (prectN->height != prectP->height ||
		       prectN->x < prectP->x + (int) prectP->width)))
		      return -1;
	  }
	  return CT_YXBANDED;
    }
    return -1;
}

int
SetClipRects(pGC, xOrigin, yOrigin, nrects, prects, ordering)
    GCPtr		pGC;
    int			xOrigin, yOrigin;
    int			nrects;
    xRectangle		*prects;
    int			ordering;
{
    int			newct, size;
    xRectangle 		*prectsNew;

    newct = VerifyRectOrder(nrects, prects, ordering);
    if (newct < 0)
	return(BadMatch);
    size = nrects * sizeof(xRectangle);
    prectsNew = (xRectangle *) xalloc(size);
    if (!prects && size)
	return BadAlloc;

    pGC->serialNumber |= GC_CHANGE_SERIAL_BIT;
    pGC->clipOrg.x = xOrigin;
    pGC->stateChanges |= GCClipXOrigin;
		 
    pGC->clipOrg.y = yOrigin;
    pGC->stateChanges |= GCClipYOrigin;

    if (size)
	bcopy((char *)prects, (char *)prectsNew, size);
    (*pGC->funcs->ChangeClip)(pGC, newct, prectsNew, nrects);
    if (pGC->funcs->ChangeGC)
	(*pGC->funcs->ChangeGC) (pGC, GCClipXOrigin|GCClipYOrigin|GCClipMask);
    return Success;
}


/*
   sets reasonable defaults 
   if we can get a pre-allocated one, use it and mark it as used.
   if we can't, create one out of whole cloth (The Velveteen GC -- if
   you use it often enough it will become real.)
*/
GCPtr
GetScratchGC(depth, pScreen)
    register unsigned depth;
    register ScreenPtr pScreen;
{
    register int i;
    register GCPtr pGC;

    for (i=0; i<=pScreen->numDepths; i++)
        if ( pScreen->GCperDepth[i]->depth == depth &&
	     !(pScreen->rgf & (1L << (i+1)))
	   )
	{
	    pScreen->rgf |= (1L << (i+1));
            pGC = (pScreen->GCperDepth[i]);

	    pGC->alu = GXcopy;
	    pGC->planemask = ~0;
	    pGC->serialNumber = 0;
	    pGC->fgPixel = 0;
	    pGC->bgPixel = 1;
	    pGC->lineWidth = 0;
	    pGC->lineStyle = LineSolid;
	    pGC->capStyle = CapButt;
	    pGC->joinStyle = JoinMiter;
	    pGC->fillStyle = FillSolid;
	    pGC->fillRule = EvenOddRule;
	    pGC->arcMode = ArcChord;
	    pGC->patOrg.x = 0;
	    pGC->patOrg.y = 0;
	    pGC->subWindowMode = ClipByChildren;
	    pGC->graphicsExposures = FALSE;
	    pGC->clipOrg.x = 0;
	    pGC->clipOrg.y = 0;
	    /* can't change clip type, because we might drop storage */
	    pGC->stateChanges = (1 << GCLastBit+1) - 1;
	    return pGC;
	}
    /* if we make it this far, need to roll our own */
    pGC = CreateScratchGC(pScreen, depth);
    if (pGC)
	pGC->graphicsExposures = FALSE;
    return pGC;
}

/*
   if the gc to free is in the table of pre-existing ones,
mark it as available.
   if not, free it for real
*/
void
FreeScratchGC(pGC)
    register GCPtr pGC;
{
    register ScreenPtr pScreen = pGC->pScreen;
    register int i;

    for (i=0; i<=pScreen->numDepths; i++)
    {
        if ( pScreen->GCperDepth[i] == pGC)
	{
	    pScreen->rgf &= ~(1L << (i+1));
	    return;
	}
    }
    (void)FreeGC(pGC, (GContext)0);
}
