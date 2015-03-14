/* $XConsortium: mi.h,v 1.8 92/05/17 10:33:25 rws Exp $ */
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
#ifndef MI_H
#define MI_H
#include "region.h"

typedef struct _miDash *miDashPtr;
#define EVEN_DASH	0
#define ODD_DASH	~0

extern void  miPutImage();
extern void  miGetImage();
extern RegionPtr  miCopyArea();
extern RegionPtr  miCopyPlane();
extern void  miClearToBackground();
extern int   miValidateTree();
extern void  miPolySegment();
extern void  miPolyRectangle();
extern void  miFillPolygon();
extern int   miPolyText8();
extern int   miPolyText16();
extern void  miImageText8();
extern void  miImageText16();
extern int   miFillConvexPoly();
extern int   miFillGeneralPoly();
extern void miNotMiter();
extern void miMiter();
extern void miWideLine();
extern void miWideDash();
extern void  miPolyArc();
extern void  miZeroPolyArc();
extern void miPolyFillRect();
extern void miPolyFillArc();
extern void  miPolyGlyphBlt();
extern void  miImageGlyphBlt();
extern void  miZeroLine();
extern void  miPaintWindow();
extern miDashPtr   miDashLine();
extern void  miPushPixels();
extern int   miPtToAngle();
extern RegionPtr miRegionCreate();
extern void miRegionInit();
extern Bool miRegionCopy();
extern void miRegionDestroy();
extern void miRegionUninit();
extern Bool miIntersect();
extern Bool miInverse();
extern Bool miUnion();
extern int miSubtract();
extern void miRegionReset();
extern void miTranslateRegion();
extern int miRectIn();
extern Bool miRegionAppend();
extern Bool miRegionValidate();
extern RegionPtr miRectsToRegion();
extern Bool miPointInRegion();
extern Bool miRegionNotEmpty();
extern void miRegionEmpty();
extern BoxPtr miRegionExtents();
extern Bool miRectAlloc();
#ifdef DEBUG
extern Bool miValidRegion();
#endif
extern void miSendExposures();
extern void miWindowExposures();
extern void miSendGraphicsExpose();
extern RegionPtr miHandleExposures();

#endif /* MI_H */
