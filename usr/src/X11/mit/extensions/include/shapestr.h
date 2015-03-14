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
NESS FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL SUN BE  LI-
ABLE  FOR  ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,  DATA  OR
PROFITS,  WHETHER  IN  AN  ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION  WITH
THE USE OR PERFORMANCE OF THIS SOFTWARE.

********************************************************/

/* $XConsortium: shapestr.h,v 1.9 90/06/07 10:08:36 rws Exp $ */

/*
 * Protocol requests constants and alignment values
 * These would really be in SHAPE's X.h and Xproto.h equivalents
 */

#include "shape.h"

#define SHAPENAME "SHAPE"

#define SHAPE_MAJOR_VERSION	1	/* current version numbers */
#define SHAPE_MINOR_VERSION	0

typedef struct _ShapeQueryVersion {
	CARD8	reqType;		/* always ShapeReqCode */
	CARD8	shapeReqType;		/* always X_ShapeQueryVersion */
	CARD16	length B16;
} xShapeQueryVersionReq;
#define sz_xShapeQueryVersionReq	4

typedef struct {
	BYTE	type;			/* X_Reply */
	CARD8	unused;			/* not used */
	CARD16	sequenceNumber B16;
	CARD32	length B32;
	CARD16	majorVersion B16;	/* major version of SHAPE protocol */
	CARD16	minorVersion B16;	/* minor version of SHAPE protocol */
	CARD32	pad0 B32;
	CARD32	pad1 B32;
	CARD32	pad2 B32;
	CARD32	pad3 B32;
	CARD32	pad4 B32;
} xShapeQueryVersionReply;
#define sz_xShapeQueryVersionReply	32

typedef struct _ShapeRectangles {
	CARD8	reqType;	/* always ShapeReqCode */
	CARD8	shapeReqType;	/* always X_ShapeRectangles */
	CARD16	length B16;
	CARD8	op;		/* Set, ... */
	CARD8	destKind;	/* ShapeBounding or ShapeClip */
	CARD8	ordering;	/* UnSorted, YSorted, YXSorted, YXBanded */
	CARD8	pad0;		/* not used */
	CARD32	dest B32;	/* Window */
	INT16	xOff B16;
	INT16	yOff B16;
} xShapeRectanglesReq;		/* followed by xRects */
#define sz_xShapeRectanglesReq	16

typedef struct _ShapeMask {
	CARD8	reqType;	/* always ShapeReqCode */
	CARD8	shapeReqType;	/* always X_ShapeMask */
	CARD16	length B16;

	CARD8	op;		/* Set, ... */
	CARD8	destKind;	/* ShapeBounding or ShapeClip */
	CARD16	junk B16;	/* not used */

	CARD32	dest B32;	/* Window */
	INT16	xOff B16;
	INT16	yOff B16;
	CARD32	src B32;	/* 1 bit pixmap */
} xShapeMaskReq;
#define sz_xShapeMaskReq	20
	
typedef struct _ShapeCombine {
	CARD8	reqType;	/* always ShapeReqCode */
	CARD8	shapeReqType;	/* always X_ShapeCombine */
	CARD16	length B16;
	CARD8	op;		/* Set, ... */
	CARD8	destKind;	/* ShapeBounding or ShapeClip */
	CARD8	srcKind;	/* ShapeBounding or ShapeClip */
	CARD8	junk;		/* not used */
	CARD32	dest B32;	/* Window */
	INT16	xOff B16;
	INT16	yOff B16;
	CARD32	src B32;	/* Window */
} xShapeCombineReq;
#define sz_xShapeCombineReq	20
	
typedef struct _ShapeOffset {
	CARD8	reqType;	/* always ShapeReqCode */
	CARD8	shapeReqType;	/* always X_ShapeOffset */
	CARD16	length B16;
	CARD8	destKind;	/* ShapeBounding or ShapeClip */
	CARD8	junk1;		/* not used */
	CARD16	junk2 B16;	/* not used */
	CARD32	dest B32;	/* Window */
	INT16	xOff B16;
	INT16	yOff B16;
} xShapeOffsetReq;
#define sz_xShapeOffsetReq	16

typedef struct _ShapeQueryExtents {
	CARD8	reqType;	/* always ShapeReqCode */
	CARD8	shapeReqType;	/* always X_ShapeQueryExtents */
	CARD16	length B16;
	CARD32	window B32;		/* request destination id */
} xShapeQueryExtentsReq;
#define sz_xShapeQueryExtentsReq	8

typedef struct {
	BYTE	type;			/* X_Reply */
	CARD8	unused;			/* not used */
	CARD16	sequenceNumber B16;
	CARD32	length B32;		/* 0 */
	CARD8	boundingShaped;		/* window has bounding shape */
	CARD8	clipShaped;		/* window has clip shape */
	CARD16	unused1 B16;
	INT16	xBoundingShape B16;	/* extents of bounding shape */
	INT16	yBoundingShape B16;
	CARD16	widthBoundingShape B16;
	CARD16	heightBoundingShape B16;
	INT16	xClipShape B16;		/* extents of clip shape */
	INT16	yClipShape B16;
	CARD16	widthClipShape B16;
	CARD16	heightClipShape B16;
	CARD32	pad1 B32;
} xShapeQueryExtentsReply;
#define sz_xShapeQueryExtentsReply	32

typedef struct _ShapeSelectInput {
	CARD8	reqType;	/* always ShapeReqCode */
	CARD8	shapeReqType;	/* always X_ShapeSelectInput */
	CARD16	length B16;
	CARD32	window B32;	 /* request destination id */
	BYTE	enable;		/* xTrue -> send events */
	BYTE	pad1;
	CARD16	pad2 B16;
} xShapeSelectInputReq;
#define sz_xShapeSelectInputReq	12

typedef struct _ShapeNotify {
	BYTE	type;		/* always eventBase + ShapeNotify */
	BYTE	kind;		/* either ShapeBounding or ShapeClip */
	CARD16	sequenceNumber B16;
	Window	window B32;
	INT16	x B16;
	INT16	y B16;		/* extents of new shape */
	CARD16	width B16;
	CARD16	height B16;
	Time	time B32;	/* time of change */
	BYTE	shaped;		/* set when a shape actual exists */
	BYTE	pad0;
	CARD16	pad1 B16;
	CARD32	pad2 B32;
	CARD32	pad3 B32;
} xShapeNotifyEvent;
#define sz_xShapeNotifyEvent	32

typedef struct _ShapeInputSelected {
	CARD8	reqType;	/* always ShapeReqCode */
	CARD8	shapeReqType;	/* always X_ShapeInputSelected */
	CARD16	length B16;
	CARD32	window B32;	/* request destination id */
} xShapeInputSelectedReq;
#define sz_xShapeInputSelectedReq 8

typedef struct {
	BYTE	type;			/* X_Reply */
	CARD8	enabled;		/* current status */
	CARD16	sequenceNumber B16;
	CARD32	length B32;		/* 0 */
	CARD32	pad1 B32;		/* unused */
	CARD32	pad2 B32;
	CARD32	pad3 B32;
	CARD32	pad4 B32;
	CARD32	pad5 B32;
	CARD32	pad6 B32;
} xShapeInputSelectedReply;
#define sz_xShapeInputSelectedReply	32

typedef struct _ShapeGetRectangles {
    CARD8   reqType;		/* always ShapeReqCode */
    CARD8   shapeReqType;	/* always X_ShapeGetRectangles */
    CARD16  length B16;
    CARD32  window B32;		/* request destination id */
    CARD8   kind;		/* ShapeBounding or ShapeClip */
    CARD8   junk1;
    CARD16  junk2 B16;
} xShapeGetRectanglesReq;
#define sz_xShapeGetRectanglesReq	12

typedef struct {
	BYTE	type;			/* X_Reply */
	CARD8	ordering;	/* UnSorted, YSorted, YXSorted, YXBanded */
	CARD16	sequenceNumber B16;
	CARD32	length B32;		/* not zero */
	CARD32	nrects B32;		/* number of rectangles */
	CARD32 pad1 B32;
	CARD32 pad2 B32;
	CARD32 pad3 B32;
	CARD32 pad4 B32;
	CARD32 pad5 B32;
} xShapeGetRectanglesReply;		/* followed by xRectangles */
#define sz_xShapeGetRectanglesReply 32
