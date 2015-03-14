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

********************************************************/

/* THIS IS NOT AN X CONSORTIUM STANDARD */

/* $XConsortium: shmstr.h,v 1.6 91/07/12 09:18:52 rws Exp $ */

#include "XShm.h"

#define SHMNAME "MIT-SHM"

#define SHM_MAJOR_VERSION	1	/* current version numbers */
#define SHM_MINOR_VERSION	1

#ifdef _XSHM_SERVER_
typedef struct _ShmFuncs {
    PixmapPtr	(* CreatePixmap)();
    void	(* PutImage)();
} ShmFuncs, *ShmFuncsPtr;
#endif

typedef struct _ShmQueryVersion {
    CARD8	reqType;		/* always ShmReqCode */
    CARD8	shmReqType;		/* always X_ShmQueryVersion */
    CARD16	length B16;
} xShmQueryVersionReq;
#define sz_xShmQueryVersionReq	4

typedef struct {
    BYTE	type;			/* X_Reply */
    BOOL	sharedPixmaps;
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    CARD16	majorVersion B16;	/* major version of SHM protocol */
    CARD16	minorVersion B16;	/* minor version of SHM protocol */
    CARD16	uid B16;
    CARD16	gid B16;
    CARD8	pixmapFormat;
    CARD8	pad0;
    CARD16	pad1 B16;
    CARD32	pad2 B32;
    CARD32	pad3 B32;
    CARD32	pad4 B32;
} xShmQueryVersionReply;
#define sz_xShmQueryVersionReply	32

typedef struct _ShmAttach {
    CARD8	reqType;	/* always ShmReqCode */
    CARD8	shmReqType;	/* always X_ShmAttach */
    CARD16	length B16;
    ShmSeg	shmseg;
    CARD32	shmid;
    BOOL	readOnly;
    BYTE	pad0;
    CARD16	pad1;
} xShmAttachReq;
#define sz_xShmAttachReq	16

typedef struct _ShmDetach {
    CARD8	reqType;	/* always ShmReqCode */
    CARD8	shmReqType;	/* always X_ShmDetach */
    CARD16	length B16;
    ShmSeg	shmseg;
} xShmDetachReq;
#define sz_xShmDetachReq	8

typedef struct _ShmPutImage {
    CARD8	reqType;	/* always ShmReqCode */
    CARD8	shmReqType;	/* always X_ShmPutImage */
    CARD16	length B16;
    Drawable	drawable B32;
    GContext	gc B32;
    CARD16	totalWidth B16;
    CARD16	totalHeight B16;
    CARD16	srcX B16;
    CARD16	srcY B16;
    CARD16	srcWidth B16;
    CARD16	srcHeight B16;
    INT16	dstX B16;
    INT16	dstY B16;
    CARD8	depth;
    CARD8	format;
    CARD8	sendEvent;
    CARD8	bpad;
    ShmSeg	shmseg B32;
    CARD32	offset B32;
} xShmPutImageReq;    
#define sz_xShmPutImageReq	40

typedef struct _ShmGetImage {
    CARD8	reqType;	/* always ShmReqCode */
    CARD8	shmReqType;	/* always X_ShmGetImage */
    CARD16	length B16;
    Drawable	drawable B32;
    INT16	x B16;
    INT16	y B16;
    CARD16	width B16;
    CARD16	height B16;
    CARD32	planeMask B32;
    CARD8	format;
    CARD8	pad0;
    CARD8	pad1;
    CARD8	pad2;
    ShmSeg	shmseg B32;
    CARD32	offset B32;
} xShmGetImageReq;    
#define sz_xShmGetImageReq	32

typedef struct _ShmGetImageReply {
    BYTE	type;  /* X_Reply */
    CARD8	depth;
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    VisualID	visual B32;
    CARD32	size B32;
    CARD32	pad0 B32;
    CARD32	pad1 B32;
    CARD32	pad2 B32;
    CARD32	pad3 B32;
} xShmGetImageReply;
#define sz_xShmGetImageReply	32

typedef struct _ShmCreatePixmap {
    CARD8	reqType;	/* always ShmReqCode */
    CARD8	shmReqType;	/* always X_ShmCreatePixmap */
    CARD16	length B16;
    Pixmap	pid B32;
    Drawable	drawable B32;
    CARD16	width B16;
    CARD16	height B16;
    CARD8	depth;
    CARD8	pad0;
    CARD8	pad1;
    CARD8	pad2;
    ShmSeg	shmseg B32;
    CARD32	offset B32;
} xShmCreatePixmapReq;
#define sz_xShmCreatePixmapReq 28

typedef struct _ShmCompletion {
    BYTE	type;		/* always eventBase + ShmCompletion */
    BYTE	bpad0;
    CARD16	sequenceNumber B16;
    Drawable	drawable B32;
    CARD16	minorEvent B16;
    BYTE	majorEvent;
    BYTE	bpad1;
    ShmSeg	shmseg B32;
    CARD32	offset B32;
    CARD32	pad0 B32;
    CARD32	pad1 B32;
    CARD32	pad2 B32;
} xShmCompletionEvent;
#define sz_xShmCompletionEvent	32
