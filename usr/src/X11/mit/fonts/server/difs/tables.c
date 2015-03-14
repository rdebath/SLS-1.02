/* $XConsortium: tables.c,v 1.5 92/05/28 16:43:08 gildea Exp $ */
/*
 * all the dispatch, error, event and reply vectors
 */
/*
 * Copyright 1990, 1991 Network Computing Devices;
 * Portions Copyright 1987 by Digital Equipment Corporation and the
 * Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of Network Computing Devices, Digital or
 * M.I.T. not be used in advertising or publicity pertaining to distribution
 * of the software without specific, written prior permission.
 *
 * NETWORK COMPUTING DEVICES, DIGITAL AND M.I.T. DISCLAIM ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL NETWORK COMPUTING DEVICES,
 * DIGITAL OR M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
 * THIS SOFTWARE.
 */

#include "globals.h"

extern int  ProcInitialConnection(), ProcEstablishConnection();

extern int  ProcSetAuthorization(),
            ProcSetResolution(), ProcGetResolution(), ProcNoop(),
            ProcListExtensions(), ProcQueryExtension(),
            ProcListFonts(), ProcListFontsWithXInfo(),
            ProcOpenBitmapFont(), ProcQueryXInfo(), ProcQueryXExtents(),
            ProcQueryXBitmaps(), ProcCloseFont(),
            ProcListCatalogues(), ProcSetCatalogues(), ProcGetCatalogues(),
            ProcSetEventMask(), ProcGetEventMask(),
            ProcCreateAC(), ProcFreeAC();

extern int  SProcSimpleRequest(), SProcResourceRequest(),
	    SProcListCatalogues(),
            SProcSetResolution(),
            SProcQueryExtension(),
            SProcListFonts(), SProcListFontsWithXInfo(),
            SProcOpenBitmapFont(), SProcQueryXExtents(),
            SProcQueryXBitmaps(),
            SProcCreateAC();

extern void SErrorEvent();

extern void NotImplemented(), SGenericReply(),
            SListExtensionsReply(),
	    SListCataloguesReply(),
            SQueryExtensionReply(),
            SListFontsReply(), SListFontsWithXInfoReply(),
            SOpenBitmapFontReply(),
            SQueryXInfoReply(), SQueryXExtentsReply(),
            SQueryXBitmapsReply(),
            SGetEventMaskReply(), SCreateACReply(), SGetResolutionReply(),
            SOpenBitmapFontReply();


int         (*InitialVector[3]) () =
{
    0,
    ProcInitialConnection,
    ProcEstablishConnection
};

int         (*ProcVector[NUM_PROC_VECTORS]) () =
{
    ProcNoop,			/* 0 */
    ProcListExtensions,
    ProcQueryExtension,
    ProcListCatalogues,
    ProcSetCatalogues,
    ProcGetCatalogues,		/* 5 */
    ProcSetEventMask,
    ProcGetEventMask,
    ProcCreateAC,
    ProcFreeAC,
    ProcSetAuthorization,	/* 10 */
    ProcSetResolution,
    ProcGetResolution,
    ProcListFonts,
    ProcListFontsWithXInfo,
    ProcOpenBitmapFont,		/* 15 */
    ProcQueryXInfo,
    ProcQueryXExtents,
    ProcQueryXExtents,
    ProcQueryXBitmaps,
    ProcQueryXBitmaps,		/* 20 */
    ProcCloseFont,
    0,
    0,
    0
};

int         (*SwappedProcVector[NUM_PROC_VECTORS]) () =
{
    SProcSimpleRequest,		/* 0 */
    SProcSimpleRequest,
    SProcQueryExtension,
    SProcListCatalogues,
    SProcSimpleRequest,		/* SetCatalogues */
    SProcSimpleRequest,		/* 5 */
    SProcResourceRequest,	/* SetEventMask */
    SProcSimpleRequest,
    SProcCreateAC,
    SProcResourceRequest,
    SProcResourceRequest,	/* 10 */
    SProcSetResolution,
    SProcSimpleRequest,
    SProcListFonts,
    SProcListFontsWithXInfo,
    SProcOpenBitmapFont,	/* 15 */
    SProcResourceRequest,
    SProcQueryXExtents,
    SProcQueryXExtents,
    SProcQueryXBitmaps,
    SProcQueryXBitmaps,		/* 20 */
    SProcResourceRequest,
    0,
    0,
    0
};

void        (*EventSwapVector[NUM_EVENT_VECTORS]) () =
{
    SErrorEvent,
    NotImplemented,
    0,
    0,
    0,
    0,
    0,
    0
};

void        (*ReplySwapVector[NUM_PROC_VECTORS]) () =
{
    NotImplemented,		/* NoOp */
    SListExtensionsReply,
    SQueryExtensionReply,	/* SQueryExtensionReply */
    SListCataloguesReply,
    NotImplemented,		/* SetCatalogues */
    SGenericReply,		/* GetCatalogues */
    NotImplemented,		/* SetEventMask */
    SGetEventMaskReply,
    SCreateACReply,
    NotImplemented,		/* FreeAC */
    NotImplemented,		/* SetAuthorization - 10 */
    NotImplemented,		/* SetResolution */
    SGetResolutionReply,
    SListFontsReply,
    SListFontsWithXInfoReply,
    SOpenBitmapFontReply,	/* 15 */
    SQueryXInfoReply,
    SQueryXExtentsReply,
    SQueryXExtentsReply,
    SQueryXBitmapsReply,
    SQueryXBitmapsReply,	/* 20 */
    NotImplemented,		/* Close */
    NotImplemented,
    NotImplemented
};
