/* $XConsortium: FSOpenFont.c,v 1.3 91/07/16 20:32:14 keith Exp $ */

/* @(#)FSOpenFont.c	4.1	91/05/02
 * Copyright 1990 Network Computing Devices;
 * Portions Copyright 1987 by Digital Equipment Corporation and the
 * Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this protoype software
 * and its documentation to Members and Affiliates of the MIT X Consortium
 * any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of Network Computing Devices, Digital or
 * MIT not be used in advertising or publicity pertaining to distribution of
 * the software without specific, written prior permission.
 *
 * NETWORK COMPUTING DEVICES, DIGITAL AND MIT DISCLAIM ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS, IN NO EVENT SHALL NETWORK COMPUTING DEVICES, DIGITAL OR MIT BE
 * LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include	"FSlibint.h"

Font
FSOpenBitmapFont(svr, hint, fmask, name, otherid)
    FSServer   *svr;
    fsBitmapFormat hint;
    fsBitmapFormatMask fmask;
    char       *name;
    Font       *otherid;
{
    unsigned char nbytes;
    fsOpenBitmapFontReq *req;
    fsOpenBitmapFontReply reply;
    Font        fid;
    char        buf[256];

    GetReq(OpenBitmapFont, req);
    nbytes = name ? strlen(name) : 0;
    buf[0] = (char) nbytes;
    bcopy(name, &buf[1], nbytes);
    nbytes++;
    req->fid = fid = svr->resource_id++;
    req->format_hint = hint;
    req->format_mask = fmask;
    req->length += (nbytes + 3) >> 2;
    _FSSend(svr, buf, (long) nbytes);
    (void) _FSReply(svr, (fsReply *) & reply,
     (sizeof(fsOpenBitmapFontReply) - sizeof(fsGenericReply)) >> 2, fsFalse);
    *otherid = reply.otherid;
    SyncHandle();
    return fid;
}
