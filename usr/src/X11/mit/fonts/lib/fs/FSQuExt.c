/* $XConsortium: FSQuExt.c,v 1.2 91/05/13 15:11:53 gildea Exp $ */

/* @(#)FSQuExt.c	4.1	91/05/02
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

Bool
FSQueryExtension(svr, name, major_opcode, first_event, first_error)
    FSServer   *svr;
    char       *name;
    int        *major_opcode;
    int        *first_event;
    int        *first_error;
{
    fsQueryExtensionReply rep;
    fsQueryExtensionReq *req;

    GetReq(QueryExtension, req);
    req->nbytes = name ? strlen(name) : 0;
    req->length += (req->nbytes + 3) >> 2;
    _FSSend(svr, name, (long) req->nbytes);
    if (!_FSReply(svr, (fsReply *) & rep,
      (sizeof(fsQueryExtensionReply) - sizeof(fsGenericReply)) >> 2, fsFalse))
	return FSBadAlloc;
    *major_opcode = rep.major_opcode;
    *first_event = rep.first_event;
    *first_error = rep.first_error;
    SyncHandle();
    return (rep.present);
}
