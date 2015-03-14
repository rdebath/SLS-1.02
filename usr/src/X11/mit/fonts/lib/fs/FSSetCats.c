/* $XConsortium: FSSetCats.c,v 1.1 91/07/16 20:32:08 keith Exp $ */

/* @(#)FSFlush.c	4.1	91/05/02
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

int
FSSetCatalogues(svr, num, cats)
    FSServer   *svr;
    int         num;
    char      **cats;
{
    unsigned char nbytes;
    fsSetCataloguesReq *req;
    char        buf[256];
    int         i;
    int         len;

    for (i = 0, len = 0; i < num; i++) {
	len += strlen(cats[i]);
    }

    GetReq(SetCatalogues, req);
    req->num_catalogues = num;
    req->length += (len + 3) >> 2;

    for (i = 0; i < num; i++) {
	nbytes = strlen(cats[i]);
	buf[0] = (char) nbytes;
	bcopy(cats[i], &buf[1], nbytes);
	nbytes++;
	_FSSend(svr, buf, (long) nbytes);
    }
    SyncHandle();
    return FSSuccess;
}
