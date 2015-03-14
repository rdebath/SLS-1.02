/* $XConsortium: FSListExt.c,v 1.2 91/05/13 15:11:42 gildea Exp $ */

/* @(#)FSListExt.c	4.1	91/05/02
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

char      **
FSListExtensions(svr, next)
    FSServer     *svr;
    int        *next;
{
    fsListExtensionsReply rep;
    char      **list;
    char       *c;
    int         i,
                length;
    fsReq      *req;
    long        rlen;

    GetEmptyReq(ListExtensions, req);

    if (!_FSReply(svr, (fsReply *) & rep, 0, fsFalse)) {
	SyncHandle();
	return (char **) NULL;
    }
    if (rep.nExtensions) {
	list = (char **) FSmalloc((unsigned)(rep.nExtensions * sizeof(char *)));
	rlen = rep.length << 2;
	c = (char *) FSmalloc((unsigned) rlen + 1);
	if ((!list) || (!c)) {
	    if (list)
		FSfree((char *) list);
	    if (c)
		FSfree(c);
	    _FSEatData(svr, (unsigned long) rlen);
	    SyncHandle();
	    return (char **) NULL;
	}
	_FSReadPad(svr, c, rlen);
	/*
	 * unpack the strings
	 */
	length = *c;
	for (i = 0; i < rep.nExtensions; i++) {
	    list[i] = c + 1;	/* skip length */
	    c += length + 1;	/* find next length */
	    length = *c;
	    *c = '\0';		/* change length to NULL */
	}
    } else {
	list = (char **) NULL;
    }
    SyncHandle();
    *next = rep.nExtensions;
    return list;

}

FSFreeExtensionList(list)
    char      **list;
{
    if (list != NULL) {
	FSfree(list[0] - 1);
	FSfree((char *) list);
    }
}
