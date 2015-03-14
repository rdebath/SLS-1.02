/* $XConsortium: FSListCats.c,v 1.1 91/07/16 20:32:03 keith Exp $ */

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

char      **
FSListCatalogues(svr, pattern, maxNames, actualCount)
    FSServer   *svr;
    char       *pattern;
    int         maxNames;
    int        *actualCount;
{
    long        nbytes;
    int         i,
                length;
    char      **clist;
    char       *c;
    fsListCataloguesReply rep;
    fsListCataloguesReq *req;
    long        rlen;

    GetReq(ListCatalogues, req);
    req->maxNames = maxNames;
    nbytes = req->nbytes = pattern ? strlen(pattern) : 0;
    req->length += (nbytes + 3) >> 2;
    _FSSend(svr, pattern, nbytes);
    if (!_FSReply(svr, (fsReply *) & rep, 
    (sizeof(fsListCataloguesReply) - sizeof(fsGenericReply)) >> 2, fsFalse))
	return (char **) 0;

    if (rep.num_catalogues) {
	clist = (char **)
	    FSmalloc((unsigned) rep.num_catalogues * sizeof(char *));
	rlen = (rep.length << 2) - sizeof(fsListCataloguesReply);
	c = (char *) FSmalloc((unsigned) (rlen + 1));

	if ((!clist) || (!c)) {
	    if (clist)
		FSfree((char *) clist);
	    if (c)
		FSFree(c);
	    _FSEatData(svr, (unsigned long) rlen);
	    SyncHandle();
	    return (char **) NULL;
	}
	_FSReadPad(svr, c, rlen);
	/* unpack */
	length = *c;
	for (i = 0; i < rep.num_catalogues; i++) {
	    clist[i] = c + 1;
	    c += length + 1;
	    length = *c;
	    *c = '\0';
	}
    } else {

	clist = (char **) NULL;
    }

    *actualCount = rep.num_catalogues;
    SyncHandle();
    return clist;

}

FSFreeCatalogues(list)
    char      **list;
{
    if (list) {
	FSFree(list[0] - 1);
	FSFree((char *) list);
    }
}
