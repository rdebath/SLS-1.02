/* $XConsortium: FSFtNames.c,v 1.2 91/05/13 15:11:40 gildea Exp $ */

/* @(#)FSFtNames.c	4.1	91/05/02
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
FSListFonts(svr, pattern, maxNames, actualCount)
    FSServer   *svr;
    char       *pattern;
    int         maxNames;
    int        *actualCount;
{
    long        nbytes;
    int         i,
                length;
    char      **flist;
    char       *c;
    fsListFontsReply rep;
    fsListFontsReq *req;
    long        rlen;

    GetReq(ListFonts, req);
    req->maxNames = maxNames;
    nbytes = req->nbytes = pattern ? strlen(pattern) : 0;
    req->length += (nbytes + 3) >> 2;
    _FSSend(svr, pattern, nbytes);
    if (!_FSReply(svr, (fsReply *) & rep,
	  (sizeof(fsListFontsReply) - sizeof(fsGenericReply)) >> 2, fsFalse))
	return (char **) 0;

    if (rep.nFonts) {
	flist = (char **) FSmalloc((unsigned) rep.nFonts * sizeof(char *));
	rlen = (rep.length << 2) - sizeof(fsListFontsReply);
	c = (char *) FSmalloc((unsigned) (rlen + 1));

	if ((!flist) || (!c)) {
	    if (flist)
		FSfree((char *) flist);
	    if (c)
		FSFree(c);
	    _FSEatData(svr, (unsigned long) rlen);
	    SyncHandle();
	    return (char **) NULL;
	}
	_FSReadPad(svr, c, rlen);
	/* unpack */
	length = *c;
	for (i = 0; i < rep.nFonts; i++) {
	    flist[i] = c + 1;
	    c += length + 1;
	    length = *c;
	    *c = '\0';
	}
    } else {

	flist = (char **) NULL;
    }

    *actualCount = rep.nFonts;
    SyncHandle();
    return flist;

}

FSFreeFontNames(list)
    char      **list;
{
    if (list) {
	FSFree(list[0] - 1);
	FSFree((char *) list);
    }
}
