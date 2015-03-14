/* $XConsortium: FSFontInfo.c,v 1.4 92/05/13 15:41:58 gildea Exp $ */
/*
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
FSListFontsWithXInfo(svr, pattern, maxNames, count, info, pprops, offsets, prop_data)
    FSServer   *svr;
    char       *pattern;
    int         maxNames;
    int        *count;
    fsFontHeader ***info;
    fsPropInfo ***pprops;
    fsPropOffset ***offsets;
    unsigned char ***prop_data;
{
    long        nbytes;
    int         i,
                j;
    int         size = 0;
    fsFontHeader **fhdr = (fsFontHeader **) 0;
    fsPropInfo **pi = (fsPropInfo **) 0;
    fsPropOffset **po = (fsPropOffset **) 0;
    unsigned char **pd = (unsigned char **) 0;
    char      **flist = NULL;
    fsListFontsWithXInfoReply reply;
    fsListFontsWithXInfoReq *req;
    Status status;

    GetReq(ListFontsWithXInfo, req);
    req->maxNames = maxNames;
    nbytes = req->nbytes = pattern ? strlen(pattern) : 0;
    req->length += (nbytes + 3) >> 2;
    _FSSend(svr, pattern, nbytes);

    for (i = 0;; i++) {
	if (FSProtocolVersion(svr) > 1)
	{
	    status = _FSReply(svr, (fsReply *) &reply, 0, fsFalse);
	    if (status != 0  &&  reply.nameLength == 0)	/* got last reply */
		break;
	    if (status)
		_FSRead(svr, (char *) &reply.nReplies,
			SIZEOF(fsListFontsWithXInfoReply) -
			SIZEOF(fsGenericReply));
	} else {
	    status = _FSReply(svr, (fsReply *) & reply,
			      ((SIZEOF(fsListFontsWithXInfoReply) -
				SIZEOF(fsGenericReply)) >> 2), fsFalse);
	}
	if (!status) {
	    for (j = (i - 1); j >= 0; j--) {
		FSfree((char *) fhdr[j]);
		FSfree((char *) pi[j]);
		FSfree((char *) po[j]);
		FSfree((char *) pd[j]);
		FSfree(flist[j]);
	    }
	    if (flist)
		FSfree((char *) flist);
	    if (fhdr)
		FSfree((char *) fhdr);
	    if (pi)
		FSfree((char *) pi);
	    if (po)
		FSfree((char *) po);
	    if (pd)
		FSfree((char *) pd);

	    SyncHandle();
	    return (char **) NULL;
	}
	if (reply.nameLength == 0)	/* got last reply in version 1 */
	    break;
	if ((i + reply.nReplies) >= size) {
	    size = i + reply.nReplies + 1;

	    if (fhdr) {
		fsFontHeader **tmp_fhdr = (fsFontHeader **)
		FSrealloc((char *) fhdr,
			  (unsigned) (sizeof(fsFontHeader *) * size));
		char      **tmp_flist = (char **) FSrealloc((char *) flist,
					 (unsigned) (sizeof(char *) * size));
		fsPropInfo **tmp_pi = (fsPropInfo **)
		FSrealloc((char *) pi,
			  (unsigned) (sizeof(fsPropInfo *) * size));
		fsPropOffset **tmp_po = (fsPropOffset **)
		FSrealloc((char *) po,
			  (unsigned) (sizeof(fsPropOffset *) * size));
		unsigned char **tmp_pd = (unsigned char **)
		FSrealloc((char *) pd,
			  (unsigned) (sizeof(unsigned char *) * size));

		if (!tmp_fhdr || !tmp_flist || !tmp_pi || !tmp_po || !tmp_pd) {
		    for (j = (i - 1); j >= 0; j--) {
			FSfree((char *) flist[j]);
			FSfree((char *) fhdr[j]);
			FSfree((char *) pi[j]);
			FSfree((char *) po[j]);
			FSfree((char *) pd[j]);
		    }
		    if (tmp_flist)
			FSfree((char *) tmp_flist);
		    else
			FSfree((char *) flist);
		    if (tmp_fhdr)
			FSfree((char *) tmp_fhdr);
		    else
			FSfree((char *) fhdr);
		    if (tmp_pi)
			FSfree((char *) tmp_pi);
		    else
			FSfree((char *) pi);
		    if (tmp_po)
			FSfree((char *) tmp_po);
		    else
			FSfree((char *) po);
		    if (tmp_pd)
			FSfree((char *) tmp_pd);
		    else
			FSfree((char *) pd);
		    goto clearwire;
		}
		fhdr = tmp_fhdr;
		flist = tmp_flist;
		pi = tmp_pi;
		po = tmp_po;
		pd = tmp_pd;
	    } else {
		if (!(fhdr = (fsFontHeader **)
		      FSmalloc((unsigned) (sizeof(fsFontHeader *) * size))))
		    goto clearwire;
		if (!(flist = (char **)
		      FSmalloc((unsigned) (sizeof(char *) * size)))) {
		    FSfree((char *) fhdr);
		    goto clearwire;
		}
		if (!(pi = (fsPropInfo **)
		      FSmalloc((unsigned) (sizeof(fsPropInfo *) * size)))) {
		    FSfree((char *) fhdr);
		    FSfree((char *) flist);
		    goto clearwire;
		}
		if (!(po = (fsPropOffset **)
		      FSmalloc((unsigned) (sizeof(fsPropOffset *) * size)))) {
		    FSfree((char *) fhdr);
		    FSfree((char *) flist);
		    FSfree((char *) pi);
		    goto clearwire;
		}
		if (!(pd = (unsigned char **)
		    FSmalloc((unsigned) (sizeof(unsigned char *) * size)))) {
		    FSfree((char *) fhdr);
		    FSfree((char *) flist);
		    FSfree((char *) pi);
		    FSfree((char *) po);
		    goto clearwire;
		}
	    }
	}
	fhdr[i] = (fsFontHeader *) FSmalloc(sizeof(fsFontHeader));
	if (!fhdr[i]) {
	    goto badmem;
	}
	bcopy((char *) &reply.header, (char *) fhdr[i], sizeof(fsFontHeader));
	if (FSProtocolVersion(svr) == 1)
	{
	    fhdr[i]->char_range.min_char.high = reply.header.char_range.min_char.low;
	    fhdr[i]->char_range.min_char.low = reply.header.char_range.min_char.high;
	    fhdr[i]->char_range.max_char.high = reply.header.char_range.max_char.low;
	    fhdr[i]->char_range.max_char.low = reply.header.char_range.max_char.high;
	    fhdr[i]->default_char.high = reply.header.default_char.low;
	    fhdr[i]->default_char.low = reply.header.default_char.high;
	}

	/* alloc space for the name */
	flist[i] = (char *) FSmalloc((unsigned int) (reply.nameLength + 1));
	if (FSProtocolVersion(svr) == 1)
	{
	    /* get the name */
	    if (!flist[i]) {
		nbytes = reply.nameLength + 3 & ~3;
		_FSEatData(svr, (unsigned long) nbytes);
		goto badmem;
	    }
	    _FSReadPad(svr, flist[i], (long) reply.nameLength);
	    flist[i][reply.nameLength] = '\0';
	}

	pi[i] = (fsPropInfo *) FSmalloc(sizeof(fsPropInfo));
	if (!pi[i]) {
	    FSfree((char *) fhdr[i]);
	    goto badmem;
	}
	_FSReadPad(svr, (char *) pi[i], sizeof(fsPropInfo));

	po[i] = (fsPropOffset *)
	    FSmalloc(pi[i]->num_offsets * sizeof(fsPropOffset));
	if (!po[i]) {
	    FSfree((char *) fhdr[i]);
	    FSfree((char *) pi[i]);
	    goto badmem;
	}
	pd[i] = (unsigned char *) FSmalloc(pi[i]->data_len);
	if (!pd[i]) {
	    FSfree((char *) fhdr[i]);
	    FSfree((char *) pi[i]);
	    FSfree((char *) po[i]);
	    goto badmem;
	}
	/* get offsets */
	_FSReadPad(svr, (char *) po[i],
		   (pi[i]->num_offsets * sizeof(fsPropOffset)));
	/* get prop data */
	if (FSProtocolVersion(svr) == 1)
	    _FSReadPad(svr, (char *) pd[i], pi[i]->data_len);
	else
	    _FSRead(svr, (char *) pd[i], pi[i]->data_len);

	if (FSProtocolVersion(svr) != 1)
	{
	    /* get the name */
	    if (!flist[i]) {
		nbytes = reply.nameLength + 3 & ~3;
		_FSEatData(svr, (unsigned long) nbytes);
		goto badmem;
	    }
	    _FSRead(svr, flist[i], (long) reply.nameLength);
	    flist[i][reply.nameLength] = '\0';

	    nbytes = pi[i]->data_len + reply.nameLength;
	    _FSEatData(svr, (unsigned long) (((nbytes+3)&~3) - nbytes));
	}
    }
    *info = fhdr;
    *count = i;
    *pprops = pi;
    *offsets = po;
    *prop_data = pd;
    SyncHandle();
    return flist;

badmem:
    for (j = (i - 1); j >= 0; j--) {
	FSfree((char *) pi[j]);
	FSfree((char *) po[j]);
	FSfree((char *) pd[j]);
	FSfree(flist[j]);
	FSfree((char *) fhdr[j]);
    }
    if (flist)
	FSfree((char *) flist);
    if (fhdr)
	FSfree((char *) fhdr);
    if (pi)
	FSfree((char *) pi);
    if (po)
	FSfree((char *) po);
    if (pd)
	FSfree((char *) pd);


clearwire:
    do {
	fsPropInfo  ti;

	_FSEatData(svr, (reply.nameLength + 3) & ~3);
	_FSReadPad(svr, (char *) &ti, sizeof(fsPropInfo));
	_FSEatData(svr, (sizeof(fsPropOffset) * ti.num_offsets));
	_FSEatData(svr, ti.data_len);
    } while (_FSReply(svr, (fsReply *) & reply,
		      ((SIZEOF(fsListFontsWithXInfoReply)
       - SIZEOF(fsGenericReply)) >> 2), fsFalse) && (reply.nameLength != 0));
    SyncHandle();
    return (char **) NULL;
}
