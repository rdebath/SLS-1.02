/* $XConsortium: FSQXInfo.c,v 1.3 92/05/12 18:07:24 gildea Exp $ */
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

/*
 * Note:  only the range in the first FSQuery is sent to the server.
 * the others exist as return values only.
 */

int
FSQueryXInfo(svr, fid, info, props, offsets, prop_data)
    FSServer   *svr;
    Font        fid;
    fsFontHeader *info;
    fsPropInfo *props;
    fsPropOffset **offsets;
    unsigned char **prop_data;
{
    fsQueryXInfoReq *req;
    fsQueryXInfoReply reply;
    fsPropOffset *offset_data;
    unsigned char *pdata;

    GetReq(QueryXInfo, req);
    req->id = fid;

    /* get back the info */
    if (!_FSReply(svr, (fsReply *) & reply, ((SIZEOF(fsQueryXInfoReply) -
			    SIZEOF(fsGenericReply)) >> 2), fsFalse)) {
	return FSBadAlloc;
    }
    bcopy((char *) &reply.header, (char *) info, sizeof(fsFontHeader));
    if (FSProtocolVersion(svr) == 1)
    {
	info->char_range.min_char.high = reply.header.char_range.min_char.low;
	info->char_range.min_char.low = reply.header.char_range.min_char.high;
	info->char_range.max_char.high = reply.header.char_range.max_char.low;
	info->char_range.max_char.low = reply.header.char_range.max_char.high;
	info->default_char.high = reply.header.default_char.low;
	info->default_char.low = reply.header.default_char.high;
    }
    /* get the prop header */
    _FSReadPad(svr, (char *) props, sizeof(fsPropInfo));
    /* prepare for prop data */
    offset_data = (fsPropOffset *)
	FSmalloc(props->num_offsets * sizeof(fsPropOffset));
    if (!offset_data)
	return FSBadAlloc;
    pdata = (unsigned char *) FSmalloc(props->data_len);
    if (!pdata) {
	FSfree((char *) offset_data);
	return FSBadAlloc;
    }
    /* get offsets */
    _FSReadPad(svr, (char *) offset_data,
	       (props->num_offsets * sizeof(fsPropOffset)));
    /* get data */
    _FSReadPad(svr, (char *) pdata, props->data_len);
    *offsets = offset_data;
    *prop_data = pdata;

    SyncHandle();
    return FSSuccess;
}
