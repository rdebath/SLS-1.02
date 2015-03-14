/* $XConsortium: fontinfo.c,v 1.8 92/05/12 18:08:08 gildea Exp $ */
/*
 * font data query
 */
/*
 * Copyright 1990, 1991 Network Computing Devices;
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

#include        "FS.h"
#include        "FSproto.h"
#include        <stdio.h>
#include        <X11/Xos.h>
#include        "clientstr.h"
#include        "difsfontst.h"
#include        "fontstruct.h"
#include        "closestr.h"
#include        "globals.h"

extern void (*ReplySwapVector[NUM_PROC_VECTORS]) ();

void
CopyCharInfo(ci, dst)
    CharInfoPtr ci;
    fsCharInfo *dst;
{
    xCharInfo  *src = &ci->metrics;

    dst->ascent = src->ascent;
    dst->descent = src->descent;
    dst->left = src->leftSideBearing;
    dst->right = src->rightSideBearing;
    dst->width = src->characterWidth;
    dst->attributes = 0;
}


static int
convert_props(pinfo, props)
    FontInfoPtr pinfo;
    fsPropInfo **props;
{
    pointer     ptr;
    fsPropInfo  info;
    fsPropOffset *po,
               *offsets;
    FontPropPtr pp;
    int         i,
                data_len = 0;
    int         po_size,
                num_props;

    info.num_offsets = num_props = pinfo->nprops;
    po_size = sizeof(fsPropOffset) * num_props;
    po = offsets = (fsPropOffset *) fsalloc(po_size);
    if (!offsets)
	return AllocError;

    /*
     * copy over the static sized info, and figure out where eveyrthing will
     * live
     */
    for (i = 0, pp = pinfo->props; i < num_props; i++, po++, pp++) {
	po->name.position = data_len;
	po->name.length = strlen(NameForAtom(pp->name));
	data_len += po->name.length;
	if (pinfo->isStringProp[i]) {
	    po->type = PropTypeString;
	    po->value.position = data_len;
	    po->value.length = strlen(NameForAtom(pp->value));
	    data_len += po->value.length;
	} else {
	    po->type = PropTypeSigned;
	    po->value.position = pp->value;
	}
    }
    info.data_len = data_len;
    /* allocate the single chunk that the difs layer requires */
    ptr = (pointer) fsalloc(sizeof(fsPropInfo) + po_size + data_len);
    if (!ptr) {
	fsfree((char *) offsets);
	return AllocError;
    }
    /* move over the static stuff */
    *props = (fsPropInfo *) ptr;
    bcopy((char *) &info, (char *) ptr, sizeof(fsPropInfo));
    ptr += sizeof(fsPropInfo);
    bcopy((char *) offsets, (char *) ptr, po_size);
    ptr += po_size;

    /* move over the string info */
    for (i = 0, pp = pinfo->props, po = offsets;
	    i < num_props;
	    i++, po++, pp++) {
	char       *t;

	t = NameForAtom(pp->name);
	bcopy(t, (char *) ptr, po->name.length);
	ptr += po->name.length;
	if (po->type == PropTypeString) {
	    t = NameForAtom(pp->value);
	    bcopy(t, (char *) ptr, po->value.length);
	    ptr += po->value.length;
	}
    }
    assert(ptr == ((pointer) (*props) + sizeof(fsPropInfo) + po_size + data_len));

    fsfree((char *) offsets);
    return Successful;
}


int
LoadXFontInfo(client, pinfo, hdr, pi)
    ClientPtr client;		/* for client version info */
    FontInfoPtr pinfo;
    fsFontHeader *hdr;
    fsPropInfo **pi;
{
    int         err;

    hdr->flags = (pinfo->allExist) ? FontInfoAllCharsExist : 0;
    if (pinfo->drawDirection == LeftToRight)
	hdr->draw_direction = LeftToRightDrawDirection;
    else
	hdr->draw_direction = RightToLeftDrawDirection;

    if (pinfo->inkInside)
	hdr->flags |= FontInfoInkInside;
    if (client->major_version > 1) {
	hdr->char_range.min_char.low = pinfo->firstCol;
	hdr->char_range.min_char.high = pinfo->firstRow;
	hdr->char_range.max_char.low = pinfo->lastCol;
	hdr->char_range.max_char.high = pinfo->lastRow;
	hdr->default_char.low = pinfo->defaultCh & 0xff;
	hdr->default_char.high = pinfo->defaultCh >> 8;
    } else {
	hdr->char_range.min_char.high = pinfo->firstCol;
	hdr->char_range.min_char.low = pinfo->firstRow;
	hdr->char_range.max_char.high = pinfo->lastCol;
	hdr->char_range.max_char.low = pinfo->lastRow;
	hdr->default_char.high = pinfo->defaultCh & 0xff;
	hdr->default_char.low = pinfo->defaultCh >> 8;
    }

    CopyCharInfo(&pinfo->ink_minbounds, &hdr->min_bounds);
    CopyCharInfo(&pinfo->ink_maxbounds, &hdr->max_bounds);

    hdr->font_ascent = pinfo->fontAscent;
    hdr->font_descent = pinfo->fontDescent;

    err = convert_props(pinfo, pi);

    return err;
}

/*
 * does the real work of turning a list of range (or chars) into
 * a list of ranges
 */
static fsRange *
build_range(type, src, item_size, num, all)
    Bool        type;
    pointer     src;
    int         item_size;
    int        *num;
    Bool       *all;
{
    fsRange    *new = (fsRange *) 0,
               *np;
    unsigned long src_num;
    int         i;

    if (type) {			/* range flag is set, deal with data as a list
				 * of char2bs */
	fsChar2b   *rp = (fsChar2b *) src;

	src_num = *num / 2;
	if (src_num == 0) {
	    *all = TRUE;
	    return new;
	}
/* XXX - handle odd length list -- this could get nasty since
 * it has to poke into the font
 */
	np = new = (fsRange *) fsalloc(sizeof(fsRange) * src_num);
	if (!np) {
	    return np;
	}
	/* copy the ranges */
	for (i = 0; i < src_num; i++) {
	    np->min_char = *rp++;
	    np->max_char = *rp++;
	    np++;
	}
	*num = src_num;
	return new;
    } else {			/* deal with data as a list of characters */
	pointer     pp = src;

	src_num = *num / item_size;
	np = new = (fsRange *) fsalloc(sizeof(fsRange) * src_num);
	if (!np) {
	    return np;
	}
	/* convert each char to a range */
	for (i = 0; i < src_num; i++) {
	    if (item_size == 1) {
		np->min_char.low = *pp;
		np->min_char.high = 0;
	    } else {
		np->min_char.low = ((fsChar2b *) pp)->low;
		np->min_char.high = ((fsChar2b *) pp)->high;
	    }
/* XXX - eventually this should get smarter, and coallesce ranges */
	    np->max_char = np->min_char;
	    np++;
	    pp += item_size;
	}
	*num = src_num / item_size;
	return new;
    }
}

/*
 * provide backward compatibility with version 1, which had
 * the bytes of char2b backwards
 */
static void
swap_char2b (values, number)
    fsChar2b *values;
    int number;
{
    fsChar2b temp;
    int i;

    for (i = 0; i < number; i++) {
	temp.low = ((fsChar2b_version1 *)values)->low;
	temp.high = ((fsChar2b_version1 *)values)->high;
	*values++ = temp;
    }
}


static Bool
do_query_extents(client, c)
    ClientPtr   client;
    QEclosurePtr c;
{
    int         err;
    unsigned long lendata,
                num_extents;
    fsCharInfo *extents;
    fsQueryXExtents8Reply reply;

    err = GetExtents (c->client, c->pfont,
		     c->flags, c->nranges, c->range, &num_extents, &extents);
    if (err == Suspended) {
	if (!c->slept) {
	    c->slept = TRUE;
	    ClientSleep(client, do_query_extents, (pointer) c);
	}
	return TRUE;
    }
    if (err != Successful) {
	SendErrToClient(c->client, FontToFSError(err), (pointer) 0);
	goto finish;
    }
    reply.type = FS_Reply;
    reply.sequenceNumber = c->client->sequence;
    reply.num_extents = num_extents;
    lendata = sizeof(fsCharInfo) * num_extents;
    reply.length = (sizeof(fsQueryXExtents8Reply) + lendata) >> 2;
    if (client->swapped)
	SwapExtents(extents, num_extents);
    WriteReplyToClient(c->client, sizeof(fsQueryXExtents8Reply), &reply);
    (void) WriteToClient(c->client, lendata, (char *) extents);
    fsfree((char *) extents);
finish:
    if (c->slept)
	ClientWakeup(c->client);
    fsfree(c->range);
    fsfree(c);
    return TRUE;
}

int
QueryExtents(client, cfp, item_size, nranges, range_flag, range_data)
    ClientPtr   client;
    ClientFontPtr cfp;
    int         item_size;
    int         nranges;
    Bool        range_flag;
    pointer     range_data;
{
    QEclosurePtr c;
    fsRange    *fixed_range;
    Bool        all_glyphs = FALSE;

    if (item_size == 2  &&  client->major_version == 1)
	swap_char2b (range_data, nranges);

    fixed_range = build_range(range_flag, range_data, item_size,
			      &nranges, &all_glyphs);

    if (!fixed_range && !all_glyphs) {
	SendErrToClient(client, FSBadRange, 0);
	return FSBadRange;
    }
    c = (QEclosurePtr) fsalloc(sizeof(QEclosureRec));
    if (!c)
	return FSBadAlloc;
    c->client = client;
    c->slept = FALSE;
    c->pfont = cfp->font;
    c->flags = (all_glyphs) ? LoadAll : 0;
    c->flags |= (item_size == 1) ? EightBitFont : SixteenBitFont;
    c->nranges = nranges;
    c->range = fixed_range;

    (void) do_query_extents(client, c);
    return FSSuccess;
}

static Bool
do_query_bitmaps(client, c)
    ClientPtr   client;
    QBclosurePtr c;
{
    int         err;
    unsigned long num_glyphs, data_size;
    fsOffset   *offsets;
    pointer     glyph_data;
    fsQueryXBitmaps8Reply reply;
    int		freedata;

    err = GetBitmaps (c->client, c->pfont, c->format,
				    c->flags, c->nranges, c->range,
			     &data_size, &num_glyphs, &offsets, &glyph_data, &freedata);

    if (err == Suspended) {
	if (!c->slept) {
	    c->slept = TRUE;
	    ClientSleep(client, do_query_bitmaps, (pointer) c);
	}
	return TRUE;
    }
    if (err != Successful) {
	SendErrToClient(c->client, FontToFSError(err), (pointer) 0);
	goto finish;
    }
    reply.type = FS_Reply;
    reply.sequenceNumber = c->client->sequence;
    reply.replies_hint = 0;
    reply.num_chars = num_glyphs;
    reply.nbytes = data_size;
    reply.length = (sizeof(fsQueryXBitmaps8Reply) + data_size +
		    (sizeof(fsOffset) * num_glyphs) + 3) >> 2;

    WriteReplyToClient(c->client, sizeof(fsQueryXBitmaps8Reply), &reply);
    if (client->swapped)
	SwapLongs((long *)offsets, num_glyphs * 2);
    (void) WriteToClient(c->client, (num_glyphs * sizeof(fsOffset)),
			 (char *) offsets);
    (void) WriteToClient(c->client, data_size, (char *) glyph_data);
    fsfree((char *) offsets);
    if (freedata)
	fsfree((char *) glyph_data);
finish:
    if (c->slept)
	ClientWakeup(c->client);
    fsfree(c->range);
    fsfree(c);
    return TRUE;
}

int
QueryBitmaps(client, cfp, item_size, format, nranges, range_flag, range_data)
    ClientPtr   client;
    ClientFontPtr cfp;
    int         item_size;
    fsBitmapFormat format;
    int         nranges;
    Bool        range_flag;
    pointer     range_data;
{
    QBclosurePtr c;
    fsRange    *fixed_range;
    Bool        all_glyphs = FALSE;

    if (item_size == 2  &&  client->major_version == 1)
	swap_char2b (range_data, nranges);

    fixed_range = build_range(range_flag, range_data, item_size,
			      &nranges, &all_glyphs);

    if (!fixed_range && !all_glyphs) {
	SendErrToClient(client, FSBadRange, 0);
	return FSBadRange;
    }
    c = (QBclosurePtr) fsalloc(sizeof(QBclosureRec));
    if (!c)
	return FSBadAlloc;
    c->client = client;
    c->slept = FALSE;
    c->pfont = cfp->font;
    c->flags = (all_glyphs) ? LoadAll : 0;
    c->nranges = nranges;
    c->range = fixed_range;
    c->format = format;

    (void) do_query_bitmaps(client, c);
    return FSSuccess;
}
