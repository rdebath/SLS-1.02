/* $XConsortium: fsfuncs.c,v 1.1 91/10/18 11:23:03 keith Exp $ */
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
 *
 * $NCDId: @(#)fsfuncs.c,v 1.6 1991/07/02 16:59:18 lemke Exp $
 *
 */

#include    <X11/Xos.h>
#include	"fontmisc.h"
#include	"fontstruct.h"
#define FSMD_H
#include	"FSproto.h"

#define GLWIDTHBYTESPADDED(bits,nbytes) \
	((nbytes) == 1 ? (((bits)+7)>>3)        /* pad to 1 byte */ \
	:(nbytes) == 2 ? ((((bits)+15)>>3)&~1)  /* pad to 2 bytes */ \
	:(nbytes) == 4 ? ((((bits)+31)>>3)&~3)  /* pad to 4 bytes */ \
	:(nbytes) == 8 ? ((((bits)+63)>>3)&~7)  /* pad to 8 bytes */ \
	: 0)

#define GLYPH_SIZE(ch, nbytes)          \
	GLWIDTHBYTESPADDED((ch)->metrics.rightSideBearing - \
			(ch)->metrics.leftSideBearing, (nbytes))

#define n2dChars(pfi)   (((pfi)->lastRow - (pfi)->firstRow + 1) * \
                         ((pfi)->lastCol - (pfi)->firstCol + 1))

static CharInfoRec  junkDefault;

static int
getCharInfos (pfont, num_ranges, range, nump, retp)
    FontPtr	pfont;
    int		num_ranges;
    fsRange	*range;
    int		*nump;
    CharInfoPtr	**retp;
{
    CharInfoPtr	*xchars, *xci;
    int		nchars;
    FontInfoPtr pinfo = &pfont->info;
    int		r, c;
    unsigned char   ch[2];
    int         firstCol = pinfo->firstCol;
    int         firstRow = pinfo->firstRow;
    int         lastRow = pinfo->lastRow;
    int         lastCol = pinfo->lastCol;
    int		minCol, maxCol;
    int         num_cols = lastCol - firstCol + 1;
    fsRange	local_range, *rp;
    int		i;
    FontEncoding    encoding;
    int		err;
    unsigned long   glyphCount;
    unsigned short  defaultCh;
    CharInfoPtr	    defaultPtr;

    if (num_ranges == 0) {
	if (lastRow)
	    nchars = n2dChars(pinfo);
	else
	    nchars = lastCol - firstCol + 1;
	local_range.min_char.low = firstCol;
	local_range.min_char.high = firstRow;
	local_range.max_char.low = lastCol;
	local_range.max_char.high = lastRow;
	range = &local_range;
	num_ranges = 1;
    } else {
	nchars = 0;
	for (i = 0, rp = range; i < num_ranges; i++, rp++) {
	    if (rp->min_char.high > rp->max_char.high)
		return BadCharRange;
	    if (rp->min_char.high == rp->max_char.high)
	    {
		if (rp->min_char.low > rp->max_char.low)
		    return BadCharRange;
		nchars += rp->max_char.low - rp->min_char.low;
	    }
	    else
	    {
		nchars += lastRow - rp->min_char.low;
		nchars += (rp->max_char.high - rp->min_char.high - 1) * num_cols;
		nchars += rp->max_char.low - firstRow;
	    }
	}
    }
    xchars = (CharInfoPtr *) xalloc (sizeof (CharInfoPtr) * nchars);
    if (!xchars)
	return AllocError;
    xci = xchars;
    encoding = Linear16Bit;
    if (lastRow)
	encoding = TwoD16Bit;
    defaultCh = pinfo->defaultCh;
    ch[0] = defaultCh >> 8;
    ch[1] = defaultCh & 0xff;
    (*pfont->get_glyphs) (pfont, 1, ch, encoding,
			  &glyphCount, &defaultPtr);
    if (glyphCount != 1)
	defaultPtr = 0;
    for (i = 0, rp = range; i < num_ranges; i++, rp++) {
	for (r = rp->min_char.high; r <= rp->max_char.high; r++)
	{
	    minCol = firstCol;
	    if (r == rp->min_char.high)
		minCol = rp->min_char.low;
	    maxCol = lastCol;
	    if (r == rp->max_char.high)
		maxCol = rp->max_char.low;
	    for (c = minCol; c <= maxCol; c++) {
		ch[0] = r;
		ch[1] = c;
		err = (*pfont->get_glyphs) (pfont, 1, ch, encoding,
					    &glyphCount, xci);
		if (err != Successful)
		{
		    xfree (xchars);
		    return err;
		}
		if (glyphCount != 1 || 
		    *xci == defaultPtr && defaultCh != ((r<<8)+c))
		    *xci = &junkDefault;
		xci++;
	    }
	}
    }
    *retp = xchars;
    *nump = nchars;
    return Successful;
}

int
GenericGetExtents(client, pfont, flags, num_ranges, range, num_extents, data)
    pointer     client;
    FontPtr     pfont;
    Mask        flags;
    unsigned long num_ranges;
    fsRange    *range;
    unsigned long *num_extents;
    fsCharInfo **data;
{
    unsigned long size;
    fsCharInfo *ci,
               *pci;
    fsRange    *rp;
    CharInfoPtr	*xchars, *xcharsFree, xci;
    int		nchars;
    int		err;

    if (flags & LoadAll)
	num_ranges = 0;
    err = getCharInfos (pfont, num_ranges, range, &nchars, &xchars);
    if (err != Successful)
	return err;

    size = sizeof(fsCharInfo) * nchars;
    pci = ci = (fsCharInfo *) xalloc(size);
    if (!ci) {
	xfree (xchars);
	return AllocError;
    }

    *num_extents = nchars;
    xcharsFree = xchars;

    while (nchars--) {
	xci = *xchars++;
	pci->ascent = xci->metrics.ascent;
	pci->descent = xci->metrics.descent;
	pci->left = xci->metrics.leftSideBearing;
	pci->right = xci->metrics.rightSideBearing;
	pci->width = xci->metrics.characterWidth;
	pci->attributes = 0;
	pci++;
    }

    xfree (xcharsFree);

    *data = ci;

    return Successful;
}

static int
packGlyphs (pfont, format, flags, num_ranges, range, tsize, num_glyphs,
		   offsets, data, freeData)
    FontPtr     pfont;
    int         format;
    Mask        flags;
    unsigned long num_ranges;
    fsRange    *range;
    int        *tsize;
    unsigned long *num_glyphs;
    fsOffset  **offsets;
    pointer    *data;
    int		*freeData;
{
    unsigned long start,
                end;
    int         i;
    fsOffset	*lengths, *l;
    unsigned long size = 0;
    pointer     gdata,
                gd;
    long        ch;
    int         bitorder,
                byteorder,
                scanlinepad,
                scanlineunit,
                mappad;
    int         height, bpr,
		charsize,
                skiprows = 0;
    Bool	contiguous, reformat;
    fsRange    *rp = range;
    int		nchars;
    int         src_glyph_pad = pfont->glyph;
    int         src_bit_order = pfont->bit;
    int         src_byte_order = pfont->byte;
    int         err;
    int		max_ascent, max_descent;
    int		min_left, max_right;
    int		srcbpr;
    int		lshift = 0, rshift = 0;
    unsigned char   *src;
    unsigned char   *dst;
    unsigned char   bits1, bits2;
    int		    width;
    int		    src_extra;
    int		    dst_extra;
    int		    r, w;
    fsRange	allRange;
    CharInfoPtr	*xchars, *xcharsFree, ci;
    FontInfoPtr	pinfo = &pfont->info;
    xCharInfo	*cim;

    err = CheckFSFormat(format, (fsBitmapFormatMask) ~ 0,
		&bitorder, &byteorder, &scanlineunit, &scanlinepad, &mappad);

    if (err != Successful)
	return err;

    if (flags & LoadAll)
	num_ranges = 0;

    err = getCharInfos (pfont, num_ranges, range, &nchars, &xcharsFree);

    if (err != Successful)
	return err;
    
    /* get space for glyph offsets */
    lengths = (fsOffset *) xalloc(sizeof(fsOffset) * nchars);
    if (!lengths) {
	xfree (xcharsFree);
	return AllocError;
    }

    /* compute bpr for padded out fonts */
    reformat = bitorder != src_bit_order || byteorder != src_byte_order;
    switch (mappad)
    {
    case BitmapFormatImageRectMax:
	max_ascent = FONT_MAX_ASCENT(pinfo);
	max_descent = FONT_MAX_DESCENT(pinfo);
	height = max_ascent + max_descent;
	if (height != pinfo->minbounds.ascent + pinfo->minbounds.descent)
	    reformat = TRUE;
	/* fall through */
    case BitmapFormatImageRectMaxWidth:
	min_left = FONT_MIN_LEFT(pinfo);
	max_right = FONT_MAX_RIGHT(pinfo);
	if (min_left != pinfo->maxbounds.leftSideBearing)
	    reformat = TRUE;
	bpr = GLWIDTHBYTESPADDED(max_right - min_left, scanlinepad);
	break;
    case BitmapFormatImageRectMin:
	break;
    }
    charsize = bpr * height;
    size = 0;
    gdata = 0;
    contiguous = TRUE;
    l = lengths;
    for (i = 0, xchars = xcharsFree; i < nchars; i++, xchars++) {
    	ci = *xchars;
    	l->position = size;
    	if (ci && ci->bits) {
	    if (!gdata)
		gdata = (pointer) ci->bits;
	    if ((char *) gdata + size != ci->bits)
		contiguous = FALSE;
	    if (mappad == BitmapFormatImageRectMin)
		bpr = GLYPH_SIZE(ci, scanlinepad);
	    if (mappad != BitmapFormatImageRectMax)
	    {
		height = ci->metrics.ascent +
			 ci->metrics.descent;
		charsize = height * bpr;
	    }
	    l->length = charsize;
	    size += charsize;
	}
	else
	    l->length = 0;
	l++;
    }
    if (contiguous && !reformat)
    {
	*num_glyphs = nchars;
	*freeData = FALSE;
	*data = gdata;
	*tsize = size;
	*offsets = lengths;
	xfree (xcharsFree);
	return Successful;
    }
    if (size)
    {
	gdata = (pointer) xalloc(size);
	if (!gdata) {
	    xfree (xcharsFree);
	    xfree (lengths);
	    return AllocError;
	}
	bzero ((char *) gdata, size);
    }

    *freeData = TRUE;
    l = lengths;
    gd = gdata;

    /* finally do the work */
    for (i = 0, xchars = xcharsFree; i < nchars; i++, xchars++, l++) {
	ci = *xchars;
	/* ignore missing chars */
	if (!ci || !ci->bits)
	    continue;
	cim = &ci->metrics;

	srcbpr = GLWIDTHBYTESPADDED(cim->rightSideBearing -
				    cim->leftSideBearing, src_glyph_pad);
	/*
	 * caculate bytes-per-row for PadNone (others done in allocation
	 * phase), what (if anything) to ignore or add as padding
	 */
	switch (mappad) {
	case BitmapFormatImageRectMin:
	    bpr = GLYPH_SIZE(ci, scanlinepad);
	    break;
	case BitmapFormatImageRectMax:
	    /* leave the first padded rows blank */
	    gd += bpr * (max_ascent - cim->ascent);
	    skiprows = bpr * (max_descent - cim->descent);
	    /* fall thru */
	case BitmapFormatImageRectMaxWidth:
	    rshift = cim->leftSideBearing - min_left;
	    lshift = 8 - lshift;
	    break;
	}
	src = (unsigned char *) ci->bits;
	dst = gd;
	width = srcbpr;
	if (srcbpr > bpr)
	    width = bpr;
	src_extra = srcbpr - width;
	dst_extra = bpr - width;

#if (DEFAULTBITORDER == MSBFirst)
#define BitLeft(b,c)	((b) << (c))
#define BitRight(b,c)	((b) >> (c))
#else
#define BitLeft(b,c)	((b) >> (c))
#define BitRight(b,c)	((b) << (c))
#endif
	if (!rshift)
	{
	    if (srcbpr == bpr)
	    {
		r = (cim->ascent + cim->descent) * width;
		bcopy (src, dst, r);
		dst += r;
	    }
	    else
	    {
		for (r = cim->ascent + cim->descent; r; r--)
		{
		    for (w = width; w; w--)
			*dst++ = *src++;
		    dst += dst_extra;
		    src += src_extra;
		}
	    }
	}
	else
	{
	    for (r = cim->ascent + cim->descent; r; r--)
	    {
		bits2 = 0;
		for (w = width; w; w--)
		{
		    bits1 = *src++;
		    *dst++ = BitRight(bits1, rshift) |
			     BitLeft (bits2, lshift);
		    bits2 = bits1;
		}
		dst += dst_extra;
		src += src_extra;
	    }
	}
	/* skip the amount we just filled in */
	gd += l->length;
    }


    /* now do the bit, byte, word swapping */
    if (bitorder != src_bit_order)
	BitOrderInvert(gdata, size);
    if (byteorder != src_byte_order) {
	if (scanlineunit == 2)
	    TwoByteSwap(gdata, size);
	else if (scanlineunit == 4)
	    FourByteSwap(gdata, size);
    }
    xfree (xcharsFree);
    *num_glyphs = nchars;
    *data = gdata;
    *tsize = size;
    *offsets = lengths;

    return Successful;
}

/* ARGSUSED */
int
GenericGetBitmaps(client, pfont, format, flags, num_ranges, range,
		 size, num_glyphs, offsets, data, freeData)
    pointer     client;
    FontPtr     pfont;
    fsBitmapFormat format;
    Mask        flags;
    unsigned long num_ranges;
    fsRange    *range;
    int        *size;
    unsigned long *num_glyphs;
    fsOffset  **offsets;
    pointer    *data;
    int		*freeData;
{
    assert(pfont);

    *size = 0;
    *data = (pointer) 0;
    return packGlyphs (pfont, format, flags,
			      num_ranges, range, size, num_glyphs,
			      offsets, data, freeData);
}
