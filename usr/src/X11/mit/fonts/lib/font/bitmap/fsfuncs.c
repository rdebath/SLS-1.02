/* $XConsortium: fsfuncs.c,v 1.6 91/07/31 01:08:36 keith Exp $ */
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

#include	"fontfilest.h"
#include	"bitmap.h"
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

static CharInfoRec junkDefault;

static void
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

/* ARGSUSED */
static      FontEncoding
font_encoding(flags, pfont)
    Mask        flags;
    FontPtr     pfont;
{
    /* XXX this isn't really right, but it works for what we use */
    return (pfont->info.lastRow == 0) ? Linear16Bit : TwoD16Bit;
}


static fsCharInfo *
do_extent_copy(pfont, flags, start, end, dst)
    FontPtr     pfont;
    Mask        flags;
    int         start,
                end;
    fsCharInfo *dst;
{
    fsCharInfo *pci;
    int         i,
                r,
                c;
    int         encoding;
    BitmapFontPtr bitmapfont;
    CharInfoPtr src;
    int         firstCol = pfont->info.firstCol;
    int         firstRow = pfont->info.firstRow;
    int         lastRow = pfont->info.lastRow;
    int         lastCol = pfont->info.lastCol;
    int         num_cols = lastCol - firstCol + 1;
    int         num_rows = lastRow - firstRow + 1;

    encoding = font_encoding(flags, pfont);
    bitmapfont = (BitmapFontPtr) pfont->fontPrivate;
    pci = dst;

    if (flags & LoadAll) {
	start = (firstRow << 8) + firstCol;
	end = (lastRow << 8) + lastCol;
    }
    switch (encoding) {
    case Linear8Bit:
    case TwoD8Bit:
    case Linear16Bit:
	start -= firstCol;
	end -= firstCol;
	for (i = start; i <= end; i++) {
	    src = bitmapfont->encoding[i];
	    if (!src)
		src = bitmapfont->pDefault;
	    CopyCharInfo(src, pci);
	    pci++;
	}
	break;
    case TwoD16Bit:
	for (i = start; i <= end; i++) {
	    r = (i >> 8) - firstRow;
	    c = (i & 0xff) - firstCol;
	    if (r < num_rows && c >= 0 && c < num_cols) {
		src = bitmapfont->encoding[r * num_cols + c];
		if (!src)
		    src = bitmapfont->pDefault;
		CopyCharInfo(src, pci);
		pci++;
	    }
	}
	break;
    }
    return pci;
}

static int
convert_index(pfont, ch)
    FontPtr     pfont;
    int         ch;

{
    int         firstCol = pfont->info.firstCol;
    int         firstRow = pfont->info.firstRow;
    int         lastRow = pfont->info.lastRow;
    int         lastCol = pfont->info.lastCol;

    if (lastRow) {
	int         num_rows = lastRow - firstRow + 1;
	int         num_cols = lastCol - firstCol + 1;
	int         r,
	            c;

	r = (ch >> 8) - firstRow;
	c = (ch & 0xff) - firstCol;
	if (r < num_rows && c >= 0 && c < num_cols)
	    return (r * num_cols + c);
	return -1;
    } else {
	ch -= firstCol;
	return ch;
    }
}

static      CharInfoPtr
font_metrics(pfont, ch)
    FontPtr     pfont;
    int         ch;

{
    BitmapFontPtr bitmapfont = (BitmapFontPtr) pfont->fontPrivate;
    int         lastRow = pfont->info.lastRow;

    ch = convert_index(pfont, ch);
    if (lastRow) {
	if (ch >= 0)
	    return bitmapfont->encoding[ch];
	else
	    return bitmapfont->pDefault;
    } else {
	return bitmapfont->encoding[ch];
    }
}



/* ARGSUSED */
int
bitmapGetExtents(client, pfont, flags, num_ranges, range, num_extents, data)
    pointer     client;
    FontPtr     pfont;
    Mask        flags;
    unsigned long num_ranges;
    fsRange    *range;
    unsigned long *num_extents;
    fsCharInfo **data;

{
    int         start,
                end,
                i;
    unsigned long size;
    fsCharInfo *ci,
               *pci;
    fsRange    *rp;
    FontInfoPtr pinfo;
    BitmapFontPtr bitmapfont;
    int         firstCol = pfont->info.firstCol;
    int         firstRow = pfont->info.firstRow;
    int         lastRow = pfont->info.lastRow;
    int         lastCol = pfont->info.lastCol;
    int         num_cols = lastCol - firstCol + 1;

    assert(pfont);
    pinfo = &pfont->info;
    bitmapfont = (BitmapFontPtr) pfont->fontPrivate;

    if (!bitmapfont->pDefault)
	bitmapfont->pDefault = &junkDefault;
    if (flags & LoadAll) {
	start = (firstRow << 8) + firstCol;
	end = (lastRow << 8) + lastCol;

	if (lastRow) {
	    *num_extents = n2dChars(pinfo);
	} else {
	    *num_extents = end - start + 1;
	}
	size = sizeof(fsCharInfo) * (*num_extents);
	pci = ci = (fsCharInfo *) xalloc(size);
	if (!ci)
	    return AllocError;

	pci = do_extent_copy(pfont, flags, 0, 0, ci);

	/* make sure it didn't go off the end */
	assert(pci == (fsCharInfo *) ((char *) ci + size));
	assert(pci == (ci + (*num_extents)));

	if (bitmapfont->pDefault == &junkDefault)
	    bitmapfont->pDefault = 0;

	*data = ci;
	return Successful;
    }
    /* normal case */
    /* figure out how big everything has to be */
    *num_extents = 0;
    for (i = 0, rp = range; i < num_ranges; i++, rp++) {
	start = (rp->min_char.high << 8) + rp->min_char.low;
	end = (rp->max_char.high << 8) + rp->max_char.low;

	/* range check */
	if (end < start ||
		(end > (pinfo->lastRow << 8) + pinfo->lastCol)
		|| (end < (pinfo->firstRow << 8) + pinfo->firstCol)
		|| (start > (pinfo->lastRow << 8) + pinfo->lastCol)
		|| (start < (pinfo->firstRow << 8) + pinfo->firstCol))
	    return BadCharRange;

	/* adjust for SNF layout */
	start -= (firstRow << 8) + firstCol;
	end -= (firstRow << 8) + firstCol;
	if (lastRow) {
	    *num_extents = ((end >> 8) - (start >> 8) + 1) * num_cols;
	} else {
	    *num_extents += end - start + 1;
	}
    }

    size = sizeof(fsCharInfo) * (*num_extents);
    pci = ci = (fsCharInfo *) xalloc(size);
    if (!ci)
	return AllocError;

    /* copy all the extents */
    for (i = 0, rp = range; i < num_ranges; i++, rp++) {
	start = (rp->min_char.high << 8) + rp->min_char.low;
	end = (rp->max_char.high << 8) + rp->max_char.low;

	pci = do_extent_copy(pfont, flags, start, end, pci);

	/* make sure it didn't go off the end */
	assert(pci == (fsCharInfo *) ((char *) ci + size));
    }

    *data = ci;
    if (bitmapfont->pDefault == &junkDefault)
	bitmapfont->pDefault = 0;

    return Successful;
}

static
numCharsInRange (startRow, endRow, startCol, endCol, firstCol, lastCol)
{
    int	nchars;

    if (startRow != endRow)
    {
    	/* first page */
    	nchars = lastCol - startCol + 1;
    	/* middle pages */
    	nchars += (endRow - startRow - 1) * (lastCol - firstCol + 1);
    	/* last page */
    	nchars += endCol - firstCol + 1;
    }
    else
    	nchars = endCol - startCol + 1;
    return nchars;
}

static unsigned long
compute_data_size(pfont, mappad, scanlinepad, startRow, endRow, startCol, endCol)
    FontPtr     pfont;
    int         mappad,
                scanlinepad;
    int		startRow, endRow, startCol, endCol;
{
    unsigned long size = 0;
    int         bpr;
    BitmapFontPtr bitmapfont = (BitmapFontPtr) pfont->fontPrivate;
    FontInfoPtr pinfo = &pfont->info;
    int		row, col;
    int		firstCol, lastCol, nCol;
    int		firstRow;
    int		ch;

    firstCol = pinfo->firstCol;
    lastCol = pinfo->lastCol;
    firstRow = pinfo->firstRow;
    nCol = lastCol - firstCol + 1;
    switch (mappad) {
	int         charsize;
	CharInfoPtr ci;
	xCharInfo  *cim;

    case BitmapFormatImageRectMin:
	ch = (startRow - firstRow) * (lastCol - firstCol + 1) +
	     (startCol - firstCol);
	for (row = startRow; row <= endRow; row++)
	{
	    for (col = (row == startRow ? startCol : firstCol);
		 col <= (row == endRow ? endCol : lastCol);
		 col++)
	    {
		if (!(ci = bitmapfont->encoding[ch]))
		    ci = bitmapfont->pDefault;
		cim = &ci->metrics;
		charsize = GLYPH_SIZE(ci, scanlinepad);
		charsize *= cim->ascent + cim->descent;
		size += charsize;
		ch++;
	    }
	}
	break;
    case BitmapFormatImageRectMaxWidth:
	bpr = GLWIDTHBYTESPADDED(FONT_MAX_WIDTH(pinfo), scanlinepad);
	ch = (startRow - firstRow) * (lastCol - firstCol + 1) +
	     (startCol - firstCol);
	for (row = startRow; row <= endRow; row++)
	{
	    for (col = (row == startRow ? startCol : firstCol);
		 col <= (row == endRow ? endCol : lastCol);
		 col++)
	    {
		if (!(ci = bitmapfont->encoding[ch]))
		    ci = bitmapfont->pDefault;
		cim = &ci->metrics;
		charsize = bpr * (cim->ascent + cim->descent);
		size += charsize;
		ch++;
	    }
	}
	break;
    case BitmapFormatImageRectMax:
	bpr = GLWIDTHBYTESPADDED(FONT_MAX_WIDTH(pinfo), scanlinepad);
	size = numCharsInRange (startRow, endRow, startCol, endCol, firstCol, lastCol)
		* bpr * FONT_MAX_HEIGHT(pinfo);
	break;
    default:
	assert(0);
    }

    return size;
}

/*
 * packs up the glyphs as requested by the format
 */

int
bitmap_pack_glyphs(pfont, format, flags, num_ranges, range, tsize, num_glyphs,
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
    FontInfoPtr pinfo = &pfont->info;
    BitmapFontPtr bitmapfont = (BitmapFontPtr) pfont->fontPrivate;
    int		row, col;
    int		nchars;
    int		startCol, endCol;
    int		startRow, endRow;
    int         firstCol = pfont->info.firstCol;
    int         firstRow = pfont->info.firstRow;
    int         lastRow = pfont->info.lastRow;
    int         lastCol = pfont->info.lastCol;
    int         numCols = lastCol - firstCol + 1;
    int         src_glyph_pad = pfont->glyph;
    int         src_bit_order = pfont->bit;
    int         src_byte_order = pfont->byte;
    int         err;
    int		max_ascent, max_descent;
    int		min_left, max_right;
    fsRange	allRange;

    err = CheckFSFormat(format, (fsBitmapFormatMask) ~ 0,
		&bitorder, &byteorder, &scanlineunit, &scanlinepad, &mappad);

    if (err != Successful)
	return err;

    /* special case for all glyphs first */
    if (flags & LoadAll) {
	if (firstRow == lastRow)
	{
	    allRange.min_char.low = firstCol & 0xff;
	    allRange.min_char.high = firstCol >> 8;
	    allRange.max_char.low = lastCol & 0xff;
	    allRange.max_char.high = lastCol >> 8;
	} 
	else
	{
	    allRange.min_char.low = firstCol;
	    allRange.min_char.high = firstRow;
	    allRange.max_char.low = lastCol;
	    allRange.max_char.high = lastRow;
	}
	range = &allRange;
	num_ranges = 1;
    }
    nchars = 0;
    for (i = 0, rp = range; i < num_ranges; i++, rp++) {
	if (firstRow == lastRow)
	{
	    endRow = startRow = firstRow;
	    startCol = (rp->min_char.high << 8) | rp->min_char.low;
	    endCol = (rp->max_char.high << 8) | rp->max_char.low;
	}
	else
	{
	    startRow = rp->min_char.high;
	    startCol = rp->min_char.low;
	    endRow = rp->max_char.high;
	    endCol = rp->max_char.low;
	}
	/* range check */
	if (startRow > endRow ||
	    startCol > endCol ||
	    startRow < firstRow ||
	    startCol < firstCol ||
	    lastRow < endRow ||
	    lastCol < endCol)
	{
	    return BadCharRange;
	}
	nchars += numCharsInRange (startRow, endRow, startCol, endCol, firstCol, lastCol);
    }

    /* get space for glyph offsets */
    lengths = (fsOffset *) xalloc(sizeof(fsOffset) * nchars);
    if (!lengths)
	return AllocError;

    bitorder = (bitorder == BitmapFormatBitOrderLSB) ?
	LSBFirst : MSBFirst;
    byteorder = (byteorder == BitmapFormatByteOrderLSB) ?
	LSBFirst : MSBFirst;

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
    for (i = 0, rp = range; i < num_ranges; i++, rp++)
    {
	CharInfoPtr ci;
    
	if (firstRow == lastRow)
	{
	    endRow = startRow = firstRow;
	    startCol = (rp->min_char.high << 8) | rp->min_char.low;
	    endCol = (rp->max_char.high << 8) | rp->max_char.low;
	}
	else
	{
	    startRow = rp->min_char.high;
	    startCol = rp->min_char.low;
	    endRow = rp->max_char.high;
	    endCol = rp->max_char.low;
	}
	ch = (startRow - firstRow) * (lastCol - firstCol + 1) +
	     (startCol - firstCol);
	for (row = startRow; row <= endRow; row++)
	{
	    for (col = (row == startRow ? startCol : firstCol);
		 col <= (row == endRow ? endCol : lastCol);
		 col++)
	    {
		l->position = size;
		if ((ci = bitmapfont->encoding[ch]) && ci->bits)
		{
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
		ch++;
	    }
	}
    }
    if (contiguous && !reformat)
    {
	*num_glyphs = nchars;
	*freeData = FALSE;
	*data = gdata;
	*tsize = size;
	*offsets = lengths;
	return Successful;
    }
    if (size)
    {
	gdata = (pointer) xalloc(size);
	if (!gdata)
	    return AllocError;
    }

    if (mappad == BitmapFormatImageRectMax)
	bzero ((char *) gdata, size);

    *freeData = TRUE;
    l = lengths;
    gd = gdata;

    /* finally do the work */
    for (i = 0, rp = range; i < num_ranges; i++, rp++) {
	if (firstRow == lastRow)
	{
	    endRow = startRow = firstRow;
	    startCol = (rp->min_char.high << 8) | rp->min_char.low;
	    endCol = (rp->max_char.high << 8) | rp->max_char.low;
	}
	else
	{
	    startRow = rp->min_char.high;
	    startCol = rp->min_char.low;
	    endRow = rp->max_char.high;
	    endCol = rp->max_char.low;
	}
	ch = (startRow - firstRow) * (lastCol - firstCol + 1) +
	     (startCol - firstCol);
	for (row = startRow; row <= endRow; row++)
	{
	    for (col = (row == startRow ? startCol : firstCol);
		 col <= (row == endRow ? endCol : lastCol);
		 col++)
	    {
	    	CharInfoPtr ci;
	    	xCharInfo  *cim;
	    	int         srcbpr;
		unsigned char	*src, *dst;
		unsigned int	bits1, bits2;
	    	int         r,
	                    lshift = 0,
			    rshift = 0,
			    width,
			    w,
			    src_extra,
			    dst_extra;
    
	    	ci = bitmapfont->encoding[ch];
	    	ch++;
    
	    	/* ignore missing chars */
	    	if (!ci) 
	    	{
		    l++;
		    continue;
	    	}
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
	    	l++;
	    }
	}
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
    *num_glyphs = nchars;
    *data = gdata;
    *tsize = size;
    *offsets = lengths;

    return Successful;
}

/* ARGSUSED */
int
bitmapGetBitmaps(client, pfont, format, flags, num_ranges, range,
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
    return bitmap_pack_glyphs(pfont, format, flags,
			      num_ranges, range, size, num_glyphs,
			      offsets, data, freeData);
}
