/* $XConsortium: format.c,v 1.3 92/04/15 16:16:08 gildea Exp $ */
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

#include	"FSproto.h"
#include	"font.h"

int
CheckFSFormat(format, fmask, bit_order, byte_order, scan, glyph, image)
    fsBitmapFormat format;
    fsBitmapFormatMask fmask;
    int        *bit_order,
               *byte_order,
               *scan,
               *glyph,
               *image;
{
    /* convert format to what the low levels want */
    if (fmask & BitmapFormatMaskBit) {
	*bit_order = format & BitmapFormatBitOrderMask;
	*bit_order = (*bit_order == BitmapFormatBitOrderMSB)
	    	     ? MSBFirst : LSBFirst;
    }
    if (fmask & BitmapFormatMaskByte) {
	*byte_order = format & BitmapFormatByteOrderMask;
	*byte_order = (*byte_order == BitmapFormatByteOrderMSB)
	    	      ? MSBFirst : LSBFirst;
    }
    if (fmask & BitmapFormatMaskScanLineUnit) {
	*scan = format & BitmapFormatScanlineUnitMask;
	/* convert byte paddings into byte counts */
	switch (*scan) {
	case BitmapFormatScanlineUnit8:
	    *scan = 1;
	    break;
	case BitmapFormatScanlineUnit16:
	    *scan = 2;
	    break;
	case BitmapFormatScanlineUnit32:
	    *scan = 4;
	    break;
	default:
	    return BadFontFormat;
	}
    }
    if (fmask & BitmapFormatMaskScanLinePad) {
	*glyph = format & BitmapFormatScanlinePadMask;
	/* convert byte paddings into byte counts */
	switch (*glyph) {
	case BitmapFormatScanlinePad8:
	    *glyph = 1;
	    break;
	case BitmapFormatScanlinePad16:
	    *glyph = 2;
	    break;
	case BitmapFormatScanlinePad32:
	    *glyph = 4;
	    break;
	default:
	    return BadFontFormat;
	}
    }
    if (fmask & BitmapFormatMaskImageRectangle) {
	*image = format & BitmapFormatImageRectMask;

	if (*image != BitmapFormatImageRectMin &&
		*image != BitmapFormatImageRectMaxWidth &&
		*image != BitmapFormatImageRectMax)
	    return BadFontFormat;
    }
    return Successful;
}
