/*
 * $XConsortium: pcf.h,v 1.1 91/05/10 14:45:47 keith Exp $
 *
 * Copyright 1990 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

#ifndef _PCF_H_
#define _PCF_H_

/*
 * Information used to read/write PCF fonts
 */

typedef struct _PCFTable {
    CARD32      type;
    CARD32      format;
    CARD32      size;
    CARD32      offset;
}           PCFTableRec, *PCFTablePtr;

#define PCF_FILE_VERSION	(('p'<<24)|('c'<<16)|('f'<<8)|1)
#define	PCF_FORMAT_MASK		0xffffff00

#define PCF_DEFAULT_FORMAT	0x00000000
#define PCF_INKBOUNDS		0x00000200
#define PCF_ACCEL_W_INKBOUNDS	0x00000100
#define PCF_COMPRESSED_METRICS	0x00000100

#define PCF_FORMAT_MATCH(a,b) (((a)&PCF_FORMAT_MASK) == ((b)&PCF_FORMAT_MASK))

#define PCF_GLYPH_PAD_MASK	(3<<0)
#define PCF_BYTE_MASK		(1<<2)
#define PCF_BIT_MASK		(1<<3)
#define PCF_SCAN_UNIT_MASK	(3<<4)

#define PCF_BYTE_ORDER(f)	(((f) & PCF_BYTE_MASK)?MSBFirst:LSBFirst)
#define PCF_BIT_ORDER(f)	(((f) & PCF_BIT_MASK)?MSBFirst:LSBFirst)
#define PCF_GLYPH_PAD_INDEX(f)	((f) & PCF_GLYPH_PAD_MASK)
#define PCF_GLYPH_PAD(f)	(1<<PCF_GLYPH_PAD_INDEX(f))
#define PCF_SCAN_UNIT_INDEX(f)	(((f) & PCF_SCAN_UNIT_MASK) >> 4)
#define PCF_SCAN_UNIT(f)	(1<<PCF_SCAN_UNIT_INDEX(f))
#define PCF_FORMAT_BITS(f)	((f) & (PCF_GLYPH_PAD_MASK|PCF_BYTE_MASK|PCF_BIT_MASK|PCF_SCAN_UNIT_MASK))

#define PCF_SIZE_TO_INDEX(s)	((s) == 4 ? 2 : (s) == 2 ? 1 : 0)
#define PCF_INDEX_TO_SIZE(b)	(1<<b)

#define PCF_FORMAT(bit,byte,glyph,scan) (\
    (PCF_SIZE_TO_INDEX(scan) << 4) | \
    (((bit) == MSBFirst ? 1 : 0) << 3) | \
    (((byte) == MSBFirst ? 1 : 0) << 2) | \
    (PCF_SIZE_TO_INDEX(glyph) << 0))

#define PCF_PROPERTIES		    (1<<0)
#define PCF_ACCELERATORS	    (1<<1)
#define PCF_METRICS		    (1<<2)
#define PCF_BITMAPS		    (1<<3)
#define PCF_INK_METRICS		    (1<<4)
#define	PCF_BDF_ENCODINGS	    (1<<5)
#define PCF_SWIDTHS		    (1<<6)
#define PCF_GLYPH_NAMES		    (1<<7)
#define PCF_BDF_ACCELERATORS	    (1<<8)

#endif				/* _PCF_H_ */
