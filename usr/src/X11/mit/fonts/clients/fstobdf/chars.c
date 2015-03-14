/* $XConsortium: chars.c,v 1.2 91/05/13 16:35:40 gildea Exp $ */
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
 *
 * @(#)chars.c	4.1	91/05/02
 *
 */

#include	<stdio.h>
#include	<X11/Xlib.h>
#include	"FSlib.h"

extern long yResolution;	/* intended vertical resoultion for font */
extern long pointSize;		/* font height in points */

#define BIT_ORDER	BitmapFormatBitOrderMSB
#define BYTE_ORDER	BitmapFormatByteOrderMSB
#define SCANLINE_UNIT	BitmapFormatScanlineUnit8
#define SCANLINE_PAD	BitmapFormatScanlinePad8
#define EXTENTS		BitmapFormatImageRectMin

#define SCANLINE_PAD_BYTES	1

#define GLWIDTHBYTESPADDED(bits, nBytes)				    \
	((nBytes) == 1 ? (((bits)  +  7) >> 3)		/* pad to 1 byte  */\
	:(nBytes) == 2 ? ((((bits) + 15) >> 3) & ~1)	/* pad to 2 bytes */\
	:(nBytes) == 4 ? ((((bits) + 31) >> 3) & ~3)	/* pad to 4 bytes */\
	:(nBytes) == 8 ? ((((bits) + 63) >> 3) & ~7)	/* pad to 8 bytes */\
	: 0)


static void
EmitBitmap(outFile, fontHeader, charInfo, encoding, bpr, data)
    FILE       *outFile;
    fsFontHeader *fontHeader;
    fsCharInfo *charInfo;
    unsigned int encoding;
    int         bpr;
    unsigned char *data;
{
    char       *glyphName;
    unsigned int row;

    /*-
     * format:
     * STARTCHAR name
     * ENCODING index
     * SWIDTH scalablewidth 0
     * DWIDTH pixels 0
     * BBX width height xoff yoff
     * ATTRIBUTES xxxx
     * BITMAP hhhhhhhh ...
     * ENDCHAR
     *
     * where, SWIDTH * (point / 1000) * (yres / 72) = DWIDTH or,
     *        SWIDTH = 72000 *
     * DWIDTH / (point * yres)
     */

    fprintf(outFile, "STARTCHAR ");
    glyphName = XKeysymToString((KeySym) encoding);
    if (glyphName)
	fputs(glyphName, outFile);
    else
	fprintf(outFile, (fontHeader->char_range.min_char.low > 0 ?
			  "C%06o" : "C%03o"), encoding);
    fputc('\n', outFile);
    fprintf(outFile, "ENCODING %u\n", encoding);
    fprintf(outFile, "SWIDTH %ld 0\n",
	    (((long) charInfo->width) * 72000L) /
	    (pointSize * yResolution));
    fprintf(outFile, "DWIDTH %d 0\n", charInfo->width);
    fprintf(outFile, "BBX %d %d %d %d\n",
	    charInfo->right - charInfo->left,
	    charInfo->ascent + charInfo->descent,
	    charInfo->left,
	    -charInfo->descent);
    if (charInfo->attributes)
	fprintf(outFile, "ATTRIBUTES 0x%04x\n", charInfo->attributes);

    /*
     * emit the bitmap
     */
    fprintf(outFile, "BITMAP\n");
    for (row = 0; row < (charInfo->ascent + charInfo->descent); row++) {
	unsigned    byte;
	unsigned    bit;

	static unsigned maskTab[] =
	{
	    (1 << 7), (1 << 6), (1 << 5), (1 << 4),
	    (1 << 3), (1 << 2), (1 << 1), (1 << 0),
	};

	byte = 0;
	for (bit = 0; bit < (charInfo->right - charInfo->left); bit++) {
	    byte |= maskTab[bit & 7] & data[bit >> 3];
	    if ((bit & 7) == 7) {
		fprintf(outFile, "%02x", byte);
		byte = 0;
	    }
	}
	if ((bit & 7) != 0)
	    fprintf(outFile, "%02x", byte);
	fputc('\n', outFile);
	data += bpr;
    }
    fprintf(outFile, "ENDCHAR\n");
}


Bool
EmitCharacters(outFile, fontServer, fontHeader, fontID)
    FILE       *outFile;
    FSServer   *fontServer;
    fsFontHeader *fontHeader;
    Font        fontID;
{
    fsCharInfo *extents;
    fsCharInfo *charInfo;
    int         encoding;
    fsOffset   *offsets;
    unsigned char *glyph;
    unsigned char *glyphs;
    unsigned int nChars;
    fsRange    *range;
    int         firstChar;
    int         lastChar;
    int         ch;
    fsBitmapFormat format;

    nChars = 0;
    range = &fontHeader->char_range;

    format = BYTE_ORDER | BIT_ORDER | SCANLINE_UNIT |
	SCANLINE_PAD | EXTENTS;
    firstChar = (range->min_char.high << 8) + range->min_char.low;
    lastChar = (range->max_char.high << 8) + range->max_char.low;

    (void) FSQueryXExtents16(fontServer, fontID, True, (fsChar2b *) 0, 0,
			     &extents);
    (void) FSQueryXBitmaps16(fontServer, fontID, format, True, (fsChar2b *) 0,
			     0, &offsets, &glyphs);

    charInfo = extents;
    /* calculate the actual number of chars */
    for (ch = 0; ch <= (lastChar - firstChar); ch++) {
	if ((charInfo->width != 0) || (charInfo->left != charInfo->right))
	    nChars++;
	charInfo++;
    }

    fprintf(outFile, "CHARS %u\n", nChars);

    /*
     * actually emit the characters
     */
    charInfo = extents;
    encoding = firstChar;
    glyph = glyphs;
    for (ch = 0; ch <= (lastChar - firstChar); ch++) {
	int         bpr;

	bpr = GLWIDTHBYTESPADDED((charInfo->right - charInfo->left),
				 SCANLINE_PAD_BYTES);
	if ((charInfo->width != 0) || (charInfo->right != charInfo->left))
	    EmitBitmap(outFile, fontHeader, charInfo, encoding, bpr, glyph);
	glyph += (charInfo->descent + charInfo->ascent) * bpr;

	charInfo++;
	encoding++;
    }
    FSFree((char *) extents);
    FSFree((char *) glyphs);
    FSFree((char *) offsets);
    return (True);
}
