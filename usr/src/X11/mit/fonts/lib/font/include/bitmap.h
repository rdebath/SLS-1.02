/* $Header: /home/x_cvs/mit/fonts/lib/font/include/bitmap.h,v 1.3 1992/05/23 12:42:40 dawes Exp $ */
/*
 * $XConsortium: bitmap.h,v 1.1 91/05/11 09:11:56 rws Exp $
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

#ifndef _BITMAP_H_
#define _BITMAP_H_

#include    <stdio.h>

/*
 * Internal format used to store bitmap fonts
 */

typedef struct _BitmapExtra {
    Atom       *glyphNames;
    int        *sWidths;
    CARD32      bitmapsSizes[GLYPHPADOPTIONS];
    FontInfoRec info;
}           BitmapExtraRec, *BitmapExtraPtr;

typedef struct _BitmapFont {
    unsigned    version_num;
    int         num_chars;
    int         num_tables;
    CharInfoPtr metrics;	/* font metrics, including glyph pointers */
    xCharInfo  *ink_metrics;	/* ink metrics */
    char       *bitmaps;	/* base of bitmaps, useful only to free */
    CharInfoPtr *encoding;	/* array of char info pointers */
    CharInfoPtr pDefault;	/* default character */
    BitmapExtraPtr bitmapExtra;	/* stuff not used by X server */
}           BitmapFontRec, *BitmapFontPtr;

extern int  bitmapReadFont(), bitmapReadFontInfo();
extern int  bitmapGetGlyphs(), bitmapGetMetrics();
extern int  bitmapGetBitmaps(), bitmapGetExtents();
extern void bitmapUnloadFont();

extern void bitmapComputeFontBounds();
extern void bitmapComputeFontInkBounds();

typedef struct _FontFile {
    FILE	*file;
    int		compressed;
} *FontFilePtr;

#define BITS		16
#define STACK_SIZE	8192

typedef struct _CompressedFile {
    FILE		*file;

    unsigned char	*stackp;
    long		oldcode;
    unsigned char	finchar;

    int			block_compress;
    int			maxbits;
    long		maxcode, maxmaxcode;

    long		free_ent;
    int			clear_flg;
    int			n_bits;

    /* bit buffer */
    int			offset, size;
    unsigned char	buf[BITS];

    unsigned char	de_stack[STACK_SIZE];
    unsigned char	*tab_suffix;
    unsigned short	*tab_prefix;
} CompressedFile;

extern int _filldcbuf();
extern CompressedFile *CompressedFileOpen();
extern int CompressedFileClose();
extern int CompressedFileRead();
extern int CompressedFileSkip();

#define getdcchar(file) \
((file)->stackp > (file)->de_stack ? (*--((file)->stackp)) : _filldcbuf(file))

#define FontFileGetc(f)	((f)->compressed ? \
    getdcchar((CompressedFile *)((f)->file)) : getc((f)->file))

#define FontFileRead(f,b,n) ((f)->compressed ? \
    CompressedFileRead(b, n, (CompressedFile *)((f)->file)) : \
    fread((char *) b, 1, n, (f)->file))

#define FontFileSkip(f,n) ((f)->compressed ? \
    CompressedFileSkip((CompressedFile *)((f)->file), n) : \
    (fseek((f)->file, n, 1) != -1))

#define FontFileSeek(f,n) \
    ((f)->compressed ? abort(), 0 : (fseek((f)->file,n,0) != -1))

#define FontFilePutc(c,f) \
    ((f)->compressed ? abort(), 0 : putc(c,(f)->file))

#define FontFileWrite(f,b,n) \
    ((f)->compressed ? abort(), 0 : fwrite ((char *) b, 1, n, (f)->file))

#define FontFileEOF	EOF

extern FontFilePtr FontFileOpen();
extern int FontFileClose();

#endif				/* _BITMAP_H_ */
