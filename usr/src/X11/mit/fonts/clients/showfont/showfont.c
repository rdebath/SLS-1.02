/* $XConsortium: showfont.c,v 1.7 92/05/19 17:10:30 gildea Exp $ */
/*
 * Copyright 1990 Network Computing Devices;
 * Portions Copyright 1987 by Digital Equipment Corporation and the
 * Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of Network Computing Devices, Digital or
 * M.I.T. not be used in advertising or publicity pertaining to distribution
 * of the software without specific, written prior permission.
 *
 * NETWORK COMPUTING DEVICES, DIGITAL AND M.I.T. DISCLAIM ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS.  IN NO EVENT SHALL NETWORK COMPUTING DEVICES,
 * DIGITAL OR M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
 * THIS SOFTWARE.
 */
#include	<stdio.h>
#include	<ctype.h>
#include	"FSlib.h"

/*
 * the equivalent of showsnf
 */

#define	GLWIDTHBYTESPADDED(bits,nbytes) \
	((nbytes) == 1 ? (((bits)+7)>>3)        /* pad to 1 byte */ \
	:(nbytes) == 2 ? ((((bits)+15)>>3)&~1)  /* pad to 2 bytes */ \
	:(nbytes) == 4 ? ((((bits)+31)>>3)&~3)  /* pad to 4 bytes */ \
	:(nbytes) == 8 ? ((((bits)+63)>>3)&~7)  /* pad to 8 bytes */ \
	: 0)

FSServer   *svr;
int         pad = 8,
            bitmap_pad = 0,
            scan_unit = 8;

/* set from bitmap_pad to ImageRect, ImageMaxWidth, or ImageMax */
int	    bitmap_format;	

int         bitorder = MSBFirst;
int         byteorder = MSBFirst;
int         first_ch = 0;
int         end_ch = ~0;
char       *cmd;
Bool	    no_props = False;	/* -noprops: don't show font properties */

static fsBitmapFormat make_format();
static Bool extents_only = False;

static void
usage()
{
    fprintf(stderr, "%s: [-server servername] [-extents_only] [-noprops] [-l] [-m] [-L] -[M] [-unit #] [-pad #] [-bitmap_pad value] [-start first_char] [-end last_char] -fn fontname\n", cmd);
    exit(0);
}

main(argc, argv)
    int         argc;
    char      **argv;
{
    char       *servername = NULL;
    char      **flist;
    int         fcount;
    char       *fontname = "xconq";
    int         i;
    Font        fid,
                dummy;
    fsBitmapFormat format;
    fsBitmapFormatMask fmask;
    fsChar2b    first,
                last;
    fsRange     range;
    fsFontHeader hdr;
    Bool        show_all = True;

    cmd = argv[0];

    for (i = 1; i < argc; i++) {
	if (!strncmp(argv[i], "-se", 3)) {
	    if (argv[++i])
		servername = argv[i];
	    else
		usage();
	} else if (!strncmp(argv[i], "-ext", 4)) {
	    extents_only = True;
	} else if (!strncmp(argv[i], "-noprops", 7)) {
	    no_props = True;
	} else if (!strncmp(argv[i], "-l", 2)) {
	    bitorder = LSBFirst;
	} else if (!strncmp(argv[i], "-m", 2)) {
	    bitorder = MSBFirst;
	} else if (!strncmp(argv[i], "-L", 2)) {
	    byteorder = LSBFirst;
	} else if (!strncmp(argv[i], "-M", 2)) {
	    byteorder = MSBFirst;
	} else if (!strncmp(argv[i], "-p", 2)) {
	    if (argv[++i])
		pad = atoi(argv[i]);
	    else
		usage();
	} else if (!strncmp(argv[i], "-u", 2)) {
	    if (argv[++i])
		scan_unit = atoi(argv[i]);
	    else
		usage();
	} else if (!strncmp(argv[i], "-b", 2)) {
	    if (argv[++i])
		bitmap_pad = atoi(argv[i]);
	    else
		usage();
	} else if (!strncmp(argv[i], "-st", 3)) {
	    if (argv[++i])
		first_ch = atoi(argv[i]);
	    else
		usage();
	} else if (!strncmp(argv[i], "-e", 2)) {
	    if (argv[++i])
		end_ch = atoi(argv[i]);
	    else
		usage();
	} else if (!strncmp(argv[i], "-f", 2)) {
	    if (argv[++i])
		fontname = argv[i];
	    else
		usage();
	} else
	    usage();
    }

    if (first_ch != 0 && end_ch != ~0 && end_ch < first_ch) {
	fprintf(stderr,
		"bad character range -- end (%d) is less than start (%d)\n",
		end_ch, first_ch);
	exit(-1);
    }
    if ((svr = FSOpenServer(servername)) == NULL) {
	fprintf(stderr, "can't open server \"%s\"\n", FSServerName(servername));
	exit(0);
    }
    format = make_format();
    fmask = (BitmapFormatMaskByte | BitmapFormatMaskBit |
	     BitmapFormatMaskImageRectangle | BitmapFormatMaskScanLinePad |
	     BitmapFormatMaskScanLineUnit);
    fid = FSOpenBitmapFont(svr, format, fmask, fontname, &dummy);
    if (fid) {
	printf("opened font %s\n", fontname);
	show_info(fid, &hdr, &first, &last);
	if (first_ch != 0 &&
		(first_ch >= (first.low + (first.high << 8)))) {
	    first.low = first_ch & 0xff;
	    first.high = first_ch >> 8;
	    show_all = False;
	}
	if (end_ch != ~0 &&
		(end_ch <= (last.low + (last.high << 8)))) {
	    last.low = end_ch & 0xff;
	    last.high = end_ch >> 8;
	    show_all = False;
	}
	/* make sure the range is legal */
	if ((first.high > last.high) || (first.high == last.high &&
					 first.low > last.low)) {
	    last = first;
	    fprintf(stderr,
		    "adjusting range -- specifed first char is after end\n");
	}
	show_glyphs(fid, &hdr, show_all, first, last);
	FSCloseFont(svr, fid);
    } else {
	printf("couldn't get font %s\n", fontname);
    }
    exit(0);
}


show_glyphs(fid, hdr, show_all, first, last)
    Font        fid;
    fsFontHeader *hdr;
    Bool        show_all;
    fsChar2b    first,
                last;
{
    fsCharInfo *extents;
    int         char_num;
    int         num_extents;
    int         err,
                ch,
                start,
                end;
    int         offset = 0;
    unsigned char *glyphs;
    fsOffset   *offsets;
    int         scanpad;
    int         r,
                b;
    fsBitmapFormat format;
    fsChar2b    chars[2];
    int         num_chars;

    if (show_all) {
	num_chars = 0;
    } else {
	chars[0] = first;
	chars[1] = last;
	num_chars = 2;
    }
    FSQueryXExtents16(svr, fid, True, chars, num_chars, &extents);

    if (!extents_only) {
	format = make_format();
	err = FSQueryXBitmaps16(svr, fid, format, True, chars, num_chars,
				&offsets, &glyphs);

	if (err != FSSuccess) {
	    fprintf(stderr, "QueryGlyphs failed\n");
	    exit(0);
	}
    }
    start = first.low + (first.high << 8);
    end = last.low + (last.high << 8);

    scanpad = pad >> 3;

    for (ch = 0; ch <= (end - start); ch++) {
	int         bottom,
	            bpr,
	            charwidth;

	printf("char #%d", ch + start);
	if (isprint(ch + start))
	    printf(" '%c'\n", (char) (ch + start));
	else
	    printf(" '\\%03o'\n", (ch + start)&0377);
	show_char_info(&extents[ch]);
	if (extents_only)
	    continue;
	if (offset != offsets[ch].position)
	    fprintf(stderr, "offset mismatch: expected %d, got %d\n",
		    offset, offsets[ch].position);
	switch (bitmap_format) {
	case BitmapFormatImageRectMin:
	    bottom = extents[ch].descent + extents[ch].ascent;
	    charwidth = extents[ch].right - extents[ch].left;
	    break;
	case BitmapFormatImageRectMaxWidth:
	    bottom = extents[ch].descent + extents[ch].ascent;
	    charwidth = hdr->max_bounds.right - hdr->min_bounds.left;
	    break;
	case BitmapFormatImageRectMax:
	    bottom = hdr->max_bounds.ascent +
		hdr->max_bounds.descent;
	    charwidth = hdr->max_bounds.right - hdr->min_bounds.left;
	    break;
	}

	if (extents[ch].left == 0 &&
	    extents[ch].right == 0 &&
	    extents[ch].width == 0 &&
	    extents[ch].ascent == 0 &&
	    extents[ch].descent == 0)
	{
	    printf ("Nonexistent character\n");
	    continue;
	}
	bpr = GLWIDTHBYTESPADDED(charwidth, scanpad);
	if (offsets[ch].length != bottom * bpr) {
	    fprintf (stderr, "length mismatch: expected %d (%dx%d), got %d\n",
			 bottom * bpr, bpr, bottom, offsets[ch].length);
	}
	offset = offsets[ch].position;
	for (r = 0; r < bottom; r++) {
	    unsigned char *row = glyphs + offset;

	    for (b = 0; b < charwidth; b++) {
		putchar((row[b >> 3] &
			 (1 << (7 - (b & 7)))) ? '#' : '-');
	    }
	    putchar('\n');
	    offset += bpr;
	}
    }
    FSFree((char *) extents);
    if (!extents_only) {
	FSFree((char *) offsets);
	FSFree((char *) glyphs);
    }
}

show_char_info(ci)
    fsCharInfo *ci;
{
    printf("Left: %-3d    Right: %-3d    Ascent: %-3d    Descent: %-3d    Width: %d\n",
	   ci->left, ci->right, ci->ascent, ci->descent, ci->width);
}

show_info(fid, hdr, first, last)
    Font        fid;
    fsFontHeader *hdr;
    fsChar2b   *first,
               *last;
{
    fsPropInfo  pi;
    fsPropOffset *po;
    unsigned char *pd;

    FSQueryXInfo(svr, fid, hdr, &pi, &po, &pd);
    printf("Direction: %s\n", (hdr->draw_direction == LeftToRightDrawDirection)
	   ? "Left to Right" : "Right to Left");
    *first = hdr->char_range.min_char;
    *last = hdr->char_range.max_char;
    printf("Range:	%d to %d\n",
	   first->low + (first->high << 8),
	   last->low + (last->high << 8));
    if (hdr->flags & FontInfoAllCharsExist)
	printf("All chars exist\n");
    printf("Default char: %d\n",
	   hdr->default_char.low + (hdr->default_char.high << 8));
    printf("Min bounds: \n");
    show_char_info(&hdr->min_bounds);
    printf("Max bounds: \n");
    show_char_info(&hdr->max_bounds);
    printf("Font Ascent: %d  Font Descent: %d\n",
	   hdr->font_ascent, hdr->font_descent);

    if (!no_props)
	show_props(&pi, po, pd);
    FSFree((char *) po);
    FSFree((char *) pd);
}

show_props(pi, po, pd)
    fsPropInfo *pi;
    fsPropOffset *po;
    unsigned char *pd;
{
    int         i;
    char        buf[512];
    int         num_props;

    num_props = pi->num_offsets;
    for (i = 0; i < num_props; i++, po++) {
	strncpy(buf, (char *) (pd + po->name.position), po->name.length);
	buf[po->name.length] = '\0';
	printf("%s\t", buf);
	switch (po->type) {
	case PropTypeString:
	    strncpy(buf, pd + po->value.position, po->value.length);
	    buf[po->value.length] = '\0';
	    printf("%s\n", buf);
	    break;
	case PropTypeUnsigned:
	    printf("%d\n", (unsigned long) po->value.position);
	    break;
	case PropTypeSigned:
	    printf("%d\n", (long) po->value.position);
	    break;
	default:
	    fprintf(stderr, "bogus property\n");
	    break;
	}
    }
}

static      fsBitmapFormat
make_format()
{
    fsBitmapFormat format;

    format = 0;
    /* set up format */
    switch (pad) {
    case 8:
	format |= BitmapFormatScanlinePad8;
	break;
    case 16:
	format |= BitmapFormatScanlinePad16;
	break;
    case 32:
	format |= BitmapFormatScanlinePad32;
	break;
    default:
	fprintf(stderr, "bogus scanline pad value: %d\n", pad);
	break;
    }
    switch (scan_unit) {
    case 8:
	format |= BitmapFormatScanlineUnit8;
	break;
    case 16:
	format |= BitmapFormatScanlineUnit16;
	break;
    case 32:
	format |= BitmapFormatScanlineUnit32;
	break;
    default:
	fprintf(stderr, "bogus scanline unit value: %d\n", scan_unit);
	break;
    }
    switch (bitmap_pad) {
    case 0:
	bitmap_format = BitmapFormatImageRectMin;
	break;
    case 1:
	bitmap_format = BitmapFormatImageRectMaxWidth;
	break;
    case 2:
	bitmap_format = BitmapFormatImageRectMax;
	break;
    default:
	fprintf(stderr, "bogus bitmap pad value: %d\n", bitmap_pad);
	break;
    }
    format |= bitmap_format;

    format |= (bitorder == MSBFirst) ? BitmapFormatBitOrderMSB :
	BitmapFormatBitOrderLSB;
    format |= (byteorder == MSBFirst) ? BitmapFormatByteOrderMSB :
	BitmapFormatByteOrderLSB;

    return format;
}
