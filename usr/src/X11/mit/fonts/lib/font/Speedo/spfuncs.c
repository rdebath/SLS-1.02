/* $XConsortium: spfuncs.c,v 1.7 92/04/15 14:35:48 gildea Exp $ */
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
 * Author: Dave Lemke, Network Computing Devices, Inc
 */

#include	<X11/Xos.h>
#include	"fontfilest.h"
#include	"spint.h"

/* ARGSUSED */
SpeedoOpenScalable (fpe, pFont, flags, entry, fileName, vals, format, fmask)
    FontPathElementPtr	fpe;
    FontPtr		*pFont;
    int			flags;
    FontEntryPtr	entry;
    char		*fileName;
    FontScalablePtr	vals;
    fsBitmapFormat	format;
    fsBitmapFormatMask	fmask;
{
    char	fullName[MAXFONTNAMELEN];

    strcpy (fullName, entry->name.name);
    FontParseXLFDName (fullName, vals, FONT_XLFD_REPLACE_VALUE);
    return SpeedoFontLoad (pFont, fullName, fileName, entry,
			    format, fmask, flags);
}

/*
 * XXX
 *
 * this does a lot more then i'd like, but it has to get the bitmaps
 * in order to get accurate metrics (which it *must* have).
 *
 * a possible optimization is to avoid allocating the glyph memory
 * and to simply save the values without doing the work.
 */
static int
get_font_info(pinfo, fontname, filename, entry, spfont)
    FontInfoPtr pinfo;
    char       *fontname;
    char       *filename;
    FontEntryPtr	entry;
    SpeedoFontPtr *spfont;
{
    SpeedoFontPtr spf;
    int         err;

    err = open_sp_font(fontname, filename, entry,
	       (fsBitmapFormat) 0, (fsBitmapFormatMask) 0, (unsigned long) 0,
		       &spf);

    if (err != Successful)
	return err;

    cur_spf = spf;
    sp_reset_master(spf->master);

    make_sp_header(spf, pinfo);

    compute_sp_bounds(spf, pinfo, (unsigned long) 0);

    compute_sp_props(spf, fontname, pinfo);

    /* compute remaining accelerators */
    FontComputeInfoAccelerators (pinfo);

    *spfont = spf;

    return Successful;
}

/* ARGSUSED */
SpeedoGetInfoScaleable(fpe, pFontInfo, entry, fontName, fileName, vals)
    FontPathElementPtr	fpe;
    FontInfoPtr		pFontInfo;
    FontEntryPtr	entry;
    FontNamePtr		fontName;
    char		*fileName;
    FontScalablePtr	vals;
{
    SpeedoFontPtr spf = NULL;
    char        fullName[MAXFONTNAMELEN];
    int         err;

    fixup_vals(vals);

    strcpy(fullName, entry->name.name);
    FontParseXLFDName(fullName, vals, FONT_XLFD_REPLACE_VALUE);

    err = get_font_info(pFontInfo, fullName, fileName, entry, &spf);

    if (spf)
	close_sp_font(spf);

    return err;
}

static FontRendererRec renderer = {
    ".spd", 4, (int (*)()) 0, SpeedoOpenScalable,
	(int (*)()) 0, SpeedoGetInfoScaleable, 0
};
    
SpeedoRegisterFontFileFunctions()
{
    make_sp_standard_props();
    sp_reset();
    FontFileRegisterRenderer(&renderer);
}
