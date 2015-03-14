/* $XConsortium: spinfo.c,v 1.7 91/09/16 11:42:32 keith Exp $ */
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
 * $NCDId: @(#)spinfo.c,v 4.6 1991/06/24 16:55:09 lemke Exp $
 *
 * Author: Dave Lemke, Network Computing Devices, Inc
 *
 */

#include	"fontfilest.h"
#include	"spint.h"

/* percentage of pointsize used to specify ascent & descent */
#define	STRETCH_FACTOR	120

enum scaleType {
    atom, pixel_size, point_size, resolution_x, resolution_y, average_width,
    scaledX, scaledY, unscaled, scaledXoverY, uncomputed
};

typedef struct _fontProp {
    char       *name;
    long        atom;
    enum scaleType type;
}           fontProp;

static fontProp fontNamePropTable[] = {
    "FOUNDRY", 0, atom,
    "FAMILY_NAME", 0, atom,
    "WEIGHT_NAME", 0, atom,
    "SLANT", 0, atom,
    "SETWIDTH_NAME", 0, atom,
    "ADD_STYLE_NAME", 0, atom,
    "PIXEL_SIZE", 0, pixel_size,
    "POINT_SIZE", 0, point_size,
    "RESOLUTION_X", 0, resolution_x,
    "RESOLUTION_Y", 0, resolution_y,
    "SPACING", 0, atom,
    "AVERAGE_WIDTH", 0, average_width,
    "CHARSET_REGISTRY", 0, atom,
    "CHARSET_ENCODING", 0, atom,
};

static fontProp extraProps[] = {
    "FONT", 0, atom,
    "COPYRIGHT", 0, atom,
};

/* this is a bit kludgy */
#define	FONTPROP	0
#define	COPYRIGHTPROP	1

#define NNAMEPROPS (sizeof(fontNamePropTable) / sizeof(fontProp))
#define NEXTRAPROPS (sizeof(extraProps) / sizeof(fontProp))

#define	NPROPS	(NNAMEPROPS + NEXTRAPROPS)

void
make_sp_standard_props()
{
    int         i;
    fontProp   *t;

    i = sizeof(fontNamePropTable) / sizeof(fontProp);
    for (t = fontNamePropTable; i; i--, t++)
	t->atom = MakeAtom(t->name, (unsigned) strlen(t->name), TRUE);
    i = sizeof(extraProps) / sizeof(fontProp);
    for (t = extraProps; i; i--, t++)
	t->atom = MakeAtom(t->name, (unsigned) strlen(t->name), TRUE);
}

void
make_sp_header(spf, pinfo)
    SpeedoFontPtr spf;
    FontInfoPtr pinfo;
{
    int         pixel_size;
    SpeedoMasterFontPtr spmf = spf->master;

    pinfo->firstCol = spmf->first_char_id & 0xff;
    pinfo->firstRow = spmf->first_char_id >> 8;
    pinfo->lastCol = spmf->max_id & 0xff;
    pinfo->lastRow = spmf->max_id >> 8;

    /* XXX -- hackery here */
    pinfo->defaultCh = 0;
/* computed by FontComputeInfoAccelerators:
 *  noOverlap
 *  constantMetrics
 *  terminalFont
 *  constantWidth
 *  inkInside
 */
    pinfo->inkMetrics = 0;
    pinfo->allExist = 0;
    pinfo->drawDirection = LeftToRight;
    pinfo->cachable = 1;
    pinfo->anamorphic = 0;
    if (spf->specs.xxmult != spf->specs.yymult)
	pinfo->anamorphic = TRUE;
/* computed by compute_sp_bounds:
 *  maxOverlap
 *  maxbounds
 *  minbounds
 *  ink_maxbounds
 *  ink_minbounds
 */
    pixel_size = spf->vals.pixel * STRETCH_FACTOR / 100;
    pinfo->fontAscent = pixel_size * 764 / 1000;	/* 764 == EM_TOP */
    pinfo->fontDescent = pixel_size - pinfo->fontAscent;
}

static void
adjust_min_max(minc, maxc, tmp)
    xCharInfo  *minc,
               *maxc,
               *tmp;
{
#define MINMAX(field,ci) \
	if (minc->field > (ci)->field) \
	     minc->field = (ci)->field; \
	if (maxc->field < (ci)->field) \
	    maxc->field = (ci)->field;

    MINMAX(ascent, tmp);
    MINMAX(descent, tmp);
    MINMAX(leftSideBearing, tmp);
    MINMAX(rightSideBearing, tmp);
    MINMAX(characterWidth, tmp);

#undef	MINMAX
}


void
compute_sp_bounds(spf, pinfo, flags)
    SpeedoFontPtr spf;
    FontInfoPtr pinfo;
    unsigned long flags;
{
    int         i,
                id,
                index,
		maxOverlap,
		overlap;
    xCharInfo   minchar,
                maxchar,
                tmpchar;
    bbox_t      bbox;
    fix31       width;
    double      pix_width,
                total_width = 0.0;
    SpeedoMasterFontPtr spmf = spf->master;
    int	firstChar;

    firstChar = spmf->first_char_id;
    minchar.ascent = minchar.descent =
	minchar.leftSideBearing = minchar.rightSideBearing =
	minchar.characterWidth = 32767;
    minchar.attributes = 0;
    maxchar.ascent = maxchar.descent =
	maxchar.leftSideBearing = maxchar.rightSideBearing =
	maxchar.characterWidth = -32767;
    maxchar.attributes = 0;
    maxOverlap = -32767;
    for (i = 0; i < spmf->num_chars; i++) {
	index = spmf->enc[i * 2 + 1];
	width = sp_get_char_width(index);

	/* convert to pixel coords */
	pix_width = width * (spf->specs.xxmult / 65536L) +
	    ((ufix32) width * ((ufix32) spf->specs.xxmult & 0xffff))
	    / 65536L;
	width = (pix_width * 720L) / (spf->specs.yymult);
	pix_width /= 65536L;

	(void) sp_get_char_bbox(index, &bbox);
	bbox.ymax = (bbox.ymax + 32768L) >> 16;
	bbox.ymin = (bbox.ymin + 32768L) >> 16;
	bbox.xmin = (bbox.xmin + 32768L) >> 16;
	bbox.xmax = (bbox.xmax + 32768L) >> 16;
	tmpchar.ascent = bbox.ymax;
	tmpchar.descent = -bbox.ymin;
	tmpchar.leftSideBearing = bbox.xmin;
	tmpchar.rightSideBearing = bbox.xmax;
	tmpchar.characterWidth = (int) (pix_width + 0.5);	/* round */
	tmpchar.attributes = 0;
	adjust_min_max(&minchar, &maxchar, &tmpchar);
	overlap = tmpchar.rightSideBearing - tmpchar.characterWidth;
	if (maxOverlap < overlap)
	    maxOverlap = overlap;

	total_width += pix_width;

	if (flags & SaveMetrics) {
	    id = spmf->enc[i * 2] - firstChar;
	    assert(id <= spmf->max_id - firstChar);
	    spf->encoding[id].metrics = tmpchar;
	}
    }


    spf->vals.width = total_width / spmf->num_chars * 10;
    pinfo->maxbounds = maxchar;
    pinfo->minbounds = minchar;
    pinfo->ink_maxbounds = maxchar;
    pinfo->ink_minbounds = minchar;
    pinfo->maxOverlap = maxOverlap;
}

void
compute_sp_props(spf, fontname, pinfo)
    SpeedoFontPtr spf;
    char       *fontname;
    FontInfoPtr pinfo;
{
    FontPropPtr pp;
    int         i,
                nprops;
    fontProp   *fpt;
    char       *is_str;
    char       *ptr1,
               *ptr2;

    nprops = pinfo->nprops = NPROPS;
    pinfo->isStringProp = (char *) xalloc(sizeof(char) * nprops);
    pinfo->props = (FontPropPtr) xalloc(sizeof(FontPropRec) * nprops);
    if (!pinfo->isStringProp || !pinfo->props) {
	xfree(pinfo->isStringProp);
	pinfo->isStringProp = (char *) 0;
	xfree(pinfo->props);
	pinfo->props = (FontPropPtr) 0;
	return;
    }
    bzero(pinfo->isStringProp, (sizeof(char) * nprops));

    ptr2 = fontname;
    for (i = NNAMEPROPS, pp = pinfo->props, fpt = fontNamePropTable,
	    is_str = pinfo->isStringProp;
	    i;
	    i--, pp++, fpt++, is_str++) {
	ptr1 = ptr2 + 1;
	if (*ptr1 == '-')
	    ptr2 = ptr1;
	else {
	    if (i > 1)
		ptr2 = index(ptr1 + 1, '-');
	    else
		ptr2 = index(ptr1 + 1, '\0');
	}
	pp->name = fpt->atom;
	switch (fpt->type) {
	case atom:
	    *is_str = TRUE;
	    pp->value = MakeAtom(ptr1, ptr2 - ptr1, TRUE);
	    break;
	case pixel_size:
	    pp->value = spf->vals.pixel;
	    break;
	case point_size:
	    pp->value = spf->vals.point;
	    break;
	case resolution_x:
	    pp->value = spf->vals.x;
	    break;
	case resolution_y:
	    pp->value = spf->vals.y;
	    break;
	case average_width:
	    pp->value = spf->vals.width;
	    break;
	}
    }

    for (i = 0, fpt = extraProps; i < NEXTRAPROPS; i++, is_str++, pp++, fpt++) {
	pp->name = fpt->atom;
	switch (i) {
	case FONTPROP:
	    *is_str = TRUE;
	    pp->value = MakeAtom(fontname, strlen(fontname), TRUE);
	    break;
	case COPYRIGHTPROP:
	    *is_str = TRUE;
	    pp->value = MakeAtom(spf->master->copyright,
				 strlen(spf->master->copyright), TRUE);
	    break;
	}
    }
}
