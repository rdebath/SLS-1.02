/***********************************************************************
Copyright 1991 by Apple Computer, Inc, Cupertino, California
			All Rights Reserved

Permission to use, copy, modify, and distribute this software
for any purpose and without fee is hereby granted, provided
that the above copyright notice appear in all copies.

APPLE MAKES NO WARRANTY OR REPRESENTATION, EITHER EXPRESS,
OR IMPLIED, WITH RESPECT TO THIS SOFTWARE, ITS QUALITY,
PERFORMANCE, MERCHANABILITY, OR FITNESS FOR A PARTICULAR
PURPOSE. AS A RESULT, THIS SOFTWARE IS PROVIDED "AS IS,"
AND YOU THE USER ARE ASSUMING THE ENTIRE RISK AS TO ITS
QUALITY AND PERFORMANCE. IN NO EVENT WILL APPLE BE LIABLE 
FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL
DAMAGES RESULTING FROM ANY DEFECT IN THE SOFTWARE.

THE WARRANTY AND REMEDIES SET FORTH ABOVE ARE EXCLUSIVE
AND IN LIEU OF ALL OTHERS, ORAL OR WRITTEN, EXPRESS OR
IMPLIED.

***********************************************************************/
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
 *
 */

#include 	"MacFont.h"
#include	"fontfilest.h"
#define APPLE	"Apple Computer, Inc."

enum scaleType {
    atom, pixel_size, point_size, resolution_x, resolution_y, average_width,
    scaledX, scaledY, unscaled, scaledXoverY, uncomputed,
	weight, x_height, quad_width
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
	"WEIGHT", 0, weight,
	"X_HEIGHT", 0, x_height,
	"QUAD_WIDTH", 0, quad_width,
};

/* this is a bit kludgy */
#define	FONTPROP	0
#define	COPYRIGHTPROP	1
#define WEIGHTPROP	2
#define X_HEIGHTPROP	3
#define QUAD_WIDTHPROP	4

#define NNAMEPROPS (sizeof(fontNamePropTable) / sizeof(fontProp))
#define NEXTRAPROPS (sizeof(extraProps) / sizeof(fontProp))

#define	NPROPS	(NNAMEPROPS + NEXTRAPROPS)


void
MacFontStandardProps()
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
MacFontComputedProps(fontname, vals, pinfo, pMacPriv)
    char       *fontname;
    FontScalablePtr vals;
    FontInfoPtr pinfo;
	MacBitmapFontRecPtr pMacPriv;
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
	    pp->value = vals -> pixel;
	    break;
	case point_size:
	    pp->value = vals -> point;
	    break;
	case resolution_x:
	    pp->value = vals -> x;
	    break;
	case resolution_y:
	    pp->value = vals -> y;
	    break;
	case average_width:
	    pp->value = pMacPriv -> avgWidth;
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
	    pp->value = MakeAtom(APPLE, strlen(APPLE), TRUE);
	    break;
	case WEIGHTPROP:
	    pp->value = pMacPriv->weight;
	    break;
	case X_HEIGHTPROP:
	    pp->value = pMacPriv->xHeight;
	    break;
	case QUAD_WIDTHPROP:
	    pp->value = pMacPriv->quadWidth;
	}
    }
}

