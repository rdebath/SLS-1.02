/* $XConsortium: header.c,v 1.2 91/05/13 16:35:53 gildea Exp $ */
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
 * @(#)header.c	4.1	91/05/02
 *
 */

#include	<stdio.h>
#include	"FSlib.h"

long        pointSize;
long        yResolution;

static char *warning[] =
{
    "COMMENT  ",
    "COMMENT  WARNING:  This bdf file was generated from a font server using",
    "COMMENT  fstobdf.  The resulting font is subject to the same copyright,",
    "COMMENT  license, and trademark restrictions as the original font.  The",
    "COMMENT  authors and distributors of fstobdf disclaim all liability for",
    "COMMENT  misuse of the program or its output.",
    "COMMENT  ",
    NULL
};

static char *
FindStringProperty(propName, propLength, propInfo, propOffsets, propData)
    char       *propName;
    int        *propLength;
    fsPropInfo *propInfo;
    fsPropOffset *propOffsets;
    unsigned char *propData;
{
    fsPropOffset *propOffset;
    int         length;
    int         i;

    propOffset = &propOffsets[0];
    length = strlen(propName);
    for (i = propInfo->num_offsets; i--; propOffset++) {
	if (propOffset->type == PropTypeString) {

#ifdef DEBUG
	    char        pname[256];

	    bcopy(propData + propOffset->name.position, pname,
		  propOffset->name.length);
	    pname[propOffset->name.length] = '\0';
	    fprintf(stderr, "prop name: %s (len %d)\n",
		    pname, propOffset->name.length);
#endif

	    if ((propOffset->name.length == length) &&
		    !strncmp(propData + propOffset->name.position, propName, length)) {
		*propLength = propOffset->value.length;
		return (char *)(propData + propOffset->value.position);
	    }
	}
    }
    *propLength = 0;
    return (NULL);
}

static int
FindNumberProperty(propName, propValue, propInfo, propOffsets, propData)
    char       *propName;
    int        *propValue;
    fsPropInfo *propInfo;
    fsPropOffset *propOffsets;
    unsigned char *propData;
{
    fsPropOffset *propOffset;
    int         i;
    int         length;

    propOffset = &propOffsets[0];
    length = strlen(propName);
    for (i = propInfo->num_offsets; i--; propOffset++) {
	if ((propOffset->type == PropTypeSigned) ||
		(propOffset->type == PropTypeUnsigned)) {
	    if ((propOffset->name.length == length) &&
		    !strncmp(propData + propOffset->name.position, propName, length)) {
		*propValue = propOffset->value.position;
		return (propOffset->type);
	    }
	}
    }
    return (-1);
}

/*
 * EmitHeader - print STARTFONT, COMMENT lines, FONT, SIZE, and
 * FONTBOUNDINGBOX lines
 */
Bool
EmitHeader(outFile, fontHeader, propInfo, propOffsets, propData)
    FILE       *outFile;
    fsFontHeader *fontHeader;
    fsPropInfo *propInfo;
    fsPropOffset *propOffsets;
    unsigned char *propData;
{
    int         len;
    int         type;
    char       *cp;
    char      **cpp;
    unsigned long xResolution;

    fprintf(outFile, "STARTFONT 2.1\n");

    /*
     * find COPYRIGHT message and print it first, followed by warning
     */
    cp = FindStringProperty("COPYRIGHT", &len, propInfo, propOffsets,
			    propData);
    if (cp) {
	fprintf(outFile, "COMMENT  \nCOMMENT  ");
	fwrite(cp, 1, len, outFile);
	fputc('\n', outFile);
    }
    for (cpp = warning; *cpp; cpp++)
	fprintf(outFile, "%s\n", *cpp);

    /*
     * FONT name
     */
    cp = FindStringProperty("FONT", &len, propInfo, propOffsets, propData);
    if (cp) {
	fprintf(outFile, "FONT ");
	fwrite(cp, 1, len, outFile);
	fputc('\n', outFile);
    } else {
	fprintf(stderr, "unable to find FONT property\n");
	return (False);
    }

    /*
     * SIZE point xres yres
     * 
     * Get XLFD values if possible, else fake it
     */
    type = FindNumberProperty("RESOLUTION_X", &xResolution, propInfo,
			      propOffsets, propData);
    if ((type != PropTypeUnsigned) && (type != PropTypeSigned))
	xResolution = 72;

    type = FindNumberProperty("RESOLUTION_Y", &yResolution, propInfo,
			      propOffsets, propData);
    if ((type != PropTypeUnsigned) && (type != PropTypeSigned))
	yResolution = 72;

    type = FindNumberProperty("POINT_SIZE", &pointSize, propInfo,
			      propOffsets, propData);
    if ((type == PropTypeUnsigned) || (type == PropTypeSigned))
	pointSize = (pointSize + 5) / 10;
    else
	pointSize = ((fontHeader->font_ascent + fontHeader->font_descent)
		     * 72) / yResolution;

    fprintf(outFile, "SIZE %d %d %d\n", pointSize, xResolution, yResolution);

    /*
     * FONTBOUNDINGBOX width height xoff yoff
     * 
     */
    fprintf(outFile, "FONTBOUNDINGBOX %d %d %d %d\n",
	    fontHeader->max_bounds.right - fontHeader->min_bounds.left,
	    fontHeader->max_bounds.ascent + fontHeader->max_bounds.descent,
	    fontHeader->min_bounds.left,
	    -fontHeader->max_bounds.descent);
    return (True);
}
