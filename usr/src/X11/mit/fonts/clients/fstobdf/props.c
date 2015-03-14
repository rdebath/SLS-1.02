/* $XConsortium: props.c,v 1.2 91/05/13 16:36:00 gildea Exp $ */
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
 * @(#)props.c	4.1	91/05/02
 *
 */

#include	<stdio.h>
#include	"FSlib.h"

static char *
AddQuotes(string, length)
    unsigned char *string;
    int         length;
{
    static unsigned char new[256] = "\"";
    unsigned char *cp;
    unsigned char *end;

    end = string + length;
    for (cp = &new[1]; string < end; cp++, string++) {
	*cp = *string;
	if (*cp == '"')
	    *++cp = '"';
    }
    *cp++ = '"';
    *cp = '\0';
    return (char *)(new);
}

Bool
EmitProperties(outFile, fontHeader, propInfo, propOffsets, propData)
    FILE       *outFile;
    fsFontHeader *fontHeader;
    fsPropInfo *propInfo;
    fsPropOffset *propOffsets;
    unsigned char *propData;
{
    int         nProperties;
    fsPropOffset *property;
    Bool        needDefaultChar;
    Bool        needFontAscent;
    Bool        needFontDescent;

    needDefaultChar = True;
    needFontAscent = True;
    needFontDescent = True;

    nProperties = propInfo->num_offsets;
    for (property = &propOffsets[0]; nProperties--; property++) {
	unsigned char *name;
	int         length;

	name = propData + property->name.position;
	length = property->name.length;

	if ((length == 12) && (!strncmp(name, "DEFAULT_CHAR", 12)))
	    needDefaultChar = False;
	else if ((length == 11) && (!strncmp(name, "FONT_ASCENT", 11)))
	    needFontAscent = False;
	else if ((length == 12) && (!strncmp(name, "FONT_DESCENT", 12)))
	    needFontDescent = False;
    }

    nProperties = propInfo->num_offsets;
    fprintf(outFile, "STARTPROPERTIES %d\n", nProperties +
	    (needDefaultChar ? 1 : 0) + (needFontAscent ? 1 : 0) +
	    (needFontDescent ? 1 : 0));

    for (property = &propOffsets[0]; nProperties--; property++) {
	unsigned long value;

	/* Don't emit properties that are computed by bdftosnf */

	fwrite(propData + property->name.position, 1, property->name.length,
	       outFile);
	fputc(' ', outFile);

	value = property->value.position;
	switch (property->type) {
	case PropTypeString:
	    fprintf(outFile, "%s\n", AddQuotes(propData + value,
					       property->value.length));
	    break;
	case PropTypeUnsigned:
	    fprintf(outFile, "%lu\n", value);
	    break;
	case PropTypeSigned:
	    fprintf(outFile, "%ld\n", value);
	    break;
	default:
	    fprintf(stderr, "unknown property type\n");
	    return (False);
	}
    }
    if (needDefaultChar) {
	fsChar2b   *def;

	def = &fontHeader->default_char;
	fprintf(outFile, "DEFAULT_CHAR %lu\n",
		(long) (def->high << 8) | (def->low));
    }
    if (needFontAscent)
	fprintf(outFile, "FONT_ASCENT %d\n", fontHeader->font_ascent);
    if (needFontDescent)
	fprintf(outFile, "FONT_DESCENT %d\n", fontHeader->font_descent);
    fprintf(outFile, "ENDPROPERTIES\n");
    return (True);
}
