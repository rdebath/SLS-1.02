/* $XConsortium: fstobdf.c,v 1.3 91/07/17 17:09:38 rws Exp $ */
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
 * @(#)fstobdf.c	4.1	91/05/02
 *
 */

#include	<stdio.h>
#include	"FSlib.h"

extern Bool EmitHeader();
extern Bool EmitProperties();
extern Bool EmitCharacters();
extern Bool EmitTrailer();

static void
usage(progName)
    char       *progName;
{
    fprintf(stderr, "Usage: %s [-s <font server>] -fn <font name>\n",
	    progName);
    exit(0);
}

main(argc, argv)
    int         argc;
    char      **argv;
{
    FSServer   *fontServer;
    Font        fontID,
                dummy;
    fsBitmapFormat bitmapFormat;
    fsFontHeader fontHeader;
    fsPropInfo  propInfo;
    fsPropOffset *propOffsets;
    unsigned char *propData;

    FILE       *outFile;
    char       *fontName;
    char       *serverName;
    int         i;

    fontName = NULL;
    serverName = NULL;
    outFile = stdout;

    for (i = 1; i < argc; i++) {
	if (!strncmp(argv[i], "-s", 2)) {
	    if (argv[++i])
		serverName = argv[i];
	    else
		usage(argv[0]);
	} else if (!strncmp(argv[i], "-fn", 3)) {
	    if (argv[++i])
		fontName = argv[i];
	    else
		usage(argv[0]);
	}
    }

    if (fontName == NULL)
	usage(argv[0]);

    fontServer = FSOpenServer(serverName);
    if (!fontServer) {
	fprintf(stderr, "can't open font server \"%s\"\n",
		FSServerName(serverName));
	exit(0);
    }
    bitmapFormat = 0;
    fontID = FSOpenBitmapFont(fontServer, bitmapFormat, (fsBitmapFormatMask) 0,
			      fontName, &dummy);
    if (!fontID) {
	printf("can't open font \"%s\"\n", fontName);
	exit(0);
    }
    FSQueryXInfo(fontServer, fontID, &fontHeader, &propInfo, &propOffsets,
		 &propData);

    if (!EmitHeader(outFile, &fontHeader, &propInfo, propOffsets, propData))
	Fail(argv[0]);
    if (!EmitProperties(outFile, &fontHeader, &propInfo, propOffsets, propData))
	Fail(argv[0]);
    if (!EmitCharacters(outFile, fontServer, &fontHeader, fontID))
	Fail(argv[0]);
    fprintf(outFile, "ENDFONT\n");

    FSFree((char *) propOffsets);
    FSFree((char *) propData);
}

Fail(progName)
    char       *progName;
{
    fprintf(stderr, "%s: unable to dump font\n", progName);
    exit(1);
}
