/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
/* $XConsortium: fonttype.h,v 1.4 90/11/07 18:01:13 keith Exp $ */

#ifndef FONTTYPE_H
#define FONTTYPE_H

#include "font.h"
#include "misc.h"

/*
 * Parameterize the procedures for internalizing a font dependent upon the
 * extension of the file name. This is somewhat OS dependent, but most
 * OSs support this kind of thing.
 */
typedef FontPtr (*ReadFontProc)(/* FID */);
typedef Bool (*ReadPropertiesProc)(/* FID, FontInfoPtr, DIXFontPropPtr */);
typedef void (*FreeFontProc)(/* FontPtr */);
typedef int  (*FontFileReadFunc) ();
typedef int  (*FontFileSkipFunc) ();
typedef FID  (*FontFileInitFunc) ();
typedef int  (*FontFileDoneFunc) ();

typedef struct _FontFID {
    FID			fid;
    int			type;
} FontFIDRec, *FontFIDPtr;

typedef struct _FontFileReader {
    char *		extension;
    ReadFontProc	loadFont;
    ReadPropertiesProc	loadProperties;
    FreeFontProc	freeFont;
    char **		filter;
    FontFileReadFunc	read;
    FontFileSkipFunc	skip;
    FontFileInitFunc	init;
    FontFileDoneFunc	done;
} FontFileReaderRec, *FontFileReader;

extern FontFileReaderRec fontFileReaders[];



#endif /* FONTTYPE_H */
