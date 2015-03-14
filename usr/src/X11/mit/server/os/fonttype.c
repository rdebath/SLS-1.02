/************************************************************************
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

************************************************************************/

/* $XConsortium: fonttype.c,v 1.6 91/01/08 12:18:54 rws Exp $ */

#include "dixfont.h"
#include "fonttype.h"

#ifdef FONT_SNF
#ifdef COMPRESSED_FONTS

extern int	CompressedFontFileRead ();
extern int	CompressedFontFileSkip ();
extern FID	CompressedFontFileInit ();
extern int	CompressedFontFileDone();

#endif
#endif

#ifndef FCFLAGS
#define FCFLAGS "-t"
#endif
#ifndef BDFTOSNFFILT
#define BDFTOSNFFILT "/usr/bin/X11/bdftosnf"
#endif
#ifndef SHELLPATH
#define SHELLPATH "/bin/sh"
#endif

extern FontPtr	ReadSNFFont();
extern Bool	ReadSNFProperties();
extern void	FreeSNFFont();

#ifdef FONT_BDF
static char *
bdfFilter[] = {BDFTOSNFFILT, FCFLAGS, NULL};
#ifdef ZBDFTOSNFFILT
static char *
bdfZFilter[] = {SHELLPATH, "-c", ZBDFTOSNFFILT, NULL};
#endif
#endif

FontFileReaderRec fontFileReaders[] = {
#ifdef FONT_SNF
    {".snf", ReadSNFFont, ReadSNFProperties, FreeSNFFont, (char **)NULL},
#ifdef COMPRESSED_FONTS
    {".snf.Z", ReadSNFFont, ReadSNFProperties, FreeSNFFont, (char **)NULL,
	CompressedFontFileRead, CompressedFontFileSkip,
 	CompressedFontFileInit, CompressedFontFileDone, },
#endif
#endif
#ifdef FONT_BDF
    {".bdf", ReadSNFFont, ReadSNFProperties, FreeSNFFont, bdfFilter},
#ifdef ZBDFTOSNFFILT
    {".bdf.Z", ReadSNFFont, ReadSNFProperties, FreeSNFFont, bdfZFilter},
#endif
#endif
    NULL
};

