/***********************************************************
Copyright 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
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

/* $XConsortium: fontdir.h,v 1.7 89/03/10 17:40:09 rws Exp $ */

#define True 1
#define False 0
typedef int Boolean;

#define NUL '\0'

#define FontDirFile "fonts.dir"
#define AliasFile "fonts.alias"
#define NoMatch -1

typedef struct _No_Such_Struct_ *Opaque;

typedef struct _FontFile {
    char		*name;
    Opaque		private;
    Boolean		alias;
} FontFileRec, *FontFile;
#define NullFontFile ((FontFile) NULL)

typedef struct _FontName {
    char		*name;
    union {
	int		index;		/* into FileEntry vector */
	FontFile	ff;
    } u;
} FontNameRec, *FontName;
#define NullFontName ((FontName) NULL)

typedef struct _FileEntry {
    int			used;
    int			size;
    FontFile		ff;
} FileEntry;

typedef struct _NameEntry {
    int			used;
    int			size;
    FontName		fn;		/* always sorted */
} NameEntry;

typedef struct _FontTable {
    char		*directory;
    FileEntry		file;
    NameEntry		name;
} FontTableRec, *FontTable;
#define NullTable ((FontTable) NULL)

/* from fontdir.c */

extern int		AddFileEntry();
extern Boolean		AddNameEntry();
extern void		FreeFontTable();
extern FontTable	MakeFontTable();
extern Boolean		Match();

