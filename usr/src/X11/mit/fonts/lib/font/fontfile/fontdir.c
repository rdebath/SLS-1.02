/*
 * $Header: /home/x_cvs/mit/fonts/lib/font/fontfile/fontdir.c,v 1.2 1992/09/08 11:08:37 dawes Exp $
 * $XConsortium: fontdir.c,v 1.9 92/03/20 15:53:29 eswu Exp $
 *
 * Copyright 1991 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

#include    "fontfilest.h"
#include    <X11/keysym.h>

Bool
FontFileInitTable (table, size)
    FontTablePtr    table;
    int		    size;
{
    if (size)
    {
	table->entries = (FontEntryPtr) xalloc(sizeof(FontEntryRec) * size);
	if (!table->entries)
	    return FALSE;
    }
    else
	table->entries = 0;
    table->used = 0;
    table->size = size;
    table->sorted = FALSE;
    return TRUE;
}

FontFileFreeEntry (entry)
    FontEntryPtr    entry;
{
    FontScalableExtraPtr   extra;

    if (entry->name.name)
	xfree(entry->name.name);

    switch (entry->type)
    {
    case FONT_ENTRY_SCALABLE:
	xfree (entry->u.scalable.fileName);
	extra = entry->u.scalable.extra;
	xfree (extra->scaled);
	xfree (extra);
	break;
    case FONT_ENTRY_BITMAP:
	xfree (entry->u.bitmap.fileName);
	break;
    case FONT_ENTRY_ALIAS:
	xfree (entry->u.alias.resolved);
	break;
    case FONT_ENTRY_BC:
	break;
    }
}

FontFileFreeTable (table)
    FontTablePtr    table;
{
    int	i;

    for (i = 0; i < table->used; i++)
	FontFileFreeEntry (&table->entries[i]);
    xfree (table->entries);
}

FontDirectoryPtr
FontFileMakeDir(dirName, size)
    char       *dirName;
    int         size;
{
    FontDirectoryPtr	dir;
    int			dirlen;
    int			needslash = 0;

    dirlen = strlen(dirName);
    if (dirName[dirlen - 1] != '/')
#ifdef NCD
    if (dirlen)     /* leave out slash for builtins */
#endif
	needslash = 1;
    dir = (FontDirectoryPtr) xalloc(sizeof *dir + dirlen + needslash + 1);
    if (!dir)
	return (FontDirectoryPtr)0;
    if (!FontFileInitTable (&dir->scalable, 0))
    {
	xfree (dir);
	return (FontDirectoryPtr)0;
    }
    if (!FontFileInitTable (&dir->nonScalable, size))
    {
	FontFileFreeTable (&dir->scalable);
	xfree (dir);
	return (FontDirectoryPtr)0;
    }
    dir->directory = (char *) (dir + 1);
    dir->dir_mtime = 0;
    dir->alias_mtime = 0;
    strcpy(dir->directory, dirName);
    if (needslash)
	strcat(dir->directory, "/");
    return dir;
}

FontFileFreeDir (dir)
    FontDirectoryPtr	dir;
{
    FontFileFreeTable (&dir->scalable);
    FontFileFreeTable (&dir->nonScalable);
    xfree(dir);
}

FontEntryPtr
FontFileAddEntry(table, prototype)
    FontTablePtr	table;
    FontEntryPtr	prototype;
{
    FontEntryPtr    entry;
    int		    newsize;

    /* can't add entries to a sorted table, pointers get broken! */
    if (table->sorted)
	return (FontEntryPtr) 0;    /* "cannot" happen */
    if (table->used == table->size) {
	newsize = table->size + 100;
	entry = (FontEntryPtr) xrealloc(table->entries,
					   newsize * sizeof(FontEntryRec));
	if (!entry)
	    return (FontEntryPtr)0;
	table->size = newsize;
	table->entries = entry;
    }
    entry = &table->entries[table->used];
    *entry = *prototype;
    entry->name.name = (char *) xalloc(prototype->name.length + 1);
    if (!entry->name.name)
	return (FontEntryPtr)0;
    bcopy (prototype->name.name, entry->name.name, prototype->name.length);
    entry->name.name[entry->name.length] = '\0';
    table->used++;
    return entry;
}

static int
FontFileNameCompare(a, b)
    char       *a,
               *b;
{
    FontEntryPtr    a_name = (FontEntryPtr) a,
		    b_name = (FontEntryPtr) b;

    return strcmp(a_name->name.name, b_name->name.name);
}

FontFileSortTable (table)
    FontTablePtr    table;
{
    if (!table->sorted) {
	if (table->used > 0)
	    qsort((char *) table->entries, table->used, sizeof(FontEntryRec),
	          FontFileNameCompare);
	table->sorted = TRUE;
    }
}

FontFileSortDir(dir)
    FontDirectoryPtr	dir;
{
    FontFileSortTable (&dir->scalable);
    FontFileSortTable (&dir->nonScalable);
    /* now that the table is fixed in size, swizzle the pointers */
    FontFileSwitchStringsToBitmapPointers (dir);
}

#define isWild(c)   ((c) == XK_asterisk || (c) == XK_question)

static int
SetupWildMatch(table, pat, leftp, rightp, privatep)
    FontTablePtr    table;
    FontNamePtr	    pat;
    int		    *leftp,
		    *rightp;
    int		    *privatep;
{
    int         nDashes;
    char        c;
    char       *t;
    char       *firstWild;
    int         first;
    int         center,
                left,
                right;
    int         result;
    char	*name;

    name = pat->name;
    nDashes = pat->ndashes;
    firstWild = 0;
    t = name;
    while (c = *t++) {
	if (isWild(c)) {
	    if (!firstWild)
		firstWild = t - 1;
	}
    }
    left = 0;
    right = table->used;
    if (firstWild)
	*privatep = nDashes;
    else
	*privatep = -1;
    if (!table->sorted) {
	*leftp = left;
	*rightp = right;
	return -1;
    } else if (firstWild) {
	first = firstWild - name;
	while (left < right) {
	    center = (left + right) / 2;
	    result = strncmp(name, table->entries[center].name.name, first);
	    if (result == 0)
		break;
	    if (result < 0)
		right = center;
	    else
		left = center + 1;
	}
	*leftp = left;
	*rightp = right;
	return -1;
    } else {
	while (left < right) {
	    center = (left + right) / 2;
	    result = strcmp(name, table->entries[center].name.name);
	    if (result == 0)
		return center;
	    if (result < 0)
		right = center;
	    else
		left = center + 1;
	}
	*leftp = 1;
	*rightp = 0;
	return -1;
    }
}

static
PatternMatch(pat, patdashes, string, stringdashes)
    char       *pat;
    char       *string;
{
    char        c,
                t;

    if (stringdashes < patdashes)
	return 0;
    for (;;) {
	switch (c = *pat++) {
	case '*':
	    if (!(c = *pat++))
		return 1;
	    if (c == XK_minus) {
		patdashes--;
		for (;;) {
		    while ((t = *string++) != XK_minus)
			if (!t)
			    return 0;
		    stringdashes--;
		    if (PatternMatch(pat, patdashes, string, stringdashes))
			return 1;
		    if (stringdashes == patdashes)
			return 0;
		}
	    } else {
		for (;;) {
		    while ((t = *string++) != c) {
			if (!t)
			    return 0;
			if (t == XK_minus) {
			    if (stringdashes-- < patdashes)
				return 0;
			}
		    }
		    if (PatternMatch(pat, patdashes, string, stringdashes))
			return 1;
		}
	    }
	case '?':
	    if (*string++ == XK_minus)
		stringdashes--;
	    break;
	case '\0':
	    return (*string == '\0');
	case XK_minus:
	    if (*string++ == XK_minus) {
		patdashes--;
		stringdashes--;
		break;
	    }
	    return 0;
	default:
	    if (c == *string++)
		break;
	    return 0;
	}
    }
}

FontEntryPtr
FontFileFindNameInDir(table, pat)
    FontTablePtr    table;
    FontNamePtr	    pat;
{
    int         i,
                start,
                stop,
                res,
                private;
    FontNamePtr	name;

    if ((i = SetupWildMatch(table, pat, &start, &stop, &private)) >= 0)
	return &table->entries[i];
    for (i = start; i < stop; i++) {
	name = &table->entries[i].name;
	res = PatternMatch(pat->name, private, name->name, name->ndashes);
	if (res > 0)
	    return &table->entries[i];
	if (res < 0)
	    break;
    }
    return (FontEntryPtr)0;
}

int
FontFileFindNamesInDir(table, pat, max, names)
    FontTablePtr    table;
    FontNamePtr	    pat;
    int		    max;
    FontNamesPtr    names;
{
    int		    i,
		    start,
		    stop,
		    res,
		    private;
    int		    ret;
    FontEntryPtr    fname;
    FontNamePtr	    name;

    if (max <= 0)
	return Successful;
    if ((i = SetupWildMatch(table, pat, &start, &stop, &private)) >= 0) {
	name = &table->entries[i].name;
	return AddFontNamesName(names, name->name, name->length);
    }
    fname = &table->entries[start];
    for (i = start, fname = &table->entries[start]; i < stop; i++, fname++) {
	res = PatternMatch(pat->name, private, fname->name.name, fname->name.ndashes);
	if (res > 0) {
	    ret = AddFontNamesName(names, fname->name.name, fname->name.length);
	    if (ret != Successful)
		return ret;
	    if (--max <= 0)
		break;
	} else if (res < 0)
	    break;
    }
    return Successful;
}

/*
 * Add a font file to a directory.  This handles bitmap and
 * scalable names both
 */

Bool
FontFileAddFontFile (dir, fontName, fileName)
    FontDirectoryPtr	dir;
    char		*fontName;
    char		*fileName;
{
    FontEntryRec	    entry;
    FontScalableRec	    vals, zeroVals;
    FontRendererPtr	    renderer;
    FontEntryPtr	    existing;
    FontScalableExtraPtr    extra;
    FontEntryPtr	    bitmap, scalable;
    Bool		    isscale;

    renderer = FontFileMatchRenderer (fileName);
    if (!renderer)
	return FALSE;
    entry.name.length = strlen (fontName);
    if (entry.name.length > MAXFONTNAMELEN)
	entry.name.length = MAXFONTNAMELEN;
    entry.name.name = fontName;
    CopyISOLatin1Lowered (entry.name.name, fontName, entry.name.length);
    entry.name.ndashes = CountDashes (entry.name.name, entry.name.length);
    entry.name.name[entry.name.length] = '\0';
    /*
     * Add a bitmap name if the incoming name isn't an XLFD name, or
     * if it isn't a scalable name (i.e. non-zero scalable fields)
     */
    isscale = FALSE;
    if (entry.name.ndashes != 14 ||
	!(isscale = FontParseXLFDName (entry.name.name,
					     &vals, FONT_XLFD_REPLACE_NONE)) ||
	  vals.pixel != 0)
    {
      /* If the fontname says it is nonScalable, make sure that the
       * renderer supports OpenBitmap and GetInfoBitmap.
       */
      if (renderer->OpenBitmap && renderer->GetInfoBitmap)
      {
	entry.type = FONT_ENTRY_BITMAP;
	entry.u.bitmap.renderer = renderer;
	entry.u.bitmap.pFont = NullFont;
	if (!(entry.u.bitmap.fileName = SaveString (fileName)))
	    return FALSE;
	if (!(bitmap = FontFileAddEntry (&dir->nonScalable, &entry)))
	{
	    xfree (entry.u.bitmap.fileName);
	    return FALSE;
	}
      }
    }
    /*
     * Parse out scalable fields from XLFD names - a scalable name
     * just gets inserted, a scaled name has more things to do.
     */
    if (isscale)
    {
      /* If the fontname says it is scalable, make sure that the
       * renderer supports OpenScalable and GetInfoScalable.
       */
      if (renderer->OpenScalable && renderer->GetInfoScalable)
      {
	if (vals.pixel != 0)
	{
	    zeroVals.pixel = 0;
	    zeroVals.point = 0;
	    zeroVals.x = vals.x;
	    zeroVals.y = vals.y;
	    zeroVals.width = 0;
	    FontParseXLFDName (entry.name.name, &zeroVals, FONT_XLFD_REPLACE_VALUE);
	    entry.name.length = strlen (entry.name.name);
	    existing = FontFileFindNameInDir (&dir->scalable, &entry.name);
	    if (existing)
	    {
		if (vals.point == GetDefaultPointSize ())
		{
		    existing->u.scalable.extra->defaults = vals;
		    xfree (existing->u.scalable.fileName);
		    if (!(existing->u.scalable.fileName = SaveString (fileName)))
			return FALSE;
		}
		FontFileAddScaledInstance (existing, &vals, NullFont, bitmap->name.name);
		return TRUE;
	    }
	}
	if (!(entry.u.scalable.fileName = SaveString (fileName)))
	    return FALSE;
	extra = (FontScalableExtraPtr) xalloc (sizeof (FontScalableExtraRec));
	if (!extra)
	{
	    xfree (entry.u.scalable.fileName);
	    return FALSE;
	}
	if (vals.point == GetDefaultPointSize())
	    extra->defaults = vals;
	else
	{
	    /* XXX fix interfaces */
	    struct resolution {
		CARD16	x B16;
		CARD16	y B16;
		CARD16	point_size B16;
	    } *resolution, *GetClientResolutions();
	    int num;

	    extra->defaults.point = GetDefaultPointSize();
	    extra->defaults.pixel = -1;
	    extra->defaults.width = -1;
	    if (vals.x <= 0 || vals.y <= 0)
	    {
	        resolution = GetClientResolutions (&num);
	        if (resolution && num > 0)
	        {
	    	    extra->defaults.x = resolution->x;
	    	    extra->defaults.y = resolution->y;
	        }
	        else
	        {
		    extra->defaults.x = 75;
		    extra->defaults.y = 75;
	        }
	     }
	     else 
	     {
		extra->defaults.x = vals.x;
		extra->defaults.y = vals.y;
	     }
	     FontFileCompleteXLFD (&extra->defaults, &extra->defaults);
	}
	extra->numScaled = 0;
	extra->sizeScaled = 0;
	extra->scaled = 0;
	extra->private = 0;
	entry.type = FONT_ENTRY_SCALABLE;
	entry.u.scalable.renderer = renderer;
	entry.u.scalable.extra = extra;
	if (!(scalable = FontFileAddEntry (&dir->scalable, &entry)))
	{
	    xfree (extra);
	    xfree (entry.u.scalable.fileName);
	    return FALSE;
	}
	if (vals.pixel != 0)
	    FontFileAddScaledInstance (scalable, &vals, NullFont, bitmap->name.name);
      }
    }
    return TRUE;
}

Bool
FontFileAddFontAlias (dir, aliasName, fontName)
    FontDirectoryPtr	dir;
    char		*aliasName;
    char		*fontName;
{
    FontEntryRec	entry;

    entry.name.length = strlen (aliasName);
    CopyISOLatin1Lowered (aliasName, aliasName, entry.name.length);
    entry.name.name = aliasName;
    entry.name.ndashes = CountDashes (entry.name.name, entry.name.length);
    entry.type = FONT_ENTRY_ALIAS;
    if (!(entry.u.alias.resolved = SaveString (fontName)))
	return FALSE;
    if (!FontFileAddEntry (&dir->nonScalable, &entry))
    {
	xfree (entry.u.alias.resolved);
	return FALSE;
    }
    return TRUE;
}
