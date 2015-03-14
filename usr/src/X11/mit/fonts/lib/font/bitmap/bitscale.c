/*
 * $XConsortium: bitscale.c,v 1.11 92/05/12 18:07:44 gildea Exp $
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
#include    "bitmap.h"

extern Atom MakeAtom();

enum scaleType {
    atom, pixel_size, point_size, resolution_x, resolution_y, average_width,
    scaledX, scaledY, unscaled, scaledXoverY, uncomputed
};

typedef struct _fontProp {
    char       *name;
    Atom        atom;
    enum scaleType type;
} fontProp;

static unsigned long fontGeneration = 0;	/* initialization flag */

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
    "FONT", 0, atom
};

#define NPROPS ((sizeof(fontNamePropTable) / sizeof(fontProp)) - 1)

static fontProp fontPropTable[] = {
    "MIN_SPACE", 0, scaledX,
    "NORM_SPACE", 0, scaledX,
    "MAX_SPACE", 0, scaledX,
    "END_SPACE", 0, scaledX,
    "AVG_CAPITAL_WIDTH", 0, scaledX,
    "AVG_LOWERCASE_WIDTH", 0, scaledX,
    "QUAD_WIDTH", 0, scaledX,
    "FIGURE_WIDTH", 0, scaledX,
    "SUPERSCRIPT_X", 0, scaledX,
    "SUPERSCRIPT_Y", 0, scaledY,
    "SUBSCRIPT_X", 0, scaledX,
    "SUBSCRIPT_Y", 0, scaledY,
    "SUPERSCRIPT_SIZE", 0, scaledY,
    "SUBSCRIPT_SIZE", 0, scaledY,
    "SMALL_CAP_SIZE", 0, scaledY,
    "UNDERLINE_POSITION", 0, scaledY,
    "UNDERLINE_THICKNESS", 0, scaledY,
    "STRIKEOUT_ASCENT", 0, scaledY,
    "STRIKEOUT_DESCENT", 0, scaledY,
    "ITALIC_ANGLE", 0, unscaled,
    "CAP_HEIGHT", 0, scaledY,
    "X_HEIGHT", 0, scaledY,
    "RELATIVE_SETWIDTH", 0, unscaled,
    "RELATIVE_WEIGHT", 0, unscaled,
    "WEIGHT", 0, scaledXoverY,
    "DESTINATION", 0, unscaled
};

static void
initFontPropTable()
{
    int         i;
    fontProp   *t;

    i = sizeof(fontNamePropTable) / sizeof(fontProp);
    for (t = fontNamePropTable; i; i--, t++)
	t->atom = MakeAtom(t->name, (unsigned) strlen(t->name), TRUE);

    i = sizeof(fontPropTable) / sizeof(fontProp);
    for (t = fontPropTable; i; i--, t++)
	t->atom = MakeAtom(t->name, (unsigned) strlen(t->name), TRUE);
}

static FontEntryPtr
GetScalableEntry (fpe, name)
    FontPathElementPtr	fpe;
    FontNamePtr		name;
{
    FontDirectoryPtr	dir;
    FontEntryPtr	entry;

    dir = (FontDirectoryPtr) fpe->private;
    return FontFileFindNameInDir (&dir->scalable, name);
}

static void
ComputeScaleFactors(from, to, dx, dy)
    FontScalablePtr from,
                to;
    double     *dx,
               *dy;
{
    /* compute scale factors */
    if (to->pixel == from->pixel)
	*dy = 1.0;
    else
	*dy = ((double) to->point * to->y) / (from->point * from->y);
    *dx = (((double) (to->x * from->y)) / (to->y * from->x)) * *dy;
    if (to->width > 0)
	*dx = to->width / (from->width * *dx);
}

#define SCORE(m,s) \
if (m >= 1.0) { \
    if (m == 1.0) \
	score += (16 * s); \
    else if (m == 2.0) \
	score += (4 * s); \
    else if (m < minfrac) \
	score += (1 * s); \
    else if (m < 2.0) \
	score += (3 * s); \
    else \
	score += (2 * s); \
} else { \
    m = 1/m; \
    if (m < minfrac) \
	score += (1 * s); \
    else \
	score += (2 * s); \
}

FontEntryPtr
FindBestToScale(fpe, entry, vals, best, dxp, dyp, fpep)
    FontPathElementPtr	fpe;
    FontEntryPtr	entry;
    FontScalablePtr	vals,
			best;
    double		*dxp,
			*dyp;
    FontPathElementPtr	*fpep;
{
    FontScalableRec temp;
    int		    source, i;
    int		    best_score,
		    score;
    double	    dx, dx_amount,
		    dy, dy_amount,
		    best_dx, best_dx_amount,
		    best_dy, best_dy_amount,
		    minfrac;
    int		    status;
    FontEntryPtr    zero;
    FontNameRec	    zeroName;
    char	    zeroChars[MAXFONTNAMELEN];
    FontDirectoryPtr	dir;
    FontScaledPtr   scaled;
    FontScalableExtraPtr   extra;
    FontScaledPtr   best_scaled;
    FontPathElementPtr	best_fpe;

    /* find the best match */
    best_scaled = 0;
    best_score = 0;
    bcopy (entry->name.name, zeroChars, entry->name.length);
    zeroChars[entry->name.length] = '\0';
    zeroName.name = zeroChars;
    FontParseXLFDName (zeroChars, &temp, FONT_XLFD_REPLACE_ZERO);
    zeroName.length = strlen (zeroChars);
    zeroName.ndashes = entry->name.ndashes;
    /*
     * Look through all the registered bitmap sources for
     * the same zero name as ours; entries along that one
     * can be scaled as desired.
     */
    for (source = 0; source < FontFileBitmapSources.count; source++)
    {
	if (FontFileBitmapSources.fpe[source] == fpe)
	    zero = entry;
	else
	{
	    dir = (FontDirectoryPtr) FontFileBitmapSources.fpe[source]->private;
	    zero = FontFileFindNameInDir (&dir->scalable, &zeroName);
	    if (!zero)
		continue;
	}
	extra = zero->u.scalable.extra;
	for (i = 0; i < extra->numScaled; i++)
	{
	    scaled = &extra->scaled[i];
	    if (!scaled->bitmap)
		continue;
	    if ((scaled->vals.pixel <= 0) || (scaled->vals.point <= 0) ||
		    (scaled->vals.x <= 0) || (scaled->vals.y <= 0))
	    	continue;
	    ComputeScaleFactors(&scaled->vals, vals, &dx, &dy);
	    minfrac = (double) (3 * scaled->vals.pixel);
	    minfrac = (minfrac + 4.0) / minfrac;
	    score = 0;
	    dx_amount = dx;
	    dy_amount = dy;
	    SCORE(dy_amount, 10);
	    SCORE(dx_amount, 1);
	    if ((score > best_score) ||
		    ((score == best_score) &&
		     ((dy_amount < best_dy_amount) ||
 		      ((dy_amount == best_dy_amount) &&
 		       (dx_amount < best_dx_amount))))) 
	    {
		best_fpe = FontFileBitmapSources.fpe[source];
	    	best_scaled = scaled;
	    	best_score = score;
	    	best_dx = dx;
	    	best_dy = dy;
	    	best_dx_amount = dx_amount;
	    	best_dy_amount = dy_amount;
	    }
	}
    }
    if (!best_scaled)
	return NULL;

    *best = best_scaled->vals;
    *fpep = best_fpe;
    *dxp = best_dx;
    *dyp = best_dy;
    return best_scaled->bitmap;
}

static int
computeProps(pf, wasStringProp, npf, isStringProp, nprops, xfactor, yfactor)
    FontPropPtr pf;
    char	*wasStringProp;
    FontPropPtr npf;
    char	*isStringProp;
    unsigned int nprops;
    double      xfactor,
                yfactor;
{
    int         n;
    int         count;
    fontProp   *t;

    for (count = 0; nprops > 0; nprops--, pf++, wasStringProp++) {
	n = sizeof(fontPropTable) / sizeof(fontProp);
	for (t = fontPropTable; n && (t->atom != pf->name); n--, t++);
	if (!n)
	    continue;

	switch (t->type) {
	case scaledX:
	    npf->value = xfactor * pf->value;
	    break;
	case scaledY:
	    npf->value = yfactor * pf->value;
	    break;
	case unscaled:
	    npf->value = pf->value;
	    break;
	case scaledXoverY:
	    npf->value = pf->value * (xfactor / yfactor);
	}
	npf->name = pf->name;
	npf++;
	count++;
	*isStringProp++ = *wasStringProp;
    }
    return count;
}

static int
ComputeScaledProperties(sourceFontInfo, name, vals, dx, dy, pProps, pIsStringProp)
    FontInfoPtr     sourceFontInfo;	/* the font to be scaled */
    char	    *name;		/* name of resulting font */
    FontScalablePtr vals;
    double	    dx,
		    dy;		/* scale factors in x and y directions */
    FontPropPtr	    *pProps;	/* returns properties; preallocated */
    char	    **pIsStringProp;  /* return booleans; preallocated */
{
    int         n;
    char       *ptr1,
               *ptr2;
    FontPropPtr fp;
    fontProp   *fpt;
    extern int  serverGeneration;
    char	*isStringProp;
    int		nProps;

    if (fontGeneration != serverGeneration) {
	initFontPropTable();
	fontGeneration = serverGeneration;
    }
    nProps = NPROPS + 1 + sizeof(fontPropTable) / sizeof(fontProp);
    fp = (FontPropPtr) xalloc(sizeof(FontPropRec) * nProps);
    *pProps = fp;
    if (!fp)
	return 1;
    isStringProp = (char *) xalloc (nProps);
    *pIsStringProp = isStringProp;
    if (!isStringProp)
    {
	xfree (fp);
	return 1;
    }
    ptr2 = name;
    for (fpt = fontNamePropTable, n = NPROPS;
	 n;
 	 fp++, fpt++, n--, isStringProp++)
    {

	ptr1 = ptr2 + 1;
	if (*ptr1 == '-')
	    ptr2 = ptr1;
	else {
	    if (n > 1)
		ptr2 = index(ptr1 + 1, '-');
	    else
		ptr2 = index(ptr1 + 1, '\0');
	}

	*isStringProp = 0;
	switch (fpt->type) {
	case atom:
	    fp->value = MakeAtom(ptr1, ptr2 - ptr1, TRUE);
	    *isStringProp = 1;
	    break;
	case pixel_size:
	    fp->value = vals->pixel;
	    break;
	case point_size:
	    fp->value = vals->point;
	    break;
	case resolution_x:
	    fp->value = vals->x;
	    break;
	case resolution_y:
	    fp->value = vals->y;
	    break;
	case average_width:
	    fp->value = vals->width;
	    break;
	}
	fp->name = fpt->atom;
    }
    n = NPROPS;
    fp->name = fpt->atom;
    fp->value = MakeAtom(name, strlen(name), TRUE);
    *isStringProp = 1;
    isStringProp++;
    fp++;
    n++;
    n += computeProps(sourceFontInfo->props, sourceFontInfo->isStringProp,
		      fp, isStringProp,
		      sourceFontInfo->nprops, dx, dy);
    return n;
}

static void ScaleBitmap();

/*
 *  ScaleFont
 *  returns a pointer to the new scaled font, or NULL (due to AllocError).
 */
FontPtr
ScaleFont(opf, widthMult, heightMult, props, propCount, isStringProp)
    FontPtr     opf;		/* originating font */
    double      widthMult;	/* glyphs width scale factor */
    double      heightMult;	/* glyphs height scale factor */
    FontPropPtr props;		/* the new properties */
    int         propCount;	/* count of new properties */
    char	*isStringProp;	/* booleans per new property */
{
    FontPtr     pf;
    FontInfoPtr pfi,
                opfi;
    BitmapFontPtr  bitmapFont,
                obitmapFont;
    CharInfoPtr pci,
                opci;
    int         nchars;		/* how many characters in the font */
    int         newWidth,
                newHeight;
    char       *glyphBytes;
    unsigned    bytestoalloc;
    int        *scratch;
    xCharInfo  *pink;
    int         i;
    int         glyph;

    extern int  bitmapGetBitmaps();
    extern int  bitmapGetExtents();
    extern int  bitmapGetGlyphs();
    extern int  bitmapGetMetrics();
    extern void bitmapUnloadScalable();

    opfi = &opf->info;
    glyph = opf->glyph;
    obitmapFont = (BitmapFontPtr) opf->fontPrivate;

    bitmapFont = 0;
    pf = (FontPtr) xalloc(sizeof(FontRec));
    if (!pf)
	goto bail;
    pf->refcnt = 0;
    pf->maxPrivate = -1;
    pf->devPrivates = (pointer *) 0;
    pf->bit = opf->bit;
    pf->byte = opf->byte;
    pf->glyph = opf->glyph;
    pf->scan = opf->scan;

    pf->get_glyphs = bitmapGetGlyphs;
    pf->get_metrics = bitmapGetMetrics;
    pf->unload_font = bitmapUnloadScalable;

    pfi = &pf->info;
    *pfi = *opfi;
    bitmapFont = (BitmapFontPtr) xalloc(sizeof(BitmapFontRec));
    if (!bitmapFont)
	goto bail;
    nchars = (opf->info.lastRow - opf->info.firstRow + 1) *
	(opf->info.lastCol - opf->info.firstCol + 1);
    pf->fontPrivate = (pointer) bitmapFont;
    bitmapFont->version_num = obitmapFont->version_num;
    bitmapFont->num_chars = obitmapFont->num_chars;
    bitmapFont->num_tables = obitmapFont->num_tables;
    bitmapFont->metrics = 0;
    bitmapFont->ink_metrics = 0;
    bitmapFont->bitmaps = 0;
    bitmapFont->encoding = 0;
    bitmapFont->bitmapExtra = 0;
    bitmapFont->pDefault = 0;
    bitmapFont->metrics = (CharInfoPtr) xalloc(nchars * sizeof(CharInfoRec));
    if (!bitmapFont->metrics)
	goto bail;
    if (obitmapFont->pDefault)
	bitmapFont->pDefault = bitmapFont->metrics + (obitmapFont->pDefault - obitmapFont->metrics);
    if (obitmapFont->ink_metrics) {
	bitmapFont->ink_metrics = (xCharInfo *) xalloc(nchars * sizeof(xCharInfo));
	if (!bitmapFont->ink_metrics)
	    goto bail;
    }
    bitmapFont->encoding = (CharInfoPtr *) xalloc(nchars * sizeof(CharInfoPtr));
    if (!bitmapFont->encoding)
	goto bail;

    bytestoalloc = 0;

#define MAXSHORT    32767
#define MINSHORT    -32768

    pfi->anamorphic = FALSE;
    if (heightMult != widthMult)
	pfi->anamorphic = TRUE;

    newHeight = (opfi->fontAscent + opfi->fontDescent) * heightMult;
    pfi->fontAscent = opfi->fontAscent * heightMult;
    pfi->fontDescent = newHeight - pfi->fontAscent;

    pfi->minbounds.leftSideBearing = MAXSHORT;
    pfi->minbounds.rightSideBearing = MAXSHORT;
    pfi->minbounds.ascent = MAXSHORT;
    pfi->minbounds.descent = MAXSHORT;
    pfi->minbounds.characterWidth = MAXSHORT;

    pfi->maxbounds.leftSideBearing = MINSHORT;
    pfi->maxbounds.rightSideBearing = MINSHORT;
    pfi->maxbounds.ascent = MINSHORT;
    pfi->maxbounds.descent = MINSHORT;
    pfi->maxbounds.characterWidth = MINSHORT;

    pci = bitmapFont->metrics;
    for (i = 0; i < nchars; i++)
    {
	if (opci = obitmapFont->encoding[i])
	{
	    bitmapFont->encoding[i] = pci;
	    newWidth = GLYPHWIDTHPIXELS(opci) * widthMult;
	    newHeight = GLYPHHEIGHTPIXELS(opci) * heightMult;
	    if (newHeight == 0)
	    	newHeight = 1;
	    if (newWidth == 0)
	    	newWidth = 1;
    
	    pci->metrics.leftSideBearing = (opci->metrics.leftSideBearing *
					    widthMult);
	    pci->metrics.rightSideBearing = newWidth +
	    	pci->metrics.leftSideBearing;
	    pci->metrics.ascent = opci->metrics.ascent * heightMult;
	    pci->metrics.descent = newHeight - pci->metrics.ascent;
	    pci->metrics.characterWidth = opci->metrics.characterWidth * widthMult;
	    pci->metrics.attributes = opci->metrics.attributes;
    
	    bytestoalloc += BYTES_FOR_GLYPH(pci, glyph);
#define MINMAX(field) \
	    if (pfi->minbounds.field > pci->metrics.field) \
	    	pfi->minbounds.field = pci->metrics.field; \
	    if (pfi->maxbounds.field < pci->metrics.field) \
	    	pfi->maxbounds.field = pci->metrics.field
    
	    MINMAX(leftSideBearing);
	    MINMAX(rightSideBearing);
	    MINMAX(ascent);
	    MINMAX(descent);
	    MINMAX(characterWidth);
#undef MINMAX
	    pci++;
	}
	else
	    bitmapFont->encoding[i] = 0;
    }
    bitmapFont->bitmaps = (char *) xalloc(bytestoalloc);
    if (!bitmapFont->bitmaps)
	goto bail;
    bzero(bitmapFont->bitmaps, bytestoalloc);

    /* Copy over the scaled XLFD properties */

    pfi->props = props;
    pfi->nprops = propCount;
    pfi->isStringProp = isStringProp;

    /*
     * For each character, set the per-character metrics, scale the glyph, and
     * check per-font minbounds and maxbounds character information.
     */

    glyphBytes = bitmapFont->bitmaps;
    pci = bitmapFont->metrics;
    if (widthMult == 1 && heightMult == 1)
    {
	for (i = 0; i < nchars; i++)
	{
	    if (opci = obitmapFont->encoding[i])
	    {
		int size;
		xCharInfo   *opink;

	    	pci = bitmapFont->encoding[i];
	    	if (pink = bitmapFont->ink_metrics)
	    	{
		    pink = pink + (pci - bitmapFont->metrics);
		    opink = obitmapFont->ink_metrics;
		    opink = opink + (opci - obitmapFont->metrics);
		    *pink = *opink;
	    	}
	    	pci->bits = glyphBytes;
		size = BYTES_FOR_GLYPH(pci, glyph);
		bcopy (opci->bits, pci->bits, size);
	    	glyphBytes += size;
	    }
	}
    }
    else
    {
    	/* Allocate the scratch space for the glyph scaling routine. */
    	scratch = (int *)
	    xalloc((int) ((opfi->maxbounds.rightSideBearing -
		       	   opfi->minbounds.leftSideBearing)
		      	  * widthMult * sizeof(int)));
    	if (!scratch)
	    goto bail;
    
    	pink = bitmapFont->ink_metrics;
    	if (pink) {
	    pfi->ink_minbounds.leftSideBearing = MAXSHORT;
	    pfi->ink_minbounds.rightSideBearing = MAXSHORT;
	    pfi->ink_minbounds.ascent = MAXSHORT;
	    pfi->ink_minbounds.descent = MAXSHORT;
	    pfi->ink_minbounds.characterWidth = MAXSHORT;
	    pfi->ink_maxbounds.leftSideBearing = MINSHORT;
	    pfi->ink_maxbounds.rightSideBearing = MINSHORT;
	    pfi->ink_maxbounds.ascent = MINSHORT;
	    pfi->ink_maxbounds.descent = MINSHORT;
	    pfi->ink_maxbounds.characterWidth = MINSHORT;
    	} else {
	    pfi->ink_minbounds = pfi->minbounds;
	    pfi->ink_maxbounds = pfi->maxbounds;
    	}
    
    	for (i = 0; i < nchars; i++)
    	{
	    if (opci = obitmapFont->encoding[i])
	    {
	    	pci = bitmapFont->encoding[i];
	    	if (pink = bitmapFont->ink_metrics)
	    	{
		    pink = pink + (pci - bitmapFont->metrics);
	    	}
	    	pci->bits = glyphBytes;
	    	ScaleBitmap (pf, opci, pci, scratch, pink);
	    	glyphBytes += BYTES_FOR_GLYPH(pci, glyph);
	    }
    	}
	FontComputeInfoAccelerators (pfi);
    	xfree(scratch);
    }

    if (pfi->defaultCh != (unsigned short) NO_SUCH_CHAR) {
	int         r,
	            c,
	            cols;

	r = pfi->defaultCh >> 8;
	c = pfi->defaultCh & 0xFF;
	if (pfi->firstRow <= r && r <= pfi->lastRow &&
		pfi->firstCol <= c && c <= pfi->lastCol) {
	    cols = pfi->lastCol - pfi->firstCol + 1;
	    r = r - pfi->firstRow;
	    c = c - pfi->firstCol;
	    bitmapFont->pDefault = bitmapFont->encoding[r * cols + c];
	}
    }

    return pf;
bail:
    if (pf)
	xfree(pf);
    if (bitmapFont) {
	xfree(bitmapFont->metrics);
	xfree(bitmapFont->ink_metrics);
	xfree(bitmapFont->bitmaps);
	xfree(bitmapFont->encoding);
    }
    return NULL;
}

static int
lcm(a, b)			/* least common multiple */
    int         a,
                b;
{
    register int m;
    register int larger,
                smaller;

    if (a > b) {
	m = larger = a;
	smaller = b;
    } else {
	m = larger = b;
	smaller = a;
    }

    while (m % smaller)
	m += larger;
    return m;
}

static void
ScaleBitmap(pFont, opci, pci, scratch, pink)
    FontPtr     pFont;
    CharInfoPtr opci;
    CharInfoPtr pci;
    int        *scratch;
    xCharInfo  *pink;
{
    char       *bitmap,
               *newBitmap;
    int         width,
                height,
                newWidth,
                newHeight;
    int         kcounter;	/* 0 <= kcounter <= k */
    int         lcounter;	/* 0 <= lcounter <= l */
    int         kkcounter;	/* 0 <= kkcounter <= kk */
    int         llcounter;	/* 0 <= llcounter <= ll */
    int         newBit;		/* newBitmap column index, by bits */
    int        *acc;		/* pseudonym for scratch */
    int         bitValue;	/* tmp variable */
    int         dataByte;
    char       *dataBytePointer;
    char       *saveDataBytePointer;

    /* The following variables have constant values, once assigned. */

    int         virtualWidth;	/* unpadded bit width of the virtual bitmap */
    int         virtualHeight;	/* unpadded bit height of the virtual bitmap */
    int         k;		/* horizontal bit copies in virtualBitmap */
    int         l;		/* vertical bit copies in virtualBitmap */
    int         kk;		/* horiz. virtual bits in a newBitmap bit */
    int         ll;		/* vertical virtual bits in a newBitmap bit */
    int         threshold;
    int         padWidth;	/* actual width in bytes of pad in bitmap */
    int         newPadWidth;

    bitmap = opci->bits;
    newBitmap = pci->bits;
    width = GLYPHWIDTHPIXELS(opci);
    height = GLYPHHEIGHTPIXELS(opci);
    newWidth = GLYPHWIDTHPIXELS(pci);
    newHeight = GLYPHHEIGHTPIXELS(pci);
    if (!newWidth || !newHeight || !width || !height)
	return;

    padWidth = BYTES_PER_ROW(width, pFont->glyph) - ((width + 7) >> 3);
    newPadWidth = BYTES_PER_ROW(newWidth, pFont->glyph) - ((newWidth + 7) >> 3);
    virtualWidth = lcm(width, newWidth);
    virtualHeight = lcm(height, newHeight);
    k = virtualWidth / width;
    l = virtualHeight / height;
    kk = virtualWidth / newWidth;
    ll = virtualHeight / newHeight;
    threshold = kk * ll;
    if (pink) {
	pink->leftSideBearing = MAXSHORT;
	pink->rightSideBearing = MINSHORT;
	pink->ascent = MINSHORT;
	pink->descent = MINSHORT;
	pink->characterWidth = pci->metrics.characterWidth;
	pink->attributes = pci->metrics.attributes;
    }
    saveDataBytePointer = bitmap;
    newBitmap--;
    lcounter = l;
    for (; newHeight; newHeight--, newBitmap += newPadWidth) {

	newBit = newWidth;
	acc = scratch;
	do
	    *acc++ = threshold;
	while (--newBit);

	llcounter = ll;
	while (llcounter) {
	    int         bit;	/* index into glyph row, indicating a bit */
	    int         row_dup;/* row duplication count from source */
	    int         kdup;
	    int         kkdup;

	    if (!lcounter) {
		lcounter = l;
		dataBytePointer += padWidth;
		saveDataBytePointer = dataBytePointer;
	    } else
		dataBytePointer = saveDataBytePointer;

	    if ((row_dup = llcounter) > lcounter)
		row_dup = lcounter;
	    lcounter -= row_dup;
	    llcounter -= row_dup;
	    row_dup <<= 1;

	    bit = 1;
	    kdup = k * row_dup;
	    kkdup = kk * row_dup;
	    kcounter = 0;
	    newBit = newWidth;
	    acc = scratch;
	    do {
		int         tmp = 0;

		kkcounter = kkdup;
		if (!kcounter) {
		    /* advance to the next column of the source bitmap */
		    kcounter = kdup;
		    if (!(--bit)) {
			bit = 8;
			dataByte = *dataBytePointer++;
		    }
		    /* extract the appropriate bit from source bitmap */
		    if (pFont->bit == LSBFirst) {
			bitValue = dataByte & 1;
			dataByte >>= 1;
		    } else {
			bitValue = dataByte & 128;
			dataByte <<= 1;
		    }
		}
		while ((kkcounter -= kcounter) > 0) {
		    if (bitValue)
			tmp += kcounter;
		    /* advance to the next column of the source bitmap */
		    kcounter = kdup;
		    if (!(--bit)) {
			bit = 8;
			dataByte = *dataBytePointer++;
		    }
		    /* extract the appropriate bit from source bitmap */
		    if (pFont->bit == LSBFirst) {
			bitValue = dataByte & 1;
			dataByte >>= 1;
		    } else {
			bitValue = dataByte & 128;
			dataByte <<= 1;
		    }
		}
		if (bitValue)
		    tmp += kcounter + kkcounter;
		kcounter = -kkcounter;
		*acc++ -= tmp;
	    }
	    while (--newBit);
	}
	/*
	 * Set the appropriate bits in newBitmap, based on the count of bits
	 * set in the virtual bitmap which map to a single bit in the new
	 * bitmap, and based on the knowing the constant number of bits in the
	 * virtual bitmap which map to a bit in the new bitmap.
	 */
	acc = scratch;

	if (pFont->bit == LSBFirst)
	    bitValue = 128;
	else
	    bitValue = 1;
	newBit = newWidth;
	do {
	    if (pFont->bit == LSBFirst) {
		if ((bitValue <<= 1) == 256) {
		    bitValue = 0x1;
		    newBitmap++;
		}
	    } else {
		if (!(bitValue >>= 1)) {
		    bitValue = 128;
		    newBitmap++;
		}
	    }
	    if (*acc++ < 0)
		*newBitmap |= bitValue;
	} while (--newBit);
	if (pink) {
	    for (acc = scratch, newBit = newWidth;
		    --newBit >= 0 && *acc++ >= 0;
		);
	    if (newBit >= 0) {
		if (newHeight >= pink->ascent)
		    pink->ascent = newHeight;
		pink->descent = newHeight;
		newBit = pci->metrics.leftSideBearing + newWidth - newBit - 1;
		if (newBit < pink->leftSideBearing)
		    pink->leftSideBearing = newBit;
	    }
	    for (acc = scratch + newWidth, newBit = newWidth;
		    --newBit >= 0 && *--acc >= 0;
		);
	    if (newBit >= 0)
	    {
	    	newBit = pci->metrics.leftSideBearing + newBit + 1;
	    	if (newBit > pink->rightSideBearing)
		    pink->rightSideBearing = newBit;
	    }
	}
    }
#define MINMAX(field) \
    if (pFont->info.ink_minbounds.field > pink->field) \
	pFont->info.ink_minbounds.field = pink->field; \
    if (pFont->info.ink_maxbounds.field < pink->field) \
	pFont->info.ink_maxbounds.field = pink->field

    if (pink) {
	if (pink->ascent == MINSHORT)
	{
	    /* hack to make computation below work */
	    pink->ascent = pci->metrics.descent;
	    /* these would also have not been set as this character is empty */
	    pink->descent = pink->ascent + 1;
	    pink->leftSideBearing = 0;
	    pink->rightSideBearing = 0;
	}
	height = pink->ascent - pink->descent + 1;
	pink->ascent = pci->metrics.ascent -
	    (GLYPHHEIGHTPIXELS(pci) - pink->ascent);
	pink->descent = height - pink->ascent;
	MINMAX(leftSideBearing);
	MINMAX(rightSideBearing);
	MINMAX(ascent);
	MINMAX(descent);
	MINMAX(characterWidth);
    }
#undef MINMAX
}

#ifdef NOTDEF
/*
 *	exported interfaces
 */

FontFileLoadName(dirs, ndirs, name, pfont, format, fmask)
    FontFileDirPtr *dirs;
    int         ndirs;
    char       *name;
    FontPtr    *pfont;
    fsBitmapFormat format;
    fsBitmapFormatMask fmask;
{
    FontFileNamePtr fname;
    char        full_name[1024];
    int         ret = BadFontName;
    int         i;

    i = 0;
    while (i < ndirs) {
	if (fname = FontFileFindNameInDir(dirs[i], name)) {
	    if (!fname->alias) {
		if (!fname->font) {
		    strcpy(full_name, dirs[i]->dir);
		    strcat(full_name, fname->file);
		    ret = FontFileLoad(pfont, full_name, format, fmask);
		    if (ret == Successful) {
			fname->font = *pfont;
			(*pfont)->fpePrivate = (pointer) fname;
		    }
		    return ret;
		}
		*pfont = fname->font;
		return Successful;
	    }
	    name = fname->file;
	    i = 0;
	} else
	    i++;
    }
    return BadFontName;
}
#endif

/* ARGSUSED */
BitmapOpenScalable (fpe, pFont, flags, entry, fileName, vals, format, fmask)
    FontPathElementPtr	fpe;
    FontPtr		*pFont;
    int			flags;
    FontEntryPtr	entry;
    char		*fileName;  /* unused */
    FontScalablePtr	vals;
    fsBitmapFormat	format;
    fsBitmapFormatMask	fmask;
{
    FontScalableRec	best;
    FontPtr		font = NullFont;
    double		dx,
			dy;
    FontPropPtr		props;
    char		*isStringProp;
    int			propCount;
    int			status;

    FontEntryPtr	scaleFrom;
    FontPathElementPtr	scaleFPE;
    FontPtr		sourceFont;
    char		fontName[MAXFONTNAMELEN];

    /* Can't deal with mix-endian fonts yet */

#ifdef NOTDEF /* XXX need better test */
    if ((format & BitmapFormatByteOrderMask) !=
	    (format & BitmapFormatBitOrderMask))
	return NullFontFileName;
#endif

    scaleFrom = FindBestToScale (fpe, entry, vals, &best, &dx, &dy, &scaleFPE);
    if (!scaleFrom)
	return BadFontName;

    status = FontFileOpenBitmap(scaleFPE, &sourceFont, LoadAll, scaleFrom, format, fmask);

    if (status != Successful)
	return BadFontName;

    if (!vals->width)
	vals->width = best.width * dx;

    /* Prepare font properties for the new font */

    strcpy (fontName, scaleFrom->name.name);
    FontParseXLFDName (fontName, vals, FONT_XLFD_REPLACE_VALUE);

    propCount = ComputeScaledProperties(&sourceFont->info, fontName, vals, dx, dy,
					&props, &isStringProp);

    if (propCount && (!props || !isStringProp))
	return AllocError;
    
    /* Compute the scaled font */

    font = ScaleFont(sourceFont, dx, dy, props, propCount, isStringProp);

    /* Dispose source font */

    if (!sourceFont->refcnt)
	FontFileCloseFont((FontPathElementPtr) 0, sourceFont);

    if (!font)
    {
	xfree (props);
	xfree (isStringProp);
	return AllocError;
    }
    *pFont = font;
    return Successful;
}

static
ScaleBounds (bounds, dx, dy)
    xCharInfo	*bounds;
    double  dx, dy;
{
    bounds->leftSideBearing *= dx;
    bounds->rightSideBearing *= dx;
    bounds->ascent *= dy;
    bounds->descent *= dy;
    bounds->characterWidth *= dx;
}

BitmapGetInfoScalable (fpe, pFontInfo, entry, fontName, fileName, vals)
    FontPathElementPtr	fpe;
    FontInfoPtr		pFontInfo;
    FontEntryPtr	entry;
    FontNamePtr		fontName;
    char		*fileName;
    FontScalablePtr	vals;
{
    FontEntryPtr	scaleFrom;
    FontPathElementPtr	scaleFPE;
    double		dx,
			dy;
    FontScalableRec	best;
    FontInfoRec		scaleInfo;
    int			ret;
    int			propCount;
    FontPropPtr		props;
    char		*isStringProp;

    scaleFrom = FindBestToScale (fpe, entry, vals, &best, &dx, &dy, &scaleFPE);
    if (!scaleFrom)
	return BadFontName;
    ret = FontFileGetInfoBitmap (fpe, &scaleInfo, scaleFrom);
    if (ret != Successful)
	return ret;

    if (!vals->width)
	vals->width = best.width * dx;

    propCount = ComputeScaledProperties (&scaleInfo, fontName->name, vals, dx, dy,
					&props, &isStringProp);
    xfree (scaleInfo.isStringProp);
    xfree (scaleInfo.props);
    if (propCount && (!props || !isStringProp))
	return AllocError;

    *pFontInfo = scaleInfo;
    pFontInfo->props = props;
    pFontInfo->isStringProp = isStringProp;
    pFontInfo->nprops = propCount;
    pFontInfo->fontDescent *= dy;
    pFontInfo->fontAscent *= dy;
    ScaleBounds (&pFontInfo->minbounds, dx, dy);
    ScaleBounds (&pFontInfo->maxbounds, dx, dy);
    ScaleBounds (&pFontInfo->ink_minbounds, dx, dy);
    ScaleBounds (&pFontInfo->ink_minbounds, dx, dy);
    return Successful;
}

void
bitmapUnloadScalable (pFont)
    FontPtr	    pFont;
{
    BitmapFontPtr   bitmapFont;
    FontInfoPtr	    pfi;

    bitmapFont = (BitmapFontPtr) pFont->fontPrivate;
    pfi = &pFont->info;
    xfree (pfi->props);
    xfree (pfi->isStringProp);
    xfree (bitmapFont->encoding);
    xfree (bitmapFont->bitmaps);
    xfree (bitmapFont->ink_metrics);
    xfree (bitmapFont->metrics);
    xfree (pFont->fontPrivate);
    xfree (pFont);
}
