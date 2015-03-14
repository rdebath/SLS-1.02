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
#include "MacFont.h"

#include <stdio.h>
#include <ctype.h>
#include "fontfilest.h"
#include "bitmap.h"
#include "font.h"

#include "MacISO.h"
#define TOISO(C)    (MacToLatin1[C])    /* Convert a character to ISO Latin1 mapping */
#define TOMAC(C)    (Latin1ToMac[C])    /* Convert a character to Mac ASCII mapping */

static void
FastCharInkMetrics32(pFont, pCI, pInk)
    FontPtr     pFont;
    CharInfoPtr pCI;
    xCharInfo  *pInk;
{
    int ascent, descent;
	unsigned int savebits;
    register int vpos, inkAscent, inkDescent, lb, rb;
    register unsigned int *p;
    register unsigned int charbits;

    pInk->characterWidth = pCI->metrics.characterWidth;
    pInk->attributes = pCI->metrics.attributes;

    ascent = pCI->metrics.ascent;
    descent = pCI->metrics.descent;

    p = (unsigned int *) pCI->bits;
	charbits = 0;
	inkAscent = ascent + 1;

    for (vpos = ascent; --vpos >= -descent; p++) {
		if (*p != 0) {
			if (inkAscent > ascent) /* must be first inked row */
				inkAscent = vpos + 1;
			inkDescent = -vpos; /* descends at least this far */
			charbits |= *p;
		}
    }

	if (inkAscent > ascent) {
    	/*
     	* special case -- font with no bits gets all zeros
     	*/
    	pInk->leftSideBearing = pCI->metrics.leftSideBearing;
    	pInk->rightSideBearing = pCI->metrics.leftSideBearing;
    	pInk->ascent = 0;
    	pInk->descent = 0;
    	return;
	} else {
    	pInk->ascent = inkAscent;
    	pInk->descent = inkDescent;
	}

	savebits = charbits;
	lb = pCI->metrics.leftSideBearing;
	while ((charbits & 0x80000000) == 0) {
		charbits <<= 1;
		lb++;
	}
    pInk->leftSideBearing = lb;

	charbits = savebits;
	rb = pCI->metrics.rightSideBearing;
	while ((charbits & 0x00000001) == 0) {
		charbits >>= 1;
		rb--;
	}
    pInk->rightSideBearing = rb;
}

static void
FastCharInkMetrics64(pFont, pCI, pInk)
    FontPtr     pFont;
    CharInfoPtr pCI;
    xCharInfo  *pInk;
{
    int ascent, descent;
	unsigned int savebits1, savebits2;
    register int vpos, inkAscent, inkDescent, lb, rb;
    register unsigned int *p0, *p1;
    register unsigned int charbits1, charbits2;

    pInk->characterWidth = pCI->metrics.characterWidth;
    pInk->attributes = pCI->metrics.attributes;

    ascent = pCI->metrics.ascent;
    descent = pCI->metrics.descent;

    p0 = (unsigned int *) pCI->bits;
	p1 = p0 + 1;
	charbits1 = 0;
	charbits2 = 0;
	inkAscent = ascent + 1;

    for (vpos = ascent; --vpos >= -descent; p0 += 2, p1 += 2) {
		if (*p0 != 0 || *p1 != 0) {
			if (inkAscent > ascent) /* must be first inked row */
				inkAscent = vpos + 1;
			inkDescent = -vpos; /* descends at least this far */
			charbits1 |= *p0;
			charbits2 |= *p1;
		}
    }

	if (inkAscent > ascent) {
    	/*
     	* special case -- font with no bits gets all zeros
     	*/
    	pInk->leftSideBearing = pCI->metrics.leftSideBearing;
    	pInk->rightSideBearing = pCI->metrics.leftSideBearing;
    	pInk->ascent = 0;
    	pInk->descent = 0;
    	return;
	} else {
    	pInk->ascent = inkAscent;
    	pInk->descent = inkDescent;
	}

	savebits1 = charbits1;
	savebits2 = charbits2;
	lb = pCI->metrics.leftSideBearing;
	if (charbits1)
		while ((charbits1 & 0x80000000) == 0) {
			charbits1 <<= 1;
			lb++;
		}
	else {
		lb += 32;
		while ((charbits2 & 0x80000000) == 0) {
			charbits2 <<= 1;
			lb++;
		}
	}
    pInk->leftSideBearing = lb;

	charbits1 = savebits1;
	charbits2 = savebits2;
	rb = pCI->metrics.rightSideBearing;
	if(charbits2)
		while ((charbits2 & 0x00000001) == 0) {
			charbits2 >>= 1;
			rb--;
		}
	else {
		rb -= 32;
		while ((charbits1 & 0x00000001) == 0) {
			charbits1 >>= 1;
			rb--;
		}
	}
    pInk->rightSideBearing = rb;
}

static unsigned char macFirst, macLast;

static int widthOf (ch)
    int ch;
{
    if (ch < macFirst || ch > macLast) {
        return (0);
    } else {
        return (CharWidth (ch));
    }
}


static Point square = {1, 1};

int OpenMacFont (pFont, pMacPriv, name, pixel, res_x, res_y)
    FontPtr     pFont;
	MacBitmapFontRecPtr pMacPriv;
    char *name;
{
    char fname [MAXFONTNAMELEN];
    char *ptr, *ptr1, *ptr2, *ptr3, *ptr4, *ptr5, *ptr6, 
	 *ptr7, *ptr8, *ptr9, *ptr10, *ptr11, *ptr12, *ptr13;
    char *family_name, *weight_name, *slant, *setwidth_name, *add_style_name, 
	 *spacing, *registry, *encoding;
    GrafPtr pgp, savePort;
    FMetricRec *pfm;
    short fNum = 0;
    short face = normal;

	pMacPriv->pgp = 0;
	pMacPriv->pfm = 0;
    pMacPriv->forceMono = false;
	pMacPriv->privateEncoding = false;

    strcpy (fname, name);

    /* The ptr? below are set at the '-' just preceding the field named in the
       comment */

    if (!(*fname == '-') ||				/* foundry */
	    !(ptr1 = ptr = index(fname + 1, '-')) ||	/* family_name */
	    !(ptr2 = ptr = index(ptr + 1, '-')) ||	/* weight_name */
	    !(ptr3 = ptr = index(ptr + 1, '-')) ||	/* slant */
	    !(ptr4 = ptr = index(ptr + 1, '-')) ||	/* setwidth_name */
	    !(ptr5 = ptr = index(ptr + 1, '-')) ||	/* add_style_name */
	    !(ptr6 = ptr = index(ptr + 1, '-')) ||	/* pixel_size */
	    !(ptr7 = ptr = index(ptr + 1, '-')) ||	/* point_size */
	    !(ptr8 = ptr = index(ptr + 1, '-')) ||	/* resolution_x */
	    !(ptr9 = ptr = index(ptr + 1, '-')) ||	/* resolution_y */
	    !(ptr10 = ptr = index(ptr + 1, '-')) ||	/* spacing */
	    !(ptr11 = ptr = index(ptr + 1, '-')) ||	/* average_width */
	    !(ptr12 = ptr = index(ptr + 1, '-')) ||	/* charset_registry */
	    !(ptr13 = ptr = index(ptr + 1, '-')) ||	/* charset_encoding */
	    index(ptr + 1, '-'))			/* Too many '-'s ! */
	return BadFontName;
    
    family_name = ptr1 + 1;
    *ptr2 = '\0';

    weight_name = ptr2 + 1;
    *ptr3 = '\0';

    slant = ptr3 + 1;
    *ptr4 = '\0';

    setwidth_name = ptr4 + 1;
    *ptr5 = '\0';

    add_style_name = ptr5 + 1;
    *ptr6 = '\0';

    spacing = ptr10 + 1;
    *ptr11 = '\0';

    registry = ptr12 + 1;
    *ptr13 = '\0';

    encoding = ptr13 + 1; /* zero terminated at the end of name */
    

    if (!strcmp(weight_name, "bold"))
		face |= bold;

    if (!strcmp(slant, "i"))
		face |= italic;

    if (!strcmp(setwidth_name, "condensed"))
		face |= condense;

    if (!strcmp(setwidth_name, "extended"))
		face |= extend;

    if (!strcmp(add_style_name, "underline"))
		face |= underline;

    if (!strcmp(add_style_name, "outline"))
		face |= outline;

    if (!strcmp(add_style_name, "shadow"))
		face |= shadow;

    if (!strcmp(spacing, "m"))
		pMacPriv->forceMono = true; /* XXX Do we really want to offer this? */

	if (!strcmp(registry, "apple") && !strcmp(encoding, "fontspecific"))
		pMacPriv->privateEncoding = true;

    pgp = (GrafPtr) NewPtr(sizeof(GrafPort));
    if (!pgp) {
		return (AllocError);
    }

    pfm = (FMetricRec *) NewPtr(sizeof(FMetricRec));
    if (!pfm) {
		DisposPtr (pgp);
		return (AllocError);
    }

    GetPort (&savePort);
    OpenPort (pgp);

    getfnum (family_name, &fNum);

    if (!fNum) {
        char sysName [256];
        /* Per technote about font names */
        getfontname (systemFont, sysName);
        if (relstring (sysName, family_name, false, true)) 
		{
			DisposPtr (pfm);
			ClosePort (pgp);
			DisposPtr (pgp);
			SetPort (savePort);
			return (BadFontName);
		}
    }

    TextFont (fNum);
    TextSize (pixel);
    TextFace (face);
    FontMetrics (pfm);

    /* IM Volume 6 12-15 says: Be careful with outline fonts. The font's designer
	   may decide that there is a lower limit to the point size at which the
       font looks acceptable.  */
	if (CIsOutline(square, square) && !RealFont(fNum, pixel))
	{
		DisposPtr (pfm);
		ClosePort (pgp);
		DisposPtr (pgp);
		SetPort (savePort);
		return (BadFontName);
	}

    pMacPriv->pgp = pgp;
    pMacPriv->pfm = pfm;
    SetPort (savePort);

    return (Successful);                /* Eureka! */
}

int ConvertMacFont (pFont, pMacPriv, bit, byte, glyph, scan)
    FontPtr     pFont;
	MacBitmapFontRecPtr pMacPriv;
    int         bit, byte, glyph, scan;
{
    BitmapFontPtr  bitmapFont = (BitmapFontPtr) pFont->fontPrivate;
    register FontInfoPtr fi;            /* Font description */
    register CharInfoPtr cip;            /* Character being worked on */
    register CharInfoPtr *enc;
    register unsigned ch;
    int nchars, height;
    unsigned isoFirst, isoLast;
    int digitCount, digitWidths;
    Handle frH;                /* FontRec handle -- n.b.: might be sfnt rec handle */
    Point numer, denom;
    GrafPtr savePort;
    WidthTable *wtP;
    int maxWidth;
	short yMin, yMax;
    BitMap bm, gbm;            /* bm is bitmap for gp, gbm is bitmap for pGlyphs */
    CharInfoRec bmcip;
    xCharInfo bmink;
    Rect srcR, dstR;
    int bmorg, Mwidth;
    int gbmSize;
	OSErr ret;

	pMacPriv->avgWidth = 0; /* DECIpoints */
	pMacPriv->xHeight = 0;
	pMacPriv->quadWidth = 0;
	pMacPriv->weight = 0;
    
    GetPort (&savePort);
    SetPort (pMacPriv->pgp);

    SetCursor (*GetCursor (watchCursor));
    wtP = *(WidthTable **) pMacPriv->pfm->wTabHandle;
    frH = wtP -> tabFont;
    numer = wtP -> inNumer;
    denom = wtP -> inDenom;
    maxWidth = FixRound (pMacPriv->pfm->widMax) * 2;    /* Glyphs can be no wider than this */
	if (maxWidth < 32)
		maxWidth = 32;
	else if (maxWidth < 64)
		maxWidth = 64;
    bmorg = maxWidth / 3;                     /* Start glyphs 1/3 into glyph bitmap */
    
    /* Get first and last used character code values -- form of font record is 
       different if this is an outline (spline) font than if it's a bitmap font
       Must be done HERE before any alloc */
    if (CIsOutline (numer, denom)) {                                        
		/* Is an outline font, which is terra incognita */
        macFirst = 0;
        macLast = 255;
    } else {                                /*  Normal bitmap font */
        macFirst = (*(MacFontRec **) frH) -> firstChar;
        macLast = (*(MacFontRec **) frH) -> lastChar;
    }

    /* Find the minimum and maximum ISO characters in this font */
	if (pMacPriv->privateEncoding) {
		isoFirst = macFirst;
		isoLast = macLast;
	} else for (isoFirst = 255, isoLast = 0, ch = macFirst; ch <= macLast; ++ch)
        if (widthOf (ch) > 0) {
            unsigned isoCH = TOISO (ch);
            
            if (isoCH < isoFirst) isoFirst = isoCH;
            if (isoCH > isoLast)  isoLast = isoCH;
        }

    nchars = isoLast - isoFirst + 1;
    bitmapFont -> num_chars = nchars;
    fi = &(pFont -> info);

    bitmapFont -> metrics = (CharInfoPtr) xalloc (sizeof (CharInfoRec) * nchars);
    if (!bitmapFont -> metrics) goto memOops;

	for (cip = bitmapFont -> metrics, ch = isoFirst; ch <= isoLast; ++cip, ++ch)
		cip -> bits = 0; /* Helps at memOops */

    bitmapFont -> encoding = (CharInfoPtr *) xalloc (sizeof (CharInfoPtr *) * nchars);
    if (!bitmapFont -> encoding) goto memOops;
    
    fi -> firstRow = 0;
    fi -> lastRow = 0;
    fi -> firstCol = isoFirst;
    fi -> lastCol = isoLast;
    fi -> fontAscent = FixRound (pMacPriv->pfm->ascent);
    fi -> fontDescent = FixRound (pMacPriv->pfm->descent);
    fi -> allExist = TRUE; 
    
	if (pMacPriv->privateEncoding)
		fi -> defaultCh = macFirst; /* A likely candidate, but who really knows ... */
	else if (isoFirst < 32)
		fi -> defaultCh = isoFirst; /* Non-printing, renders as the empty box */
	else if (isoLast >= 128)
		fi -> defaultCh = 128; /* Non-printing, renders as the empty box */
	else
		fi -> defaultCh = isoFirst;
		
	/* IM Volume 6, 12-7 says: For bitmapped fonts, the ascent line marks the maximum y-value and
	   the descent line marks the minimum y-value. For outline fonts, a font designer can
	   create individual glyphs that extend above the ascent line or below the descent line. */

	if (CIsOutline(numer, denom)) {
		unsigned char isoChars[256], *p;

    	for (p = isoChars, ch = isoFirst; ch <= isoLast; ++p, ++ch)
			*p = pMacPriv->privateEncoding ? ch : TOMAC (ch);

		if ((ret = COutlineMetrics(nchars, isoChars, numer, denom, &yMax, &yMin, NULL, NULL, NULL))) {
			/* Shouldn't happen, do something rational */
			yMin = fi -> fontDescent;
			yMax = fi -> fontAscent;
		} else {
			yMin = -yMin; /* School of hard knocks... */
		}
	} else {
		yMin = fi -> fontDescent;
		yMax = fi -> fontAscent;
	}
	height = yMax + yMin;

    digitCount = digitWidths = 0;

    bm.bounds.left = bm.bounds.top = 0;
    bm.bounds.bottom = height;
    bm.bounds.right = maxWidth;
    bm.rowBytes = BYTES_PER_ROW(maxWidth, glyph); /* Must be even! see MacRenderFont*/

    if (pMacPriv->forceMono) Mwidth = widthOf ('M');
    
    bm.baseAddr = (Ptr) xalloc (bm.rowBytes * height); /* Space for glyphs to be drawn in */
    if (!bm.baseAddr) goto memOops;
    bzero (bm.baseAddr, bm.rowBytes * height);

    PortSize (bm.bounds.right, bm.bounds.bottom);
    SetPortBits (&bm);

    bmcip.metrics.ascent = yMax;
    bmcip.metrics.descent = yMin;
    bmcip.metrics.leftSideBearing = -bmorg;
    bmcip.metrics.rightSideBearing = maxWidth - bmorg;
    bmcip.metrics.characterWidth = maxWidth; /* for the sake of completeness */
    bmcip.metrics.attributes = 0; /* for the sake of completeness */
    bmcip.bits = bm.baseAddr;

    for (cip = bitmapFont -> metrics, enc = bitmapFont -> encoding, ch = isoFirst; 
         ch <= isoLast; 
         ++ch, ++enc, ++cip) {
		int macCH = pMacPriv->privateEncoding ? ch : TOMAC (ch);

		/* Take care not to lock up the UI for too long. (This is
		   especially important when querying all fonts with info
		   as we have to render all characters to get the info!) */
		if ((ch & 0x3F) == 0)
			MacCheckUI();
        
		*enc = cip;

        cip -> metrics.attributes = 0;
        cip -> metrics.characterWidth = widthOf (macCH);
        if (cip -> metrics.characterWidth <= 0) {
            cip -> metrics.ascent = 0;
            cip -> metrics.descent = 0;
            cip -> metrics.leftSideBearing = 0;
            cip -> metrics.rightSideBearing = 0;
            cip -> metrics.characterWidth = 0;
            cip -> bits = NULL;
    	    fi -> allExist = FALSE;
            continue;
        }
        /* Make sure advance width is constant if forceMono is set */
        if (pMacPriv->forceMono) cip -> metrics.characterWidth = Mwidth;

        EraseRect (&bm.bounds);
        MoveTo (bmorg, yMax);    /* Leave a margin to the left of glyph origin */
        DrawChar (macCH);

        /* Now find tight left and right bearings by examining the bits in the bm bitmap */
		if (maxWidth == 32) 
			FastCharInkMetrics32(pFont, &bmcip, &bmink);
		else if (maxWidth == 64) 
			FastCharInkMetrics64(pFont, &bmcip, &bmink);
		else 
			FontCharInkMetrics(pFont, &bmcip, &bmink);

		/* Some characters (e.g. 'space') have no inked bits, but positive characterWidth */
        if (bmink.ascent == 0 && bmink.descent == 0 && 
	        bmink.leftSideBearing == bmink.rightSideBearing) {
            cip -> metrics.ascent = 0;
            cip -> metrics.descent = 0;
            cip -> metrics.leftSideBearing = 0;
            cip -> metrics.rightSideBearing = 0;
            cip -> bits = NULL;
            continue;
        }

#ifndef notdef
    	if (bmcip.metrics.leftSideBearing == bmink.leftSideBearing ||
    		bmcip.metrics.rightSideBearing == bmink.rightSideBearing)
			printf("Bumped against bmcip! ch: 0x%x\n", macCH);
#endif

        cip -> metrics.ascent = bmink.ascent;
        cip -> metrics.descent = bmink.descent;
        cip -> metrics.leftSideBearing = bmink.leftSideBearing;
        cip -> metrics.rightSideBearing = bmink.rightSideBearing;

		srcR.left = bmorg + bmink.leftSideBearing;
		srcR.top = yMax - bmink.ascent;
		srcR.bottom = yMax + bmink.descent;
		srcR.right = bmorg + bmink.rightSideBearing;

        /* Set up glyph bitmap descriptor */
        gbm.bounds.left = gbm.bounds.top = 0;
        gbm.bounds.bottom = cip -> metrics.ascent + cip -> metrics.descent;
        gbm.bounds.right = cip -> metrics.rightSideBearing - cip -> metrics.leftSideBearing;
        gbm.rowBytes = BYTES_PER_ROW(gbm.bounds.right, glyph); /* Must be even! see MacRenderFont */
		gbmSize = gbm.rowBytes * (gbm.bounds.bottom - gbm.bounds.top);
		gbm.baseAddr = (Ptr) xalloc(gbmSize);
		if (!gbm.baseAddr) goto memOops;

		bzero(gbm.baseAddr, gbmSize);
		cip -> bits = gbm.baseAddr;

        /* Set up destination so that top edge of glyph is butted against the top of */
        /* the font glyph bitmap.  */

        dstR.left = dstR.top = 0;
        dstR.bottom = srcR.bottom - srcR.top;
        dstR.right = srcR.right - srcR.left;

        /* Copy glyph */
        CopyBits (&bm, &gbm, &srcR, &dstR, srcCopy, (RgnHandle) nil);

        if (bit == LSBFirst)
            BitOrderInvert(gbm.baseAddr, gbmSize);
        if (bit != byte) {
            if (scan == 2)
                TwoByteSwap(gbm.baseAddr, gbmSize);
            else if (scan == 4)
                FourByteSwap(gbm.baseAddr, gbmSize);
        }

		pMacPriv->avgWidth += gbm.bounds.right;

		if (ch == 'x')
			pMacPriv->xHeight = bmink.ascent;

        if (ch >= '0' && ch <= '9') {
            ++digitCount;
            digitWidths += gbm.bounds.right;
        }
    }
    
	pMacPriv->avgWidth = (nchars ? (pMacPriv->avgWidth * 10)/nchars : 0);
	pMacPriv->quadWidth = 
		(digitCount ? (int)((float) digitWidths / (float) digitCount) : 0);

    xfree (bm.baseAddr);
    SetPort (savePort);
    InitCursor ();
    return (Successful);

memOops:
    if (bitmapFont -> metrics) {
        for (cip = bitmapFont -> metrics, ch = isoFirst; ch <= isoLast; ++ch, ++cip) {
			if (cip -> bits) xfree(cip -> bits);
		}
        xfree(bitmapFont -> metrics);
    }
    if (bitmapFont -> encoding) xfree(bitmapFont -> encoding);
    if (bm.baseAddr) xfree(bm.baseAddr);
    if (gbm.baseAddr) xfree(gbm.baseAddr);
    return (AllocError);
}

