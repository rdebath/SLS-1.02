/* $XConsortium: t1funcs.c,v 1.10 92/05/12 18:07:55 gildea Exp $ */
/* Copyright International Business Machines,Corp. 1991
 * All Rights Reserved
 *
 * License, subject to the license given below, to use,
 * copy, modify, and distribute this software * and its
 * documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear
 * in all copies and that both that copyright notice and
 * this permission notice appear in supporting documentation,
 * and that the name of IBM not be used in advertising or
 * publicity pertaining to distribution of the software
 * without specific, written prior permission.
 *
 * IBM PROVIDES THIS SOFTWARE "AS IS", WITHOUT ANY WARRANTIES
 * OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING, BUT NOT
 * LIMITED TO ANY IMPLIED WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, AND NONINFRINGEMENT OF
 * THIRD PARTY RIGHTS.  THE ENTIRE RISK AS TO THE QUALITY AND
 * PERFORMANCE OF THE SOFTWARE, INCLUDING ANY DUTY TO SUPPORT
 * OR MAINTAIN, BELONGS TO THE LICENSEE.  SHOULD ANY PORTION OF
 * THE SOFTWARE PROVE DEFECTIVE, THE LICENSEE (NOT IBM) ASSUMES
 * THE ENTIRE COST OF ALL SERVICING, REPAIR AND CORRECTION.  IN
 * NO EVENT SHALL IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING
 * FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
 * CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
 * OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 *
 * Author: Jeffrey B. Lotspiech, IBM Almaden Research Center
 *   Modeled on spfuncs.c by Dave Lemke, Network Computing Devices, Inc
 *   which contains the following copyright and permission notices:
 *
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
 */
 
#include    <string.h>
#include    "X11/Xfuncs.h"
#include    "fontfilest.h"
#include    "FSproto.h"
#include    "t1intf.h"
 
#include "objects.h"
#include "spaces.h"
#include "regions.h"
#include "t1stdio.h"
#include "util.h"
#include "fontfcn.h"
 
int         Type1OpenScalable ();
static int  Type1GetGlyphs();
void        Type1CloseFont();
extern int  Type1GetInfoScalable ();
 
static int  Type1GetMetrics ();
 
static void fillrun();
 
 
extern psfont *FontP;
extern psobj *ISOLatin1EncArrayP;

extern unsigned long *Xalloc();
static void fill();
 
/*ARGSUSED*/
int Type1OpenScalable (fpe, ppFont, flags, entry, fileName, vals, format, fmask)
    FontPathElementPtr  fpe;
    FontPtr             *ppFont;
    int                 flags;
    FontEntryPtr        entry;
    char                *fileName;
    FontScalablePtr     vals;
    fsBitmapFormat      format;
    fsBitmapFormatMask  fmask;
{
       extern struct XYspace *IDENTITY;
       extern Bool fontfcnA();
       extern struct region *fontfcnB();
 
 
       FontPtr     pFont;
       int         bit,
                   byte,
                   glyph,
                   scan,
                   image;
       int pad,wordsize;     /* scan & image in bits                         */
       unsigned long *pool;  /* memory pool for ximager objects              */
       int size;             /* for memory size calculations                 */
       struct XYspace *S;    /* coordinate space for character               */
       struct region *area;
       double scale;         /* scale factor for font                        */
       CharInfoRec *glyphs;
       register int i;
       int len,rc;
       struct type1font *type1;
       char *p;
       psobj *fontencoding = NULL;

       /* set up default values */
       FontDefaultFormat(&bit, &byte, &glyph, &scan);
       /* get any changes made from above */
       rc = CheckFSFormat(format, fmask, &bit, &byte, &scan, &glyph, &image);
       if (rc != Successful)
               return rc;
 
       pad                = glyph * 8;
       wordsize           = scan * 8;
 
#define  PAD(bits, pad)  (((bits)+(pad)-1)&-(pad))
 
       pFont = (FontPtr) xalloc(sizeof(FontRec));
       if (pFont == NULL)
           return AllocError;
 
       type1 = (struct type1font *)xalloc(sizeof(struct type1font));
       if (type1 == NULL) {
               xfree(pFont);
               return AllocError;
       }
       bzero(type1, sizeof(struct type1font));
 
       /* heuristic for "maximum" size of pool we'll need: */
       size = 200000 + 120 * vals->pixel * sizeof(short);
       if (size < 0 || NULL == (pool = (unsigned long *) xalloc(size))) {
               xfree(type1);
               xfree(pFont);
               return AllocError;
       }
 
       addmemory(pool, size);
 
       scale = (double) vals->pixel;
       S = (struct XYspace *)Permanent(Scale(IDENTITY, scale, - scale));
 
       glyphs = type1->glyphs;
 
       /* load font if not already loaded */
       if (!fontfcnA(fileName, &rc))
	   return (rc);

       p = entry->name.name + entry->name.length - 19;
       if (entry->name.ndashes == 14 &&
	   p >= entry->name.name &&
	   !strcmp (p, "-adobe-fontspecific"))
       {
	   fontencoding = FontP->fontInfoP[ENCODING].value.data.arrayP;
       }

       if (!fontencoding)
	   fontencoding = ISOLatin1EncArrayP;

       for (i=0; i < 256-FIRSTCOL; i++) {
               long h,w;
               long paddedW;
	       char *codename;

	       codename = fontencoding[i + FIRSTCOL].data.valueP;
	       len = fontencoding[i + FIRSTCOL].len;
	       if (len == 7 && strcmp(codename,".notdef")==0)
		   continue;

               rc = 0;
               area = fontfcnB(S, codename, &len, &rc);
               if (rc < 0) {
                       rc = BadFontName;
                       break;
               }
               else if (rc > 0)
                       continue;
 
               if (area == NULL)
                       continue;
 
               h       = area->ymax - area->ymin;
               w       = area->xmax - area->xmin;
               paddedW = PAD(w, pad);
 
               if (h > 0 && w > 0) {
                       size = h * paddedW / 8;
                       glyphs[i].bits = (char *)xalloc(size);
                       if (glyphs[i].bits == NULL) {
                               rc = AllocError;
                               break;
                       }
               }
               else {
                       h = w = 0;
                       area->xmin = area->xmax = 0;
                       area->ymax = area->ymax = 0;
               }
 
               glyphs[i].metrics.leftSideBearing  = area->xmin;
               glyphs[i].metrics.characterWidth   = NEARESTPEL(area->ending.x - area->origin.x);
               glyphs[i].metrics.rightSideBearing = w + area->xmin;
               glyphs[i].metrics.descent          = area->ymax - NEARESTPEL(area->origin.y);
               glyphs[i].metrics.ascent           = h - glyphs[i].metrics.descent;
 
               if (h > 0 && w > 0) {
                       bzero(glyphs[i].bits, size);
                       fill(glyphs[i].bits, h, paddedW, area, byte, bit, wordsize );
               }
 
               Destroy(area);
       }
 
       delmemory();
       xfree(pool);
 
       if (i != 256 - FIRSTCOL) {
               for (i--; i >= 0; i--)
                       if (glyphs[i].bits != NULL)
                               xfree(glyphs[i].bits);
               xfree(type1);
               xfree(pFont);
               return rc;
       }
       type1->pDefault    = NULL;
 
       pFont->format      = format;
 
       pFont->bit         = bit;
       pFont->byte        = byte;
       pFont->glyph       = glyph;
       pFont->scan        = scan;
 
       pFont->info.firstCol = FIRSTCOL;
       pFont->info.lastCol  = 255;
       pFont->info.firstRow = 0;
       pFont->info.lastRow  = 0;
 
       pFont->get_metrics = Type1GetMetrics;
       pFont->get_glyphs  = Type1GetGlyphs;
       pFont->unload_font = Type1CloseFont;
       pFont->refcnt = 0;
       pFont->maxPrivate = -1;
       pFont->devPrivates = 0;
 
       pFont->fontPrivate = (unsigned char *) type1;
 
       T1FillFontInfo(pFont, vals, fileName, entry->name.name);
 
       *ppFont = pFont;
       return Successful;
}
 
static int
Type1GetGlyphs(pFont, count, chars, charEncoding, glyphCount, glyphs)
    FontPtr     pFont;
    unsigned long count;
    register unsigned char *chars;
    FontEncoding charEncoding;
    unsigned long *glyphCount;  /* RETURN */
    CharInfoPtr *glyphs;        /* RETURN */
{
    unsigned int firstRow;
    unsigned int numRows;
    CharInfoPtr *glyphsBase;
    register unsigned int c;
    register CharInfoPtr pci;
    unsigned int r;
    CharInfoPtr pDefault;
    register struct type1font *type1Font;
    register int firstCol;
 
    type1Font  = (struct type1font *) pFont->fontPrivate;
    firstCol   = pFont->info.firstCol;
    pDefault   = type1Font->pDefault;
    glyphsBase = glyphs;
 
    switch (charEncoding) {
 
    case Linear8Bit:
    case TwoD8Bit:
        if (pFont->info.firstRow > 0)
            break;
        while (count--) {
                c = (*chars++);
                if (c >= firstCol &&
                       (pci = &type1Font->glyphs[c-firstCol])->metrics.characterWidth != 0)
                    *glyphs++ = pci;
                else if (pDefault)
                    *glyphs++ = pDefault;
        }
        break;
    case Linear16Bit:
        while (count--) {
                c = *chars++ << 8;
                c = (c | *chars++);
                if (c < 256 && c >= firstCol &&
                        (pci = &type1Font->glyphs[c-firstCol])->metrics.characterWidth != 0)
                    *glyphs++ = pci;
                else if (pDefault)
                    *glyphs++ = pDefault;
        }
        break;
 
    case TwoD16Bit:
        firstRow = pFont->info.firstRow;
        numRows = pFont->info.lastRow - firstRow + 1;
        while (count--) {
            r = (*chars++) - firstRow;
            c = (*chars++);
            if (r < numRows && c < 256 && c >= firstCol &&
                    (pci = &type1Font->glyphs[(r << 8) + c - firstCol])->metrics.characterWidth != 0)
                *glyphs++ = pci;
            else if (pDefault)
                *glyphs++ = pDefault;
        }
        break;
    }
    *glyphCount = glyphs - glyphsBase;
    return Successful;
}
 
static int
Type1GetMetrics(pFont, count, chars, charEncoding, glyphCount, glyphs)
    FontPtr     pFont;
    unsigned long count;
    register unsigned char *chars;
    FontEncoding charEncoding;
    unsigned long *glyphCount;  /* RETURN */
    xCharInfo **glyphs;         /* RETURN */
{
    static CharInfoRec nonExistantChar;
 
    int         ret;
    struct type1font *type1Font;
    CharInfoPtr oldDefault;
 
    type1Font = (struct type1font *) pFont->fontPrivate;
    oldDefault = type1Font->pDefault;
    type1Font->pDefault = &nonExistantChar;
    ret = Type1GetGlyphs(pFont, count, chars, charEncoding, glyphCount, (CharInfoPtr *) glyphs);
    type1Font->pDefault = oldDefault;
    return ret;
}
 
void Type1CloseFont(pFont)
       FontPtr pFont;
{
       register int i;
       struct type1font *type1;
 
       type1 = (struct type1font *) pFont->fontPrivate;
       for (i=0; i < 256 - pFont->info.firstCol; i++)
               if (type1->glyphs[i].bits != NULL)
                        xfree(type1->glyphs[i].bits);
       xfree(type1);

       if (pFont->info.props)
	   xfree(pFont->info.props);

       if (pFont->info.isStringProp)
	   xfree(pFont->info.isStringProp);

       xfree(pFont);
}
 
 
 
static void fill(dest, h, w, area, byte, bit, wordsize)
       register char *dest;  /* destination bitmap                           */
       int h,w;              /* dimensions of 'dest', w padded               */
       register struct region *area;  /* region to write to 'dest'           */
       int byte,bit;         /* flags; LSBFirst or MSBFirst                  */
       int wordsize;         /* number of bits per word for LSB/MSB purposes */
{
       register struct edgelist *edge;  /* for looping through edges         */
       register char *p;     /* current scan line in 'dest'                  */
       register int y;       /* for looping through scans                    */
       register int wbytes = w / 8;  /* number of bytes in width             */
       register pel *leftP,*rightP;  /* pointers to X values, left and right */
       int xmin = area->xmin;  /* upper left X                               */
       int ymin = area->ymin;  /* upper left Y                               */
 
       for (edge = area->anchor; VALIDEDGE(edge); edge = edge->link->link) {
 
               p = dest + (edge->ymin - ymin) * wbytes;
               leftP = edge->xvalues;
               rightP = edge->link->xvalues;
 
               for (y = edge->ymin; y < edge->ymax; y++) {
                       fillrun(p, *leftP++ - xmin, *rightP++ - xmin, bit);
                       p += wbytes;
               }
       }
/*
Now, as an afterthought, we'll go reorganize if odd byte order requires
it:
*/
       if (byte == LSBFirst && wordsize != 8) {
               register int i;
 
               switch (wordsize) {
                   case 16:
                   {
                       register unsigned short data,*p;
 
                       p = (unsigned short *) dest;
 
                       for (i = h * w /16; --i >= 0;) {
                               data = *p;
                               *p++ = (data << 8) + (data >> 8);
                       }
                       break;
                   }
                   case 64:
                   case 32:
                   {
                       register unsigned long data,*p;
 
                       p = (unsigned long *) dest;
 
                       for (i = h * w / 32; --i >= 0;) {
                               data = *p;
                               *p++ = (data << 24) + (data >> 24)
                                      + (0xFF00 & (data >> 8))
                                      + (0xFF0000 & (data << 8));
                       }
                       if (wordsize == 64) {
 
                               p = (unsigned long *) dest;
 
                               for (i = h * w / 64; --i >= 0;) {
                                       data = *p++;
                                       p[-1] = p[0];
                                       *p++ = data;
                               }
                       }
                       break;
                   }
                   default:
                       abort("xiFill: unknown format");
               }
       }
 
}
 
#define  ALLONES  0xFF
 
static void fillrun(p, x0, x1, bit)
       register char *p;     /* address of this scan line                    */
       pel x0,x1;            /* left and right X                             */
       int bit;              /* format:  LSBFirst or MSBFirst                */
{
       register int startmask,endmask;  /* bits to set in first and last char*/
       register int middle;  /* number of chars between start and end + 1    */
 
       if (x1 <= x0)
               return;
       middle = x1/8 - x0/8;
       p += x0/8;
       x0 &= 7;  x1 &= 7;
       if (bit == LSBFirst) {
               startmask = ALLONES << x0;
               endmask = ~(ALLONES << x1);
       }
       else {
               startmask = ALLONES >> x0;
               endmask = ~(ALLONES >> x1);
       }
       if (middle == 0)
               *p++ |= startmask & endmask;
       else {
               *p++ |= startmask;
               while (--middle > 0)
                       *p++ = ALLONES;
               *p |= endmask;
       }
}
 
 
static FontRendererRec renderers[] = {
  { ".pfa", 4, (int (*)()) 0, Type1OpenScalable,
        (int (*)()) 0, Type1GetInfoScalable, 0 },
  { ".pfb", 4, (int (*)()) 0, Type1OpenScalable,
        (int (*)()) 0, Type1GetInfoScalable, 0 }
};
 
Type1RegisterFontFileFunctions()
{
    int i;
 
    T1InitStdProps();
    for (i=0; i < sizeof(renderers) / sizeof(FontRendererRec); i++)
            FontFileRegisterRenderer(&renderers[i]);
}
