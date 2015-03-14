/*
 * Copyright (c) 1990, 1991 Stanford University
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Stanford not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Stanford makes no representations about
 * the suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * STANFORD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
 * IN NO EVENT SHALL STANFORD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * IBuildCatalog implementation.
 */

#include "ibcatalog.h"
#include "ibcmds.h"
#include "ibellipse.h"
#include "ibgrcomp.h"
#include "ibline.h"
#include "ibpolygon.h"
#include "ibraster.h"
#include "ibrect.h"
#include "ibspline.h"
#include "ibstencil.h"
#include "ibtext.h"

#include <Unidraw/Components/psformat.h>
#include <Unidraw/Components/text.h>

#include <Unidraw/Graphic/ellipses.h>
#include <Unidraw/Graphic/lines.h>
#include <Unidraw/Graphic/polygons.h>
#include <Unidraw/Graphic/rasterrect.h>
#include <Unidraw/Graphic/splines.h>
#include <Unidraw/Graphic/ustencil.h>

#include <InterViews/bitmap.h>
#include <InterViews/raster.h>
#include <InterViews/textbuffer.h>
#include <InterViews/transformer.h>

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/

// octal converts a string of three octal digits to a character.

static char octal(const char* p) {
    char c = *p - '0';
    c = c*8 + *++p - '0';
    c = c*8 + *++p - '0';
    return c;
}

/*****************************************************************************/

char IBuildCatalog::_buf[CHARBUFSIZE];
float IBuildCatalog::_psversion;

IBuildCatalog::IBuildCatalog (
    const char* name, Creator* creator, float version
) : Catalog(name, creator, version) {
    _psversion = PSV_ORIGINAL;
}

boolean IBuildCatalog::Retrieve (const char* name, EditorInfo*& o) {
    return Catalog::Retrieve(name, o);
}

boolean IBuildCatalog::Retrieve (const char* name, Command*& o) {
    return Catalog::Retrieve(name, o);
}

boolean IBuildCatalog::Retrieve (const char* name, Tool*& o) {
    return Catalog::Retrieve(name, o);
}

boolean IBuildCatalog::Retrieve (const char* name, Component*& comp) {
    _valid = false;

    if (Valid(name, comp)) {
        _valid = true;

    } else if (UnidrawFormat(name)) {
        _valid = Catalog::Retrieve(name, comp);

    } else if (!IBViewCompCmd::Executing()) {
        filebuf fbuf;
        _valid = fbuf.open(name, input) != 0;

        if (_valid) {
            istream in(&fbuf);
            comp = ReadPostScript(in);

            if (_valid) {
                Forget(comp, name);
                Register(comp, name);
            }
        }
    }
    return _valid;
}

boolean IBuildCatalog::UnidrawFormat (const char* name) {
    filebuf fbuf;
    boolean unidraw_format = false;

    if (fbuf.open(name, input) != 0) {
        istream in(&fbuf);

        Skip(in);
        in >> _buf;

        if (strcmp(_buf, "Unidraw") == 0) {
            unidraw_format = true;
        }
    }
    return unidraw_format;
}

GraphicComp* IBuildCatalog::ReadPostScript (istream& in) {
    Skip(in);
    in >> _buf >> _psversion;

    if (_psversion > PSV_LATEST) {
        fprintf(stderr, "warning: drawing version %d ", _psversion);
        fprintf(stderr, "newer than idraw version %d\n", PSV_LATEST);
    }

    float xincr, yincr;
    PSReadGridSpacing(in, xincr, yincr);
    IGraphicComps* comp = new IGraphicComps;

    if (_psversion < PSV_NONREDUNDANT) {
        Skip(in);
    }

    Graphic* g = comp->GetGraphic();
    Transformer* t = g->GetTransformer();

    PSReadPictGS(in, g);
    PSReadChildren(in, comp);
    ScaleToScreenCoords(g);

    if (_psversion < PSV_NONROTATED && t != nil && t->Rotated90()) {
        Transformer identity;
        *t = identity;
        g->Translate(0.0, -8.5*inches);
        g->Rotate(90.0, 0.0, 0.0);
        comp->Bequeath();
    }

    _valid = in.good();
    return comp;
}

/*
 * ScaleToScreenCoords scales the picture back to screen coordinates
 * if screen and Postscript inches are different.
 */

void IBuildCatalog::ScaleToScreenCoords (Graphic* g) {
    const double postscriptinch = 72.;

    if (inch != postscriptinch) {
	double toscreen = inch / postscriptinch;
	g->Scale(toscreen, toscreen);
    }
}

/*
 * reads data to initialize graphic comp and create children
 */

GraphicComp* IBuildCatalog::ReadPict (istream& in) {
    FullGraphic gs;
    PSReadPictGS(in, &gs);
    IGraphicComps* pict = new IGraphicComps;
    *pict->GetGraphic() = gs;
    PSReadChildren(in, pict);
    _valid = in.good();
    return pict;
}

/*
 * PSReadChildren loops determining which kind of Component follows and
 * creating it until it reads "end" which means all of the children
 * have been created.
*/

void IBuildCatalog::PSReadChildren (istream& in, GraphicComp* comp) {
    while (in.good()) {
	Skip(in);
	GraphicComp* child = nil;
	in >> _buf;

	if (strcmp(_buf, "BSpl") == 0) 		child = ReadBSpline(in);
	else if (strcmp(_buf, "Circ") == 0) 	child = ReadCircle(in);
	else if (strcmp(_buf, "CBSpl") == 0)    child = ReadClosedBSpline(in);
	else if (strcmp(_buf, "Elli") == 0)     child = ReadEllipse(in);
	else if (strcmp(_buf, "Line") == 0)     child = ReadLine(in);
	else if (strcmp(_buf, "MLine") == 0)    child = ReadMultiLine(in);
	else if (strcmp(_buf, "Pict") == 0)     child = ReadPict(in);
	else if (strcmp(_buf, "Poly") == 0)     child = ReadPolygon(in);
	else if (strcmp(_buf, "Rect") == 0)     child = ReadRect(in);
	else if (strcmp(_buf, "Text") == 0)     child = ReadText(in);
	else if (strcmp(_buf, "SSten") == 0)    child = ReadSStencil(in);
	else if (strcmp(_buf, "FSten") == 0)    child = ReadFStencil(in);
	else if (strcmp(_buf, "Rast") == 0)     child = ReadRaster(in);
	else if (strcmp(_buf, "eop") == 0)      break;

	else {
	    fprintf(stderr, "unknown graphical object %s, skipping\n", _buf);
	    continue;
	}

	if (child != nil) {
	    if (in.good()) {
		comp->Append(child);
	    } else {
		delete child;
	    }
	}
    }
}

/*
 * PSReadGridSpacing reads the grid spacing used by the drawing and
 * stores the new grid spacing value.  It must correct the default
 * grid spacing it gives to old drawings for an implementation botch
 * in InterViews 2.4 that calculated point's value incorrectly using
 * 72.07/inch instead of inch/72.27 (it was a botch in TWO ways).
 */

void IBuildCatalog::PSReadGridSpacing (istream& in, float& xincr, float& yincr){
    if (_psversion < PSV_GRIDSPACING) {
	const int oldspacing = 8;
	const double oldpoints = 72.07/inches;
	xincr = yincr = oldpoints * round(oldspacing * oldpoints);

    } else {
	in >> _buf;

	if (strcmp(_buf, "Grid") == 0) {
 	    in >> xincr;

            if (_psversion < PSV_UNIDRAW) {
                yincr = xincr;
            } else {
                in >> yincr;
            }
	}
    }
}

/*
 * PSReadGS reads data to initialize the graphic for IGraphicComps
 * which don't contain any text.
 */

void IBuildCatalog::PSReadGS (istream& in, Graphic* gs) {
    PSReadBrush(in, gs);

    if (_psversion >= PSV_FGANDBGCOLOR) {
	PSReadFgColor(in, gs);
	PSReadBgColor(in, gs);
	gs->SetFont(nil);

    } else if (_psversion >= PSV_FGCOLOR) {
	PSReadFgColor(in, gs);
	gs->SetColors(gs->GetFgColor(), pswhite);
	gs->SetFont(nil);

    } else {
	gs->SetColors(psblack, pswhite);
	PSReadFont(in, gs);
    }

    PSReadPattern(in, gs);
    PSReadTransformer(in, gs);
}

/*
 * PSReadPictGS reads data to initialize the state of GraphicComps
 * which may contain some text.
 */

void IBuildCatalog::PSReadPictGS (istream& in, Graphic* gs) {
    PSReadBrush(in, gs);

    if (_psversion >= PSV_FGANDBGCOLOR) {
	PSReadFgColor(in, gs);
	PSReadBgColor(in, gs);

    } else if (_psversion >= PSV_FGCOLOR) {
	PSReadFgColor(in, gs);
	gs->SetColors(gs->GetFgColor(), nil);

    } else {
	gs->SetColors(nil, nil);
    }

    PSReadFont(in, gs);
    PSReadPattern(in, gs);
    PSReadTransformer(in, gs);
}

/*
 * PSReadTextGS reads data to initialize the graphic gs for
 * ITextComp which doesn't need a brush or pattern.
 */

void IBuildCatalog::PSReadTextGS (istream& in, Graphic* gs) {
    if (_psversion >= PSV_FGCOLOR) {
	gs->SetBrush(nil);
	PSReadFgColor(in, gs);
	gs->SetColors(gs->GetFgColor(), nil);

    } else {
	PSReadBrush(in, gs);
	gs->SetColors(psblack, nil);
    }

    PSReadFont(in, gs);

    if (_psversion < PSV_NONREDUNDANT) {
	PSReadPattern(in, gs);
	PSPattern* pattern = gs->GetPattern();
	float graylevel = pattern->GetGrayLevel();
	const char* c = "Black";
	int r = 0, g = 0, b = 0;

	if (graylevel != 0 && graylevel != -1) {
	    if (graylevel == 1) {
		c = "White";
		r = g = b = 65535;
	    } else {
		c = "Gray";
		r = g = b = 49152;
	    }
	}
	PSColor* fgcolor = FindColor(c, r, g, b);
	gs->SetColors(fgcolor, nil);

    } else {
	gs->SetPattern(nil);
    }

    PSReadTransformer(in, gs);

    if (_psversion < PSV_TEXTOFFSET) {
        PSFont* f = gs->GetFont();
        float sep = f->GetLineHt() - f->Height() - 1;
        CorrectTextVPos(gs, sep);
    }
    PSFont* f = gs->GetFont();
    float sep = 1 - f->GetLineHt();
    CorrectTextVPos(gs, sep);
}

void IBuildCatalog::CorrectTextVPos (Graphic* gs, float sep) {
    PSFont* f = gs->GetFont();
    Transformer* t = gs->GetTransformer();
    float dx = 0., dy = sep;

    if (t != nil) {
        float x0, y0, x1, y1;
        t->Transform(0., 0., x0, y0);
        t->Transform(0., sep, x1, y1);
        dx = x1 - x0;
        dy = y1 - y0;
    }
    gs->Translate(dx, dy);
}

/*
 * PSReadBrush reads data to set the IGraphicComp's brush.
 */

void IBuildCatalog::PSReadBrush (istream& in, Graphic* gs) {
    Skip(in);
    in >> _buf;

    if (_buf[0] == 'b') {
	char lookahead = 'u';
	boolean undefined = false;
	boolean none = false;
	int p = 0;
	int w = 0;
	int head = false;
	int tail = false;
 
	in >> lookahead;
	in.putback(lookahead);
	switch (lookahead) {
	case 'u':
	    undefined = true;
	    break;
	case 'n':
	    none = true;
	    break;
	default:
	    in >> p >> w >> head >> tail;
	    break;
	}

	if (undefined || !in.good()) {
	    gs->SetBrush(nil);

	} else {
            PSBrush* brush = none ? FindNoneBrush() : FindBrush(p,w);

	    gs->SetBrush(brush);
	}
    }
}

/*
 * PSReadFgColor reads data to set the IGraphicComp's foreground color.
 */

void IBuildCatalog::PSReadFgColor (istream& in, Graphic* gs) {
    Skip(in);
    in >> _buf;

    if (
        _buf[0] == 'c' && (_buf[1] == 'f' || _psversion < PSV_FGANDBGCOLOR)
    ) {
	char lookahead = 'u';
	boolean undefined = false;
	char name[100];
	float fr = 0, fg = 0, fb = 0;

	in >> lookahead; 
	in.putback(lookahead);

	if (lookahead == 'u') {
	    undefined = true;

	} else {		
	    in >> name;

	    if (_psversion >= PSV_FGANDBGCOLOR) {
		in >> fr >> fg >> fb;
	    }
	}

	if (undefined || !in.good()) {
	    gs->SetColors(nil, gs->GetBgColor());

	} else {
	    int r = round(fr * 0xffff);
	    int g = round(fg * 0xffff);
	    int b = round(fb * 0xffff);
	    PSColor* fgcolor = FindColor(name, r, g, b);
	    gs->SetColors(fgcolor, gs->GetBgColor());
	}
    }
}

/*
 * PSReadBgColor reads data to set the IGraphicComp's background color.
 */

void IBuildCatalog::PSReadBgColor (istream& in, Graphic* gs) {
    Skip(in);
    in >> _buf;

    if (_buf[0] == 'c' && _buf[1] == 'b') {
	char lookahead = 'u';
	boolean undefined = false;
	char name[100];
	float fr = 0, fg = 0, fb = 0;

	in >> lookahead;
	in.putback(lookahead);

	if (lookahead == 'u') {
	    undefined = true;
	} else {
	    in >> name >> fr >> fg >> fb;
	}

	if (undefined || !in.good()) {
	    gs->SetColors(gs->GetFgColor(), nil);

	} else {
	    int r = round(fr * 0xffff);
	    int g = round(fg * 0xffff);
	    int b = round(fb * 0xffff);
	    PSColor* bgcolor = FindColor(name, r, g, b);
	    gs->SetColors(gs->GetFgColor(), bgcolor);
	}
    }
}

/*
 * PSReadFont reads data to set the IGraphicComp's font.
 */

void IBuildCatalog::PSReadFont (istream& in, Graphic* gs) {
    Skip(in);
    in >> _buf;

    if (_buf[0] == 'f') {
	char lookahead = 'u';
	boolean undefined = false;

	char name[CHARBUFSIZE];
	char printfont[CHARBUFSIZE];
	char printsize[CHARBUFSIZE];

	in >> lookahead;
	in.putback(lookahead);

	if (lookahead == 'u') {
	    undefined = true;

	} else {
	    in.get(name, CHARBUFSIZE);
	    in >> printfont;
	    in >> printsize;
	}

	if (undefined || !in.good()) {
	    gs->SetFont(nil);

	} else {
	    char* pf = printfont;
	    if (
		_psversion >= PSV_NONREDUNDANT
		&& _psversion < PSV_ISOLATIN1
	    ) {
		pf = &printfont[1];
	    }

	    PSFont* font = FindFont(name, pf, printsize);
	    gs->SetFont(font);
	}
    }
}

/*
 * CalcGrayLevel calculates a 4x4 bitmap's gray level on the printer.
 * Since the gray level ranges in 0 = solid to 1 = clear,
 * CalcGrayLevel counts the number of 0 bits in the bitmap and divides
 * the sum by the total number of bits in the bitmap.
 */

float IBuildCatalog::CalcGrayLevel (int seed) {
    const int numbits = 16;
    int numzeros = 0;

    for (int i = 0; i < numbits; i++) {
        numzeros += !((seed >> i) & 0x1);
    }
    return float(numzeros) / numbits;
}

/*
 * PSReadPattern reads data to set the IGraphicComp's pattern.
 */

void IBuildCatalog::PSReadPattern (istream& in, Graphic* gs) {
    Skip(in);
    in >> _buf;

    if (_buf[0] == 'p') {
	char lookahead = 'u';
	boolean undefined = false;
	boolean none = false;
	float graylevel = 0;
	int data[patternHeight];
	int size = 0;

	in >> lookahead;

	switch (lookahead) {
	case 'u':
	    undefined = true;
	    break;
	case 'n':
	    none = true;
	    break;
	case '<':
	    graylevel = -1;
	    break;
	default:
	    in.putback(lookahead);
	    break;
	}

	if (!undefined && !none && graylevel != -1) {
	    if (_psversion >= PSV_FGANDBGCOLOR) {
		in >> graylevel;
	    } else {
		in >> data[0];
		graylevel = CalcGrayLevel(data[0]);
	    }
	} else if (graylevel == -1) {
	    for (int i = 0; in >> _buf && i < patternHeight; i++) {
		if (_buf[0] == '>' || sscanf(_buf, "%x", &data[i]) != 1) {
		    break;
		}
	    }
	    if (_buf[0] == '>') {
		size = i;
	    } else {
		undefined = true;
	    }
	}

	if (undefined || !in.good()) {
	    gs->SetPattern(nil);
	} else {
            PSPattern* pattern;
            if (none) {
                pattern = FindNonePattern();
            } else if (graylevel == -1) {
	        pattern = FindPattern(data, size);
            } else {
	        pattern = FindGrayLevel(graylevel);
            }
	    gs->SetPattern(pattern);
	}
    }
}

/*
 * PSReadTransformer reads data to set the IGraphicComp's transformation
 * matrix.
 */

void IBuildCatalog::PSReadTransformer (istream& in, Graphic* gs) {
    Skip(in);
    in >> _buf;

    if (_buf[0] == 't') {
	char uorbracket = 'u';
	boolean undefined = false;
	float a00, a01, a10, a11, a20, a21;

	in >> uorbracket;

	if (uorbracket == 'u') {
	    undefined = true;
	} else {
	    if (_psversion < PSV_NONREDUNDANT) {
		in.putback(uorbracket);
	    }
	    in >> a00 >> a01 >> a10 >> a11 >> a20 >> a21;
	}

	if (in.good() && !undefined) {
            Transformer* t = new Transformer(a00, a01, a10, a11, a20, a21);
	    gs->SetTransformer(t);
            Unref(t);
	}
    }
}

/*
 * PSReadPoints reads a set of points as efficiently as possible by
 * using dynamic static buffers instead of mallocing on every call.
 */

void IBuildCatalog::PSReadPoints (istream& in, const Coord*& x, const Coord*& y,
int& n) {
    const int INITIALSIZE = 15;
    static int sizepoints = 0;
    static Coord* xcoords = nil;
    static Coord* ycoords = nil;

    Skip(in);
    in >> n;

    if (n > sizepoints) {
        delete xcoords;
        delete ycoords;
        sizepoints = max(n, INITIALSIZE);
        xcoords = new Coord[sizepoints];
        ycoords = new Coord[sizepoints];
    }

    for (int i = 0; i < n; i++) {
        if (_psversion < PSV_NONREDUNDANT) {
            Skip(in);
        }
        in >> xcoords[i] >> ycoords[i];
    }

    x = xcoords;
    y = ycoords;
}

/*
 * ReadBSpline reads data to initialize its graphic comp and
 * create its components.
 */

GraphicComp* IBuildCatalog::ReadBSpline (istream& in) {
    FullGraphic gs;
    PSReadGS(in, &gs);
    Coord* x, *y;
    int n;

    const Coord* cx, * cy;
    PSReadPoints(in, cx, cy, n);
    x = (Coord*)cx; y = (Coord*)cy;

    float mag;
    if (_psversion < PSV_UNIDRAW) {
        mag = 1.;
    } else {
        Skip(in);
        in >> mag;
    }
    return new ISplineComp(
        new SFH_OpenBSpline(x, y, n, &gs)
    );
}

/*
 * ReadClosedBSpline reads data to initialize its graphic comp
 * and create the closed B-spline's filled interior and outline.
 */

GraphicComp* IBuildCatalog::ReadClosedBSpline (istream& in) {
    FullGraphic gs;
    PSReadGS(in, &gs);
    Coord* x, *y;
    int n;

    const Coord* cx, * cy;
    PSReadPoints(in, cx, cy, n);
    x = (Coord*)cx; y = (Coord*)cy;
    return new IClosedSplineComp(new SFH_ClosedBSpline(x, y, n, &gs));
}

/*
 * ReadRect reads data to initialize its graphic comp and create
 * its filled interior and outline.
 */

GraphicComp* IBuildCatalog::ReadRect (istream& in) {
    FullGraphic gs;
    PSReadGS(in, &gs);
    Skip(in);
    Coord l, b, r, t;
    in >> l >> b >> r >> t;
    return new IRectComp(new SF_Rect(l, b, r, t, &gs));
}

GraphicComp* IBuildCatalog::ReadPolygon (istream& in) {
    FullGraphic gs;
    PSReadGS(in, &gs);
    Coord* x, *y;
    int n;

    const Coord* cx, * cy;
    PSReadPoints(in, cx, cy, n);
    x = (Coord*)cx; y = (Coord*)cy;
    return new IPolygonComp(new SF_Polygon(x, y, n, &gs));
}

/*
 * ReadLine reads data to initialize its graphic comp and create
 * its line.
 */

GraphicComp* IBuildCatalog::ReadLine (istream& in) {
    FullGraphic gs;
    PSReadGS(in, &gs);
    Skip(in);
    Coord x0, y0, x1, y1;
    in >> x0 >> y0 >> x1 >> y1;

    float mag;
    if (_psversion < PSV_UNIDRAW) {
        mag = 1.;
    } else {
        Skip(in);
        in >> mag;
    }
    return new ILineComp(new Line(x0, y0, x1, y1, &gs));
}

/*
 * ReadMultiLine reads data to initialize its graphic comp and
 * create its components.
 */

GraphicComp* IBuildCatalog::ReadMultiLine (istream& in) {
    FullGraphic gs;
    PSReadGS(in, &gs);
    Coord* x, *y;
    int n;

    const Coord* cx, * cy;
    PSReadPoints(in, cx, cy, n);
    x = (Coord*)cx; y = (Coord*)cy;

    float mag;
    if (_psversion < PSV_UNIDRAW) {
        mag = 1.;
    } else {
        Skip(in);
        in >> mag;
    }
    return new IMultiLineComp(new SF_MultiLine(x, y, n, &gs));
}

/*
 * ReadEllipse reads data to initialize its graphic comp and
 * create its filled interior and outline.
 */

GraphicComp* IBuildCatalog::ReadEllipse (istream& in) {
    FullGraphic gs;
    PSReadGS(in, &gs);
    Skip(in);
    Coord x0, y0;
    int rx, ry;
    in >> x0 >> y0 >> rx >> ry;
    return new IEllipseComp(new SF_Ellipse(x0, y0, rx, ry, &gs));
}

/*
 * ReadCircle reads data to initialize its graphic comp and
 * create its filled interior and outline.
 */

GraphicComp* IBuildCatalog::ReadCircle (istream& in) {
    FullGraphic gs;
    PSReadGS(in, &gs);
    Skip(in);
    Coord x0, y0;
    int r;
    in >> x0 >> y0 >> r;
    return new IEllipseComp(new SF_Circle(x0, y0, r, &gs));
}

/*
 * ReadText reads its graphic comp and text in a file.
 */

GraphicComp* IBuildCatalog::ReadText (istream& in) {
    FullGraphic gs;
    PSReadTextGS(in, &gs);
    const int sbuf_size = 10000;
    char sbuf[sbuf_size];
    PSReadTextData(in, sbuf, sbuf_size);

    int lineHt = 0;

    PSFont* f = gs.GetFont();
    if (f != nil) lineHt = f->GetLineHt();

    TextGraphic* tg = new TextGraphic(sbuf, lineHt, &gs); 
    tg->FillBg(false);
    return new ITextComp(tg);
}

GraphicComp* IBuildCatalog::ReadSStencil (istream& in) {
    FullGraphic gs;
    PSReadFgColor(in, &gs);
    PSReadBgColor(in, &gs);
    PSReadTransformer(in, &gs);
    Skip(in);
    Coord w, h;
    in >> w >> h;

    Bitmap* bitmap = new Bitmap((void*) nil, w, h);
    ReadBitmapData(bitmap, in);

    return new IStencilComp(new UStencil(bitmap, bitmap, &gs));
}

GraphicComp* IBuildCatalog::ReadFStencil (istream& in) {
    FullGraphic gs;
    PSReadFgColor(in, &gs);
    PSReadBgColor(in, &gs);
    PSReadTransformer(in, &gs);
    Skip(in);
    Coord w, h;
    in >> w >> h;

    Bitmap* bitmap = new Bitmap((void*) nil, w, h);
    ReadBitmapData(bitmap, in);

    return new IStencilComp(new UStencil(bitmap, nil, &gs));
}

GraphicComp* IBuildCatalog::ReadRaster (istream& in) {
    FullGraphic gs;
    PSReadTransformer(in, &gs);
    Skip(in);
    Coord w, h;
    in >> w >> h;

    char* sync_string = "colorimage";
    int n = strlen(sync_string);

    while (GetToken(in, _buf, CHARBUFSIZE) != 0) {
        if (strncmp("colorimage", _buf, n) == 0) {
            break;
        }
    }

    Raster* raster = new Raster(w, h);
    ReadRasterData(raster, in);

    return new IRasterComp(new RasterRect(raster, &gs));
}

/*
 * PSReadTextData reads and returns the text contained in the PostScript
 * representation of the ReadText.
 */

void IBuildCatalog::PSReadTextData (istream& in, char* sbuf, int len) {
    TextBuffer stext(sbuf, 0, len);
    char nl = '\n';
    char null = '\0';

    if (_psversion >= PSV_EIGHTBIT) {
	Skip(in);
	char c = ' ';
        int dot = 0;

	while (in >> c && c != ']') {
	    while (c != '(' && in.get(c));

	    while (in.get(c) && c != ')') {
		if (c == '\\') {
		    in.get(c);

		    if (isdigit(c)) {
			char buf[4];
			buf[0] = c;
			in.get(buf[1]);
			in.get(buf[2]);
			buf[3] = '\0';
			c = octal(buf);
		    }
		}
		dot += stext.Insert(dot, &c, 1);
	    }
	    dot += stext.Insert(dot, "\n", 1);
	}
	stext.Delete(--dot, 1); // buffer must not terminate in '\n'
    
    } else if (_psversion >= PSV_NONREDUNDANT) {
	Skip(in);
	char c = ' ';
        int dot = 0;

	while (in >> c && c != ']') {
	    while (c != '(' && in.get(c));

	    while (in.get(c) && c != ')') {
		if (c == '\\') {
		    in.get(c);
		}
		stext.Insert(dot++, &c, 1);
	    }
	    stext.Insert(dot++, &nl, 1);
	}
	stext.Delete(--dot, 1); // buffer must not terminate in '\n'

    } else {
        int dot = 0;

	while (in >> _buf && strcmp(_buf, MARK) == 0) {
	    char blank;
	    in.get(blank);
	    in.get(_buf, CHARBUFSIZE - 1);
	    int buflen = strlen(_buf) + 1;
	    _buf[buflen - 1] = '\n';
	    stext.Insert(dot, _buf, buflen);
	    dot += buflen;
	}
	stext.Delete(--dot, 1); // buffer must not terminate in '\n'
    }
    stext.Insert(stext.Length(), &null, 1);
}
