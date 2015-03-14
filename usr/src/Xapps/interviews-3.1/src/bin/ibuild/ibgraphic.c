/*
 * Copyright (c) 1991 Stanford University
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
 * User interface builder-specific structured graphics implementations.
 */

#include "ibgraphic.h"
#include "ibvars.h"

#include <Unidraw/catalog.h>
#include <Unidraw/iterator.h>
#include <Unidraw/unidraw.h>

#include <InterViews/shape.h>
#include <InterViews/transformer.h>

/*****************************************************************************/

IBGraphic::IBGraphic (CanvasVar* c, Graphic* g) : Picture(g) { _canvasVar = c;}

Graphic* IBGraphic::Copy () {
    Iterator i;
    Graphic* copy = new IBGraphic(nil, this);

    for (First(i); !Done(i); Next(i)) {
        copy->Append(GetGraphic(i)->Copy());
    }
    return copy;
}

void IBGraphic::CalcExtent (
    int w, int h, 
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    transformRect(0, 0, float(w)/2, float(h)/2, l, b, cx, cy, gs);
    PSBrush* br = gs->GetBrush();
    float width = (br == nil) ? 0 : float(br->Width());
    tol = (width > 1) ? width/2 : 0;
}

void IBGraphic::concatGS (Graphic* a, Graphic* b, Graphic* dest) {
    int fill;
    PSColor* fg, *bg;
    PSFont* font;
    PSBrush* br;
    PSPattern* pat;
    
    if (a == nil) {
        *dest = *b;
        return;
    } else if (b == nil) {
        *dest = *a;
        return;
    }
    if ((fill = a->BgFilled()) == UNDEF) {
	fill = b->BgFilled();
    }
    dest->FillBg(fill);

    if ((fg = a->GetFgColor()) == nil) {
	fg = b->GetFgColor();
    }
    if ((bg = a->GetBgColor()) == nil) {
	bg = b->GetBgColor();
    }
    dest->SetColors(fg, bg);

    if ((pat = a->GetPattern()) == nil) {
	pat = b->GetPattern();
    }
    dest->SetPattern(pat);

    if ((font = a->GetFont()) == nil) {
	font = b->GetFont();
    }
    dest->SetFont(font);

    if ((br = a->GetBrush()) == nil) {
	br = b->GetBrush();
    }
    dest->SetBrush(br);
}

boolean IBGraphic::contains (PointObj& po, Graphic* gs) {
    return Graphic::contains(po, gs);
}

boolean IBGraphic::intersects (BoxObj& userb, Graphic* gs) {
    return Graphic::intersects(userb, gs);
}

void IBGraphic::ReadGS (istream& in) {
    Catalog* catalog = unidraw->GetCatalog();

    FillBg(catalog->ReadBgFilled(in));
    SetBrush(catalog->ReadBrush(in));
    PSColor* fg = catalog->ReadColor(in);
    PSColor* bg = catalog->ReadColor(in);
    SetColors(fg, bg);
    SetFont(catalog->ReadFont(in));
    SetPattern(catalog->ReadPattern(in));

    Transformer* t = catalog->ReadTransformer(in);
    SetTransformer(t);
    Unref(t);
}

void IBGraphic::WriteGS (ostream& out) {
    Catalog* catalog = unidraw->GetCatalog();

    catalog->WriteBgFilled(BgFilled(), out);
    catalog->WriteBrush(GetBrush(), out);
    catalog->WriteColor(GetFgColor(), out);
    catalog->WriteColor(GetBgColor(), out);
    catalog->WriteFont(GetFont(), out);
    catalog->WritePattern(GetPattern(), out);
    catalog->WriteTransformer(GetTransformer(), out);
}

/*****************************************************************************/

HVGraphic::HVGraphic (CanvasVar* c, Graphic* g) : IBGraphic(c, g) {
    _minorAxisSize = 1; 
}

int HVGraphic::MinorAxisSize () { return _minorAxisSize; }

void HVGraphic::SetMinorAxisSize (int w) { _minorAxisSize = w; }

void HVGraphic::Init (int nat, int shr, int str, Orientation o, int w) {
    _natural = nat;
    _shrink = shr;
    _stretch = str;
    _orientation = o;
    _minorAxisSize = w;
}

void HVGraphic::GetShape (int& nat, int& shr, int& str) {
    nat = _natural;
    shr = _shrink;
    str = _stretch;
}

void HVGraphic::SetShape (int nat, int shr, int str) {
    _natural = nat;
    _shrink = shr;
    _stretch = str;
}

void HVGraphic::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    CanvasVar* cvar = GetCanvasVar();

    if (cvar == nil) {
        if (_orientation == Horizontal) {
            CalcExtent(_natural, MinorAxisSize(), l, b, cx, cy, tol, gs);
        } else {
            CalcExtent(MinorAxisSize(), _natural, l, b, cx, cy, tol, gs);
        }            
    } else {
        CalcExtent(cvar->Width(), cvar->Height(), l,b,cx,cy,tol,gs);
    }
    tol = 0;
}

void HVGraphic::drawClipped (
    Canvas* c, Coord l, Coord b, Coord r, Coord t, Graphic* gs
) {
    Graphic::drawClipped(c, l, b, r, t, gs);
}
