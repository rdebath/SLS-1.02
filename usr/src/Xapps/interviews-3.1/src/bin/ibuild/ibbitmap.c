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
 * Bitmap component definitions.
 */

#include "ibbitmap.h"
#include "ibclasses.h"

#include <Unidraw/catalog.h>
#include <Unidraw/manips.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/creator.h>
#include <Unidraw/Tools/tool.h>
#include <Unidraw/iterator.h>

#include <InterViews/bitmap.h>
#include <InterViews/painter.h>
#include <InterViews/transformer.h>

#include <stream.h>
#include <string.h>

/*****************************************************************************/

static const int pad = 0;

/*****************************************************************************/

BitmapGraphic* BitmapComp::GetBitmapGraphic () {
    return (BitmapGraphic*) GetGraphic();
}

ClassId BitmapComp::GetClassId () { return BITMAP_COMP; }

boolean BitmapComp::IsA (ClassId id) {
    return BITMAP_COMP == id || GraphicComp::IsA(id);
}

BitmapComp::BitmapComp (BitmapGraphic* g) : GraphicComp(g) {}

void BitmapComp::Read (istream& in) {
    GraphicComp::Read(in);
    Catalog* catalog = unidraw->GetCatalog();
    ClassId id;
    in >> id;
    BitmapGraphic* g = (BitmapGraphic*) catalog->GetCreator()->Create(id);
    g->Read(in);
    SetGraphic(g);
}

void BitmapComp::Write (ostream& out) {
    ClassId id;
    GraphicComp::Write(out);
    Catalog* catalog = unidraw->GetCatalog();
    BitmapGraphic* g = GetBitmapGraphic();
    id = g->GetClassId();
    out << " " << id << " ";
    g->Write(out);
}

/*****************************************************************************/

BitmapView::BitmapView (BitmapComp* subj) : GraphicView(subj) { }
BitmapComp* BitmapView::GetBitmapComp () { return (BitmapComp*) GetSubject(); }
ClassId BitmapView::GetClassId () { return BITMAP_VIEW; }

boolean BitmapView::IsA (ClassId id) {
    return BITMAP_VIEW == id || GraphicView::IsA(id);
}

void BitmapView::Update () {
    Graphic* bcomp = GetBitmapComp()->GetBitmapGraphic();
    Graphic* bview = (BitmapGraphic*) GetGraphic();

    IncurDamage(bview);
    *bview = *bcomp;
    IncurDamage(bview);
    EraseHandles();
}

Graphic* BitmapView::GetGraphic () {
    Graphic* g = GraphicView::GetGraphic();

    if (g == nil) {
        g = GetGraphicComp()->GetGraphic()->Copy();
        SetGraphic(g);
    }
    return g;
}

/*****************************************************************************/

BitmapGraphic::BitmapGraphic (Graphic* g, char* fg_name) : Graphic(g) {
    Init(fg_name);
}

BitmapGraphic::~BitmapGraphic () {
    if (_fg_name != nil) {
	delete _fg_name;
    }
}

void BitmapGraphic::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    int w, h;
    w = _fg_map->pwidth();
    h = _fg_map->pheight();
    CalcExtent(w, h, l, b, cx, cy, tol, gs);
    tol = 0;
}

void BitmapGraphic::CalcExtent (
    int w, int h,
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    transformRect(0, 0, float(w)/2, float(h)/2, l, b, cx, cy, gs);
    PSBrush* br = gs->GetBrush();
    float width = (br == nil) ? 0 : float(br->Width());
    tol = (width > 1) ? width/2 : 0;
}

void BitmapGraphic::Init (char* fg_name) {
    _fg_map = nil;
    _fg_name = nil;

    if (fg_name != nil && *fg_name != '\0') {
	_fg_map = Bitmap::open(fg_name);
    	_fg_map->Reference();
	_fg_name = strnew(fg_name);
    }
}

Graphic* BitmapGraphic::Copy () {
    return new BitmapGraphic(this, _fg_name);
}

void BitmapGraphic::Read (istream& in) {
    Catalog* catalog = unidraw->GetCatalog();
    char* fg_name;

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

    fg_name = catalog->ReadString(in);
    Init(fg_name);
    delete fg_name;
}

void BitmapGraphic::Write (ostream& out) {
    Catalog* catalog = unidraw->GetCatalog();

    catalog->WriteBgFilled(BgFilled(), out);
    catalog->WriteBrush(GetBrush(), out);
    catalog->WriteColor(GetFgColor(), out);
    catalog->WriteColor(GetBgColor(), out);
    catalog->WriteFont(GetFont(), out);
    catalog->WritePattern(GetPattern(), out);
    catalog->WriteTransformer(GetTransformer(), out);

    catalog->WriteString(_fg_name, out);
}

ClassId BitmapGraphic::GetClassId () { return BITMAP_GRAPHIC; }
PSPattern* BitmapGraphic::GetPattern () { return pssolid; }

void BitmapGraphic::draw (Canvas* c, Graphic* gs) {
    int w, h;
    w = _fg_map->pwidth();
    h = _fg_map->pheight();
    update(gs);
    _p->ClearRect(c, 0, 0, w, h);

    _p->Stencil(c, pad/2, pad/2, _fg_map);
}

void BitmapGraphic::drawClipped (
    Canvas* c, Coord l, Coord b, Coord r, Coord t, Graphic* gs
) {
    Graphic::drawClipped(c, l, b, r, t, gs);
}
