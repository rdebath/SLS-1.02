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
 * Graphic base class implementation.
 */

#include <Unidraw/globals.h>
#include <Unidraw/iterator.h>
#include <Unidraw/Graphic/graphic.h>
#include <Unidraw/Graphic/util.h>

#include <IV-2_6/InterViews/painter.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

Transformer* Graphic::_identity;
boolean Graphic::_caching;
Painter* Graphic::_p;
BoxObj* Graphic::_clipping;

Graphic::Graphic (Graphic* gr) {
    _parent = nil;
    _fg = _bg = nil;
    _tag = nil;
    _t = nil;

    if (_identity == nil) {
	_identity = new Transformer;
	cachingOn();
    }
    
    if (_p == nil) {
        _p = new Painter;
        Ref(_p);
    }

    if (gr == nil) {
	FillBg(UNDEF);

    } else {
	FillBg(gr->BgFilled());
	Graphic::SetColors(gr->GetFgColor(), gr->GetBgColor());

	if (gr->_t != nil) {
	    _t = new Transformer(gr->_t);
	}
    }
}

Graphic::~Graphic () {
    Unref(_fg);
    Unref(_bg);
    Unref(_t);
}

void Graphic::Draw (Canvas* c) {
    if (Parent() == nil) {
	draw(c, this);

    } else {
	FullGraphic gs;
	totalGS(gs);
	draw(c, &gs);
    }
}

void Graphic::Draw (Canvas* c, Coord l, Coord b, Coord r, Coord t) {
    if (Parent() == nil) {
	drawClipped(c, l, b, r, t, this);

    } else {
	FullGraphic gs;
	totalGS(gs);
	drawClipped(c, l, b, r, t, &gs);
    }
}

void Graphic::DrawClipped (Canvas* c, Coord l, Coord b, Coord r, Coord t) {
    _clipping = new BoxObj(l, b, r, t);
    _p->Clip(c, l, b, r, t);

    if (Parent() == nil) {
	drawClipped(c, l, b, r, t, this);
    } else {
	FullGraphic gs;
	totalGS(gs);
	drawClipped(c, l, b, r, t, &gs);
    }
    _p->NoClip();
    delete _clipping;
    _clipping = nil;
}

void Graphic::Erase (Canvas* c) {
    if (Parent() == nil) {
	erase(c, this);

    } else {
	FullGraphic gs;
	totalGS(gs);
	erase(c, &gs);
    }
}

void Graphic::Erase (Canvas* c, Coord l, Coord b, Coord r, Coord t) {
    if (Parent() == nil) {
	eraseClipped(c, l, b, r, t, this);

    } else {
	FullGraphic gs;
	totalGS(gs);
	eraseClipped(c, l, b, r, t, &gs);
    }
}

void Graphic::EraseClipped (Canvas* c, Coord l, Coord b, Coord r, Coord t) {
    _clipping = new BoxObj(l, b, r, t);
    _p->Clip(c, l, b, r, t);

    if (Parent() == nil) {
	eraseClipped(c, l, b, r, t, this);

    } else {
	FullGraphic gs;
	totalGS(gs);
	eraseClipped(c, l, b, r, t, &gs);
    }
    _p->NoClip();
    delete _clipping;
    _clipping = nil;
}

void Graphic::FillBg (int fillBg) { _fillBg = fillBg; }
int Graphic::BgFilled () { return _fillBg; }

void Graphic::SetColors (PSColor* fg, PSColor* bg) { 
    Ref(fg);
    Ref(bg);
    Unref(_fg);
    Unref(_bg);

    _fg = fg; 
    _bg = bg;
}

PSColor* Graphic::GetFgColor () { return _fg; }
PSColor* Graphic::GetBgColor () { return _bg; }

void Graphic::SetPattern (PSPattern*) { }
PSPattern* Graphic::GetPattern () { return nil; }

void Graphic::SetBrush (PSBrush*) { }
PSBrush* Graphic::GetBrush() { return nil; }

void Graphic::SetFont (PSFont*) { }
PSFont* Graphic::GetFont () { return nil; }

void Graphic::Translate (float dx, float dy) { 
    if (dx != 0 || dy != 0) {
	if (_t == nil) {
	    _t = new Transformer;
	}
	_t->Translate(dx, dy);
	uncacheParents();
    }
}

void Graphic::Scale (float sx, float sy, float cx, float cy) {
    float ncx, ncy;

    if (sx != 1 || sy != 1) {
	if (_t == nil) {
	    _t = new Transformer;
	}
	Transformer parents;
	parentXform(parents);
	parents.InvTransform(cx, cy, ncx, ncy);
	
	if (ncx != 0 || ncy != 0) {
	    _t->Translate(-ncx, -ncy);
	    _t->Scale(sx, sy);
	    _t->Translate(ncx, ncy);
	} else {
	    _t->Scale(sx, sy);
	}
	uncacheParents();
    }
}

void Graphic::Rotate (float angle, float cx, float cy) {
    float mag = (angle < 0) ? -angle : angle;
    float ncx, ncy;

    if ((mag - int(mag)) != 0 || int(mag)%360 != 0) {
	if (_t == nil) {
	    _t = new Transformer;
	}
	Transformer parents;
	parentXform(parents);
	parents.InvTransform(cx, cy, ncx, ncy);
	
	if (ncx != 0 || ncy != 0) {
	    _t->Translate(-ncx, -ncy);
	    _t->Rotate(angle);
	    _t->Translate(ncx, ncy);
	} else {
	    _t->Rotate(angle);
	}
	uncacheParents();
    }
}

void Graphic::Align (Alignment falign, Graphic* moved, Alignment malign) {
    float fx0, fy0, fx1, fy1, mx0, my0, mx1, my1, dx = 0, dy = 0;

    GetBounds(fx0, fy0, fx1, fy1);
    moved->GetBounds(mx0, my0, mx1, my1);
    
    switch (falign) {
	case BottomLeft:
	case CenterLeft:
	case TopLeft:
	case Left:
	    dx = fx0;
	    break;
	case BottomCenter:
	case Center:
	case TopCenter:
	case HorizCenter:
	    dx = (fx0 + fx1 + 1)/2;
	    break;
	case BottomRight:
	case CenterRight:
	case TopRight:
	case Right:
	    dx = fx1 + 1;
	    break;
    }
    switch (falign) {
	case BottomLeft:
	case BottomCenter:
	case BottomRight:
	case Bottom:
	    dy = fy0;
	    break;
	case CenterLeft:
	case Center:
	case CenterRight:
	case VertCenter:
	    dy = (fy0 + fy1 + 1)/2;
	    break;
	case TopLeft:
	case TopCenter:
	case TopRight:
	case Top:
	    dy = fy1 + 1;
	    break;
    }
    
    switch (malign) {
	case BottomLeft:
	case CenterLeft:
	case TopLeft:
	case Left:
	    dx -= mx0;
	    break;	
	case BottomCenter:
	case Center:
	case TopCenter:
	case HorizCenter:
	    dx -= (mx0 + mx1 + 1)/2;
	    break;
	case BottomRight:
	case CenterRight:
	case TopRight:
	case Right:
	    dx -= (mx1 + 1);
	    break;
    }
    switch (malign) {
	case BottomLeft:
	case BottomCenter:
	case BottomRight:
	case Bottom:
	    dy -= my0;
	    break;
	case CenterLeft:
	case Center:
	case CenterRight:
	case VertCenter:
	    dy -= (my0 + my1 + 1)/2;
	    break;
	case TopLeft:
	case TopCenter:
	case TopRight:
	case Top:
	    dy -= (my1 + 1);
	    break;
    }
    if (dx != 0 || dy != 0) {
        Transformer parents;
	moved->parentXform(parents);

        parents.Invert();
	parents.Transform(0.0, 0.0, fx0, fy0);
	parents.Transform(dx, dy, mx0, my0);

	moved->Translate(mx0-fx0, my0-fy0);
    }
}

void Graphic::SetTransformer (Transformer* t) {
    if (t != _t) {
        Unref(_t);
	if (t != nil) {
	    Ref(t);
	}
	_t = t;
	uncacheParents();
    }
}

void Graphic::TotalTransformation (Transformer& total) {
    Graphic* parent = Parent();
    
    if (parent == nil) {
        concatTransformer(nil, _t, &total);

    } else {
        parent->TotalTransformation(total);
        concatTransformer(_t, &total, &total);
    }
}

void Graphic::GetBounds (float& x0, float& y0, float& x1, float& y1) {
    FullGraphic gs;
    
    totalGS(gs);
    getBounds(x0, y0, x1, y1, &gs);
}

void Graphic::GetBox (Coord& x0, Coord& y0, Coord& x1, Coord& y1) {
    float left, bottom, right, top;

    GetBounds(left, bottom, right, top);
    x0 = Coord(left - 1);
    y0 = Coord(bottom - 1);
    x1 = Coord(right + 1);
    y1 = Coord(top + 1);
}

void Graphic::GetCenter (float& x, float& y) {
    FullGraphic gs;
    float l, b, tol;
    
    totalGS(gs);
    getExtent(l, b, x, y, tol, &gs);
}    

boolean Graphic::Contains (PointObj& p) {
    if (Parent() == nil) {
        return contains(p, this);

    } else {
	FullGraphic gs;
	totalGS(gs);
	return contains(p, &gs);
    }
}

boolean Graphic::Intersects (BoxObj& b) {
    if (Parent() == nil) {
        return intersects(b, this);

    } else {
	FullGraphic gs;
	totalGS(gs);
	return intersects(b, &gs);
    }
}

void Graphic::First (Iterator&) { }
void Graphic::Last (Iterator&) { }
void Graphic::Next (Iterator&) { }
void Graphic::Prev (Iterator&) { }
boolean Graphic::Done (Iterator) { return true; }

Graphic* Graphic::GetGraphic (Iterator) { return nil; }
void Graphic::SetGraphic (Graphic*, Iterator&) { }

void Graphic::Append (Graphic*, Graphic*, Graphic*, Graphic*) { }
void Graphic::Prepend (Graphic*, Graphic*, Graphic*, Graphic*) { }
void Graphic::InsertBefore (Iterator, Graphic*) { }
void Graphic::InsertAfter (Iterator, Graphic*) { }
void Graphic::Remove (Graphic*) { }
void Graphic::Remove (Iterator&) { }
void Graphic::Bequeath () { }

Graphic* Graphic::FirstGraphicContaining (PointObj&) { return nil; }
Graphic* Graphic::LastGraphicContaining (PointObj&) { return nil; }
Graphic* Graphic::FirstGraphicIntersecting (BoxObj&) { return nil; }
Graphic* Graphic::LastGraphicIntersecting (BoxObj&) { return nil; }
Graphic* Graphic::FirstGraphicWithin (BoxObj&) { return nil; }
Graphic* Graphic::LastGraphicWithin (BoxObj&) { return nil; }

Graphic& Graphic::operator = (Graphic& g) {
    SetColors(g.GetFgColor(), g.GetBgColor());
    FillBg(g.BgFilled());
    SetPattern(g.GetPattern());
    SetBrush(g.GetBrush());
    SetFont(g.GetFont());

    if (g._t == nil) {
        Unref(_t);
        _t = nil;

    } else {
	if (_t == nil) {
	    _t = new Transformer(g._t);
	} else {
	    *_t = *g._t;
	}
    }
    invalidateCaches();
    return *this;
}

Graphic* Graphic::Copy () { return nil; }

void Graphic::update (Graphic* gs) {
    Transformer* t = _p->GetTransformer();

    _p->FillBg(gs->BgFilled());
    _p->SetColors(gs->GetFgColor(), gs->GetBgColor());
    _p->SetPattern(gs->GetPattern());
    _p->SetBrush(gs->GetBrush());
    _p->SetBrush(gs->GetBrush());
    _p->SetFont(gs->GetFont());

    if (t == nil) {
	if (gs->_t != nil) {
	    Transformer* newt = new Transformer(gs->_t);
	    _p->SetTransformer(newt);
            Unref(newt);
	}
    } else {
	if (gs->_t == nil) {
	    *t = *_identity;
	} else {
	    *t = *gs->_t;
	}
    }
}

void Graphic::draw (Canvas*, Graphic*) { }

void Graphic::erase (Canvas* c, Graphic* gs) {
    PSColor* fg = gs->GetFgColor();
    PSColor* bg = gs->GetBgColor();
    gs->SetColors(bg, bg);
    draw(c, gs);
    gs->SetColors(fg, bg);
}

void Graphic::drawClipped (
    Canvas* c, Coord left, Coord bottom, Coord right, Coord top, Graphic* gs
) {
    BoxObj thisBox;
    BoxObj clipBox(left, bottom, right, top);

    getBox(thisBox, gs);
    if (clipBox.Intersects(thisBox)) {
	draw(c, gs);
    }
}

void Graphic::eraseClipped (
    Canvas* c, Coord left, Coord bottom, Coord right, Coord top, Graphic* gs
) {
    BoxObj thisBox;
    BoxObj clipBox(left, bottom, right, top);

    getBox(thisBox, gs);
    if (clipBox.Intersects(thisBox)) {
	erase(c, gs);
    }
}

void Graphic::getExtent (float&, float&, float&, float&, float&, Graphic*) { }

void Graphic::GetExtent (Extent& e) {
    FullGraphic gs;
    
    totalGS(gs);
    getExtent(e._left, e._bottom, e._cx, e._cy, e._tol, &gs);
}

void Graphic::getBox (Coord& x0, Coord& y0, Coord& x1, Coord& y1, Graphic* gs){
    float left, bottom, right, top;

    getBounds(left, bottom, right, top, gs);
    x0 = Coord(left - 1);
    y0 = Coord(bottom - 1);
    x1 = Coord(right + 1);
    y1 = Coord(top + 1);
}

boolean Graphic::contains (PointObj& po, Graphic* gs) { 
    BoxObj b;

    getBox(b, gs);
    return b.Contains(po);
}

boolean Graphic::intersects (BoxObj& userb, Graphic* gs) { 
    BoxObj b;

    getBox(b, gs);
    return b.Intersects(userb);
}

Graphic* Graphic::getRoot () {
    Graphic* cur, *parent = this;
    
    do {
	cur = parent;
	parent = cur->Parent();
    } while (parent != nil);

    return cur;
}

void Graphic::totalGS (Graphic& gs) {
    Graphic* parent = Parent();
    
    if (parent == nil) {
        concat(nil, this, &gs);

    } else {
        parent->totalGS(gs);
        concat(this, &gs, &gs);
    }
}

void Graphic::parentXform (Transformer& t) {
    Graphic* parent = Parent();

    if (parent == nil) {
        t = *_identity;
    } else {
        parent->TotalTransformation(t);
    }
}

void Graphic::setParent (Graphic* g, Graphic* parent) {
    if (g->Parent() == nil) {	    // a graphic can have only one parent
	g->_parent = parent;
    }
}

void Graphic::unsetParent (Graphic* g) {
    g->_parent = nil;
    g->invalidateCaches();
}

void Graphic::cachingOn () { _caching = true; }
void Graphic::cachingOff () { _caching = false; }
boolean Graphic::extentCached () { return false; }
void Graphic::uncacheExtent () { }
void Graphic::uncacheChildren () { }

void Graphic::uncacheParents () {
    Graphic* p = Parent();

    if (p != nil) {
        p->uncacheExtent();
	p->uncacheParents();
    }
}

void Graphic::invalidateCaches() {
    uncacheParents();
    uncacheExtent();
    uncacheChildren();
}

void Graphic::concatGS (Graphic* a, Graphic* b, Graphic* dest) {
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
    if ((fill = b->BgFilled()) == UNDEF) {
	fill = a->BgFilled();
    }
    dest->FillBg(fill);

    if ((fg = b->GetFgColor()) == nil) {
	fg = a->GetFgColor();
    }
    if ((bg = b->GetBgColor()) == nil) {
	bg = a->GetBgColor();
    }
    dest->SetColors(fg, bg);

    if ((pat = b->GetPattern()) == nil) {
	pat = a->GetPattern();
    }
    dest->SetPattern(pat);

    if ((font = b->GetFont()) == nil) {
	font = a->GetFont();
    }
    dest->SetFont(font);

    if ((br = b->GetBrush()) == nil) {
	br = a->GetBrush();
    }
    dest->SetBrush(br);
}

void Graphic::concatTransformer (
    Transformer* a, Transformer* b, Transformer* dest
) {
    if (a == nil) {
        *dest = (b == nil) ? *_identity : *b;

    } else if (b == nil) {
        *dest = *a;
        
    } else {
        Transformer tmp(a);
        tmp.Postmultiply(b);
        *dest = tmp;
    }
}

void Graphic::concat (Graphic* a, Graphic* b, Graphic* dest) {
    Transformer* ta = (a == nil) ? nil : a->GetTransformer();
    Transformer* tb = (b == nil) ? nil : b->GetTransformer();
    Transformer* td = dest->GetTransformer();
    if (td == nil) {
        td = new Transformer;
    } else {
	Ref(td);
    }
    concatTransformer(ta, tb, td);
    dest->SetTransformer(td);
    concatGS(a, b, dest);
    Unref(td);
}

void Graphic::transform (Coord& x, Coord& y, Graphic* g) {
    Transformer* t = (g == nil) ? GetTransformer() : g->GetTransformer();

    if (t != nil) {
	t->Transform(x, y);
    }
}

void Graphic::transform (Coord x, Coord y, Coord& tx, Coord& ty, Graphic* g) {
    Transformer* t = (g == nil) ? GetTransformer() : g->GetTransformer();

    if (t != nil) {
	t->Transform(x, y, tx, ty);
    } else {
	tx = x;
	ty = y;
    }
}

void Graphic::transform (float x, float y, float& tx, float& ty, Graphic* g) {
    Transformer* t = (g == nil) ? GetTransformer() : g->GetTransformer();

    if (t != nil) {
	t->Transform(x, y, tx, ty);
    } else {
	tx = x;
	ty = y;
    }
}

void Graphic::transformList (
    Coord x[], Coord y[], int n, Coord tx[], Coord ty[], Graphic* g
) {
    Transformer* t = (g == nil) ? GetTransformer() : g->GetTransformer();

    if (t != nil) {
	t->TransformList(x, y, n, tx, ty);
    } else {
	ArrayCopy(x, y, n, tx, ty);
    }
}

void Graphic::transformRect (
    float x0, float y0, float x1, float y1,
    float& nx0, float& ny0, float& nx1, float& ny1, Graphic* g
) {
    Transformer* t = (g == nil) ? GetTransformer() : g->GetTransformer();
    nx0 = x0; ny0 = y0; nx1 = x1; ny1 = y1;

    if (t != nil) {
        t->TransformRect(nx0, ny0, nx1, ny1);
    }
}

void Graphic::invTransform (Coord& tx, Coord& ty, Graphic* g) {
    Transformer* t = (g == nil) ? GetTransformer() : g->GetTransformer();

    if (t != nil) {
	t->InvTransform(tx, ty);
    }
}

void Graphic::invTransform (
    Coord tx, Coord ty, Coord& x, Coord& y, Graphic* g
) {
    Transformer* t = (g == nil) ? GetTransformer() : g->GetTransformer();

    if (t != nil) {
	t->InvTransform(tx, ty, x, y);
    } else {
	x = tx;
	y = ty;
    }
}

void Graphic::invTransform (
    float tx, float ty, float& x, float& y, Graphic* g
) {
    Transformer* t = (g == nil) ? GetTransformer() : g->GetTransformer();

    if (t != nil) {
	t->InvTransform(tx, ty, x, y);
    } else {
	x = tx;
	y = ty;
    }
}

void Graphic::invTransformList(
    Coord tx[], Coord ty[], int n, Coord x[], Coord y[], Graphic* g
) {
    Transformer* t = (g == nil) ? GetTransformer() : g->GetTransformer();

    if (t != nil) {
	t->InvTransformList(tx, ty, n, x, y);
    } else {
	ArrayCopy(tx, ty, n, x, y);
    }
}

void Graphic::invTransformRect (
    float x0, float y0, float x1, float y1,
    float& nx0, float& ny0, float& nx1, float& ny1, Graphic* g
) {
    Transformer* t = (g == nil) ? GetTransformer() : g->GetTransformer();
    nx0 = x0; ny0 = y0; nx1 = x1; ny1 = y1;

    if (t != nil) {
        t->InvTransformRect(nx0, ny0, nx1, ny1);
    }
}

/*****************************************************************************/

FullGraphic::FullGraphic (Graphic* gr) : Graphic(gr) {
    _br = nil;
    _pat = nil;
    _font = nil;

    if (gr != nil) {
	FullGraphic::SetPattern(gr->GetPattern());
	FullGraphic::SetBrush(gr->GetBrush());
	FullGraphic::SetFont(gr->GetFont());
    }	
}

FullGraphic::~FullGraphic () {
    Unref(_pat);
    Unref(_br);
    Unref(_font);
}

void FullGraphic::SetPattern (PSPattern* pat) {
    Ref(pat);
    Unref(_pat);
    _pat = pat;
}

PSPattern* FullGraphic::GetPattern () { return _pat; }

void FullGraphic::SetBrush (PSBrush* br) {
    if (_br != br) {
        Ref(br);
        Unref(_br);
	_br = br;
	invalidateCaches();
    }
}

PSBrush* FullGraphic::GetBrush () { return _br; }

void FullGraphic::SetFont (PSFont* font) {
    if (_font != font) {
        Ref(font);
        Unref(_font);
	_font = font;
	invalidateCaches();
    }
}

PSFont* FullGraphic::GetFont () { return _font; }
Graphic* FullGraphic::Copy () { return new FullGraphic(this); }
