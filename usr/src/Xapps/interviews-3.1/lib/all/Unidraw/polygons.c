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
 * Implementation of Rectangles and Polygons, objects derived from Graphic.
 */

#include <Unidraw/globals.h>
#include <Unidraw/Graphic/polygons.h>

#include <IV-2_6/InterViews/painter.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

Rect::Rect (Coord x0, Coord y0, Coord x1, Coord y1, Graphic* gr) : Graphic(gr){
    _x0 = min(x0, x1);
    _y0 = min(y0, y1);
    _x1 = max(x0, x1);
    _y1 = max(y0, y1);
}

void Rect::GetOriginal (Coord& x0, Coord& y0, Coord& x1, Coord& y1) {
    x0 = _x0;
    y0 = _y0;
    x1 = _x1;
    y1 = _y1;
}

void Rect::s_getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    float width, dummy1, dummy2;

    width = float(gs->GetBrush()->Width());
    tol = (width > 1) ? width/2 : 0;
    transformRect(_x0, _y0, _x1, _y1, l, b, dummy1, dummy2, gs);
    transform(float(_x0+_x1)/2, float(_y0+_y1)/2, cx, cy, gs);
}

void Rect::f_getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
){
    float dummy1, dummy2;

    transformRect(_x0, _y0, _x1, _y1, l, b, dummy1, dummy2, gs);
    transform(float(_x0+_x1)/2, float(_y0+_y1)/2, cx, cy, gs);
    tol = 0;
}

boolean Rect::s_contains (PointObj& po, Graphic* gs) {
    PointObj pt (&po);
    invTransform(pt._x, pt._y, gs);

    return (
        ((pt._x == _x0 || pt._x == _x1) && _y0 <= pt._y && pt._y <= _y1) ||
	((pt._y == _y0 || pt._y == _y1) && _x0 <= pt._x && pt._x <= _x1)
    );
}

boolean Rect::f_contains (PointObj& po, Graphic* gs) {
    PointObj pt (&po);
    invTransform(pt._x, pt._y, gs);
    BoxObj b (_x0, _y0, _x1, _y1);
    return b.Contains(pt);
}

boolean Rect::s_intersects (BoxObj& userb, Graphic* gs) {
    Coord x[4], tx[5];
    Coord y[4], ty[5];
    
    x[0] = x[3] = _x0;
    y[0] = y[1] = _y0;
    x[2] = x[1] = _x1;
    y[2] = y[3] = _y1;
    transformList(x, y, 4, tx, ty, gs);
    tx[4] = tx[0];
    ty[4] = ty[0];
    MultiLineObj ml (tx, ty, 5);

    return ml.Intersects(userb) || ml.Within(userb);
}

boolean Rect::f_intersects (BoxObj& userb, Graphic* gs) {
    Transformer* t = gs->GetTransformer();
    Coord tx0, ty0, tx1, ty1;
    
    if (t != nil && t->Rotated()) {
	Coord x[4], tx[5];
	Coord y[4], ty[5];
    
	x[0] = x[3] = _x0;
	y[0] = y[1] = _y0;
	x[2] = x[1] = _x1;
	y[2] = y[3] = _y1;
	transformList(x, y, 4, tx, ty, gs);
	tx[4] = tx[0];
	ty[4] = ty[0];
	FillPolygonObj fp (tx, ty, 5);
	return fp.Intersects(userb);
    
    } else if (t != nil) {
	t->Transform(_x0, _y0, tx0, ty0);
	t->Transform(_x1, _y1, tx1, ty1);
	BoxObj b1 (tx0, ty0, tx1, ty1);
	return b1.Intersects(userb);

    } else {
	BoxObj b2 (_x0, _y0, _x1, _y1);
	return b2.Intersects(userb);
    }
}

/*****************************************************************************/

S_Rect::S_Rect (
    Coord x0, Coord y0, Coord x1, Coord y1, Graphic* gr
) : Rect(x0, y0, x1, y1, gr) {
    _br = nil;
    if (gr != nil) {
        S_Rect::SetBrush(gr->GetBrush());
    }
}

S_Rect::~S_Rect () { Unref(_br); }

void S_Rect::SetBrush (PSBrush* br) {
    if (_br != br) {
        Ref(br);
        Unref(_br);
        _br = br;
	invalidateCaches();
    }
}

PSBrush* S_Rect::GetBrush () { return _br; }
Graphic* S_Rect::Copy () { return new S_Rect(_x0, _y0, _x1, _y1, this); }

void S_Rect::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    s_getExtent(l, b, cx, cy, tol, gs);
}

boolean S_Rect::contains (PointObj& po, Graphic* gs) {
    return s_contains(po, gs);
}

boolean S_Rect::intersects (BoxObj& userb, Graphic* gs) {
    return s_intersects(userb, gs);
}

void S_Rect::draw (Canvas *c, Graphic* gs) {
    if (!gs->GetBrush()->None()) {
	update(gs);
	_p->Rect(c, _x0, _y0, _x1, _y1);
    }
}

/*****************************************************************************/

F_Rect::F_Rect (
    Coord _x0, Coord _y0, Coord _x1, Coord _y1, Graphic* gr
) : Rect(_x0, _y0, _x1, _y1, gr) {
    _pat = nil;
    if (gr != nil) {
	F_Rect::SetPattern(gr->GetPattern());
    }
}

F_Rect::~F_Rect () { Unref(_pat); }

void F_Rect::SetPattern (PSPattern* pat) {
    Ref(pat);
    Unref(_pat);
    _pat = pat;
}

PSPattern* F_Rect::GetPattern () { return _pat; }
Graphic* F_Rect::Copy () { return new F_Rect(_x0, _y0, _x1, _y1, this); }

void F_Rect::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    f_getExtent(l, b, cx, cy, tol, gs);
}

boolean F_Rect::contains (PointObj& po, Graphic* gs) {
    return !gs->GetPattern()->None() && f_contains(po, gs);
}

boolean F_Rect::intersects (BoxObj& userb, Graphic* gs) {
    return !gs->GetPattern()->None() && f_intersects(userb, gs);
}

void F_Rect::draw (Canvas *c, Graphic* gs) {
    if (!gs->GetPattern()->None()) {
        update(gs);
        _p->FillRect(c, _x0, _y0, _x1, _y1);
    }
}

/*****************************************************************************/

SF_Rect::SF_Rect (
    Coord x0, Coord y0, Coord x1, Coord y1, Graphic* gr
) : Rect(x0, y0, x1, y1, gr) {
    _br = nil;
    _pat = nil;

    if (gr != nil) {
        SF_Rect::SetBrush(gr->GetBrush());
        SF_Rect::SetPattern(gr->GetPattern());
    }
}

SF_Rect::~SF_Rect () {
    Unref(_br);
    Unref(_pat);
}

void SF_Rect::SetBrush (PSBrush* br) {
    if (_br != br) {
        Ref(br);
        Unref(_br);
        _br = br;
        invalidateCaches();
    }
}

PSBrush* SF_Rect::GetBrush () { return _br; }

void SF_Rect::SetPattern (PSPattern* pat) {
    Ref(pat);
    Unref(_pat);
    _pat = pat;
}

PSPattern* SF_Rect::GetPattern () { return _pat; }
Graphic* SF_Rect::Copy () { return new SF_Rect(_x0, _y0, _x1, _y1, this);
}

void SF_Rect::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    s_getExtent(l, b, cx, cy, tol, gs);
}

boolean SF_Rect::contains (PointObj& po, Graphic* gs) {
    return 
        (!gs->GetPattern()->None() && f_contains(po, gs)) ||
        s_contains(po, gs);
}

boolean SF_Rect::intersects (BoxObj& userb, Graphic* gs) {
    return 
        (!gs->GetPattern()->None() && f_intersects(userb, gs)) ||
        s_intersects(userb, gs);
}

void SF_Rect::draw (Canvas* c, Graphic* gs) {
    update(gs);
    if (!gs->GetPattern()->None()) {
        _p->FillRect(c, _x0, _y0, _x1, _y1);
    }
    if (!gs->GetBrush()->None()) {
        _p->Rect(c, _x0, _y0, _x1, _y1);
    }
}

/*****************************************************************************/

Polygon::Polygon (
    Coord* x, Coord* y, int count, Graphic* gr
) : Vertices(x, y, count, gr) { }

boolean Polygon::s_contains (PointObj& po, Graphic* gs) {
    BoxObj b;
    PointObj pt (&po);
    getBox(b, gs);

    if (b.Contains(pt)) {
	MultiLineObj ml (_x, _y, _count);
	LineObj l (_x[_count - 1], _y[_count - 1], *_x, *_y);
	invTransform(pt._x, pt._y, gs);
	return ml.Contains(pt) || l.Contains(pt);
    }
    return false;
}

boolean Polygon::f_contains (PointObj& po, Graphic* gs) {
    BoxObj b;
    PointObj pt (&po);
    getBox(b, gs);

    if (b.Contains(pt)) {
	FillPolygonObj fp (_x, _y, _count);
	invTransform(pt._x, pt._y, gs);
	return fp.Contains(pt);
    }
    return false;
}

boolean Polygon::s_intersects (BoxObj& userb, Graphic* gs) {
    Coord* convx, *convy;
    BoxObj b;
    boolean result = false;
    getBox(b, gs);

    if (b.Intersects(userb)) {
	convx = new Coord[_count+1];
	convy = new Coord[_count+1];
	transformList(_x, _y, _count, convx, convy, gs);
	convx[_count] = *convx;
	convy[_count] = *convy;
	MultiLineObj ml(convx, convy, _count+1);
	result = ml.Intersects(userb);
	delete convx;
	delete convy;
    }
    return result;
}

boolean Polygon::f_intersects (BoxObj& userb, Graphic* gs) {
    Coord* convx, *convy;
    BoxObj b;
    boolean result = false;
    getBox(b, gs);

    if (b.Intersects(userb)) {
	convx = new Coord[_count+1];
	convy = new Coord[_count+1];
	transformList(_x, _y, _count, convx, convy, gs);
	FillPolygonObj fp (convx, convy, _count);
	result = fp.Intersects(userb);
	delete convx;
	delete convy;
    }
    return result;    
}

/*****************************************************************************/

S_Polygon::S_Polygon (
    Coord* x, Coord* y, int count, Graphic* gr
) : Polygon(x, y, count, gr) {
    _br = nil;
    if (gr != nil) {
        S_Polygon::SetBrush(gr->GetBrush());
    }
}

S_Polygon::~S_Polygon () { Unref(_br); }

void S_Polygon::SetBrush (PSBrush* br) {
    if (_br != br) {
        Ref(br);
        Unref(_br);
        _br = br;
        invalidateCaches();
    }
}

PSBrush* S_Polygon::GetBrush () { return _br; }
Graphic* S_Polygon::Copy () { return new S_Polygon(_x, _y, _count, this); }

void S_Polygon::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    s_getExtent(l, b, cx, cy, tol, gs);
}

boolean S_Polygon::contains (PointObj& po, Graphic* gs) {
    return s_contains(po, gs);
}

boolean S_Polygon::intersects (BoxObj& userb, Graphic* gs) {
    return s_intersects(userb, gs);
}

void S_Polygon::draw (Canvas *c, Graphic* gs) {
    if (!gs->GetBrush()->None()) {
	update(gs);
	_p->Polygon(c, _x, _y, _count);
    }
}

/*****************************************************************************/

F_Polygon::F_Polygon (
    Coord* x, Coord* y, int count, Graphic* gr
) : Polygon(x, y, count, gr) {
    _pat = nil;
    if (gr != nil) {
	F_Polygon::SetPattern(gr->GetPattern());
    }
}

F_Polygon::~F_Polygon () { Unref(_pat); }

void F_Polygon::SetPattern (PSPattern* pat) {
    Ref(pat);
    Unref(_pat);
    _pat = pat;
}

PSPattern* F_Polygon::GetPattern () { return _pat; }
Graphic* F_Polygon::Copy () { return new F_Polygon(_x, _y, _count, this); }

void F_Polygon::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    f_getExtent(l, b, cx, cy, tol, gs);
}

boolean F_Polygon::contains (PointObj& po, Graphic* gs) {
    return !gs->GetPattern()->None() && f_contains(po, gs);
}

boolean F_Polygon::intersects (BoxObj& userb, Graphic* gs) {
    return !gs->GetPattern()->None() && f_intersects(userb, gs);
}

void F_Polygon::draw (Canvas *c, Graphic* gs) {
    if (!gs->GetPattern()->None()) {
        update(gs);
        _p->FillPolygon(c, _x, _y, _count);
    }
}

/*****************************************************************************/

SF_Polygon::SF_Polygon (
    Coord* x, Coord* y, int count, Graphic* gr
) : Polygon(x, y, count, gr) {
    _br = nil;
    _pat = nil;

    if (gr != nil) {
        SF_Polygon::SetBrush(gr->GetBrush());
	SF_Polygon::SetPattern(gr->GetPattern());
    }
}

SF_Polygon::~SF_Polygon () {
    Unref(_br);
    Unref(_pat);
}

void SF_Polygon::SetBrush (PSBrush* br) {
    if (_br != br) {
        Ref(br);
        Unref(_br);
        _br = br;
        invalidateCaches();
    }
}

PSBrush* SF_Polygon::GetBrush () { return _br; }

void SF_Polygon::SetPattern (PSPattern* pat) {
    Ref(pat);
    Unref(_pat);
    _pat = pat;
}

PSPattern* SF_Polygon::GetPattern () { return _pat; }
Graphic* SF_Polygon::Copy () { return new SF_Polygon(_x, _y, _count, this); }

void SF_Polygon::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    s_getExtent(l, b, cx, cy, tol, gs);
}

boolean SF_Polygon::contains (PointObj& po, Graphic* gs) {
    return 
        (!gs->GetPattern()->None() && f_contains(po, gs)) ||
        s_contains(po, gs);
}

boolean SF_Polygon::intersects (BoxObj& userb, Graphic* gs) {
    return
        (!gs->GetPattern()->None() && f_intersects(userb, gs)) ||
        s_intersects(userb, gs);
}

void SF_Polygon::draw (Canvas *c, Graphic* gs) {
    update(gs);
    if (!gs->GetPattern()->None()) {
        _p->FillPolygon(c, _x, _y, _count);
    }
    if (!gs->GetBrush()->None()) {
        _p->Polygon(c, _x, _y, _count);
    }
}
