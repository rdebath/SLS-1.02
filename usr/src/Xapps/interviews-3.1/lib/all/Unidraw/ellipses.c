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
 * Implementation of Ellipses and Circles, objects derived from Graphic.
 */

#include <Unidraw/globals.h>
#include <Unidraw/Graphic/ellipses.h>
#include <Unidraw/Graphic/util.h>

#include <IV-2_6/InterViews/painter.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

Coord Ellipse::_x[8];
Coord Ellipse::_y[8];

void Ellipse::GetOriginal (Coord& x0, Coord& y0, int& r1, int& r2) {
    x0 = _x0;
    y0 = _y0;
    r1 = _r1;
    r2 = _r2;
}

Ellipse::Ellipse (
    Coord x0, Coord y0, int r1, int r2, Graphic* gr
) : Graphic(gr) {
    _x0 = x0;
    _y0 = y0;
    _r1 = r1;
    _r2 = r2;
}    

void Ellipse::s_getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    float width, dummy1, dummy2, bx0, by0, bx1, by1;

    width = float(gs->GetBrush()->Width());
    tol = (width > 1) ? width/2 : 0;
    bx0 = float(_x0 - _r1);
    by0 = float(_y0 - _r2);
    bx1 = float(_x0 + _r1);
    by1 = float(_y0 + _r2);
    transformRect(bx0, by0, bx1, by1, l, b, dummy1, dummy2, gs);
    transform(float(_x0), float(_y0), cx, cy, gs);
}

void Ellipse::f_getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    float bx0, by0, bx1, by1, dummy1, dummy2;

    tol = 0;
    bx0 = float(_x0 - _r1);
    by0 = float(_y0 - _r2);
    bx1 = float(_x0 + _r1);
    by1 = float(_y0 + _r2);
    transformRect(bx0, by0, bx1, by1, l, b, dummy1, dummy2, gs);
    transform(float(_x0), float(_y0), cx, cy, gs);
}

boolean Ellipse::s_contains (PointObj& po, Graphic* gs) {
    PointObj pt (&po);
    invTransform(pt._x, pt._y, gs);

    return (
	square(_r2)*square(pt._x - _x0) + square(_r1)*square(pt._y - _y0) - 
	square(_r1*_r2) == 0
    );
}

boolean Ellipse::f_contains (PointObj& po, Graphic* gs) {
    PointObj pt (&po);
    invTransform(pt._x, pt._y, gs);

    return (
        square(_r2)*square(pt._x - _x0) + square(_r1)*square(pt._y - _y0) - 
	square(_r1*_r2)
    ) <= 0;
}

boolean Ellipse::s_intersects (BoxObj& userb, Graphic* gs) {
    BoxObj b;
    getBox(b, gs);

    if (b.Intersects(userb)) {
	Transformer* t = gs->GetTransformer();
        MultiLineObj ml;

        CalcControlPts(t);
        ml.ClosedSplineToPolygon(_x, _y, 8);
	return ml.Intersects(userb);
    }
    return false;
}

boolean Ellipse::f_intersects (BoxObj& userb, Graphic* gs) {
    BoxObj b;
    getBox(b, gs);

    if (b.Intersects(userb)) {
	Transformer* t = gs->GetTransformer();
        FillPolygonObj fp;

        CalcControlPts(t);
        fp.ClosedSplineToPolygon(_x, _y, 8);
	return fp.Intersects(userb);
    }
    return false;
}

static const float axis = 0.42;
static const float seen = 1.025;

void Ellipse::CalcControlPts (Transformer* t) {
    if (t == nil) {
        Coord px1, py1, px2, py2;

	px1 = round(float(_r1)*axis); py1 = round(float(_r2)*axis);
	px2 = round(float(_r1)*seen); py2 = round(float(_r2)*seen);

        _x[0] = _x0 + px1;    _y[0] = _y0 + py2;
        _x[1] = _x0 - px1;    _y[1] = _y[0];
        _x[2] = _x0 - px2;    _y[2] = _y0 + py1;
        _x[3] = _x[2];        _y[3] = _y0 - py1;
        _x[4] = _x[1];	      _y[4] = _y0 - py2;
        _x[5] = _x[0];	      _y[5] = _y[4];
        _x[6] = _x0 + px2;    _y[6] = _y[3];
        _x[7] = _x[6];	      _y[7] = _y[2];
    
    } else {
        float fx1, fy1, fx2, fy2, tx[8], ty[8], tmpx, tmpy;

        fx1 = float(_r1)*axis; fy1 = float(_r2)*axis;
        fx2 = float(_r1)*seen; fy2 = float(_r2)*seen;

        tx[0] = _x0 + fx1;    ty[0] = _y0 + fy2;
        tx[1] = _x0 - fx1;    ty[1] = ty[0];
        tx[2] = _x0 - fx2;    ty[2] = _y0 + fy1;
        tx[3] = tx[2];        ty[3] = _y0 - fy1;
        tx[4] = tx[1];	      ty[4] = _y0 - fy2;
        tx[5] = tx[0];	      ty[5] = ty[4];
        tx[6] = _x0 + fx2;    ty[6] = ty[3];
        tx[7] = tx[6];	      ty[7] = ty[2];

        for (int i = 0; i < 8; ++i) {
            t->Transform(tx[i], ty[i], tmpx, tmpy);
            _x[i] = round(tmpx);
            _y[i] = round(tmpy);
        }
    }
}

/*****************************************************************************/

S_Ellipse::S_Ellipse (
    Coord x0, Coord y0, int r1, int r2, Graphic* gr
) : Ellipse(x0, y0, r1, r2, gr) {
    _br = nil;
    if (gr != nil) {
        S_Ellipse::SetBrush(gr->GetBrush());
    }
}

S_Ellipse::~S_Ellipse () { Unref(_br); }

void S_Ellipse::SetBrush (PSBrush* br) {
    if (_br != br) {
        Ref(br);
        Unref(_br);
        _br = br;
	invalidateCaches();
    }
}

PSBrush* S_Ellipse::GetBrush () { return _br; }
Graphic* S_Ellipse::Copy () { return new S_Ellipse(_x0, _y0, _r1, _r2, this); }

void S_Ellipse::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    s_getExtent(l, b, cx, cy, tol, gs);
}

boolean S_Ellipse::contains (PointObj& po, Graphic* gs) {
    return s_contains(po, gs);
}

boolean S_Ellipse::intersects (BoxObj& userb, Graphic* gs) {
    return s_intersects(userb, gs);
}

void S_Ellipse::draw (Canvas *c, Graphic* gs) {
    if (!gs->GetBrush()->None()) {
	update(gs);
	_p->Ellipse(c, _x0, _y0, _r1, _r2);
    }
}

/*****************************************************************************/

F_Ellipse::F_Ellipse (
    Coord x0, Coord y0, int r1, int r2, Graphic* gr
) : Ellipse(x0, y0, r1, r2, gr) {
    _pat = nil;
    if (gr != nil) {
	F_Ellipse::SetPattern(gr->GetPattern());
    }
}

F_Ellipse::~F_Ellipse () { Unref(_pat); }

void F_Ellipse::SetPattern (PSPattern* pat) {
    Ref(pat);
    Unref(_pat);
    _pat = pat;
}

PSPattern* F_Ellipse::GetPattern () { return _pat; }
Graphic* F_Ellipse::Copy () { return new F_Ellipse(_x0, _y0, _r1, _r2, this); }

void F_Ellipse::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    f_getExtent(l, b, cx, cy, tol, gs);
}

boolean F_Ellipse::contains (PointObj& po, Graphic* gs) {
    return !gs->GetPattern()->None() && f_contains(po, gs);
}

boolean F_Ellipse::intersects (BoxObj& userb, Graphic* gs) {
    return !gs->GetPattern()->None() && f_intersects(userb, gs);
}

void F_Ellipse::draw (Canvas* c, Graphic* gs) {
    if (!gs->GetPattern()->None()) {
        update(gs);
        _p->FillEllipse(c, _x0, _y0, _r1, _r2);
    }
}

/*****************************************************************************/

SF_Ellipse::SF_Ellipse (
    Coord x0, Coord y0, int r1, int r2, Graphic* gr
) : Ellipse(x0, y0, r1, r2, gr) {
    _br = nil;
    _pat = nil;

    if (gr != nil) {
        SF_Ellipse::SetBrush(gr->GetBrush());
	SF_Ellipse::SetPattern(gr->GetPattern());
    }
}

SF_Ellipse::~SF_Ellipse () {
    Unref(_br);
    Unref(_pat);
}

void SF_Ellipse::SetBrush (PSBrush* br) {
    if (_br != br) {
        Ref(br);
        Unref(_br);
        _br = br;
	invalidateCaches();
    }
}

PSBrush* SF_Ellipse::GetBrush () { return _br; }

void SF_Ellipse::SetPattern (PSPattern* pat) {
    Ref(pat);
    Unref(_pat);
    _pat = pat;

}
PSPattern* SF_Ellipse::GetPattern () { return _pat; }
Graphic* SF_Ellipse::Copy () { return new SF_Ellipse(_x0, _y0, _r1, _r2,this);}

void SF_Ellipse::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    s_getExtent(l, b, cx, cy, tol, gs);
}

boolean SF_Ellipse::contains (PointObj& po, Graphic* gs) {
    return
        (!gs->GetPattern()->None() && f_contains(po, gs)) ||
        s_contains(po, gs);
}

boolean SF_Ellipse::intersects (BoxObj& userb, Graphic* gs) {
    return
        (!gs->GetPattern()->None() && f_intersects(userb, gs)) ||
        s_intersects(userb, gs);
}

void SF_Ellipse::draw (Canvas* c, Graphic* gs) {
    update(gs);
    if (!gs->GetPattern()->None()) {
        _p->FillEllipse(c, _x0, _y0, _r1, _r2);
    }
    if (!gs->GetBrush()->None()) {
        _p->Ellipse(c, _x0, _y0, _r1, _r2);
    }
}

/****************************************************************************/

S_Circle::S_Circle (
    Coord x0, Coord y0, int radius, Graphic* gr
) : S_Ellipse(x0, y0, radius, radius, gr) { }

Graphic* S_Circle::Copy () { return new S_Circle(_x0, _y0, _r1, this); }

/*****************************************************************************/

F_Circle::F_Circle (
    Coord x0, Coord y0, int radius, Graphic* gr
) : F_Ellipse(x0, y0, radius, radius, gr) { }

Graphic* F_Circle::Copy () { return new F_Circle(_x0, _y0, _r1, this); }

/*****************************************************************************/

SF_Circle::SF_Circle (
    Coord x0, Coord y0, int radius, Graphic* gr
) : SF_Ellipse(x0, y0, radius, radius, gr) { }

Graphic* SF_Circle::Copy () { return new SF_Circle(_x0, _y0, _r1, this); }
