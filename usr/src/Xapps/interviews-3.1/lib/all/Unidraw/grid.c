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
 * Grid implementation.
 */

#include <Unidraw/globals.h>
#include <Unidraw/grid.h>

#include <Unidraw/Graphic/graphic.h>

#include <InterViews/canvas.h>
#include <IV-2_6/InterViews/painter.h>
#include <InterViews/transformer.h>

#include <OS/math.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

class GridGraphic : public Graphic {
public:
    GridGraphic(float w, float h, float xincr, float yincr, Graphic* = nil);
    virtual ~GridGraphic();

    void SetIncr(float xincr, float yincr);
    void GetOriginal(float&, float&, float&, float&);

    virtual void SetBrush(PSBrush*);
    virtual PSBrush* GetBrush();

    virtual Graphic* Copy();
protected:
    void getExtent(float&, float&, float&, float&, float&, Graphic*);
    void draw(Canvas*, Graphic*);
    void drawClipped(Canvas*, Coord, Coord, Coord, Coord, Graphic*);
private:
    float _width, _height;
    float _xincr, _yincr;
    PSBrush* _br;
};

GridGraphic::GridGraphic (
    float width, float height, float xincr, float yincr, Graphic* gr
) : Graphic(gr) {
    _br = nil;
    if (gr != nil) {
        GridGraphic::SetBrush(gr->GetBrush());
    }
    _width = width;
    _height = height;
    _xincr = xincr;
    _yincr = yincr;
}

GridGraphic::~GridGraphic () { Unref(_br); }

void GridGraphic::SetIncr(float xincr, float yincr) {
    _xincr = xincr;
    _yincr = yincr;
    invalidateCaches();
}

void GridGraphic::GetOriginal (
    float& width, float& height, float& xincr, float& yincr
) {
    width = _width;
    height = _height;
    xincr = _xincr;
    yincr = _yincr;
}

void GridGraphic::SetBrush (PSBrush* br) {
    if (_br != br) {
        Ref(br);
        Unref(_br);
        _br = br;
	invalidateCaches();
    }
}

PSBrush* GridGraphic::GetBrush () { return _br; }

Graphic* GridGraphic::Copy () {
    return new GridGraphic(_width, _height, _xincr, _yincr, this);
}

void GridGraphic::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    float width, dummy1, dummy2;

    width = float(gs->GetBrush()->Width());
    tol = (width > 1) ? width/2 : 0;
    transformRect(0.0, 0.0, _width-1, _height-1, l, b, dummy1, dummy2, gs);
    transform((_width-1)/2, (_height-1)/2, cx, cy, gs); 
}

void GridGraphic::draw (Canvas* c, Graphic* gs) {
    drawClipped(c, 0, 0, c->Width()-1, c->Height()-1, gs);
}

static inline float frac (float r) {
    if (r < 0) {
	r = -r;
	r = r - int(r);
	r = -r;
    } else {
	r = r - int(r);
    }
    return r;
}

void GridGraphic::drawClipped (
    Canvas* c, Coord l, Coord b, Coord r, Coord t, Graphic* gs
) {
    if (!gs->GetBrush()->None()) {
	const float min_xincr = 4;
	const float min_yincr = 4;

	float x0, y0, x1, y1;
	transformRect(0., 0., _xincr, _yincr, x0, y0, x1, y1, gs);
	float xincr = x1 - x0;
	float yincr = y1 - y0;

	xincr = Math::max(min_xincr, xincr * int(1 + min_xincr/xincr));
	yincr = Math::max(min_yincr, yincr * int(1 + min_yincr/yincr));

	transform(0., 0.0, x0, y0, gs);

	x0 -= l;
	y0 -= b;

	x0 = l + frac(x0/xincr) * xincr;
	y0 = b + frac(y0/yincr) * yincr;

	update(gs);
	_p->SetTransformer(nil);

	++r; ++t;

        for (register float x = x0; x <= r; x += xincr) {
            for (register float y = y0; y <= t; y += yincr) {
                _p->Point(c, round(x), round(y));
            }
        }
    }
}

/*****************************************************************************/

Grid::Grid (float width, float height, float xincr, float yincr) {
    _graphic = new GridGraphic(width, height, xincr, yincr, stdgraphic);
}

Grid::Grid (Graphic* g) { _graphic = g; }
Grid::~Grid () { delete _graphic; }
Graphic* Grid::GetGraphic () { return _graphic; }

void Grid::Constrain (Coord& x, Coord& y) {
    Transformer total(_graphic->GetTransformer());
    GridGraphic* gg = (GridGraphic*) _graphic;
    float xincr, yincr, dummy1, dummy2;

    gg->TotalTransformation(total);
    gg->GetOriginal(dummy1, dummy2, xincr, yincr);
    float x0, y0;
    total.InvTransform(float(x), float(y), x0, y0);
    x0 = round(x0/xincr) * xincr;
    y0 = round(y0/yincr) * yincr;
    total.Transform(x0, y0, x0, y0);
    x = round(x0);
    y = round(y0);
}

void Grid::SetSpacing (float xincr, float yincr) {
    ((GridGraphic*)_graphic)->SetIncr(xincr,yincr);
}

void Grid::Visibility (boolean visible) {
    if (visible != IsVisible()) {
        PSBrush* br = visible ? pssingle : psnonebr;
        _graphic->SetBrush(br);
    }
}

boolean Grid::IsVisible () { return !_graphic->GetBrush()->None(); }
