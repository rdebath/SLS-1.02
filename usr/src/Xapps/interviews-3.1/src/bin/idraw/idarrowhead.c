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
 * Arrowhead implementation.
 */

#include "idarrowhead.h"

#include <InterViews/transformer.h>

#include <math.h>

/*****************************************************************************/

static const int COUNT = 4;

static const int BOTLEFT = 0;
static const int TIP = 1;
static const int BOTRIGHT = 2;
static const int BOTCTR = 3;

/*****************************************************************************/

Arrowhead::Arrowhead (
    Coord tx, Coord ty, Coord w, Coord h, Graphic* g
) : SF_Polygon(x(tx, w), y(ty, h), COUNT, g) { }

Arrowhead::Arrowhead (
    Coord* x, Coord* y, Graphic* g
) : SF_Polygon(x, y, COUNT, g) { }

Graphic* Arrowhead::Copy () { return new Arrowhead(_x, _y, this); }
Graphic& Arrowhead::operator = (Graphic& g) { return Graphic::operator=(g); }

Coord Arrowhead::CorrectedHeight (float t) {
    float w = _x[BOTRIGHT] - _x[BOTCTR];
    float h = _y[TIP] - _y[BOTRIGHT];

    float a = -4*h * w*w;
    float radicand = 4*w*w + 4*h*h - t*t;
    float root = (radicand < 0.) ? 0. : sqrt(radicand);
    float b = t*w * root;
    float c = t*t - 4*w*w;

    if (c == 0) return 0;

    Coord h1 = round((a + b) / c);
    Coord h2 = round((a - b) / c);

    return (h1 < h && h1 > 0) ? h1 : h2;
}

void Arrowhead::CorrectedTip (
    Coord& tipx, Coord& tipy, PSBrush* br, Transformer* t
) {
    Transformer total(t);
    Transformer* my_t = GetTransformer();
    concatTransformer(my_t, t, &total);
    
    float thk = UnscaledLength(br->Width(), &total);
    tipx = _x[TIP];
    tipy = _y[BOTLEFT] + CorrectedHeight(thk);

    if (my_t != nil) my_t->Transform(tipx, tipy);
}

float Arrowhead::UnscaledLength (float length, Transformer* t) {
    Transformer inverse(t);
    inverse.Invert();

    float x0 = 0, y0 = 0, x1 = length, y1 = 0;
    float tx0, ty0, tx1, ty1;

    inverse.Transform(x0, y0, tx0, ty0);
    inverse.Transform(x1, y1, tx1, ty1);

    return hypot(tx0-tx1, ty0-ty1);
}

void Arrowhead::draw (Canvas* c, Graphic* gs) {
    PSPattern* pat = gs->GetPattern();
    PSBrush* br = gs->GetBrush();

    if (br->None()) {
        _y[BOTCTR] = _y[BOTLEFT];
        SF_Polygon::draw(c, gs);

    } else {
        Coord ytip = _y[TIP];
        float thk = UnscaledLength(br->Width(), gs->GetTransformer());
        Coord hcorrect = CorrectedHeight(thk);

        if (pat->None()) {
            _y[BOTCTR] = _y[TIP] = _y[BOTLEFT] + hcorrect;
            SF_Polygon::draw(c, gs);
            _y[BOTCTR] = _y[TIP] = ytip;

        } else {
            _y[BOTCTR] = _y[BOTLEFT];
            _y[TIP] = _y[BOTLEFT] + hcorrect;
            SF_Polygon::draw(c, gs);
            _y[TIP] = ytip;
        }
    }
}

Coord* Arrowhead::x (Coord tipx, Coord w) {
    static Coord px[COUNT];
    
    px[BOTLEFT] = tipx - w/2;
    px[TIP] = px[BOTCTR] = tipx;
    px[BOTRIGHT] = tipx + w/2;

    return px;
}

Coord* Arrowhead::y (Coord tipy, Coord h) {
    static Coord py[COUNT];
    
    py[BOTLEFT] = py[BOTRIGHT] = py[BOTCTR] = tipy - h;
    py[TIP] = tipy;

    return py;
}
