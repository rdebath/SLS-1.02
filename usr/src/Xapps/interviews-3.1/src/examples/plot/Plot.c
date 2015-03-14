/*
 * Plot - simple graph glyph
 */

#include "Plot.h"

#include <stdio.h>
#include <InterViews/brush.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/event.h>
#include <InterViews/hit.h>
#include <OS/math.h>

static const float min_size = 10.0;

Plot::Plot (int& count, float* x, float* y, const Color* c, const Brush* b) {
    _count = &count;
    _x = x;
    _y = y;
    _color = c; _color->ref();
    _brush = b; _brush->ref();
}

Plot::~Plot () {
    _color->unref();
    _brush->unref();
}

void Plot::request (Requisition& r) const {
    Glyph::request(r);
    Requirement& rx = r.x_requirement();
    Requirement& ry = r.y_requirement();
    rx.natural(min_size);
    rx.stretch(fil);
    rx.shrink(min_size);
    rx.alignment(0.0);
    ry.natural(min_size);
    ry.stretch(fil);
    ry.shrink(min_size);
    ry.alignment(1.0);
/*
    Requirement x(10.0, fil, fil, 0.0);
    r.require(Dimension_X, x);
    Requirement y(10.0, fil, fil, 0.0);
    r.require(Dimension_Y, y);
*/
}
void Plot::allocate (Canvas* c, const Allocation& a, Extension& ext) {
    ext.merge(c, a);
}

void Plot::draw (Canvas* c, const Allocation& a) const {
    Coord l = a.left();   Coord r = a.right();
    Coord b = a.bottom(); Coord t = a.top();
    for (int i = 0; i < *_count; ++i) {
        Coord x = l * (1 - _x[i]) + r * _x[i];
        Coord y = b * (1 - _y[i]) + t * _y[i];
        c->move_to(x-1, y-1);
        c->line_to(x-1, y+1);
        c->line_to(x+1, y+1);
        c->line_to(x+1, y-1);
        c->line_to(x-1, y-1);
        c->stroke(_color, _brush);
    }
}

void Plot::pick (Canvas*, const Allocation& a, int depth, Hit& h) {
    const Event* e = h.event();
    Coord x = e->pointer_x();
    Coord y = e->pointer_y();
    Coord l = a.left(); Coord r = a.right();
    Coord b = a.bottom(); Coord t = a.top();
    for (int i = 0; i < *_count; ++i) {
        Coord dx = x - (l * (1 - _x[i]) + r * _x[i]);
        Coord dy = y - (b * (1 - _y[i]) + t * _y[i]);
        if (Math::abs(dx) < 5 && Math::abs(dy) < 5) {
            h.target(depth, this, i);
        }
    }
}
