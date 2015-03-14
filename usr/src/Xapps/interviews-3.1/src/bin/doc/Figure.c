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
 * planar figures
 */

#include "Figure.h"

#include <InterViews/brush.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <OS/math.h>

Figure::Figure (
    const Brush* brush, const Color* stroke, const Color* fill,
    boolean closed, boolean curved, int coords
) {
    _brush = brush;
    Resource::ref(_brush);
    _stroke = stroke;
    Resource::ref(_stroke);
    _fill = fill;
    Resource::ref(_fill);
    _closed = closed;
    _curved = curved;
    _count = 0;
    _x = new Coord[coords];
    _y = new Coord[coords];
}

Figure::~Figure () {
    Resource::unref(_brush);
    Resource::unref(_stroke);
    Resource::unref(_fill);
    delete _x;
    delete _y;
}

void Figure::add_point(Coord x, Coord y) {
    if (_count == 0) {
        _xmin = x - 1;
        _xmax = x + 1;
        _ymin = y - 1;
        _ymax = y + 1;
    } else {
        _xmin = Math::min(_xmin, x);
        _xmax = Math::max(_xmax, x);
        _ymin = Math::min(_ymin, y);
        _ymax = Math::max(_ymax, y);
    }
    _x[_count] = x;
    _y[_count] = y;
    _count += 1;
}

void Figure::add_curve(
    Coord x, Coord y, Coord x1, Coord y1, Coord x2, Coord y2
) {
    add_point(x1, y1);
    add_point(x2, y2);
    add_point(x, y);
}

void Figure::Bspline_move_to (
    Coord x, Coord y, Coord x1, Coord y1, Coord x2, Coord y2
) {
    Coord p1x = (x + x + x1) / 3;
    Coord p1y = (y + y + y1) / 3;
    Coord p2x = (x + x + x2) / 3;
    Coord p2y = (y + y + y2) / 3;
    add_point((p1x + p2x) / 2, (p1y + p2y) / 2);
}

void Figure::Bspline_curve_to (
    Coord x, Coord y, Coord x1, Coord y1, Coord x2, Coord y2
) {
    Coord p1x = (x + x + x1) / 3;
    Coord p1y = (y + y + y1) / 3;
    Coord p2x = (x + x + x2) / 3;
    Coord p2y = (y + y + y2) / 3;
    Coord p3x = (x1 + x1 + x) / 3;
    Coord p3y = (y1 + y1 + y) / 3;
    add_curve((p1x + p2x) / 2, (p1y + p2y) / 2, p3x, p3y, p1x, p1y);
}

void Figure::request(Requisition& requisition) const {
    if (_count > 0) {
        Requirement rx(-_xmin, -_xmin, -_xmin, _xmax, _xmax, _xmax);
        Requirement ry(-_ymin, -_ymin, -_ymin, _ymax, _ymax, _ymax);
        requisition.require(Dimension_X, rx);
        requisition.require(Dimension_Y, ry);
    }
}

void Figure::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    if (_count > 0) {
        Coord w = _brush == nil ? 0 : _brush->width();
        Coord x = a.x();
        Coord y = a.y();
	ext.set_xy(c, x+_xmin-w, y+_ymin-w, x+_xmax+w, y+_ymax+w);
    }
}

void Figure::draw(Canvas* c, const Allocation& allocation) const {
    if (c != nil && _count > 0) {
        Coord x = allocation.x();
        Coord y = allocation.y();
        c->new_path();
        c->move_to(x + _x[0], y + _y[0]);
        if (_curved) {
            for (int i = 1; i < _count; i += 3) {
                c->curve_to(
                    x + _x[i + 2], y + _y[i + 2],
                    x + _x[i], y + _y[i],
                    x + _x[i + 1], y + _y[i + 1]
                );
            }
        } else {
            for (int i = 1; i < _count; ++i) {
                c->line_to(x + _x[i], y + _y[i]);
            }
        }
        if (_closed) {
            c->close_path();
        }
        if (_fill != nil) {
	    c->fill(_fill);
	}
	if (_brush != nil && _stroke != nil) {
	    c->stroke(_stroke, _brush);
	}
    }
}

Line::Line(
    const Brush* brush, const Color* stroke, const Color* fill,
    Coord x1, Coord y1, Coord x2, Coord y2
) : Figure(brush, stroke, fill, false, false, 2) {
    add_point(x1, y1);
    add_point(x2, y2);
}

Line::~Line () { }

Rectangle::Rectangle (
    const Brush* brush, const Color* stroke, const Color* fill,
    Coord l, Coord b, Coord r, Coord t
) : Figure(brush, stroke, fill, true, false, 4) {
    add_point(l, b);
    add_point(l, t);
    add_point(r, t);
    add_point(r, b);
}

Rectangle::~Rectangle () { }

static float p0 = 1.00000000;
static float p1 = 0.89657547;   // cos 30 * sqrt(1 + tan 15 * tan 15)
static float p2 = 0.70710678;   // cos 45 
static float p3 = 0.51763809;   // cos 60 * sqrt(1 + tan 15 * tan 15)
static float p4 = 0.26794919;   // tan 15

Circle::Circle (
    const Brush* brush, const Color* stroke, const Color* fill,
    Coord x, Coord y, Coord r
) : Figure(brush, stroke, fill, true, true, 25) {
    float px0 = p0 * r, py0 = p0 * r;
    float px1 = p1 * r, py1 = p1 * r;
    float px2 = p2 * r, py2 = p2 * r;
    float px3 = p3 * r, py3 = p3 * r;
    float px4 = p4 * r, py4 = p4 * r;
    
    add_point(x + r, y);
    add_curve(x + px2, y + py2, x + px0, y + py4, x + px1, y + py3);
    add_curve(x, y + r, x + px3, y + py1, x + px4, y + py0);
    add_curve(x - px2, y + py2, x - px4, y + py0, x - px3, y + py1);
    add_curve(x - r, y, x - px1, y + py3, x - px0, y + py4);
    add_curve(x - px2, y - py2, x - px0, y - py4, x - px1, y - py3);
    add_curve(x, y - r, x - px3, y - py1, x - px4, y - py0);
    add_curve(x + px2, y - py2, x + px4, y - py0, x + px3, y - py1);
    add_curve(x + r, y, x + px1, y - py3, x + px0, y - py4);
}

Circle::~Circle () { }

Ellipse::Ellipse (
    const Brush* brush, const Color* stroke, const Color* fill,
    Coord x, Coord y, Coord rx, Coord ry
) : Figure(brush, stroke, fill, true, true, 25) {
    float px0 = p0 * rx, py0 = p0 * ry;
    float px1 = p1 * rx, py1 = p1 * ry;
    float px2 = p2 * rx, py2 = p2 * ry;
    float px3 = p3 * rx, py3 = p3 * ry;
    float px4 = p4 * rx, py4 = p4 * ry;
    
    add_point(x + rx, y);
    add_curve(x + px2, y + py2, x + px0, y + py4, x + px1, y + py3);
    add_curve(x, y + ry, x + px3, y + py1, x + px4, y + py0);
    add_curve(x - px2, y + py2, x - px4, y + py0, x - px3, y + py1);
    add_curve(x - rx, y, x - px1, y + py3, x - px0, y + py4);
    add_curve(x - px2, y - py2, x - px0, y - py4, x - px1, y - py3);
    add_curve(x, y - ry, x - px3, y - py1, x - px4, y - py0);
    add_curve(x + px2, y - py2, x + px4, y - py0, x + px3, y - py1);
    add_curve(x + rx, y, x + px1, y - py3, x + px0, y - py4);
}

Ellipse::~Ellipse () { }

Open_BSpline::Open_BSpline (
    const Brush* brush, const Color* stroke, const Color* fill,
    Coord* x, Coord* y, int n
) : Figure(brush, stroke, fill, false, true, (n + 2) * 3 + 1) {
    Bspline_move_to(x[0], y[0], x[0], y[0], x[0], y[0]);
    Bspline_curve_to(x[0], y[0], x[0], y[0], x[1], y[1]);
    for (int i = 1; i < n - 1; ++i) {
        Bspline_curve_to(x[i], y[i], x[i-1], y[i-1], x[i+1], y[i+1]);
    }
    Bspline_curve_to(x[n-1], y[n-1], x[n-2], y[n-2], x[n-1], y[n-1]);
    Bspline_curve_to(x[n-1], y[n-1], x[n-1], y[n-1], x[n-1], y[n-1]);
}

Open_BSpline::~Open_BSpline () { }

Closed_BSpline::Closed_BSpline (
    const Brush* brush, const Color* stroke, const Color* fill,
    Coord* x, Coord* y, int n
) : Figure(brush, stroke, fill, true, true, n * 3 + 1) {
    Bspline_move_to(x[0], y[0], x[n-1], y[n-1], x[1], y[1]);
    for (int i = 1; i < n - 1; ++i) {
        Bspline_curve_to(x[i], y[i], x[i-1], y[i-1], x[i+1], y[i+1]);
    }
    Bspline_curve_to(x[n-1], y[n-1], x[n-2], y[n-2], x[0], y[0]);
    Bspline_curve_to(x[0], y[0], x[n-1], y[n-1], x[1], y[1]);
}

Closed_BSpline::~Closed_BSpline () { }

Polyline::Polyline (
    const Brush* brush, const Color* stroke, const Color* fill,
    Coord* x, Coord* y, int n
) : Figure(brush, stroke, fill, false, false, n) {
    add_point(x[0], y[0]);
    for (int i = 1; i < n; ++i) {
        add_point(x[i], y[i]);
    }
}

Polyline::~Polyline () { }

Polygon::Polygon (
    const Brush* brush, const Color* stroke, const Color* fill,
    Coord* x, Coord* y, int n
) : Figure(brush, stroke, fill, true, false, n) {
    add_point(x[0], y[0]);
    for (int i = 1; i < n; ++i) {
        add_point(x[i], y[i]);
    }
}

Polygon::~Polygon () { }
