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

#ifndef figure_h
#define figure_h

#include <InterViews/glyph.h>

class Brush;
class Color;

class Figure : public Glyph {
public:
    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
protected:
    Figure (
        const Brush* brush, const Color* stroke, const Color* fill,
        boolean closed, boolean curved, int coords
    );
    virtual ~Figure ();

    void add_point (Coord x, Coord y);
    void add_curve (Coord x, Coord y, Coord x1, Coord y1, Coord x2, Coord y2);
    void Bspline_move_to (
        Coord x, Coord y, Coord x1, Coord y1, Coord x2, Coord y2
    );
    void Bspline_curve_to (
        Coord x, Coord y, Coord x1, Coord y1, Coord x2, Coord y2
    );

    const Brush* _brush;
    const Color* _stroke;
    const Color* _fill;

    boolean _closed;
    boolean _curved;
    int _count;
    Coord* _x;
    Coord* _y;

    Coord _xmin;
    Coord _xmax;
    Coord _ymin;
    Coord _ymax;
};

class Line : public Figure {
public:
    Line (
        const Brush* brush, const Color* stroke, const Color* fill,
        Coord x1, Coord y1, Coord x2, Coord y2
    );
protected:
    virtual ~Line ();
};

class Rectangle : public Figure {
public:
    Rectangle (
        const Brush* brush, const Color* stroke, const Color* fill,
        Coord l, Coord b, Coord r, Coord t
    );
protected:
    virtual ~Rectangle ();
};

class Circle : public Figure {
public:
    Circle (
        const Brush* brush, const Color* stroke, const Color* fill,
        Coord x, Coord y, Coord r
    );
protected:
    virtual ~Circle ();
};

class Ellipse : public Figure {
public:
    Ellipse (
        const Brush* brush, const Color* stroke, const Color* fill,
        Coord x, Coord y, Coord rx, Coord ry
    );
protected:
    virtual ~Ellipse ();
};

class Open_BSpline : public Figure {
public:
    Open_BSpline (
        const Brush* brush, const Color* stroke, const Color* fill,
        Coord* x, Coord* y, int count
    );
protected:
    virtual ~Open_BSpline ();
};

class Closed_BSpline : public Figure {
public:
    Closed_BSpline (
        const Brush* brush, const Color* stroke, const Color* fill,
        Coord* x, Coord* y, int count
    );
protected:
    virtual ~Closed_BSpline ();
};

class Polyline : public Figure {
public:
    Polyline (
        const Brush* brush, const Color* stroke, const Color* fill,
        Coord* x, Coord* y, int count
    );
protected:
    virtual ~Polyline ();
};

class Polygon : public Figure {
public:
    Polygon (
        const Brush* brush, const Color* stroke, const Color* fill,
        Coord* x, Coord* y, int count
    );
protected:
    virtual ~Polygon ();
};

#endif
