/*
 * Copyright (c) 1987, 1988, 1989 Stanford University
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
 * Rubberbanding for ibuild components.
 */

#ifndef ibrubrect_h
#define ibrubrect_h

#include <InterViews/rubrect.h>

class ConstrainRect : public RubberRect {
public:
    ConstrainRect(
        Painter*, Canvas*, Coord x0, Coord y0, Coord x1, Coord y1,
	Coord offx, Coord offy, int xcon, int ycon
    );

    virtual void GetCurrent(Coord&, Coord&, Coord&, Coord&);
protected:
    virtual void NorthEast(Coord, Coord);
    virtual void NorthWest(Coord, Coord);
    virtual void SouthEast(Coord, Coord);
    virtual void SouthWest(Coord, Coord);
protected:
    int _xcon, _ycon;
};

class ConstrainScaleRect : public ScalingRect {
public:
    ConstrainScaleRect(
        Painter*, Canvas*, Coord x0, Coord y0, Coord x1, Coord y1,
	Coord cx, Coord cy, Coord offx = 0, Coord offy = 0
    );

    virtual void GetCurrent(Coord&, Coord&, Coord&, Coord&);
protected:
    virtual float XScaling();
    virtual float YScaling();
};

#endif
