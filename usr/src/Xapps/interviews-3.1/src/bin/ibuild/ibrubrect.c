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
 * Rubberbanding rectangles.
 */

#include "ibrubrect.h"

#include <InterViews/minmax.h>
#include <InterViews/painter.h>

#include <math.h>
#include <stdlib.h>

/**************************************************************************/

ConstrainRect::ConstrainRect (
    Painter* p, Canvas* c, Coord x0, Coord y0, Coord x1, Coord y1,
    Coord offx, Coord offy, int xcon, int ycon
) : RubberRect(p, c, x0, y0, x1, y1, offx, offy) {
    _xcon = xcon;
    _ycon = ycon;
}

void ConstrainRect::GetCurrent (Coord& l, Coord& b, Coord& r, Coord& t) {
    if (trackx > fixedx && tracky > fixedy) {
        NorthEast(trackx, tracky);
    } else if (trackx > fixedx && tracky < fixedy) {
        SouthEast(trackx, tracky);
    } else if (trackx < fixedx && tracky > fixedy) {
        NorthWest(trackx, tracky);
    } else {
        SouthWest(trackx, tracky);
    }
    RubberRect::GetCurrent(l, b, r, t);
}

void ConstrainRect::NorthEast(Coord x, Coord y) {
    trackx = ((x-fixedx) > _xcon ) ? x : fixedx + _xcon;
    tracky = ((y-fixedy) > _ycon ) ? y : fixedy + _ycon;
}

void ConstrainRect::SouthEast(Coord x, Coord y) {
    trackx = ((x-fixedx) > _xcon ) ? x : fixedx + _xcon;
    tracky = ((fixedy-y) > _ycon ) ? y : fixedy - _ycon;
}

void ConstrainRect::NorthWest(Coord x, Coord y) {
    trackx = ((fixedx-x) > _xcon ) ? x : fixedx - _xcon;
    tracky = ((y-fixedy) > _ycon ) ? y : fixedy + _ycon;
}

void ConstrainRect::SouthWest(Coord x, Coord y) {
    trackx = ((fixedx-x) > _xcon ) ? x : fixedx - _xcon;
    tracky = ((fixedy-y) > _ycon ) ? y : fixedy - _ycon;
}

/**************************************************************************/

ConstrainScaleRect::ConstrainScaleRect (
    Painter* p, Canvas* c, Coord x0, Coord y0, Coord x1, Coord y1,
    Coord cx, Coord cy, Coord offx, Coord offy
) : ScalingRect(p, c, x0, y0, x1, y1, cx, cy, offx, offy) {}

void ConstrainScaleRect::GetCurrent (
    Coord& x0, Coord& y0, Coord& x1, Coord& y1
) {
    double xfactor = XScaling();
    double yfactor = YScaling();

    xfactor = (xfactor < 1.0) ? 1.0 : xfactor;
    yfactor = (yfactor < 1.0) ? 1.0 : yfactor;

    x0 = round(double(fixedx - centerx) * xfactor) + centerx;
    y0 = round(double(fixedy - centery) * yfactor) + centery;
    x1 = round(double(movingx - centerx) * xfactor) + centerx;
    y1 = round(double(movingy - centery) * yfactor) + centery; 
}

float ConstrainScaleRect::XScaling () {
    Coord dx;
    double factor = 1;

    dx = abs(trackx - centerx);
    if (width != 0) {
        factor = double(2 * dx) / double(width);
    }
    return factor;
}

float ConstrainScaleRect::YScaling () {
    Coord dy;
    double factor = 1;

    dy = abs(tracky - centery);
    if (height != 0) {
        factor = double(2 * dy) / double(height);
    }
    return factor;
}
