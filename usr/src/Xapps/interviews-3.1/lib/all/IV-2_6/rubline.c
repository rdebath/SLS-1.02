/*
 * Copyright (c) 1987, 1988, 1989, 1990, 1991 Stanford University
 * Copyright (c) 1991 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Stanford and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Stanford and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL STANFORD OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

/*
 * Rubberbanding lines.
 */

#include <IV-2_6/InterViews/rubline.h>
#include <IV-2_6/InterViews/painter.h>
#include <OS/math.h>
#include <math.h>
#include <stdlib.h>

RubberLine::RubberLine(
    Painter* p, Canvas* c,
    IntCoord x0, IntCoord y0, IntCoord x1, IntCoord y1,
    IntCoord offx, IntCoord offy
) : Rubberband(p, c, offx, offy) {
    fixedx = x0;
    fixedy = y0;
    movingx = x1;
    movingy = y1;
    trackx = x1;
    tracky = y1;
}

void RubberLine::GetOriginal(
    IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
) {
    x0 = fixedx;
    y0 = fixedy;
    x1 = movingx;
    y1 = movingy;
}

void RubberLine::GetCurrent(
    IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
) {
    x0 = fixedx;
    y0 = fixedy;
    x1 = trackx;
    y1 = tracky;
}

void RubberLine::Draw() {
    IntCoord x0, y0, x1, y1;

    if (!drawn) {
	GetCurrent(x0, y0, x1, y1);
	output->Line(canvas, x0+offx, y0+offy, x1+offx, y1+offy);
	drawn = true;
    }
}

/*****************************************************************************/

RubberAxis::RubberAxis(
    Painter* p, Canvas* c, IntCoord x0, IntCoord y0, IntCoord x1, IntCoord y1,
    IntCoord offx, IntCoord offy
) : RubberLine(p, c, x0, y0, x1, y1, offx, offy) {
    /* nothing else to do */
}

void RubberAxis::GetCurrent(
    IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
) {
    x0 = fixedx;
    y0 = fixedy;
    if (abs(fixedx - trackx) < abs(fixedy - tracky)) {
	x1 = fixedx;
	y1 = tracky;
    } else {
	x1 = trackx;
	y1 = fixedy;
    }
}

/*****************************************************************************/

SlidingLine::SlidingLine(
    Painter* p, Canvas* c, IntCoord x0, IntCoord y0, IntCoord x1, IntCoord y1, 
    IntCoord rfx, IntCoord rfy, IntCoord offx, IntCoord offy
) : RubberLine(p, c, x0, y0, x1, y1, offx, offy) {
    refx = rfx;
    refy = rfy;
    trackx = rfx;
    tracky = rfy;
}

void SlidingLine::GetCurrent(
    IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
) {
    IntCoord dx = trackx - refx;
    IntCoord dy = tracky - refy;

    x0 = fixedx + dx;
    y0 = fixedy + dy;
    x1 = movingx + dx;
    y1 = movingy + dy;
}

/*****************************************************************************/

ScalingLine::ScalingLine(
    Painter* p, Canvas* c, IntCoord x0, IntCoord y0, IntCoord x1, IntCoord y1,
    IntCoord cx, IntCoord cy, IntCoord offx, IntCoord offy
) : RubberLine(p, c, x0, y0, x1, y1, offx, offy) {
    centerx = cx;
    centery = cy;
    width = abs(x0 - x1);
    height = abs(y0 - y1);
}

void ScalingLine::GetCurrent(
    IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
) {
    double factor = CurrentScaling();

    x0 = Math::round(double(fixedx - centerx) * factor) + centerx;
    y0 = Math::round(double(fixedy - centery) * factor) + centery;
    x1 = Math::round(double(movingx - centerx) * factor) + centerx;
    y1 = Math::round(double(movingy - centery) * factor) + centery;
}

float ScalingLine::CurrentScaling() {
    IntCoord dx, dy;
    double factor = 1;
    
    dx = abs(trackx - centerx);
    dy = abs(tracky - centery);
    if (width != 0 && dx > dy) {
        factor = double(2 * dx) / double(width);
    } else if (height != 0) {
        factor = double(2 * dy) / double(height);
    }
    return factor;
}

/*****************************************************************************/

void RotatingLine::Transform(
    IntCoord& x, IntCoord& y,
    double a0, double a1, double b0, double b1, double c0, double c1
) {
    double tx, ty;

    tx = double(x);
    ty = double(y);
    x = Math::round(a0*tx + b0*ty + c0);
    y = Math::round(a1*tx + b1*ty + c1);
}

RotatingLine::RotatingLine(
    Painter* p, Canvas* c, IntCoord x0, IntCoord y0, IntCoord x1, IntCoord y1, 
    IntCoord cx, IntCoord cy, IntCoord rfx, IntCoord rfy,
    IntCoord offx, IntCoord offy
) : RubberLine(p, c, x0, y0, x1, y1, offx, offy) {
    centerx = cx;
    centery = cy;
    refx = rfx;
    refy = rfy;
}    

void RotatingLine::GetCurrent(
    IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
) {
    double sin, cos, hprod, dx1, dy1, dx2, dy2;
    
    x0 = fixedx - centerx;
    y0 = fixedy - centery;
    x1 = movingx - centerx;
    y1 = movingy - centery;

    dx1 = double(refx - centerx);
    dy1 = double(refy - centery);
    dx2 = double(trackx - centerx);
    dy2 = double(tracky - centery);
    hprod = sqrt((dx1*dx1 + dy1*dy1) * (dx2*dx2 + dy2*dy2));
    if (hprod != 0.0) {
        cos = (dx1*dx2 + dy1*dy2) / hprod;
        sin = (dx1*dy2 - dx2*dy1) / hprod;
        Transform(x0, y0, cos, sin, -sin, cos, 0.0, 0.0);
        Transform(x1, y1, cos, sin, -sin, cos, 0.0, 0.0);
    }
    x0 += centerx;
    y0 += centery;
    x1 += centerx;
    y1 += centery;
}

float RotatingLine::OriginalAngle() {
    IntCoord x0, y0, x1, y1;

    GetOriginal(x0, y0, x1, y1);
    return Angle(x0, y0, x1, y1);
}

float RotatingLine::CurrentAngle() {
    IntCoord x0, y0, x1, y1;

    GetCurrent(x0, y0, x1, y1);
    return Angle(x0, y0, x1, y1);
}
