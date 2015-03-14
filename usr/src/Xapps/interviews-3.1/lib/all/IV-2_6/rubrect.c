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
 * Rubberbanding rectangles.
 */

#include <IV-2_6/InterViews/rubrect.h>
#include <IV-2_6/InterViews/painter.h>
#include <OS/math.h>
#include <math.h>
#include <stdlib.h>
#ifndef apollo
#include <values.h>
#endif

RubberRect::RubberRect(
    Painter* p, Canvas* c, IntCoord x0, IntCoord y0, IntCoord x1, IntCoord y1,
    IntCoord offx, IntCoord offy
) : Rubberband(p, c, offx, offy) {
    fixedx = x0;
    fixedy = y0;
    movingx = x1;
    movingy = y1;
    trackx = x1;
    tracky = y1;
}

void RubberRect::GetOriginal(
    IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
) {
    x0 = fixedx;
    y0 = fixedy;
    x1 = movingx;
    y1 = movingy;
}

void RubberRect::GetCurrent(
    IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
) {
    x0 = fixedx;
    y0 = fixedy;
    x1 = trackx;
    y1 = tracky;
}

void RubberRect::Draw() {
    IntCoord x0, y0, x1, y1;

    if (!drawn) {
	GetCurrent(x0, y0, x1, y1);
	if (x0 == x1 || y0 == y1) {
	    output->Line(canvas, x0+offx, y0+offy, x1+offx, y1+offy);
	} else {
            output->Rect(canvas, x0+offx, y0+offy, x1+offx, y1+offy);
	}
	drawn = true;
    }
}

RubberSquare::RubberSquare(
    Painter* p, Canvas* c, IntCoord x0, IntCoord y0, IntCoord x1, IntCoord y1,
    IntCoord offx, IntCoord offy
) : RubberRect(p, c, x0, y0, x1, y1, offx, offy) {
}

void RubberSquare::GetCurrent(
    IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
) {
    x0 = fixedx;
    y0 = fixedy;
    IntCoord dx = abs(trackx - x0);
    IntCoord dy = abs(tracky - y0);
    if (dx > dy) {
	x1 = trackx;
	y1 = (tracky > y0) ? y0 + dx : y0 - dx;
    } else {
	x1 = (trackx > x0) ? x0 + dy : x0 - dy;
	y1 = tracky;
    }
}

SlidingRect::SlidingRect(
    Painter* p, Canvas* c, IntCoord x0, IntCoord y0, IntCoord x1, IntCoord y1,
    IntCoord rfx, IntCoord rfy, IntCoord offx, IntCoord offy
) : RubberRect(p, c, x0, y0, x1, y1, offx, offy) {
    refx = rfx;
    refy = rfy;
    trackx = rfx;
    tracky = rfy;
}

void SlidingRect::GetCurrent(
    IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
) {
    IntCoord dx, dy;

    dx = trackx - refx;
    dy = tracky - refy;
    x0 = fixedx + dx;
    y0 = fixedy + dy;
    x1 = movingx + dx;
    y1 = movingy + dy;
}


StretchingRect::StretchingRect(
    Painter* p, Canvas* c,
    IntCoord x0, IntCoord y0, IntCoord x1, IntCoord y1, Side s,
    IntCoord offx, IntCoord offy
) : RubberRect(p, c, x0, y0, x1, y1, offx, offy) {
    side = s;
}

void StretchingRect::GetCurrent(
    IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
) {
    x0 = fixedx;
    y0 = fixedy;
    x1 = movingx;
    y1 = movingy;
    switch (side) {
        case LeftSide:
	    x0 = trackx;
	    break;
	case RightSide:
	    x1 = trackx;
	    break;
	case BottomSide:
	    y0 = tracky;
	    break;
	case TopSide:
	    y1 = tracky;
	    break;
    }
}

float StretchingRect::CurrentStretching() {
    IntCoord l, b, r, t;
    GetOriginal(l, b, r, t);
    IntCoord nl, nb, nr, nt;
    GetCurrent(nl, nb, nr, nt);

    float nsz = 0;
    float osz = 0;
    switch (side) {
    case LeftSide:
    case RightSide:
	nsz = nr - nl + 1;
	osz = r - l + 1;
	break;
    case BottomSide:
    case TopSide:
	nsz = nt - nb + 1;
	osz = t - b + 1;
	break;
    }

    if (osz == 0) {
	return MAXFLOAT;
    } else {
	return nsz / osz;
    }
}

ScalingRect::ScalingRect(
    Painter* p, Canvas* c, IntCoord x0, IntCoord y0, IntCoord x1, IntCoord y1,
    IntCoord cx, IntCoord cy, IntCoord offx, IntCoord offy
) : RubberRect(p, c, x0, y0, x1, y1, offx, offy) {
    centerx = cx;
    centery = cy;
    width = abs(x0 - x1);
    height = abs(y0 - y1);
}

void ScalingRect::GetCurrent(
    IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
) {
    double factor = CurrentScaling();

    x0 = Math::round(double(fixedx - centerx) * factor) + centerx;
    y0 = Math::round(double(fixedy - centery) * factor) + centery;
    x1 = Math::round(double(movingx - centerx) * factor) + centerx;
    y1 = Math::round(double(movingy - centery) * factor) + centery;
}

float ScalingRect::CurrentScaling() {
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

void RotatingRect::Transform(
    IntCoord& x, IntCoord& y,
    double a0, double a1, double b0, double b1, double c0, double c1
) {
    double tx, ty;

    tx = double(x);
    ty = double(y);
    
    x = Math::round(a0*tx + b0*ty + c0);
    y = Math::round(a1*tx + b1*ty + c1);
}

RotatingRect::RotatingRect(
    Painter* p, Canvas* c, IntCoord x0, IntCoord y0, IntCoord x1, IntCoord y1, 
    IntCoord cx, IntCoord cy, IntCoord rfx, IntCoord rfy,
    IntCoord offx, IntCoord offy
) : Rubberband(p, c, offx, offy) {
    left = x0;
    bottom = y0;
    right = x1;
    top = y1;
    centerx = cx;
    centery = cy;
    trackx = cx;
    tracky = cy;
    refx = rfx;
    refy = rfy;
}

void RotatingRect::GetOriginal(
    IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
) {
    x0 = left;
    y0 = bottom;
    x1 = right;
    y1 = top;
}

void RotatingRect::GetCurrent(
    IntCoord& leftbotx, IntCoord& leftboty,
    IntCoord& rightbotx, IntCoord& rightboty,
    IntCoord& righttopx, IntCoord& righttopy,
    IntCoord& lefttopx, IntCoord& lefttopy
) {
    double sin, cos, hprod, dx1, dy1, dx2, dy2;
    
    leftbotx = lefttopx = left - centerx;
    leftboty = rightboty = bottom - centery;
    rightbotx = righttopx = right - centerx;
    lefttopy = righttopy = top - centery;

    dx1 = double(refx - centerx);
    dy1 = double(refy - centery);
    dx2 = double(trackx - centerx);
    dy2 = double(tracky - centery);
    hprod = sqrt((dx1*dx1 + dy1*dy1) * (dx2*dx2 + dy2*dy2));
    if (hprod != 0.0) {
        cos = (dx1*dx2 + dy1*dy2) / hprod;
        sin = (dx1*dy2 - dx2*dy1) / hprod;
        Transform(leftbotx, leftboty, cos, sin, -sin, cos, 0.0, 0.0);
        Transform(rightbotx, rightboty, cos, sin, -sin, cos, 0.0, 0.0);
        Transform(righttopx, righttopy, cos, sin, -sin, cos, 0.0, 0.0);
        Transform(lefttopx, lefttopy, cos, sin, -sin, cos, 0.0, 0.0);
    }
    leftbotx += centerx;
    leftboty += centery;
    rightbotx += centerx;
    rightboty += centery;
    righttopx += centerx;
    righttopy += centery;
    lefttopx += centerx;
    lefttopy += centery;
}

float RotatingRect::CurrentAngle() {
    IntCoord x0, y0, x1, y1, dummy;

    GetCurrent(x0, y0, x1, y1, dummy, dummy, dummy, dummy);
    return Angle(x0, y0, x1, y1);
}

void RotatingRect::Draw() {
    IntCoord x[5], y[5];

    if (!drawn) {
	GetCurrent(x[0], y[0], x[1], y[1], x[2], y[2], x[3], y[3]);
	if ((x[0] == x[1] && y[1] != y[2]) || (y[0] == y[1] && x[1] != x[2]) ||
	    (x[1] == x[2] && y[1] == y[2])
	) {
	    output->Line(canvas, x[0]+offx, y[0]+offy, x[2]+offx, y[2]+offy);
	} else {
	    for (int i = 0; i < 4; ++i) {
	        x[i] += offx;
		y[i] += offy;
	    }
	    x[4] = x[0];
	    y[4] = y[0];
	    output->MultiLine(canvas, x, y, 5);
	}
	drawn = true;
    }

}
