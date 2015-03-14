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
 * Rubberbanding curves.
 */

#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/rubcurve.h>
#include <OS/math.h>
#include <OS/memory.h>
#include <math.h>
#include <stdlib.h>

RubberEllipse::RubberEllipse(
    Painter* p, Canvas* c, IntCoord cx, IntCoord cy, IntCoord rx, IntCoord ry, 
    IntCoord offx, IntCoord offy
) : Rubberband(p, c, offx, offy) {
    centerx = cx;
    centery = cy;
    radiusx = rx;
    radiusy = ry;    
    trackx = rx;
    tracky = ry;
}

void RubberEllipse::GetOriginal(
    IntCoord& cx, IntCoord& cy, IntCoord& rx, IntCoord& ry
) {
    cx = centerx;
    cy = centery;
    rx = radiusx;
    ry = radiusy;
}

void RubberEllipse::GetCurrent(
    IntCoord& cx, IntCoord& cy, IntCoord& rx, IntCoord& ry
) {
    cx = centerx;
    cy = centery;
    rx = trackx;
    ry = tracky;
}

void RubberEllipse::OriginalRadii(int& xr, int& yr) {
    xr = abs(radiusx - centerx);
    yr = abs(radiusy - centery);
}

void RubberEllipse::CurrentRadii(int& xr, int& yr) {
    xr = abs(trackx - centerx);
    yr = abs(tracky - centery);
}

void RubberEllipse::Draw() {
    IntCoord cx, cy, rx, ry, xr, yr;

    if (!drawn) {
	GetCurrent(cx, cy, rx, ry);
	CurrentRadii(xr, yr);
	output->Ellipse(canvas, cx+offx, cy+offy, xr, yr);
	drawn = true;
    }
}

/**************************************************************************/

SlidingEllipse::SlidingEllipse(
    Painter* p, Canvas* c, IntCoord cx, IntCoord cy, IntCoord xr, IntCoord yr,
    IntCoord rfx, IntCoord rfy, IntCoord offx, IntCoord offy
) : RubberEllipse(p, c, cx, cy, xr, yr, offx, offy) {
    refx = trackx = rfx;
    refy = tracky = rfy;
}

void SlidingEllipse::GetCurrent(
    IntCoord& cx, IntCoord& cy, IntCoord& xr, IntCoord& yr
) {
    IntCoord dx = trackx - refx;
    IntCoord dy = tracky - refy;

    cx = centerx + dx;
    cy = centery + dy;
    xr = radiusx;
    yr = radiusy;
}

void SlidingEllipse::OriginalRadii(int& xr, int& yr) {
    xr = radiusx;
    yr = radiusy;
}

void SlidingEllipse::CurrentRadii(int& xr, int& yr) {
    xr = radiusx;
    yr = radiusy;
}

/*****************************************************************************/

RubberCircle::RubberCircle(
    Painter* p, Canvas* c, IntCoord cx, IntCoord cy, IntCoord rx, IntCoord ry,
    IntCoord offx, IntCoord offy
) : RubberEllipse(p, c, cx, cy, rx, ry, offx, offy) {
    /* nothing else to do */
}

void RubberCircle::OriginalRadii(int& xr, int& yr) {
    IntCoord dx = radiusx - centerx;
    IntCoord dy = radiusy - centery;
    int radius = Math::round(sqrt(dx*dx + dy*dy));
    xr = radius;
    yr = radius;
}

void RubberCircle::CurrentRadii(int& xr, int& yr) {
    IntCoord dx = trackx - centerx;
    IntCoord dy = tracky - centery;
    int radius = Math::round(sqrt(dx*dx + dy*dy));
    xr = radius;
    yr = radius;
}

void RubberCircle::Draw() {
    int radius;

    if (!drawn) {
	CurrentRadii(radius, radius);
	output->Circle(canvas, centerx + offx, centery + offy, radius);
	drawn = true;
    }
}

/*****************************************************************************/

void RubberPointList::Copy(
    IntCoord* x, IntCoord* y, int n, IntCoord*& nx, IntCoord*& ny
) {
    nx = new IntCoord[n];
    ny = new IntCoord[n];
    Memory::copy(x, nx, n*sizeof(IntCoord));
    Memory::copy(y, ny, n*sizeof(IntCoord));
}

RubberPointList::RubberPointList(
    Painter* p, Canvas* c, IntCoord px[], IntCoord py[], int n,
    IntCoord offx, IntCoord offy
) : Rubberband(p, c, offx, offy) {
    Copy(px, py, n, x, y);
    count = n;
}

RubberPointList::~RubberPointList() {
    delete x;
    delete y;
}

/*****************************************************************************/

RubberVertex::RubberVertex(
    Painter* p, Canvas* c, IntCoord px[], IntCoord py[], int n, int pt,
    IntCoord offx, IntCoord offy
) : RubberPointList(p, c, px, py, n, offx, offy) {
    rubberPt = pt;
    trackx = x[rubberPt];
    tracky = y[rubberPt];
}

void RubberVertex::GetOriginal(IntCoord*& px, IntCoord*& py, int& n, int& pt) {
    Copy(x, y, count, px, py);
    n = count;
    pt = rubberPt;
}

void RubberVertex::GetCurrent(IntCoord*& px, IntCoord*& py, int& n, int& pt) {
    Copy(x, y, count, px, py);
    n = count;
    pt = rubberPt;
    px[rubberPt] = trackx;
    py[rubberPt] = tracky;
}

void RubberVertex::DrawSplineSection(
    Painter* p, Canvas* c, IntCoord x[], IntCoord y[]
) {
    double twicex1, twicex2, p0x, p1x, p2x, p3x, tempx;
    double twicey1, twicey2, p0y, p1y, p2y, p3y, tempy;

    twicex1 = 2.0*double(x[1]);
    twicey1 = 2.0*double(y[1]);
    twicex2 = 2.0*double(x[2]);
    twicey2 = 2.0*double(y[2]);
    
    p1x = (twicex1 + double(x[2])) / 3.0;
    p1y = (twicey1 + double(y[2])) / 3.0;
    p2x = (twicex2 + double(x[1])) / 3.0;
    p2y = (twicey2 + double(y[1])) / 3.0;
    tempx = (twicex1 + double(x[0])) / 3.0;
    tempy = (twicey1 + double(y[0])) / 3.0;
    p0x = (tempx + p1x) / 2.0;
    p0y = (tempy + p1y) / 2.0;
    tempx = (twicex2 + double(x[3])) / 3.0;
    tempy = (twicey2 + double(y[3])) / 3.0;
    p3x = (tempx + p2x) / 2.0;
    p3y = (tempy + p2y) / 2.0;
    p->Curve(c,
        Math::round(p0x)+offx, Math::round(p0y)+offy,
	Math::round(p1x)+offx, Math::round(p1y)+offy,
	Math::round(p2x)+offx, Math::round(p2y)+offy,
	Math::round(p3x)+offx, Math::round(p3y)+offy
    );
}

/*****************************************************************************/

RubberHandles::RubberHandles(
    Painter* p, Canvas* c, IntCoord px[], IntCoord py[], int n,
    int pt, int size, IntCoord offx, IntCoord offy
) : RubberVertex(p, c, px, py, n, pt, offx, offy) {
     d = size / 2;
}

void RubberHandles::Draw() {
    register int i;

    if (x == nil || y == nil) {
        return;
    }
    if (!drawn) {
	for (i = 0; i < count; ++i) {
	    if (i == rubberPt) {
		output->FillRect(canvas,
		    trackx - d + offx, tracky - d + offy, 
		    trackx + d + offx, tracky + d + offy
		);
	    } else {
		output->FillRect(canvas,
		    x[i] - d + offx, y[i] - d + offy,
		    x[i] + d + offx, y[i] + d + offy
		);
	    }	    
	}
	drawn = true;
    }
}

void RubberHandles::Track(IntCoord x, IntCoord y) {
    if (x != trackx || y != tracky) {
        if (drawn) {
	    /* erase */
	    output->FillRect(canvas,
		trackx - d + offx, tracky - d + offy,
		trackx + d + offx, tracky + d + offy
	    );
	}
	trackx = x;
	tracky = y;
	output->FillRect(canvas,
	    trackx - d + offx, tracky - d + offy,
	    trackx + d + offx, tracky + d + offy
	);
	drawn = true;
    }
}

/*****************************************************************************/

RubberSpline::RubberSpline(
    Painter* p, Canvas* c, IntCoord px[], IntCoord py[], int n, int pt,
    IntCoord offx, IntCoord offy
) : RubberVertex(p, c, px, py, n, pt, offx, offy) {
    /* nothing else to do */
}

void RubberSpline::Draw() {
    register int i, j;
    IntCoord sx[7], sy[7];

    if (x == nil || y == nil) {
        return;
    }
    if (!drawn) {
        for (i = -3; i <= 3; ++i) {
            j = Math::min(Math::max(rubberPt + i, 0), count - 1);
	    if (j == rubberPt) {
	        sx[i + 3] = trackx;
		sy[i + 3] = tracky;
	    } else {
	        sx[i + 3] = x[j];
	        sy[i + 3] = y[j];
	    }
        }
        DrawSplineSection(output, canvas, sx, sy);
        DrawSplineSection(output, canvas, &sx[1], &sy[1]);
        DrawSplineSection(output, canvas, &sx[2], &sy[2]);
        DrawSplineSection(output, canvas, &sx[3], &sy[3]);
	drawn = true;
    }
}

/*****************************************************************************/

RubberClosedSpline::RubberClosedSpline(
    Painter* p, Canvas* c, IntCoord px[], IntCoord py[], int n, int pt,
    IntCoord offx, IntCoord offy
) : RubberVertex(p, c, px, py, n, pt, offx, offy) {
    /* nothing else to do */
}

void RubberClosedSpline::Draw() {
    register int i, j;
    IntCoord sx[7], sy[7];
    
    if (x == nil || y == nil) {
        return;
    }
    if (!drawn) {
	if (count > 2) {
	    for (i = -3; i <= 3; ++i) {
		j = (rubberPt + count + i) % count;
		if (j == rubberPt) {
		    sx[i + 3] = trackx;
		    sy[i + 3] = tracky;
		} else {
		    sx[i + 3] = x[j];
		    sy[i + 3] = y[j];
		}
	    }
	    DrawSplineSection(output, canvas, sx, sy);
	    DrawSplineSection(output, canvas, &sx[1], &sy[1]);
	    DrawSplineSection(output, canvas, &sx[2], &sy[2]);
	    if (count > 3) {
		DrawSplineSection(output, canvas, &sx[3], &sy[3]);
	    }
	} else {
	    i = 1 - rubberPt;
	    output->Line(canvas, x[i], y[i], trackx, tracky);
	}
	drawn = true;
    }
}

/*****************************************************************************/

SlidingPointList::SlidingPointList(
    Painter* p, Canvas* c, IntCoord px[], IntCoord py[], int n,
    IntCoord rfx, IntCoord rfy, IntCoord offx, IntCoord offy
) : RubberPointList(p, c, px, py, n, offx, offy) {
    refx = rfx;
    refy = rfy;
    trackx = rfx;
    tracky = rfy;
}

void SlidingPointList::GetOriginal(IntCoord*& px, IntCoord*& py, int& n) {
    register int i;
    register IntCoord dx = trackx - refx;
    register IntCoord dy = tracky - refy;

    px = new IntCoord[count];
    py = new IntCoord[count];
    n = this->count;
    for (i = 0; i < count; i++) {
        px[i] = x[i] - dx;
	py[i] = y[i] - dy;
    }
}

void SlidingPointList::GetCurrent(IntCoord*& px, IntCoord*& py, int& n) {
    Copy(x, y, count, px, py);
    n = count;
}

void SlidingPointList::Draw() {
    if (x == nil || y == nil) {
        return;
    }
    if (!drawn) {
	if (offx == 0 && offy == 0) {
	    output->MultiPoint(canvas, x, y, count);
	} else {
	    register IntCoord* ox = new IntCoord[count];
	    register IntCoord* oy = new IntCoord[count];
	    for (register int i = 0; i < count; i++) {
		ox[i] = x[i] + offx;
		oy[i] = y[i] + offy;
	    }
	    output->MultiPoint(canvas, ox, oy, count);
	    delete ox;
	    delete oy;
	}
	drawn = true;
    }
}

void SlidingPointList::Track(IntCoord x0, IntCoord y0) {
    register int i;
    register IntCoord dx, dy;

    if (x0 != trackx || y0 != tracky) {
        Erase();
	dx = x0 - trackx;
	dy = y0 - tracky;
	for (i = 0; i < count; i++) {
	    x[i] += dx;
	    y[i] += dy;
	}
	trackx = x0;
	tracky = y0;
	Draw();
    }
}

/*****************************************************************************/

SlidingLineList::SlidingLineList(
    Painter* p, Canvas* c, IntCoord px[], IntCoord py[], int n,
    IntCoord rfx, IntCoord rfy, IntCoord offx, IntCoord offy
) : SlidingPointList(p, c, px, py, n, rfx, rfy, offx, offy) {
}

void SlidingLineList::Draw() {
    if (x == nil || y == nil) {
        return;
    }
    if (!drawn) {
	if (offx == 0 && offy == 0) {
	    output->MultiLine(canvas, x, y, count);
	} else {
	    register IntCoord* ox = new IntCoord[count];
	    register IntCoord* oy = new IntCoord[count];
	    for (register int i = 0; i < count; i++) {
		ox[i] = x[i] + offx;
		oy[i] = y[i] + offy;
	    }
	    output->MultiLine(canvas, ox, oy, count);
	    delete ox;
	    delete oy;
	}
	drawn = true;
    }
}

/**************************************************************************/

static void Bounds(IntCoord c[], int n, IntCoord& lower, IntCoord& upper) {
    lower = upper = c[0];

    for (int i = 1; i < n; ++i) {
        lower = Math::min(lower, c[i]);
        upper = Math::max(upper, c[i]);
    }
}

ScalingLineList::ScalingLineList(
    Painter* p, Canvas* c, IntCoord px[], IntCoord py[], int n,
    IntCoord cx, IntCoord cy, IntCoord offx, IntCoord offy
) : RubberPointList(p, c, px, py, n, offx, offy) {
    Copy(px, py, n, newx, newy);
    centerx = cx;
    centery = cy;

    IntCoord l, b, r, t;
    Bounds(px, n, l, r);
    Bounds(py, n, b, t);
    width = r - l;
    height = b - t;
}

ScalingLineList::~ScalingLineList() {
    delete newx;
    delete newy;
}

void ScalingLineList::Update() {
    float factor = CurrentScaling();

    for (int i = 0; i < count; ++i) {
	newx[i] = Math::round(float(x[i] - centerx)*factor) + centerx;
	newy[i] = Math::round(float(y[i] - centery)*factor) + centery;
    }
}

void ScalingLineList::GetOriginal(IntCoord*& px, IntCoord*& py, int& n) {
    Copy(x, y, count, px, py);
    n = count;
}

void ScalingLineList::GetCurrent(IntCoord*& px, IntCoord*& py, int& n) {
    Copy(newx, newy, count, px, py);
    n = count;
}

float ScalingLineList::CurrentScaling() {
    IntCoord dx = abs(trackx - centerx);
    IntCoord dy = abs(tracky - centery);
    double factor = 1;

    if (width != 0 && dx > dy) {
        factor = double(2 * dx) / double(width);
    } else if (height != 0) {
        factor = double(2 * dy) / double(height);
    }
    return factor;
}

void ScalingLineList::Track(IntCoord x, IntCoord y) {
    if (x != trackx || y != tracky) {
	Erase();
	trackx = x;
	tracky = y;
	Update();
	Draw();
    }
}    

void ScalingLineList::Draw() {
    if (!drawn) {
        output->MultiLine(canvas, newx, newy, count);
        drawn = true;
    }
}

/**************************************************************************/

RotatingLineList::RotatingLineList(
    Painter* p, Canvas* c, IntCoord px[], IntCoord py[], int n,
    IntCoord cx, IntCoord cy, IntCoord rfx, IntCoord rfy,
    IntCoord offx, IntCoord offy
) : RubberPointList(p, c, px, py, n, offx, offy) {
    Copy(px, py, n, newx, newy);
    centerx = cx;
    centery = cy;
    refx = rfx;
    refy = rfy;
}

RotatingLineList::~RotatingLineList() {
    delete newx;
    delete newy;
}

void RotatingLineList::Update() {
    float angle = (CurrentAngle() - OriginalAngle()) * M_PI/180.0;
    float cosine = cos(angle);
    float sine = sin(angle);
    float tx, ty;
    
    for (int i = 0; i < count; ++i) {
	tx = float(x[i] - centerx);
	ty = float(y[i] - centery);
	newx[i] = Math::round(cosine*tx - sine*ty) + centerx;
	newy[i] = Math::round(sine*tx + cosine*ty) + centery;
    }
}

void RotatingLineList::GetOriginal(IntCoord*& px, IntCoord*& py, int& n) {
    Copy(x, y, count, px, py);
    n = count;
}

void RotatingLineList::GetCurrent(IntCoord*& px, IntCoord*& py, int& n) {
    Copy(newx, newy, count, px, py);
    n = count;
}

float RotatingLineList::OriginalAngle() {
    return Angle(centerx, centery, refx, refy);
}

float RotatingLineList::CurrentAngle() {
    return Angle(centerx, centery, trackx, tracky);
}

void RotatingLineList::Track(IntCoord x, IntCoord y) {
    if (x != trackx || y != tracky) {
	Erase();
	trackx = x;
	tracky = y;
	Update();
	Draw();
    }
}    

void RotatingLineList::Draw() {
    if (!drawn) {
        output->MultiLine(canvas, newx, newy, count);
        drawn = true;
    }
}
