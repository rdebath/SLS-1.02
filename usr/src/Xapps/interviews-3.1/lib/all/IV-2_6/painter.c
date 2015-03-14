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
 * Graphics primitives.
 */

#include <InterViews/brush.h>
#include <InterViews/color.h>
#include <InterViews/font.h>
#include <InterViews/pattern.h>
#include <InterViews/transformer.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/textstyle.h>
#include <IV-2_6/InterViews/world.h>
#include <OS/math.h>
#include <OS/memory.h>
#include <stdlib.h>
#include <string.h>

/*
 * For reasons of caching, it is important that the attributes
 * are set to nil (or 0) before being set to their default values.
 * Also, it is important that the colors are set before the fill pattern.
 */

void Painter::Init() {
    foreground = nil;
    background = nil;
    pattern = nil;
    br = nil;
    font = nil;
    style = 0;
    matrix = nil;

    World* w = World::current();
    SetColors(w->foreground(), w->background());
    SetPattern(new Pattern);
    FillBg(true);
    SetBrush(new Brush(0xffff, 0));
    SetFont(w->font());
    SetStyle(Plain);
    SetOrigin(0, 0);
    MoveTo(0, 0);
}

void Painter::Copy(Painter* copy) {
    foreground = nil;
    background = nil;
    pattern = nil;
    br = nil;
    font = nil;
    style = 0;
    matrix = nil;
    SetColors(copy->foreground, copy->background);
    SetPattern(copy->pattern);
    SetBrush(copy->br);
    SetFont(copy->font);
    SetStyle(copy->style);
    SetTransformer(copy->matrix);
    SetOrigin(copy->xoff, copy->yoff);
    MoveTo(copy->curx, copy->cury);
}

const Color* Painter::GetFgColor() const { return foreground; }
const Color* Painter::GetBgColor() const { return background; }
const Pattern* Painter::GetPattern() const { return pattern; }
const Brush* Painter::GetBrush() const { return br; }
const Font* Painter::GetFont() const { return font; }
void Painter::SetStyle(int s) { style = s; }
int Painter::GetStyle() const { return style; }

void Painter::SetTransformer(Transformer *t) {
    if (matrix != t) {
	Unref(matrix);
	matrix = t;
	if (matrix != nil) {
	    matrix->Reference();
	}
    }
}

Transformer* Painter::GetTransformer() const { return matrix; }

void Painter::MoveTo(int x, int y) {
    curx = x;
    cury = y;
}

void Painter::GetPosition(int& x, int& y) const {
    x = curx;
    y = cury;
}

void Painter::SetOrigin(int x0, int y0) {
    xoff = x0;
    yoff = y0;
}

void Painter::GetOrigin(int& x0, int& y0) const {
    x0 = xoff;
    y0 = yoff;
}

void Painter::Translate(float dx, float dy) {
    if (dx != 0.0 || dy != 0.0) {
	if (matrix == nil) {
	    matrix = new Transformer;
	}
	matrix->Translate(dx, dy);
    }
}

void Painter::Scale(float sx, float sy) {
    if (sx != 1.0 || sy != 1.0) {
	if (matrix == nil) {
	    matrix = new Transformer;
	}
	matrix->Scale(sx, sy);
    }
}

void Painter::Rotate(float angle) {
    if (angle - int(angle) != 0.0 || int(angle) % 360 != 0) {
	if (matrix == nil) {
	    matrix = new Transformer;
	}
	matrix->Rotate(angle);
    }
}

void Painter::CurveTo(Canvas* c,
    IntCoord x0, IntCoord y0, IntCoord x1, IntCoord y1,
    IntCoord x2, IntCoord y2
) {
    Curve(c, curx, cury, x0, y0, x1, y1, x2, y2);
    curx = x2;
    cury = y2;
}

void Painter::Text(Canvas* c, const char* s) {
    int len = strlen(s);
    Text(c, s, len, curx, cury);
    curx += font->Width(s, len);
}

void Painter::Text(Canvas* c, const char* s, int len) {
    Text(c, s, len, curx, cury);
    curx += font->Width(s, len);
}

void Painter::Text(Canvas* c, const char* s, IntCoord x, IntCoord y) {
    Text(c, s, strlen(s), x, y);
}

/*
 * Spline drawing.
 */

const int INITBUFSIZE = 100;
const double SMOOTHNESS = 1.0;

static int llsize = 0;
static int llcount = 0;
static IntCoord* llx;
static IntCoord* lly;

static void GrowBufs(IntCoord*& b1, IntCoord*& b2, int& cur) {
    IntCoord* newb1;
    IntCoord* newb2;
    int newsize;

    if (cur == 0) {
        cur = INITBUFSIZE;
	b1 = new IntCoord[INITBUFSIZE];
	b2 = new IntCoord[INITBUFSIZE];
    } else {
	newsize = cur * 2;
	newb1 = new IntCoord[newsize];
	newb2 = new IntCoord[newsize];
	Memory::copy(b1, newb1, cur * sizeof(IntCoord));
	Memory::copy(b2, newb2, cur * sizeof(IntCoord));
	delete b1;
	delete b2;
	b1 = newb1;
	b2 = newb2;
	cur = newsize;
    }
}

inline void Midpoint(
    double x0, double y0, double x1, double y1, double& mx, double& my
) {
    mx = (x0 + x1) / 2.0;
    my = (y0 + y1) / 2.0;
}

inline void ThirdPoint(
    double x0, double y0, double x1, double y1, double& tx, double& ty
) {
    tx = (2*x0 + x1) / 3.0;
    ty = (2*y0 + y1) / 3.0;
}

inline boolean CanApproxWithLine(
    double x0, double y0, double x2, double y2, double x3, double y3
) {
    double triangleArea, sideSquared, dx, dy;
    
    triangleArea = x0*y2 - x2*y0 + x2*y3 - x3*y2 + x3*y0 - x0*y3;
    triangleArea *= triangleArea;	// actually 4 times the area
    dx = x3 - x0;
    dy = y3 - y0;
    sideSquared = dx*dx + dy*dy;
    return triangleArea <= SMOOTHNESS * sideSquared;
}

inline void AddLine(double x0, double y0, double x1, double y1) {
    if (llcount >= llsize) {
	GrowBufs(llx, lly, llsize);
    }

    if (llcount == 0) {
	llx[llcount] = Math::round(x0);
	lly[llcount] = Math::round(y0);
	++llcount;
    }
    llx[llcount] = Math::round(x1);
    lly[llcount] = Math::round(y1);
    ++llcount;
}

static void AddBezierCurve(
    double x0, double y0, double x1, double y1,
    double x2, double y2, double x3, double y3
) {
    double midx01, midx12, midx23, midlsegx, midrsegx, cx;
    double midy01, midy12, midy23, midlsegy, midrsegy, cy;
    
    Midpoint(x0, y0, x1, y1, midx01, midy01);
    Midpoint(x1, y1, x2, y2, midx12, midy12);
    Midpoint(x2, y2, x3, y3, midx23, midy23);
    Midpoint(midx01, midy01, midx12, midy12, midlsegx, midlsegy);
    Midpoint(midx12, midy12, midx23, midy23, midrsegx, midrsegy);
    Midpoint(midlsegx, midlsegy, midrsegx, midrsegy, cx, cy);    

    if (CanApproxWithLine(x0, y0, midlsegx, midlsegy, cx, cy)) {
        AddLine(x0, y0, cx, cy);
    } else if (
        (midx01 != x1) || (midy01 != y1) ||
	(midlsegx != x2) || (midlsegy != y2) ||
	(cx != x3) || (cy != y3)
    ) {    
        AddBezierCurve(
	    x0, y0, midx01, midy01, midlsegx, midlsegy, cx, cy
	);
    }
    if (CanApproxWithLine(cx, cy, midx23, midy23, x3, y3)) {
        AddLine(cx, cy, x3, y3);
    } else if (
        (cx != x0) || (cy != y0) ||
	(midrsegx != x1) || (midrsegy != y1) ||
	(midx23 != x2) || (midy23 != y2)
    ) {        
        AddBezierCurve(
	    cx, cy, midrsegx, midrsegy, midx23, midy23, x3, y3
	);
    }
}

static void CalcBSpline(
    IntCoord cminus1x, IntCoord cminus1y, IntCoord cx, IntCoord cy,
    IntCoord cplus1x, IntCoord cplus1y, IntCoord cplus2x, IntCoord cplus2y
) {
    double p0x, p1x, p2x, p3x, tempx,
	   p0y, p1y, p2y, p3y, tempy;
    
    ThirdPoint(
        double(cx), double(cy), double(cplus1x), double(cplus1y), p1x, p1y
    );
    ThirdPoint(
        double(cplus1x), double(cplus1y), double(cx), double(cy), p2x, p2y
    );
    ThirdPoint(
        double(cx), double(cy), double(cminus1x), double(cminus1y),
	tempx, tempy
    );
    Midpoint(tempx, tempy, p1x, p1y, p0x, p0y);
    ThirdPoint(
        double(cplus1x), double(cplus1y), double(cplus2x), double(cplus2y),
	tempx, tempy
    );
    Midpoint(tempx, tempy, p2x, p2y, p3x, p3y);
    
    AddBezierCurve(p0x, p0y, p1x, p1y, p2x, p2y, p3x, p3y);
}

inline void CreateOpenLineList(IntCoord *cpx, IntCoord *cpy, int cpcount) {
    int cpi;
    
    llcount = 0;
    CalcBSpline(
	cpx[0], cpy[0], cpx[0], cpy[0], cpx[0], cpy[0], cpx[1], cpy[1]
    );
    CalcBSpline(
	cpx[0], cpy[0], cpx[0], cpy[0], cpx[1], cpy[1], cpx[2], cpy[2]
    );

    for (cpi = 1; cpi < cpcount - 2; ++cpi) {
	CalcBSpline(
	    cpx[cpi - 1], cpy[cpi - 1], cpx[cpi], cpy[cpi],
	    cpx[cpi + 1], cpy[cpi + 1], cpx[cpi + 2], cpy[cpi + 2]
        );
    }
    CalcBSpline(
	cpx[cpi - 1], cpy[cpi - 1], cpx[cpi], cpy[cpi],
	cpx[cpi + 1], cpy[cpi + 1], cpx[cpi + 1], cpy[cpi + 1]
    );
    CalcBSpline(
	cpx[cpi], cpy[cpi], cpx[cpi + 1], cpy[cpi + 1],
	cpx[cpi + 1], cpy[cpi + 1], cpx[cpi + 1], cpy[cpi + 1]
    );
}

inline void CreateClosedLineList(IntCoord *cpx, IntCoord *cpy, int cpcount) {
    int cpi;
    
    llcount = 0;
    CalcBSpline(
	cpx[cpcount - 1], cpy[cpcount - 1], cpx[0], cpy[0], 
	cpx[1], cpy[1], cpx[2], cpy[2]
    );

    for (cpi = 1; cpi < cpcount - 2; ++cpi) {
	CalcBSpline(
	    cpx[cpi - 1], cpy[cpi - 1], cpx[cpi], cpy[cpi],
	    cpx[cpi + 1], cpy[cpi + 1], cpx[cpi + 2], cpy[cpi + 2]
        );
    }
    CalcBSpline(
	cpx[cpi - 1], cpy[cpi - 1], cpx[cpi], cpy[cpi],
	cpx[cpi + 1], cpy[cpi + 1], cpx[0], cpy[0]
    );
    CalcBSpline(
	cpx[cpi], cpy[cpi], cpx[cpi + 1], cpy[cpi + 1],
	cpx[0], cpy[0], cpx[1], cpy[1]
    );
}

static int bufsize = 0;
static IntCoord* bufx, * bufy;

static void CheckBufs(IntCoord*& b1, IntCoord*& b2, int& cur, int desired) {
    if (cur < desired) {
	if (cur == 0) {
	    cur = Math::max(INITBUFSIZE, desired);
	} else {
	    delete b1;
	    delete b2;
	    cur = Math::max(cur * 2, desired);
	}
	b1 = new IntCoord[cur];
	b2 = new IntCoord[cur];
    }
}

void Painter::Curve(
    Canvas* c, IntCoord x0, IntCoord y0, IntCoord x1, IntCoord y1,
    IntCoord x2, IntCoord y2, IntCoord x3, IntCoord y3
) {
    IntCoord tx0, ty0, tx1, ty1, tx2, ty2, tx3, ty3;
    
    llcount = 0;
    Map(c, x0, y0, tx0, ty0);
    Map(c, x1, y1, tx1, ty1);
    Map(c, x2, y2, tx2, ty2);
    Map(c, x3, y3, tx3, ty3);
    AddBezierCurve(tx0, ty0, tx1, ty1, tx2, ty2, tx3, ty3);
    MultiLineNoMap(c, llx, lly, llcount);
}

void Painter::BSpline(Canvas* c, IntCoord x[], IntCoord y[], int count) {
    CheckBufs(bufx, bufy, bufsize, count);
    MapList(c, x, y, count, bufx, bufy);
    if (count < 3) {
        MultiLineNoMap(c, bufx, bufy, count);
    } else {
	CreateOpenLineList(bufx, bufy, count);
	MultiLineNoMap(c, llx, lly, llcount);
    }
}

void Painter::ClosedBSpline(Canvas* c, IntCoord x[], IntCoord y[], int count) {
    CheckBufs(bufx, bufy, bufsize, count);
    MapList(c, x, y, count, bufx, bufy);
    if (count < 3) {
        MultiLineNoMap(c, bufx, bufy, count);
    } else {
        CreateClosedLineList(bufx, bufy, count);
        MultiLineNoMap(c, llx, lly, llcount);
    }
}

void Painter::FillBSpline(Canvas* c, IntCoord x[], IntCoord y[], int count) {
    CheckBufs(bufx, bufy, bufsize, count);
    MapList(c, x, y, count, bufx, bufy);
    if (count < 3) {
        FillPolygonNoMap(c, bufx, bufy, count);
    } else {
        CreateClosedLineList(bufx, bufy, count);
        FillPolygonNoMap(c, llx, lly, llcount);
    }
}

void Painter::Map(Canvas* c, IntCoord x, IntCoord y, short& sx, short& sy) {
    IntCoord cx, cy;

    Map(c, x, y, cx, cy);
    sx = short(cx);
    sy = short(cy);
}

const float axis = 0.42;
const float seen = 1.025;

void Painter::Ellipse(Canvas* c, IntCoord cx, IntCoord cy, int r1, int r2) {
    float px1, py1, px2, py2, x[8], y[8];

    px1 = float(r1)*axis; py1 = float(r2)*axis;
    px2 = float(r1)*seen; py2 = float(r2)*seen;
    x[0] = cx + px1;	y[0] = cy + py2;
    x[1] = cx - px1;	y[1] = y[0];
    x[2] = cx - px2;	y[2] = cy + py1;
    x[3] = x[2];	y[3] = cy - py1;
    x[4] = x[1];	y[4] = cy - py2;
    x[5] = x[0];	y[5] = y[4];
    x[6] = cx + px2;	y[6] = y[3];
    x[7] = x[6];	y[7] = y[2];

    CheckBufs(bufx, bufy, bufsize, 8);
    MapList(c, (float*) x, (float*) y, 8, bufx, bufy);
    CreateClosedLineList(bufx, bufy, 8);
    MultiLineNoMap(c, llx, lly, llcount);
}

void Painter::FillEllipse(
    Canvas* c, IntCoord cx, IntCoord cy, int r1, int r2
) {
    float px1, py1, px2, py2, x[8], y[8];

    px1 = float(r1)*axis; py1 = float(r2)*axis;
    px2 = float(r1)*seen; py2 = float(r2)*seen;
    x[0] = cx + px1;	y[0] = cy + py2;
    x[1] = cx - px1;	y[1] = y[0];
    x[2] = cx - px2;	y[2] = cy + py1;
    x[3] = x[2];	y[3] = cy - py1;
    x[4] = x[1];	y[4] = cy - py2;
    x[5] = x[0];	y[5] = y[4];
    x[6] = cx + px2;	y[6] = y[3];
    x[7] = x[6];	y[7] = y[2];

    CheckBufs(bufx, bufy, bufsize, 8);
    MapList(c, (float*) x, (float*) y, 8, bufx, bufy);
    CreateClosedLineList(bufx, bufy, 8);
    FillPolygonNoMap(c, llx, lly, llcount);
}
