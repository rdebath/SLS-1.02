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
 * Implementation of geometrical objects used by Graphic.
 */

#include <Unidraw/Graphic/geomobjs.h>
#include <Unidraw/Graphic/util.h>

#include <OS/memory.h>

#include <IV-2_6/_enter.h>

#include <math.h>

/*****************************************************************************/

static const int NUMPOINTS = 200;	// must be > 1
static const double SMOOTHNESS = 1.0;
static int mlsize = 0;
static int mlcount = 0;
static Coord* mlx, *mly;

/*****************************************************************************/

PointObj::PointObj (Coord x, Coord y) { _x = x; _y = y; }
PointObj::PointObj (PointObj* p) { _x = p->_x; _y = p->_y; }

float PointObj::Distance (PointObj& p) {
    return sqrt(float(square(_x - p._x) + square(_y - p._y)));
}

/*****************************************************************************/

LineObj::LineObj (Coord x0, Coord y0, Coord x1, Coord y1) {
    _p1._x = x0; _p1._y = y0; _p2._x = x1; _p2._y = y1;
}

LineObj::LineObj (LineObj* l) {
    _p1._x = l->_p1._x; _p1._y = l->_p1._y; 
    _p2._x = l->_p2._x; _p2._y = l->_p2._y;
}
boolean LineObj::Contains (PointObj& p) {
    return
	(p._x >= min(_p1._x, _p2._x)) && (p._x <= max(_p1._x, _p2._x)) &&
	(p._y >= min(_p1._y, _p2._y)) && (p._y <= max(_p1._y, _p2._y)) && (
            (p._y - _p1._y)*(_p2._x - _p1._x) - 
            (_p2._y - _p1._y)*(p._x - _p1._x)
        ) == 0;
}

inline int signum (int a) {
    if (a < 0) {
        return -1;
    } else if (a > 0) {
        return 1;
    } else {
        return 0;
    }
}

int LineObj::Same (PointObj& p1, PointObj& p2) {
    Coord dx, dx1, dx2;
    Coord dy, dy1, dy2;
    
    dx = _p2._x - _p1._x;
    dy = _p2._y - _p1._y;
    dx1 = p1._x - _p1._x;
    dy1 = p1._y - _p1._y;
    dx2 = p2._x - _p2._x;
    dy2 = p2._y - _p2._y;

    return signum(dx*dy1 - dy*dx1) * signum(dx*dy2 - dy*dx2);
}

boolean LineObj::Intersects (LineObj& l) {  // from Sedgewick, p. 313
    BoxObj b1 (_p1._x, _p1._y, _p2._x, _p2._y);
    BoxObj b2 (l._p1._x, l._p1._y, l._p2._x, l._p2._y);
    
    return
        b1.Intersects(b2) && Same(l._p1, l._p2) <= 0 && l.Same(_p1, _p2) <= 0;
}

/*****************************************************************************/

BoxObj::BoxObj (Coord x0, Coord y0, Coord x1, Coord y1) {
    _left = min(x0, x1); _bottom = min(y0, y1); 
    _right = max(x0, x1); _top = max(y0, y1);
}

BoxObj::BoxObj (BoxObj* b) {
    _left = b->_left; _bottom = b->_bottom; _right = b->_right; _top = b->_top;
}

boolean BoxObj::Contains (PointObj& p) {
    return
        (p._x >= _left) && (p._x <= _right) &&
        (p._y >= _bottom) && (p._y <= _top);
}

boolean BoxObj::Intersects (BoxObj& b) {
    return (
        (_left <= b._right) && (b._left <= _right) && 
	(_bottom <= b._top) && (b._bottom <= _top) 
    );
}

boolean BoxObj::Intersects (LineObj& l) {
    Coord x1 = min(l._p1._x, l._p2._x);
    Coord x2 = max(l._p1._x, l._p2._x);
    Coord y1 = min(l._p1._y, l._p2._y);
    Coord y2 = max(l._p1._y, l._p2._y);
    BoxObj lbox(x1, y1, x2, y2);
    boolean intersects = false;

    if (Intersects(lbox)) {
	intersects = Contains(l._p1) || Contains(l._p2);
        if (!intersects) {
            LineObj l0 (_left, _bottom, _right, _bottom);
            LineObj l1 (_right, _bottom, _right, _top);
            LineObj l2 (_right, _top, _left, _top);
            LineObj l3 (_left, _top, _left, _bottom);
            intersects =
	        l.Intersects(l0) || l.Intersects(l1) || 
	        l.Intersects(l2) || l.Intersects(l3);
	}
    }
    return intersects;
}

BoxObj BoxObj::operator- (BoxObj& b) {
    BoxObj i;

    if (Intersects(b)) {
        i._left = max(_left, b._left);
	i._bottom = max(_bottom, b._bottom);
	i._right = min(_right, b._right);
	i._top = min(_top, b._top);
    }
    return i;
}

BoxObj BoxObj::operator+ (BoxObj& b) {
    BoxObj m;
    
    m._left = min(_left, b._left);
    m._bottom = min(_bottom, b._bottom);
    m._right = max(_right, b._right);
    m._top = max(_top, b._top);
    return m;
}

boolean BoxObj::Within (BoxObj& b) {
    return (
        (_left >= b._left) && (_bottom >= b._bottom) && 
        (_right <= b._right) && (_top <= b._top) 
    );
}

/*****************************************************************************/

MultiLineObj::MultiLineObj (Coord* x, Coord* y, int count) {
    _x = x; _y = y; _count = count;
}

void MultiLineObj::GrowBuf () {
    Coord* newx, *newy;
    int newsize;

    if (mlsize == 0) {
        mlsize = NUMPOINTS;
	mlx = new Coord[NUMPOINTS];
	mly = new Coord[NUMPOINTS];
    } else {
	newsize = mlsize * 2;
	newx = new Coord[newsize];
	newy = new Coord[newsize];
	Memory::copy(mlx, newx, newsize * sizeof(Coord));
	Memory::copy(mly, newy, newsize * sizeof(Coord));
	delete mlx;
	delete mly;
	mlx = newx;
	mly = newy;
	mlsize = newsize;
    }
}

boolean MultiLineObj::CanApproxWithLine (
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

void MultiLineObj::AddLine (double x0, double y0, double x1, double y1) {
    if (mlcount >= mlsize) {
	GrowBuf();
    } 
    if (mlcount == 0) {
	mlx[mlcount] = round(x0);
	mly[mlcount] = round(y0);
	++mlcount;
    }
    mlx[mlcount] = round(x1);
    mly[mlcount] = round(y1);
    ++mlcount;
}

void MultiLineObj::AddBezierArc (
     double x0, double y0, double x1, double y1,
     double x2, double y2, double x3, double y3
) {
    double midx01, midx12, midx23, midlsegx, midrsegx, cx,
    	   midy01, midy12, midy23, midlsegy, midrsegy, cy;
    
    Midpoint(x0, y0, x1, y1, midx01, midy01);
    Midpoint(x1, y1, x2, y2, midx12, midy12);
    Midpoint(x2, y2, x3, y3, midx23, midy23);
    Midpoint(midx01, midy01, midx12, midy12, midlsegx, midlsegy);
    Midpoint(midx12, midy12, midx23, midy23, midrsegx, midrsegy);
    Midpoint(midlsegx, midlsegy, midrsegx, midrsegy, cx, cy);    

    if (CanApproxWithLine(x0, y0, midlsegx, midlsegy, cx, cy)) {
        AddLine(x0, y0, cx, cy);
    } else if (
        (midx01 != x1) || (midy01 != y1) || (midlsegx != x2) ||
	(midlsegy != y2) || (cx != x3) || (cy != y3)
    ) {    
        AddBezierArc(x0, y0, midx01, midy01, midlsegx, midlsegy, cx, cy);
    }

    if (CanApproxWithLine(cx, cy, midx23, midy23, x3, y3)) {
        AddLine(cx, cy, x3, y3);
    } else if (
        (cx != x0) || (cy != y0) || (midrsegx != x1) || (midrsegy != y1) ||
	(midx23 != x2) || (midy23 != y2)
    ) {        
        AddBezierArc(cx, cy, midrsegx, midrsegy, midx23, midy23, x3, y3);
    }
}

void MultiLineObj::CalcSection (
    Coord cminus1x, Coord cminus1y, Coord cx, Coord cy,
    Coord cplus1x, Coord cplus1y, Coord cplus2x, Coord cplus2y
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
    AddBezierArc(p0x, p0y, p1x, p1y, p2x, p2y, p3x, p3y);
}

void MultiLineObj::SplineToMultiLine (Coord* cpx, Coord* cpy, int cpcount) {
    register int cpi;

    if (cpcount < 3) {
        _x = cpx;
	_y = cpy;
	_count = cpcount;
    } else {
        mlcount = 0;

        CalcSection(
            cpx[0], cpy[0], cpx[0], cpy[0], cpx[0], cpy[0], cpx[1], cpy[1]
        );
        CalcSection(
            cpx[0], cpy[0], cpx[0], cpy[0], cpx[1], cpy[1], cpx[2], cpy[2]
        );

        for (cpi = 1; cpi < cpcount - 2; ++cpi) {
            CalcSection(
                cpx[cpi - 1], cpy[cpi - 1], cpx[cpi], cpy[cpi],
                cpx[cpi + 1], cpy[cpi + 1], cpx[cpi + 2], cpy[cpi + 2]
            );
        }

        CalcSection(
            cpx[cpi - 1], cpy[cpi - 1], cpx[cpi], cpy[cpi],
            cpx[cpi + 1], cpy[cpi + 1], cpx[cpi + 1], cpy[cpi + 1]
        );
        CalcSection(
            cpx[cpi], cpy[cpi], cpx[cpi + 1], cpy[cpi + 1],
            cpx[cpi + 1], cpy[cpi + 1], cpx[cpi + 1], cpy[cpi + 1]
        );
        _x = mlx;
        _y = mly;
        _count = mlcount;
    }
}

void MultiLineObj::ClosedSplineToPolygon (Coord* cpx, Coord* cpy, int cpcount){
    register int cpi;

    if (cpcount < 3) {
        _x = cpx;
	_y = cpy;
	_count = cpcount;
    } else {
        mlcount = 0;
        CalcSection(
	    cpx[cpcount - 1], cpy[cpcount - 1], cpx[0], cpy[0], 
	    cpx[1], cpy[1], cpx[2], cpy[2]
        );

        for (cpi = 1; cpi < cpcount - 2; ++cpi) {
	    CalcSection(
	        cpx[cpi - 1], cpy[cpi - 1], cpx[cpi], cpy[cpi],
	        cpx[cpi + 1], cpy[cpi + 1], cpx[cpi + 2], cpy[cpi + 2]
            );
        }

        CalcSection(
	    cpx[cpi - 1], cpy[cpi - 1], cpx[cpi], cpy[cpi],
	    cpx[cpi + 1], cpy[cpi + 1], cpx[0], cpy[0]
        );
        CalcSection(
	    cpx[cpi], cpy[cpi], cpx[cpi + 1], cpy[cpi + 1],
	    cpx[0], cpy[0], cpx[1], cpy[1]
        );
        _x = mlx;
        _y = mly;
        _count = mlcount;
    }
}

void MultiLineObj::GetBox (BoxObj& b) {
    b._left = b._right = _x[0];
    b._bottom = b._top = _y[0];

    for (int i = 1; i < _count; ++i) {
	b._left = min(b._left, _x[i]);
	b._bottom = min(b._bottom, _y[i]);
	b._right = max(b._right, _x[i]);
	b._top = max(b._top, _y[i]);
    }
}


boolean MultiLineObj::Contains (PointObj& p) {
    register int i;
    BoxObj b;
    
    GetBox(b);
    if (b.Contains(p)) {
	for (i = 1; i < _count; ++i) {
	    LineObj l (_x[i-1], _y[i-1], _x[i], _y[i]);
	    if (l.Contains(p)) {
	        return true;
	    }
	}
    }    
    return false;
}

boolean MultiLineObj::Intersects (LineObj& l) {
    register int i;
    BoxObj b;
    
    GetBox(b);
    if (b.Intersects(l)) {
	for (i = 1; i < _count; ++i) {
            LineObj test(_x[i-1], _y[i-1], _x[i], _y[i]);

	    if (l.Intersects(test)) {
	        return true;
	    }
	}
    }    
    return false;
}

boolean MultiLineObj::Intersects (BoxObj& userb) {
    register int i;
    BoxObj b;
    
    GetBox(b);
    if (b.Intersects(userb)) {
	for (i = 1; i < _count; ++i) {
            LineObj test(_x[i-1], _y[i-1], _x[i], _y[i]);

	    if (userb.Intersects(test)) {
	        return true;
	    }
	}
    }    
    return false;
}

boolean MultiLineObj::Within (BoxObj& userb) {
    BoxObj b;
    
    GetBox(b);
    return b.Within(userb);
}

/*****************************************************************************/

FillPolygonObj::FillPolygonObj (
    Coord* x, Coord* y, int n
) : MultiLineObj(x, y, n) {
    _normCount = 0;
    _normx = _normy = nil;
}

FillPolygonObj::~FillPolygonObj () {
    delete _normx;
    delete _normy;
}

static int LowestLeft (Coord* x, Coord* y, int count) {
    register int i;
    int lowestLeft = 0;
    Coord lx = *x;
    Coord ly = *y;

    for (i = 1; i < count; ++i) {
        if (y[i] < ly || (y[i] == ly && x[i] < lx)) {
	    lowestLeft = i;
	    lx = x[i];
	    ly = y[i];
	}
    }
    return lowestLeft;
}

void FillPolygonObj::Normalize () {
    if (_count != 0) {
        register int i, newcount = 1;
        int lowestLeft, limit = _count;

	if (*_x == _x[_count - 1] && *_y == _y[_count - 1]) {
	    --limit;
	}
	lowestLeft = LowestLeft(_x, _y, limit);
	_normCount = limit + 2;
	_normx = new Coord[_normCount];
	_normy = new Coord[_normCount];

	for (i = lowestLeft; i < limit; ++i, ++newcount) {
	    _normx[newcount] = _x[i];
	    _normy[newcount] = _y[i];
	}
	for (i = 0; i < lowestLeft; ++i, ++newcount) {
	    _normx[newcount] = _x[i];
	    _normy[newcount] = _y[i];
	}

	_normx[newcount] = _normx[1];
	_normy[newcount] = _normy[1];
	--newcount;
	_normx[0] = _normx[newcount];
	_normy[0] = _normy[newcount];
    }
}

boolean FillPolygonObj::Contains (PointObj& p) { // derived from A. Glassner,
  if (_normCount == 0) {                         // "An Introduction to
      Normalize();                               // Ray Tracing", p. 53,
  }                                              // courtesy R. Cooperman

  int count = 0;
  PointObj p0(0, 0);
  boolean cur_y_sign = _normy[0] >= p._y;

  for (int i = 0; i < _normCount - 2; ++i) {
      LineObj l (
          _normx[i] - p._x, _normy[i] - p._y,
          _normx[i+1] - p._x, _normy[i+1] - p._y
      );

      if (l.Contains(p0)) {
          return true;
      }

      boolean next_y_sign = l._p2._y >= 0;

      if (next_y_sign != cur_y_sign) {
          boolean cur_x_sign = l._p1._x >= 0;
          boolean next_x_sign = l._p2._x >= 0;

          if (cur_x_sign && next_x_sign) {
              ++count;

          } else if (cur_x_sign || next_x_sign) {
              Coord dx = l._p2._x - l._p1._x;
              Coord dy = l._p2._y - l._p1._y;

              if (dy >= 0) {
                  if (l._p1._x * dy > l._p1._y * dx) {
                      ++count;
                  }
              } else {
                  if (l._p1._x * dy < l._p1._y * dx) {
                      ++count;
                  }
              }
          }
      }
      cur_y_sign = next_y_sign;
  }
  return count % 2 == 1;
}

boolean FillPolygonObj::Intersects (LineObj& l) {
    BoxObj b;
    boolean intersects = false;
    
    if (_normCount == 0) {
        Normalize();
    }

    GetBox(b);

    if (b.Intersects(l)) {
        MultiLineObj ml (_normx, _normy, _normCount - 1);
	intersects = ml.Intersects(l) || Contains(l._p1) || Contains(l._p2);
    }

    return intersects;
}

boolean FillPolygonObj::Intersects (BoxObj& ub) {
    BoxObj b;
    
    GetBox(b);
    if (!b.Intersects(ub)) {
	return false;
    }
    if (b.Within(ub)) {
	return true;
    }
    LineObj bottom(ub._left, ub._bottom, ub._right, ub._bottom);

    if (Intersects(bottom)) {
	return true;
    }

    LineObj right(ub._right, ub._bottom, ub._right, ub._top);

    if (Intersects(right)) {
	return true;
    }

    LineObj top(ub._right, ub._top, ub._left, ub._top);

    if (Intersects(top)) {
	return true;
    }

    LineObj left(ub._left, ub._top, ub._left, ub._bottom);

    return Intersects(left);
}

/*****************************************************************************/

Extent::Extent (float x0, float y0, float x1, float y1, float t) {
    _left = x0; _bottom = y0; _cx = x1; _cy = y1; _tol = t;
}

Extent::Extent (Extent& e) {
    _left = e._left; _bottom = e._bottom;
    _cx = e._cx; _cy = e._cy; _tol = e._tol;
}

boolean Extent::Within (Extent& e) {
    float l = _left - _tol, b = _bottom - _tol;
    float el = e._left - _tol, eb = e._bottom - _tol;

    return 
	l >= el && b >= eb && 2*_cx - l <= 2*e._cx - el && 
	2*_cy - b <= 2*e._cy - eb;
    }

void Extent::Merge (Extent& e) {
    float nl = min(_left, e._left);
    float nb = min(_bottom, e._bottom);

    if (Undefined()) {
	_left = e._left; _bottom = e._bottom; _cx = e._cx; _cy = e._cy;
    } else if (!e.Undefined()) {
	_cx = (nl + max(2*_cx - _left, 2*e._cx - e._left)) / 2;
	_cy = (nb + max(2*_cy - _bottom, 2*e._cy - e._bottom)) / 2;
	_left = nl;
	_bottom = nb;
    }
    _tol = max(_tol, e._tol);
}
