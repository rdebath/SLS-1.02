#ifndef globals_h
#define globals_h

#include <InterViews/coord.h>
#include <IV-2_6/InterViews/minmax.h>
#include <OS/memory.h>
#include <OS/math.h>
#include <math.h>

/*
 * Alignment needs to be unsigned so that it can be stored
 * as a bit field.
 */

typedef unsigned Alignment;

/*
 * Use the concrete type (unsigned) instead of Alignment
 * to get around cfront 2.1 incorrect warning.
 */
static const unsigned TopLeft = 0;
static const unsigned TopCenter = 1;
static const unsigned TopRight = 2;
static const unsigned CenterLeft = 3;
static const unsigned Center = 4;
static const unsigned CenterRight = 5;
static const unsigned BottomLeft = 6;
static const unsigned BottomCenter = 7;
static const unsigned BottomRight = 8;
static const unsigned Left = 9;
static const unsigned Right = 10;
static const unsigned Top = 11;
static const unsigned Bottom = 12;
static const unsigned HorizCenter = 13;
static const unsigned VertCenter = 14;

class PointObj {
public:
    PointObj(Coord = 0, Coord = 0);
    PointObj(PointObj*);

    Coord Distance(PointObj&);
public:
    Coord _x, _y;
};

class LineObj {
public:
    LineObj(Coord = 0, Coord = 0, Coord = 0, Coord = 0);
    LineObj(LineObj*);

    boolean Contains(PointObj&);
    int Same(PointObj& p1, PointObj& p2);
    boolean Intersects(LineObj&);
public:
    PointObj _p1, _p2;
};

class BoxObj {
public:
    BoxObj(Coord = 0, Coord = 0, Coord = 0, Coord = 0);
    BoxObj(BoxObj*);

    boolean operator==(BoxObj&);
    boolean Contains(PointObj&);
    boolean Intersects(BoxObj&);
    boolean Intersects(LineObj&);
    BoxObj operator-(BoxObj&);
    BoxObj operator+(BoxObj&);
    boolean Within(BoxObj&);
public:
    Coord _left, _right;
    Coord _bottom, _top;
};

class MultiLineObj {
public:
    MultiLineObj(Coord* = nil, Coord* = nil, int = 0);

    void GetBox(BoxObj& b);
    boolean Contains(PointObj&);
    boolean Intersects(LineObj&);
    boolean Intersects(BoxObj&);
    boolean Within(BoxObj&);
    void SplineToMultiLine(Coord* cpx, Coord* cpy, int cpcount);
    void ClosedSplineToPolygon(Coord* cpx, Coord* cpy, int cpcount);
protected:
    void GrowBuf();
    boolean CanApproxWithLine(
	double x0, double y0, double x2, double y2, double x3, double y3
    );
    void AddLine(double x0, double y0, double x1, double y1);
    void AddBezierArc(
        double x0, double y0, double x1, double y1,
        double x2, double y2, double x3, double y3
    );
    void CalcSection(
	Coord cminus1x, Coord cminus1y, Coord cx, Coord cy,
	Coord cplus1x, Coord cplus1y, Coord cplus2x, Coord cplus2y
    );
public:
    Coord* _x, *_y;
    int _count;
};

class FillPolygonObj : public MultiLineObj {
public:
    FillPolygonObj(Coord* = nil, Coord* = nil, int = 0);
    virtual ~FillPolygonObj();

    boolean Contains(PointObj&);
    boolean Intersects(LineObj&);
    boolean Intersects(BoxObj&);
protected:
    void Normalize();
protected:
    Coord* _normx, *_normy;
    int _normCount;
};

class Extent {
public:
    Extent(Coord = 0, Coord = 0, Coord = 0, Coord = 0, Coord = 0);
    Extent(Extent&);

    boolean Undefined();
    boolean Within(Extent& e);
    void Merge(Extent&);
public:
    /* defines lower left and center of an object */
    Coord _left, _bottom, _cx, _cy, _tol;
};

/*
 * inlines
 */

inline boolean Extent::Undefined () { return _left == _cx && _bottom == _cy; }

inline void exch (int& a, int& b) {
    int temp = a;
    a = b;
    b = temp;
}

inline int square(int a) { return a *= a; }
inline Coord square(Coord a) { return a *= a; }

inline Coord degrees(Coord rad) { return rad * 180.0 / M_PI; }
inline Coord radians(Coord deg) { return deg * M_PI / 180.0; }

inline Coord Distance(Coord x0, Coord y0, Coord x1, Coord y1) {
    return sqrt(Coord(square(x0 - x1) + square(y0 - y1)));
}

inline void ArrayCopy (
    const Coord* x, const Coord* y, int n, Coord* newx, Coord* newy
) {
    Memory::copy(x, newx, n * sizeof(Coord));
    Memory::copy(y, newy, n * sizeof(Coord));
}

inline void ArrayDup (
    const Coord* x, const Coord* y, int n, Coord*& newx, Coord*& newy
) {
    newx = new Coord[n];
    newy = new Coord[n];
    Memory::copy(x, newx, n * sizeof(Coord));
    Memory::copy(y, newy, n * sizeof(Coord));
}

inline void Midpoint (
    double x0, double y0, double x1, double y1, double& mx, double& my
) {
    mx = (x0 + x1) / 2.0;
    my = (y0 + y1) / 2.0;
}

inline void ThirdPoint (
    double x0, double y0, double x1, double y1, double& tx, double& ty
) {
    tx = (2*x0 + x1) / 3.0;
    ty = (2*y0 + y1) / 3.0;
}

#endif
