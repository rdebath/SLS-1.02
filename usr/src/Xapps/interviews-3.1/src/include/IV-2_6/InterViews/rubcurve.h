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
 * Rubberbanding for curves.
 */

#ifndef iv2_6_rubcurve_h
#define iv2_6_rubcurve_h

#include <IV-2_6/InterViews/rubband.h>

class RubberEllipse : public Rubberband {
public:
    RubberEllipse(
        Painter*, Canvas*, IntCoord cx, IntCoord cy, IntCoord rx, IntCoord ry,
	IntCoord offx = 0, IntCoord offy = 0
    );

    virtual void GetOriginal(
	IntCoord& cx, IntCoord& cy, IntCoord& rx, IntCoord& ry
    );
    virtual void GetCurrent(
	IntCoord& cx, IntCoord& cy, IntCoord& rx, IntCoord& ry
    );
    virtual void OriginalRadii(int& xr, int& yr);
    virtual void CurrentRadii(int& xr, int& yr);
    virtual void Draw();
protected:
    IntCoord centerx, radiusx;
    IntCoord centery, radiusy;
};

class SlidingEllipse : public RubberEllipse  {
public:
    SlidingEllipse(
        Painter*, Canvas*, IntCoord cx, IntCoord cy, IntCoord xr, IntCoord yr,
	IntCoord rfx, IntCoord rfy, IntCoord offx = 0, IntCoord offy = 0
    );
    virtual void GetCurrent(
	IntCoord& cx, IntCoord& cy, IntCoord& xr, IntCoord& yr
    );
    virtual void OriginalRadii(int&, int&);
    virtual void CurrentRadii(int&, int&);
protected:
    IntCoord refx;
    IntCoord refy;
};

class RubberCircle : public RubberEllipse {
public:
    RubberCircle(
        Painter*, Canvas*, IntCoord cx, IntCoord cy, IntCoord rx, IntCoord ry,
	IntCoord offx = 0, IntCoord offy = 0
    );

    virtual void OriginalRadii(int& xr, int& yr);
    virtual void CurrentRadii(int& xr, int& yr);
    virtual void Draw();
};

class RubberPointList : public Rubberband {
public:
    RubberPointList(
        Painter*, Canvas*, IntCoord px[], IntCoord py[], int n,
	IntCoord offx = 0, IntCoord offy = 0
    );
    ~RubberPointList();
protected:
    void Copy(IntCoord*, IntCoord*, int, IntCoord*&, IntCoord*&);
protected:
    IntCoord *x;
    IntCoord *y;
    int count;
};    

class RubberVertex : public RubberPointList {
public:
    RubberVertex(
        Painter*, Canvas*, IntCoord px[], IntCoord py[], int n, int pt,
	IntCoord offx = 0, IntCoord offy = 0
    );

    virtual void GetOriginal(IntCoord*& px, IntCoord*& py, int& n, int& pt);
    virtual void GetCurrent(IntCoord*& px, IntCoord*& py, int& n, int& pt);
protected:
    void DrawSplineSection (Painter*, Canvas*, IntCoord x[], IntCoord y[]);
protected:
    int rubberPt;
};

class RubberHandles : public RubberVertex {
public:
    RubberHandles(
        Painter*, Canvas*, IntCoord x[], IntCoord y[], int n, int pt, int size,
	IntCoord offx = 0, IntCoord offy = 0
    );

    virtual void Track(IntCoord x, IntCoord y);
    virtual void Draw();
protected:
    int d;	/* half of handle size */
};

class RubberSpline : public RubberVertex {
public:
    RubberSpline(
        Painter*, Canvas*, IntCoord px[], IntCoord py[], int n, int pt,
	IntCoord offx = 0, IntCoord offy = 0
    );

    virtual void Draw();
};

class RubberClosedSpline : public RubberVertex {
public:
    RubberClosedSpline(
        Painter*, Canvas*, IntCoord px[], IntCoord py[], int n, int pt,
	IntCoord offx = 0, IntCoord offy = 0
    );

    virtual void Draw();
};

class SlidingPointList : public RubberPointList {
public:
    SlidingPointList (
        Painter*, Canvas*, IntCoord px[], IntCoord py[], int n,
	IntCoord rfx, IntCoord rfy, IntCoord offx = 0, IntCoord offy = 0
    );

    virtual void GetOriginal(IntCoord*& px, IntCoord*& py, int& n);
    virtual void GetCurrent(IntCoord*& px, IntCoord*& py, int& n);
    virtual void Draw();
    virtual void Track(IntCoord x, IntCoord y);
protected:
    IntCoord refx;
    IntCoord refy;
};

class SlidingLineList : public SlidingPointList {
public:
    SlidingLineList(
        Painter*, Canvas*, IntCoord x[], IntCoord y[], int n,
	IntCoord rfx, IntCoord rfy, IntCoord offx = 0, IntCoord offy = 0
    );

    virtual void Draw();
};    

class ScalingLineList : public RubberPointList {
public:
    ScalingLineList (
        Painter*, Canvas*, IntCoord px[], IntCoord py[], int n,
        IntCoord cx, IntCoord cy, IntCoord offx = 0, IntCoord offy = 0
    );
    virtual ~ScalingLineList();

    virtual void GetOriginal(IntCoord*& px, IntCoord*& py, int& n);
    virtual void GetCurrent(IntCoord*& px, IntCoord*& py, int& n);
    virtual void Track(IntCoord, IntCoord);
    virtual void Draw();
    float CurrentScaling();
protected:
    virtual void Update();
protected:
    IntCoord* newx, *newy;
    IntCoord centerx, centery;
    IntCoord width, height;
};

class RotatingLineList : public RubberPointList {
public:
    RotatingLineList (
        Painter*, Canvas*, IntCoord px[], IntCoord py[], int n,
        IntCoord cx, IntCoord cy, IntCoord rfx, IntCoord rfy,
	IntCoord offx = 0, IntCoord offy = 0
    );
    virtual ~RotatingLineList();

    virtual void GetOriginal(IntCoord*& px, IntCoord*& py, int& n);
    virtual void GetCurrent(IntCoord*& px, IntCoord*& py, int& n);
    virtual void Track(IntCoord, IntCoord);
    virtual void Draw();
    float OriginalAngle();
    float CurrentAngle();
protected:
    virtual void Update();
protected:
    IntCoord* newx, *newy;
    IntCoord centerx, centery;
    IntCoord refx, refy;
};

#endif
