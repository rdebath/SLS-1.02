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
 * Rubberbanding for simple lines.
 */

#ifndef iv2_6_rubline_h
#define iv2_6_rubline_h

#include <IV-2_6/InterViews/rubband.h>

class RubberLine : public Rubberband {
public:
    RubberLine(
        Painter*, Canvas*, IntCoord x0, IntCoord y0, IntCoord x1, IntCoord y1,
	IntCoord offx = 0, IntCoord offy = 0
    );

    virtual void GetOriginal(
	IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
    );
    virtual void GetCurrent(
	IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
    );
    virtual void Draw();
protected:
    IntCoord fixedx, fixedy;
    IntCoord movingx, movingy;
};

class RubberAxis : public RubberLine {
public:
    RubberAxis(
        Painter*, Canvas*, IntCoord x0, IntCoord y0, IntCoord x1, IntCoord y1,
	IntCoord offx = 0, IntCoord offy = 0
    );

    virtual void GetCurrent(
	IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
    );
};

class SlidingLine : public RubberLine {
public:
    SlidingLine(
	Painter*, Canvas*, IntCoord x0, IntCoord y0, IntCoord x1, IntCoord y1,
	IntCoord rfx, IntCoord rfy, IntCoord offx = 0, IntCoord offy = 0
    );

    virtual void GetCurrent(
	IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
    );
protected:
    IntCoord refx;
    IntCoord refy;
};

class ScalingLine : public RubberLine {
public:
    ScalingLine(
        Painter*, Canvas*, IntCoord x0, IntCoord y0, IntCoord x1, IntCoord y1,
	IntCoord cx, IntCoord cy, IntCoord offx = 0, IntCoord offy = 0
    );
    virtual void GetCurrent(
	IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
    );
    float CurrentScaling();
protected:
    IntCoord centerx, centery;
    int width, height;
};

class RotatingLine : public RubberLine {
public:
    RotatingLine(
	Painter*, Canvas*, IntCoord x0, IntCoord y0, IntCoord x1, IntCoord y1, 
	IntCoord cx, IntCoord cy, IntCoord rfx, IntCoord rfy, 
	IntCoord offx = 0, IntCoord offy = 0
    );

    virtual void GetCurrent(
	IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
    );
    float OriginalAngle();
    float CurrentAngle();
protected:
    void Transform (
	IntCoord& x, IntCoord& y,
	double a0, double a1, double b0, double b1, double c0, double c1
    );
protected:
    IntCoord centerx, centery, refx, refy;
};

#endif
