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
 * GrowingVertices and subclasses - rubberbands defined by a set of vertices 
 * that can grow dynamically in number.
 */

#ifndef iv2_6_rubverts_h
#define iv2_6_rubverts_h

#include <IV-2_6/InterViews/rubband.h>

class GrowingVertices : public Rubberband {
public:
    virtual ~GrowingVertices();

    virtual void AddVertex(IntCoord, IntCoord);
    virtual void RemoveVertex();
    virtual void Draw();

    virtual void GetOriginal(IntCoord*& px, IntCoord*& py, int& n, int& pt);
    virtual void GetCurrent(IntCoord*& px, IntCoord*& py, int& n, int& pt);
protected:
    GrowingVertices(
        Painter*, Canvas*, IntCoord px[], IntCoord py[], int n, int pt = -1,
	int handleSize = -1
    );

    void Init(IntCoord px[], IntCoord py[], int, int, int);
    void CheckBufs();
    void Copy(IntCoord*, IntCoord*, int, IntCoord*&, IntCoord*&);

    virtual void DrawVertices(IntCoord*, IntCoord*, int);
    virtual void DrawHandles();
protected:
    IntCoord* x, *y;
    int count, bufsize, origbufsize;
    int curPt, origPt;
    int handleSize;
public:					// obsolete calls
    virtual void AppendVertex(IntCoord, IntCoord);
    virtual void GetOriginal(IntCoord*& px, IntCoord*& py, int& n);
    virtual void GetCurrent(IntCoord*& px, IntCoord*& py, int& n);
};

class GrowingMultiLine : public GrowingVertices {
public:
    GrowingMultiLine(
        Painter*, Canvas*, IntCoord px[], IntCoord py[], int n, int pt = -1,
	int handleSize = -1
    );
protected:
    virtual void DrawVertices(IntCoord*, IntCoord*, int);
};

class GrowingPolygon : public GrowingVertices {
public:
    GrowingPolygon(
        Painter*, Canvas*, IntCoord px[], IntCoord py[], int n, int pt = -1,
	int handleSize = -1
    );
protected:
    virtual void DrawVertices(IntCoord*, IntCoord*, int);
};

class GrowingBSpline : public GrowingVertices {
public:
    GrowingBSpline(
        Painter*, Canvas*, IntCoord px[], IntCoord py[], int n, int pt = -1,
        int handleSize = -1
    );
protected:
    virtual void DrawVertices(IntCoord*, IntCoord*, int);
};

class GrowingClosedBSpline : public GrowingVertices {
public:
    GrowingClosedBSpline(
        Painter*, Canvas*, IntCoord px[], IntCoord py[], int n, int pt = -1,
	int handleSize = -1
    );
protected:
    virtual void DrawVertices(IntCoord*, IntCoord*, int);
};

#endif
