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
 * Implementation of GrowingVertices and derived classes.
 */

#include <IV-2_6/InterViews/rubverts.h>
#include <IV-2_6/InterViews/painter.h>
#include <OS/math.h>
#include <OS/memory.h>
#include <stdlib.h>

GrowingVertices::GrowingVertices(
    Painter* p, Canvas* c, IntCoord px[], IntCoord py[], int n, int pt,
    int handleSize
) : Rubberband(p, c, 0, 0) {
    pt = (pt < 0) ? n : pt;
    Init(px, py, n, pt, handleSize);
}

void GrowingVertices::Init (
    IntCoord px[], IntCoord py[], int n, int pt, int h
) {
    origbufsize = count = n;
    bufsize = Math::max(2*n, 50);
    origPt = curPt = pt;
    x = new IntCoord[bufsize];
    y = new IntCoord[bufsize];
    trackx = px[n-1];
    tracky = py[n-1];
    handleSize = h;
    Memory::copy(px, x, n*sizeof(IntCoord));
    Memory::copy(py, y, n*sizeof(IntCoord));
}

GrowingVertices::~GrowingVertices() {
    delete x;
    delete y;
}

void GrowingVertices::GetOriginal(
    IntCoord*& px, IntCoord*& py, int& n, int& pt
) {
    Copy(x, y, origbufsize, px, py);
    n = origbufsize;
    pt = origPt;
}

void GrowingVertices::GetCurrent(
    IntCoord*& px, IntCoord*& py, int& n, int& pt
) {
    Copy(x, y, count, px, py);
    n = count;
    pt = curPt;
}

void GrowingVertices::CheckBufs() {
    if (count + 1 > bufsize) {
        bufsize *= 2;

        IntCoord* nx = new IntCoord[bufsize];
        IntCoord* ny = new IntCoord[bufsize];

        Memory::copy(x, nx, count*sizeof(IntCoord));
        Memory::copy(y, ny, count*sizeof(IntCoord));
        delete x;
        delete y;
        x = nx;
        y = ny;
    }
}

void GrowingVertices::Copy(
    IntCoord* x, IntCoord* y, int n, IntCoord*& nx, IntCoord*& ny
) {
    nx = new IntCoord[n];
    ny = new IntCoord[n];
    Memory::copy(x, nx, n*sizeof(IntCoord));
    Memory::copy(y, ny, n*sizeof(IntCoord));
}

void GrowingVertices::Draw() {
    if (!drawn) {
        x[curPt] = trackx;
        y[curPt] = tracky;

        DrawVertices(x, y, Math::max(curPt+1, count));
	DrawHandles();

        drawn = true;
    }
}

void GrowingVertices::DrawVertices(IntCoord*, IntCoord*, int) { }

void GrowingVertices::DrawHandles () {
    if (handleSize > 0) {
	int d = handleSize/2;

	for (int i = 0; i < count; ++i) {
	    if (i != curPt) {
		output->FillRect(
		    canvas, x[i] - d, y[i] - d, x[i] + d, y[i] + d
		);
	    }
	}
    }
}

void GrowingVertices::AddVertex (IntCoord vx, IntCoord vy) {
    boolean wasDrawn = drawn;

    Erase();
    ++curPt;

    for (int i = count; i > curPt; --i) {
	x[i] = x[i-1];
	y[i] = y[i-1];
    }

    x[curPt] = vx;
    y[curPt] = vy;
    ++count;
    CheckBufs();

    if (wasDrawn) {
	Draw();
    }
}

void GrowingVertices::RemoveVertex () {
    boolean wasDrawn = drawn;

    Erase();

    count = Math::max(0, count-1);
    curPt = Math::max(0, curPt-1);

    for (int i = curPt; i < count; ++i) {
	x[i] = x[i+1];
	y[i] = y[i+1];
    }

    if (wasDrawn) {
	Draw();
    }
}

void GrowingVertices::AppendVertex (IntCoord vx, IntCoord vy) {
    curPt = count;
    AddVertex(vx, vy);
}

void GrowingVertices::GetOriginal(IntCoord*& px, IntCoord*& py, int& n) {
    Copy(x, y, origbufsize, px, py);
    n = origbufsize;
}

void GrowingVertices::GetCurrent(IntCoord*& px, IntCoord*& py, int& n) {
    Copy(x, y, count, px, py);
    n = count;
}

/*****************************************************************************/

GrowingMultiLine::GrowingMultiLine(
    Painter* p, Canvas* c, IntCoord px[], IntCoord py[], int n, int pt,
    int handleSize
) : GrowingVertices(p, c, px, py, n, pt, handleSize) { }

void GrowingMultiLine::DrawVertices(IntCoord* x, IntCoord* y, int n) {
    output->MultiLine(canvas, x, y, n);
}

/*****************************************************************************/

GrowingPolygon::GrowingPolygon(
    Painter* p, Canvas* c, IntCoord px[], IntCoord py[], int n, int pt,
    int handleSize
) : GrowingVertices(p, c, px, py, n, pt, handleSize) { }

void GrowingPolygon::DrawVertices(IntCoord* x, IntCoord* y, int n) {
    if (n == 2) {
        output->Line(canvas, x[0], y[0], x[1], y[1]);
    } else {
        output->Polygon(canvas, x, y, n);
    }
}

/*****************************************************************************/

GrowingBSpline::GrowingBSpline(
    Painter* p, Canvas* c, IntCoord px[], IntCoord py[], int n, int pt,
    int handleSize
) : GrowingVertices(p, c, px, py, n, pt, handleSize) { }

void GrowingBSpline::DrawVertices(IntCoord* x, IntCoord* y, int n) {
    output->BSpline(canvas, x, y, n);
}

/*****************************************************************************/

GrowingClosedBSpline::GrowingClosedBSpline(
    Painter* p, Canvas* c, IntCoord px[], IntCoord py[], int n, int pt,
    int handleSize
) : GrowingVertices(p, c, px, py, n, pt, handleSize) { }

void GrowingClosedBSpline::DrawVertices(IntCoord* x, IntCoord* y, int n) {
    output->ClosedBSpline(canvas, x, y, n);
}
