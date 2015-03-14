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
 * Vertices implementation.
 */

#include <Unidraw/Graphic/util.h>
#include <Unidraw/Graphic/verts.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

Vertices::Vertices (Graphic* gr) : Graphic(gr) { _extent = nil; }

Vertices::Vertices (Coord* x, Coord* y, int count, Graphic* gr) : Graphic(gr){
    _extent = nil;
    _count = count;
    ArrayDup(x, y, count, _x, _y);
}

Vertices::~Vertices () {
    uncacheExtent();
    delete _x;
    delete _y;
}

int Vertices::GetOriginal (const Coord*& x, const Coord*& y) {
    x = _x;
    y = _y;
    return _count;
}

boolean Vertices::operator == (Vertices& ml) {
    if (_count == ml._count) {
        for (int i = 0; i < _count; ++i) {
            if (_x[i] != ml._x[i] || _y[i] != ml._y[i]) {
                return false;
            }
        }
        return true;
    }
    return false;
}

boolean Vertices::operator != (Vertices& ml) { return !(*this == ml); }
Graphic* Vertices::Copy () { return new Vertices(_x, _y, _count, this); }
boolean Vertices::extentCached () { return _caching && _extent != nil; }

void Vertices::cacheExtent (float l, float b, float cx, float cy, float tol) {
    if (_caching) {
	uncacheExtent();
	_extent = new Extent(l, b, cx, cy, tol);
    }
}

void Vertices::uncacheExtent() { 
    delete _extent; 
    _extent = nil;
}

void Vertices::getCachedExtent (
    float& l, float& b, float& cx, float& cy, float& tol
) {
    l = _extent->_left;
    b = _extent->_bottom;
    cx = _extent->_cx;
    cy = _extent->_cy;
    tol = _extent->_tol;
}

void Vertices::s_getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    float bx0, by0, bx1, by1, tcx, tcy, width, dummy1, dummy2;

    if (extentCached()) {
	getCachedExtent(bx0, by0, tcx, tcy, tol);
	bx1 = 2*tcx - bx0;
	by1 = 2*tcy - by0;

    } else {
	width = float(gs->GetBrush()->Width());
	tol = (width > 1) ? width/2 : 0;
	bx0 = bx1 = _x[0]; by0 = by1 = _y[0];

	for (int i = 1; i < _count; ++i) {
	    bx0 = min(bx0, float(_x[i]));
	    by0 = min(by0, float(_y[i]));
	    bx1 = max(bx1, float(_x[i]));
	    by1 = max(by1, float(_y[i]));
	}
	tcx = (bx0 + bx1) / 2;
	tcy = (by0 + by1) / 2;
	cacheExtent(bx0, by0, tcx, tcy, tol);
    }
    transformRect(bx0, by0, bx1, by1, l, b, dummy1, dummy2, gs);
    transform(tcx, tcy, cx, cy, gs);
}

void Vertices::f_getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    float bx0, by0, bx1, by1, tcx, tcy, dummy1, dummy2;
	
    if (extentCached()) {
	getCachedExtent(bx0, by0, tcx, tcy, tol);
	bx1 = 2*tcx - bx0;
	by1 = 2*tcy - by0;

    } else {
	bx0 = bx1 = _x[0]; by0 = by1 = _y[0];

	for (int i = 1; i < _count; ++i) {
	    bx0 = min(bx0, float(_x[i]));
	    by0 = min(by0, float(_y[i]));
	    bx1 = max(bx1, float(_x[i]));
	    by1 = max(by1, float(_y[i]));
	}
	tcx = (bx0 + bx1) / 2;
	tcy = (by0 + by1) / 2;
	tol = 0;
	cacheExtent(bx0, by0, tcx, tcy, tol);
    }
    transformRect(bx0, by0, bx1, by1, l, b, dummy1, dummy2, gs);
    transform(tcx, tcy, cx, cy, gs);
}
