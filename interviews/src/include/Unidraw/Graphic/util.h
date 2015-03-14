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
 * Various useful types and functions.
 */

#ifndef unidraw_graphic_util_h
#define unidraw_util_h

#include <IV-2_6/InterViews/defs.h>
#include <Unidraw/enter-scope.h>
#include <OS/memory.h>
#include <math.h>

#include <IV-2_6/_enter.h>

inline void exch (int& a, int& b) {
    int temp = a;
    a = b;
    b = temp;
}

inline int square(int a) { return a *= a; }
inline float square(float a) { return a *= a; }

inline float degrees(float rad) { return rad * 180.0 / M_PI; }
inline float radians(float deg) { return deg * M_PI / 180.0; }

inline float Distance(Coord x0, Coord y0, Coord x1, Coord y1) {
    return sqrt(float(square(x0 - x1) + square(y0 - y1)));
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

#include <IV-2_6/_leave.h>

#endif
