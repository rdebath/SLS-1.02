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
 * Vertices is a base class for graphics that are characterized by a set
 * of vertices.
 */

#ifndef unidraw_graphic_verts_h
#define unidraw_graphic_verts_h

#include <Unidraw/Graphic/graphic.h>

#include <IV-2_6/_enter.h>

class Vertices : public Graphic {
public:
    virtual ~Vertices();

    virtual int GetOriginal(const Coord*&, const Coord*&);

    virtual boolean operator == (Vertices&);
    virtual boolean operator != (Vertices&);

    virtual Graphic* Copy();
protected:
    Vertices(Graphic* gr = nil);
    Vertices(Coord* x, Coord* y, int count, Graphic* gr = nil);

    virtual boolean extentCached();
    void cacheExtent(float, float, float, float, float);
    virtual void uncacheExtent();
    void getCachedExtent(float&, float&, float&, float&, float&);

    void s_getExtent(float&, float&, float&, float&, float&, Graphic*);
    void f_getExtent(float&, float&, float&, float&, float&, Graphic*);
protected:
    Coord* _x, *_y;
    int _count;
    Extent* _extent;
};

#include <IV-2_6/_leave.h>

#endif
