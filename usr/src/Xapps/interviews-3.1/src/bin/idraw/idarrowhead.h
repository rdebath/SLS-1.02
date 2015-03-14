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
 * Definition of Arrowhead graphic.
 */

#ifndef idarrowhead_h
#define idarrowhead_h

#include <Unidraw/Graphic/polygons.h>

class Arrowhead : public SF_Polygon {
public:
    Arrowhead(Coord tipx, Coord tipy, Coord w, Coord h, Graphic* gr = nil);

    void CorrectedTip(Coord&, Coord&, PSBrush*, Transformer*);

    virtual Graphic* Copy();
    virtual Graphic& operator = (Graphic&);
protected:
    Arrowhead(Coord*, Coord*, Graphic*);

    virtual void draw(Canvas*, Graphic*);

    Coord CorrectedHeight(float line_thk);
    float UnscaledLength(float length, Transformer* t);
private:
    Coord* x(Coord, Coord);
    Coord* y(Coord, Coord);
};

#endif


