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
 * Interface to Picture, a composite of one or more Graphics.
 */

#ifndef unidraw_graphic_picture_h
#define unidraw_graphic_picture_h

#include <Unidraw/Graphic/graphic.h>

#include <IV-2_6/_enter.h>

class Picture : public FullGraphic {
public:
    Picture(Graphic* gr = nil);
    virtual ~Picture();

    virtual void First(Iterator&);
    virtual void Last(Iterator&);
    virtual void Next(Iterator&);
    virtual void Prev(Iterator&);
    virtual boolean Done(Iterator);
    boolean IsEmpty();

    virtual Graphic* GetGraphic(Iterator);
    virtual void SetGraphic(Graphic*, Iterator&);

    virtual void Append(Graphic*, Graphic* =nil, Graphic* =nil, Graphic* =nil);
    virtual void Prepend(Graphic*,Graphic* =nil, Graphic* =nil, Graphic* =nil);
    virtual void InsertBefore(Iterator, Graphic*);
    virtual void InsertAfter(Iterator, Graphic*);
    virtual void Remove(Graphic*);
    virtual void Remove(Iterator&);
    virtual void Bequeath();

    virtual Graphic* FirstGraphicContaining(PointObj&);
    virtual Graphic* LastGraphicContaining(PointObj&);

    virtual Graphic* FirstGraphicIntersecting(BoxObj&);
    virtual Graphic* LastGraphicIntersecting(BoxObj&);

    virtual Graphic* FirstGraphicWithin(BoxObj&);
    virtual Graphic* LastGraphicWithin(BoxObj&);

    virtual Graphic* Copy();
protected:
    Graphic* graphic(class UList*);
    UList* Elem(Iterator);

    virtual void draw(Canvas*, Graphic*);
    virtual void drawClipped(Canvas*, Coord, Coord, Coord, Coord, Graphic*);
    
    virtual void getExtent(float&, float&, float&, float&, float&, Graphic*);
    virtual boolean contains(PointObj&, Graphic*);
    virtual boolean intersects(BoxObj&, Graphic*);

    void getCachedExtent(float&, float&, float&, float&, float&);
    virtual boolean extentCached();
    virtual void cacheExtent(float, float, float, float, float);
    virtual void uncacheExtent();
    virtual void uncacheChildren();
protected:
    UList* _kids;
private:
    Extent* _extent;
};

#include <IV-2_6/_leave.h>

#endif
