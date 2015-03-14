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
 * Interface to Graphic base class and FullGraphic, a subclass of Graphic
 * for which all graphics state is defined.
 */

#ifndef unidraw_graphic_graphic_h
#define unidraw_graphic_graphic_h

#include <Unidraw/Graphic/geomobjs.h>
#include <Unidraw/Graphic/pspaint.h>

#include <IV-2_6/_enter.h>

static const int UNDEF = -1;

class Canvas;
class Iterator;
class Painter;
class Transformer;

class Graphic {
public:
    virtual ~Graphic();

    virtual void Draw(Canvas*);
    virtual void Draw(Canvas*, Coord, Coord, Coord, Coord);
    virtual void DrawClipped(Canvas*, Coord, Coord, Coord, Coord);
    virtual void Erase(Canvas*);
    virtual void Erase(Canvas*, Coord, Coord, Coord, Coord);
    virtual void EraseClipped(Canvas*, Coord, Coord, Coord, Coord);

    virtual void FillBg(int);
    virtual int BgFilled();
    virtual void SetColors(PSColor* f, PSColor* b);
    virtual PSColor* GetFgColor();
    virtual PSColor* GetBgColor();
    virtual void SetPattern(PSPattern*);
    virtual PSPattern* GetPattern();
    virtual void SetBrush(PSBrush*);
    virtual PSBrush* GetBrush();
    virtual void SetFont(PSFont*);
    virtual PSFont* GetFont();

    void Translate(float dx, float dy);
    void Scale(float sx, float sy, float ctrx = 0.0, float ctry = 0.0);
    void Rotate(float angle, float ctrx = 0.0, float ctry = 0.0);
    void Align(Alignment, Graphic*, Alignment);
    void SetTransformer(Transformer*);
    Transformer* GetTransformer();
    void TotalTransformation(Transformer&);

    void GetBounds(float&, float&, float&, float&);
    void GetBox(Coord&, Coord&, Coord&, Coord&);
    void GetBox(BoxObj&);
    virtual void GetCenter(float&, float&);    
    virtual boolean Contains(PointObj&);
    virtual boolean Intersects(BoxObj&);

    void SetTag(void*);
    void* GetTag();

    Graphic* Parent();
    virtual void First(Iterator&);
    virtual void Last(Iterator&);
    virtual void Next(Iterator&);
    virtual void Prev(Iterator&);
    virtual boolean Done(Iterator);

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

    virtual Graphic& operator = (Graphic&);
    virtual Graphic* Copy();
protected:
    Graphic(Graphic* gr = nil);
/*
 * Member functions that declare a "Graphic* gs" parameter
 * take into account that graphic's graphic state information when
 * performing their function.  This is useful in hierarchical graphic objects
 * (such as Pictures) where higher-level graphics' graphics state influences
 * lower (and ultimately leaf) graphics'.
 */
    void update(Graphic* gs);		    /* updates painter w/gs' state */
    virtual void draw(Canvas*, Graphic* gs);
    virtual void drawClipped(Canvas*, Coord, Coord, Coord, Coord, Graphic* gs);
    virtual void erase(Canvas*, Graphic* gs);
    virtual void eraseClipped(Canvas*, Coord, Coord, Coord, Coord,Graphic* gs);
/*
 * Bounding box operations.
 */
    virtual void getExtent(float&, float&, float&, float&, float&,Graphic* gs);
        /* Returns lower-left and center coordinates, and a tolerance (tol)
	 * (in canvas coordinates) by which the final extent will be grown
	 * in each direction (i.e. l-=tol, b-=tol, r+=tol, t+=tol).
	 */
    void GetExtent(Extent& e);
    void getBounds(float&, float&, float&, float&, Graphic* gs);
    void getBox(Coord&, Coord&, Coord&, Coord&, Graphic* gs);
    void getBox(BoxObj&, Graphic* gs);
    virtual boolean contains(PointObj&, Graphic* gs);
    virtual boolean intersects(BoxObj&, Graphic* gs);
/*
 * Parent-related operations.
 */
    Graphic* getRoot();			    /* top level parent */
    void totalGS(Graphic& p);
    void parentXform(Transformer& t);	    /* parents' transformation */
    void setParent(Graphic*, Graphic* parent);
    void unsetParent(Graphic*);
/*
 * Bounding box caching operations.
 */
    void cachingOn();
    void cachingOff();
    virtual boolean extentCached();
    virtual void uncacheExtent();
    virtual void uncacheParents();
    virtual void uncacheChildren();
    virtual void invalidateCaches();
/*
 * Graphics state concatentation operations.
 */
    virtual void concatGS(Graphic* a, Graphic* b, Graphic* dest);
    virtual void concatTransformer(
        Transformer* a, Transformer* b, Transformer* dest
    );
    virtual void concat(Graphic* a, Graphic* b, Graphic* dest);
/*
 * Convenient transformations that check first if there's a transformer and
 * then perform the (inverse) transformation.  The functions use the
 * transformer of the supplied Graphic if there is one; otherwise this'
 * transformer is used.
 */
    void transform(Coord& x, Coord& y, Graphic* = nil);
    void transform(Coord x, Coord y, Coord& tx, Coord& ty, Graphic* = nil);
    void transform(float x, float y, float& tx, float& ty, Graphic* = nil);
    void transformList(
        Coord x[], Coord y[], int n, Coord tx[], Coord ty[], Graphic* = nil
    );
    void transformRect(
	float, float, float, float, 
        float&, float&, float&, float&, Graphic* = nil
    );
    void invTransform(Coord& tx, Coord& ty, Graphic* = nil);
    void invTransform(Coord tx, Coord ty, Coord& x, Coord& y, Graphic* = nil);
    void invTransform(float tx, float ty, float& x, float& y, Graphic* = nil);
    void invTransformList(
        Coord tx[], Coord ty[], int n, Coord x[], Coord y[], Graphic* = nil
    );
    void invTransformRect(
	float, float, float, float, 
        float&, float&, float&, float&, Graphic* = nil
    );
/*
 * "Helper" functions that allow graphic subclasses to call
 * the protected member functions redefined by other graphic subclasses.
 */
    void drawGraphic(Graphic*, Canvas*, Graphic*);
    void drawClippedGraphic(Graphic*,Canvas*,Coord,Coord,Coord,Coord,Graphic*);
    void eraseGraphic(Graphic*, Canvas*, Graphic*);
    void eraseClippedGraphic(
        Graphic*, Canvas*, Coord, Coord, Coord, Coord, Graphic*
    );

    void getExtentGraphic(
        Graphic*, float&, float&, float&, float&, float&, Graphic* gs
    );
    boolean containsGraphic(Graphic*, PointObj&, Graphic* gs);
    boolean intersectsGraphic(Graphic*, BoxObj&, Graphic* gs);

    boolean extentCachedGraphic(Graphic*);
    void uncacheExtentGraphic(Graphic*);
    void uncacheParentsGraphic(Graphic*);
    void uncacheChildrenGraphic(Graphic*);
    void invalidateCachesGraphic(Graphic*);

    void concatGSGraphic(Graphic*, Graphic*, Graphic*, Graphic*);
    void concatTransformerGraphic(
        Graphic*,Transformer*, Transformer*, Transformer*
    );
    void concatGraphic(Graphic*, Graphic*, Graphic*, Graphic*);
protected:
    static Transformer* _identity;  /* identity matrix */
    static boolean _caching;	    /* state of bounding box caching */
    static Painter* _p;
    static BoxObj* _clipping;       /* current painter clipping, if any */

    Graphic* _parent;
    int _fillBg;
    PSColor* _fg;
    PSColor* _bg;
    void* _tag;
    Transformer* _t;
};

class FullGraphic : public Graphic {
public:
    FullGraphic(Graphic* = nil);
    virtual ~FullGraphic();

    virtual void SetPattern(PSPattern*);
    virtual PSPattern* GetPattern();
    virtual void SetBrush(PSBrush*);
    virtual PSBrush* GetBrush();
    virtual void SetFont(PSFont*);
    virtual PSFont* GetFont();

    virtual Graphic* Copy();
private:
    PSPattern* _pat;
    PSBrush* _br;
    PSFont* _font;
};

/*
 * inlines
 */

inline Transformer* Graphic::GetTransformer () { return _t; }
inline Graphic* Graphic::Parent () { return _parent; }
inline void Graphic::SetTag (void* tag) { _tag = tag; }
inline void* Graphic::GetTag () { return _tag; }

inline void Graphic::GetBox (BoxObj& b) { 
    GetBox(b._left, b._bottom, b._right, b._top);
}

inline void Graphic::getBox (BoxObj& b, Graphic* p) {
    getBox(b._left, b._bottom, b._right, b._top, p);
}

inline void Graphic::getBounds (
    float& l, float& b, float& r, float& t, Graphic* gs
) {
    float tol;

    getExtent(l, b, r, t, tol, gs);
    r += r - l;
    t += t - b;
    l -= tol;
    b -= tol;
    r += tol;
    t += tol;
}

inline void Graphic::drawGraphic (Graphic* g, Canvas* c, Graphic* gs) {
     g->draw(c, gs);
}
inline void Graphic::eraseGraphic (Graphic* g, Canvas* c, Graphic* gs) {
    g->erase(c, gs);
}

inline void Graphic::drawClippedGraphic (
    Graphic* g, Canvas* c, Coord l, Coord b, Coord r, Coord t, Graphic* gs
) { g->drawClipped(c, l, b, r, t, gs); }

inline void Graphic::eraseClippedGraphic (
    Graphic* g, Canvas* c, Coord l, Coord b, Coord r, Coord t, Graphic* gs
) { g->eraseClipped(c, l, b, r, t, gs); }

inline void Graphic::getExtentGraphic (
    Graphic* g, float& l, float& b, float& r, float& t, float& tol, Graphic* gs
) { g->getExtent(l, b, r, t, tol, gs); }

inline boolean Graphic::containsGraphic (Graphic* g, PointObj& p, Graphic* gs){
    return g->contains(p, gs);
}

inline boolean Graphic::intersectsGraphic (Graphic* g, BoxObj& b, Graphic* gs){
    return g->intersects(b, gs);
}

inline boolean Graphic::extentCachedGraphic (Graphic* g) {
    return g->extentCached();
}

inline void Graphic::uncacheExtentGraphic (Graphic* g) { g->uncacheExtent(); }
inline void Graphic::uncacheParentsGraphic (Graphic* g) { g->uncacheParents();}

inline void Graphic::uncacheChildrenGraphic (Graphic* g) {
    g->uncacheChildren();
}

inline void Graphic::invalidateCachesGraphic (Graphic* g) {
    g->invalidateCaches();
}

inline void Graphic::concatGSGraphic (
    Graphic* g, Graphic* a, Graphic* b, Graphic* d
) {
    g->concatGS(a, b, d);
}

inline void Graphic::concatTransformerGraphic (
    Graphic* g, Transformer* a, Transformer* b, Transformer* dest
) {
    g->concatTransformer(a, b, dest);
}

inline void Graphic::concatGraphic (
    Graphic* g, Graphic* a, Graphic* b, Graphic* d
) {
    g->concat(a, b, d);
}

#include <IV-2_6/_leave.h>

#endif
