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
 * Interface to Ellipses and Circles, objects derived from Graphic.
 */

#ifndef unidraw_graphic_ellipses_h
#define unidraw_graphic_ellipses_h

#include <Unidraw/Graphic/graphic.h>

#include <IV-2_6/_enter.h>

class Ellipse : public Graphic {
public:
    void GetOriginal(Coord&, Coord&, int&, int&);
protected:
    Ellipse(Coord x0, Coord y0, int r1, int r2, Graphic* gr = nil);

    void s_getExtent(float&, float&, float&, float&, float&, Graphic*);
    void f_getExtent(float&, float&, float&, float&, float&, Graphic*);

    boolean s_contains(PointObj&, Graphic*);
    boolean f_contains(PointObj&, Graphic*);
    boolean s_intersects(BoxObj&, Graphic*);
    boolean f_intersects(BoxObj&, Graphic*);
protected:
    Coord _x0, _y0;
    int _r1, _r2;
    static Coord _x[8], _y[8];
private:
    void CalcControlPts(Transformer*);
};

class S_Ellipse : public Ellipse {
public:
    S_Ellipse(Coord x0, Coord y0, int r1, int r2, Graphic* gr = nil);
    virtual ~S_Ellipse();
    
    virtual void SetBrush(PSBrush*);
    virtual PSBrush* GetBrush();

    virtual Graphic* Copy();
protected:
    virtual void getExtent(float&, float&, float&, float&, float&, Graphic*);
    virtual boolean contains(PointObj&, Graphic*);
    virtual boolean intersects(BoxObj&, Graphic*);
    virtual void draw(Canvas*, Graphic*);
private:
    PSBrush* _br;
};

class F_Ellipse : public Ellipse {
public:
    F_Ellipse(Coord x0, Coord y0, int r1, int r2, Graphic* gr = nil);
    virtual ~F_Ellipse();

    virtual void SetPattern(PSPattern*);
    virtual PSPattern* GetPattern();

    virtual Graphic* Copy();
protected:
    virtual void getExtent(float&, float&, float&, float&, float&, Graphic*);
    virtual boolean contains(PointObj&, Graphic*);
    virtual boolean intersects(BoxObj&, Graphic*);
    virtual void draw(Canvas*, Graphic*);
private:
    PSPattern* _pat;
};

class SF_Ellipse : public Ellipse {
public:
    SF_Ellipse(Coord x0, Coord y0, int r1, int r2, Graphic* gr = nil);
    virtual ~SF_Ellipse();

    virtual void SetBrush(PSBrush*);
    virtual PSBrush* GetBrush();
    virtual void SetPattern(PSPattern*);
    virtual PSPattern* GetPattern();

    virtual Graphic* Copy();
protected:
    virtual void getExtent(float&, float&, float&, float&, float&, Graphic*);
    virtual boolean contains(PointObj&, Graphic*);
    virtual boolean intersects(BoxObj&, Graphic*);
    virtual void draw(Canvas*, Graphic*);
private:
    PSBrush* _br;
    PSPattern* _pat;
};

class S_Circle : public S_Ellipse {
public:
    S_Circle(Coord x0, Coord y0, int radius, Graphic* gr = nil);

    virtual Graphic* Copy();
};

class F_Circle : public F_Ellipse {
public:
    F_Circle(Coord x0, Coord y0, int radius, Graphic* gr = nil);

    virtual Graphic* Copy();
};

class SF_Circle : public SF_Ellipse {
public:
    SF_Circle(Coord x0, Coord y0, int radius, Graphic* gr = nil);

    virtual Graphic* Copy();
};

#include <IV-2_6/_leave.h>

#endif
