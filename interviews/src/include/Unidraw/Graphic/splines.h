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
 * Interface to BSplines, objects derived from Graphic.
 */

#ifndef unidraw_graphic_splines_h
#define unidraw_graphic_splines_h

#include <Unidraw/Graphic/lines.h>

#include <IV-2_6/_enter.h>

class OpenBSpline : public Vertices {
public:
    virtual int GetOriginal(const Coord*&, const Coord*&);
protected:
    OpenBSpline(Coord* x, Coord* y, int count, Graphic* gr = nil) ;

    boolean s_contains(PointObj&, Graphic*);
    boolean f_contains(PointObj&, Graphic*);
    boolean s_intersects(BoxObj&, Graphic*);
    boolean f_intersects(BoxObj&, Graphic*);
};

class S_OpenBSpline : public OpenBSpline {
public:
    S_OpenBSpline(Coord* x, Coord* y, int count, Graphic* gr =nil);
    virtual ~S_OpenBSpline();

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

class F_OpenBSpline : public OpenBSpline {
public:
    F_OpenBSpline(Coord* x, Coord* y, int count, Graphic* = nil);
    virtual ~F_OpenBSpline();

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

class SF_OpenBSpline : public OpenBSpline {
public:
    SF_OpenBSpline(Coord* x, Coord* y, int count, Graphic* = nil);
    virtual ~SF_OpenBSpline();

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

class SFH_OpenBSpline : public SF_OpenBSpline {
public:
    SFH_OpenBSpline(Coord* x, Coord* y, int count, Graphic* = nil);

    virtual Graphic* Copy();
protected:
    virtual boolean contains(PointObj&, Graphic*);
    virtual boolean intersects(BoxObj&, Graphic*);
};

class ClosedBSpline : public Vertices {
protected:
    ClosedBSpline(Coord* x, Coord* y, int count, Graphic* gr = nil) ;

    boolean s_contains(PointObj&, Graphic*);
    boolean f_contains(PointObj&, Graphic*);
    boolean s_intersects(BoxObj&, Graphic*);
    boolean f_intersects(BoxObj&, Graphic*);
};

class S_ClosedBSpline : public ClosedBSpline {
public:
    S_ClosedBSpline(Coord* x, Coord* y, int count, Graphic* gr =nil);
    virtual ~S_ClosedBSpline();

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

class F_ClosedBSpline : public ClosedBSpline {
public:
    F_ClosedBSpline(Coord* x, Coord* y, int count, Graphic* gr =nil);
    virtual ~F_ClosedBSpline();

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

class SF_ClosedBSpline : public ClosedBSpline {
public:
    SF_ClosedBSpline(Coord* x, Coord* y, int count, Graphic* = nil);
    virtual ~SF_ClosedBSpline();

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

class SFH_ClosedBSpline : public SF_ClosedBSpline {
public:
    SFH_ClosedBSpline(Coord* x, Coord* y, int count, Graphic* = nil);

    virtual Graphic* Copy();
protected:
    virtual boolean contains(PointObj&, Graphic*);
    virtual boolean intersects(BoxObj&, Graphic*);
};

#include <IV-2_6/_leave.h>

#endif
