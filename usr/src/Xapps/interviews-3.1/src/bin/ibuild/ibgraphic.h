/*
 * Copyright (c) 1991 Stanford University
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
 * User interface builder-specific structured graphics objects.
 */

#ifndef ibgraphic_h
#define ibgraphic_h

#include <Unidraw/globals.h>
#include <Unidraw/Graphic/picture.h>

class CanvasVar;
class istream;
class ostream;

class IBGraphic : public Picture {
public:
    IBGraphic(CanvasVar* = nil, Graphic* g = nil);

    CanvasVar* GetCanvasVar();

    virtual void SetCanvasVar(CanvasVar*);
    virtual Graphic* Copy();

    void ReadGS(istream&);
    void WriteGS(ostream&);
protected:
    virtual void concatGS(Graphic*, Graphic*, Graphic*);
    virtual boolean contains(PointObj&, Graphic*);
    virtual boolean intersects(BoxObj&, Graphic*);

    void CalcExtent(int w, int h, float&,float&,float&,float&,float&,Graphic*);
private:
    CanvasVar* _canvasVar;
};

inline CanvasVar* IBGraphic::GetCanvasVar () { return _canvasVar; }
inline void IBGraphic::SetCanvasVar (CanvasVar* c) { _canvasVar = c; }

class HVGraphic : public IBGraphic {
public:
    void GetShape(int&, int&, int&);
    void SetShape(int, int, int);
    Orientation GetOrientation();
    virtual int MinorAxisSize();
    virtual void SetMinorAxisSize(int);
protected:
    HVGraphic(CanvasVar*, Graphic* = nil);
    void Init(int, int, int, Orientation, int = 1);
    virtual void getExtent(float&, float&, float&, float&, float&, Graphic*);
    virtual void drawClipped(Canvas*, Coord, Coord, Coord, Coord, Graphic*);
protected:
    int _natural, _shrink, _stretch;
    Orientation _orientation;
    int _minorAxisSize;
};

inline Orientation HVGraphic::GetOrientation () { return _orientation; }

#endif
