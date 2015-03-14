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
 * Bitmap component declarations.
 */

#ifndef ibbitmap_h
#define ibbitmap_h

#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/grview.h>
#include <Unidraw/Graphic/graphic.h>

class Bitmap;
class BitmapGraphic;

class BitmapComp : public GraphicComp {
public:
    BitmapComp(BitmapGraphic* = nil);

    BitmapGraphic* GetBitmapGraphic();

    virtual void Read(istream&);
    virtual void Write(ostream&);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};


class BitmapView : public GraphicView {
public:
    BitmapView(BitmapComp* = nil);

    virtual void Update();
    BitmapComp* GetBitmapComp();
    virtual Graphic* GetGraphic();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class BitmapGraphic : public Graphic {
public:
    BitmapGraphic(Graphic* = nil, char* = "");
    virtual ~BitmapGraphic();

    virtual PSPattern* GetPattern();		// disallows patterned bitmaps
						// to work around IV botch
    virtual void Init(char* fg_name);
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual Graphic* Copy();
    virtual ClassId GetClassId();
protected:
    virtual void getExtent(float&, float&, float&, float&, float&, Graphic*);
    virtual void draw(Canvas*, Graphic*);
    virtual void drawClipped(Canvas*, Coord, Coord, Coord, Coord, Graphic*);
    void CalcExtent(int w, int h, float&,float&,float&,float&,float&,Graphic*);
protected:
    char* _fg_name;
    Bitmap* _fg_map;
};

#endif

