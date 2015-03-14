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
 * Viewport component declarations.
 */

#ifndef ibviewport_h
#define ibviewport_h

#include "ibscene.h"
#include "ibgraphic.h"

class ViewportGraphic;
class Command;

class ViewportComp : public MonoSceneComp {
public:
    ViewportComp(ViewportGraphic* = nil);

    virtual boolean IsRelatable();
    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);
    virtual void Reconfig();
    virtual void Resize();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual void StoreCanvas(Command*);
    virtual void RestoreCanvas(Command*);

    void SetViewportGraphic(ViewportGraphic*);
    ViewportGraphic* GetViewportGraphic(); 
private:
    ViewportGraphic* _viewportgr;
};

inline boolean ViewportComp::IsRelatable () { return true; }
inline ViewportGraphic* ViewportComp::GetViewportGraphic() { 
    return _viewportgr; 
}

class ViewportView : public MonoSceneView {
public:
    ViewportView(ViewportComp* = nil);
    virtual ~ViewportView();

    ViewportComp* GetViewportComp();

    virtual Graphic* GetGraphic();
    virtual void Update();
    virtual boolean UpdateCanvasVar();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    ViewportGraphic* _viewportgr;
};

class ViewportCode : public MonoSceneCode {
public:
    ViewportCode(ViewportComp* = nil);

    virtual boolean Definition(ostream&);
    ViewportComp* GetViewportComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean CoreConstDecls(ostream&);
    virtual boolean CoreConstInits(ostream&);
    virtual boolean ConstDecls(ostream&);
    virtual boolean ConstInits(ostream&);
    virtual boolean EmitIncludeHeaders(ostream&);
};

class ViewportGraphic : public IBGraphic {
public:
    ViewportGraphic(
        CanvasVar* = nil, Graphic* = nil, Alignment = Center
    );

    void SetAlignment(Alignment);
    Alignment GetAlignment();

    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual Graphic* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

protected:
    virtual void getExtent(float&, float&, float&, float&, float&, Graphic*);
    virtual void draw(Canvas*, Graphic*);
    virtual void drawClipped(Canvas*, Coord, Coord, Coord, Coord, Graphic*);
protected:
    Alignment _align;
};

inline void ViewportGraphic::SetAlignment (Alignment a) { _align = a; }
inline Alignment ViewportGraphic::GetAlignment () { return _align; }

#endif
