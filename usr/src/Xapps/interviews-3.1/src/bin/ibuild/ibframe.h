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
 * Frame component declarations.
 */

#ifndef ibframe_h
#define ibframe_h

#include "ibscene.h"
#include "ibgraphic.h"

class ButtonState;
class Command;
class FrameGraphic;
class MarginFrameGraphic;
class ShapeVar;

class FrameComp : public MonoSceneComp {
public:
    FrameComp(FrameGraphic* = nil);

    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);
    virtual void Resize();
    virtual void Reconfig();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual void StoreCanvas(Command*);
    virtual void RestoreCanvas(Command*);

    void SetFrameGraphic(FrameGraphic*);
    FrameGraphic* GetFrameGraphic(); 
protected:
    FrameGraphic* _framegr;
};

inline FrameGraphic* FrameComp::GetFrameGraphic() { return _framegr; }

class FrameView : public MonoSceneView {
public:
    FrameView(FrameComp* = nil);
    virtual ~FrameView();

    FrameComp* GetFrameComp();

    virtual Graphic* GetGraphic();
    virtual void Update();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    FrameGraphic* _framegr;
};

class FrameCode : public MonoSceneCode {
public:
    FrameCode(FrameComp* = nil);

    virtual void Update();
    virtual boolean Definition(ostream&);
    FrameComp* GetFrameComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean CoreConstDecls(ostream&);
    virtual boolean CoreConstInits(ostream&);
    virtual boolean ConstDecls(ostream&);
    virtual boolean ConstInits(ostream&);
    virtual boolean EmitIncludeHeaders(ostream&);
};

class FrameGraphic : public IBGraphic {
public:
    FrameGraphic(CanvasVar* = nil, Graphic* = nil, int width = 1);
    virtual const char* GetClassName();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual Graphic* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

    void GetThickness(int& l, int& b, int& r, int& t);
    virtual void SetThickness(int l, int b, int r, int t);
protected:
    virtual void getExtent(float&, float&, float&, float&, float&, Graphic*);
    virtual void draw(Canvas*, Graphic*);
    virtual void drawClipped(Canvas*, Coord, Coord, Coord, Coord, Graphic*);
protected:
    int _l, _b, _r, _t;
};

class ShadowFrameGraphic : public FrameGraphic {
public:
    ShadowFrameGraphic(
	CanvasVar* = nil, Graphic* = nil, int h = 2, int v = 2
    );
    virtual const char* GetClassName();
    virtual Graphic* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
    virtual void GetShadow(int&, int&);
    virtual void SetThickness(int l, int b, int r, int t);
protected:
    virtual void draw(Canvas*, Graphic*);
};

class MarginFrameComp : public FrameComp {
public:
    MarginFrameComp(MarginFrameGraphic* gr = nil);

    ShapeVar* GetMarginStateVar();
    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    virtual StateVar* GetState(const char*);
    
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

    virtual void Read(istream&);
    virtual void Write(ostream&);

    virtual void Resize();
    virtual void Reconfig();
    MarginFrameGraphic* GetMarginFrameGraphic();
protected:
    void Init(int, int, int, int, int, int);
protected:
    ShapeVar* _margin;
};

inline MarginFrameGraphic* MarginFrameComp::GetMarginFrameGraphic() { 
    return (MarginFrameGraphic*) _framegr; 
}
inline ShapeVar* MarginFrameComp::GetMarginStateVar() { return _margin;}

class MarginFrameView : public FrameView {
public:
    MarginFrameView(MarginFrameComp* = nil);
    MarginFrameComp* GetMarginFrameComp();

    virtual InfoDialog* GetInfoDialog();
    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);

    virtual void Update();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    boolean _rigid;
};

class MarginFrameGraphic : public FrameGraphic {
public:
    MarginFrameGraphic(
	CanvasVar* = nil, Graphic* = nil, int hm = 0, int hshr = 0,
	int hstr = 0, int vm = 0, int vshr = 0, int vstr = 0
    );
    virtual Graphic* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
    virtual void Read(istream&);
    virtual void Write(ostream&);

    void GetShrStr(int&, int&, int&, int&, int&, int&);
    void SetShrStr(int, int, int, int, int, int);
    const char* GetClassName();
protected:
    virtual void draw(Canvas*, Graphic*);
protected:
    int _hmargin, _hshrink, _hstretch;
    int _vmargin, _vshrink, _vstretch;
};

#endif

