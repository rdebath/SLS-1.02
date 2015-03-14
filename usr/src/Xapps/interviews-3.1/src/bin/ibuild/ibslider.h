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
 * Slider component declarations.
 */

#ifndef ibslider_h
#define ibslider_h

#include "ibcode.h"
#include "ibgraphic.h"
#include "ibinteractor.h"

class MemberNameVar;
class SliderGraphic;
class UList;

class SliderComp : public InteractorComp {
public:
    SliderComp(SliderGraphic* = nil);
    virtual ~SliderComp();

    virtual boolean IsRelatableTo(InteractorComp*);
    virtual boolean IsRelatable();

    virtual void Instantiate();
    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    SliderGraphic* GetSliderGraphic();
    MemberNameVar* GetAdjusteeVar();

    virtual StateVar* GetState(const char*);
    virtual void SetState(const char*, StateVar*);
    virtual void Reconfig();
    virtual InteractorComp& operator = (InteractorComp&);

    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    MemberNameVar* _adjusteeVar;

};

inline MemberNameVar* SliderComp::GetAdjusteeVar() { return _adjusteeVar; }
inline boolean SliderComp::IsRelatable () { return true; }

class SliderView : public InteractorView {
public:
    SliderView(SliderComp* = nil);
    SliderComp* GetSliderComp();

    virtual void Update();
    virtual InfoDialog* GetInfoDialog();
    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);

    InteractorComp* InitComp (Coord, Coord, Coord, Coord);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class SliderCode : public CodeView {
public:
    SliderCode(SliderComp* = nil);

    virtual void Update();
    virtual boolean Definition(ostream&);
    SliderComp* GetSliderComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean CoreConstDecls(ostream&);
    virtual boolean CoreConstInits(ostream&);
    virtual boolean ConstDecls(ostream&);
    virtual boolean ConstInits(ostream&);
    virtual boolean EmitIncludeHeaders(ostream&);
};

class SliderGraphic : public IBGraphic {
public:
    SliderGraphic(int w = 0, int h = 0, CanvasVar* = nil, Graphic* = nil);

    virtual Graphic* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);

    virtual ClassId GetClassId();
    virtual const char* GetClassName();

    void GetSize(int&, int&);
protected:
    virtual void draw(Canvas*, Graphic*);
    virtual void Init(int w, int h);
    virtual void getExtent(float&, float&, float&, float&, float&, Graphic*);
    virtual void drawClipped(Canvas*, Coord, Coord, Coord, Coord, Graphic*);
private:
    int _width, _height;
};

inline void SliderGraphic::GetSize (int& w, int& h) { w = _width; h = _height;}

#endif
