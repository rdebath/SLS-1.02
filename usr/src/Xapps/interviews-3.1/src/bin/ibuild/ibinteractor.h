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
 * Interactor component declarations.
 */

#ifndef ibinteractor_h
#define ibinteractor_h

#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/grview.h>
#include <Unidraw/Commands/datas.h>

class ButtonState;
class ButtonStateVar;
class CanvasVar;
class Clipboard;
class Editor;
class EditorInfo;
class HVGraphic;
class InfoDialog;
class MemberNameVar;
class InstanceNameVar;
class InteractorComp;
class IBShape;
class ShapeVar;
class StateVar;
class SubclassNameVar;
class Transformer;
class IBGraphic;

class lbrtData : public VoidData {
public:
    lbrtData(InteractorComp*);
    virtual ~lbrtData();

public:
    Coord _l, _b, _r, _t;
    IBShape* _ibshape;
    Transformer* _tr;
};

class Props {
public:
    Props(const char*);
    virtual ~Props();

    void SetPropsText(const char*);
    const char* GetPropsText();

    Props* Copy();
    void Read(istream&);
    void Write(ostream&);
private:
    char* _props;
    int _size;
};

inline const char* Props::GetPropsText () { return _props; }

class InteractorComp : public GraphicComps {
public:
    virtual ~InteractorComp();
    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);
    virtual void Resize();
    virtual void Reconfig();

    virtual InteractorComp* GetIComp(Iterator);

    virtual boolean IsAScene();
    virtual boolean IsANewScope();
    virtual boolean IsRelatableTo(InteractorComp*);
    virtual boolean IsRelatable();
    virtual void Relate(InteractorComp*);
    virtual void Unrelate(InteractorComp*);

    CanvasVar* GetCanvasVar();
    SubclassNameVar* GetClassNameVar();
    InstanceNameVar* GetInstanceNameVar();
    MemberNameVar* GetMemberNameVar();
    ShapeVar* GetShapeVar();
    Props* GetProps();

    void Propagate(Command*);
    void Unpropagate(Command*);

    virtual IBGraphic* GetIBGraphic();
    virtual ButtonStateVar* GetButtonStateVar();
    virtual InteractorComp& operator = (InteractorComp&);
    virtual StateVar* GetState(const char*);
    virtual void SetState(const char*, StateVar*);
    virtual void StoreCanvas(Command*);
    virtual void RestoreCanvas(Command*);

    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

    virtual void Instantiate();
protected:
    InteractorComp(IBGraphic* = nil);
    void Place(InteractorComp*);
    void Place(InteractorComp*, Coord, Coord, Coord, Coord);
protected:
    CanvasVar* _canvasVar;
    SubclassNameVar* _classNameVar;
    InstanceNameVar* _instanceNameVar;
    MemberNameVar* _memberVar;
    ShapeVar* _shapeVar;
    Props* _props;
private:
    void InitCanvas(InteractorComp*);
};

inline CanvasVar* InteractorComp::GetCanvasVar () { return _canvasVar; }
inline SubclassNameVar* InteractorComp::GetClassNameVar () {
    return _classNameVar; 
}
inline InstanceNameVar* InteractorComp::GetInstanceNameVar () {
    return _instanceNameVar; 
}
inline ShapeVar* InteractorComp::GetShapeVar () { return _shapeVar; }
inline Props* InteractorComp::GetProps () { return _props; }
inline MemberNameVar* InteractorComp::GetMemberNameVar () {
    return _memberVar; 
}
inline ButtonStateVar* InteractorComp::GetButtonStateVar () { return nil; }
inline boolean InteractorComp::IsAScene() { return false; }
inline boolean InteractorComp::IsANewScope() { return false; }
inline boolean InteractorComp::IsRelatableTo(InteractorComp*) { return false; }
inline boolean InteractorComp::IsRelatable() { return false; }

class InteractorView : public GraphicViews {
public:
    InteractorComp* GetInteractorComp();

    virtual Graphic* GetGraphic();
    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);
    virtual void Interpret(Command*);
    virtual InfoDialog* GetInfoDialog();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    InteractorView(InteractorComp* = nil);
    virtual boolean UpdateCanvasVar();
    boolean Different(Graphic*, Graphic*);
    void GetABSCoord(Editor*, Coord&, Coord&, Coord&, Coord&);
};

class HVComp : public InteractorComp {
public:
    virtual void Reconfig();
    HVGraphic* GetHVGraphic();

    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    HVComp(HVGraphic* = nil);
    virtual HVGraphic* InitGraphic(Orientation, int = 1);
};

class HVView : public InteractorView {
public:
    virtual void Update();
    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);

    HVComp* GetHVComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    HVView(HVComp* = nil);
    virtual HVComp* InitComp(Coord, Coord, Coord, Coord);
};

#endif

