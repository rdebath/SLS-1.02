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
 *  PanelCtrl component declaration
 */

#ifndef ibpanelctrl_h
#define ibpanelctrl_h

#include "ibmessage.h"
#include "ibviewer.h"

class ButtonStateVar;
class MemberNameVar;
class IDVar;
class ITextComp;
class IView;
class InfoDialog;
class PanelCtrlGraphic;

class PanelCtrlComp : public GrBlockComp {
public:
    PanelCtrlComp(PanelCtrlGraphic* = nil);
    virtual ~PanelCtrlComp();

    virtual boolean IsRelatableTo(InteractorComp*);
    virtual boolean IsRelatable();

    virtual void Resize();
    virtual void Reconfig();

    virtual void Instantiate();
    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

    virtual StateVar* GetState(const char*);
    virtual void SetState(const char*, StateVar*);
    virtual InteractorComp& operator = (InteractorComp&);
    virtual ButtonStateVar* GetButtonStateVar();

    ITextComp* GetKeyLabel();
    MemberNameVar* GetToolName();
    MemberNameVar* GetEditorVar();
    PanelCtrlGraphic* GetPanelCtrlGraphic();
protected:
    ITextComp* _keylabel;
    MemberNameVar* _toolname;
    MemberNameVar* _edVar;
    ButtonStateVar* _curCtrlVar;
};

inline boolean PanelCtrlComp::IsRelatable () { return true; }
inline ITextComp* PanelCtrlComp::GetKeyLabel() { return _keylabel; }
inline MemberNameVar* PanelCtrlComp::GetToolName() { return _toolname; }
inline MemberNameVar* PanelCtrlComp::GetEditorVar() { return _edVar; }
inline ButtonStateVar* PanelCtrlComp::GetButtonStateVar() {
    return _curCtrlVar; 
}

class PanelCtrlView : public GrBlockView {
public:
    PanelCtrlView(PanelCtrlComp* = nil);
    PanelCtrlComp* GetPanelCtrlComp();

    virtual GraphicComp* CreateProtoComp(Editor*, Coord, Coord, Coord, Coord);
    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);

    virtual void Interpret(Command*);
    virtual Graphic* GetGraphic();
    virtual void Update();
    virtual InfoDialog* GetInfoDialog();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    IView* _keylabel;
};

class PanelCtrlGraphic : public IBViewerGraphic {
public:
    PanelCtrlGraphic(
        const char* name = "", Orientation = Horizontal, 
        CanvasVar* = nil, Graphic* = nil
    );
    virtual ~PanelCtrlGraphic();
    Orientation GetOrientation();
    const char* GetName();

    virtual Graphic* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    Orientation _orient;
    char* _name;
};

inline Orientation PanelCtrlGraphic::GetOrientation () { return _orient; }
inline const char* PanelCtrlGraphic::GetName () { return _name; }

class PanelCtrlCode : public GrBlockCode {
public:
    PanelCtrlCode(PanelCtrlComp* = nil);

    virtual void Update();
    virtual boolean Definition(ostream&);
    PanelCtrlComp* GetPanelCtrlComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean CoreConstDecls(ostream&);
    virtual boolean CoreConstInits(ostream&);
    virtual boolean ConstDecls(ostream&);
    virtual boolean ConstInits(ostream&);
    virtual boolean EmitIncludeHeaders(ostream&);
    virtual boolean EmitCommonHeaders(const char*, ostream&);

    virtual boolean ToolCoreConstDecls(ostream&);
    virtual boolean ToolCoreConstInits(ostream&);
    virtual boolean ToolConstDecls(ostream&);
    virtual boolean ToolConstInits(ostream&);

    void HashKeyCode(char*);
    boolean SingleKid();
};

#endif

