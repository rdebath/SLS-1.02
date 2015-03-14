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
 * MenuItem component declarations.
 */

#ifndef ibmenu_h
#define ibmenu_h

#include "ibmessage.h"
#include "ibbox.h"

class ButtonState;
class BooleanStateVar;
class Command;
class MenuItemGraphic;
class TrackNameVar;
class UList;

class MenuItemComp : public MessageComp {
public:
    MenuItemComp(MessageGraphic* = nil);

    virtual void Instantiate();
    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    TrackNameVar* GetTrackNameVar();
    MenuItemGraphic* GetMenuItemGraphic();
    virtual void Reconfig();

    virtual StateVar* GetState(const char*);
    virtual void SetState(const char*, StateVar*);
    virtual InteractorComp& operator = (InteractorComp&);

    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

protected:
    TrackNameVar* _proc;
};

inline TrackNameVar* MenuItemComp::GetTrackNameVar() { return _proc; }

class MenuItemView : public MessageView {
public:
    MenuItemView(MenuItemComp* = nil);
    MenuItemComp* GetMenuItemComp();

    virtual InfoDialog* GetInfoDialog();
    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class MenuItemCode : public MessageCode {
public:
    MenuItemCode(MenuItemComp* = nil);

    virtual boolean Definition(ostream&);
    MenuItemComp* GetMenuItemComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean CoreConstDecls(ostream&);
    virtual boolean CoreConstInits(ostream&);
    virtual boolean ConstDecls(ostream&);
    virtual boolean ConstInits(ostream&);
    virtual boolean EmitIncludeHeaders(ostream&);
};

class MenuBodyComp : public VBoxComp {
public:
    MenuBodyComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class MenuBarComp : public HBoxComp {
public:
    MenuBarComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class MenuBarCode : public BoxCode {
public:
    MenuBarCode(MenuBarComp* = nil);

    virtual boolean Definition(ostream&);
    MenuBarComp* GetMenuBarComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean CoreConstDecls(ostream&);
    virtual boolean CoreConstInits(ostream&);
    virtual boolean ConstDecls(ostream&);
    virtual boolean ConstInits(ostream&);
    virtual boolean EmitIncludeHeaders(ostream&);
};

class PopupMenuComp : public VBoxComp {
public:
    PopupMenuComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class PullMenuComp : public SceneComp {
public:
    PullMenuComp(MenuItemGraphic* = nil);
    virtual ~PullMenuComp();

    void SetMenuItemGraphic(MenuItemGraphic*);
    void ReAdjust();

    virtual void Resize();
    virtual void Reconfig();

    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    virtual void First(Iterator&);
    virtual void Last(Iterator&);
    virtual void Next(Iterator&);
    virtual void Prev(Iterator&);
    virtual boolean Done(Iterator);
    virtual boolean IsEmpty();

    virtual GraphicComp* GetComp(Iterator);
    virtual void SetComp(GraphicComp*, Iterator&);
    virtual void Bequeath();

    virtual void Append(GraphicComp*);
    virtual void Prepend(GraphicComp*);
    virtual void InsertBefore(Iterator, GraphicComp*);
    virtual void InsertAfter(Iterator, GraphicComp*);

    virtual void Remove(GraphicComp*);
    virtual void Remove(Iterator&);

    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

    MenuItemGraphic* GetMenuItemGraphic();
    MenuBodyComp* GetMenuBody();
private:
    MenuItemGraphic* _menuGraphic;
};

inline MenuItemGraphic* PullMenuComp::GetMenuItemGraphic () {
    return _menuGraphic;
}

class PullMenuView : public SceneView {
public:
    PullMenuView(PullMenuComp* = nil);

    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);
    virtual void Interpret(Command*);

    PullMenuComp* GetPullMenuComp();
    virtual Graphic* GetGraphic();

    virtual void Update();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    MenuItemGraphic* _menuGraphic;
};

class PullMenuCode : public MessageCode {
public:
    PullMenuCode(PullMenuComp* = nil);

    virtual boolean Definition(ostream&);
    PullMenuComp* GetPullMenuComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean CoreConstDecls(ostream&);
    virtual boolean CoreConstInits(ostream&);
    virtual boolean ConstDecls(ostream&);
    virtual boolean ConstInits(ostream&);
    virtual boolean EmitIncludeHeaders(ostream&);
};

class MenuItemGraphic : public MessageGraphic {
public:
    MenuItemGraphic(	
	const char* = nil, CanvasVar* = nil, 
	Graphic* = nil, Alignment = Left
    );

    virtual const char* GetClassName();
    virtual Graphic* Copy();
    virtual ClassId GetClassId();
};

class PDMenuGraphic : public MenuItemGraphic {
public:
    PDMenuGraphic(
	const char* = nil, CanvasVar* = nil, 
	Graphic* = nil, Alignment = Center
    );

    virtual const char* GetClassName();
    virtual Graphic* Copy();
    virtual ClassId GetClassId();
};

class PRMenuGraphic : public MenuItemGraphic {
public:
    PRMenuGraphic(
	const char* = nil, CanvasVar* = nil, 
	Graphic* = nil, Alignment = Center
    );

    virtual const char* GetClassName();
    virtual Graphic* Copy();
    virtual ClassId GetClassId();
};

#endif

