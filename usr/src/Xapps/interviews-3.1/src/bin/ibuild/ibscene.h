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
 * SceneComp and SceneView subclasses declarations 
 */

#ifndef ibscene_h
#define ibscene_h

#include "ibcode.h"
#include "ibinteractor.h"

class UList;
class Selection;

class SceneComp : public InteractorComp {
public:
    SceneComp(IBGraphic* = nil);

    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    virtual boolean IsAScene();

    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

inline boolean SceneComp::IsAScene () { return true; }

class SceneView : public InteractorView {
public:
    SceneView(SceneComp* = nil);

    virtual Graphic* GetGraphic();
    SceneComp* GetSceneComp();

    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    Selection* _isel;
};

class MonoSceneComp : public SceneComp {
public:
    MonoSceneComp(IBGraphic* = nil);
    InteractorComp* GetKid();

    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    virtual void Reconfig();
    virtual void Resize();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class MonoSceneView : public SceneView {
public:
    MonoSceneView(MonoSceneComp* = nil);

    MonoSceneComp* GetMonoSceneComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class MonoSceneCode : public CodeView {
public:
    MonoSceneCode(GraphicComp* = nil);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    CodeView* GetKidView();
};

class MonoSceneClass : public MonoSceneComp {
public:
    MonoSceneClass(IBGraphic* = nil);

    virtual void Interpret(Command*);
    virtual boolean IsANewScope();
    virtual void Read(istream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
    virtual void Instantiate();

    void Clone(Command*);
    void UnClone(Command*);
};

inline boolean MonoSceneClass::IsANewScope () { return true; }

class MonoSceneClassView : public MonoSceneView {
public:
    MonoSceneClassView(MonoSceneClass* = nil);
    MonoSceneClass* GetMonoSceneClass();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class MonoSceneClassCode : public MonoSceneCode {
public:
    MonoSceneClassCode(MonoSceneClass* = nil);

    virtual void Update();
    virtual boolean Definition(ostream&);
    MonoSceneClass* GetMonoSceneClass();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean CoreConstDecls(ostream&);
    virtual boolean CoreConstInits(ostream&);
    virtual boolean ConstDecls(ostream&);
    virtual boolean ConstInits(ostream&);
    virtual boolean EmitIncludeHeaders(ostream&);
};

#endif

