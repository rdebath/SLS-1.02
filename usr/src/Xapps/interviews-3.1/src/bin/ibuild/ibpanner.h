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
 * Panner component declarations.
 */

#ifndef ibpanner_h
#define ibpanner_h

#include "ibbox.h"
#include "ibgraphic.h"

class MemberNameVar;
class UList;

class PannerComp : public VBoxComp {
public:
    PannerComp(int w = 0, int h = 0);
    virtual ~PannerComp();

    MemberNameVar* GetAdjusteeVar();

    virtual boolean IsRelatableTo(InteractorComp*);
    virtual boolean IsRelatable();

    virtual void Instantiate();
    virtual void Reconfig();
    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    virtual void SetState(const char*, StateVar*);
    virtual StateVar* GetState(const char*);

    virtual boolean IsAScene();

    virtual InteractorComp& operator = (InteractorComp&);

    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    MemberNameVar* _adjusteeVar;
    void Interior(int, int);
};

inline boolean PannerComp::IsAScene() { return false; }
inline boolean PannerComp::IsRelatable() { return true; }
inline MemberNameVar* PannerComp::GetAdjusteeVar() { return _adjusteeVar; }

class PannerView : public SceneView {
public:
    PannerView(PannerComp* = nil);

    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);
    virtual InfoDialog* GetInfoDialog();

    virtual Selection* SelectAll();
    virtual Selection* ViewContaining(Coord, Coord);
    virtual Selection* ViewsContaining(Coord, Coord);
    virtual Selection* ViewIntersecting(Coord, Coord, Coord, Coord);
    virtual Selection* ViewsIntersecting(Coord, Coord, Coord, Coord);
    virtual Selection* ViewsWithin(Coord, Coord, Coord, Coord);

    PannerComp* GetPannerComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class PannerCode : public CodeView {
public:
    PannerCode(PannerComp* = nil);

    virtual void Update();
    virtual boolean Definition(ostream&);
    PannerComp* GetPannerComp();

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
