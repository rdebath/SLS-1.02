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
 * Scroller component declarations.
 */

#ifndef ibscroller_h
#define ibscroller_h

#include "ibcode.h"
#include "ibgraphic.h"
#include "ibinteractor.h"

class MemberNameVar;
class ScrollerGraphic;
class UList;

class ScrollerComp : public HVComp {
public:
    ScrollerComp(ScrollerGraphic* = nil);
    virtual ~ScrollerComp();

    virtual boolean IsRelatableTo(InteractorComp*);
    virtual boolean IsRelatable();

    virtual void Instantiate();
    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    ScrollerGraphic* GetScrollerGraphic();
    MemberNameVar* GetAdjusteeVar();

    virtual void SetState(const char*, StateVar*);
    virtual StateVar* GetState(const char*);
    virtual InteractorComp& operator = (InteractorComp&);

    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual HVGraphic* InitGraphic(Orientation, int = 1);
private:
    MemberNameVar* _adjusteeVar;
};

inline MemberNameVar* ScrollerComp::GetAdjusteeVar() { return _adjusteeVar; }
inline boolean ScrollerComp::IsRelatable () { return true; }

class ScrollerView : public HVView {
public:
    ScrollerView(ScrollerComp* = nil);

    ScrollerComp* GetScrollerComp();
    virtual InfoDialog* GetInfoDialog();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual HVComp* InitComp(Coord, Coord, Coord, Coord);
};

class ScrollerCode : public CodeView {
public:
    ScrollerCode(ScrollerComp* = nil);

    virtual void Update();
    virtual boolean Definition(ostream&);
    ScrollerComp* GetScrollerComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean CoreConstDecls(ostream&);
    virtual boolean CoreConstInits(ostream&);
    virtual boolean ConstDecls(ostream&);
    virtual boolean ConstInits(ostream&);
    virtual boolean EmitIncludeHeaders(ostream&);
};

class ScrollerGraphic : public HVGraphic {
public:
    ScrollerGraphic(Orientation, CanvasVar*, Graphic* = nil, int nat = 0);

    virtual int MinorAxisSize();

    virtual Graphic* Copy();
protected:
    virtual void draw(Canvas*, Graphic*);
};

#endif
