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
 * StrEditor component declarations.
 */

#ifndef ibstred_h
#define ibstred_h

#include "ibbutton.h"

class StrEditGraphic;

class StrEditComp : public ButtonComp {
public:
    StrEditComp(MessageGraphic* = nil);

    StrEditGraphic* GetStrEditGraphic();
    virtual void Relate(InteractorComp*);

    virtual void Reconfig();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
    virtual void Instantiate();
};

class StrEditView : public ButtonView {
public:
    StrEditView(StrEditComp* = nil);

    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);
    StrEditComp* GetStrEditComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class StrEditGraphic : public MessageGraphic {
public:
    StrEditGraphic(const char* = nil, CanvasVar* = nil, Graphic* = nil);

    virtual const char* GetClassName();
    virtual Graphic* Copy();
    virtual ClassId GetClassId();
protected:
    virtual void draw(Canvas*, Graphic*);
};

class StrEditCode : public ButtonCode {
public:
    StrEditCode(StrEditComp* = nil);

    virtual boolean Definition(ostream&);
    StrEditComp* GetStrEditComp();

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
