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
 * Border component declarations.
 */

#ifndef ibborder_h
#define ibborder_h

#include "ibcode.h"
#include "ibgraphic.h"
#include "ibinteractor.h"

class BorderGraphic;

class BorderComp : public HVComp {
public:
    BorderComp(BorderGraphic* = nil);

    BorderGraphic* GetBorderGraphic();

    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual HVGraphic* InitGraphic(Orientation, int = 1);
};

class BorderView : public HVView {
public:
    BorderView(BorderComp* = nil);

    virtual Command* InterpretManipulator(Manipulator*);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
    virtual void Update();
protected:
    virtual HVComp* InitComp(Coord, Coord, Coord, Coord);
};

class BorderCode : public CodeView {
public:
    BorderCode(BorderComp* = nil);

    BorderComp* GetBorderComp();
    virtual void Update();
    virtual boolean Definition(ostream&);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean CoreConstDecls(ostream&);
    virtual boolean CoreConstInits(ostream&);
    virtual boolean ConstDecls(ostream&);
    virtual boolean ConstInits(ostream&);
    virtual boolean EmitIncludeHeaders(ostream&);
};

class BorderGraphic : public HVGraphic {
public:
    BorderGraphic(Orientation, CanvasVar*, Graphic* = nil, int nat = 0, int w = 1);

    virtual Graphic* Copy();
protected:
    virtual void draw(Canvas*, Graphic*);
};

#endif

