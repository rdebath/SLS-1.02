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
 * Glue component declarations.
 */

#ifndef ibglue_h
#define ibglue_h

#include "ibcode.h"
#include "ibgraphic.h"
#include "ibinteractor.h"

class GlueGraphic;

class GlueComp : public HVComp {
public:
    GlueComp(GlueGraphic* = nil);

    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);
    GlueGraphic* GetGlueGraphic();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual HVGraphic* InitGraphic(Orientation, int);
};

class GlueView : public HVView {
public:
    GlueView(GlueComp* = nil);

    virtual Manipulator* CreateManipulator(
	Viewer*,Event&,Transformer*,Tool*
    );
    virtual Command* InterpretManipulator(Manipulator*);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual HVComp* InitComp(Coord, Coord, Coord, Coord);
private:
    boolean _rigid;
};

class GlueCode : public CodeView {
public:
    GlueCode(GlueComp* = nil);

    virtual void Update();
    virtual boolean Definition(ostream&);
    GlueComp* GetGlueComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean CoreConstDecls(ostream&);
    virtual boolean CoreConstInits(ostream&);
    virtual boolean ConstDecls(ostream&);
    virtual boolean ConstInits(ostream&);
    virtual boolean EmitIncludeHeaders(ostream&);
};

class GlueGraphic : public HVGraphic {
public:
    GlueGraphic(int nat, Orientation, CanvasVar*, Graphic* = nil);

    virtual int MinorAxisSize();

    virtual Graphic* Copy();
protected:
    virtual void draw(Canvas*, Graphic*);
};

#endif
