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
 * IGraphicViews - class for GraphicView Composition.
 * IGraphicComps - class for GraphicComp composition.
 */

#ifndef ibgrcomp_h
#define ibgrcomp_h

#include <Unidraw/Components/grview.h>
#include <Unidraw/Components/grcomp.h>
#include "ibcode.h"
#include "ibcomp.h"

class Selection;

class IGraphicComps : public IComp {
public:
    IGraphicComps(Graphic* = nil);

    virtual GraphicComp* GetTarget();
    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual void Instantiate();

    virtual ClassId GetSubstId(const char*& delim);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class GroupCode : public GraphicCodeView {
public:
    GroupCode(IGraphicComps* = nil);

    IGraphicComps* GetIGraphicComps();
    virtual boolean Definition(ostream&);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean GCoreConstDecls(ostream&);
    virtual boolean GCoreConstInits(ostream&);
    virtual boolean GConstDecls(ostream&);
    virtual boolean GConstInits(ostream&);

    virtual boolean CConstDecls(ostream&);
    virtual boolean CConstInits(ostream&);
    virtual boolean CCoreConstInits(ostream&);
    virtual boolean VCoreConstDecls(ostream&);
    virtual boolean VCoreConstInits(ostream&);
    virtual boolean EmitIncludeHeaders(ostream&);

    virtual const char* GetGHeader();
    virtual const char* GetCVHeader();
};

class IGraphicViews : public IView {
public:
    IGraphicViews(IGraphicComps* = nil);

    virtual void DrawHandles();
    virtual void RedrawHandles();
    virtual void EraseHandles();
    virtual void InitHandles();

    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);

    virtual Selection* SelectAll();
    virtual Selection* ViewContaining(Coord, Coord);
    virtual Selection* ViewsContaining(Coord, Coord);
    virtual Selection* ViewIntersecting(Coord, Coord, Coord, Coord);
    virtual Selection* ViewsIntersecting(Coord, Coord, Coord, Coord);
    virtual Selection* ViewsWithin(Coord, Coord, Coord, Coord);
protected:
    Selection* _isel;
};

#endif
