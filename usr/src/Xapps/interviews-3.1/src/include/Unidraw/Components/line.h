/*
 * Copyright (c) 1990, 1991 Stanford University
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
 * Line and MultiLine component declarations.
 */

#ifndef unidraw_components_line_h
#define unidraw_components_line_h

#include <Unidraw/Components/vertices.h>

#include <IV-2_6/_enter.h>

class Line;
class SF_MultiLine;

class LineComp : public GraphicComp {
public:
    LineComp(Line* = nil);

    virtual void Interpret(Command*);
    Line* GetLine();

    virtual Component* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class LineView : public GraphicView {
public:
    LineView(LineComp* = nil);

    virtual void Interpret(Command*);
    virtual void Update();

    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);

    virtual void GetEndpoints(Coord&, Coord&, Coord&, Coord&);
    LineComp* GetLineComp();
    virtual Graphic* GetGraphic();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual void CreateHandles();
};

class PSLine : public PostScriptView {
public:
    PSLine(LineComp* = nil);

    virtual boolean Definition(ostream&);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class MultiLineComp : public VerticesComp {
public:
    MultiLineComp(SF_MultiLine* = nil);

    SF_MultiLine* GetMultiLine();

    virtual Component* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class MultiLineView : public VerticesView {
public:
    MultiLineView(MultiLineComp* = nil);

    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);
    MultiLineComp* GetMultiLineComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean VertexChanged();
};

class PSMultiLine : public PSVertices {
public:
    PSMultiLine(MultiLineComp* = nil);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual const char* Name();
};

#include <IV-2_6/_leave.h>

#endif
