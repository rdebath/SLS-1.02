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
 * Vertices component declarations.  A vertices component's
 * geometry is defined by a set of vertices.
 */

#ifndef unidraw_components_vertices_h
#define unidraw_components_vertices_h

#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/grview.h>
#include <Unidraw/Components/psview.h>

#include <IV-2_6/_enter.h>

class Vertices;

class VerticesComp : public GraphicComp {
public:
    Vertices* GetVertices();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    VerticesComp(Vertices* = nil);
};

class VerticesView : public GraphicView {
public:
    virtual void Interpret(Command*);
    virtual void Update();

    virtual void GetVertices(Coord*&, Coord*&, int&);
    VerticesComp* GetVerticesComp();
    virtual Graphic* GetGraphic();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    VerticesView(VerticesComp* = nil);

    virtual void CreateHandles();
    virtual boolean VertexChanged();
protected:
    int _reshapePt;
};

class PSVertices : public PostScriptView {
public:
    virtual boolean Definition(ostream&);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    PSVertices(VerticesComp* = nil);

    virtual const char* Name();
};

#include <IV-2_6/_leave.h>

#endif
