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
 * Polygon component declarations.
 */

#ifndef unidraw_components_polygon_h
#define unidraw_components_polygon_h

#include <Unidraw/Components/vertices.h>

class SF_Polygon;

class PolygonComp : public VerticesComp {
public:
    PolygonComp(SF_Polygon* = nil);

    SF_Polygon* GetPolygon();

    virtual Component* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class PolygonView : public VerticesView {
public:
    PolygonView(PolygonComp* = nil);

    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);
    PolygonComp* GetPolygonComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean VertexChanged();
};

class PSPolygon : public PSVertices {
public:
    PSPolygon(PolygonComp* = nil);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual const char* Name();
};

#endif
