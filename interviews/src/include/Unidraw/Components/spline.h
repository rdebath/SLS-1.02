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
 * Spline component declarations.
 */

#ifndef unidraw_components_spline_h
#define unidraw_components_spline_h

#include <Unidraw/Components/vertices.h>

class SFH_OpenBSpline;
class SFH_ClosedBSpline;

class SplineComp : public VerticesComp {
public:
    SplineComp(SFH_OpenBSpline* = nil);

    SFH_OpenBSpline* GetSpline();

    virtual Component* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class SplineView : public VerticesView {
public:
    SplineView(SplineComp* = nil);

    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);

    SplineComp* GetSplineComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean VertexChanged();
};

class PSSpline : public PSVertices {
public:
    PSSpline(SplineComp* = nil);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual const char* Name();
};

class ClosedSplineComp : public VerticesComp {
public:
    ClosedSplineComp(SFH_ClosedBSpline* = nil);

    SFH_ClosedBSpline* GetClosedSpline();

    virtual Component* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class ClosedSplineView : public VerticesView {
public:
    ClosedSplineView(ClosedSplineComp* = nil);

    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);

    ClosedSplineComp* GetClosedSplineComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean VertexChanged();
};

class PSClosedSpline : public PSVertices {
public:
    PSClosedSpline(ClosedSplineComp* = nil);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual const char* Name();
};

#endif
