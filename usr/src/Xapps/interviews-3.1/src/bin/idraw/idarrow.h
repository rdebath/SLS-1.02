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
 * ArrowLineComp, ArrowMultiLineComp, ArrowSplineComp and related
 * classes for components with arrowheads.
 */

#ifndef idarrow_h
#define idarrow_h

#include <Unidraw/Components/line.h>
#include <Unidraw/Components/spline.h>

class ArrowLine;
class ArrowMultiLine;
class ArrowOpenBSpline;

class ArrowLineComp : public LineComp {
public:
    ArrowLineComp(ArrowLine* = nil);

    ArrowLine* GetArrowLine();
    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    virtual Component* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual ClassId GetSubstId(const char*&);
    virtual boolean IsA(ClassId);
};

class ArrowLineView : public LineView {
public:
    ArrowLineView(ArrowLineComp* = nil);

    virtual Command* InterpretManipulator(Manipulator*);
    virtual void Update();
    
    ArrowLineComp* GetArrowLineComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class PSArrowLine : public PSLine {
public:
    PSArrowLine(ArrowLineComp* = nil);

    virtual boolean Definition(ostream&);
    virtual void Brush(ostream&);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class ArrowMultiLineComp : public MultiLineComp {
public:
    ArrowMultiLineComp(ArrowMultiLine* = nil);

    ArrowMultiLine* GetArrowMultiLine();
    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    virtual Component* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual ClassId GetSubstId(const char*&);
    virtual boolean IsA(ClassId);
};

class ArrowMultiLineView : public MultiLineView {
public:
    ArrowMultiLineView(ArrowMultiLineComp* = nil);

    virtual Command* InterpretManipulator(Manipulator*);
    virtual void Update();
    
    ArrowMultiLineComp* GetArrowMultiLineComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class PSArrowMultiLine : public PSMultiLine {
public:
    PSArrowMultiLine(ArrowMultiLineComp* = nil);

    virtual boolean Definition(ostream&);
    virtual void Brush(ostream&);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class ArrowSplineComp : public SplineComp {
public:
    ArrowSplineComp(ArrowOpenBSpline* = nil);

    ArrowOpenBSpline* GetArrowOpenBSpline();
    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    virtual Component* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual ClassId GetSubstId(const char*&);
    virtual boolean IsA(ClassId);
};

class ArrowSplineView : public SplineView {
public:
    ArrowSplineView(ArrowSplineComp* = nil);

    virtual Command* InterpretManipulator(Manipulator*);
    virtual void Update();
    
    ArrowSplineComp* GetArrowSplineComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class PSArrowSpline : public PSSpline {
public:
    PSArrowSpline(ArrowSplineComp* = nil);

    virtual boolean Definition(ostream&);
    virtual void Brush(ostream&);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

#endif
