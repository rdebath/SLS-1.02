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
 * Rect component declarations.
 */

#ifndef unidraw_components_rect_h
#define unidraw_components_rect_h

#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/grview.h>
#include <Unidraw/Components/psview.h>

#include <IV-2_6/_enter.h>

class SF_Rect;

class RectComp : public GraphicComp {
public:
    RectComp(SF_Rect* = nil);

    SF_Rect* GetRect();

    virtual Component* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class RectView : public GraphicView {
public:
    RectView(RectComp* = nil);

    virtual void Interpret(Command*);
    virtual void Update();

    virtual Manipulator* CreateManipulator(Viewer*, Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);

    virtual void GetCorners(Coord*, Coord*);
    RectComp* GetRectComp();
    virtual Graphic* GetGraphic();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual void CreateHandles();
protected:
    int _reshapeCorner;
};

class PSRect : public PostScriptView {
public:
    PSRect(RectComp* = nil);

    virtual boolean Definition(ostream&);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

#include <IV-2_6/_leave.h>

#endif
