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
 * GraphicView - view of a GraphicComp.
 * GraphicViews - class for GraphicView Composition.
 */

#ifndef unidraw_components_grview_h
#define unidraw_components_grview_h

#include <Unidraw/Components/compview.h>

#include <IV-2_6/_enter.h>

class ConnectorView;
class Event;
class Manipulator;
class Graphic;
class GraphicComp;
class GraphicComps;
class Picture;
class Rubberband;
class Selection;
class Tool;
class Transformer;
class Viewer;

class GraphicView : public ComponentView {
public:
    virtual ~GraphicView();

    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    virtual void DrawHandles();
    virtual void RedrawHandles();
    virtual void InitHandles();
    virtual void EraseHandles();

    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);

    virtual ComponentView* GetParent();
    virtual Graphic* GetGraphic();
    virtual Viewer* GetViewer();
    GraphicComp* GetGraphicComp();
    boolean Includes(GraphicView*);
    GraphicView* GetGraphicView(Component*);

    virtual GraphicView* GetView(Iterator);
    virtual void SetView(GraphicView*, Iterator&);

    virtual Selection* SelectAll();
    virtual Selection* ViewContaining(Coord, Coord);
    virtual Selection* ViewsContaining(Coord, Coord);
    virtual Selection* ViewIntersecting(Coord, Coord, Coord, Coord);
    virtual Selection* ViewsIntersecting(Coord, Coord, Coord, Coord);
    virtual Selection* ViewsWithin(Coord, Coord, Coord, Coord);
    virtual ConnectorView* ConnectorIntersecting(Coord, Coord, Coord, Coord);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    GraphicView(GraphicComp* = nil);

    virtual void SetGraphic(Graphic*);
    virtual void CreateHandles();
    virtual int ClosestPoint(Coord x[], Coord y[], int n, Coord px, Coord py);

    GraphicView* View(UList*);
    GraphicView* GetGraphicView(Graphic*);

    Manipulator* CreateGraphicCompManip(Viewer*, Event&, Transformer*, Tool*);
    Manipulator* CreateStretchManip(Viewer*, Event&, Transformer*, Tool*);
    Command* InterpretGraphicCompManip(Manipulator*);
    Command* InterpretStretchManip(Manipulator*);

    friend class GVUpdater;
    void AddDamage(Graphic*);
    void IncurDamage(Graphic*);
    void Unselect(GraphicView*);

    virtual void Add(GraphicView*);
    virtual void Append(GraphicView*);
    virtual void InsertBefore(Iterator, GraphicView*);
    virtual void Remove(Iterator&);
    virtual void DeleteView(Iterator&);
protected:
    Graphic* _gr;
    Rubberband* _handles;
};

class GraphicViews : public GraphicView {
public:
    GraphicViews(GraphicComps* = nil);
    virtual ~GraphicViews();

    virtual void Interpret(Command*);
    virtual void Update();

    virtual Graphic* GetGraphic();
    GraphicComps* GetGraphicComps();

    virtual void First(Iterator&);
    virtual void Last(Iterator&);
    virtual void Next(Iterator&);
    virtual void Prev(Iterator&);
    virtual boolean Done(Iterator);

    virtual GraphicView* GetView(Iterator);
    virtual void SetView(GraphicView*, Iterator&);

    virtual Selection* SelectAll();
    virtual Selection* ViewContaining(Coord, Coord);
    virtual Selection* ViewsContaining(Coord, Coord);
    virtual Selection* ViewIntersecting(Coord, Coord, Coord, Coord);
    virtual Selection* ViewsIntersecting(Coord, Coord, Coord, Coord);
    virtual Selection* ViewsWithin(Coord, Coord, Coord, Coord);
    virtual ConnectorView* ConnectorIntersecting(Coord, Coord, Coord, Coord);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    UList* Elem(Iterator);

    virtual void Add(GraphicView*);
    virtual void Append(GraphicView*);
    virtual void InsertBefore(Iterator, GraphicView*);
    virtual void Remove(Iterator&);
    virtual void DeleteView(Iterator&);
protected:
    UList* _views;
};

#include <IV-2_6/_leave.h>

#endif
