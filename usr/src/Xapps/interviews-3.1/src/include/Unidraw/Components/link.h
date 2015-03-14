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
 * Link component declarations.
 */

#ifndef unidraw_components_link_h
#define unidraw_components_link_h

#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/grview.h>
#include <Unidraw/Components/psview.h>

#include <IV-2_6/_enter.h>

class Connector;
class Line;

class LinkComp : public GraphicComp {
public:
    LinkComp(Line* = nil);
    virtual ~LinkComp();

    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    virtual void Update();
    virtual void First(Iterator&);
    virtual void Last(Iterator&);
    virtual void Next(Iterator&);
    virtual void Prev(Iterator&);
    virtual boolean Done(Iterator);

    virtual GraphicComp* GetComp(Iterator);
    virtual void SetComp(GraphicComp*, Iterator&);
    virtual void SetMobility(Mobility);

    Line* GetLine();
    void GetConnectors(Connector*&, Connector*&);

    virtual Component* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    Connector* _conn1, *_conn2;
};

class LinkView : public GraphicView {
public:
    LinkView(LinkComp* = nil);
    virtual ~LinkView();

    virtual void Update();
    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);

    virtual void First(Iterator&);
    virtual void Last(Iterator&);
    virtual void Next(Iterator&);
    virtual void Prev(Iterator&);
    virtual boolean Done(Iterator);

    virtual GraphicView* GetView(Iterator);
    virtual void SetView(GraphicView*, Iterator&);

    virtual void GetEndpoints(Coord&, Coord&, Coord&, Coord&);
    LinkComp* GetLinkComp();
    virtual Graphic* GetGraphic();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual void CreateHandles();
    Line* GetLine();
    virtual LinkComp* NewSubject(Line*);

    Manipulator* CreateLinkCompManip(Viewer*, Event&, Transformer*, Tool*);
    Command* InterpLinkCompManip(Manipulator*);
protected:
    ConnectorView* _connView1, *_connView2;
};

class PSLink : public PostScriptView {
public:
    PSLink(LinkComp* = nil);

    virtual boolean Definition(ostream&);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

#include <IV-2_6/_leave.h>

#endif
