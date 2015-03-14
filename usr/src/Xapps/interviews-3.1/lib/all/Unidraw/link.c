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
 * Link component definitions.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/globals.h>
#include <Unidraw/iterator.h>
#include <Unidraw/manips.h>
#include <Unidraw/selection.h>
#include <Unidraw/statevars.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>
#include <Unidraw/Commands/edit.h>
#include <Unidraw/Commands/macro.h>
#include <Unidraw/Commands/transforms.h>
#include <Unidraw/Components/link.h>
#include <Unidraw/Components/pin.h>
#include <Unidraw/Graphic/lines.h>
#include <Unidraw/Graphic/picture.h>
#include <Unidraw/Tools/tool.h>

#include <InterViews/event.h>
#include <IV-2_6/InterViews/rubgroup.h>
#include <IV-2_6/InterViews/rubline.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

#include <math.h>
#include <stdlib.h>
#include <stream.h>

/*****************************************************************************/

ClassId LinkComp::GetClassId () { return LINK_COMP; }

boolean LinkComp::IsA (ClassId id) {
    return LINK_COMP == id || GraphicComp::IsA(id);
}

static void InitLine (Line* line, float x0, float y0, float x1, float y1) {
    Transformer* t = new Transformer(x1-x0, 0, 0, y1-y0, x0, y0);
    line->SetTransformer(t);
    Unref(t);
}

Component* LinkComp::Copy () { 
    LinkComp* copy = new LinkComp((Line*) GetLine()->Copy());
    
    *copy->GetGraphic() = *GetGraphic();
    return copy;
}

LinkComp::LinkComp (Line* line) {
    if (line != nil) {
        Coord x0, y0, x1, y1;
        float fx0, fy0, fx1, fy1;

        line->GetOriginal(x0, y0, x1, y1);
        Transformer* t = line->GetTransformer();
        Graphic* parent = new Picture(line);
        parent->SetTransformer(nil);

        if (t == nil) {
            fx0 = x0; fy0 = y0; fx1 = x1; fy1 = y1;
        } else {
            t->Transform(float(x0), float(y0), fx0, fy0);
            t->Transform(float(x1), float(y1), fx1, fy1);
        }
        delete line;
        line = new Line(0, 0, 1, 1);
        InitLine(line, fx0, fy0, fx1, fy1);

        PinGraphic* pg1 = new PinGraphic;
        PinGraphic* pg2 = new PinGraphic;
        pg1->SetBrush(psnonebr);
        pg2->SetBrush(psnonebr);
        pg1->Translate(fx0, fy0);
        pg2->Translate(fx1, fy1);

        _conn1 = new PinComp(pg1);
        _conn2 = new PinComp(pg2);

        parent->Append(line, pg1, pg2);
        SetGraphic(parent);
    }
}

void LinkComp::Interpret (Command* cmd) {
    if (cmd->IsA(DELETE_CMD) || cmd->IsA(CUT_CMD)) {
        _conn1->Interpret(cmd);
        _conn2->Interpret(cmd);

    } else {
        GraphicComp::Interpret(cmd);
    }
}

void LinkComp::Uninterpret (Command* cmd) {
    if (cmd->IsA(DELETE_CMD) || cmd->IsA(CUT_CMD)) {
        _conn2->Uninterpret(cmd);
        _conn1->Uninterpret(cmd);

    } else {
        GraphicComp::Uninterpret(cmd);
    }
}

void LinkComp::Read (istream& in) {
    GraphicComp::Read(in);

    Line* line = new Line(0, 0, 1, 1);

    Transformer* t = ReadTransformer(in);
    line->SetTransformer(t);
    Unref(t);

    _conn1 = (Connector*) unidraw->GetCatalog()->ReadComponent(in);
    _conn2 = (Connector*) unidraw->GetCatalog()->ReadComponent(in);

    Graphic* parent = new Picture;
    parent->FillBg(ReadBgFilled(in));
    PSColor* fg = ReadColor(in);
    PSColor* bg = ReadColor(in);
    parent->SetColors(fg, bg);
    parent->SetBrush(ReadBrush(in));

    t = ReadTransformer(in);
    parent->SetTransformer(t);
    Unref(t);

    parent->Append(line, _conn1->GetGraphic(), _conn2->GetGraphic());
    SetGraphic(parent);
}

void LinkComp::Write (ostream& out) {
    GraphicComp::Write(out);
    Line* line = GetLine();

    WriteTransformer(line->GetTransformer(), out);

    unidraw->GetCatalog()->WriteComponent(_conn1, out);
    unidraw->GetCatalog()->WriteComponent(_conn2, out);

    Graphic* parent = line->Parent();
    WriteBgFilled(parent->BgFilled(), out);
    WriteColor(parent->GetFgColor(), out);
    WriteColor(parent->GetBgColor(), out);
    WriteBrush(parent->GetBrush(), out);
    WriteTransformer(parent->GetTransformer(), out);
}

void LinkComp::First (Iterator& i) { i.SetValue(_conn1); }
void LinkComp::Last (Iterator& i) { i.SetValue(_conn2); }
boolean LinkComp::Done (Iterator i) { return i.GetValue() == nil; }

void LinkComp::Next (Iterator& i) { 
    void* v = i.GetValue();

    if (v == nil) {
        i.SetValue(_conn1);

    } else if (v == _conn1) {
        i.SetValue(_conn2);

    } else {
        i.SetValue(nil);
    }
}

void LinkComp::Prev (Iterator& i) { 
    void* v = i.GetValue();

    if (v == nil) {
        i.SetValue(_conn2);

    } else if (v == _conn1) {
        i.SetValue(nil);

    } else {
        i.SetValue(_conn1);
    }
}

GraphicComp* LinkComp::GetComp (Iterator i) { 
    return (GraphicComp*) i.GetValue(); 
}

void LinkComp::SetComp (GraphicComp* gc, Iterator& i) { i.SetValue(gc); }

void LinkComp::Update () {
    float fx0, fy0, fx1, fy1;
    Transformer* t1 = _conn1->GetGraphic()->GetTransformer();
    Transformer* t2 = _conn2->GetGraphic()->GetTransformer();

    t1->Transform(0., 0., fx0, fy0);
    t2->Transform(0., 0., fx1, fy1);

    InitLine(GetLine(), fx0, fy0, fx1, fy1);
    Notify();
}

Line* LinkComp::GetLine () { 
    Iterator i;
    Graphic* gr = GetGraphic();

    gr->First(i);
    return (Line*) gr->GetGraphic(i);
}

void LinkComp::SetMobility (Mobility m) {
    _conn1->SetMobility(m);
    _conn2->SetMobility(m);
}

void LinkComp::GetConnectors (Connector*& c1, Connector*& c2) {
    c1 = _conn1;
    c2 = _conn2;
}

LinkComp::~LinkComp () {
    Graphic* parent = GraphicComp::GetGraphic();
    Graphic* g1 = _conn1->GetGraphic();
    Graphic* g2 = _conn2->GetGraphic();

    parent->Remove(g1);
    parent->Remove(g2);

    delete _conn1;
    delete _conn2;
}

/****************************************************************************/

LinkComp* LinkView::GetLinkComp () { return (LinkComp*) GetSubject(); }
ClassId LinkView::GetClassId () { return LINK_VIEW; }

boolean LinkView::IsA (ClassId id) {
    return LINK_VIEW == id || GraphicView::IsA(id);
}

LinkView::LinkView (LinkComp* subj) : GraphicView(subj) {
    _connView1 = _connView2 = nil;
}

void LinkView::Update () {
    LinkComp* linkComp = GetLinkComp();
    Graphic* link = GetGraphic();
    Graphic* line = GetLine();
    Graphic* subjLine = linkComp->GetLine();

    IncurDamage(line);
    *line = *subjLine;
    *link = *linkComp->GetGraphic();
    IncurDamage(line);
    EraseHandles();
}

void LinkView::CreateHandles () {
    Coord x[2], y[2];
    Viewer* v = GetViewer();
    
    if (v != nil) {
        GetEndpoints(x[0], y[0], x[1], y[1]);
        _handles = new RubberHandles(nil, nil, x, y, 2, 0, HANDLE_SIZE);
        v->InitRubberband(_handles);
    }
}

Manipulator* LinkView::CreateLinkCompManip (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    GraphicView* views = v->GetGraphicView();
    Selection* s = v->GetSelection();
    RubberGroup* rg = new RubberGroup(nil, nil);
    float x, y, tx, ty;
    Coord cx = 0, rad = PIN_RAD, dum1 = 0, dum2 = 0;
    ConnectorView* target = views->ConnectorIntersecting(
        e.x-SLOP, e.y-SLOP, e.x+SLOP, e.y+SLOP
    );

    s->Clear();
    if (target != nil) {
        target->GetConnector()->GetCenter(x, y);
        rel->Transform(x, y, tx, ty);
        e.x = round(tx);
        e.y = round(ty);
    }
    if (rel != nil) {
        rel->Transform(cx, dum1);
        rel->Transform(rad, dum2);
        rad = abs(rad - cx);
    }
    rg->Append(
        new RubberLine(nil, nil, e.x, e.y, e.x, e.y),
        new FixedPin(nil, nil, e.x, e.y, rad),
        new SlidingPin(nil, nil, e.x, e.y, rad, e.x, e.y)
    );
    return new ConnectManip(v, rg, rel, tool);
}

Manipulator* LinkView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Coord x0, y0, x1, y1;
    Rubberband* rub = nil;
    Manipulator* m = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        m = CreateLinkCompManip(v, e, rel, tool);

    } else if (tool->IsA(MOVE_TOOL)) {
        GetEndpoints(x0, y0, x1, y1);
	rub = new SlidingLine(nil, nil, x0, y0, x1, y1, e.x, e.y);
        m = new DragManip(v, rub, rel, tool, Gravity);

    } else if (tool->IsA(SCALE_TOOL)) {
        GetEndpoints(x0, y0, x1, y1);
        rub = new ScalingLine(nil, nil, x0, y0, x1, y1, (x0+x1)/2, (y0+y1)/2);
        m = new DragManip(v, rub, rel, tool, Gravity);

    } else if (tool->IsA(ROTATE_TOOL)) {
        GetEndpoints(x0, y0, x1, y1);
        rub = new RotatingLine(
            nil, nil, x0, y0, x1, y1, (x0+x1)/2, (y0+y1)/2, e.x, e.y
        );
        m = new DragManip(v, rub, rel, tool, Gravity);
    }
    return m;
}

Command* LinkView::InterpLinkCompManip (Manipulator* m) {
    Viewer* v = m->GetViewer();
    Editor* ed = v->GetEditor();
    GraphicView* views = v->GetGraphicView();
    BrushVar* brVar = (BrushVar*) ed->GetState("BrushVar");
    ConnectManip* cm = (ConnectManip*) m;
    Transformer* rel = cm->GetTransformer();
    RubberGroup* rg = (RubberGroup*) cm->GetRubberband();
    RubberLine* rl = (RubberLine*) rg->First();
    Coord x0, y0, x1, y1;
    Connector* c1, *c2;
    ConnectorView* target1, *target2;
    MacroCmd* macro = new MacroCmd(ed);
    
    rl->GetCurrent(x0, y0, x1, y1);
    if (rel != nil) {
        rel = new Transformer(rel);
        rel->Invert();
    }

    Graphic* pg = GetGraphicComp()->GetGraphic();
    Line* line = new Line(x0, y0, x1, y1, pg);

    if (brVar != nil) line->SetBrush(brVar->GetBrush());
    line->SetTransformer(rel);
    Unref(rel);
    LinkComp* linkComp = NewSubject(line);
    linkComp->GetConnectors(c1, c2);

    macro->Append(new PasteCmd(ed, new Clipboard(linkComp)));
    target1 = views->ConnectorIntersecting(x0-SLOP, y0-SLOP, x0+SLOP, y0+SLOP);
    target2 = views->ConnectorIntersecting(x1-SLOP, y1-SLOP, x1+SLOP, y1+SLOP);

    if (target1 != nil) {
        macro->Append(new ConnectCmd(ed, c1, target1->GetConnector()));
    }
    if (target2 != nil) {
        macro->Append(new ConnectCmd(ed, c2, target2->GetConnector()));
    }
    return macro;
}

LinkComp* LinkView::NewSubject (Line* line) { return new LinkComp(line); }

Command* LinkView::InterpretManipulator (Manipulator* m) {
    DragManip* dm = (DragManip*) m;
    Editor* ed = dm->GetViewer()->GetEditor();
    Tool* tool = dm->GetTool();
    Transformer* rel = dm->GetTransformer();
    Command* cmd = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        cmd = InterpLinkCompManip(dm);

    } else if (tool->IsA(MOVE_TOOL)) {
        SlidingLine* sl;
        Coord x0, y0, x1, y1, dummy1, dummy2;
        float fx0, fy0, fx1, fy1;

        sl = (SlidingLine*) dm->GetRubberband();
        sl->GetOriginal(x0, y0, dummy1, dummy2);
        sl->GetCurrent(x1, y1, dummy1, dummy2);
        if (rel != nil) {
            rel->InvTransform(float(x0), float(y0), fx0, fy0);
            rel->InvTransform(float(x1), float(y1), fx1, fy1);
        }
        cmd = new MoveCmd(ed, fx1-fx0, fy1-fy0);

    } else if (tool->IsA(SCALE_TOOL)) {
        ScalingLine* sl = (ScalingLine*) dm->GetRubberband();
        float sxy = sl->CurrentScaling();

        cmd = new ScaleCmd(ed, sxy, sxy);

    } else if (tool->IsA(ROTATE_TOOL)) {
        RotatingLine* rl = (RotatingLine*) dm->GetRubberband();
        float angle = rl->CurrentAngle() - rl->OriginalAngle();

        cmd = new RotateCmd(ed, angle);
    }
    return cmd;
}

void LinkView::First (Iterator& i) { i.SetValue(_connView1); }
void LinkView::Last (Iterator& i) { i.SetValue(_connView2); }
boolean LinkView::Done (Iterator i) { return i.GetValue() == nil; }

void LinkView::Next (Iterator& i) { 
    void* v = i.GetValue();

    if (v == nil) {
        i.SetValue(_connView1);

    } else if (v == _connView1) {
        i.SetValue(_connView2);

    } else {
        i.SetValue(nil);
    }
}

void LinkView::Prev (Iterator& i) { 
    void* v = i.GetValue();

    if (v == nil) {
        i.SetValue(_connView2);

    } else if (v == _connView1) {
        i.SetValue(nil);

    } else {
        i.SetValue(_connView1);
    }
}

GraphicView* LinkView::GetView (Iterator i) { 
    return (GraphicView*) i.GetValue(); 
}

void LinkView::SetView (GraphicView* gv, Iterator& i) { i.SetValue(gv); }

void LinkView::GetEndpoints (Coord& x0, Coord& y0, Coord& x1, Coord& y1) {
    Line* line = GetLine();
    Transformer t;

    line->GetOriginal(x0, y0, x1, y1);
    line->TotalTransformation(t);
    t.Transform(x0, y0);
    t.Transform(x1, y1);
}

Line* LinkView::GetLine () {
    Iterator i;
    Graphic* gr = GetGraphic();

    gr->First(i);
    return (Line*) gr->GetGraphic(i);
}

Graphic* LinkView::GetGraphic () {
    Graphic* gr = GraphicView::GetGraphic();

    if (gr == nil) {
        LinkComp* linkComp = GetLinkComp();
        gr = new Picture(linkComp->GetGraphic());
        gr->Append(linkComp->GetLine()->Copy());
        SetGraphic(gr);

        Connector* c1, *c2;
        linkComp->GetConnectors(c1, c2);
        _connView1 = (ConnectorView*) c1->Create(COMPONENT_VIEW);
        _connView2 = (ConnectorView*) c2->Create(COMPONENT_VIEW);

        c1->Attach(_connView1);
        c2->Attach(_connView2);
        _connView1->Update();
        _connView2->Update();

        gr->Append(_connView1->GetGraphic(), _connView2->GetGraphic());
    }
    return gr;
}

LinkView::~LinkView () {
    Graphic* parent = GraphicView::GetGraphic();
    Graphic* g1 = _connView1->GetGraphic();
    Graphic* g2 = _connView2->GetGraphic();

    parent->Remove(g1);
    parent->Remove(g2);

    delete _connView1;
    delete _connView2;
}

/****************************************************************************/

ClassId PSLink::GetClassId () { return PS_LINK; }

boolean PSLink::IsA (ClassId id) { 
    return PS_LINK == id || PostScriptView::IsA(id);
}

PSLink::PSLink (LinkComp* subj) : PostScriptView(subj) { }

boolean PSLink::Definition (ostream& out) {
    LinkComp* comp = (LinkComp*) GetSubject();
    Graphic* link = comp->GetGraphic();
    Line* line = comp->GetLine();

    Transformer* link_t = link->GetTransformer();
    Transformer* line_t = line->GetTransformer();
    Transformer* temp_t = new Transformer(line_t);

    Resource::ref(link_t);
    temp_t->postmultiply(*link_t);
    link->SetTransformer(temp_t);

    Coord x0, y0, x1, y1;
    line->GetOriginal(x0, y0, x1, y1);

    out << "Begin " << MARK << " Line\n";
    MinGS(out);
    out << MARK << "\n";
    out << x0 << " " << y0 << " " << x1 << " " << y1 << " Line\n";
    out << "End\n\n";

    link->SetTransformer(link_t);
    Resource::unref(link_t);
    Resource::unref(temp_t);

    return out.good();
}
