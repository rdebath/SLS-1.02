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
 * Pin component definitions.
 */

#include <Unidraw/classes.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/manips.h>
#include <Unidraw/selection.h>
#include <Unidraw/statevars.h>
#include <Unidraw/ulist.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/align.h>
#include <Unidraw/Commands/edit.h>

#include <Unidraw/Components/cglue.h>
#include <Unidraw/Components/csolver.h>
#include <Unidraw/Components/pad.h>
#include <Unidraw/Components/pin.h>
#include <Unidraw/Components/slot.h>

#include <Unidraw/Graphic/lines.h>
#include <Unidraw/Graphic/picture.h>
#include <Unidraw/Graphic/util.h>

#include <Unidraw/Tools/tool.h>

#include <InterViews/event.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/rubgroup.h>
#include <IV-2_6/InterViews/rubline.h>
#include <IV-2_6/InterViews/shape.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

#include <math.h>
#include <stdlib.h>
#include <stream.h>

/*****************************************************************************/

ClassId PinComp::GetClassId () { return PIN_COMP; }

boolean PinComp::IsA (ClassId id) {
    return PIN_COMP == id || Connector::IsA(id);
}

Component* PinComp::Copy () {
    PinComp* copy = new PinComp((PinGraphic*) GetGraphic()->Copy());
    copy->_mobility = _mobility;
    return copy;
}

PinComp::PinComp (PinGraphic* graphic) : Connector(graphic) {
    _mobility = Fixed;
}

void PinComp::Interpret (Command* cmd) {
    if (
        cmd->IsA(MOVE_CMD) || cmd->IsA(BRUSH_CMD) || 
        cmd->IsA(ALIGN_CMD) || cmd->IsA(MOBILITY_CMD) ||
        cmd->IsA(DELETE_CMD) || cmd->IsA(CUT_CMD) ||
        cmd->IsA(GROUP_CMD) || cmd->IsA(FRONT_CMD) || cmd->IsA(BACK_CMD)
    ) {
        Connector::Interpret(cmd);
    }
}

void PinComp::Uninterpret (Command* cmd) {
    if (
        cmd->IsA(MOVE_CMD) || cmd->IsA(BRUSH_CMD) || 
        cmd->IsA(ALIGN_CMD) || cmd->IsA(MOBILITY_CMD) ||
        cmd->IsA(DELETE_CMD) || cmd->IsA(CUT_CMD) ||
        cmd->IsA(GROUP_CMD) || cmd->IsA(FRONT_CMD) || cmd->IsA(BACK_CMD)
    ) {
        Connector::Uninterpret(cmd);
    } 
}

void PinComp::Connect (Connector* target, CGlue* g) {
    float l, b, r, t;

    if (target->IsA(PIN_COMP)) {
        csolver->Connect(this, target, g);
        Connector::Connect(target, g);

    } else if (target->IsA(HSLOT_COMP)) {
        Graphic* peergr = target->GetGraphic();
        peergr->GetBounds(l, b, r, t);
        float h = (r - l)/2;
        CGlue slotGlue(0, 0, h*hfil, h*hfil, 0, 0, h, h, 0, 0);
        slotGlue.Interpose(g);
        csolver->Connect(this, target, &slotGlue);
        Connector::Connect(target, &slotGlue);

    } else if (target->IsA(VSLOT_COMP)) {
        Graphic* peergr = target->GetGraphic();
        peergr->GetBounds(l, b, r, t);
        float v = (t - b)/2;
        CGlue slotGlue(0, 0, 0, 0, v*vfil, v*vfil, 0, 0, v, v);
        slotGlue.Interpose(g);
        csolver->Connect(this, target, &slotGlue);
        Connector::Connect(target, &slotGlue);

    } else if (target->IsA(PAD_COMP)) {
        Graphic* peergr = target->GetGraphic();
        peergr->GetBounds(l, b, r, t);
        float h = (r - l)/2;
        float v = (t - b)/2;
        CGlue padGlue(0, 0, h*hfil, h*hfil, v*vfil, v*vfil, h, h, v, v);

        padGlue.Interpose(g);
        csolver->Connect(this, target, &padGlue);
        Connector::Connect(target, &padGlue);
    }
}

void PinComp::SetMobility (Mobility m) { _mobility = m; }
Mobility PinComp::GetMobility () { return _mobility; }
PinGraphic* PinComp::GetPin () { return (PinGraphic*) GetGraphic(); }

void PinComp::Read (istream& in) {
    Connector::Read(in);
    Coord x0, y0;
    int mobility;

    in >> x0 >> y0 >> mobility;
    PinGraphic* pin = new PinGraphic(x0, y0);
    _mobility = Mobility(mobility);
    
    pin->FillBg(ReadBgFilled(in));
    PSColor* fg = ReadColor(in);
    PSColor* bg = ReadColor(in);
    pin->SetColors(fg, bg);
    pin->SetBrush(ReadBrush(in));

    Transformer* t = ReadTransformer(in);
    pin->SetTransformer(t);
    Unref(t);

    SetGraphic(pin);
}

void PinComp::Write (ostream& out) {
    Connector::Write(out);
    PinGraphic* pin = GetPin();
    Coord x0, y0;
    int mobility = _mobility;

    pin->GetOriginal(x0, y0);
    out << x0 << " " << y0 << " " << mobility << " ";

    WriteBgFilled(pin->BgFilled(), out);
    WriteColor(pin->GetFgColor(), out);
    WriteColor(pin->GetBgColor(), out);
    WriteBrush(pin->GetBrush(), out);
    WriteTransformer(pin->GetTransformer(), out);
}

/*****************************************************************************/

PinComp* PinView::GetPinComp () { return (PinComp*) GetSubject(); }
ClassId PinView::GetClassId () { return PIN_VIEW; }

boolean PinView::IsA (ClassId id) {
    return PIN_VIEW == id || ConnectorView::IsA(id);
}

PinView::PinView (PinComp* subj) : ConnectorView(subj) { }
PinGraphic* PinView::GetPin () { return (PinGraphic*) GetGraphic(); }

void PinView::Interpret (Command* cmd) {
    if (cmd->IsA(ALIGNTOGRID_CMD)) {
        float cx, cy;
        GetGraphic()->GetCenter(cx, cy);
        ((AlignToGridCmd*) cmd)->Align(this, cx, cy);

    } else {
        ConnectorView::Interpret(cmd);
    }
}

void PinView::Update () {
    PinGraphic* pin = GetPin();
    Graphic* pingr = pin;
    
    IncurDamage(pin);
    *pingr = *GetPinComp()->GetGraphic();
    IncurDamage(pin);
    EraseHandles();
}

Manipulator* PinView::CreateGraphicCompManip (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    GraphicView* views = v->GetGraphicView();
    Selection* s = v->GetSelection();
    SlidingPin* sp;
    Coord cx = 0, rad = PIN_RAD, dum1 = 0, dum2 = 0;

    s->Clear();
    if (rel != nil) {
        rel->Transform(cx, dum1);
        rel->Transform(rad, dum2);
        rad = abs(rad - cx);
    }
    v->Constrain(e.x, e.y);
    sp = new SlidingPin(nil, nil, e.x, e.y, rad, e.x, e.y);
    return new DragManip(v, sp, rel, tool, Gravity);
}

Manipulator* PinView::CreateConnectManip (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    GraphicView* views = v->GetGraphicView();
    Selection* s = v->GetSelection();
    RubberGroup* rg = new RubberGroup(nil, nil);
    Coord cx = 0, rad = PIN_RAD, dum1 = 0, dum2 = 0;

    s->Clear();
    if (rel != nil) {
        rel->Transform(cx, dum1);
        rel->Transform(rad, dum2);
        rad = abs(rad - cx);
    }
    rg->Append(
        new SlidingPin(nil, nil, e.x, e.y, rad, e.x, e.y),
        new RubberLine(nil, nil, e.x, e.y, e.x, e.y)
    );
    return new ConnectManip(v, rg, rel, tool);
}

Manipulator* PinView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Manipulator* manip = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        manip = CreateGraphicCompManip(v, e, rel, tool);

    } else if (tool->IsA(MOVE_TOOL)) {
        manip = GraphicView::CreateManipulator(v, e, rel, tool);

    } else if (tool->IsA(CONNECT_TOOL)) {
        manip = CreateConnectManip(v, e, rel, tool);
    }
    return manip;
}

inline void GetOffset (Transformer* t, Coord x, Coord y, float& dx, float& dy){
    t->InvTransform(float(x), float(y), dx, dy);
    dx -= x;
    dy -= y;
}

Command* PinView::InterpGraphicCompManip (Manipulator* m) {
    DragManip* dm = (DragManip*) m;
    Editor* ed = dm->GetViewer()->GetEditor();
    BrushVar* brVar = (BrushVar*) ed->GetState("Brush");
    SlidingPin* sp = (SlidingPin*) dm->GetRubberband();
    Transformer* rel = dm->GetTransformer();
    Coord px, py, dum;
    float dx, dy;
    PinGraphic* pinGraphic;

    sp->GetCurrent(px, py, dum, dum);
    if (rel != nil) {
        GetOffset(rel, px, py, dx, dy);
        rel = new Transformer;
        rel->Translate(dx, dy);
    }

    Graphic* pg = GetGraphicComp()->GetGraphic();
    pinGraphic = new PinGraphic(px, py, pg);

    if (brVar != nil) pinGraphic->SetBrush(brVar->GetBrush());
    pinGraphic->SetTransformer(rel);
    Unref(rel);
    return new PasteCmd(ed, new Clipboard(NewSubject(pinGraphic)));
}

PinComp* PinView::NewSubject (PinGraphic* pingr) { return new PinComp(pingr); }

Command* PinView::InterpConnectManip (Manipulator* m) {
    Editor* ed = m->GetViewer()->GetEditor();
    ConnectManip* cm = (ConnectManip*) m;
    ConnectorView* target = cm->GetTarget();
    Command* cmd = nil;

    if (target != nil) {
        cmd = new ConnectCmd(ed, GetConnector(), target->GetConnector());
    }
    return cmd;
}

Command* PinView::InterpretManipulator (Manipulator* m) {
    DragManip* dm = (DragManip*) m;
    Tool* tool = dm->GetTool();
    Command* cmd = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        cmd = InterpGraphicCompManip(m);
    } else if (tool->IsA(MOVE_TOOL)) {
        cmd = GraphicView::InterpretManipulator(m);
    } else if (tool->IsA(CONNECT_TOOL)) {
        cmd = InterpConnectManip(m);
    }
    return cmd;
}

Graphic* PinView::GetGraphic () {
    Graphic* graphic = GraphicView::GetGraphic();

    if (graphic == nil) {
        PinComp* pinComp = GetPinComp();
        graphic = pinComp->GetGraphic()->Copy();
        SetGraphic(graphic);
    }
    return graphic;
}

/*****************************************************************************/

PinGraphic::PinGraphic (Coord x, Coord y, Graphic* gr) : Graphic(gr) {
    _x = x;
    _y = y;
    _br = nil;

    if (gr != nil) {
        PinGraphic::SetBrush(gr->GetBrush());
    }
}

PinGraphic::~PinGraphic () { Unref(_br); }
Graphic* PinGraphic::Copy () { return new PinGraphic(_x, _y, this); }

void PinGraphic::GetOriginal (Coord& x, Coord& y) {
    x = _x;
    y = _y;
}

void PinGraphic::SetBrush (PSBrush* br) {
    if (_br != br) {
        Ref(br);
        Unref(_br);
        _br = br;
        invalidateCaches();
    }
}

PSBrush* PinGraphic::GetBrush () { return _br; }

void PinGraphic::concatGS (Graphic* g1, Graphic* g2, Graphic* dest) {
    Graphic::concatGS(g1, g2, dest);
    dest->SetBrush(GetBrush());
}

void PinGraphic::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    float fx = float(_x), fy = float(_y), rx, ry, rad;

    transform(fx - PIN_RAD, fy, rx, ry, gs);
    transform(fx, fy, cx, cy, gs);
    rad = sqrt(square(rx - cx) + square(ry - cy));
    l = cx - rad;
    b = cy - rad;
    tol = 0;
}

void PinGraphic::draw (Canvas* c, Graphic* gs) {
    if (!gs->GetBrush()->None()) {
        update(gs);
        _p->Circle(c, _x, _y, PIN_RAD);
        _p->Line(c, _x, _y - PIN_RAD, _x, _y + PIN_RAD);
        _p->Line(c, _x - PIN_RAD, _y, _x + PIN_RAD, _y);
    }
}

/**************************************************************************/

ClassId PSPin::GetClassId () { return PS_PIN; }
boolean PSPin::IsA (ClassId id) {return PS_PIN==id || PostScriptView::IsA(id);}

PSPin::PSPin (PinComp* subj) : PostScriptView(subj) { }

boolean PSPin::Definition (ostream&) {
    // unimplemented

    return true;
}

/**************************************************************************/

SlidingPin::SlidingPin (
    Painter* p, Canvas* c, Coord cx, Coord cy, int r, Coord rfx, Coord rfy
) : SlidingEllipse(p, c, cx, cy, r, r, rfx, rfy) { }

void SlidingPin::Draw () {
    Coord cx, cy, rx, ry, xr, yr;

    if (!drawn) {
        GetCurrent(cx, cy, rx, ry);
        CurrentRadii(xr, yr);
        cx += offx;
        cy += offy;
        output->Ellipse(canvas, cx, cy, xr, yr);
        output->Line(canvas, cx, cy - yr, cx, cy + yr);
        output->Line(canvas, cx - xr, cy, cx + xr, cy);
        drawn = true;
    }
}

/**************************************************************************/

FixedPin::FixedPin (
    Painter* p, Canvas* c, Coord x, Coord y, int r
) : Rubberband(p, c, 0, 0) { 
    _cx = x;
    _cy = y;
    _rad = r;
}

void FixedPin::GetOriginal (Coord& x, Coord& y, int& r) {
    x = _cx;
    y = _cy;
    r = _rad;
}

void FixedPin::Draw () {
    if (!drawn) {
        output->Circle(canvas, _cx, _cy, _rad);
        output->Line(canvas, _cx, _cy - _rad, _cx, _cy + _rad);
        output->Line(canvas, _cx - _rad, _cy, _cx + _rad, _cy);
        drawn = true;
    }
}
