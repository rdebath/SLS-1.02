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
 * Pad component definitions.
 */

#include <Unidraw/classes.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/manips.h>
#include <Unidraw/selection.h>
#include <Unidraw/statevars.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/align.h>
#include <Unidraw/Commands/edit.h>

#include <Unidraw/Components/cglue.h>
#include <Unidraw/Components/csolver.h>
#include <Unidraw/Components/pad.h>
#include <Unidraw/Components/slot.h>

#include <Unidraw/Tools/tool.h>

#include <InterViews/event.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/rubgroup.h>
#include <IV-2_6/InterViews/rubline.h>
#include <IV-2_6/InterViews/rubrect.h>
#include <IV-2_6/InterViews/shape.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

#include <stream.h>

/*****************************************************************************/

ClassId PadComp::GetClassId () { return PAD_COMP; }

boolean PadComp::IsA (ClassId id) {
    return PAD_COMP == id || Connector::IsA(id);
}

Component* PadComp::Copy () {
    PadComp* copy = new PadComp((PadGraphic*) GetGraphic()->Copy());
    copy->_mobility = _mobility;
    return copy;
}

PadComp::PadComp (PadGraphic* graphic) : Connector(graphic) {
    _mobility = Fixed;
}

void PadComp::Interpret (Command* cmd) {
    if (
        cmd->IsA(MOVE_CMD) || cmd->IsA(BRUSH_CMD) || 
        cmd->IsA(ALIGN_CMD) || cmd->IsA(MOBILITY_CMD) ||
        cmd->IsA(DELETE_CMD) || cmd->IsA(CUT_CMD) ||
        cmd->IsA(GROUP_CMD) || cmd->IsA(FRONT_CMD) || cmd->IsA(BACK_CMD)
    ) {
        Connector::Interpret(cmd);
    }
}

void PadComp::Uninterpret (Command* cmd) {
    if (
        cmd->IsA(MOVE_CMD) || cmd->IsA(BRUSH_CMD) || 
        cmd->IsA(ALIGN_CMD) || cmd->IsA(MOBILITY_CMD) ||
        cmd->IsA(DELETE_CMD) || cmd->IsA(CUT_CMD) ||
        cmd->IsA(GROUP_CMD) || cmd->IsA(FRONT_CMD) || cmd->IsA(BACK_CMD)
    ) {
        Connector::Uninterpret(cmd);
    } 
}

void PadComp::Connect (Connector* target, CGlue* g) {
    PadGraphic* padgr = GetPad();
    float l, b, r, t;
    padgr->GetBounds(l, b, r, t);
    float h = (r - l)/2;
    float v = (t - b)/2;

    if (target->IsA(PIN_COMP)) {
        CGlue padGlue(0, 0, h*hfil, h*hfil, v*vfil, v*vfil, h, h, v, v);
        padGlue.Interpose(g);
        csolver->Connect(this, target, &padGlue);
        Connector::Connect(target, &padGlue);

    } else if (target->IsA(HSLOT_COMP)) {
        Graphic* peergr = target->GetGraphic();
        peergr->GetBounds(l, b, r, t);
        h += (r - l)/2;
        CGlue slotGlue(0, 0, h*hfil, h*hfil, v*vfil, v*vfil, h, h, v, v);

        slotGlue.Interpose(g);
        csolver->Connect(this, target, &slotGlue);
        Connector::Connect(target, &slotGlue);

    } else if (target->IsA(VSLOT_COMP)) {
        Graphic* peergr = target->GetGraphic();
        peergr->GetBounds(l, b, r, t);
        v += (t - b)/2;
        CGlue slotGlue(0, 0, h*hfil, h*hfil, v*vfil, v*vfil, h, h, v, v);

        slotGlue.Interpose(g);
        csolver->Connect(this, target, &slotGlue);
        Connector::Connect(target, &slotGlue);

    } else if (target->IsA(PAD_COMP)) {
        Graphic* peergr = target->GetGraphic();
        peergr->GetBounds(l, b, r, t);
        h += (r - l)/2;
        v += (t - b)/2;
        CGlue padGlue(0, 0, h*hfil, h*hfil, v*vfil, v*vfil, h, h, v, v);

        padGlue.Interpose(g);
        csolver->Connect(this, target, &padGlue);
        Connector::Connect(target, &padGlue);
    }
}

void PadComp::SetMobility (Mobility m) { _mobility = m; }
Mobility PadComp::GetMobility () { return _mobility; }
PadGraphic* PadComp::GetPad () { return (PadGraphic*) GetGraphic(); }

void PadComp::Read (istream& in) {
    Connector::Read(in);
    Coord l, b, r, t;
    int mobility;

    in >> l >> b >> r >> t >> mobility;
    PadGraphic* pad = new PadGraphic(l, b, r, t);
    _mobility = Mobility(mobility);
    
    pad->FillBg(ReadBgFilled(in));
    PSColor* fg = ReadColor(in);
    PSColor* bg = ReadColor(in);
    pad->SetColors(fg, bg);
    pad->SetBrush(ReadBrush(in));

    Transformer* xf = ReadTransformer(in);
    pad->SetTransformer(xf);
    Unref(xf);

    SetGraphic(pad);
}

void PadComp::Write (ostream& out) {
    Connector::Write(out);
    PadGraphic* pad = GetPad();
    Coord l, b, r, t;
    int mobility = _mobility;

    pad->GetOriginal(l, b, r, t);
    out << l << " " << b << " " << r << " " << t << " " << mobility << " ";

    WriteBgFilled(pad->BgFilled(), out);
    WriteColor(pad->GetFgColor(), out);
    WriteColor(pad->GetBgColor(), out);
    WriteBrush(pad->GetBrush(), out);
    WriteTransformer(pad->GetTransformer(), out);
}

/*****************************************************************************/

PadComp* PadView::GetPadComp () { return (PadComp*) GetSubject(); }
ClassId PadView::GetClassId () { return PAD_VIEW; }

boolean PadView::IsA (ClassId id) {
    return PAD_VIEW == id || ConnectorView::IsA(id);
}

PadView::PadView (PadComp* subj) : ConnectorView(subj) { }
PadGraphic* PadView::GetPad () { return (PadGraphic*) GetGraphic(); }

void PadView::Interpret (Command* cmd) {
    if (cmd->IsA(ALIGNTOGRID_CMD)) {
        PadGraphic* padg = (PadGraphic*) GetGraphic();
        Transformer total;
        padg->TotalTransformation(total);

        Coord x0, y0, x1, y1;
        float tx0, ty0;

        padg->GetOriginal(x0, y0, x1, y1);
        total.Transform(float(x0), float(y0), tx0, ty0);
        ((AlignToGridCmd*) cmd)->Align(this, tx0, ty0);

    } else {
        ConnectorView::Interpret(cmd);
    }
}

void PadView::Update () {
    PadGraphic* pad = GetPad();
    Graphic* padgr = pad;
    
    IncurDamage(pad);
    *padgr = *GetPadComp()->GetGraphic();
    IncurDamage(pad);
    EraseHandles();
}

Manipulator* PadView::CreateGraphicCompManip (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    v->Constrain(e.x, e.y);
    Rubberband* rub = new RubberRect(nil, nil, e.x, e.y, e.x, e.y);
    return new DragManip(v, rub, rel, tool, DragConstraint(XYEqual | Gravity));
}

Manipulator* PadView::CreateConnectManip (
    Viewer* v, Event&, Transformer* rel, Tool* tool
) {
    Selection* s = v->GetSelection();
    RubberGroup* rg = new RubberGroup(nil, nil);
    Coord l, b, r, t;

    s->Clear();
    GetGraphic()->GetBox(l, b, r, t);
    Coord cx = (l+r)/2;
    Coord cy = (b+t)/2;
    rg->Append(
        new SlidingRect(nil, nil, l, b, r, t, cx, cy),
        new SlidingLine(nil, nil, l, b, r, t, cx, cy),
        new SlidingLine(nil, nil, l, t, r, b, cx, cy),
        new RubberLine(nil, nil, cx, cy, cx, cy)
    );
    return new ConnectManip(v, rg, rel, tool);
}

Manipulator* PadView::CreateManipulator (
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

Command* PadView::InterpGraphicCompManip (Manipulator* m) {
    Command* cmd = nil;
    DragManip* dm = (DragManip*) m;
    SlidingRect* sr = (SlidingRect*) dm->GetRubberband();
    Coord l, b, r, t;
    sr->GetCurrent(l, b, r, t);

    if (l != r || b != t) {
        DragManip* dm = (DragManip*) m;
        Editor* ed = dm->GetViewer()->GetEditor();
        BrushVar* brVar = (BrushVar*) ed->GetState("Brush");
        Transformer* rel = dm->GetTransformer();

        if (rel != nil) {
            rel = new Transformer(rel);
            rel->Invert();
        }

        Graphic* pg = GetGraphicComp()->GetGraphic();
        PadGraphic* padGraphic = new PadGraphic(l, b, r, t, pg);

        if (brVar != nil) padGraphic->SetBrush(brVar->GetBrush());

        padGraphic->SetTransformer(rel);
        Unref(rel);
        cmd = new PasteCmd(ed, new Clipboard(NewSubject(padGraphic)));
    }
    return cmd;
}

PadComp* PadView::NewSubject (PadGraphic* padgr) { return new PadComp(padgr); }

Command* PadView::InterpConnectManip (Manipulator* m) {
    Editor* ed = m->GetViewer()->GetEditor();
    ConnectManip* cm = (ConnectManip*) m;
    ConnectorView* target = cm->GetTarget();
    Command* cmd = nil;

    if (target != nil) {
        cmd = new ConnectCmd(ed, GetConnector(), target->GetConnector());
    }
    return cmd;
}

Command* PadView::InterpretManipulator (Manipulator* m) {
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

Graphic* PadView::GetGraphic () {
    Graphic* graphic = GraphicView::GetGraphic();

    if (graphic == nil) {
        PadComp* padComp = GetPadComp();
        graphic = padComp->GetGraphic()->Copy();
        SetGraphic(graphic);
    }
    return graphic;
}

/*****************************************************************************/

PadGraphic::PadGraphic (
    Coord l, Coord b, Coord r, Coord t, Graphic* gr
) : Graphic(gr) {
    _l = l;
    _b = b;
    _r = r;
    _t = t;
    _br = nil;

    if (gr != nil) {
        PadGraphic::SetBrush(gr->GetBrush());
    }
}

PadGraphic::~PadGraphic () { Unref(_br); }
Graphic* PadGraphic::Copy () { return new PadGraphic(_l, _b, _r, _t, this); }

void PadGraphic::GetOriginal (Coord& l, Coord& b, Coord& r, Coord& t) {
    l = _l;
    b = _b;
    r = _r;
    t = _t;
}

void PadGraphic::SetBrush (PSBrush* br) {
    if (_br != br) {
        Ref(br);
        Unref(_br);
        _br = br;
        invalidateCaches();
    }
}

PSBrush* PadGraphic::GetBrush () { return _br; }

void PadGraphic::concatGS (Graphic* g1, Graphic* g2, Graphic* dest) {
    Graphic::concatGS(g1, g2, dest);
    dest->SetBrush(GetBrush());
}

void PadGraphic::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    float fr, ft;

    transformRect(float(_l), float(_b), float(_r), float(_t), l, b, fr, ft,gs);
    cx = (l + fr)/2;
    cy = (b + ft)/2;
    tol = 0;
}

void PadGraphic::draw (Canvas* c, Graphic* gs) {
    if (!gs->GetBrush()->None()) {
        update(gs);
        _p->Rect(c, _l, _b, _r, _t);
        _p->Line(c, _l, _b, _r, _t);
        _p->Line(c, _l, _t, _r, _b);
    }
}

/**************************************************************************/

ClassId PSPad::GetClassId () { return PS_PAD; }
boolean PSPad::IsA (ClassId id) {return PS_PAD==id || PostScriptView::IsA(id);}

PSPad::PSPad (PadComp* subj) : PostScriptView(subj) { }

boolean PSPad::Definition (ostream&) {
    // unimplemented

    return true;
}
