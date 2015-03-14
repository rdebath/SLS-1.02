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
 * Slot component definitions.
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
#include <Unidraw/Components/slot.h>

#include <Unidraw/Graphic/lines.h>
#include <Unidraw/Graphic/util.h>

#include <Unidraw/Tools/tool.h>

#include <InterViews/event.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/rubgroup.h>
#include <IV-2_6/InterViews/rubline.h>
#include <IV-2_6/InterViews/rubrect.h>
#include <IV-2_6/InterViews/shape.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

#include <math.h>
#include <stdlib.h>
#include <stream.h>

/*****************************************************************************/

ClassId SlotComp::GetClassId () { return SLOT_COMP; }

boolean SlotComp::IsA (ClassId id) {
    return SLOT_COMP == id || Connector::IsA(id);
}

SlotComp::SlotComp (SlotGraphic* graphic) : Connector(graphic) {
    _mobility = Fixed;
}

void SlotComp::Interpret (Command* cmd) {
    if (
        cmd->IsA(MOVE_CMD) || cmd->IsA(BRUSH_CMD) || 
        cmd->IsA(ALIGN_CMD) || cmd->IsA(MOBILITY_CMD) ||
        cmd->IsA(DELETE_CMD) || cmd->IsA(CUT_CMD) ||
        cmd->IsA(GROUP_CMD) || cmd->IsA(FRONT_CMD) || cmd->IsA(BACK_CMD)
    ) {
        Connector::Interpret(cmd);
    }
}

void SlotComp::Uninterpret (Command* cmd) {
    if (
        cmd->IsA(MOVE_CMD) || cmd->IsA(BRUSH_CMD) || 
        cmd->IsA(ALIGN_CMD) || cmd->IsA(MOBILITY_CMD) ||
        cmd->IsA(DELETE_CMD) || cmd->IsA(CUT_CMD) ||
        cmd->IsA(GROUP_CMD) || cmd->IsA(FRONT_CMD) || cmd->IsA(BACK_CMD)
    ) {
        Connector::Uninterpret(cmd);
    } 
}

void SlotComp::SetMobility (Mobility m) { _mobility = m; }
Mobility SlotComp::GetMobility () { return _mobility; }
SlotGraphic* SlotComp::GetSlot () { return (SlotGraphic*) GetGraphic(); }

void SlotComp::Read (istream& in) {
    Connector::Read(in);
    Coord x0, y0, length;
    int mobility;

    in >> x0 >> y0 >> length >> mobility;
    SlotGraphic* pin = new SlotGraphic(x0, y0, length);
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

void SlotComp::Write (ostream& out) {
    Connector::Write(out);
    SlotGraphic* pin = GetSlot();
    Coord x0, y0, length;
    int mobility = _mobility;

    pin->GetOriginal(x0, y0, length);
    out << x0 << " " << y0 << " " << length << " " << mobility << " ";

    WriteBgFilled(pin->BgFilled(), out);
    WriteColor(pin->GetFgColor(), out);
    WriteColor(pin->GetBgColor(), out);
    WriteBrush(pin->GetBrush(), out);
    WriteTransformer(pin->GetTransformer(), out);
}

void SlotComp::SetOrientation (SlotGraphic* sg, Orientation o) {
    sg->SetOrientation(o);
}

/*****************************************************************************/

SlotComp* SlotView::GetSlotComp () { return (SlotComp*) GetSubject(); }
SlotComp* SlotView::NewSubject (SlotGraphic*) { return nil; }

ClassId SlotView::GetClassId () { return SLOT_VIEW; }

boolean SlotView::IsA (ClassId id) {
    return SLOT_VIEW == id || ConnectorView::IsA(id);
}

SlotView::SlotView (SlotComp* subj) : ConnectorView(subj) { }
SlotGraphic* SlotView::GetSlot () { return (SlotGraphic*) GetGraphic(); }

void SlotView::Interpret (Command* cmd) {
    if (cmd->IsA(ALIGNTOGRID_CMD)) {
        SlotGraphic* slotg = (SlotGraphic*) GetGraphic();
        Transformer total;
        slotg->TotalTransformation(total);

        Coord x0, y0, l;
        float tx0, ty0;

        slotg->GetOriginal(x0, y0, l);
        total.Transform(float(x0), float(y0), tx0, ty0);
        ((AlignToGridCmd*) cmd)->Align(this, tx0, ty0);

    } else {
        ConnectorView::Interpret(cmd);
    }
}

void SlotView::Update () {
    SlotGraphic* slot = GetSlot();
    Graphic* slotgr = slot;
    
    IncurDamage(slot);
    *slotgr = *GetSlotComp()->GetGraphic();
    IncurDamage(slot);
    EraseHandles();
}

Manipulator* SlotView::CreateGraphicCompManip (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    SlotGraphic* sg = (SlotGraphic*) GetGraphic();
    Side side = (sg->GetOrientation() == Horizontal) ? RightSide : BottomSide;
    Selection* s = v->GetSelection();
    Coord cx = 0, rad = PIN_RAD, dum1 = 0, dum2 = 0;
    Coord l, b, r, t;

    v->Constrain(e.x, e.y);
    s->Clear();

    if (rel != nil) {
        rel->Transform(cx, dum1);
        rel->Transform(rad, dum2);
        rad = abs(rad - cx);
    }

    if (side == RightSide) {
        l = r = e.x;
        b = e.y - rad;
        t = e.y + rad;

    } else {
        l = e.x - rad;
        r = e.x + rad;
        b = t = e.y;
    }
    
    StretchingRect* sr = new StretchingRect(nil, nil, l, b, r, t, side);
    return new DragManip(v, sr, rel, tool, Gravity);
}

Manipulator* SlotView::CreateConnectManip (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
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
        new SlidingLine(nil, nil, l, cy, r, cy, cx, cy),
        new SlidingLine(nil, nil, cx, b, cx, t, cx, cy),
        new RubberLine(nil, nil, e.x, e.y, e.x, e.y)
    );
    return new ConnectManip(v, rg, rel, tool);
}

Manipulator* SlotView::CreateManipulator (
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

static void Correct (
    Transformer* t, Coord x, Coord y, Coord& length,
    float& dx, float& dy
) {
    Transformer corr(t);
    corr.Invert();

    corr.Transform(float(x), float(y), dx, dy);
    dx -= x;
    dy -= y;

    float x0 = 0, y0 = 0, x1 = length, y1 = 0;
    corr.TransformRect(x0, y0, x1, y1);
    length = round(sqrt(square(x0-x1) + square(y0-y1)));
}

Command* SlotView::InterpGraphicCompManip (Manipulator* m) {
    Command* cmd = nil;
    DragManip* dm = (DragManip*) m;
    StretchingRect* sr = (StretchingRect*) dm->GetRubberband();
    Coord l, b, r, t;
    sr->GetCurrent(l, b, r, t);

    if (l != r || b != t) {
        SlotGraphic* sg = (SlotGraphic*) GetGraphic();
        Editor* ed = dm->GetViewer()->GetEditor();
        BrushVar* brVar = (BrushVar*) ed->GetState("Brush");
        Transformer* rel = dm->GetTransformer();
        Coord x0, y0, length;

        NormalRect(l, b, r, t);

        if (sg->GetOrientation() == Horizontal) {
            x0 = l;
            y0 = (b + t)/2;
            length = r - l;
        } else {
            x0 = (l + r)/2;
            y0 = b;
            length = t - b;
        }

        if (rel != nil) {
            float dx, dy;
            Correct(rel, x0, y0, length, dx, dy);
            rel = new Transformer;
            rel->Translate(dx, dy);
        }

        Graphic* pg = GetGraphicComp()->GetGraphic();
        SlotGraphic* slotGraphic = new SlotGraphic(x0, y0, length, pg);

        if (brVar != nil) slotGraphic->SetBrush(brVar->GetBrush());
        slotGraphic->SetTransformer(rel);
        Unref(rel);
        cmd = new PasteCmd(ed, new Clipboard(NewSubject(slotGraphic)));
    } 
    return cmd;
}

Command* SlotView::InterpConnectManip (Manipulator* m) {
    Editor* ed = m->GetViewer()->GetEditor();
    ConnectManip* cm = (ConnectManip*) m;
    ConnectorView* target = cm->GetTarget();
    Command* cmd = nil;

    if (target != nil) {
        cmd = new ConnectCmd(ed, GetConnector(), target->GetConnector());
    }
    return cmd;
}

Command* SlotView::InterpretManipulator (Manipulator* m) {
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

Graphic* SlotView::GetGraphic () {
    Graphic* graphic = GraphicView::GetGraphic();

    if (graphic == nil) {
        SlotComp* slotComp = GetSlotComp();
        graphic = slotComp->GetGraphic()->Copy();
        SetGraphic(graphic);
    }
    return graphic;
}

/*****************************************************************************/

SlotGraphic::SlotGraphic (
    Coord x, Coord y, Coord length, Graphic* gr
) : Graphic(gr) {
    _x = x;
    _y = y;
    _length = length;
    _br = nil;

    if (gr != nil) {
        SlotGraphic::SetBrush(gr->GetBrush());
    }
}

SlotGraphic::~SlotGraphic () { Unref(_br); }
Graphic* SlotGraphic::Copy () { return new SlotGraphic(_x, _y, _length, this);}

void SlotGraphic::GetOriginal (Coord& x, Coord& y, Coord& length) {
    x = _x;
    y = _y;
    length = _length;
}

void SlotGraphic::SetBrush (PSBrush* br) {
    if (_br != br) {
        Ref(br);
        Unref(_br);
        _br = br;
        invalidateCaches();
    }
}

PSBrush* SlotGraphic::GetBrush () { return _br; }

void SlotGraphic::concatGS (Graphic* g1, Graphic* g2, Graphic* dest) {
    Graphic::concatGS(g1, g2, dest);
    dest->SetBrush(GetBrush());
}

void SlotGraphic::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    float r, t;
    float x0 = _x, y0 = _y - PIN_RAD, x1 = _x+_length, y1 = _y+PIN_RAD;

    transformRect(x0, y0, x1, y1, l, b, r, t, gs);
    cx = (l + r)/2;
    cy = (b + t)/2;
    tol = 0;
}

void SlotGraphic::draw (Canvas* c, Graphic* gs) {
    if (!gs->GetBrush()->None()) {
        update(gs);
        Coord l = _x, b = _y - PIN_RAD, r = _x+_length, t = _y + PIN_RAD;
        Coord cx = (l+r)/2;
        Coord cy = (b+t)/2;

        _p->Rect(c, l, b, r, t);
        _p->Line(c, l, cy, r, cy);
        _p->Line(c, cx, b, cx, t);
    }
}

Orientation SlotGraphic::GetOrientation () {
    Transformer* t = GetTransformer();

    return (t == nil || !t->Rotated90()) ? Horizontal : Vertical;
}

void SlotGraphic::SetOrientation (Orientation newOrient) {
    Orientation curOrient = GetOrientation();

    if (newOrient != curOrient) {
        Transformer* rot = new Transformer, *t = GetTransformer();
        rot->Translate(-_x, -_y);
        rot->Rotate((newOrient == Vertical) ? 90 : -90);
        rot->Translate(_x, _y);

        if (t != nil) {
            rot->Postmultiply(t);
        }
        SetTransformer(rot);
        Unref(rot);
    }
}

/**************************************************************************/

HSlotComp::HSlotComp (SlotGraphic* sg) : SlotComp(sg) {
    if (sg != nil) {
        SetOrientation(sg, Horizontal);
    }
}

Component* HSlotComp::Copy () {
    HSlotComp* copy = new HSlotComp((SlotGraphic*) GetGraphic()->Copy());
    copy->_mobility = _mobility;
    return copy;
}

void HSlotComp::Connect (Connector* target, CGlue* g) {
    SlotGraphic* slotgr = GetSlot();
    float l, b, r, t;
    slotgr->GetBounds(l, b, r, t);
    float h = (r - l)/2;

    if (target->IsA(PIN_COMP)) {
        CGlue slotGlue(0, 0, h*hfil, h*hfil, 0, 0, h, h, 0, 0);
        slotGlue.Interpose(g);
        csolver->Connect(this, target, &slotGlue);
        Connector::Connect(target, &slotGlue);

    } else if (target->IsA(HSLOT_COMP)) {
        Graphic* peergr = target->GetGraphic();
        peergr->GetBounds(l, b, r, t);
        h += (r - l)/2 ;
        CGlue slotGlue(0, 0, h*hfil, h*hfil, 0, 0, h, h, 0, 0);
        slotGlue.Interpose(g);
        csolver->Connect(this, target, &slotGlue);
        Connector::Connect(target, &slotGlue);

    } else if (target->IsA(VSLOT_COMP)) {
        Graphic* peergr = target->GetGraphic();
        peergr->GetBounds(l, b, r, t);
        float v = (t - b)/2;
        CGlue slotGlue(0, 0, h*hfil, h*hfil, v*vfil, v*vfil, h, h, v, v);
        slotGlue.Interpose(g);
        csolver->Connect(this, target, &slotGlue);
        Connector::Connect(target, &slotGlue);

    } else if (target->IsA(PAD_COMP)) {
        Graphic* peergr = target->GetGraphic();
        peergr->GetBounds(l, b, r, t);
        h += (r - l)/2;
        float v = (t - b)/2;
        CGlue slotGlue(0, 0, h*hfil, h*hfil, v*vfil, v*vfil, h, h, v, v);

        slotGlue.Interpose(g);
        csolver->Connect(this, target, &slotGlue);
        Connector::Connect(target, &slotGlue);
    }
}

ClassId HSlotComp::GetClassId () { return HSLOT_COMP; }
boolean HSlotComp::IsA (ClassId id) {return HSLOT_COMP==id||SlotComp::IsA(id);}

/**************************************************************************/

HSlotComp* HSlotView::GetHSlotComp () { return (HSlotComp*) GetSubject(); }
HSlotView::HSlotView (HSlotComp* subj) : SlotView(subj) { }
ClassId HSlotView::GetClassId () { return HSLOT_VIEW; }
boolean HSlotView::IsA (ClassId id) {return HSLOT_VIEW==id||SlotView::IsA(id);}

SlotComp* HSlotView::NewSubject (SlotGraphic* slotgr) {
    return new HSlotComp(slotgr);
}

/**************************************************************************/

VSlotComp::VSlotComp (SlotGraphic* sg) : SlotComp(sg) {
    if (sg != nil) {
        SetOrientation(sg, Vertical);
    }
}

Component* VSlotComp::Copy () {
    VSlotComp* copy = new VSlotComp((SlotGraphic*) GetGraphic()->Copy());
    copy->_mobility = _mobility;
    return copy;
}

void VSlotComp::Connect (Connector* target, CGlue* g) {
    SlotGraphic* slotgr = GetSlot();
    float l, b, r, t;
    slotgr->GetBounds(l, b, r, t);
    float v = (t - b)/2;

    if (target->IsA(PIN_COMP)) {
        CGlue slotGlue(0, 0, 0, 0, v*vfil, v*vfil, 0, 0, v, v);
        slotGlue.Interpose(g);
        csolver->Connect(this, target, &slotGlue);
        Connector::Connect(target, &slotGlue);

    } else if (target->IsA(HSLOT_COMP)) {
        Graphic* peergr = target->GetGraphic();
        peergr->GetBounds(l, b, r, t);
        float h = (r - l)/2;
        CGlue slotGlue(0, 0, h*hfil, h*hfil, v*vfil, v*vfil, h, h, v, v);
        slotGlue.Interpose(g);
        csolver->Connect(this, target, &slotGlue);
        Connector::Connect(target, &slotGlue);

    } else if (target->IsA(VSLOT_COMP)) {
        Graphic* peergr = target->GetGraphic();
        peergr->GetBounds(l, b, r, t);
        v += (t - b)/2;
        CGlue slotGlue(0, 0, 0, 0, v*vfil, v*vfil, 0, 0, v, v);
        slotGlue.Interpose(g);
        csolver->Connect(this, target, &slotGlue);
        Connector::Connect(target, &slotGlue);

    } else if (target->IsA(PAD_COMP)) {
        Graphic* peergr = target->GetGraphic();
        peergr->GetBounds(l, b, r, t);
        float h = (r - l)/2;
        v += (t - b)/2;
        CGlue slotGlue(0, 0, h*hfil, h*hfil, v*vfil, v*vfil, h, h, v, v);

        slotGlue.Interpose(g);
        csolver->Connect(this, target, &slotGlue);
        Connector::Connect(target, &slotGlue);
    }
}

ClassId VSlotComp::GetClassId () { return VSLOT_COMP; }
boolean VSlotComp::IsA (ClassId id) {return VSLOT_COMP==id||SlotComp::IsA(id);}

/**************************************************************************/

VSlotComp* VSlotView::GetVSlotComp () { return (VSlotComp*) GetSubject(); }
VSlotView::VSlotView (VSlotComp* subj) : SlotView(subj) { }
ClassId VSlotView::GetClassId () { return VSLOT_VIEW; }
boolean VSlotView::IsA (ClassId id) {return VSLOT_VIEW==id||SlotView::IsA(id);}

SlotComp* VSlotView::NewSubject (SlotGraphic* slotgr) {
    return new VSlotComp(slotgr);
}

/**************************************************************************/

ClassId PSSlot::GetClassId () { return PS_SLOT; }
boolean PSSlot::IsA (ClassId id) {return PS_SLOT==id||PostScriptView::IsA(id);}
PSSlot::PSSlot (SlotComp* subj) : PostScriptView(subj) { }

boolean PSSlot::Definition (ostream&) {
    // unimplemented

    return true;
}
