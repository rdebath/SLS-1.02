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
 * Implementation of GraphicComp and derived classes.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/creator.h>
#include <Unidraw/editor.h>
#include <Unidraw/iterator.h>
#include <Unidraw/selection.h>
#include <Unidraw/ulist.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/align.h>
#include <Unidraw/Commands/brushcmd.h>
#include <Unidraw/Commands/colorcmd.h>
#include <Unidraw/Commands/datas.h>
#include <Unidraw/Commands/edit.h>
#include <Unidraw/Commands/font.h>
#include <Unidraw/Commands/patcmd.h>
#include <Unidraw/Commands/struct.h>
#include <Unidraw/Commands/transforms.h>

#include <Unidraw/Components/connector.h>
#include <Unidraw/Components/csolver.h>

#include <Unidraw/Graphic/picture.h>
#include <Unidraw/Graphic/pspaint.h>

#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

#include <stream.h>

/*****************************************************************************/

static GraphicComp* Pred (GraphicComp* child) {
    Iterator i;
    GraphicComp* parent = (GraphicComp*) child->GetParent();

    parent->SetComp(child, i);
    parent->Prev(i);
    return parent->GetComp(i);
}

static void NullGS (Graphic* g) { FullGraphic null; *g = null; }

/*****************************************************************************/

class UngroupData : public Data {
public:
    UngroupData(GraphicComp* parent, Graphic*);
    virtual ~UngroupData();
public:
    GraphicComp* _parent;
    FullGraphic* _gs;
};

UngroupData::UngroupData (GraphicComp* p, Graphic* g) { 
    _parent = p;
    _gs = new FullGraphic(g);
}

UngroupData::~UngroupData () {
    delete _gs;
}

/*****************************************************************************/

UList* GraphicComp::_brushes;
UList* GraphicComp::_colors;
UList* GraphicComp::_fonts;
UList* GraphicComp::_patterns;

ClassId GraphicComp::GetClassId () { return GRAPHIC_COMP; }

boolean GraphicComp::IsA (ClassId id) {
    return GRAPHIC_COMP == id || Component::IsA(id);
}

GraphicComp::GraphicComp (Graphic* g) { GraphicComp::SetGraphic(g); }
GraphicComp::~GraphicComp () { delete _gr; }
Graphic* GraphicComp::GetGraphic () { return _gr; }

void GraphicComp::SetGraphic (Graphic* gr) {
    _gr = gr;
    if (gr != nil) gr->SetTag(this);
}

GraphicComp* GraphicComp::GetGraphicComp (Graphic* g) {
    return (GraphicComp*) g->GetTag();
}

Component* GraphicComp::GetParent () { 
    Graphic* parent = GetGraphic()->Parent();
    return (parent == nil) ? nil : GetGraphicComp(parent);
}

GraphicComp* GraphicComp::GetComp (Iterator) { return nil; }
void GraphicComp::SetComp (GraphicComp*, Iterator&) { }
void GraphicComp::Bequeath () { }

void GraphicComp::Append (GraphicComp*) { }
void GraphicComp::Prepend (GraphicComp*) { }
void GraphicComp::InsertBefore (Iterator, GraphicComp*) { }
void GraphicComp::InsertAfter (Iterator, GraphicComp*) { }
void GraphicComp::Remove (GraphicComp*) { }
void GraphicComp::Remove (Iterator&) { }

Mobility GraphicComp::GetMobility () { return Undef; }
void GraphicComp::SetMobility (Mobility) { }

void GraphicComp::Interpret (Command* cmd) {
    Graphic* gr = GetGraphic();

    if (gr == nil) {
        return;
    }

    if (cmd->IsA(BRUSH_CMD)) {
        PSBrush* br = ((BrushCmd*) cmd)->GetBrush();
        cmd->Store(this, new VoidData(gr->GetBrush()));
        gr->SetBrush(br);
        Notify();

    } else if (cmd->IsA(FONT_CMD)) {
        PSFont* font = ((FontCmd*) cmd)->GetFont();
        cmd->Store(this, new VoidData(gr->GetFont()));
        gr->SetFont(font);
//        gr->SetLineHeight(font->Height());      // hack; should be state var
        Notify();

    } else if (cmd->IsA(PATTERN_CMD)) {
        PSPattern* pat = ((PatternCmd*) cmd)->GetPattern();
        cmd->Store(this, new VoidData(gr->GetPattern()));
        gr->SetPattern(pat);
        Notify();

    } else if (cmd->IsA(MOBILITY_CMD)) {
        Mobility m = ((MobilityCmd*) cmd)->GetMobility();
        cmd->Store(this, new MobilityData(GetMobility(), gr));
        SetMobility(m);
        Notify();

    } else if (cmd->IsA(COLOR_CMD)) {
        ColorCmd* colorCmd = (ColorCmd*) cmd;
        PSColor* fg = colorCmd->GetFgColor();
	PSColor* bg = colorCmd->GetBgColor();

        fg = (fg == nil) ? gr->GetFgColor() : fg;
        bg = (bg == nil) ? gr->GetBgColor() : bg;
        cmd->Store(this, new ColorData(gr->GetFgColor(), gr->GetBgColor()));
        gr->SetColors(fg, bg);
        Notify();

    } else if (cmd->IsA(MOVE_CMD)) {
        float dx, dy;
        ((MoveCmd*) cmd)->GetMovement(dx, dy);
        gr->Translate(dx, dy);
        Notify();

    } else if (cmd->IsA(SCALE_CMD)) {
        float sx, sy, x, y;
        ScaleCmd* scaleCmd = (ScaleCmd*) cmd;
        scaleCmd->GetScaling(sx, sy);
        Alignment a = scaleCmd->GetAlignment();

        GetAlignmentPoint(gr, a, x, y);
        cmd->Store(this, new GSData(gr));
        gr->Scale(sx, sy, x, y);
        Notify();

    } else if (cmd->IsA(ROTATE_CMD)) {
        float angle, cx, cy;
        angle = ((RotateCmd*) cmd)->GetRotation();
        gr->GetCenter(cx, cy);
        gr->Rotate(angle, cx, cy);
        Notify();

    } else if (cmd->IsA(ALIGN_CMD)) {
        AlignCmd* alignCmd = (AlignCmd*) cmd;
        Alignment a1, a2;
        float cx0, cy0, cx1, cy1;
        GraphicComp* refcomp = alignCmd->GetReference(this);

        if (refcomp != this) {
            alignCmd->GetAlignment(a1, a2);
            gr->GetCenter(cx0, cy0);
            refcomp->GetGraphic()->Align(a1, gr, a2);
            gr->GetCenter(cx1, cy1);
            cmd->Store(this, new MoveData(cx1 - cx0, cy1 - cy0));
            Notify();
        }

    } else if (
        cmd->IsA(GROUP_CMD) || cmd->IsA(FRONT_CMD) || cmd->IsA(BACK_CMD)
    ) {
        cmd->GetClipboard()->Append(this);

    } else {
        Component::Interpret(cmd);
    }
}

void GraphicComp::Uninterpret (Command* cmd) {
    Graphic* gr = GetGraphic();

    if (gr == nil) {
        return;
    }

    if (cmd->IsA(BRUSH_CMD)) {
        VoidData* vd = (VoidData*) cmd->Recall(this);
        gr->SetBrush((PSBrush*) vd->_void);
        Notify();

    } else if (cmd->IsA(FONT_CMD)) {
        VoidData* vd = (VoidData*) cmd->Recall(this);
        gr->SetFont((PSFont*) vd->_void);
        Notify();

    } else if (cmd->IsA(PATTERN_CMD)) {
        VoidData* vd = (VoidData*) cmd->Recall(this);
        gr->SetPattern((PSPattern*) vd->_void);
        Notify();

    } else if (cmd->IsA(MOBILITY_CMD)) {
        MobilityData* xd = (MobilityData*) cmd->Recall(this);
        SetMobility(xd->_mobility);
        *gr = *xd->_gs;
        Notify();

    } else if (cmd->IsA(COLOR_CMD)) {
        ColorData* cd = (ColorData*) cmd->Recall(this);

        if (cd != nil) {
            gr->SetColors(cd->_fg, cd->_bg);
            Notify();
        }

    } else if (cmd->IsA(MOVE_CMD)) {
        float dx, dy;
        ((MoveCmd*) cmd)->GetMovement(dx, dy);
        gr->Translate(-dx, -dy);
        Notify();

    } else if (cmd->IsA(SCALE_CMD)) {
        GSData* gd = (GSData*) cmd->Recall(this);
        *gr = *gd->_gs;
        Notify();

    } else if (cmd->IsA(ROTATE_CMD)) {
        float angle, cx, cy;
        angle = ((RotateCmd*) cmd)->GetRotation();
        gr->GetCenter(cx, cy);
        gr->Rotate(-angle, cx, cy);
        Notify();

    } else if (cmd->IsA(ALIGN_CMD)) {
        MoveData* md = (MoveData*) cmd->Recall(this);

        if (md != nil) {
            gr->Translate(-md->_dx, -md->_dy);
            Notify();
        }

    } else {
        Component::Uninterpret(cmd);
    }
}

void GraphicComp::Skip (istream& in) { unidraw->GetCatalog()->Skip(in); }
void GraphicComp::Mark (ostream& out) { unidraw->GetCatalog()->Mark(out); }

int GraphicComp::ReadBgFilled (istream& in) {
    return unidraw->GetCatalog()->ReadBgFilled(in);
}

Transformer* GraphicComp::ReadTransformer (istream& in) {
    return unidraw->GetCatalog()->ReadTransformer(in);
}

PSBrush* GraphicComp::ReadBrush (istream& in) {
    return unidraw->GetCatalog()->ReadBrush(in);
}

PSColor* GraphicComp::ReadColor (istream& in) { 
    return unidraw->GetCatalog()->ReadColor(in);
}

PSFont* GraphicComp::ReadFont (istream& in) {
    return unidraw->GetCatalog()->ReadFont(in);
}

PSPattern* GraphicComp::ReadPattern (istream& in) {
    return unidraw->GetCatalog()->ReadPattern(in);
}

void GraphicComp::WriteBgFilled (boolean bgFilled, ostream& out) {
    unidraw->GetCatalog()->WriteBgFilled(bgFilled, out);
}

void GraphicComp::WriteTransformer (Transformer* t, ostream& out) {
    unidraw->GetCatalog()->WriteTransformer(t, out);
}

void GraphicComp::WriteBrush (PSBrush* br, ostream& out) {
    unidraw->GetCatalog()->WriteBrush(br, out);
}

void GraphicComp::WriteColor (PSColor* color, ostream& out) {
    unidraw->GetCatalog()->WriteColor(color, out);
}

void GraphicComp::WriteFont (PSFont* font, ostream& out) {
    unidraw->GetCatalog()->WriteFont(font, out);
}

void GraphicComp::WritePattern (PSPattern* pat, ostream& out) {
    unidraw->GetCatalog()->WritePattern(pat, out);
}

char* GraphicComp::ReadString (istream& in) {
    return unidraw->GetCatalog()->ReadString(in);
}

Bitmap* GraphicComp::ReadBitmap (istream& in) {
    return unidraw->GetCatalog()->ReadBitmap(in);
}

Raster* GraphicComp::ReadGraymap (istream& in) {
    return unidraw->GetCatalog()->ReadGraymap(in);
}

Raster* GraphicComp::ReadRaster(istream& in) {
    return unidraw->GetCatalog()->ReadRaster(in);
}

void GraphicComp::WriteString (const char* string, ostream& out) {
    unidraw->GetCatalog()->WriteString(string, out);
}

void GraphicComp::WriteBitmap (Bitmap* bm, ostream& out) {
    unidraw->GetCatalog()->WriteBitmap(bm, out);
}

void GraphicComp::WriteGraymap (Raster* gm, ostream& out) {
    unidraw->GetCatalog()->WriteGraymap(gm, out);
}

void GraphicComp::WriteRaster (Raster* raster, ostream& out) {
    unidraw->GetCatalog()->WriteRaster(raster, out);
}

void GraphicComp::ReadVertices (
    istream& in, Coord*& x, Coord*& y, int& count
) {
    in >> count;
    x = new Coord[count];
    y = new Coord[count];

    if (unidraw->GetCatalog()->FileVersion() > UV_PRERELEASE) {
        for (int i = 0; i < count; ++i) {
            Skip(in);
            in >> x[i] >> y[i];
        }

    } else {
        for (int i = 0; i < count; ++i) {
            in >> x[i] >> y[i];
        }
    }
}

void GraphicComp::WriteVertices (
    const Coord* x, const Coord* y, int count, ostream& out
) {
    out << count;
    
    for (int i = 0; i < count; ++i) {
        Mark(out);
        out << x[i] << " " << y[i] << " ";
    }
}
/*****************************************************************************/

ClassId GraphicComps::GetClassId () { return GRAPHIC_COMPS; }

boolean GraphicComps::IsA (ClassId id) {
    return GRAPHIC_COMPS == id || GraphicComp::IsA(id);
}

Component* GraphicComps::Copy () { return unidraw->GetCatalog()->Copy(this); }
GraphicComps::GraphicComps () : GraphicComp(new Picture) { _comps = new UList;}
GraphicComps::GraphicComps (Graphic* g) : GraphicComp(g) { _comps = new UList;}

GraphicComps::~GraphicComps () {
    Iterator i;
    GraphicComp* comp;

    First(i);
    while (!Done(i)) {
        comp = GetComp(i);
        Remove(i);
        delete comp;
    }
    delete _comps;
}

void GraphicComps::Interpret (Command* cmd) {
    Editor* ed = cmd->GetEditor();

    if (
        (cmd->IsA(DELETE_CMD) || cmd->IsA(CUT_CMD)) && 
        ed->GetComponent() != this
    ) {
        Iterator i;
        for (First(i); !Done(i); Next(i)) {
            GetComp(i)->Interpret(cmd);
        }

    } else if (cmd->IsA(DELETE_CMD)) {
        Clipboard* cb = cmd->GetClipboard();
        Selection* s = ed->GetSelection();

        if (cb == nil) {
            if (s->IsEmpty()) {
                return;
            }
            cmd->SetClipboard(cb = new Clipboard);
            cb->Init(s);
        }
        s->Clear();
        Iterator i;

        for (cb->First(i); !cb->Done(i); cb->Next(i)) {
            GraphicComp* comp = cb->GetComp(i);
            unidraw->CloseDependents(comp);
            comp->Interpret(cmd);
            StorePosition(comp, cmd);
            Remove(comp);
        }
        Notify();
        unidraw->Update();

    } else if (cmd->IsA(CUT_CMD)) {
        Clipboard* cb = cmd->GetClipboard();
        Selection* s = ed->GetSelection();

        if (cb == nil) {
            if (s->IsEmpty()) {
                return;
            }
            GraphicView* views = ed->GetViewer()->GetGraphicView();
            s->Sort(views);
            cmd->SetClipboard(cb = new Clipboard);
            cb->Init(s);

            Clipboard* globalcb = unidraw->GetCatalog()->GetClipboard();
            globalcb->DeleteComps();
            globalcb->CopyInit(s);
        }
        s->Clear();
        Iterator i;

        for (cb->First(i); !cb->Done(i); cb->Next(i)) {
            GraphicComp* comp = cb->GetComp(i);
            unidraw->CloseDependents(comp);
            comp->Interpret(cmd);
            StorePosition(comp, cmd);
            Remove(comp);
        }
        Notify();
        unidraw->Update();

    } else if (cmd->IsA(PASTE_CMD)) {
        Clipboard* cb = cmd->GetClipboard();
        Iterator i;

        if (cb == nil) {
            Clipboard* globalcb = unidraw->GetCatalog()->GetClipboard();

            if (globalcb->IsEmpty()) {
                return;
            }
            cmd->SetClipboard(cb = globalcb->DeepCopy());
        }

        for (cb->First(i); !cb->Done(i); cb->Next(i)) {
            Append(cb->GetComp(i));
        }
        Notify();
        SelectClipboard(cb, ed);
        unidraw->Update();

    } else if (cmd->IsA(DUP_CMD)) {
        GraphicView* views = ed->GetViewer()->GetGraphicView();
        GraphicComp* prev, *dup1;
        Iterator i, pos;
        Clipboard* cb = cmd->GetClipboard();
        const float offset = 8;
        MoveCmd move(ed, offset, offset);

        if (cb == nil) {
            Selection* s = ed->GetSelection();

            if (s->IsEmpty()) {
                return; 
            }
            cmd->SetClipboard(cb = new Clipboard);
            s->Sort(views);

            for (s->First(i); !s->Done(i); s->Next(i)) {
                dup1 = (GraphicComp*) s->GetView(i)->GetGraphicComp()->Copy();
                dup1->Interpret(&move);
                cb->Append(dup1);
            }
            cb->First(i);
            dup1 = cb->GetComp(i);
            Last(pos);
            prev = GetComp(pos);
            cmd->Store(dup1, new VoidData(prev));

        } else {
            cb->First(i);
            dup1 = cb->GetComp(i);
            VoidData* vd = (VoidData*) cmd->Recall(dup1);
            prev = (GraphicComp*) vd->_void;
            SetComp(prev, pos);
        }

        for (cb->Last(i); !cb->Done(i); cb->Prev(i)) {
            InsertAfter(pos, cb->GetComp(i));
        }

        Notify();
        SelectClipboard(cb, ed);
        unidraw->Update();

    } else if (cmd->IsA(GROUP_CMD)) {
        GroupCmd* gcmd = (GroupCmd*) cmd;
        GraphicComp* group = gcmd->GetGroup();
        Component* edComp = gcmd->GetEditor()->GetComponent();

        if (group == this) {
            edComp->Interpret(gcmd);

        } else if (edComp == (Component*) this) {
            Clipboard* cb = cmd->GetClipboard();
            NullGS(group->GetGraphic());
            Group(cb, group, cmd);
            Notify();
            SelectViewsOf(group, ed);
            unidraw->Update();

        } else {
            GraphicComp::Interpret(gcmd);
        }

    } else if (cmd->IsA(UNGROUP_CMD)) {
        UngroupCmd* ucmd = (UngroupCmd*) cmd;
        Component* edComp = ucmd->GetEditor()->GetComponent();

        if (edComp == (Component*) this) {
            Clipboard* cb = cmd->GetClipboard();
            Clipboard* kids = new Clipboard;
            ucmd->SetKids(kids);
            Iterator i;

            for (cb->First(i); !cb->Done(i); cb->Next(i)) {
                GraphicComp* parent = cb->GetComp(i);
                unidraw->CloseDependents(parent);
                Ungroup(parent, kids, cmd);
            }
            Notify();
            SelectClipboard(kids, ed);
            unidraw->Update();

        } else {
            cmd->GetClipboard()->Append(this);
        }

    } else if (cmd->IsA(FRONT_CMD) || cmd->IsA(BACK_CMD)) {
        Component* edComp = cmd->GetEditor()->GetComponent();

        if (edComp == (Component*) this) {
            Clipboard* cb = cmd->GetClipboard();
            Iterator i;

            if (cmd->IsA(FRONT_CMD)) {
                for (cb->First(i); !cb->Done(i); cb->Next(i)) {
                    GraphicComp* comp = cb->GetComp(i);
                    StorePosition(comp, cmd);
                    Remove(comp);
                    Append(comp);
                }

            } else {
                for (cb->Last(i); !cb->Done(i); cb->Prev(i)) {
                    GraphicComp* comp = cb->GetComp(i);
                    StorePosition(comp, cmd);
                    Remove(comp);
                    Prepend(comp);
                }
            }
            Notify();
            unidraw->Update();

        } else {
            GraphicComp::Interpret(cmd);
        }

    } else {
        GraphicComp::Interpret(cmd);
    }
}

void GraphicComps::Uninterpret (Command* cmd) {
    Editor* ed = cmd->GetEditor();

    if (
        (cmd->IsA(DELETE_CMD) || cmd->IsA(CUT_CMD)) && 
        ed->GetComponent() != this
    ) {
        Iterator i;
        for (Last(i); !Done(i); Prev(i)) {
            GetComp(i)->Uninterpret(cmd);
        }

    } else if (cmd->IsA(DELETE_CMD)) {
        Clipboard* cb = cmd->GetClipboard();

        if (cb != nil) {
            Iterator i;

            for (cb->Last(i); !cb->Done(i); cb->Prev(i)) {
                GraphicComp* comp = cb->GetComp(i);
                RestorePosition(comp, cmd);
                comp->Uninterpret(cmd);
            }
            Notify();
            SelectClipboard(cb, ed);
            unidraw->Update();
        }

    } else if (cmd->IsA(CUT_CMD)) {
        Clipboard* cb = cmd->GetClipboard();

        if (cb != nil) {
            Iterator i;

            for (cb->Last(i); !cb->Done(i); cb->Prev(i)) {
                GraphicComp* comp = cb->GetComp(i);
                RestorePosition(comp, cmd);
                comp->Uninterpret(cmd);
            }
            Notify();
            SelectClipboard(cb, ed);
            unidraw->Update();
        }

    } else if (cmd->IsA(PASTE_CMD)) {
        Clipboard* cb = cmd->GetClipboard();

        if (cb != nil) {
            Selection* s = ed->GetSelection();
            Iterator i, pos;

            s->Clear();

            for (cb->First(i); !cb->Done(i); cb->Next(i)) {
                GraphicComp* comp = cb->GetComp(i);
                unidraw->CloseDependents(comp);
                Remove(comp);
            }
            Notify();
            unidraw->Update();
        }

    } else if (cmd->IsA(DUP_CMD)) {
        Clipboard* cb = cmd->GetClipboard();

        if (cb != nil) {
            Selection* s = ed->GetSelection();
            Iterator i;

            s->Clear();

            for (cb->First(i); !cb->Done(i); cb->Next(i)) {
                GraphicComp* comp = cb->GetComp(i);
                unidraw->CloseDependents(comp);
                Remove(comp);
            }
            Notify();
            unidraw->Update();
        }

    } else if (cmd->IsA(GROUP_CMD)) {
        GroupCmd* gcmd = (GroupCmd*) cmd;
        GraphicComp* group = gcmd->GetGroup();
        Component* edComp = gcmd->GetEditor()->GetComponent();

        if (group == this) {
            edComp->Uninterpret(gcmd);

        } else if (edComp == (Component*) this) {
            Clipboard* cb = cmd->GetClipboard();
            Iterator i;
            cb->First(i);
            GraphicComp* group = (GraphicComp*) cb->GetComp(i)->GetParent();

            GroupCmd* gcmd = (GroupCmd*) cmd;
            unidraw->CloseDependents(group);

            for (cb->Last(i); !cb->Done(i); cb->Prev(i)) {
                RestorePosition(cb->GetComp(i), cmd);
            }
            Remove(group);
            Notify();
            SelectClipboard(cb, ed);
            unidraw->Update();

        } else {
            GraphicComp::Uninterpret(gcmd);
        }

    } else if (cmd->IsA(UNGROUP_CMD)) {
        UngroupCmd* ucmd = (UngroupCmd*) cmd;
        Component* edComp = ucmd->GetEditor()->GetComponent();

        if (edComp == (Component*) this) {
            Clipboard* cb = ucmd->GetClipboard();
            Clipboard* kids = ucmd->GetKids();
            Clipboard insertedParents;
            Iterator k;

            for (kids->First(k); !kids->Done(k); kids->Next(k)) {
                GraphicComp* kid = kids->GetComp(k);
                UngroupData* ud = (UngroupData*) cmd->Recall(kid);
                GraphicComp* parent = ud->_parent;
                *kid->GetGraphic() = *ud->_gs;

                if (!insertedParents.Includes(parent)) {
                    GSData* gd = (GSData*) cmd->Recall(parent);
                    *parent->GetGraphic() = *gd->_gs;

                    Iterator insertPt;
                    SetComp(kid, insertPt);
                    InsertBefore(insertPt, parent);
                    insertedParents.Append(parent);
                }

                Remove(kid);
                parent->Append(kid);
            }
            Notify();
            SelectClipboard(cb, ed);
            unidraw->Update();

            delete kids;
            ucmd->SetKids(nil);
        }

    } else if (cmd->IsA(FRONT_CMD)) {
        Component* edComp = cmd->GetEditor()->GetComponent();

        if (edComp == (Component*) this) {
            Clipboard* cb = cmd->GetClipboard();
            Iterator i;

            for (cb->Last(i); !cb->Done(i); cb->Prev(i)) {
                RestorePosition(cb->GetComp(i), cmd);
            }
            Notify();
            SelectClipboard(cb, ed);
            unidraw->Update();

        } else {
            GraphicComp::Uninterpret(cmd);
        }

    } else if (cmd->IsA(BACK_CMD)) {
        Component* edComp = cmd->GetEditor()->GetComponent();

        if (edComp == (Component*) this) {
            Clipboard* cb = cmd->GetClipboard();
            Iterator i;

            for (cb->First(i); !cb->Done(i); cb->Next(i)) {
                RestorePosition(cb->GetComp(i), cmd);
            }
            Notify();
            SelectClipboard(cb, ed);
            unidraw->Update();

        } else {
            GraphicComp::Uninterpret(cmd);
        }

    } else {
        GraphicComp::Uninterpret(cmd);
    }
}

UList* GraphicComps::Elem (Iterator i) { return (UList*) i.GetValue(); }
void GraphicComps::First (Iterator& i) { i.SetValue(_comps->First()); }
void GraphicComps::Last (Iterator& i) { i.SetValue(_comps->Last()); }
void GraphicComps::Next (Iterator& i) { i.SetValue(Elem(i)->Next()); }
void GraphicComps::Prev (Iterator& i) { i.SetValue(Elem(i)->Prev()); }
boolean GraphicComps::Done (Iterator i) { return Elem(i) == _comps->End(); }
GraphicComp* GraphicComps::Comp (UList* r) { return (GraphicComp*) (*r)(); }
GraphicComp* GraphicComps::GetComp (Iterator i) { return Comp(Elem(i)); }

void GraphicComps::SetComp (GraphicComp* gc, Iterator& i) {
    i.SetValue(_comps->Find(gc));
}

void GraphicComps::Append (GraphicComp* comp) {
    Graphic* g = comp->GetGraphic();

    _comps->Append(new UList(comp));
    if (g != nil) GetGraphic()->Append(g);

    SetParent(comp, this);
}

void GraphicComps::Prepend (GraphicComp* comp) {
    Graphic* g = comp->GetGraphic();

    _comps->Prepend(new UList(comp));
    if (g != nil) GetGraphic()->Prepend(g);

    SetParent(comp, this);
}

void GraphicComps::InsertBefore (Iterator i, GraphicComp* comp) {
    Graphic* g = comp->GetGraphic();
    Graphic* parent;

    Elem(i)->Append(new UList(comp));

    if (g != nil) {
        Iterator j;
        parent = GetGraphic();    
        parent->SetGraphic(GetComp(i)->GetGraphic(), j);
        parent->InsertBefore(j, g);
    }
    SetParent(comp, this);
}

void GraphicComps::InsertAfter (Iterator i, GraphicComp* comp) {
    Graphic* g = comp->GetGraphic();
    Graphic* parent;
    
    Elem(i)->Prepend(new UList(comp));
    
    if (g != nil) {
        Iterator j;
        parent = GetGraphic();
        parent->SetGraphic(GetComp(i)->GetGraphic(), j);
        parent->InsertAfter(j, g);
    }
    SetParent(comp, this);
}

void GraphicComps::Remove (Iterator& i) {
    UList* doomed = Elem(i);
    GraphicComp* comp = Comp(doomed);
    Graphic* g = comp->GetGraphic();

    Next(i);
    _comps->Remove(doomed);
    if (g != nil) GetGraphic()->Remove(g);

    SetParent(comp, nil);
    delete doomed;
}

void GraphicComps::Remove (GraphicComp* comp) {
    Graphic* g = comp->GetGraphic();

    _comps->Delete(comp);
    if (g != nil) GetGraphic()->Remove(g);

    SetParent(comp, nil);
}

void GraphicComps::Bequeath () { GetGraphic()->Bequeath(); }

void GraphicComps::SetMobility (Mobility m) {
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
        GetComp(i)->SetMobility(m);
    }
}

void GraphicComps::Read (istream& in) {
    GraphicComp::Read(in);
    Graphic* g = GetGraphic();

    int count;
    in >> count;

    for (int i = 0; i < count; ++i) {
        Append((GraphicComp*) unidraw->GetCatalog()->ReadComponent(in));
    }

    g->FillBg(ReadBgFilled(in));
    g->SetBrush(ReadBrush(in));
    PSColor* fg = ReadColor(in);
    PSColor* bg = ReadColor(in);
    g->SetColors(fg, bg);
    g->SetFont(ReadFont(in));
    g->SetPattern(ReadPattern(in));

    Transformer* t = ReadTransformer(in);
    g->SetTransformer(t);
    Unref(t);
}

void GraphicComps::Write (ostream& out) {
    GraphicComp::Write(out);
    Iterator i;
    int count = 0;
    Graphic* g = GetGraphic();

    for (First(i); !Done(i); Next(i), ++count);
    out << count << "\n";

    for (First(i); !Done(i); Next(i)) {
        GraphicComp* comp = GetComp(i);
        unidraw->GetCatalog()->WriteComponent(comp, out);
        out << "\n";
    }

    WriteBgFilled(g->BgFilled(), out);
    WriteBrush(g->GetBrush(), out);
    WriteColor(g->GetFgColor(), out);
    WriteColor(g->GetBgColor(), out);
    WriteFont(g->GetFont(), out);
    WritePattern(g->GetPattern(), out);
    WriteTransformer(g->GetTransformer(), out);
}

void GraphicComps::SelectViewsOf (GraphicComp* comp, Editor* ed) {
    Selection* s = ed->GetSelection();
    s->Clear();
    Viewer* viewer;

    for (int i = 0; (viewer = ed->GetViewer(i)) != nil; ++i) {
        GraphicView* views = viewer->GetGraphicView();
        GraphicView* view = views->GetGraphicView(comp);

        if (view != nil) s->Append(view);
    }
}

void GraphicComps::SelectClipboard (Clipboard* cb, Editor* ed) {
    Selection* s = ed->GetSelection();
    s->Clear();
    Viewer* viewer;
    Iterator i;

    for (int j = 0; (viewer = ed->GetViewer(j)) != nil; ++j) {
        for (cb->First(i); !cb->Done(i); cb->Next(i)) {
            GraphicView* views = viewer->GetGraphicView();
            GraphicView* view = views->GetGraphicView(cb->GetComp(i));

            if (view != nil) s->Append(view);
        }
    }
}

void GraphicComps::StorePosition (GraphicComp* comp, Command* cmd) {
    cmd->Store(comp, new VoidData(Pred(comp)));
}

void GraphicComps::RestorePosition (GraphicComp* comp, Command* cmd) {
    VoidData* vd = (VoidData*) cmd->Recall(comp);
    GraphicComp* pred = (GraphicComp*) vd->_void;
    GraphicComp* parent = (GraphicComp*) comp->GetParent();

    if (parent != nil) parent->Remove(comp);

    if (pred == nil) {
        Prepend(comp);

    } else {
        Iterator insertPt;
        SetComp(pred, insertPt);
        InsertAfter(insertPt, comp);
    }
}

void GraphicComps::Group (Clipboard* cb, GraphicComp* group, Command* cmd) {
    Iterator insertPt, i;

    cb->Last(i);
    GraphicComp* last = cb->GetComp(i);
    SetComp(last, insertPt);
    InsertAfter(insertPt, group);

    for (cb->First(i); !cb->Done(i); cb->Next(i)) {
        GraphicComp* comp = cb->GetComp(i);
        StorePosition(comp, cmd);
        Remove(comp);
        group->Append(comp);
    }
}

void GraphicComps::Ungroup (GraphicComp* parent, Clipboard* cb, Command* cmd) {
    Iterator i, insertPt;
    parent->First(i);

    if (!parent->Done(i)) {
        SetComp(parent, insertPt);

        for (parent->First(i); !parent->Done(i); parent->Next(i)) {
            GraphicComp* kid = parent->GetComp(i);
            cmd->Store(kid, new UngroupData(parent, kid->GetGraphic()));
        }

        cmd->Store(parent, new GSData(parent->GetGraphic()));
        parent->Bequeath();
        parent->First(i);

        do {
            GraphicComp* kid = parent->GetComp(i);
            parent->Remove(i);
            InsertBefore(insertPt, kid);
            cb->Append(kid);
        } while (!parent->Done(i));

        Remove(parent);
    }
}
