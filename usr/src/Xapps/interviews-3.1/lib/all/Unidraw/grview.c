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
 * Implementation of GraphicView and derived classes.
 */

#include <Unidraw/classes.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/grid.h>
#include <Unidraw/iterator.h>
#include <Unidraw/manips.h>
#include <Unidraw/selection.h>
#include <Unidraw/ulist.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/align.h>
#include <Unidraw/Commands/edit.h>
#include <Unidraw/Commands/transforms.h>

#include <Unidraw/Components/connector.h>
#include <Unidraw/Components/gvupdater.h>

#include <Unidraw/Graphic/damage.h>
#include <Unidraw/Graphic/picture.h>

#include <Unidraw/Tools/tool.h>

#include <InterViews/event.h>
#include <IV-2_6/InterViews/rubcurve.h>
#include <IV-2_6/InterViews/rubrect.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

#include <math.h>
#include <stdlib.h>

/*****************************************************************************/

ClassId GraphicView::GetClassId () { return GRAPHIC_VIEW; }

boolean GraphicView::IsA (ClassId id) {
    return GRAPHIC_VIEW == id || ComponentView::IsA(id);
}

GraphicView::GraphicView (GraphicComp* subj) : ComponentView(subj) {
    SetGraphic(nil);
    _handles = nil;
}

void GraphicView::SetGraphic (Graphic* graphic) {
    _gr = graphic;
    if (graphic != nil) {
        graphic->SetTag(this);
    }
}

void GraphicView::AddDamage (Graphic* g) {
    Viewer* viewer = GetViewer();

    if (viewer != nil) {
        viewer->GetDamage()->Added(g);
    }
}

void GraphicView::IncurDamage (Graphic* g) {        
    Viewer* viewer = GetViewer();

    if (viewer != nil) {
        viewer->GetDamage()->Incur(g);
    }
}

void GraphicView::Unselect (GraphicView* gv) {
    Viewer* viewer = GetViewer();

    if (viewer != nil) {
        Selection* s = viewer->GetSelection();
        gv->EraseHandles();
        viewer->GetSelection()->Remove(gv);
    }
}

GraphicView::~GraphicView () {
    if (_handles != nil) {
        delete _handles;
    }
    if (_gr != nil) {
        delete _gr;
    }
}

GraphicView* GraphicView::View (UList* r) { return (GraphicView*) (*r)(); }

void GraphicView::Interpret (Command* cmd) {
    if (cmd->IsA(ALIGNTOGRID_CMD)) {
        float l, b, r, t;
        GetGraphic()->GetBounds(l, b, r, t);
        ((AlignToGridCmd*) cmd)->Align(this, l, b);

    } else {
        ComponentView::Interpret(cmd);
    }
}

void GraphicView::Uninterpret (Command* cmd) {
    if (cmd->IsA(ALIGNTOGRID_CMD)) {
        ((AlignToGridCmd*) cmd)->Unalign(this);

    } else {
        ComponentView::Uninterpret(cmd);
    }
}

void GraphicView::CreateHandles () {
    Coord left, bottom, right, top, halfx, halfy;
    Coord x[8], y[8];
    Viewer* v = GetViewer();
    
    if (v != nil) {
        GetGraphic()->GetBox(left, bottom, right, top);
        halfx = (right + left)/2;
        halfy = (top + bottom)/2;
        x[0] = left;    y[0] = bottom;
        x[1] = halfx;   y[1] = bottom;
        x[2] = right;   y[2] = bottom;
        x[3] = right;   y[3] = halfy;
        x[4] = right;   y[4] = top;
        x[5] = halfx;   y[5] = top;
        x[6] = left;    y[6] = top;
        x[7] = left;    y[7] = halfy;

        _handles = new RubberHandles(nil, nil, x, y, 8, 0, HANDLE_SIZE);
        v->InitRubberband(_handles);
    }
}

void GraphicView::DrawHandles () {
    if (_handles == nil) {
        CreateHandles();
    }
    _handles->Draw();
}

void GraphicView::RedrawHandles () {
    if (_handles == nil) {
        CreateHandles();
    }
    _handles->Redraw();
}

void GraphicView::InitHandles () {
    if (_handles != nil) {
        delete _handles;
        _handles = nil;
        CreateHandles();
    }
}

void GraphicView::EraseHandles () {
    if (_handles != nil) {
        _handles->Erase();
        delete _handles;
        _handles = nil;
    }
}

Manipulator* GraphicView::CreateGraphicCompManip (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    GraphicView* views = v->GetGraphicView();
    GraphicComp* comp = GetGraphicComp();
    Selection* s = v->GetSelection();
    SlidingRect* sr;
    Coord x0, y0, x1, y1, halfw, halfh;

    s->Clear();
    GetGraphic()->GetBox(x0, y0, x1, y1);
    if (rel != nil) {
        rel->Transform(x0, y0);
        rel->Transform(x1, y1);
        halfw = abs(x1 - x0) / 2;
        halfh = abs(y1 - y0) / 2;
    }
    v->Constrain(e.x, e.y);
    sr = new SlidingRect(
        nil, nil, e.x - halfw, e.y - halfh, e.x + halfw, e.y + halfh, e.x, e.y
    );
    return new DragManip(v, sr, rel, tool, Gravity);
}

Manipulator* GraphicView::CreateStretchManip (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Coord l, b, r, t, tmp;
    DragConstraint dc = HorizOrVert;

    v->Constrain(e.x, e.y);
    GetGraphic()->GetBox(l, b, r, t);
    boolean horizCtr = e.x > (2*l + r)/3 && e.x < (l + 2*r)/3;
    boolean vertCtr  = e.y > (2*b + t)/3 && e.y < (b + 2*t)/3;

    if (e.x < (l + r)/2) {
        tmp = r;
        r = l;
        l = tmp;
    }
    if (e.y < (b + t)/2) {
        tmp = t;
        t = b;
        b = tmp;
    }
    if (horizCtr && !vertCtr) {
        dc = XFixed;
    } else if (!horizCtr && vertCtr) {
        dc = YFixed;
    }

    RubberRect* rub = new RubberRect(nil, nil, l, b, r, t);
    return new DragManip(
	v, rub, rel, tool, DragConstraint(dc | Gravity), r, t
    );
}

Manipulator* GraphicView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Coord l, r, b, t;
    Rubberband* rub = nil;
    Manipulator* m = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        m = CreateGraphicCompManip(v, e, rel, tool);

    } else if (tool->IsA(MOVE_TOOL)) {
        v->Constrain(e.x, e.y);
        v->GetSelection()->GetBox(l, b, r, t);
        rub = new SlidingRect(nil, nil, l, b, r, t, e.x, e.y);
        m = new DragManip(
	    v, rub, rel, tool, DragConstraint(HorizOrVert | Gravity)
	);

    } else if (tool->IsA(SCALE_TOOL)) {
        v->Constrain(e.x, e.y);
        GetGraphic()->GetBox(l, b, r, t);
        rub = new ScalingRect(nil, nil, l, b, r, t, (l+r)/2, (b+t)/2);
        m = new DragManip(v, rub, rel, tool, Gravity);

    } else if (tool->IsA(STRETCH_TOOL)) {
        m = CreateStretchManip(v, e, rel, tool);

    } else if (tool->IsA(ROTATE_TOOL)) {
        v->Constrain(e.x, e.y);
        GetGraphic()->GetBox(l, b, r, t);
        rub = new RotatingRect(
            nil, nil, l, b, r, t, (l+r)/2, (b+t)/2, e.x, e.y
        );
        m = new DragManip(v, rub, rel, tool, Gravity);
    }
    return m;
}

Command* GraphicView::InterpretGraphicCompManip (Manipulator* m) {
    DragManip* dm = (DragManip*) m;
    Editor* ed = dm->GetViewer()->GetEditor();
    Transformer* rel = dm->GetTransformer();
    SlidingRect* slidingRect = (SlidingRect*) dm->GetRubberband();
    Coord cx0, cy0, cx1, cy1;
    float cx, cy, px, py;
    GraphicComp* newComp = (GraphicComp*) GetGraphicComp()->Copy();
    Graphic* grcomp = newComp->GetGraphic();

    slidingRect->GetCurrent(cx0, cy0, cx1, cy1);
    px = float(cx0 + cx1) / 2;
    py = float(cy0 + cy1) / 2;
    rel->InvTransform(px, py, cx, cy);
    grcomp->GetCenter(px, py);
    grcomp->Translate(cx - px, cy - py);
    return new PasteCmd(ed, new Clipboard(newComp));
}

Command* GraphicView::InterpretStretchManip (Manipulator* m) {
    Viewer* v = m->GetViewer();
    DragManip* dm = (DragManip*) m;
    Editor* ed = v->GetEditor();
    RubberRect* rr = (RubberRect*) dm->GetRubberband();
    Coord l0, b0, r0, t0, l1, b1, r1, t1;
    float sx, sy;
    Alignment a;

    rr->GetOriginal(l0, b0, r0, t0);

    if (v->GetOrientation() == Landscape) {
        if (l0 > r0) {
            a = (b0 > t0) ? TopLeft : TopRight;
        } else {
            a = (b0 > t0) ? BottomLeft : BottomRight;
        }
    } else {
        if (l0 > r0) {
            a = (b0 > t0) ? TopRight : BottomRight;
        } else {
            a = (b0 > t0) ? TopLeft : BottomLeft;
        }
    }

    rr->GetCurrent(l1, b1, r1, t1);
    sx = float(r1 - l1) / float(r0 - l0);
    sy = float(t1 - b1) / float(t0 - b0);

    if (v->GetOrientation() == Landscape) {
        float tmp = sx;
        sx = sy;
        sy = tmp;
    }
    return new ScaleCmd(ed, sx, sy, a);
}

Command* GraphicView::InterpretManipulator (Manipulator* m) {
    DragManip* dm = (DragManip*) m;
    Editor* ed = dm->GetViewer()->GetEditor();
    Tool* tool = dm->GetTool();
    Transformer* rel = dm->GetTransformer();
    Command* cmd = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        cmd = InterpretGraphicCompManip(m);

    } else if (tool->IsA(MOVE_TOOL)) {
        SlidingRect* sr = (SlidingRect*) dm->GetRubberband();
        Coord x0, y0, x1, y1, dummy1, dummy2;
        float fx0, fy0, fx1, fy1;

        sr->GetOriginal(x0, y0, dummy1, dummy2);
        sr->GetCurrent(x1, y1, dummy1, dummy2);
        if (rel != nil) {
            rel->InvTransform(float(x0), float(y0), fx0, fy0);
            rel->InvTransform(float(x1), float(y1), fx1, fy1);
        }
        cmd = new MoveCmd(ed, fx1-fx0, fy1-fy0);

    } else if (tool->IsA(SCALE_TOOL)) {
        ScalingRect* sr = (ScalingRect*) dm->GetRubberband();
        float scale = sr->CurrentScaling();

        cmd = new ScaleCmd(ed, scale, scale);

    } else if (tool->IsA(STRETCH_TOOL)) {
        cmd = InterpretStretchManip(m);

    } else if (tool->IsA(ROTATE_TOOL)) {
        RotatingRect* rr = (RotatingRect*) dm->GetRubberband();

        cmd = new RotateCmd(ed, rr->CurrentAngle());
    }
    return cmd;
}

Graphic* GraphicView::GetGraphic () { return _gr; }

GraphicView* GraphicView::GetGraphicView (Graphic* g) {
    return (GraphicView*) g->GetTag();
}

GraphicView* GraphicView::GetView (Iterator) { return nil; }
void GraphicView::SetView (GraphicView*, Iterator&) { }
void GraphicView::Add (GraphicView*) { }
void GraphicView::Append (GraphicView*) { }
void GraphicView::InsertBefore (Iterator, GraphicView*) { }
void GraphicView::Remove (Iterator&) { }
void GraphicView::DeleteView (Iterator&) { }

Selection* GraphicView::SelectAll () { return nil; }
Selection* GraphicView::ViewContaining (Coord, Coord) { return nil; }
Selection* GraphicView::ViewsContaining (Coord, Coord) { return nil; }

Selection* GraphicView::ViewIntersecting (Coord, Coord, Coord, Coord) {
    return nil; 
}

Selection* GraphicView::ViewsIntersecting (Coord, Coord, Coord, Coord) {
    return nil;
}

Selection* GraphicView::ViewsWithin (Coord, Coord, Coord, Coord) {
    return nil;
}

ConnectorView* GraphicView::ConnectorIntersecting (Coord,Coord,Coord,Coord) {
    return nil;
}

boolean GraphicView::Includes (GraphicView* view) {
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
        if (GetView(i) == view) {
            return true;
        }
    }
    return false;
}

GraphicView* GraphicView::GetGraphicView (Component* c) {
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
        GraphicView* view = GetView(i);
        if (view->GetSubject() == c) {
            return view;
        }
    }
    return nil;
}

ComponentView* GraphicView::GetParent () {
    Graphic* parent = GetGraphic()->Parent();

    if (parent == nil) {
        return nil;
    } else {
        return GetGraphicView(parent);
    }
}

Viewer* GraphicView::GetViewer () {
    GraphicView* parent = (GraphicView*) GetParent();

    if (parent == nil) {
        return nil;
    } else {
        return parent->GetViewer();
    }
}

GraphicComp* GraphicView::GetGraphicComp () {
    return (GraphicComp*) GetSubject();
}    

int GraphicView::ClosestPoint(
    Coord x[], Coord y[], int n, Coord px, Coord py
) {
    int closestPt = 0;
    PointObj p(x[0],y[0]), cp(px,py);
    float minDist = p.Distance(cp);

    for (int i = 1; i < n; ++i) {
	p._x = x[i];
	p._y = y[i];
	float dist = p.Distance(cp);

	if (dist < minDist) {
	    minDist = dist;
	    closestPt = i;
	}
    }
    return closestPt;
}

/****************************************************************************/

GraphicViews::GraphicViews (GraphicComps* subj) : GraphicView(subj) {
    _views = new UList;
}

ClassId GraphicViews::GetClassId () { return GRAPHIC_VIEWS; }

boolean GraphicViews::IsA (ClassId id) {
    return GRAPHIC_VIEWS == id || GraphicView::IsA(id);
}

GraphicViews::~GraphicViews () {
    Iterator i;
    Graphic* parent = GetGraphic();

    First(i);
    while (!Done(i)) {
        UList* doomed = Elem(i);
        GraphicView* view = GetView(i);
        Graphic* g = view->GetGraphic();

        Next(i);
        _views->Remove(doomed);
        parent->Remove(g);
        delete doomed;
        delete view;
    }
    delete _views;
}

static GraphicView* GetLeaf (GraphicView* gv) {
    Iterator i;
    gv->First(i);

    if (!gv->Done(i)) {
        gv = GetLeaf(gv->GetView(i));
    }

    return gv;
}    

void GraphicViews::Interpret (Command* cmd) {
    if (cmd->IsA(ALIGNTOGRID_CMD)) {
        Viewer* viewer = GetViewer();
        Grid* grid = (viewer == nil) ? nil : viewer->GetGrid();

        if (grid == nil) {
            return;
        }

        AlignToGridCmd* acmd = (AlignToGridCmd*) cmd;
        GraphicView* leaf = GetLeaf(this);
        Graphic* leafg = leaf->GetGraphic();

        float cx, cy, dx, dy;
        leafg->GetCenter(cx, cy);
        leaf->Interpret(acmd);
        leafg->GetCenter(dx, dy);
        leaf->Uninterpret(acmd);
    
        dx -= cx;
        dy -= cy;

        Coord rcx = 0, rcy = 0;
        grid->Constrain(rcx, rcy);

        acmd->Align(this, float(rcx) - dx, float(rcy) - dy);

    } else {
        GraphicView::Interpret(cmd);
    }
}

void GraphicViews::Update () { GVUpdater gvu(this); gvu.Update(); }

UList* GraphicViews::Elem (Iterator i) { return (UList*) i.GetValue(); }
void GraphicViews::First (Iterator& i) { i.SetValue(_views->First()); }
void GraphicViews::Last (Iterator& i) { i.SetValue(_views->Last()); }
void GraphicViews::Next (Iterator& i) { i.SetValue(Elem(i)->Next()); }
void GraphicViews::Prev (Iterator& i) { i.SetValue(Elem(i)->Prev()); }
boolean GraphicViews::Done (Iterator i) { return Elem(i) == _views->End(); }
GraphicView* GraphicViews::GetView (Iterator i) { return this->View(Elem(i)); }

void GraphicViews::SetView (GraphicView* gv, Iterator& i) {
    i.SetValue(_views->Find(gv));
}

Selection* GraphicViews::SelectAll () {
    Iterator i;
    Selection* selection = new Selection;
    
    for (First(i); !Done(i); Next(i)) {
        selection->Append(GetView(i));
    }
    return selection;
}

Selection* GraphicViews::ViewContaining (Coord x, Coord y) {
    Selection* s = new Selection;
    PointObj pt(x, y);
    Graphic* g = GetGraphic()->LastGraphicContaining(pt);

    if (g != nil) {
        GraphicView* gv = GetGraphicView(g);

        if (gv != nil) {
            s->Append(gv);
        }
    }
    return s;
}

Selection* GraphicViews::ViewsContaining (Coord x, Coord y) {
    Iterator i;
    Selection* s = new Selection;
    PointObj pt(x, y);

    for (First(i); !Done(i); Next(i)) {
        GraphicView* view = GetView(i);

        if (view->GetGraphic()->Contains(pt)) {
            s->Append(view);
        }
    }
    return s;
}

Selection* GraphicViews::ViewIntersecting (
    Coord x0, Coord y0, Coord x1, Coord y1
) {
    Selection* s = new Selection;
    BoxObj b(x0, y0, x1, y1);
    Graphic* g = GetGraphic()->LastGraphicIntersecting(b);

    if (g != nil) {
        GraphicView* gv = GetGraphicView(g);

        if (gv != nil) {
            s->Append(gv);
        }
    }
    return s;
}

Selection* GraphicViews::ViewsIntersecting (
    Coord x0, Coord y0, Coord x1, Coord y1
) {
    Iterator i;
    Selection* s = new Selection;
    BoxObj b(x0, y0, x1, y1);

    for (First(i); !Done(i); Next(i)) {
        GraphicView* view = GetView(i);

        if (view->GetGraphic()->Intersects(b)) {
            s->Append(view);
        }
    }
    return s;
}

Selection* GraphicViews::ViewsWithin (Coord x0, Coord y0, Coord x1, Coord y1){
    Iterator i;
    Selection* s = new Selection;
    BoxObj b(x0, y0, x1, y1);

    for (First(i); !Done(i); Next(i)) {
        GraphicView* view = GetView(i);
        BoxObj tmpbox;
        view->GetGraphic()->GetBox(tmpbox);

        if (tmpbox.Within(b)) {
            s->Append(view);
        }
    }
    return s;
}

Graphic* GraphicViews::GetGraphic () {
    Graphic* g = GraphicView::GetGraphic();
    
    if (g == nil) {
        g = new Picture;
        Iterator i;

        for (First(i); !Done(i); Next(i)) {
            g->Append(GetView(i)->GetGraphic());
        }
        SetGraphic(g);
    }
    return g;
}

GraphicComps* GraphicViews::GetGraphicComps () {
    return (GraphicComps*) GetSubject();
}    

void GraphicViews::Add (GraphicView* view) {
    Graphic* g = view->GetGraphic();
    Graphic* parent = GetGraphic();
    UList* rv = new UList(view);

    _views->Append(rv);
    parent->Append(g);
    SetParent(view, this);
}

void GraphicViews::Append (GraphicView* view) {
    Graphic* g = view->GetGraphic();
    Graphic* parent = GetGraphic();
    UList* rv = new UList(view);

    _views->Append(rv);
    parent->Append(g);
    SetParent(view, this);
}

void GraphicViews::InsertBefore (Iterator i, GraphicView* view) {
    Graphic* g = view->GetGraphic();
    Graphic* parent = GetGraphic();
    UList* r = Elem(i);
    UList* rv = new UList(view);

    r->Append(rv);

    if (r == _views->End()) {
        parent->Append(g);

    } else {
        Iterator j;
        parent->SetGraphic(this->View(r)->GetGraphic(), j);
        parent->InsertBefore(j, g);
    }
    SetParent(view, this);
}

void GraphicViews::Remove (Iterator& i) {
    UList* doomed = Elem(i);
    GraphicView* view = GetView(i);
    Graphic* g = view->GetGraphic();
    Graphic* parent = GetGraphic();

    Next(i);
    view->EraseHandles();
    _views->Remove(doomed);
    parent->Remove(g);
    SetParent(view, nil);
    delete doomed;
}

void GraphicViews::DeleteView (Iterator& i) {
    UList* doomed = Elem(i);
    GraphicView* view = GetView(i);
    Graphic* g = view->GetGraphic();
    Graphic* parent = GetGraphic();

    Next(i);
    IncurDamage(g);
    view->EraseHandles();
    _views->Remove(doomed);
    parent->Remove(g);
    delete doomed;
    delete view;
}

inline boolean ConnectorIntersects (
    Coord l, Coord b, Coord r, Coord t, ConnectorView* cv
) {
    BoxObj box(l, b, r, t);
    return cv->GetGraphic()->Intersects(box);
}

ConnectorView* GraphicViews::ConnectorIntersecting (
    Coord l, Coord b, Coord r, Coord t
) {
    UList* rv;
    Iterator i;
    GraphicView* gv, *subgv;
    ConnectorView* cv;

    for (rv = _views->Last(); rv != _views->End(); rv = rv->Prev()) {
        gv = this->View(rv);
        if (gv->IsA(CONNECTOR_VIEW)) {
            cv = (ConnectorView*) gv;
            if (ConnectorIntersects(l, b, r, t, cv)) {
                return cv;
            }
        }
        for (gv->Last(i); !gv->Done(i); gv->Prev(i)) {
            subgv = gv->GetView(i);

            if (subgv->IsA(CONNECTOR_VIEW)) {
                cv = (ConnectorView*) subgv;

                if (ConnectorIntersects(l, b, r, t, cv)) {
                    return cv;
                }
            }
        }
    }
    return nil;
}
