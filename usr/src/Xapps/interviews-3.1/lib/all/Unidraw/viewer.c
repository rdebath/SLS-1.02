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
 * Viewer implementation.
 */

#include <Unidraw/classes.h>
#include <Unidraw/ctrlinfo.h>
#include <Unidraw/editor.h>
#include <Unidraw/globals.h>
#include <Unidraw/grid.h>
#include <Unidraw/iterator.h>
#include <Unidraw/keymap.h>
#include <Unidraw/kybd.h>
#include <Unidraw/manips.h>
#include <Unidraw/selection.h>
#include <Unidraw/statevars.h>
#include <Unidraw/uctrl.h>
#include <Unidraw/upage.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/transforms.h>

#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/grview.h>

#include <Unidraw/Graphic/damage.h>
#include <Unidraw/Graphic/picture.h>

#include <Unidraw/Tools/tool.h>

#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/perspective.h>
#include <IV-2_6/InterViews/rubband.h>
#include <IV-2_6/InterViews/sensor.h>
#include <IV-2_6/InterViews/shape.h>
#include <IV-2_6/InterViews/textdisplay.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

#include <OS/math.h>

#include <string.h>

/*****************************************************************************/

static Painter* xorPainter;

/*****************************************************************************/

static boolean Different (Graphic* g1, Graphic* g2) {
    boolean different = true;

    if (
        g1->GetFgColor() == g2->GetFgColor() &&
        g1->GetBgColor() == g2->GetBgColor() &&
        g1->BgFilled() == g2->BgFilled() &&
        g1->GetPattern() == g2->GetPattern() &&
        g1->GetBrush() == g2->GetBrush() &&
        g1->GetFont() == g2->GetFont()
    ) {
        Transformer identity;
        Transformer* t1 = g1->GetTransformer();
        Transformer* t2 = g2->GetTransformer();

        if (t1 == t2) {
            different = false;
        } else if (t1 == nil) {
            different = *t2 != identity;
        } else if (t2 == nil) {
            different = *t1 != identity;
        } else {
            different = *t1 != *t2;
        }
    }
    return different;
}

/*****************************************************************************/

class ViewerGraphic : public Picture {
public:
    ViewerGraphic(GraphicView*);

    void Update();
private:
    void totalGSGraphic(Graphic*, Graphic&);    // should be in base class
private:
    GraphicView* _view;
};

ViewerGraphic::ViewerGraphic (GraphicView* view) {
    _view = view;
    Append(_view->GetGraphic());
}

void ViewerGraphic::Update () {
    GraphicComp* parent = (GraphicComp*) _view->GetGraphicComp()->GetParent();

    if (parent != nil) {
        FullGraphic gs;
        totalGSGraphic(parent->GetGraphic(), gs);

        if (Different(this, &gs)) {
            Damage* damage = _view->GetViewer()->GetDamage();
            
            damage->Incur(this);
            *(Graphic*)this = *(Graphic*)(&gs);
            damage->Incur(this);
        }
    }
}

void ViewerGraphic::totalGSGraphic (Graphic* g, Graphic& gs) {
    Graphic* parent = g->Parent();

    if (parent == nil) {
        concatGraphic(g, nil, g, &gs);

    } else {
        totalGSGraphic(parent, gs);
        concatGraphic(g, g, &gs, &gs);
    }
}

/*****************************************************************************/

class ViewerView : public GraphicView {
public:
    ViewerView(GraphicView*, UPage*, Grid*, Viewer*);
    virtual ~ViewerView();

    virtual void Update();
    virtual Viewer* GetViewer();
private:
    Viewer* _viewer;
    ViewerGraphic* _vg;
};

ViewerView::ViewerView (GraphicView* g, UPage* page, Grid* grid, Viewer* v) {
    Picture* p = new Picture;

    if (grid != nil) {
        p->Append(grid->GetGraphic());
    }
    p->Append(page->GetGraphic());
    p->Append(_vg = new ViewerGraphic(g));

    SetGraphic(p);
    _vg->SetTag(this);

    _viewer = v;
    Update();
}

ViewerView::~ViewerView () {
    Graphic* g = GetGraphic();
    Iterator i;
    g->First(i);

    while (!g->Done(i)) {
        g->Remove(i);
    }
    _vg->First(i);
    _vg->Remove(i);
    delete _vg;
}

void ViewerView::Update () { _vg->Update(); }
Viewer* ViewerView::GetViewer () { return _viewer; }

/*****************************************************************************/

Viewer::Viewer (
    Editor* ed, GraphicView* gv, UPage* page, Grid* grid, 
    Coord w, Coord h, Orientation orientation,
    Alignment align, Zooming zoom
) : GraphicBlock(nil, 0, align, zoom) {
    Init(ed, gv, page, grid, w, h, orientation);
}

Viewer::Viewer (
    const char* instance,
    Editor* ed, GraphicView* gv, UPage* page, Grid* grid, 
    Coord w, Coord h, Orientation orientation,
    Alignment align, Zooming zoom
) : GraphicBlock(instance, nil, 0, align, zoom) {
    Init(ed, gv, page, grid, w, h, orientation);
}

void Viewer::Init(
    Editor* ed, GraphicView* gv, UPage* page, Grid* grid,
    Coord w, Coord h, Orientation orientation
) {    
    SetClassName("Viewer");
    if (xorPainter == nil) {
        xorPainter = new Painter;
        Ref(xorPainter);
    }
    _damage = new Damage;
    _orientation = orientation;
    input = new Sensor(updownEvents);

    Init(ed, gv, page, grid);
    shape->width = w;
    shape->height = h;
    SetCanvasType(CanvasSaveContents);
}

void Viewer::Init (Editor* ed, GraphicView* gview, UPage* page, Grid* grid) {
    _editor = ed;
    _gview = gview;
    _page = page;
    _grid = grid;

    _viewerView = new ViewerView(_gview, _page, _grid, this);
    _graphic = _viewerView->GetGraphic();
    _damage->SetGraphic(_graphic);

    Reorient();
    GraphicBlock::Init();
    UpdateMagnifVar();
}

Viewer::~Viewer () {
    delete _damage;
    delete _viewerView;
    delete _page;
    delete _grid;
    delete _gview;
}

void Viewer::Reconfig () { _damage->SetPainter(output); }

void Viewer::Update () {
    Selection* s = GetSelection();
    GraphicView* view = GetGraphicView();
    Component* viewComp = view->GetGraphicComp();
    Component* edComp = _editor->GetComponent();

    if (viewComp != edComp) {
        ComponentView* newView = edComp->Create(ViewCategory());

        if (newView->IsA(GRAPHIC_VIEW)) {
            edComp->Attach(newView);
            newView->Update();
            SetGraphicView((GraphicView*) newView);

        } else {
            delete newView;
        }

    } else {
	s->Hide(this);
        _viewerView->Update();
        GraphicBlock::UpdatePerspective();
	_damage->Repair();
        s->Show(this);
    }
}

void Viewer::Draw () {
    Selection* s = GetSelection();

    GraphicBlock::Draw();
    s->Init(this);
    s->Show(this);

    _damage->Reset();
}

void Viewer::Redraw (Coord l, Coord b, Coord r, Coord t) {
    GraphicBlock::Redraw(l, b, r, t);

    xorPainter->Clip(canvas, l, b, r, t);
    GetSelection()->Show(this);
    xorPainter->NoClip();
}

void Viewer::Resize () {
    GraphicBlock::Resize();
    _damage->SetCanvas(canvas);
    GetSelection()->Init(this);
}

void Viewer::GetGraphicBox (Coord& l, Coord& b, Coord& r, Coord& t) {
    _page->GetGraphic()->GetBox(l, b, r, t);
}

void Viewer::Adjust (Perspective& np) {
    GraphicBlock::Adjust(np);
    UpdateMagnifVar();
}    

void Viewer::Handle (Event& e) {
    Tool* tool = CurTool();

    if (tool != nil && e.eventType == DownEvent) {
	switch (e.button) {
        case LEFTMOUSE:
            UseTool(tool, e);
            break;

        case MIDDLEMOUSE:
            if (e.control) {
                GrabScroll(e);
            } else {
                MomentaryUseTool(CODE_MOVE, e);
            }
            break;

        case RIGHTMOUSE:
            if (e.control) {
                RateScroll(e);
            } else {
                MomentaryUseTool(CODE_SELECT, e);
            }
            break;
	}
    }
}

void Viewer::InitRubberband (Rubberband* r) {
    r->SetPainter(xorPainter);
    r->SetCanvas(canvas);
}

void Viewer::InitTextDisplay (TextDisplay* td, Painter* p) {
    if (!p->BgFilled()) {
        p->FillBg(true);
        p->SetColors(nil, output->GetBgColor());
    }
    td->Draw(p, canvas);
}

void Viewer::IncurTextDisplayDamage (TextDisplay* td, Painter* p) {
    Coord l, b, r, t;

    td->Bounds(l, b, r, t);
    --l; --b; ++r; ++t;

    Transformer* rel = p->GetTransformer();
    if (rel != nil) rel->TransformRect(l, b, r, t);

    _damage->Incur(l, b, r, t);
}

void Viewer::SetGraphicView (GraphicView* gv) {
    Perspective np = *perspective;

    GetEditor()->GetSelection()->Clear();
    delete _viewerView;
    delete _gview;

    _gview = gv;
    _viewerView = new ViewerView(_gview, _page, _grid, this);
    _graphic = _viewerView->GetGraphic();
    _damage->SetGraphic(_graphic);
    _damage->Incur(0, 0, 0, 0);                 // for detecting Draw in Adjust

    Reorient();
    GraphicBlock::Init();
    Adjust(np);

    if (_damage->Incurred()) {                  // Adjust didn't Draw
        Draw();
    }
}

void Viewer::SetPage (UPage* page) {
    if (_page != page) {
        delete _viewerView;
        delete _page;

        Init(_editor, _gview, page, _grid);
        GraphicBlock::Update();
    }
}

void Viewer::SetGrid (Grid* grid) {
    if (_grid != grid) {
        delete _viewerView;
        delete _grid;

        Init(_editor, _gview, _page, grid);
        GraphicBlock::Update();
    }
}

void Viewer::UpdateMagnifVar () {
    MagnifVar* magnifVar = (MagnifVar*) _editor->GetState("MagnifVar");
    if (magnifVar != nil) magnifVar->SetMagnif(GetMagnification());
}

void Viewer::SetMagnification (float m) {
    GraphicBlock::SetMagnification(m);
    UpdateMagnifVar();
}    

void Viewer::Reorient () {
    if (
        _orientation == Rotated ||
        _orientation == Landscape ||
        _orientation == Horizontal
    ) {
        Coord l, b, r, t;
        GetGraphicBox(l, b, r, t);
        Graphic* g = GraphicBlock::GetGraphic();

        g->Rotate(-90., l, b);
        g->Translate(0., r-l);
    }
}    

void Viewer::SetOrientation (Orientation o) {
    if (_orientation != o) {
        _orientation = o;

        Coord l, b, r, t;
        GetGraphicBox(l, b, r, t);
        Graphic* g = GraphicBlock::GetGraphic();

        if (
            _orientation == Normal ||
            _orientation == Portrait ||
            _orientation == Vertical
        ) {
            g->Rotate(90., l, b);
            g->Translate(t-b, 0.);

        } else if (
            _orientation == Rotated ||
            _orientation == Landscape ||
            _orientation == Horizontal
        ) {
            g->Rotate(-90., l, b);
            g->Translate(0., r-l);
        }
        GraphicBlock::Update();
    }
}

Graphic* Viewer::GetGraphic () { return _gview->GetGraphic(); }
GraphicView* Viewer::GetGraphicView () { return _gview; }
UPage* Viewer::GetPage () { return _page; }
Grid* Viewer::GetGrid () { return _grid; }
Orientation Viewer::GetOrientation () { return _orientation; }
Editor* Viewer::GetEditor () { return _editor; }
Selection* Viewer::GetSelection () { return _editor->GetSelection(); }
Damage* Viewer::GetDamage () { return _damage; }

void Viewer::CenterOp () {
    Perspective np = *perspective;
    np.curx = (np.width - np.curwidth)/2;
    np.cury = (np.height - np.curheight)/2;
    Adjust(np);
}

void Viewer::Magnify (Coord left, Coord bottom, Coord right, Coord top) {
    Perspective np = *perspective;
    NormalRect(left, bottom, right, top);
    np.curx += left;
    np.cury += bottom;
    np.curwidth = Math::max(right - left, 1);
    np.curheight = Math::max(top - bottom, 1);
    Adjust(np);
}

void Viewer::ReduceToFit () {
    Perspective np = *perspective;
    np.curx = np.x0;
    np.cury = np.y0;
    np.curwidth = np.width;
    np.curheight = np.height;
    Adjust(np);
}

void Viewer::Constrain (Coord& x, Coord& y) {
    if (_grid != nil) {
        GravityVar* grav = (GravityVar*) GetEditor()->GetState("GravityVar");

        if (grav != nil && grav->IsActive()) {
            _grid->Constrain(x, y);
        }
    }
}

Tool* Viewer::CurTool () { return _editor->GetCurTool(); }

void Viewer::Manipulate (Manipulator* m, Event& e) {
    Listen(allEvents);
    m->Grasp(e);

    /*
     * boolean b is just here to workaround a cfront 3.0 bug.
     */
    boolean b = false;
    do {
        Read(e);
	b = m->Manipulating(e);
    } while (b);

    m->Effect(e);
    Listen(input);
}

static Transformer* ComputeRel (Viewer* v, Transformer* t) {
    Transformer* rel = new Transformer;
    GraphicComp* comp = v->GetGraphicView()->GetGraphicComp();
    comp->GetGraphic()->TotalTransformation(*rel);
    rel->Postmultiply(t);
    return rel;
}    

void Viewer::UseTool (Tool* t) {
    Event e;
    e.target = nil;
    e.eventType = EnterEvent;
    UseTool(t, e);
}

void Viewer::UseTool (Tool* t, Event& e) {
    Transformer* relative = ComputeRel(this, _graphic->GetTransformer());
    Manipulator* m = t->CreateManipulator(this, e, relative);

    if (m != nil) {
        Manipulate(m, e);
        Command* cmd = t->InterpretManipulator(m);

        if (cmd != nil) {
            cmd->Execute();

            if (cmd->Reversible()) {
                cmd->Log();
	    } else {
		delete cmd;
            }
        }
        delete m;
    }
    Unref(relative);
}

void Viewer::MomentaryUseTool (const char* keyCode, Event& e) {
    Tool* curTool= _editor->GetCurTool();
    const char* origCode = curTool->GetControlInfo()->GetKeyCode();
    KeyMap* keymap = _editor->GetKeyMap();
    
    if (strcmp(origCode, keyCode) == 0) {
	UseTool(CurTool(), e);		/* already using this tool */
    } else {
	keymap->Execute(keyCode);	/* engage tool momentarily */
	UseTool(CurTool(), e);
	keymap->Execute(origCode);	/* revert to original tool */
    }
}

Transformer* Viewer::GetTransformer () {
    Transformer* t = _graphic->GetTransformer();
    
    if (t == nil) {
	t = new Transformer;
	_graphic->SetTransformer(t);
        Unref(t);
    }
    return t;
}

float Viewer::LimitMagnification (float desired) {
    const float lo = 1./8.;
    const float hi = 16.;

    return (desired < lo) ? lo : (desired > hi) ? hi : desired;
}

ClassId Viewer::ViewCategory () { return COMPONENT_VIEW; }

void Viewer::Align (GraphicComp* comp, Alignment a) {
    Graphic* g = comp->GetGraphic();
    float cl, cb, cr, ct;
    g->GetBounds(cl, cb, cr, ct);

    Perspective* p = GetPerspective();
    float mag = GetMagnification();
    float vl = float(p->curx - p->x0) / mag;
    float vb = float(p->cury - p->y0) / mag;
    float vr = float(p->curx - p->x0 + p->curwidth - 1) / mag;
    float vt = float(p->cury - p->y0 + p->curheight - 1) / mag;

    float dx, dy;

    switch (a) {
    case TopLeft:
    case CenterLeft:
    case BottomLeft:
    case Left:
        dx = vl - cl;
        break;

    case TopCenter:
    case Center:
    case BottomCenter:
        dx = (vr + vl - cr - cl) / 2;
        break;

    case TopRight:
    case CenterRight:
    case BottomRight:
    case Right:
        dx = vr - cr;
        break;
    }
    
    switch (a) {
    case TopLeft:
    case TopCenter:
    case TopRight:
    case Top:
        dy = vt - ct;
        break;

    case CenterLeft:
    case Center:
    case CenterRight:
        dy = (vt + vb - ct - cb) / 2;
        break;

    case BottomLeft:
    case BottomCenter:
    case BottomRight:
    case Bottom:
        dy = vb - cb;
        break;
    }
    MoveCmd mvcmd(GetEditor(), dx, dy);
    comp->Interpret(&mvcmd);
}
