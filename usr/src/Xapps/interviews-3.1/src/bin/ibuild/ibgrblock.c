/*
 * Copyright (c) 1991 Stanford University
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
 *  GraphicBlock component implementation
 */

#include "ibclasses.h"
#include "ibcmds.h"
#include "ibcreator.h"
#include "ibdialogs.h"
#include "ibed.h"
#include "ibgraphic.h"
#include "ibgrblock.h"
#include "ibgrcomp.h"
#include "ibinteractor.h"
#include "ibmanips.h"
#include "ibrect.h"
#include "ibrubrect.h"
#include "ibtools.h"
#include "ibvars.h"
#include "ibvarviews.h"

#include <Unidraw/catalog.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/creator.h>
#include <Unidraw/iterator.h>
#include <Unidraw/manips.h>
#include <Unidraw/selection.h>
#include <Unidraw/ulist.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>
#include <Unidraw/Commands/align.h>
#include <Unidraw/Commands/colorcmd.h>
#include <Unidraw/Commands/datas.h>
#include <Unidraw/Commands/edit.h>
#include <Unidraw/Commands/macro.h>
#include <Unidraw/Graphic/polygons.h>
#include <Unidraw/Tools/tool.h>

#include <InterViews/brush.h>
#include <InterViews/event.h>
#include <InterViews/painter.h>
#include <InterViews/rubrect.h>
#include <InterViews/shape.h>
#include <InterViews/transformer.h>

#include <string.h>
#include <stream.h>

/*****************************************************************************/

static const float PAGE_WIDTH = 8.5;
static const float PAGE_HEIGHT = 11.0;

/*****************************************************************************/

class IdrawData : public VoidData {
public:
    IdrawData(IGraphicComps*);
    virtual ~IdrawData();
};

IdrawData::IdrawData(IGraphicComps* grcomp) : VoidData(grcomp) {}

IdrawData::~IdrawData() {
    IGraphicComps* top = (IGraphicComps*) _void;
    delete top;
}

/*****************************************************************************/

class GrBlockData : public lbrtData {
public:
    GrBlockData(GrBlockComp*);
    virtual ~GrBlockData();
public:
    Transformer* _tf;
};

GrBlockData::GrBlockData(GrBlockComp* grbcomp) : lbrtData(grbcomp) {
    _tf = new Transformer(grbcomp->GetGrBlockGraphic()->GetTransformer());
}
    
GrBlockData::~GrBlockData () {
    delete _tf;
}

GrBlockPicture::GrBlockPicture (CanvasVar* c, Graphic* g) : IBGraphic(c, g) {
    _grblockgr = nil;
    _clipped = true;
    _damage = false;
}

void GrBlockPicture::SetColors(PSColor*, PSColor*) {}

PSColor* GrBlockPicture::GetBgColor () {
    if (_grblockgr != nil) {
        return _grblockgr->GetBgColor();
    }
    return nil;
}

void GrBlockPicture::SetClip (boolean clipped) { _clipped = clipped; }

boolean GrBlockPicture::GetClip () { return _clipped; } 

void GrBlockPicture::SetGrBlockGraphic (GrBlockGraphic* grblockgr) {
    _grblockgr = grblockgr;
}

void GrBlockPicture::SetCanvasVar (CanvasVar* cvar) {
    IBGraphic::SetCanvasVar(cvar);
    if (_grblockgr != nil) {
        _grblockgr->SetCanvasVar(cvar);
    }
}

void GrBlockPicture::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    if (_grblockgr != nil && _clipped) {
	FullGraphic gstemp;
        concat(_grblockgr, gs, &gstemp);
        getExtentGraphic(_grblockgr, l, b, cx, cy, tol, &gstemp);

    } else {
	IBGraphic::getExtent(l, b, cx, cy, tol, gs);

    }
}

void GrBlockPicture::draw (Canvas* c, Graphic* gs) {
    Coord l, b, r, t;

    if (_grblockgr != nil && _clipped) {
	getBox(l, b, r, t, gs);

	if (_clipping != nil) {
	    BoxObj* src1 = new BoxObj(l, b, r, t);
	    BoxObj* src2 = _clipping;
	    BoxObj dest = *src1 - *src2;
	    
	    _p->Clip(c, dest._left, dest._bottom, dest._right, dest._top);
	    _clipping = &dest;
	    IBGraphic::draw(c, gs);
	    _clipping = src2; 
	    _p->Clip(c, src2->_left, src2->_bottom, src2->_right, src2->_top);
	    delete src1;
	} else {
	    _clipping = new BoxObj(l, b, r, t);
	    _p->Clip(c, l, b, r, t);
	    IBGraphic::draw(c, gs);
	    _p->NoClip();
	    delete _clipping;
	    _clipping = nil;
	}
    } else {
	IBGraphic::draw(c, gs);
    }
}

void GrBlockPicture::drawClipped (
    Canvas* c, Coord l, Coord b, Coord r, Coord t, Graphic* gs
) {
    Graphic::drawClipped(c, l, b, r, t, gs);
}

void GrBlockPicture::concatGS (Graphic* a, Graphic* b, Graphic* dest) {
    IBGraphic::concatGS(a, b, dest);
    dest->FillBg(UNDEF);
    dest->SetColors(nil, nil);
    dest->SetFont(nil);
    dest->SetBrush(nil);
}

/*****************************************************************************/

GrBlockComp::GrBlockComp (
    GrBlockGraphic* g
) : MonoSceneComp(new GrBlockPicture) {
    if (g != nil) {

        GrBlockPicture* gp = (GrBlockPicture*) GetGraphic();
        gp->Append(g);
        gp->SetGrBlockGraphic(g);

        GetClassNameVar()->SetName("GraphicBlock");
        GetClassNameVar()->SetBaseClass("GraphicBlock");
    }
    _top = new IGraphicComps;
    _top->Instantiate();
    MonoSceneComp::Append(_top);
    _top->GetGraphic()->SetTag(this);
    _grblockgr = g;
}

GrBlockComp::~GrBlockComp () {
    MonoSceneComp::Remove(_top);
    delete _top;
}

ClassId GrBlockComp::GetClassId () { return GRBLOCK_COMP; }

boolean GrBlockComp::IsA (ClassId id) {
    return GRBLOCK_COMP == id || MonoSceneComp::IsA(id);
}

void GrBlockComp::SetGrBlockGraphic(GrBlockGraphic* g) {
    GetGraphic()->Remove(_grblockgr);
    GetGraphic()->Append(g);
    delete _grblockgr;
    _grblockgr = g;
}

void GrBlockComp::Instantiate() {
    if (_instanceNameVar == nil) {
        MonoSceneComp::Instantiate();
    } else {
        MonoSceneComp::Instantiate();
    }
}

void GrBlockComp::Interpret(Command* cmd) {
    if (cmd->IsA(IDRAW_CMD)) {
        IdrawData* idata = (IdrawData*) cmd->Recall(this);
        if (idata == nil) {
            cmd->Store(this, new IdrawData(_top));
        } else {
            MonoSceneComp::Remove(_top);
            IGraphicComps* top = (IGraphicComps*) idata->_void;
            idata->_void = _top;
            _top = top;
            MonoSceneComp::Append(_top);
            Propagate(cmd);
        }

    } else if (cmd->IsA(ALIGN_CMD)) {
        AlignCmd* alignCmd = (AlignCmd*) cmd;
        Alignment al;
	GrBlockGraphic* gg = GetGrBlockGraphic();
	cmd->Store(this, new VoidData((void*) gg->GetAlignment()));
        alignCmd->GetAlignment(al, al);
	gg->SetAlignment(al);

        Picture* topgr = (Picture*) _top->GetGraphic();
        _grblockgr->Align(al, topgr, al);
        SubNotify();
        Propagate(cmd);

    } else if (cmd->IsA(UNGROUP_CMD)) {
        Component* comp = cmd->GetEditor()->GetComponent();
        if (comp == (Component*) this) {
            GraphicComps::Interpret(cmd);
            Propagate(cmd);
        }
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

        for (cb->First(i); !cb->Done(i);) {
            GraphicComp* kid = cb->GetComp(i);
            if (kid->IsA(INTERACTOR_COMP)) {
                cb->Remove(i);
            } else {
                cb->Next(i);
            }
        }
        GraphicComps::Interpret(cmd);
        for (cb->First(i); !cb->Done(i); cb->Next(i)) {
            IComp* kid = (IComp*)cb->GetComp(i);
            kid->Instantiate();
        }
        Propagate(cmd);

    } else if (
        cmd->IsA(SCAN_CMD) || cmd->IsA(GETCONFLICT_CMD) || cmd->IsA(IDMAP_CMD)
    ) {
        MonoSceneComp::Interpret(cmd);

    } else if (cmd->IsA(COLOR_CMD)) {
        ColorCmd* colorCmd = (ColorCmd*) cmd;
        PSColor* fg = colorCmd->GetFgColor();
        PSColor* bg = colorCmd->GetBgColor();
        if (fg == nil) {
            bg = (bg == nil) ? _grblockgr->GetBgColor() : bg;
            cmd->Store(
                this, new ColorData(
                    _grblockgr->GetFgColor(), _grblockgr->GetBgColor()
                )
            );
            _grblockgr->SetColors(fg, bg);
            Notify();
            Propagate(cmd);
        }
    } else if (cmd->IsA(INFO_CMD)) {
        MonoSceneComp::Interpret(cmd);

    } else if (cmd->IsA(COMPCHECK_CMD)) {
        CompCheckCmd* ccmd = (CompCheckCmd*) cmd;
        Iterator i;
        for (First(i); !Done(i) && ccmd->IsOK(); Next(i)) {
            GetComp(i)->Interpret(cmd);
        }
    } else if (!cmd->IsA(FONT_CMD) && !cmd->IsA(PLACE_CMD)) {
        InteractorComp::Interpret(cmd);
    }
}

void GrBlockComp::Uninterpret(Command* cmd) {
    if (cmd->IsA(IDRAW_CMD)) {
        IdrawData* idata = (IdrawData*) cmd->Recall(this);
        if (idata != nil) {
            MonoSceneComp::Remove(_top);
            IGraphicComps* top = (IGraphicComps*) idata->_void;
            idata->_void = _top;
            _top = top;
            MonoSceneComp::Append(_top);
            Unpropagate(cmd);
        }

    } else if (cmd->IsA(ALIGN_CMD)) {
        VoidData* vd = (VoidData*) cmd->Recall(this);
	Alignment al = (Alignment) vd->_void;
	GrBlockGraphic* gg = GetGrBlockGraphic();
	gg->SetAlignment(al);

        Picture* topgr = (Picture*) _top->GetGraphic();
        _grblockgr->Align(al, topgr, al);
        SubNotify();
        if (cmd->GetClipboard()->Includes(this)) {
            Unpropagate(cmd);
        }

    } else if (cmd->IsA(UNGROUP_CMD)) {
        Component* comp = cmd->GetEditor()->GetComponent();
        if (comp == (Component*) this) {
            GraphicComps::Uninterpret(cmd);
            Unpropagate(cmd);
        }
    } else if (cmd->IsA(COLOR_CMD)) {
        ColorData* cd = (ColorData*) cmd->Recall(this);

        if (cd != nil) {
            _grblockgr->SetColors(cd->_fg, cd->_bg);
            Notify();
            Unpropagate(cmd);
        }
    } else if (cmd->IsA(INFO_CMD)) {
        MonoSceneComp::Uninterpret(cmd);

    } else if (!cmd->IsA(FONT_CMD) && !cmd->IsA(PLACE_CMD)) {
        InteractorComp::Uninterpret(cmd);
    }
}

void GrBlockComp::First(Iterator& i) {
    _top->First(i);
}
 
void GrBlockComp::Last(Iterator& i) {
    _top->Last(i);
}
 
void GrBlockComp::Next(Iterator& i) {
    _top->Next(i);
}
 
void GrBlockComp::Prev(Iterator& i) {
    _top->Prev(i);
}
 
boolean GrBlockComp::Done(Iterator i) {
    return _top->Done(i);
}
 
boolean GrBlockComp::IsEmpty() {
    Iterator i;
    _top->First(i);
    return _top->Done(i);
}
 
GraphicComp* GrBlockComp::GetComp(Iterator i) {
    return _top->GetComp(i);
}
 
void GrBlockComp::SetComp(GraphicComp* comp, Iterator& i) {
    _top->SetComp(comp, i);
}
 
void GrBlockComp::Append(GraphicComp* comp) {
    _top->Append(comp);
}

void GrBlockComp::Prepend(GraphicComp* comp) {
    _top->Prepend(comp);
}

void GrBlockComp::InsertBefore(Iterator i, GraphicComp* comp) {
    _top->InsertBefore(i, comp);
}

void GrBlockComp::InsertAfter(Iterator i, GraphicComp* comp) {
    _top->InsertAfter(i, comp);
}

void GrBlockComp::Remove(GraphicComp* comp) {
    _top->Remove(comp);
}

void GrBlockComp::Remove(Iterator& i) {
    _top->Remove(i);
}

void GrBlockComp::Bequeath () {
    Graphic* root = GetGraphic();
    root->Bequeath();
    _top->Bequeath();
}
 
void GrBlockComp::StoreCanvas(Command* cmd) {
    GrBlockData* prevData = (GrBlockData*) cmd->Recall(this);
    Iterator i;
    if (prevData == nil && GetCanvasVar() != nil) {
	GrBlockGraphic* gr = GetGrBlockGraphic();
        GrBlockData* grblockbox = new GrBlockData(this);
        cmd->Store(this, grblockbox);
    }
}

void GrBlockComp::RestoreCanvas(Command* cmd) {
    Iterator i;
    GrBlockData* d = (GrBlockData*) cmd->Recall(this);
    if (d != nil) {
        Place(this, d->_l, d->_b, d->_r-1, d->_t-1);
        *GetShapeVar()->GetShape() = *d->_ibshape;
        GetGraphic()->SetTransformer(new Transformer(d->_tr));
        GetGrBlockGraphic()->SetTransformer(new Transformer(d->_tf));
        Notify();
    }
}

void GrBlockComp::Reconfig() {
    Coord l, b, r, t;
    _top->GetGraphic()->GetBox(l, b, r, t);
    Shape* shape = GetShapeVar()->GetShape();
    shape->width = r - l - 2;
    shape->height = t - b - 2;
}

void GrBlockComp::Resize () {
    Picture* topgr = (Picture*) _top->GetGraphic();
    Alignment al = _grblockgr->GetAlignment();

     if (!topgr->IsEmpty()) {
        topgr->Align(al, _grblockgr, al);
    }
    SubNotify();
}

void GrBlockComp::SubNotify () {
    Iterator i;
    Picture* topgr = (Picture*) _top->GetGraphic();

    topgr->Bequeath();
    for (First(i); !Done(i); Next(i)) {
        GetComp(i)->Notify();
    }
}

void GrBlockComp::Read(istream& in) {
    Catalog* catalog = unidraw->GetCatalog();
    MonoSceneComp::Read(in);

    ClassId id;
    in >> id;
    _grblockgr = (GrBlockGraphic*) catalog->GetCreator()->Create(id);
    _grblockgr->Read(in);
    _grblockgr->SetCanvasVar(GetCanvasVar());

    GrBlockPicture* fp = (GrBlockPicture*) GetGraphic();
    fp->Append(_grblockgr);
    fp->SetGrBlockGraphic(_grblockgr);
    _top->GetGraphic()->SetTag(this);
}

void GrBlockComp::Write(ostream& out) {
    ClassId id;
    Catalog* catalog = unidraw->GetCatalog();
    MonoSceneComp::Write(out);
    id = _grblockgr->GetClassId();
    out << " " << id << " ";
    _grblockgr->Write(out);
}

void GrBlockComp::ReadGraphicComp(const char* filename) {
    IComp::SetRelease(false);
    IBCreator::SetLock(false);
    Catalog* catalog = unidraw->GetCatalog();
    float cx, cy, ncx, ncy;

    Picture* topgr = (Picture*) _top->GetGraphic();
    if (!topgr->IsEmpty()) {
        topgr->GetCenter(cx, cy);
    } else {
        _grblockgr->GetCenter(cx, cy);
    }

    MonoSceneComp::Remove(_top);
    catalog->Forget(_top, filename);
    catalog->Retrieve(filename, (Component*&) _top);
    MonoSceneComp::Append(_top);
    _top->Instantiate();
    topgr = (Picture*) _top->GetGraphic();
    topgr->GetCenter(ncx, ncy);
    topgr->Translate(cx-ncx, cy-ncy);
    topgr->SetTag(this);
    IBCreator::SetLock(true);
    IComp::SetRelease(true);
}

void GrBlockComp::WriteGraphicComp(const char* filename) {
    IComp::SetRelease(false);
    Catalog* catalog = unidraw->GetCatalog();
    const float w = round(PAGE_WIDTH * inches);
    const float h = round(PAGE_HEIGHT * inches);
    float cx, cy;
    MonoSceneComp::Remove(_top);
    Graphic* topgr = _top->GetGraphic();
    topgr->GetCenter(cx, cy);
    topgr->Translate(w/2-cx, h/2-cy);
    catalog->Save(_top, filename);
    topgr->Translate(cx-w/2, cy-h/2);
    MonoSceneComp::Append(_top);
    IComp::SetRelease(true);
}

/*****************************************************************************/

GrBlockComp* GrBlockView::GetGrBlockComp() {
    return (GrBlockComp*) GetSubject();
}

GrBlockView::GrBlockView (GrBlockComp* subj) : MonoSceneView(subj) { }

GrBlockView::~GrBlockView () {
    GetGraphic()->Remove(_grblockgr);
    delete _grblockgr;
}

ClassId GrBlockView::GetClassId () { return GRBLOCK_VIEW; }

boolean GrBlockView::IsA (ClassId id) {
    return GRBLOCK_VIEW == id || MonoSceneView::IsA(id);
}

Graphic* GrBlockView::GetGraphic () {
    Graphic* g = GraphicView::GetGraphic();

    if (g == nil) {
	GrBlockComp* grbcomp = GetGrBlockComp();
	_grblockgr = (GrBlockGraphic*) grbcomp->GetGrBlockGraphic()->Copy();
	GrBlockPicture* grblock = new GrBlockPicture;
	grblock->Append(_grblockgr);
	grblock->SetGrBlockGraphic(_grblockgr);

	g = grblock;
        SetGraphic(g);
    }
    return g;
}

void GrBlockView::Update () {
    GrBlockPicture* grbcomp = (GrBlockPicture*) GetGrBlockComp()->GetGraphic();
    GrBlockPicture* grbview = (GrBlockPicture*) GetGraphic();
    GrBlockGraphic* grblockgr = GetGrBlockComp()->GetGrBlockGraphic();

    Viewer* viewer = GetViewer();
    if (viewer != nil && viewer->GetGraphicView() == this) {
        boolean clipped = grbview->GetClip();
        if (clipped) {
            grbview->SetClip(false);
            IncurDamage(grbview);
            grbview->Remove(_grblockgr);
        }
    } else {
        if (
            Different(grbcomp, grbview) || Different(grblockgr, _grblockgr)
        ) {
            IncurDamage(grbview);
            *(Graphic*)grbview = *(Graphic*)grbcomp;
            *(Graphic*)_grblockgr = *(Graphic*)grblockgr;
            UpdateCanvasVar();
            IncurDamage(grbview);
        }
    }
    GraphicViews::Update();
}

Manipulator* GrBlockView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Rubberband* rub = nil;
    Manipulator* m = nil;
    Coord l, b, r, t;
    int ixcon, iycon;

    l = 0, b = 0, r = 1, t = 1;
    rel->TransformRect(l, b, r, t);
    ixcon = r-l, iycon = t-b;

    if (tool->IsA(IBGRAPHIC_COMP_TOOL)) {
	m = InteractorView::CreateManipulator(v, e, rel, tool);

    } else if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        rub = new ConstrainRect(
	    nil, nil, e.x, e.y, e.x, e.y, 0, 0, ixcon, iycon 
	);
        m = new DragManip(v, rub, rel, tool, XYEqual);

    } else if (tool->IsA(STRETCH_TOOL)) {
	m = InteractorView::CreateManipulator(v, e, rel, tool);
	DragManip* dm = (DragManip*) m;
	DragConstraint dc = dm->GetConstraint();
	RubberRect* rr = (RubberRect*) dm->GetRubberband();
	rr->GetOriginal(l, b, r, t);
	delete dm;

        rub = new ConstrainRect(
	    nil, nil, l, b, r, t, 0, 0, ixcon, iycon 
	);
        m = new DragManip(
	    v, rub, rel, tool, DragConstraint(dc | Gravity), r, t
	);
    } else if (tool->IsA(EXAMINE_TOOL)) {
        Editor* ed = v->GetEditor();
        ExamineMenu* popup = new ExamineMenu;
        ExamineTool* etool = (ExamineTool*) tool;
        Selection* selPath = etool->GetSelPath();

        Control* info = etool->CreateInfoEntry(selPath, ed);
        Control* att = etool->CreatePropsEntry(selPath, ed);
        if (info != nil) {
            popup->Include(info);
        } else {
            return nil;
        }
        
        if (att != nil) {
            popup->Include(att);
        }
        popup->Include(
            new CommandItem(
                " Graphics ", Center, new IdrawCmd(ed, GetGrBlockComp())
            )
        );
        m = new PopupManip(v, popup, tool);

    } else if (tool->IsA(RESHAPE_TOOL)) {
	m = SceneView::CreateManipulator(v, e, rel, tool);

    } else {
	m = InteractorView::CreateManipulator(v, e, rel, tool);
    }
    return m;
}

Command* GrBlockView::InterpretManipulator (Manipulator* m) {
    Command* cmd = nil;
    Tool* tool = m->GetTool();

    if (tool->IsA(IBGRAPHIC_COMP_TOOL)) {
	cmd = InteractorView::InterpretManipulator(m);

    } else if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        DragManip* dm = (DragManip*) m;
        IBEditor* ed = (IBEditor*) dm->GetViewer()->GetEditor();

        Tool* tool = dm->GetTool();
        Transformer* rel = dm->GetTransformer();
        RubberRect* rubberRect = (RubberRect*) dm->GetRubberband();
        Coord x0, y0, x1, y1;
        rubberRect->GetCurrent(x0, y0, x1, y1);
        NormalRect(x0, y0, x1, y1);

        if (rel != nil) {
            rel->InvTransformRect(x0, y0, x1, y1);
        }
	GetABSCoord(ed, x0, y0, x1, y1);
        GraphicComp* gcomp = CreateProtoComp(ed, x0, y0, x1, y1);

	cmd = new MacroCmd(
            ed, new PasteCmd(ed, new Clipboard(gcomp)),
            new PlaceCmd(ed, x0, y0, x1-1, y1-1, new Clipboard(gcomp))
        );

    } else if (tool->IsA(RESHAPE_TOOL)){
        cmd = SceneView::InterpretManipulator(m);

    } else {
        cmd = InteractorView::InterpretManipulator(m);
    }
    return cmd;
}

GraphicComp* GrBlockView::CreateProtoComp (
    Editor* ed, Coord x0, Coord y0, Coord x1, Coord y1
) {
    ColorVar* colVar = (ColorVar*) ed->GetState("ColorVar");

    GrBlockComp* gcomp = (GrBlockComp*) GetGrBlockComp()->Copy();
    GrBlockGraphic* grblockgr = gcomp->GetGrBlockGraphic();
    grblockgr->SetColors(nil, colVar->GetBgColor());

    SF_Rect* rect = new SF_Rect(x0, y0, x1, y1, stdgraphic);
    rect->SetPattern(psclear);
    IRectComp* rectcomp = new IRectComp(rect);
    rectcomp->Instantiate();
    gcomp->GetTop()->Append(rectcomp);
    return gcomp;
}
/*****************************************************************************/

boolean GrBlockCode::IsA (ClassId id) {
    return GRBLOCK_CODE == id || MonoSceneCode::IsA(id);
}

ClassId GrBlockCode::GetClassId () { return GRBLOCK_CODE; }
GrBlockCode::GrBlockCode (GrBlockComp* subj) : MonoSceneCode(subj) {}

void GrBlockCode::Update () {
    MonoSceneCode::Update();
    InteractorComp* subj = GetIntComp();
    Graphic* gr = subj->GetGraphic();
    gr->SetColors(nil, gr->GetBgColor());
    gr->SetFont(nil);
}

GrBlockComp* GrBlockCode::GetGrBlockComp () {
    return (GrBlockComp*) GetSubject();
}

boolean GrBlockCode::Definition (ostream& out) {
    boolean ok = true;
    Iterator i;

    char coreclass[CHARBUFSIZE];
    GrBlockComp* grbcomp = GetGrBlockComp();
    SubclassNameVar* snamer = grbcomp->GetClassNameVar();
    MemberNameVar* mnamer = grbcomp->GetMemberNameVar();
    const char* subclass = snamer->GetName();
    const char* baseclass = snamer->GetBaseClass();
    const char* mname = mnamer->GetName();

    GetCoreClassName(coreclass);
    if (_emitProperty) {
        ok = ok && CodeView::Definition(out);

    } else if (
        _emitInstanceDecls || _emitForward || 
        _emitClassHeaders || _emitHeaders
    ) {
        ok = ok && CodeView::Definition(out);
        ok = ok && Iterate(out);

    } else if (_emitExpHeader) {
        if (!snamer->IsSubclass()) {
            if (_scope && mnamer->GetExport()&&!_namelist->Search("grblock")) {
                _namelist->Append("grblock");
                out << "#include <Unidraw/Graphic/grblock.h> \n";
            }
        } else {
            ok = ok && CodeView::Definition(out);
        }
        ok = ok && Iterate(out);

    } else if (_emitCorehHeader) {
        if (snamer->IsSubclass() && strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("grblock")) {
                _namelist->Append("grblock");
                out << "#include <Unidraw/Graphic/grblock.h>\n";
            }
        }
        ok = ok && Iterate(out);

    } else if (_emitInstanceInits) {
        if (!_instancelist->Find((void*)mname)) {
            _instancelist->Append(new UList((void*)mname));

            ok = ok && EmitGraphicState(out);

            Alignment al = grbcomp->GetGrBlockGraphic()->GetAlignment();
            BeginInstantiate(out);
            out << "(";
            InstanceName(out);
            out << "nil, 0, ";
            ok = ok && Align(al, out);
            out << ")";
            EndInstantiate(out);

            for(First(i); !Done(i); Next(i)) {
                CodeView* kid = (CodeView*) GetView(i);
                ok = ok && EmitInstanceDecls(kid, out);
            }
            ok = ok && Iterate(out);
            out << "    Picture* " << mname << "_pic = new Picture;\n";
            for(First(i); !Done(i); Next(i)) {
                CodeView* kid = (CodeView*) GetView(i);
                MemberNameVar* kmnamer = kid->GetIComp()->GetMemberNameVar();
                const char* kmname = kmnamer->GetName();
                out << "    " << mname << "_pic->Append(";
                out << kmname << ");\n";
            }
            out << "    " << mname << "->SetGraphic(" << mname << "_pic);\n";
        }

    } else if (
	_emitBSDecls || _emitBSInits ||
	_emitFunctionDecls || _emitFunctionInits
    ) {
	ok = true;

    } else if (
        _emitCoreDecls || _emitCoreInits || _emitClassDecls || _emitClassInits
    ) {
	ok = ok && CodeView::Definition(out);
        ok = ok && Iterate(out);
        
    } else if (_emitMain) {
	ok = ok && CodeView::Definition(out);
        
    }
    return out.good() && ok;
}

boolean GrBlockCode::CoreConstDecls(ostream& out) { 
    out << "(const char*, Graphic*, Coord pad, Alignment);\n";
    return out.good();
}

boolean GrBlockCode::CoreConstInits(ostream& out) {
    InteractorComp* icomp = GetIntComp();
    SubclassNameVar* snamer = icomp->GetClassNameVar();
    const char* baseclass = snamer->GetBaseClass();
    const char* subclass = snamer->GetName();

    out << "(\n    const char* name, Graphic* gr, Coord pad, Alignment al\n)";
    out << " : ";
    out << baseclass << "(name, gr, pad, al) {\n";
    out << "    SetClassName(\"" << subclass << "\");\n";
    out << "}\n\n";

    return out.good();
}

boolean GrBlockCode::ConstDecls(ostream& out) {
    out << "(const char*, Graphic*, Coord pad, Alignment);\n";
    return out.good();
}

boolean GrBlockCode::ConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n    const char* name, Graphic* gr, Coord pad, Alignment al\n)";
    out << " : ";
    out << coreclass << "(name, gr, pad, al) {}\n\n";

    return out.good();
}

boolean GrBlockCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();

    if (!snamer->IsSubclass() && !_namelist->Search("grblock")) {
        _namelist->Append("grblock");
    }
    if (strcmp(snamer->GetName(), _classname) != 0) {
        if (!_namelist->Search("transformer")) {
            _namelist->Append("transformer");
            out << "#include <InterViews/transformer.h> \n";
            out << "#include <Unidraw/Graphic/grblock.h> \n";
            out << "#include <Unidraw/Graphic/pspaint.h> \n";
        }
        if (!_namelist->Search("picture")) {
            _namelist->Append("picture");
            out << "#include <Unidraw/Graphic/picture.h> \n";
        }
    }
    return out.good();
}

boolean GrBlockCode::EmitGraphicState(ostream& out) {
    Iterator i;
    boolean ok = true;
    _emitGraphicState = true;
    ok = ok && Iterate(out);
    _emitGraphicState = false;
    if (!ok && _err_count < 10) {
        strcat(_errbuf, "GraphicState initialization failed.\n");
        _err_count++;
    }
    return ok;
}

/*****************************************************************************/

GrBlockGraphic::GrBlockGraphic (
    CanvasVar* c, Graphic* g, Alignment align
) : IBGraphic(c, g) {
    _align = align;
}

void GrBlockGraphic::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    CanvasVar* cvar = GetCanvasVar();
    l = b = cx = cy = 0.0;
    if (cvar != nil) {
        CalcExtent(cvar->Width(), cvar->Height(), l,b,cx,cy,tol,gs);

    }
    tol = 0;
}

Graphic* GrBlockGraphic::Copy () {
    Graphic* copy = new GrBlockGraphic(nil, this, _align);
    return copy;
}

ClassId GrBlockGraphic::GetClassId () { return GRBLOCK_GRAPHIC; }
boolean GrBlockGraphic::IsA (ClassId id) { return GRBLOCK_GRAPHIC == id; }

void GrBlockGraphic::Read (istream& in) {
    ReadGS(in);
    float version = unidraw->GetCatalog()->FileVersion();
    if (version > 1.05) {
        in >> _align;
    }
}

void GrBlockGraphic::Write (ostream& out) {
    WriteGS(out);
    out << _align << " ";
}

void GrBlockGraphic::draw (Canvas* c, Graphic* gs) {
    Coord xmax, ymax;
    CanvasVar* cvar = GetCanvasVar();

    if (cvar != nil) {
	update(gs);
        xmax = cvar->xmax();
        ymax = cvar->ymax();
        _p->SetPattern(psclear);
        _p->ClearRect(c, 0, 0, xmax, ymax);
    }
}

void GrBlockGraphic::drawClipped (
    Canvas* c, Coord left, Coord bottom, Coord right, Coord top, Graphic* gs
) {
    Graphic::drawClipped(c, left, bottom, right, top, gs);
}
