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
 * Implementation of Viewport component and derived classes.
 */

#include "ibclasses.h"
#include "ibcmds.h"
#include "ibvars.h"
#include "ibviewport.h"

#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/iterator.h>
#include <Unidraw/ulist.h>
#include <Unidraw/catalog.h>
#include <Unidraw/Graphic/graphic.h>
#include <Unidraw/Graphic/geomobjs.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>
#include <Unidraw/creator.h>

#include <Unidraw/Commands/brushcmd.h>
#include <Unidraw/Commands/align.h>

#include <InterViews/shape.h>
#include <InterViews/painter.h>
#include <InterViews/transformer.h>
#include <InterViews/canvas.h>
#include <string.h>
#include <stream.h>
#include <stdlib.h>

/*****************************************************************************/

class ViewportData : public lbrtData {
public:
    ViewportData(ViewportComp*);
    virtual ~ViewportData();
public:
    Transformer* _tv;

};

ViewportData::ViewportData(ViewportComp* vcomp) : lbrtData(vcomp) {
    _tv = new Transformer(vcomp->GetViewportGraphic()->GetTransformer());
}
    
ViewportData::~ViewportData () {
    delete _tv;
}

class ViewportClipper : public IBGraphic {
public:
    ViewportClipper(CanvasVar* = nil, Graphic* = nil);

    virtual void SetCanvasVar(CanvasVar*);
    virtual void SetColors(PSColor* f, PSColor* b);
    virtual void SetBrush(PSBrush*);
    virtual void Bequeath();

    void SetViewportGraphic(ViewportGraphic*);
    boolean GetClip();
    void SetClip(boolean);
    
protected:
    virtual void getExtent(float&, float&, float&, float&, float&, Graphic*);
    virtual void draw(Canvas*, Graphic*);
    virtual void drawClipped(Canvas*, Coord, Coord, Coord, Coord, Graphic*);
protected:
    ViewportGraphic* _viewportgr;
    boolean _clipped;
};

void ViewportClipper::SetViewportGraphic(ViewportGraphic* vgr) {
    _viewportgr = vgr;
}

void ViewportClipper::SetClip (boolean clipped) { _clipped = clipped; }

boolean ViewportClipper::GetClip () { return _clipped; }

ViewportClipper::ViewportClipper (CanvasVar* c, Graphic* g) : IBGraphic(c, g) {
    _viewportgr = nil;
    _clipped = true;
}

void ViewportClipper::Bequeath () {
    Remove(_viewportgr);
    IBGraphic::Bequeath();
    Append(_viewportgr);
}

void ViewportClipper::SetColors(PSColor* fg, PSColor* bg) {
    IBGraphic::SetColors(fg, bg);
    if (_viewportgr != nil) {
        _viewportgr->SetColors(fg, bg);
    }
} 

void ViewportClipper::SetBrush(PSBrush*) { } 

void ViewportClipper::SetCanvasVar(CanvasVar* cvar) {
    IBGraphic::SetCanvasVar(cvar);
    if (_viewportgr != nil) {
        _viewportgr->SetCanvasVar(cvar);
    }
}

void ViewportClipper::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    if (_viewportgr != nil && _clipped) {
	FullGraphic gstemp;
        concat(_viewportgr, gs, &gstemp);
        getExtentGraphic(_viewportgr, l, b, cx, cy, tol, &gstemp);

    } else {
	IBGraphic::getExtent(l, b, cx, cy, tol, gs);

    }
}

void ViewportClipper::draw (Canvas* c, Graphic* gs) {
    Coord l, b, r, t;

    if (_viewportgr != nil && _clipped) {
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

void ViewportClipper::drawClipped (
    Canvas* c, Coord left, Coord bottom, Coord right, Coord top, Graphic* gs
) {
    Graphic::drawClipped(c, left, bottom, right, top, gs);
}

/*****************************************************************************/

ViewportComp::ViewportComp (
    ViewportGraphic* vg
) : MonoSceneComp(new ViewportClipper) { 
    ViewportClipper* vc = (ViewportClipper*) GetGraphic();
    if (vg != nil) {
	vc->Prepend(vg);
	vc->SetViewportGraphic(vg);
    }
    GetClassNameVar()->SetName("Viewport");
    GetClassNameVar()->SetBaseClass("Viewport");
    _viewportgr = vg;
}

ClassId ViewportComp::GetClassId () { return VIEWPORT_COMP; }

boolean ViewportComp::IsA (ClassId id) { 
    return VIEWPORT_COMP==id || MonoSceneComp::IsA(id);
}

void ViewportComp::Interpret (Command* cmd) {
    if (cmd->IsA(UNGROUP_CMD) || cmd->IsA(MONOSCENE_CMD)) {
        Editor* ed = cmd->GetEditor();
        if (ed->GetComponent() != this) {
            Notify();
        }
        MonoSceneComp::Interpret(cmd);

    } else if (cmd->IsA(ALIGN_CMD)) {
        AlignCmd* alignCmd = (AlignCmd*) cmd;
        Alignment al;
        alignCmd->GetAlignment(al, al);
        InteractorComp* kid = GetKid();
        if (al != Left && al != Right && al != Top && al != Bottom) {
            cmd->Store(this, new VoidData((void*)_viewportgr->GetAlignment()));
            _viewportgr->SetAlignment(al);
            
            if (kid != nil) {
                Graphic* kidgr = kid->GetGraphic();
                _viewportgr->Align(al, kidgr, al);
                kid->Notify();
            }
        } else {
            //cmd->GetClipboard()->Remove(this);  /* not implemented */
        }
        if (!cmd->GetClipboard()->Includes(this)) {
            if (kid != nil) {
                kid->Interpret(cmd);
            }
        } else {
            Propagate(cmd);
        }

    } else if (cmd->IsA(COLOR_CMD)) {
        GraphicComps::Interpret(cmd);
        if (!cmd->GetClipboard()->Includes(this)) {
            InteractorComp* kid = GetKid();
            if (kid != nil) {
                kid->Interpret(cmd);
            }
        } else {
            Propagate(cmd);
        }
        
    } else {
        MonoSceneComp::Interpret(cmd);
    }
}

void ViewportComp::Uninterpret (Command* cmd) {
    if (cmd->IsA(UNGROUP_CMD) || cmd->IsA(MONOSCENE_CMD)) {
        Editor* ed = cmd->GetEditor();
        if (ed->GetComponent() != this) {
            Notify();
        }
        MonoSceneComp::Uninterpret(cmd);

    } else if (cmd->IsA(ALIGN_CMD)) {
        VoidData* vd = (VoidData*) cmd->Recall(this);
	Alignment al = (Alignment) vd->_void;

        InteractorComp* kid = GetKid();
        if (al != Left && al != Right && al != Top && al != Bottom) {
            _viewportgr->SetAlignment(al);
            if (kid != nil) {
                Graphic* kidgr = kid->GetGraphic();
                _viewportgr->Align(al, kidgr, al);
                kid->Notify();
            }
        }
        if (!cmd->GetClipboard()->Includes(this)) {
            if (kid != nil) {
                kid->Uninterpret(cmd);
            }
        } else {
            Unpropagate(cmd);
        }

    } else if (cmd->IsA(COLOR_CMD)) {
        GraphicComps::Uninterpret(cmd);
        if (!cmd->GetClipboard()->Includes(this)) {
            InteractorComp* kid = GetKid();
            if (kid != nil) {
                kid->Uninterpret(cmd);
            }
        } else {
            Unpropagate(cmd);
        }
        
    } else {
        MonoSceneComp::Uninterpret(cmd);
    }
}

void ViewportComp::SetViewportGraphic(ViewportGraphic* vg) {
    GetGraphic()->Remove(_viewportgr);
    GetGraphic()->Prepend(vg);
    _viewportgr = vg;
}

void ViewportComp::Reconfig () {
    MonoSceneComp::Reconfig();
    Shape* shape = GetShapeVar()->GetShape();
    shape->Rigid(hfil, hfil, vfil, vfil);
}

void ViewportComp::Resize () {
    InteractorComp* kid = GetKid();
    if (kid != nil) {
        Shape* s = kid->GetShapeVar()->GetShape();
        int w = s->width; 
        int h = s->height; 
        
        float cx, cy;
        Coord x1, y1, x2, y2;
        
        Graphic* kidgr = kid->GetGraphic();
        Alignment al = _viewportgr->GetAlignment();
        kidgr->Align(al, _viewportgr, al);
        kidgr->GetCenter(cx, cy);
        
        x1 = round(cx) - w/2;
        y1 = round(cy) - h/2;
        x2 = round(cx) + (w+1)/2;
        y2 = round(cy) + (h+1)/2;
        
        Place(kid, x1, y1, x2-1, y2-1);
    }
}

void ViewportComp::StoreCanvas(Command* cmd) {

    ViewportData* prevData = (ViewportData*) cmd->Recall(this);
    Iterator i;
    if (prevData == nil && GetCanvasVar() != nil) {
        ViewportData* framebox = new ViewportData(this);
        cmd->Store(this, framebox);
        InteractorComp* kid = GetKid();
        if (kid != nil) {
            GetKid()->StoreCanvas(cmd);
        }
    }
}

void ViewportComp::RestoreCanvas(Command* cmd) {
    Iterator i;
    ViewportData* d = (ViewportData*) cmd->Recall(this);
    if (d != nil) {
        Place(this, d->_l, d->_b, d->_r-1, d->_t-1);
        *GetShapeVar()->GetShape() = *d->_ibshape;
        Transformer* tr = GetGraphic()->GetTransformer();
        if (tr == nil) {
            GetGraphic()->SetTransformer(new Transformer(d->_tr));
        } else {
            *tr = *d->_tr;
        }
        *GetViewportGraphic()->GetTransformer() = *d->_tv;
        Notify();
        InteractorComp* kid = GetKid();
        if (kid != nil) {
            GetKid()->RestoreCanvas(cmd);
        }
    }
}

void ViewportComp::Read(istream& in) {
    MonoSceneComp::Read(in);
    Catalog* catalog = unidraw->GetCatalog();
    ClassId id;
    in >> id;
    _viewportgr = (ViewportGraphic*) catalog->GetCreator()->Create(id);
    _viewportgr->Read(in);
    _viewportgr->SetCanvasVar(GetCanvasVar());

    ViewportClipper* vc = (ViewportClipper*) GetGraphic();
    vc->Prepend(_viewportgr);
    vc->SetViewportGraphic(_viewportgr);
}

void ViewportComp::Write(ostream& out) {
    ClassId id;
    MonoSceneComp::Write(out);
    Catalog* catalog = unidraw->GetCatalog();
    id = _viewportgr->GetClassId();
    out << " " << id << " ";
    _viewportgr->Write(out);
}

/*****************************************************************************/

ViewportView::ViewportView (ViewportComp* subj) : MonoSceneView(subj) {
    _viewportgr = nil;
}

ViewportView::~ViewportView () {
    GetGraphic()->Remove(_viewportgr);
    delete _viewportgr;
}

ViewportComp* ViewportView::GetViewportComp () { 
    return (ViewportComp*) GetSubject();
}

ClassId ViewportView::GetClassId () { return VIEWPORT_VIEW; }

boolean ViewportView::IsA (ClassId id) {
    return VIEWPORT_VIEW == id || MonoSceneView::IsA(id);
}

Graphic* ViewportView::GetGraphic () {
    Graphic* g = GraphicView::GetGraphic();

    if (g == nil) {
	ViewportComp* vcomp = GetViewportComp();
	_viewportgr = (ViewportGraphic*) vcomp->GetViewportGraphic()->Copy();
	ViewportClipper* vc = new ViewportClipper;
	vc->Prepend(_viewportgr);
	vc->SetViewportGraphic(_viewportgr);
        SetGraphic(vc);
	g = vc;
    }
    return g;
}

void ViewportView::Update () {
    ViewportClipper* vcomp = (ViewportClipper*)GetViewportComp()->GetGraphic();
    ViewportClipper* vview = (ViewportClipper*) GetGraphic();
    Viewer* viewer = GetViewer();
    
    ViewportGraphic* viewportgr = GetViewportComp()->GetViewportGraphic();
    if (viewer != nil && viewer->GetGraphicView() == this) {
        boolean clipped = vview->GetClip();
        if (clipped) {
            vview->SetClip(false);
            IncurDamage(vview);
            vview->Remove(_viewportgr);
        }
    } else {
        IncurDamage(vview);
        *(Graphic*)vview = *(Graphic*)vcomp;
        *(Graphic*)_viewportgr = *(Graphic*)viewportgr;
        UpdateCanvasVar();
        IncurDamage(vview);
    }
    GraphicViews::Update();
}

boolean ViewportView::UpdateCanvasVar () {
    boolean changed = InteractorView::UpdateCanvasVar();
    IBGraphic* gview = (IBGraphic*) GetGraphic();
    _viewportgr->SetCanvasVar(gview->GetCanvasVar());
    return changed;
}

/*****************************************************************************/

ClassId ViewportCode::GetClassId () { return VIEWPORT_CODE; }

boolean ViewportCode::IsA (ClassId id) { 
    return VIEWPORT_CODE ==id || MonoSceneCode::IsA(id);
}

ViewportCode::ViewportCode (ViewportComp* subj) : MonoSceneCode(subj) { }
ViewportComp* ViewportCode::GetViewportComp () { 
    return (ViewportComp*) GetSubject(); 
}

boolean ViewportCode::Definition (ostream& out) {
    boolean ok = true;
    CodeView* kview = GetKidView();

    if (
	_emitProperty || _emitInstanceDecls || 
        _emitForward  || _emitClassHeaders || _emitHeaders
    ) {
	ok = ok && CodeView::Definition(out);
        if (kview != nil) {
            ok = ok && kview->Definition(out);
        }

    } else if (_emitExpHeader) {
	InteractorComp* icomp = GetIntComp();
	MemberNameVar* mnamer = icomp->GetMemberNameVar();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        if (!snamer->IsSubclass()) {
            if (
                _scope && mnamer->GetExport() && !_namelist->Search("viewport")
            ) {
                _namelist->Append("viewport");
                out << "#include <InterViews/viewport.h>\n";
            }
        } else {
            ok = ok && CodeView::Definition(out);
        }
        if (kview != nil) {
            ok = ok && kview->Definition(out);
        }

    } else if (_emitCorehHeader) {
	InteractorComp* icomp = GetIntComp();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        const char* subclass = snamer->GetName();
        if (snamer->IsSubclass() && strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("viewport")) {
                _namelist->Append("viewport");
                out << "#include <InterViews/viewport.h>\n";
            }
        } else {
            if (kview != nil) {
                ok = ok && kview->Definition(out);
            }
        }
    } else if (_emitInstanceInits) {
        ViewportComp* vcomp = GetViewportComp();
        const char* mname = vcomp->GetMemberNameVar()->GetName();

        if (!_instancelist->Find((void*) mname)) {
            _instancelist->Append(new UList((void*) mname));

            Alignment al = vcomp->GetViewportGraphic()->GetAlignment();
	    BeginInstantiate(out);
            out << "(";
            InstanceName(out);
            out << "nil, ";
            ok = ok && Align(al, out);
            out << ")";
            EndInstantiate(out);
	}

        if (kview != nil) {
            ok = kview->Definition(out) && ok;
        }

	if (AllKidsDefined() && !_lock) {
	    _lock = true;
            InteractorComp* kid = vcomp->GetKid();
            if (kid != nil) {
                const char* instance = kid->GetMemberNameVar()->GetName();
                out << "    " << mname << "->Insert(";
                out << instance << ");\n";
            }
	}

    } else if (
	_emitBSDecls || _emitBSInits || 
	_emitFunctionDecls || _emitFunctionInits ||
        _emitCreatorHeader || _emitCreatorSubj || _emitCreatorView
    ) {
        if (kview != nil) {
            ok = ok && kview->Definition(out);
        }

    } else if (
        _emitCoreDecls || _emitCoreInits || _emitClassDecls || _emitClassInits
    ) {
	ok = ok && CodeView::Definition(out);
        if (kview != nil) {
            ok = ok && kview->Definition(out);
        }
        
    } else if (_emitMain) {
	ok = ok && CodeView::Definition(out);
        
    }
    return ok;
}

boolean ViewportCode::CoreConstDecls(ostream& out) { 
    out << "(const char*, Interactor*, Alignment);\n";
    return out.good();
}

boolean ViewportCode::CoreConstInits(ostream& out) {
    InteractorComp* icomp = GetIntComp();
    SubclassNameVar* snamer = icomp->GetClassNameVar();
    const char* baseclass = snamer->GetBaseClass();
    const char* subclass = snamer->GetName();

    out << "(\n    const char* name, Interactor* i, Alignment a\n) : ";
    out << baseclass << "(name, i, a) {\n";
    out << "    SetClassName(\"" << subclass << "\");\n";
    out << "}\n\n";
    return out.good();
}

boolean ViewportCode::ConstDecls(ostream& out) {
    out << "(const char*, Interactor*, Alignment);\n";
    return out.good();
}

boolean ViewportCode::ConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n    const char* name, Interactor* i, Alignment a\n) : ";
    out << coreclass << "(name, i, a) {}\n\n";
    return out.good();
}

boolean ViewportCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();

    if (!snamer->IsSubclass() && !_namelist->Search("viewport")) {
        _namelist->Append("viewport");
        out << "#include <InterViews/viewport.h> \n";
    }
    return out.good();
}

/*************************************************************************/

ViewportGraphic::ViewportGraphic (
    CanvasVar* c, Graphic* g, Alignment align
) : IBGraphic(c, g) {
    _align = align;
}

void ViewportGraphic::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    CanvasVar* cvar = GetCanvasVar();
    l = b = cx = cy = 0.0;
    if (cvar != nil) {
        CalcExtent(cvar->Width(), cvar->Height(), l,b,cx,cy,tol,gs);
    }
    tol = 0;
}

Graphic* ViewportGraphic::Copy () {
    Iterator i;
    ViewportGraphic* copy = new ViewportGraphic(nil, this, _align);
    return copy;
}

void ViewportGraphic::Read (istream& in) {
    ReadGS(in);
    float version = unidraw->GetCatalog()->FileVersion();
    if (version > 1.05) {
        in >> _align;
    }
}

void ViewportGraphic::Write (ostream& out) {
    WriteGS(out);
    out << _align << " ";
}

ClassId ViewportGraphic::GetClassId () { return VIEWPORT_GRAPHIC; }
boolean ViewportGraphic::IsA (ClassId id) { return VIEWPORT_GRAPHIC == id; }

void ViewportGraphic::draw (Canvas* c, Graphic* gs) {
    Coord xmax, ymax;
    CanvasVar* cvar = GetCanvasVar();

    if (cvar != nil) {
	update(gs);
        xmax = GetCanvasVar()->xmax();
        ymax = GetCanvasVar()->ymax();
	
        static Pattern* ltgray;

        if (ltgray == nil) {
            ltgray = new Pattern(Pattern::lightgray);
            ltgray->Reference();
        }

        _p->SetPattern(ltgray);
	_p->FillRect(c, 0, 0, xmax, ymax);
    }
}

void ViewportGraphic::drawClipped (
    Canvas* c, Coord left, Coord bottom, Coord right, Coord top, Graphic* gs
) {
    Graphic::drawClipped(c, left, bottom, right, top, gs);
}
