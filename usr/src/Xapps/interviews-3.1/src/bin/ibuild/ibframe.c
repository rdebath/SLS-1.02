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
 * Implementation of Frame component and derived classes.
 */

#include "ibclasses.h"
#include "ibcmds.h"
#include "ibdialogs.h"
#include "ibframe.h"
#include "ibrubrect.h"
#include "ibvars.h"
#include "ibvarviews.h"

#include <Unidraw/catalog.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/creator.h>
#include <Unidraw/editor.h>
#include <Unidraw/Graphic/graphic.h>
#include <Unidraw/iterator.h>
#include <Unidraw/manips.h>
#include <Unidraw/selection.h>
#include <Unidraw/statevars.h>
#include <Unidraw/ulist.h>
#include <Unidraw/viewer.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/Commands/brushcmd.h>
#include <Unidraw/Commands/colorcmd.h>
#include <Unidraw/Tools/grcomptool.h>

#include <InterViews/button.h>
#include <InterViews/canvas.h>
#include <InterViews/event.h>
#include <InterViews/painter.h>
#include <InterViews/shape.h>
#include <InterViews/transformer.h>
#include <string.h>
#include <stream.h>
#include <stdlib.h>

/*****************************************************************************/

class FrameData : public lbrtData {
public:
    FrameData(FrameComp*, int);
    virtual ~FrameData();
public:
    Transformer* _tf;
};

FrameData::FrameData(FrameComp* fcomp, int width) : lbrtData(fcomp) {
    _void = (void*) width;
    _tf = new Transformer(fcomp->GetFrameGraphic()->GetTransformer());
}
    
FrameData::~FrameData () {
    delete _tf;
}

class FramePicture : public IBGraphic {
public:
    FramePicture(CanvasVar* = nil, Graphic* = nil);

    virtual void SetCanvasVar(CanvasVar*);
    virtual void SetColors(PSColor* f, PSColor* b);
    virtual void SetBrush(PSBrush*);
    virtual void Bequeath();
    void SetFrameGraphic(FrameGraphic*);

    virtual void drawClipped(Canvas*, Coord, Coord, Coord, Coord, Graphic*);

protected:
    FrameGraphic* _framegr;
};

FramePicture::FramePicture (CanvasVar* c, Graphic* g) : IBGraphic(c, g) {
    _framegr = nil;
}

void FramePicture::Bequeath () {
    Remove(_framegr);
    IBGraphic::Bequeath();
    Append(_framegr);
}

void FramePicture::SetColors(PSColor* f, PSColor* b) {
    IBGraphic::SetColors(f, b);
    if (_framegr != nil) {
	_framegr->SetColors(f, b);
    }
}

void FramePicture::SetBrush(PSBrush* br) {
    if (br != nil && _framegr != nil) {
        int w = br->Width();
        _framegr->SetThickness(w, w, w, w);
    }
}

void FramePicture::SetFrameGraphic(FrameGraphic* fg) {
    _framegr = fg;
}

void FramePicture::SetCanvasVar(CanvasVar* cvar) {
    IBGraphic::SetCanvasVar(cvar);
    if (_framegr != nil) {
        _framegr->SetCanvasVar(cvar);
    }
}

void FramePicture::drawClipped (
    Canvas* c, Coord l, Coord b, Coord r, Coord t, Graphic* gs
) {
    Graphic::drawClipped(c, l, b, r, t, gs);
}

/*****************************************************************************/

FrameComp::FrameComp (
    FrameGraphic* fg
) : MonoSceneComp(new FramePicture) { 
    if (fg != nil) {
        FramePicture* fp = (FramePicture*) GetGraphic();
        GetClassNameVar()->SetName(fg->GetClassName());
        GetClassNameVar()->SetBaseClass(fg->GetClassName());
	fp->Append(fg);
	fp->SetFrameGraphic(fg);
    }
    _framegr = fg;
}

ClassId FrameComp::GetClassId () { return FRAME_COMP; }

boolean FrameComp::IsA (ClassId id) { 
    return FRAME_COMP==id || MonoSceneComp::IsA(id);
}

void FrameComp::SetFrameGraphic(FrameGraphic* fg) {
    GetClassNameVar()->SetName(fg->GetClassName());
    GetClassNameVar()->SetBaseClass(fg->GetClassName());
    GetGraphic()->Remove(_framegr);
    GetGraphic()->Append(fg);
    _framegr = fg;
}

void FrameComp::Interpret (Command* cmd) {
    if (cmd->IsA(BRUSH_CMD) && !cmd->IsA(GLUEVISIBILITY_CMD)) {
	BrushCmd* brushcmd = (BrushCmd*) cmd;
	FrameGraphic* gr = GetFrameGraphic();

	int lbrt;
	gr->GetThickness(lbrt, lbrt, lbrt, lbrt);
        cmd->Store(this, new VoidData((void*) lbrt));
	lbrt = brushcmd->GetBrush()->Width();
	gr->SetThickness(lbrt, lbrt, lbrt, lbrt);
	Reconfig();
        Notify();

        if (!cmd->GetClipboard()->Includes(this)) {
            InteractorComp* kid = GetKid();
            if (kid != nil) {
                kid->Interpret(cmd);
            }
        } else {
            Propagate(cmd);
        }

    } else if (cmd->IsA(COLOR_CMD)) {
        InteractorComp::Interpret(cmd);
        if (!cmd->GetClipboard()->Includes(this)) {
            InteractorComp* kid = GetKid();
            if (kid != nil) {
                kid->Interpret(cmd);
            }
        } else {
            Propagate(cmd);
        }
        
    } else if (cmd->IsA(UNGROUP_CMD) || cmd->IsA(MONOSCENE_CMD)) {
        Editor* ed = cmd->GetEditor();
        if (ed->GetComponent() != this) {
            Notify();
        }
        MonoSceneComp::Interpret(cmd);

    } else {
	MonoSceneComp::Interpret(cmd);
    }
}

void FrameComp::Uninterpret (Command* cmd) {
    if (cmd->IsA(BRUSH_CMD) && !cmd->IsA(GLUEVISIBILITY_CMD)) {
	FrameGraphic* gr = GetFrameGraphic();
        VoidData* vd = (VoidData*) cmd->Recall(this);
        int lbrt = (int) vd->_void;
        gr->SetThickness(lbrt, lbrt, lbrt, lbrt);
        Reconfig();
        Notify();

        if (!cmd->GetClipboard()->Includes(this)) {
            InteractorComp* kid = GetKid();
            if (kid != nil) {
                kid->Uninterpret(cmd);
            }
        } else {
            Unpropagate(cmd);
        }

    } else if (cmd->IsA(COLOR_CMD)) {
        InteractorComp::Uninterpret(cmd);
        if (!cmd->GetClipboard()->Includes(this)) {
            InteractorComp* kid = GetKid();
            if (kid != nil) {
                kid->Uninterpret(cmd);
            }
        } else {
            Unpropagate(cmd);
        }
        
    } else if (cmd->IsA(UNGROUP_CMD) || cmd->IsA(MONOSCENE_CMD)) {
        Editor* ed = cmd->GetEditor();
        if (ed->GetComponent() != this) {
            Notify();
        }
        MonoSceneComp::Uninterpret(cmd);
        
    } else {
        MonoSceneComp::Uninterpret(cmd);
    }
}

void FrameComp::Resize () {
    int w = _canvasVar->Width(), h = _canvasVar->Height();
    float cx, cy;
    int l, b, r, t;
    Coord x1, y1, x2, y2;

    InteractorComp* kid = GetKid();
    if (kid != nil) {
        Graphic* kidgr = kid->GetGraphic();
        _framegr->GetThickness(l, b, r, t);
        
        kidgr->Align(Center, _framegr, Center);
        _framegr->GetCenter(cx, cy);
        
        x1 = round(cx) - w/2 + l;
        y1 = round(cy) - h/2 + b;
        x2 = round(cx) + (w+1)/2 - r;
        y2 = round(cy) + (h+1)/2 - t;
        
        Place(kid, x1, y1, x2-1, y2-1);
    }
}

void FrameComp::Reconfig () {
    MonoSceneComp::Reconfig();
    int l, b, r, t;
    GetFrameGraphic()->GetThickness(l, b, r, t);
    Shape* shape = GetShapeVar()->GetShape();
    shape->width += l + r;
    shape->height += b + t;
}

void FrameComp::StoreCanvas(Command* cmd) {
    FrameData* prevData = (FrameData*) cmd->Recall(this);
    Iterator i;
    if (prevData == nil && GetCanvasVar() != nil) {
	int lbrt;
	FrameGraphic* gr = GetFrameGraphic();
	gr->GetThickness(lbrt, lbrt, lbrt, lbrt);
        FrameData* framebox = new FrameData(this, lbrt);
        cmd->Store(this, framebox);
        InteractorComp* kid = GetKid();
        if (kid != nil) {
            kid->StoreCanvas(cmd);
        }
    }
}

void FrameComp::RestoreCanvas(Command* cmd) {
    Iterator i;
    FrameData* d = (FrameData*) cmd->Recall(this);
    if (d != nil) {
        int lbrt;
        lbrt = (int) d->_void;
        Place(this, d->_l, d->_b, d->_r-1, d->_t-1);
        *GetShapeVar()->GetShape() = *d->_ibshape;
        Transformer* tr = GetGraphic()->GetTransformer();
        if (tr == nil) {
            GetGraphic()->SetTransformer(new Transformer(d->_tr));
        } else {
            *tr = *d->_tr;
        }
        FrameGraphic* fgr = GetFrameGraphic();
        *fgr->GetTransformer() = *d->_tf;
        fgr->SetThickness(lbrt, lbrt, lbrt, lbrt);
        Notify();
        InteractorComp* kid = GetKid();
        if (kid != nil) {
            kid->RestoreCanvas(cmd);
        }
    }
}

void FrameComp::Read(istream& in) {
    MonoSceneComp::Read(in);
    Catalog* catalog = unidraw->GetCatalog();
    ClassId id;
    in >> id;
    _framegr = (FrameGraphic*) catalog->GetCreator()->Create(id);
    _framegr->Read(in);
    _framegr->SetCanvasVar(GetCanvasVar());

    FramePicture* fp = (FramePicture*) GetGraphic();
    fp->Append(_framegr);
    fp->SetFrameGraphic(_framegr);
}

void FrameComp::Write(ostream& out) {
    ClassId id;
    MonoSceneComp::Write(out);
    Catalog* catalog = unidraw->GetCatalog();
    id = _framegr->GetClassId();
    out << " " << id << " ";
    _framegr->Write(out);
}

/*****************************************************************************/

FrameView::FrameView (FrameComp* subj) : MonoSceneView(subj) {
    _framegr = nil;
}

FrameView::~FrameView () {
    GetGraphic()->Remove(_framegr);
    delete _framegr;
}

FrameComp* FrameView::GetFrameComp () { return (FrameComp*) GetSubject();}

ClassId FrameView::GetClassId () { return FRAME_VIEW; }

boolean FrameView::IsA (ClassId id) {
    return FRAME_VIEW == id || MonoSceneView::IsA(id);
}

Graphic* FrameView::GetGraphic () {
    Graphic* g = GraphicView::GetGraphic();

    if (g == nil) {
	FrameComp* fcomp = GetFrameComp();
	_framegr = (FrameGraphic*) fcomp->GetFrameGraphic()->Copy();
	FramePicture* fp = new FramePicture;
	fp->Append(_framegr);
	fp->SetFrameGraphic(_framegr);

	g = fp;
        SetGraphic(g);
    }
    return g;
}

void FrameView::Update () {
    FramePicture* fcomp = (FramePicture*) GetFrameComp()->GetGraphic();
    FramePicture* fview = (FramePicture*) GetGraphic();

    int l, b, r, t;
    FrameGraphic* framegr = GetFrameComp()->GetFrameGraphic();
    Viewer* viewer = GetViewer();
    if (viewer != nil && viewer->GetGraphicView() == this) {
        IncurDamage(fview);
        fview->Remove(_framegr);

    } else {
        framegr->GetThickness(l, b, r, t);
        IncurDamage(fview);
        *(Graphic*)fview = *(Graphic*)fcomp;
        *(Graphic*)_framegr = *(Graphic*)framegr;
        UpdateCanvasVar();
        _framegr->SetThickness(l, b, r, t);
        IncurDamage(fview);
    } 
    GraphicViews::Update();
}

/*****************************************************************************/

ClassId FrameCode::GetClassId () { return FRAME_CODE; }

boolean FrameCode::IsA (ClassId id) { 
    return FRAME_CODE ==id || MonoSceneCode::IsA(id);
}

FrameCode::FrameCode (FrameComp* subj) : MonoSceneCode(subj) {}

void FrameCode::Update () {
    MonoSceneCode::Update();
    InteractorComp* subj = GetIntComp();
    Graphic* gr = subj->GetGraphic();
    gr->SetFont(nil);
}

FrameComp* FrameCode::GetFrameComp () { return (FrameComp*) GetSubject(); }

boolean FrameCode::Definition (ostream& out) {
    int l, b, r, t;
    boolean ok = true;
    CodeView* kview = GetKidView();

    if (
        _emitInstanceDecls || _emitForward || 
        _emitProperty || _emitClassHeaders || _emitHeaders
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
            if (_scope && mnamer->GetExport() && !_namelist->Search("frame")) {
                _namelist->Append("frame");
                out << "#include <InterViews/frame.h>\n";
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
            if (!_namelist->Search("frame")) {
                _namelist->Append("frame");
                out << "#include <InterViews/frame.h>\n";
            }
        } else {
            if (kview != nil) {
                ok = ok && kview->Definition(out);
            }
        }
    } else if (_emitInstanceInits) {
        FrameComp* fcomp = GetFrameComp();
        const char* mname = fcomp->GetMemberNameVar()->GetName();

        if (!_instancelist->Find((void*) mname)) {
            _instancelist->Append(new UList((void*)mname));
		
	    BeginInstantiate(out);
            out << "(";
            InstanceName(out);
            out << "nil";
	    const char* classname =fcomp->GetClassNameVar()->GetBaseClass();

	    if ( strcmp(classname, "Frame") == 0) {
                FrameGraphic* fg = fcomp->GetFrameGraphic();
    		fg->GetThickness(l, b, r, t);
        	out << ", " << l << ")";

    	    } else if (strcmp(classname, "ShadowFrame") == 0) {
        	int h, v;
                FrameGraphic* fg = fcomp->GetFrameGraphic();
        	ShadowFrameGraphic* sg = (ShadowFrameGraphic*) fg;

        	sg->GetShadow(h, v);
        	out << ", " << h << ", " << v << ")";

    	    } else if (strcmp(classname, "MarginFrame") == 0) {
		MarginFrameComp* mfc = (MarginFrameComp*) fcomp;
		Shape* s = mfc->GetMarginStateVar()->GetShape();

        	out << ", ";
		out << s->width << ", ";
		out << s->hshrink << ", " << s->hstretch << ", ";
		out << s->height << ", ";
		out << s->vshrink << ", " << s->vstretch << ")";
	
    	    }
	    EndInstantiate(out);
	}

        if (kview != nil) {
            ok = kview->Definition(out) && ok;
        }

	if (AllKidsDefined() && !_lock) {
	    _lock = true;
            InteractorComp* kid = fcomp->GetKid();
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

boolean FrameCode::CoreConstDecls(ostream& out) { 
    const char* classname =GetIntComp()->GetClassNameVar()->GetBaseClass();

    out << "(const char*, Interactor*, ";
    if ( strcmp(classname, "Frame") == 0) {
        out << "int);\n";

    } else if (strcmp(classname, "ShadowFrame") == 0) {
        out << "int, int);\n";

    } else if (strcmp(classname, "MarginFrame") == 0) {
        out << "int, int, int, int, int, int);\n";
    }
    return out.good();
}

boolean FrameCode::CoreConstInits(ostream& out) {
    const char* classname =GetIntComp()->GetClassNameVar()->GetBaseClass();
    const char* subclass = GetIntComp()->GetClassNameVar()->GetName();

    out << "(\n    const char* name, Interactor* i, ";
    if ( strcmp(classname, "Frame") == 0) {
        out << "int w\n) : " << classname << "(name, i, w) {\n";
        out << "    SetClassName(\"" << subclass << "\");\n";
        out << "}\n\n";

    } else if (strcmp(classname, "ShadowFrame") == 0) {
        out << "int h, int v\n) : " << classname << "(name, i, h, v) {\n";
        out << "    SetClassName(\"" << subclass << "\");\n";
        out << "}\n\n";

    } else if (strcmp(classname, "MarginFrame") == 0) {
        out << "int hn, int hshr, int hstr, int vn, int vshr, int vstr\n) : ";
        out << classname << "(name, i, hn, hshr, hstr, vn, vshr, vstr) {\n";
        out << "    SetClassName(\"" << subclass << "\");\n";
        out << "}\n\n";
    }

    return out.good();
}

boolean FrameCode::ConstDecls(ostream& out) {
    const char* classname =GetIntComp()->GetClassNameVar()->GetBaseClass();

    out << "(const char*, Interactor*, ";
    if ( strcmp(classname, "Frame") == 0) {
        out << "int);\n";

    } else if (strcmp(classname, "ShadowFrame") == 0) {
        out << "int, int);\n";

    } else if (strcmp(classname, "MarginFrame") == 0) {
        out << "int, int, int, int, int, int);\n";
    }
    return out.good();
}

boolean FrameCode::ConstInits(ostream& out) {
    const char* classname =GetIntComp()->GetClassNameVar()->GetBaseClass();
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n    const char* name, Interactor* i, ";
    if ( strcmp(classname, "Frame") == 0) {
        out << "int w\n) : " << coreclass << "(name, i, w) {}\n\n";

    } else if (strcmp(classname, "ShadowFrame") == 0) {
        out << "int h, int v\n) : " << coreclass << "(name, i, h, v) {}\n\n";

    } else if (strcmp(classname, "MarginFrame") == 0) {
        out << "int hn, int hshr, int hstr, int vn, int vshr, int vstr\n) : ";
        out << coreclass << "(name, i, hn, hshr, hstr, vn, vshr, vstr) {}\n\n";
    }
    return out.good();
}

boolean FrameCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();

    if (!snamer->IsSubclass() && !_namelist->Search("frame")) {
        _namelist->Append("frame");
        out << "#include <InterViews/frame.h> \n";
    }
    return out.good();
}

/*************************************************************************/

FrameGraphic::FrameGraphic (
    CanvasVar* c, Graphic* g, int width
) : IBGraphic(c, g) {
    SetThickness(width, width, width, width);
}

void FrameGraphic::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    CanvasVar* cvar = GetCanvasVar();
    l = b = cx = cy = 0.0;
    if (cvar != nil) {
        CalcExtent(cvar->Width(), cvar->Height(), l,b,cx,cy,tol,gs);
    }
    tol = 0;
}

Graphic* FrameGraphic::Copy () {
    Iterator i;
    FrameGraphic* copy = new FrameGraphic(nil, this, _l);
    return copy;
}

void FrameGraphic::Read (istream& in) {
    ReadGS(in);
    in >> _l >> _b >> _r >> _t;
}

void FrameGraphic::Write (ostream& out) {
    WriteGS(out);
    out << _l << " " << _b << " " << _r << " " << _t << " ";
}

const char* FrameGraphic::GetClassName () { return "Frame"; }
ClassId FrameGraphic::GetClassId () { return FRAME_GRAPHIC; }
boolean FrameGraphic::IsA (ClassId id) { return FRAME_GRAPHIC == id; }

void FrameGraphic::draw (Canvas* c, Graphic* gs) {
    Coord xmax, ymax;
    CanvasVar* cvar = GetCanvasVar();

    if (cvar != nil) {
	update(gs);
        xmax = cvar->xmax();
        ymax = cvar->ymax();

        register Coord r = xmax - _r;
        register Coord t = ymax - _t;

        _p->ClearRect(c, 0, 0, xmax, ymax);
        _p->FillRect(c, 0, 0, _l-1, t);
        _p->FillRect(c, _l, 0, xmax, _b-1);
        _p->FillRect(c, r+1, _b, xmax, ymax);
        _p->FillRect(c, 0, t+1, r, ymax);
    }
}

void FrameGraphic::drawClipped (
    Canvas* c, Coord left, Coord bottom, Coord right, Coord top, Graphic* gs
) {
    Graphic::drawClipped(c, left, bottom, right, top, gs);
}

void FrameGraphic::GetThickness(int& l , int& b , int& r, int& t) {
    l = _l;
    b = _b;
    r = _r;
    t = _t;
}

void FrameGraphic::SetThickness(int l, int b, int r, int t) {
    _l = l;
    _b = b;
    _r = r;
    _t = t;
}

/*************************************************************************/

ShadowFrameGraphic::ShadowFrameGraphic (
    CanvasVar* c, Graphic* g, int h, int v
) : FrameGraphic(c, g) {
    SetThickness(2, v, h, 2);
}

void ShadowFrameGraphic::GetShadow(int& h, int& v) {
    h = _r;
    v = _b;
}

void ShadowFrameGraphic::SetThickness(int , int b, int r, int ) {
    _l = 2;
    _b = b;
    _r = r;
    _t = 2;
}

Graphic* ShadowFrameGraphic::Copy () {
    Iterator i;
    FrameGraphic* copy = new ShadowFrameGraphic(nil, this, _r, _b);
    return copy;
}

const char* ShadowFrameGraphic::GetClassName () { return "ShadowFrame"; }
ClassId ShadowFrameGraphic::GetClassId () { return SHADOWFRAME_GRAPHIC; }
boolean ShadowFrameGraphic::IsA (ClassId id) {
    return SHADOWFRAME_GRAPHIC == id;
}

void ShadowFrameGraphic::draw (Canvas* c, Graphic* gs) {
    Coord xmax, ymax;
    CanvasVar* cvar = GetCanvasVar();

    if (cvar != nil) {
        update(gs);
        xmax = cvar->xmax();
        ymax = cvar->ymax();

        register Coord r = xmax - _r;
        register Coord t = ymax - _t;
        register Coord v = _b + _t - 2;
        register Coord h = _l + _r - 2;

	/* borders */
        _p->FillRect(c, _l, _b, _l, t);
        _p->FillRect(c, _l+1, _b, r, _b);
        _p->FillRect(c, r, _b+1, r, t+1);
        _p->FillRect(c, _l, t+1, r-1, t+1);

        /* shadows */
        _p->FillRect(c, _r+1, 0, xmax, _b);
        _p->FillRect(c, xmax-_r, 0, xmax, ymax-_b);
    }
}
    
/*************************************************************************/

MarginFrameComp::MarginFrameComp (MarginFrameGraphic* fg) : FrameComp(fg) 
{
    int hm, hshr, hstr;
    int vm, vshr, vstr;

    _margin = nil;
    if (fg != nil) {
        fg->GetShrStr(hm, hshr, hstr, vm, vshr, vstr);
        Init(hm, hshr, hstr, vm, vshr, vstr);    
    }
}

void MarginFrameComp::Init (
    int hm, int hshr, int hstr, int vm, int vshr, int vstr
) {
    if (_margin != nil) {
	delete _margin;
    }
    IBShape* margin = new IBShape;
    _margin = new ShapeVar(margin);
    margin->width = hm;
    margin->hshrink = hshr;
    margin->hstretch = hstr;
    margin->height = vm;
    margin->vshrink = vshr;
    margin->vstretch = vstr;

    margin->hnat = margin->hstr = margin->hshr = true;
    margin->vnat = margin->vstr = margin->vshr = true;
}


ClassId MarginFrameComp::GetClassId () { return MARGINFRAME_COMP; }

boolean MarginFrameComp::IsA (ClassId id) { 
    return MARGINFRAME_COMP==id || FrameComp::IsA(id);
}

void MarginFrameComp::Interpret (Command* cmd) {
    if (cmd->IsA(COLOR_CMD)) {
	ColorCmd* colorCmd = (ColorCmd*) cmd;
        PSColor* fg = colorCmd->GetFgColor();
	if (fg == nil) {
	    InteractorComp::Interpret(cmd);
	}
    } else if (!cmd->IsA(BRUSH_CMD) || cmd->IsA(GLUEVISIBILITY_CMD)) {
	FrameComp::Interpret(cmd);
    }
}

void MarginFrameComp::Uninterpret (Command* cmd) {
    if (cmd->IsA(COLOR_CMD)) {
	ColorCmd* colorCmd = (ColorCmd*) cmd;
        PSColor* fg = colorCmd->GetFgColor();
	if (fg == nil) {
	    InteractorComp::Uninterpret(cmd);
	}
    } else if (!cmd->IsA(BRUSH_CMD) || cmd->IsA(GLUEVISIBILITY_CMD)) {
	FrameComp::Uninterpret(cmd);
    }
}

void MarginFrameComp::Resize () {
    int xmax = _canvasVar->Width(), ymax = _canvasVar->Height();
    Shape* margin = _margin->GetShape();
    float cx, cy;
    Coord x1, y1;

    InteractorComp* kid = GetKid();
    if (kid != nil) {
        Graphic* kidgr = kid->GetGraphic();
        _framegr->SetCanvasVar(_canvasVar);
        
        kidgr->Align(Center, _framegr, Center);
        
        _framegr->GetCenter(cx, cy);
        
        x1 = round(cx) - xmax/2;
        y1 = round(cy) - ymax/2;
        
        Shape* shape = GetShapeVar()->GetShape();
        Coord hextra = (xmax+1) - shape->width;
        Coord h = margin->width*2;
        
        if (hextra > 0 && shape->hstretch != 0) {
            h += int(float(margin->hstretch*2) / 
            float(shape->hstretch) * float(hextra));    
        } else if (hextra < 0 && shape->hshrink != 0) {
            h += int(float(margin->hshrink*2) / 
            float(shape->hshrink) * float(hextra));
        }
        
        Coord vextra = (ymax+1) - shape->height;
        Coord v = margin->height*2;
        if (vextra > 0 && shape->vstretch != 0) {
            v += int(float(margin->vstretch*2) / 
            float(shape->vstretch) * float(vextra));    
        } else if (vextra < 0 && shape->vshrink != 0) {
            v += int(float(margin->vshrink*2) / 
            float(shape->vshrink) * float(vextra));
        }
        
        Place(kid, x1+h/2, y1+v/2, x1+xmax-h/2-1, y1+ymax-v/2-1);
    }
}

void MarginFrameComp::Reconfig () {
    MonoSceneComp::Reconfig();

    Shape* margin = _margin->GetShape();
    Shape* shape = GetShapeVar()->GetShape();
    shape->width += margin->width*2;
    shape->height += margin->height*2;
    shape->hshrink += margin->hshrink*2;
    shape->hstretch += margin->hstretch*2;
    shape->vshrink += margin->vshrink*2;
    shape->vstretch += margin->vstretch*2;
    
}

StateVar* MarginFrameComp::GetState (const char* name) {
    StateVar* stateVar = nil;

    if (strcmp(name, "MarginStateVar") == 0) {
        stateVar = _margin;
    } else {
	stateVar = FrameComp::GetState(name);
    }

    return stateVar;
}

void MarginFrameComp::Read(istream& in) {
    FrameComp::Read(in);
    MarginFrameGraphic* mg = (MarginFrameGraphic*) _framegr;
    _margin = (ShapeVar*) unidraw->GetCatalog()->ReadStateVar(in);
}

void MarginFrameComp::Write(ostream& out) {
    FrameComp::Write(out);
    unidraw->GetCatalog()->WriteStateVar(_margin, out);
}

/*****************************************************************************/

MarginFrameView::MarginFrameView (MarginFrameComp* subj) : FrameView(subj) {
    _rigid = false;
}

MarginFrameComp* MarginFrameView::GetMarginFrameComp () { 
    return (MarginFrameComp*) GetSubject();
}
ClassId MarginFrameView::GetClassId () { return MARGINFRAME_VIEW; }

boolean MarginFrameView::IsA (ClassId id) {
    return MARGINFRAME_VIEW == id || FrameView::IsA(id);
}

InfoDialog* MarginFrameView::GetInfoDialog () {
    InfoDialog* info = FrameView::GetInfoDialog();
    ButtonState* state = info->GetState();
    ShapeVar* margin = GetMarginFrameComp()->GetMarginStateVar();
    info->Include(new MarginVarView(margin, state));
    return info;
}
 
Manipulator* MarginFrameView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Manipulator* m = nil;
    Rubberband* rub = nil;

    if (tool->IsA(IBGRAPHIC_COMP_TOOL)) {
	m = InteractorView::CreateManipulator(v, e, rel, tool);

    } else if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        _rigid = e.shift;
	GraphicView* views = v->GetGraphicView();
	Selection* newSel = views->ViewIntersecting(
	    e.x-SLOP, e.y-SLOP, e.x+SLOP, e.y+SLOP
	);
	if (!newSel->IsEmpty()) {
            Coord l, b, r, t;
	    Iterator i;
	    newSel->First(i);
	    GraphicView* gv = newSel->GetView(i);
            gv->GetGraphic()->GetBox(l, b, r, t);
            rub = new ConstrainScaleRect(
		nil, nil, l, b, r, t, (l+r)/2, (b+t)/2
	    );
	    v->GetSelection()->Append(gv);
            m = new DragManip(v, rub, rel, tool, Gravity);
	}

    } else {
        m = FrameView::CreateManipulator(v, e, rel, tool);
    }
    return m;
}

Command* MarginFrameView::InterpretManipulator (Manipulator* m) {
    Command* cmd = nil;
    Tool* tool = m->GetTool();

    if (tool->IsA(IBGRAPHIC_COMP_TOOL)) {
	cmd = InteractorView::InterpretManipulator(m);

    } else if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        Coord l, b, r, t;
        Coord ll, bb, rr, tt;

        DragManip* dm = (DragManip*) m;
	Selection* s = dm->GetViewer()->GetSelection();
	Iterator i;
	s->First(i);
	GraphicView* gv = s->GetView(i);
        gv->GetGraphic()->GetBox(ll, bb, rr, tt);

        ConstrainScaleRect* sr = (ConstrainScaleRect*) dm->GetRubberband();
        sr->GetCurrent(l, b, r, t);

        Transformer* rel = dm->GetTransformer();
        Editor* ed = dm->GetViewer()->GetEditor();
        ColorVar* colVar = (ColorVar*) ed->GetState("ColorVar");


        if (rel != nil) {
            rel->InvTransformRect(l, b, r, t);
            rel->InvTransformRect(ll, bb, rr, tt);
        }
	_rigid = _rigid || dm->GraspEvent().shift;
        MarginFrameGraphic* mg;
        if (_rigid) {
            mg = new MarginFrameGraphic(
                nil, stdgraphic, r-rr+1, 0, 0, t-tt+1, 0, 0
            );
        } else {
            mg = new MarginFrameGraphic(
                nil, stdgraphic, r-rr+1, 1000000, 1000000, 
                t-tt+1, 1000000, 1000000
            );
        }
        mg->SetColors(
            stdgraphic->GetFgColor(), colVar->GetBgColor()
        );
        cmd = new MonoSceneCmd(ed, new MarginFrameComp(mg));

    } else {
	cmd = FrameView::InterpretManipulator(m);
    }
    return cmd;
}

void MarginFrameView::Update () {
    MarginFrameComp* mcomp = GetMarginFrameComp();
    IBGraphic* fcomp = mcomp->GetIBGraphic();
    IBGraphic* fview = (IBGraphic*) GetGraphic();

    int l, b, r, t;
    int hm, hshr, hstr, vm, vshr, vstr;

    MarginFrameGraphic* framegr = mcomp->GetMarginFrameGraphic();
    Viewer* viewer = GetViewer();
    if (viewer != nil && viewer->GetGraphicView() == this) {
        IncurDamage(fview);
        fview->Remove(_framegr);
        
    } else {
        framegr->GetThickness(l, b, r, t);
        framegr->GetShrStr(hm, hshr, hstr, vm, vshr, vstr);
        
        IncurDamage(fview);
        *(Graphic*)_framegr = *(Graphic*)framegr;
        *(Graphic*)fview = *(Graphic*)fcomp;
        UpdateCanvasVar();
        
        MarginFrameGraphic* mg= (MarginFrameGraphic*) _framegr;
        mg->SetThickness(l, b, r, t);
        mg->SetShrStr(hm, hshr, hstr, vm, vshr, vstr);
        IncurDamage(fview);
    }
    GraphicViews::Update();
}

/*************************************************************************/

MarginFrameGraphic::MarginFrameGraphic (
    CanvasVar* c, Graphic* g, int hm, int hshr, int hstr, 
    int vm, int vshr, int vstr
) : FrameGraphic(c, g) {
    _hmargin = hm;
    _hshrink = hshr;
    _hstretch = hstr;
    _vmargin = vm;
    _vshrink = vshr;
    _vstretch = vstr;
}

Graphic* MarginFrameGraphic::Copy () {
    Iterator i;
    Graphic* copy = new MarginFrameGraphic(
    	nil, this, _hmargin, _hshrink, _hstretch, 
	_vmargin, _vshrink, _vstretch
    );
    return copy;
}

void MarginFrameGraphic::GetShrStr(
    int& hm, int& hshr, int& hstr, int& vm, int& vshr, int& vstr
) {
    hm = _hmargin;
    hshr = _hshrink;
    hstr = _hstretch;
    vm = _vmargin;
    vshr = _vshrink;
    vstr = _vstretch;
}

void MarginFrameGraphic::SetShrStr(
    int hm, int hshr, int hstr, int vm, int vshr, int vstr
) {
    _hmargin = hm;
    _hstretch = hshr;
    _hstretch = hstr;
    _vmargin = vm;
    _vshrink = vshr;
    _vstretch = vstr;
}

void MarginFrameGraphic::Read (istream& in) {
    FrameGraphic::Read(in);
    in >> _hmargin >> _hshrink >> _hstretch;
    in >> _vmargin >> _vshrink >> _vstretch;
}

void MarginFrameGraphic::Write (ostream& out) {
    FrameGraphic::Write(out);
    out << _hmargin << " " << _hshrink << " " << _hstretch << '\n';
    out << _vmargin << " " << _vshrink << " " << _vstretch << '\n';
}

const char* MarginFrameGraphic::GetClassName () { return "MarginFrame"; }
ClassId MarginFrameGraphic::GetClassId () { return MARGINFRAME_GRAPHIC; }
boolean MarginFrameGraphic::IsA (ClassId id) {
    return (MARGINFRAME_GRAPHIC == id || FrameGraphic::IsA(id));
}

void MarginFrameGraphic::draw (Canvas* c, Graphic* gs) {
    CanvasVar* cvar = GetCanvasVar();

    if (cvar != nil) {
	update(gs);
        _p->SetColors(gs->GetFgColor(), gs->GetBgColor());
	_p->ClearRect(c, 0, 0, cvar->xmax(), cvar->ymax()); 
    }
}

