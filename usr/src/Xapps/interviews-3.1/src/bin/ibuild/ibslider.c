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
 * Slider component definitions.
 */

#include "ibclasses.h"
#include "ibcmds.h"
#include "ibcreator.h"
#include "ibdialogs.h"
#include "ibed.h"
#include "ibgraphic.h"
#include "ibrubrect.h"
#include "ibslider.h"
#include "ibvars.h"
#include "ibvarviews.h"

#include <Unidraw/catalog.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/manips.h>
#include <Unidraw/ulist.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/edit.h>
#include <Unidraw/Commands/macro.h>
#include <Unidraw/Graphic/pspaint.h>
#include <Unidraw/Tools/tool.h>

#include <InterViews/brush.h>
#include <InterViews/event.h>
#include <InterViews/paint.h>
#include <InterViews/painter.h>
#include <InterViews/rubrect.h>
#include <InterViews/shape.h>
#include <InterViews/transformer.h>

#include <math.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/
SliderGraphic* SliderComp::GetSliderGraphic () {
    return (SliderGraphic*) GetGraphic();
}

ClassId SliderComp::GetClassId () { return SLIDER_COMP; }
boolean SliderComp::IsA (ClassId id) {
    return SLIDER_COMP == id || InteractorComp::IsA(id);
}

SliderComp::SliderComp (SliderGraphic* g) : InteractorComp(g) {
    GetClassNameVar()->SetName("Slider");
    GetClassNameVar()->SetBaseClass("Slider");
    _adjusteeVar = nil;
}

SliderComp::~SliderComp () {
    delete _adjusteeVar;
}

void SliderComp::Instantiate () {
    InteractorComp::Instantiate();
    if (_adjusteeVar == nil) {
        _adjusteeVar = new MemberNameVar("", false, false);
    }
}

void SliderComp::Reconfig () {
    Shape* shape = GetShapeVar()->GetShape();
    int w, h;
    GetSliderGraphic()->GetSize(w, h);
    shape->Rect(w, h);
    shape->Rigid(hfil, hfil, vfil, vfil);
    GetShapeVar()->Notify();
}

StateVar* SliderComp::GetState (const char* name) {
    StateVar* stateVar = nil;

    if (strcmp(name, "AdjusteeVar") == 0) {
        stateVar = _adjusteeVar;

    } else {
        stateVar = InteractorComp::GetState(name);
    }

    return stateVar;
}

void SliderComp::SetState(const char* adjustee, StateVar* stateVar) {
    if (
        strcmp(adjustee, "RelatedVar") == 0 ||
        strcmp(adjustee, "AdjusteeVar") == 0
    ) {
        MemberNameVar* memberVar = (MemberNameVar*) stateVar;
        *_adjusteeVar = *memberVar;

    } else {
	InteractorComp::SetState(adjustee, stateVar);
    }
}

InteractorComp& SliderComp::operator = (InteractorComp& comp) {
    StateVar* adjusteevar = comp.GetState("AdjusteeVar");

    if (adjusteevar != nil) {
        SetState("AdjusteeVar", adjusteevar);

    } else {
        MemberNameVar* member = comp.GetMemberNameVar();
        SetState("AdjusteeVar", member);
    }
    return *this;
}

boolean SliderComp::IsRelatableTo (InteractorComp* comp) {
    boolean ok = false;
    if (
        comp->GetClassNameVar()->IsSubclass() ||
        comp->IsA(ADJUSTER_COMP) || comp->IsA(DECK_COMP) || 
        comp->IsA(GRBLOCK_COMP) || comp->IsA(PANNER_COMP) || 
        comp->IsA(SCROLLER_COMP) || comp->IsA(SLIDER_COMP) ||
        comp->IsA(STRBROWSER_COMP) || comp->IsA(STREDIT_COMP) ||
        comp->IsA(TEXTEDIT_COMP) || comp->IsA(IBVIEWER_COMP) || 
        comp->IsA(VIEWPORT_COMP)
    ) {
        ok = true;
    }
    return ok;
}

void SliderComp::Interpret (Command* cmd) {
    if (!cmd->IsA(FONT_CMD)) {
        InteractorComp::Interpret(cmd);
    }
}

void SliderComp::Uninterpret (Command* cmd) {
    if (!cmd->IsA(FONT_CMD)) {
        InteractorComp::Uninterpret(cmd);
    }
}

void SliderComp::Read (istream& in) {
    InteractorComp::Read(in);
    Catalog* catalog = unidraw->GetCatalog();

    ClassId id;
    in >> id;
    SliderGraphic* g = (SliderGraphic*) catalog->GetCreator()->Create(id);
    g->Read(in);
    g->SetCanvasVar(GetCanvasVar());
    SetGraphic(g);
    
    delete _adjusteeVar;
    _adjusteeVar = (MemberNameVar*) unidraw->GetCatalog()->ReadStateVar(in);
}

void SliderComp::Write (ostream& out) {
    ClassId id;
    InteractorComp::Write(out);
    Catalog* catalog = unidraw->GetCatalog();
    SliderGraphic* g = GetSliderGraphic();
    id = g->GetClassId();
    out << " " << id << " ";
    g->Write(out);
    unidraw->GetCatalog()->WriteStateVar(_adjusteeVar, out);
}

/*****************************************************************************/

SliderView::SliderView (SliderComp* subj) : InteractorView(subj) { }
ClassId SliderView::GetClassId () { return SLIDER_VIEW; }

boolean SliderView::IsA (ClassId id) {
    return SLIDER_VIEW == id || InteractorView::IsA(id);
}

SliderComp* SliderView::GetSliderComp () {
    return (SliderComp*) GetSubject();
}

void SliderView::Update () {
    SliderGraphic* scomp = GetSliderComp()->GetSliderGraphic();
    SliderGraphic* sview = (SliderGraphic*) GetGraphic();

    IncurDamage(sview);
    *(Graphic*)sview = *(Graphic*)scomp;
    UpdateCanvasVar();
    IncurDamage(sview);
    EraseHandles();
}

InfoDialog* SliderView::GetInfoDialog () {
    InfoDialog* info = InteractorView::GetInfoDialog();
    ButtonState* state = info->GetState();
    SliderComp* scomp = GetSliderComp();
    MemberNameVar* adjusteeVar = scomp->GetAdjusteeVar();
    info->Include(new RelatedVarView(adjusteeVar, state, scomp));
    return info;
}

Manipulator* SliderView::CreateManipulator (
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
    } else {
        m = InteractorView::CreateManipulator(v, e, rel, tool);
    }
    return m;
}

Command* SliderView::InterpretManipulator (Manipulator* m) {
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

	ColorVar* colVar = (ColorVar*) ed->GetState("ColorVar");
        SliderGraphic* g = new SliderGraphic(x1-x0, y1-y0, nil,stdgraphic);

	if (colVar != nil) {
            g->SetColors(colVar->GetFgColor(), colVar->GetBgColor());
        }
        SliderComp* comp = new SliderComp(g);
	cmd = new MacroCmd(
            ed, new PasteCmd(ed, new Clipboard(comp)),
            new PlaceCmd(ed, x0, y0, x1-1, y1-1, new Clipboard(comp))
        );

    } else {
        cmd = InteractorView::InterpretManipulator(m);
    }
    return cmd;
}

InteractorComp* SliderView::InitComp (Coord l, Coord b, Coord r, Coord t) {
    IBGraphic* gs = GetInteractorComp()->GetIBGraphic();
    int w, h;
    w = r - l;
    h = t - b;
    SliderGraphic* g = new SliderGraphic(w, h, nil, gs);
    return new SliderComp(g);
}

/*****************************************************************************/

ClassId SliderCode::GetClassId () { return SLIDER_CODE; }

boolean SliderCode::IsA(ClassId id) {
    return SLIDER_CODE==id || CodeView::IsA(id);
}

SliderCode::SliderCode (SliderComp* subj) : CodeView(subj) {}

void SliderCode::Update () {
    CodeView::Update();
    InteractorComp* subj = GetIntComp();
    Graphic* gr = subj->GetGraphic();
    gr->SetFont(nil);
}

SliderComp* SliderCode::GetSliderComp () { 
    return (SliderComp*) GetSubject(); 
}

boolean SliderCode::Definition (ostream& out) {
    boolean ok = true;
    if (
	_emitProperty || _emitInstanceDecls || 
        _emitForward || _emitClassHeaders || _emitHeaders
    ) {
        return CodeView::Definition(out);

    } else if (_emitExpHeader) {
	InteractorComp* icomp = GetIntComp();
	MemberNameVar* mnamer = icomp->GetMemberNameVar();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        if (!snamer->IsSubclass()) {
            if (
                _scope && mnamer->GetExport() && 
                !_namelist->Search("panner")
            ) {
                _namelist->Append("panner");
                out << "#include <InterViews/panner.h>\n";
            }
        } else {
            ok = ok && CodeView::Definition(out);
        }

    } else if (_emitCorehHeader) {
	InteractorComp* icomp = GetIntComp();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        const char* subclass = snamer->GetName();
        if (snamer->IsSubclass() && strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("panner")) {
                _namelist->Append("panner");
                out << "#include <InterViews/panner.h>\n";
            }
        }
    } else if (_emitInstanceInits) {
        InteractorComp* icomp = GetIntComp();
        InteractorComp* ctarget = nil;
        const char* mname = icomp->GetMemberNameVar()->GetName();
        MemberNameVar* mnamer = (MemberNameVar*) icomp->GetState(
	    "AdjusteeVar"
	);
        const char* slidee = mnamer->GetName();

	if (*slidee == '\0') {
            if (_err_count < 10) {
                strcat(_errbuf, mname);
                strcat(_errbuf, " has undefined sliding target.\n");
                _err_count++;
            }
	    return false;

        } else if (!Search(mnamer, ctarget)) {
            if (_err_count < 10) {
                strcat(_errbuf, mname);
                strcat(
                    _errbuf, 
                    "'s sliding target is not in the same hierarchy.\n"
                );
                _err_count++;
            }
	    return false;

        } else if (ctarget != nil && !icomp->IsRelatableTo(ctarget)) {
            if (_err_count < 10) {
                strcat(_errbuf, mname);
                strcat(
                    _errbuf, 
                    "'s adjusting target is not subclassed nor adjustable.\n"
                );
                _err_count++;
            }
	    return false;
        }
        if (_instancelist->Find((void*) slidee)) {
            if (!_instancelist->Find((void*) mname)) {
                _instancelist->Append(new UList((void*) mname));
                
                char coreclass[CHARBUFSIZE];
                char SliderClass[CHARBUFSIZE];
                
                GetCoreClassName(coreclass);
                strcpy(SliderClass, coreclass);
                strcat(SliderClass, "_Slider");
                
                boolean export = icomp->GetMemberNameVar()->GetExport();
                const char* classname = icomp->GetClassNameVar()->GetName();

		int w = icomp->GetCanvasVar()->Width();
                slidee = (*slidee == '\0') ? "nil" : slidee;

                if (icomp->GetClassNameVar()->IsSubclass()) {
                    BeginInstantiate(out);
                    out << "(";
                } else {
                    if (export && !_emitMain) {
                        out << "    " << mname << " = new " << SliderClass;
                        out << "(";
                    } else {
                        out << "    " << classname << "* ";
                        out << mname << " = new " << SliderClass << "(";
                    }
                }
                InstanceName(out);
                out << slidee << ", " << w << ")";
                EndInstantiate(out);
                _icomplete = true;
            }
        } else {
            _icomplete = false;
        }

    } else if (
	_emitBSDecls || _emitBSInits || 
        _emitFunctionDecls || _emitFunctionInits
    ) {
        return true;

    } else if (
        _emitCoreDecls || _emitCoreInits || _emitClassDecls || _emitClassInits
    ) {
	ok = ok && CodeView::Definition(out);
        
    } else if (_emitMain) {
	ok = ok && CodeView::Definition(out);
        
    }
    return out.good() && ok;
}

boolean SliderCode::CoreConstDecls(ostream& out) { 
    out << "(const char*, Interactor*, int width);\n";
    return out.good();
}

boolean SliderCode::CoreConstInits(ostream& out) {
    InteractorComp* icomp = GetIntComp();
    SubclassNameVar* snamer = icomp->GetClassNameVar();
    const char* baseclass = snamer->GetBaseClass();
    const char* subclass = snamer->GetName();

    out << "(\n    const char* name, Interactor* i, int width\n) : ";
    out << baseclass << "(name, i) {\n";
    out << "    SetClassName(\"" << subclass << "\");\n";
    out << "    Perspective* target = i->GetPerspective();\n";
    out << "    shape->width = width;\n";
    out << "    float aspect = float(target->height)";
    out << " / float(target->width);\n";
    out << "    shape->height = (int) (aspect * width);\n";
    out << "    shape->Rigid();\n";
    out << "}\n\n";
    return out.good();
}

boolean SliderCode::ConstDecls(ostream& out) {
    out << "(const char*, Interactor*, int width);\n";
    return out.good();
}

boolean SliderCode::ConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n    const char* name, Interactor* i, int width\n) : ";
    out << coreclass << "(name, i, width) {}\n\n";

    return out.good();
}

boolean SliderCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();

    if (!snamer->IsSubclass() && !_namelist->Search("panner")) {
        _namelist->Append("panner");
        out << "#include <InterViews/panner.h> \n";
    }
    if (!_namelist->Search("shape")) {
        _namelist->Append("shape");
        out << "#include <InterViews/shape.h> \n";
    }
    if (!_namelist->Search("perspective")) {
        _namelist->Append("perspective");
        out << "#include <InterViews/perspective.h> \n";
    }
    return out.good();
}

/*****************************************************************************/

const char* SliderGraphic::GetClassName () { return "slider"; }
ClassId SliderGraphic::GetClassId () { return SLIDER_GRAPHIC; }

SliderGraphic::SliderGraphic (
    int w, int h, CanvasVar* c, Graphic* g
) : IBGraphic(c, g) {
    Init(w, h);
}

void SliderGraphic::Init(int w, int h) {
    _width = w;
    _height = h;
}

Graphic* SliderGraphic::Copy () {
    return new SliderGraphic(_width, _height, nil, this);
}

void SliderGraphic::Read (istream& in) {
    ReadGS(in);
    in >> _width >> _height;
}

void SliderGraphic::Write (ostream& out) {
    WriteGS(out);
    out << _width << " " << _height << " ";
}

void SliderGraphic::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    CanvasVar* cvar = GetCanvasVar();

    if (cvar == nil) {
        int w, h;
        w = _width;
        h = _height;
        CalcExtent(w, h, l, b, cx, cy, tol, gs);

    } else {
        CalcExtent(cvar->Width(), cvar->Height(), l,b,cx,cy,tol,gs);
    }
    tol = 0;
}

void SliderGraphic::drawClipped (
    Canvas* c, Coord l, Coord b, Coord r, Coord t, Graphic* gs
) {
    Graphic::drawClipped(c, l, b, r, t, gs);
}

void SliderGraphic::draw (Canvas* c, Graphic* gs) {
    update(gs);
    Coord xmax, ymax;
    CanvasVar* cvar = GetCanvasVar();

    if (cvar == nil) {
	xmax = _width;
	ymax = _height;
    } else {
        xmax = cvar->xmax();
        ymax = cvar->ymax();
    }

    static Pattern* ltgray;

    if (ltgray == nil) {
        ltgray = new Pattern(Pattern::lightgray);
        ltgray->Reference();
    }

    _p->SetPattern(ltgray);
    _p->FillRect(c, 0, 0, xmax, ymax);

    Coord l, b, r, t;
    l = xmax/3;
    b = ymax/3;
    r = xmax - xmax/3;
    t = ymax - ymax/3;
    _p->ClearRect(c, l, b, r, t);
    _p->Rect(c, l, b, r, t);
    _p->Line(c, l+1, b-1, r+1, b-1);
    _p->Line(c, r+1, b-1, r+1, t-1);
}
