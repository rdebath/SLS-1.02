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
 * Panner component definitions.
 */

#include "ibadjuster.h"
#include "ibborder.h"
#include "ibclasses.h"
#include "ibcmds.h"
#include "ibdeck.h"
#include "ibdialogs.h"
#include "ibed.h"
#include "ibglue.h"
#include "ibgraphic.h"
#include "ibpanner.h"
#include "ibrubrect.h"
#include "ibslider.h"
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

#include <Unidraw/Commands/edit.h>
#include <Unidraw/Commands/macro.h>
#include <Unidraw/Tools/tool.h>

#include <InterViews/bitmap.h>
#include <InterViews/canvas.h>
#include <InterViews/event.h>
#include <InterViews/paint.h>
#include <InterViews/painter.h>
#include <InterViews/rubrect.h>
#include <InterViews/shape.h>
#include <InterViews/transformer.h>

#include <stream.h>
#include <string.h>

/*****************************************************************************/

ClassId PannerComp::GetClassId () { return PANNER_COMP; }

boolean PannerComp::IsA (ClassId id) {
    return PANNER_COMP == id || InteractorComp::IsA(id);
}

PannerComp::PannerComp (int w, int h) { 
    Graphic* graphic = GetGraphic();
    delete graphic;
    SetGraphic(new IBGraphic(nil, stdgraphic));
    GetClassNameVar()->SetName("Panner");
    GetClassNameVar()->SetBaseClass("Panner");

    _adjusteeVar = nil;
    if (w > 0 && h > 0) {
        Interior(w, h);
    }
}

PannerComp::~PannerComp () {
    delete _adjusteeVar;
}

void PannerComp::Instantiate () {
    InteractorComp::Instantiate();
    if (_adjusteeVar == nil) {
        _adjusteeVar = new MemberNameVar("", false, false);
    }
}

void PannerComp::Reconfig () {
    VBoxComp::Reconfig();
    Shape* shape = GetShapeVar()->GetShape();
    shape->Rigid();
}


void PannerComp::SetState(const char* adjustee, StateVar* stateVar) {
    if (
        strcmp(adjustee, "RelatedVar") == 0 ||
        strcmp(adjustee, "AdjusteeVar") == 0
    ) {
        MemberNameVar* memberVar = (MemberNameVar*) stateVar;
        *_adjusteeVar = *memberVar;

    } else {
        VBoxComp::SetState(adjustee, stateVar);
    }
}

InteractorComp& PannerComp::operator = (InteractorComp& comp) {
    StateVar* adjusteevar = comp.GetState("AdjusteeVar");

    if (adjusteevar != nil) {
        SetState("AdjusteeVar", adjusteevar);

    } else {
        MemberNameVar* member = comp.GetMemberNameVar();
        SetState("AdjusteeVar", member);
    }
    return *this;
}

boolean PannerComp::IsRelatableTo (InteractorComp* comp) {
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

void PannerComp::Interior (int w, int h) {
    GlueGraphic* hglue, *vglue, *backgr;
    HBoxComp* hbox1 = new HBoxComp();

    hglue = new GlueGraphic(0, Horizontal, nil, nil);
    hglue->SetBrush(psnonebr);

    hbox1->Append(new GlueComp(hglue));
    LMoverGraphic* lgr = new LMoverGraphic(nil, nil);
    hbox1->Append(new AdjusterComp(lgr));
    hglue = new GlueGraphic(0, Horizontal, nil, nil);
    hglue->SetBrush(psnonebr);

    hbox1->Append(new GlueComp(hglue));
    RMoverGraphic* rgr = new RMoverGraphic(nil, nil);
    hbox1->Append(new AdjusterComp(rgr));
    hglue = new GlueGraphic(0, Horizontal, nil, nil);
    hglue->SetBrush(psnonebr);

    hbox1->Append(new GlueComp(hglue));
    VBoxComp* vbox1 = new VBoxComp();
    vglue = new GlueGraphic(0, Vertical, nil, nil);
    vglue->SetBrush(psnonebr);

    vbox1->Append(new GlueComp(vglue));
    UMoverGraphic* ugr = new UMoverGraphic(nil, nil);
    vbox1->Append(new AdjusterComp(ugr));
    vbox1->Append(hbox1);
    DMoverGraphic* dgr = new DMoverGraphic(nil, nil);
    vbox1->Append(new AdjusterComp(dgr));
    vglue = new GlueGraphic(0, Vertical, nil, nil);
    vglue->SetBrush(psnonebr);

    vbox1->Append(new GlueComp(vglue));
    VBoxComp* vbox2 = new VBoxComp();
    vglue = new GlueGraphic(2, Vertical, nil, nil);
    vglue->SetBrush(psnonebr);

    vbox2->Append(new GlueComp(vglue));
    EnlargerGraphic* egr = new EnlargerGraphic(nil, nil);
    vbox2->Append(new AdjusterComp(egr));
    vglue = new GlueGraphic(4, Vertical, nil, nil);
    vglue->SetBrush(psnonebr);

    vbox2->Append(new GlueComp(vglue));
    ReducerGraphic* rdgr = new ReducerGraphic(nil, nil);
    vbox2->Append(new AdjusterComp(rdgr));
    vglue = new GlueGraphic(2, Vertical, nil, nil);
    vglue->SetBrush(psnonebr);

    vbox2->Append(new GlueComp(vglue));
    HBoxComp* hbox2 = new HBoxComp();
    hglue = new GlueGraphic(0, Horizontal, nil, nil);
    hglue->SetBrush(psnonebr);

    hbox2->Append(new GlueComp(hglue));
    hbox2->Append(vbox1);
    hglue = new GlueGraphic(0, Horizontal, nil, nil);
    hglue->SetBrush(psnonebr);

    hbox2->Append(new GlueComp(hglue));
    hbox2->Append(vbox2);
    hglue = new GlueGraphic(0, Horizontal, nil, nil);
    hglue->SetBrush(psnonebr);

    hbox2->Append(new GlueComp(hglue));
    SliderGraphic* slgr = new SliderGraphic(w, h/2, nil, nil);
    SliderComp* slider = new SliderComp(slgr);

    BorderGraphic* bogr = new BorderGraphic(Horizontal, nil, nil);
    BorderComp* border = new BorderComp(bogr);

    backgr = new GlueGraphic(0, Horizontal, nil, nil);
    backgr->SetBrush(psnonebr);

    DeckComp* deck = new DeckComp;
    deck->Append(new GlueComp(backgr));
    deck->Append(hbox2);

    Append(deck);
    Append(border);
    Append(slider);
}

void PannerComp::Interpret (Command* cmd) {
    if (!cmd->IsA(FONT_CMD)) {
        InteractorComp::Interpret(cmd);
    }
}

void PannerComp::Uninterpret (Command* cmd) {
    if (!cmd->IsA(FONT_CMD)) {
        InteractorComp::Uninterpret(cmd);
    }
}

StateVar* PannerComp::GetState (const char* name) {
    StateVar* stateVar = nil;

    if (strcmp(name, "AdjusteeVar") == 0) {
        stateVar = _adjusteeVar;
    } else {
        stateVar = VBoxComp::GetState(name);
    }

    return stateVar;
}

void PannerComp::Read (istream& in) {
    VBoxComp::Read(in);
    delete _adjusteeVar;

    _adjusteeVar = (MemberNameVar*) unidraw->GetCatalog()->ReadStateVar(in);
}

void PannerComp::Write (ostream& out) {
    VBoxComp::Write(out);
    unidraw->GetCatalog()->WriteStateVar(_adjusteeVar, out);
}

/*****************************************************************************/

PannerView::PannerView (PannerComp* subj) : SceneView(subj) { }
PannerComp* PannerView::GetPannerComp () { return (PannerComp*) GetSubject(); }
ClassId PannerView::GetClassId () { return PANNER_VIEW; }

boolean PannerView::IsA (ClassId id) {
    return PANNER_VIEW == id || SceneView::IsA(id);
}

InfoDialog* PannerView::GetInfoDialog () {
    InfoDialog* info = InteractorView::GetInfoDialog();
    ButtonState* state = info->GetState();
    PannerComp* pcomp = GetPannerComp();
    MemberNameVar* adjusteeVar = pcomp->GetAdjusteeVar();
    info->Include(new RelatedVarView(adjusteeVar, state, pcomp));
    return info;
}

Selection* PannerView::SelectAll() { return new Selection; }
Selection* PannerView::ViewContaining(Coord, Coord) { return new Selection; }
Selection* PannerView::ViewsContaining(Coord, Coord) { return new Selection; }

Selection* PannerView::ViewIntersecting(Coord, Coord, Coord, Coord) {
    return new Selection; 
}
Selection* PannerView::ViewsIntersecting(Coord, Coord, Coord, Coord) {
    return new Selection; 
}
Selection* PannerView::ViewsWithin(Coord, Coord, Coord, Coord) {
    return new Selection; 
}

Manipulator* PannerView::CreateManipulator (
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

Command* PannerView::InterpretManipulator (Manipulator* m) {
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

        PannerComp* comp = new PannerComp(x1-x0, y1-y0);
	IBGraphic* g = comp->GetIBGraphic();
	ColorVar* colVar = (ColorVar*) ed->GetState("ColorVar");

	if (colVar != nil) {
            g->SetColors(colVar->GetFgColor(), colVar->GetBgColor());
        }
	cmd = new MacroCmd(
            ed, new PasteCmd(ed, new Clipboard(comp)),
            new PlaceCmd(ed, x0, y0, x1-1, y1-1, new Clipboard(comp))
        );

    } else {
	cmd = InteractorView::InterpretManipulator(m);
    }
    return cmd;
}

/*****************************************************************************/

ClassId PannerCode::GetClassId () { return PANNER_CODE; }

boolean PannerCode::IsA(ClassId id) {
    return PANNER_CODE==id || CodeView::IsA(id);
}

PannerCode::PannerCode (PannerComp* subj) : CodeView(subj) {}

void PannerCode::Update () {
    CodeView::Update();
    InteractorComp* subj = GetIntComp();
    Graphic* gr = subj->GetGraphic();
    gr->SetFont(nil);
}

PannerComp* PannerCode::GetPannerComp () {
    return (PannerComp*) GetSubject(); 
}

boolean PannerCode::Definition (ostream& out) {
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
            if (_scope && mnamer->GetExport()&&!_namelist->Search("panner")) {
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
        const char* pannee = mnamer->GetName();

	if (*pannee == '\0') {
            if (_err_count < 10) {
                strcat(_errbuf, mname);
                strcat(_errbuf, " has undefined panning target.\n");
                _err_count++;
            } 
	    return false;

        } else if (!Search(mnamer, ctarget)) {
            if (_err_count < 10) {
                strcat(_errbuf, mname);
                strcat(
                    _errbuf, 
                    "'s panning target is not in the same hierarchy.\n"
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
        if (_instancelist->Find((void*) pannee)) {
            if (!_instancelist->Find((void*) mname)) {
                _instancelist->Append(new UList((void*) mname));

                BeginInstantiate(out);
                out << "(";
                InstanceName(out);
                int width = icomp->GetShapeVar()->GetShape()->width;
                out << pannee << ", " << width << ")";
                EndInstantiate(out);

                _icomplete = true;
            }
        } else {
            _icomplete = false;
        }

    } else if (
	_emitBSDecls ||  _emitBSInits || 
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

boolean PannerCode::CoreConstDecls(ostream& out) { 
    out << "(const char*, Interactor*, int);\n";
    return out.good();
}

boolean PannerCode::CoreConstInits(ostream& out) {
    InteractorComp* icomp = GetIntComp();
    SubclassNameVar* snamer = icomp->GetClassNameVar();
    const char* baseclass = snamer->GetBaseClass();
    const char* subclass = snamer->GetName();

    out << "(\n    const char* name, Interactor* i, int w\n) : " << baseclass;
    out << "(name, i, w) {\n";
    out << "    SetClassName(\"" << subclass << "\");\n";
    out << "}\n\n";
    return out.good();
}

boolean PannerCode::ConstDecls(ostream& out) {
    out << "(const char*, Interactor*, int);\n";
    return out.good();
}

boolean PannerCode::ConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n    const char* name, Interactor* i, int w\n) : " << coreclass;
    out << "(name, i, w) {}\n\n";
    return out.good();
}

boolean PannerCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();

    if (!snamer->IsSubclass() && !_namelist->Search("panner")) {
        _namelist->Append("panner");
        out << "#include <InterViews/panner.h> \n";
    }
    return out.good();
}

