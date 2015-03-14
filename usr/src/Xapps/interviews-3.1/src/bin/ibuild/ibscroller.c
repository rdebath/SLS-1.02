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
 * Scroller component definitions.
 */

#include "ibclasses.h"
#include "ibcmds.h"
#include "ibdialogs.h"
#include "ibscroller.h"
#include "ibvars.h"
#include "ibvarviews.h"

#include <Unidraw/catalog.h>
#include <Unidraw/ulist.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/Graphic/pspaint.h>

#include <InterViews/paint.h>
#include <InterViews/painter.h>
#include <InterViews/shape.h>
#include <InterViews/transformer.h>

#include <math.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/

static const int inset = 1;     /* space between scroller canvas and bar */

/*****************************************************************************/

ScrollerGraphic* ScrollerComp::GetScrollerGraphic () {
    return (ScrollerGraphic*) GetGraphic();
}

ClassId ScrollerComp::GetClassId () { return SCROLLER_COMP; }

boolean ScrollerComp::IsA (ClassId id) {
    return SCROLLER_COMP == id || HVComp::IsA(id);
}

ScrollerComp::ScrollerComp (ScrollerGraphic* g) : HVComp(g) {
    if (g != nil) {
        ShapeVar* shapeVar = GetShapeVar();
        Shape* shape = shapeVar->GetShape();
        int nat, shr, str;
        g->GetShape(nat, shr, str);

        if (g->GetOrientation() == Horizontal) {
            shape->Rect(nat, g->MinorAxisSize());
            shape->Rigid(shr, str, 0, 0);
            GetClassNameVar()->SetName("HScroller");
            GetClassNameVar()->SetBaseClass("HScroller");

        } else {
            shape->Rect(g->MinorAxisSize(), nat);
            shape->Rigid(0, 0, shr, str);
            GetClassNameVar()->SetName("VScroller");
            GetClassNameVar()->SetBaseClass("VScroller");
        }
    }
    _adjusteeVar = nil;
}

ScrollerComp::~ScrollerComp () {
    delete _adjusteeVar;
}

void ScrollerComp::Instantiate () {
    HVComp::Instantiate();
    if (_adjusteeVar == nil) {
        _adjusteeVar = new MemberNameVar("", false, false);
    }
}

void ScrollerComp::SetState(const char* adjustee, StateVar* stateVar) {
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

InteractorComp& ScrollerComp::operator = (InteractorComp& comp) {
    StateVar* adjusteevar = comp.GetState("AdjusteeVar");

    if (adjusteevar != nil) {
        SetState("AdjusteeVar", adjusteevar);

    } else {
        MemberNameVar* member = comp.GetMemberNameVar();
        SetState("AdjusteeVar", member);
    }
    return *this;
}

boolean ScrollerComp::IsRelatableTo (InteractorComp* comp) {
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

void ScrollerComp::Interpret (Command* cmd) {
    if (!cmd->IsA(FONT_CMD)) {
        HVComp::Interpret(cmd);
    }
}

void ScrollerComp::Uninterpret (Command* cmd) {
    if (!cmd->IsA(FONT_CMD)) {
        HVComp::Uninterpret(cmd);
    }
}

HVGraphic* ScrollerComp::InitGraphic (Orientation o, int) {
    return new ScrollerGraphic(o, GetCanvasVar());
}

StateVar* ScrollerComp::GetState (const char* name) {
    StateVar* stateVar = nil;

    if (strcmp(name, "AdjusteeVar") == 0) {
        stateVar = _adjusteeVar;
    } else {
        stateVar = HVComp::GetState(name);
    }

    return stateVar;
}

void ScrollerComp::Read (istream& in) {
    HVComp::Read(in);
    delete _adjusteeVar;

    _adjusteeVar = (MemberNameVar*) unidraw->GetCatalog()->ReadStateVar(in);
}

void ScrollerComp::Write (ostream& out) {
    HVComp::Write(out);
    unidraw->GetCatalog()->WriteStateVar(_adjusteeVar, out);
}

/*****************************************************************************/

ScrollerView::ScrollerView (ScrollerComp* subj) : HVView(subj) { }
ClassId ScrollerView::GetClassId () { return SCROLLER_VIEW; }

boolean ScrollerView::IsA (ClassId id) {
    return SCROLLER_VIEW == id || HVView::IsA(id);
}

ScrollerComp* ScrollerView::GetScrollerComp () {
    return (ScrollerComp*) GetSubject();
}

InfoDialog* ScrollerView::GetInfoDialog () {
    InfoDialog* info = InteractorView::GetInfoDialog();
    ButtonState* state = info->GetState();
    ScrollerComp* scomp = GetScrollerComp();
    MemberNameVar* adjusteeVar = scomp->GetAdjusteeVar();
    info->Include(new RelatedVarView(adjusteeVar, state, scomp));
    return info;
}

HVComp* ScrollerView::InitComp (Coord , Coord , Coord , Coord) {
    HVGraphic* gs = GetHVComp()->GetHVGraphic();
    Orientation orient = gs->GetOrientation();
    ScrollerGraphic* g = new ScrollerGraphic(orient, nil, gs);
    return new ScrollerComp(g);
}

/*****************************************************************************/

boolean ScrollerCode::IsA(ClassId id) {
    return SCROLLER_CODE == id || CodeView::IsA(id);
}

ClassId ScrollerCode::GetClassId () { return SCROLLER_CODE; }

ScrollerCode::ScrollerCode (ScrollerComp* subj) : CodeView(subj) {}

void ScrollerCode::Update () {
    CodeView::Update();
    InteractorComp* subj = GetIntComp();
    Graphic* gr = subj->GetGraphic();
    gr->SetFont(nil);
}

ScrollerComp* ScrollerCode::GetScrollerComp () {
    return (ScrollerComp*) GetSubject();
}

boolean ScrollerCode::Definition (ostream& out) {
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
                _scope && mnamer->GetExport() && !_namelist->Search("scroller")
            ) {
                _namelist->Append("scroller");
                out << "#include <InterViews/scroller.h>\n";
            }
        } else {
            ok = ok && CodeView::Definition(out);
        }

    } else if (_emitCorehHeader) {
	InteractorComp* icomp = GetIntComp();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        const char* subclass = snamer->GetName();
        if (snamer->IsSubclass() && strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("scroller")) {
                _namelist->Append("scroller");
                out << "#include <InterViews/scroller.h>\n";
            }
        }
    } else if (_emitInstanceInits) {
        InteractorComp* icomp = GetIntComp();
        InteractorComp* ctarget = nil;
        const char* mname = icomp->GetMemberNameVar()->GetName();
	MemberNameVar* mnamer = (MemberNameVar*) icomp->GetState(
	    "AdjusteeVar"
	);
        const char* scrollee = mnamer->GetName();

	if (*scrollee == '\0') {
            if (_err_count < 10) {
                strcat(_errbuf, mname);
                strcat(_errbuf, " has undefined scrolling target.\n");
                _err_count++;
            } 
	    return false;

        } else if (!Search(mnamer, ctarget)) {
            if (_err_count < 10) {
                strcat(_errbuf, mname);
                strcat(
                    _errbuf, "'s scrolling target is not in the same hierarchy.\n"
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
	if (_instancelist->Find((void*) scrollee)) {
            if (!_instancelist->Find((void*) mname)) {
                _instancelist->Append(new UList((void*)mname));

        	scrollee = (*scrollee == '\0') ? "nil" : scrollee;

        	BeginInstantiate(out);
        	out << "(";
        	InstanceName(out);
        	out << scrollee << ")";
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

boolean ScrollerCode::CoreConstDecls(ostream& out) { 
    out << "(const char*, Interactor*);\n";
    return out.good();
}

boolean ScrollerCode::CoreConstInits(ostream& out) {
    InteractorComp* icomp = GetIntComp();
    SubclassNameVar* snamer = icomp->GetClassNameVar();
    const char* baseclass = snamer->GetBaseClass();
    const char* subclass = snamer->GetName();

    out << "(\n    const char* name, Interactor* i\n) : " << baseclass;
    out << "(name, i) {\n";
    out << "    SetClassName(\"" << subclass << "\");\n";
    out << "}\n\n";
    return out.good();
}

boolean ScrollerCode::ConstDecls(ostream& out) {
    out << "(const char*, Interactor* i);\n";
    return out.good();
}

boolean ScrollerCode::ConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n    const char* name, Interactor* i\n) : " << coreclass;
    out << "(name, i) {}\n\n";
    return out.good();
}

boolean ScrollerCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();

    if (!snamer->IsSubclass() && !_namelist->Search("scroller")) {
        _namelist->Append("scroller");
        out << "#include <InterViews/scroller.h> \n";
    }
    return out.good();
}

/*****************************************************************************/

ScrollerGraphic::ScrollerGraphic (
    Orientation o, CanvasVar* c, Graphic* g, int nat
) : HVGraphic(c, g) {
    int f = (o == Horizontal) ? hfil : vfil;
    Init(nat, f, f, o);
}

int ScrollerGraphic::MinorAxisSize () { return round(.2*inch); }

Graphic* ScrollerGraphic::Copy () {
    return new ScrollerGraphic(GetOrientation(), nil, this, _natural);
}

void ScrollerGraphic::draw (Canvas* c, Graphic* gs) {
    update(gs);
    Coord xmax, ymax;
    CanvasVar* cvar = GetCanvasVar();

    if (cvar == nil) {
        if (_orientation == Horizontal) {
            xmax = _natural;
            ymax = MinorAxisSize() - 1;
        } else {
            xmax = MinorAxisSize() - 1;
            ymax = _natural;
        }
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
    if (_orientation == Horizontal) {
        l = b = inset;
        r = xmax/3;
        t = ymax-inset;
    } else {
        l = inset;
        b = ymax - ymax/3;
        r = xmax - inset;
        t = ymax - inset;
    }
    _p->ClearRect(c, l, b, r, t);
    _p->Rect(c, l, b, r, t);
    _p->SetPattern(gs->GetPattern());
}
