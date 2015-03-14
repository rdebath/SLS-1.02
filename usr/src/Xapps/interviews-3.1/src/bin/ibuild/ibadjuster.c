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
 * Adjuster component definitions.
 */

#include "ibadjuster.h"
#include "ibclasses.h"
#include "ibcmds.h"
#include "ibdialogs.h"
#include "ibed.h"
#include "ibgraphic.h"
#include "ibvars.h"
#include "ibvarviews.h"

#include <Unidraw/catalog.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/creator.h>
#include <Unidraw/iterator.h>
#include <Unidraw/manips.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/ulist.h>
#include <Unidraw/viewer.h>
#include <Unidraw/Commands/edit.h>
#include <Unidraw/Commands/macro.h>
#include <Unidraw/Tools/tool.h>

#include <InterViews/bitmap.h>
#include <InterViews/canvas.h>
#include <InterViews/painter.h>
#include <InterViews/shape.h>
#include <InterViews/Bitmaps/dmoverMask.bm>
#include <InterViews/Bitmaps/umoverMask.bm>
#include <InterViews/Bitmaps/lmoverMask.bm>
#include <InterViews/Bitmaps/rmoverMask.bm>
#include <InterViews/Bitmaps/enlargeMask.bm>
#include <InterViews/Bitmaps/reducerMask.bm>
#include <InterViews/Bitmaps/dmover.bm>
#include <InterViews/Bitmaps/umover.bm>
#include <InterViews/Bitmaps/lmover.bm>
#include <InterViews/Bitmaps/rmover.bm>
#include <InterViews/Bitmaps/enlarge.bm>
#include <InterViews/Bitmaps/reducer.bm>

#include <InterViews/event.h>
#include <InterViews/paint.h>
#include <InterViews/rubrect.h>
#include <InterViews/transformer.h>

#include <stream.h>
#include <string.h>

/*****************************************************************************/

AdjusterGraphic* AdjusterComp::GetAdjusterGraphic () {
    return (AdjusterGraphic*) GetGraphic();
}

ClassId AdjusterComp::GetClassId () { return ADJUSTER_COMP; }

boolean AdjusterComp::IsA (ClassId id) {
    return ADJUSTER_COMP == id || InteractorComp::IsA(id);
}

AdjusterComp::AdjusterComp (AdjusterGraphic* g) : InteractorComp(g) { 
    if (g != nil) {
        GetClassNameVar()->SetName(g->GetClassName());
        GetClassNameVar()->SetBaseClass(g->GetClassName());
    }
    _adjusteeVar = nil;
}

AdjusterComp::~AdjusterComp () {
    delete _adjusteeVar;
}

void AdjusterComp::Instantiate () {
    InteractorComp::Instantiate();
   if (_adjusteeVar == nil) {
        _adjusteeVar = new MemberNameVar("", false, false);
    }
}

StateVar* AdjusterComp::GetState (const char* name) {
    StateVar* stateVar = nil;

    if (strcmp(name, "AdjusteeVar") == 0) {
        stateVar = _adjusteeVar;
    } else {
        stateVar = InteractorComp::GetState(name);
    }

    return stateVar;
}

void AdjusterComp::SetState(const char* adjustee, StateVar* stateVar) {
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

InteractorComp& AdjusterComp::operator = (InteractorComp& comp) {
    StateVar* adjusteevar = comp.GetState("AdjusteeVar");
    if (adjusteevar != nil) {
        SetState("AdjusteeVar", adjusteevar);

    } else {
        MemberNameVar* member = comp.GetMemberNameVar();
        SetState("AdjusteeVar", member);
    }
    return *this;
}

boolean AdjusterComp::IsRelatableTo (InteractorComp* comp) {
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

void AdjusterComp::Interpret (Command* cmd) {
    if (cmd->IsA(COLOR_CMD)) {
	ColorCmd* colorCmd = (ColorCmd*) cmd;
        PSColor* bg = colorCmd->GetBgColor();
	if (bg == nil) {
	    InteractorComp::Interpret(cmd);
	}
    } else if (!cmd->IsA(FONT_CMD) && !cmd->IsA(BRUSH_CMD)) {
        InteractorComp::Interpret(cmd);
    }
}

void AdjusterComp::Uninterpret (Command* cmd) {
    if (cmd->IsA(COLOR_CMD)) {
	ColorCmd* colorCmd = (ColorCmd*) cmd;
        PSColor* bg = colorCmd->GetBgColor();
	if (bg == nil) {
	    InteractorComp::Uninterpret(cmd);
	}
    } else if (!cmd->IsA(FONT_CMD) && !cmd->IsA(BRUSH_CMD)) {
        InteractorComp::Uninterpret(cmd);
    }
}

void AdjusterComp::Reconfig () {
    char* name = (char*)GetClassNameVar()->GetName();
    Shape* shape = GetShapeVar()->GetShape();
    int w, h;
    GetAdjusterGraphic()->GetSize(w, h);
    shape->Rect(w, h);
    if (strcmp(name, "LeftMover") == 0) {
        shape->Rigid(w/2, 0, h/2, vfil);
    } else if (strcmp(name, "RightMover") == 0) {
        shape->Rigid(w/2, 0, h/2, vfil);
    } else if (strcmp(name, "UpMover") == 0) {
        shape->Rigid(w/2, hfil, h/2);
    } else if (strcmp(name, "DownMover") == 0) {
        shape->Rigid(w/2, hfil, h/2);
    } else if (strcmp(name, "Enlarger") == 0) {
        shape->Rigid(w/2, hfil, h/2);
    } else if (strcmp(name, "Reducer") == 0) {
        shape->Rigid(w/2, hfil, h/2);
    }
    GetShapeVar()->Notify();
}

void AdjusterComp::Read (istream& in) {
    InteractorComp::Read(in);
    Catalog* catalog = unidraw->GetCatalog();
    ClassId id;
    in >> id;
    AdjusterGraphic* g = (AdjusterGraphic*) catalog->GetCreator()->Create(id);
    g->Read(in);
    g->SetCanvasVar(GetCanvasVar());
    SetGraphic(g);

    delete _adjusteeVar;
    _adjusteeVar = (MemberNameVar*) unidraw->GetCatalog()->ReadStateVar(in);
}

void AdjusterComp::Write (ostream& out) {
    ClassId id;
    InteractorComp::Write(out);
    Catalog* catalog = unidraw->GetCatalog();
    AdjusterGraphic* g = GetAdjusterGraphic();
    id = g->GetClassId();
    out << " " << id << " ";
    g->Write(out);
    unidraw->GetCatalog()->WriteStateVar(_adjusteeVar, out);
}

/*****************************************************************************/

AdjusterView::AdjusterView (AdjusterComp* subj) : InteractorView(subj) { }

AdjusterComp* AdjusterView::GetAdjusterComp () { 
    return (AdjusterComp*) GetSubject();
}

ClassId AdjusterView::GetClassId () { return ADJUSTER_VIEW; }

boolean AdjusterView::IsA (ClassId id) {
    return ADJUSTER_VIEW == id || InteractorView::IsA(id);
}

void AdjusterView::Update () {
    AdjusterGraphic* acomp = GetAdjusterComp()->GetAdjusterGraphic();
    AdjusterGraphic* aview = (AdjusterGraphic*) GetGraphic();

    IncurDamage(aview);
    *(Graphic*)aview = *(Graphic*)acomp;
    UpdateCanvasVar();
    IncurDamage(aview);
    EraseHandles();
}

InfoDialog* AdjusterView::GetInfoDialog () {
    InfoDialog* info = InteractorView::GetInfoDialog();
    ButtonState* state = info->GetState();
    AdjusterComp* acomp = GetAdjusterComp();
    MemberNameVar* adjusteeVar = acomp->GetAdjusteeVar();
    info->Include(new RelatedVarView(adjusteeVar, state, acomp));
    return info;
}

/*****************************************************************************/

ClassId AdjusterCode::GetClassId () { return ADJUSTER_CODE; }

boolean AdjusterCode::IsA(ClassId id) {
    return ADJUSTER_CODE == id || CodeView::IsA(id);
}

AdjusterCode::AdjusterCode (AdjusterComp* subj) : CodeView(subj) {}

void AdjusterCode::Update () {
    CodeView::Update();
    InteractorComp* subj = GetIntComp();
    Graphic* gr = subj->GetGraphic();
    gr->SetFont(nil);
}

AdjusterComp* AdjusterCode::GetAdjusterComp () {
    return (AdjusterComp*) GetSubject(); 
}

boolean AdjusterCode::Definition (ostream& out) {
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
                _scope && mnamer->GetExport() && !_namelist->Search("adjuster")
            ) {
                _namelist->Append("adjuster");
                out << "#include <InterViews/adjuster.h>\n";
            }
        } else {
            ok = ok && CodeView::Definition(out);
        }
    } else if (_emitCorehHeader) {
	InteractorComp* icomp = GetIntComp();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        const char* subclass = snamer->GetName();
        if (snamer->IsSubclass() && strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("adjuster")) {
                _namelist->Append("adjuster");
                out << "#include <InterViews/adjuster.h>\n";
            }
        }
    } else if (_emitInstanceInits) {
        InteractorComp* icomp = GetIntComp();
        InteractorComp* ctarget = nil;
        const char* mname = icomp->GetMemberNameVar()->GetName();
        AdjusterComp* adjuster = GetAdjusterComp();
	MemberNameVar* mnamer = (MemberNameVar*) icomp->GetState(
	    "AdjusteeVar"
	);
	const char* adjustee = mnamer->GetName();

	if (*adjustee == '\0') {
            if (_err_count < 10) {
                strcat(_errbuf, mname);
                strcat(_errbuf, " has undefined adjusting target.\n");
                _err_count++;
            }
            return false;

        } else if (!Search(mnamer, ctarget)) {
            if (_err_count < 10) {
                strcat(_errbuf, mname);
                strcat(
                    _errbuf, 
                    "'s adjusting target is not in the same hierarchy.\n"
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
	if (_instancelist->Find((void*) adjustee)) {
	    if (!_instancelist->Find((void*) mname)) {
                _instancelist->Append(new UList((void*)mname));
        
        	BeginInstantiate(out);
        	out << "(";
        	InstanceName(out);
		const char* classname = icomp->GetClassNameVar()->GetBaseClass();

		if ( 
		    strcmp(classname, "Enlarger") == 0 ||
                    strcmp(classname, "Reducer") == 0
                ) {
            	    out << adjustee << ")";

		} else if (strcmp(classname, "Panner") == 0) {
	    	    int width = adjuster->GetShapeVar()->GetShape()->width;
	    	    out << adjustee << ", " << width << ")";

		} else {
	    	    out << adjustee << ", 1" << ")";
		}
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
    return out.good();
}

boolean AdjusterCode::CoreConstDecls(ostream& out) { 
    const char* classname =GetIntComp()->GetClassNameVar()->GetBaseClass();

    out << "(const char*, Interactor*";
    if ( 
        strcmp(classname, "Enlarger") == 0 ||
        strcmp(classname, "Reducer") == 0
    ) {
        out << ");\n";

    } else {
        out << ", int);\n";
    }
    return out.good();
}

boolean AdjusterCode::CoreConstInits(ostream& out) {
    const char* classname = GetIntComp()->GetClassNameVar()->GetBaseClass();
    const char* subclass = GetIntComp()->GetClassNameVar()->GetName();

    out << "(\n    const char* name, Interactor* i";
    if ( 
        strcmp(classname, "Enlarger") == 0 ||
        strcmp(classname, "Reducer") == 0
    ) {
        out << "\n) : " << classname << "(name, i) {\n";
        out << "    SetClassName(\"" << subclass << "\");\n";
        out << "}\n\n";

    } else {
        out << ", int w\n) : " << classname << "(name, i, w) {\n";
        out << "    SetClassName(\"" << subclass << "\");\n";
        out << "}\n\n";
    }

    return out.good();
}

boolean AdjusterCode::ConstDecls(ostream& out) {
    const char* classname =GetIntComp()->GetClassNameVar()->GetBaseClass();

    out << "(const char*, Interactor*";
    if ( 
        strcmp(classname, "Enlarger") == 0 ||
        strcmp(classname, "Reducer") == 0
    ) {
        out << ");\n";

    } else {
        out << ", int);\n";
    }
    return out.good();
}

boolean AdjusterCode::ConstInits(ostream& out) {
    const char* classname =GetIntComp()->GetClassNameVar()->GetBaseClass();
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n    const char* name, Interactor* i";
    if ( 
        strcmp(classname, "Enlarger") == 0 ||
        strcmp(classname, "Reducer") == 0
    ) {
        out << "\n) : " << coreclass << "(name, i) {}\n\n";

    } else {
        out << ", int w\n) : " << coreclass << "(name, i, w) {}\n\n";
    }

    return out.good();
}

boolean AdjusterCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();

    if (!snamer->IsSubclass() && !_namelist->Search("adjuster")) {
        _namelist->Append("adjuster");
        out << "#include <InterViews/adjuster.h> \n";
    }
    return out.good();
}

/*****************************************************************************/

AdjusterGraphic::AdjusterGraphic (
    CanvasVar* c, Graphic* g,
    Bitmap* fg_map, Bitmap* bg_map, int h, int w
) : IBGraphic(c, g) {
    Init(fg_map, bg_map, h, w);
}

void AdjusterGraphic::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    CanvasVar* cvar = GetCanvasVar();

    if (cvar == nil) {
        int w, h;
	w = _wmap;
	h = _hmap;
        CalcExtent(w, h, l, b, cx, cy, tol, gs);
    } else {
        CalcExtent(cvar->Width(), cvar->Height(), l,b,cx,cy,tol,gs);
    }
    tol = 0;
}

void AdjusterGraphic::Init(Bitmap* fg_map, Bitmap* bg_map, int h, int w) {

    _fg_map = fg_map;
    if (_fg_map != nil) {
    	_fg_map->Reference();
    }
    _bg_map = bg_map;
    if (_bg_map != nil) {
    	_bg_map->Reference();
    }
    _hmap = h;
    _wmap = w;
}

AdjusterGraphic::~AdjusterGraphic () {
    _fg_map->Unreference();
    _bg_map->Unreference();
}

Graphic* AdjusterGraphic::Copy () {
    return new AdjusterGraphic(nil, this, _fg_map, _bg_map, _hmap, _wmap);
}

void AdjusterGraphic::Read (istream& in) {
    ReadGS(in);
}

void AdjusterGraphic::Write (ostream& out) {
    WriteGS(out);
}

const char* AdjusterGraphic::GetClassName () { return "adjuster"; }
ClassId AdjusterGraphic::GetClassId () { return ADJUSTER_GRAPHIC; }

boolean AdjusterGraphic::IsA (ClassId id) {
    return ADJUSTER_GRAPHIC == id;
}

void AdjusterGraphic::draw (Canvas* c, Graphic* gs) {
    Coord xmax, ymax;

    if (GetCanvasVar() == nil) {
	xmax = _wmap;
	ymax = _hmap;
    } else {
        xmax = GetCanvasVar()->xmax();
        ymax = GetCanvasVar()->ymax();
    }
    update(gs);
    int offx = 0;
    int offy = 0;
    if (xmax >= _wmap) {
        offx = xmax - _wmap;
    }
    if (ymax >= _hmap) {
        offy = ymax - _hmap;
    }
    _p->Stencil(c, offx/2, offy/2, _fg_map, _bg_map);
}

void AdjusterGraphic::drawClipped (
    Canvas* c, Coord l, Coord b, Coord r, Coord t, Graphic* gs
) {
    Graphic::drawClipped(c, l, b, r, t, gs);
}

/*****************************************************************************/

LMoverGraphic::LMoverGraphic (
    CanvasVar* cv, Graphic* g
) : AdjusterGraphic(cv, g) {
    if (_fg_map == nil) {
        _fg_map = new Bitmap(
            lmover_plain_bits, lmover_plain_width, lmover_plain_height
        );
        _bg_map = new Bitmap(
            lmover_mask_bits, lmover_mask_width, lmover_mask_height
        );
    }
    Init(_fg_map, _bg_map, lmover_plain_height, lmover_plain_width);
}

const char* LMoverGraphic::GetClassName () { return "LeftMover"; }
ClassId LMoverGraphic::GetClassId () { return LMOVER_GRAPHIC; }

boolean LMoverGraphic::IsA (ClassId id) {
    return LMOVER_GRAPHIC == id || AdjusterGraphic::IsA(id);
}

/*****************************************************************************/

RMoverGraphic::RMoverGraphic (
    CanvasVar* cv, Graphic* g
) : AdjusterGraphic(cv, g) {
    if (_fg_map == nil) {
        _fg_map = new Bitmap(
            rmover_plain_bits, rmover_plain_width, rmover_plain_height
        );
        _bg_map = new Bitmap(
            rmover_mask_bits, rmover_mask_width, rmover_mask_height
        );
    }
    Init(_fg_map, _bg_map, rmover_plain_height, rmover_plain_width);
}

const char* RMoverGraphic::GetClassName () { return "RightMover"; }
ClassId RMoverGraphic::GetClassId () { return RMOVER_GRAPHIC; }

boolean RMoverGraphic::IsA (ClassId id) {
    return RMOVER_GRAPHIC == id || AdjusterGraphic::IsA(id);
}

/*****************************************************************************/

UMoverGraphic::UMoverGraphic (
    CanvasVar* cv, Graphic* g
) : AdjusterGraphic(cv, g) {
    if (_fg_map == nil) {
        _fg_map = new Bitmap(
            umover_plain_bits, umover_plain_width, umover_plain_height
        );
        _bg_map = new Bitmap(
            umover_mask_bits, umover_mask_width, umover_mask_height
        );
    }
    Init(_fg_map, _bg_map, umover_plain_height, umover_plain_width);
}

const char* UMoverGraphic::GetClassName () { return "UpMover"; }
ClassId UMoverGraphic::GetClassId () { return UMOVER_GRAPHIC; }
boolean UMoverGraphic::IsA (ClassId id) {
    return UMOVER_GRAPHIC == id || AdjusterGraphic::IsA(id);
}

/*****************************************************************************/

DMoverGraphic::DMoverGraphic (
    CanvasVar* cv, Graphic* g
) : AdjusterGraphic(cv, g) {
    if (_fg_map == nil) {
        _fg_map = new Bitmap(
            dmover_plain_bits, dmover_plain_width, dmover_plain_height
        );
        _bg_map = new Bitmap(
            dmover_mask_bits, dmover_mask_width, dmover_mask_height
        );
    }
    Init(_fg_map, _bg_map, dmover_plain_height, dmover_plain_width);
}

const char* DMoverGraphic::GetClassName () { return "DownMover"; }
ClassId DMoverGraphic::GetClassId () { return DMOVER_GRAPHIC; }

boolean DMoverGraphic::IsA (ClassId id) {
    return DMOVER_GRAPHIC == id || AdjusterGraphic::IsA(id);
}

/*****************************************************************************/

EnlargerGraphic::EnlargerGraphic (
    CanvasVar* cv, Graphic* g
) : AdjusterGraphic(cv, g) {
    if (_fg_map == nil) {
        _fg_map = new Bitmap(
            enlarger_plain_bits, enlarger_plain_width, enlarger_plain_height
        );
        _bg_map = new Bitmap(
            enlarger_mask_bits, enlarger_mask_width, enlarger_mask_height
        );
    }
    Init(_fg_map, _bg_map, enlarger_plain_height, enlarger_plain_width);
}

const char* EnlargerGraphic::GetClassName () { return "Enlarger"; }
ClassId EnlargerGraphic::GetClassId () { return ENLARGER_GRAPHIC; }

boolean EnlargerGraphic::IsA (ClassId id) {
    return ENLARGER_GRAPHIC == id || AdjusterGraphic::IsA(id);
}

/*****************************************************************************/

ReducerGraphic::ReducerGraphic (
    CanvasVar* cv, Graphic* g
) : AdjusterGraphic(cv, g) {
    if (_fg_map == nil) {
        _fg_map = new Bitmap(
            reducer_plain_bits, reducer_plain_width, reducer_plain_height
        );
        _bg_map = new Bitmap(
            reducer_mask_bits, reducer_mask_width, reducer_mask_height
        );
    }
    Init(_fg_map, _bg_map, reducer_plain_height, reducer_plain_width);
}

const char* ReducerGraphic::GetClassName () { return "Reducer"; }
ClassId ReducerGraphic::GetClassId () { return REDUCER_GRAPHIC; }

boolean ReducerGraphic::IsA (ClassId id) {
    return REDUCER_GRAPHIC == id || AdjusterGraphic::IsA(id);
}

