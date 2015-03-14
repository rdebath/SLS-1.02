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
 *  StrBrowser component definitions
 */

#include "ibclasses.h"
#include "ibcmds.h"
#include "ibed.h"
#include "ibdialogs.h"
#include "ibmessage.h"
#include "ibrubrect.h"
#include "ibstrbrowser.h"
#include "ibtools.h"
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

StrBrowserComp::StrBrowserComp (StrBrowserGraphic* g) : ButtonComp(g) {
    _uniqueSel = nil;
}

StrBrowserComp::~StrBrowserComp () {
    delete _uniqueSel;
}

ClassId StrBrowserComp::GetClassId () { return STRBROWSER_COMP; }

boolean StrBrowserComp::IsA (ClassId id) {
    return STRBROWSER_COMP == id || ButtonComp::IsA(id);
}

StrBrowserGraphic* StrBrowserComp::GetStrBrowserGraphic () {
    return (StrBrowserGraphic*) GetGraphic();
}

void StrBrowserComp::Reconfig () {
    Shape* shape = GetShapeVar()->GetShape();
    StrBrowserGraphic* g = GetStrBrowserGraphic();
    int w, h, rows, cols;
    g->Natural(w, h);
    g->GetRowsCols(rows, cols);
    shape->Rect(w, h);
    shape->Rigid(hfil, hfil, h - h/rows, vfil);
    GetShapeVar()->Notify();
}

void StrBrowserComp::Instantiate() {
    if (_instanceNameVar == nil) {
        ButtonComp::Instantiate();
        GetMemberNameVar()->SetExport(true);
        GetButtonStateVar()->HideSetting();
        _uniqueSel = new BooleanStateVar(true);
    } else {
        ButtonComp::Instantiate();
    }
}

StateVar* StrBrowserComp::GetState (const char* name) {
    StateVar* stateVar = nil;

    if (strcmp(name, "UniqueSel") == 0) {
        stateVar = _uniqueSel;
    } else {
        stateVar = ButtonComp::GetState(name);
    }

    return stateVar;
}

void StrBrowserComp::SetState (const char* name, StateVar* stateVar) {
    if (strcmp(name, "UniqueSel") == 0) {
        _uniqueSel = (BooleanStateVar*) stateVar;
    } else {
        ButtonComp::SetState(name, stateVar);
    }
}

void StrBrowserComp::Read (istream& in) {
    ButtonComp::Read(in);
    delete _uniqueSel;
    _uniqueSel = (BooleanStateVar*) unidraw->GetCatalog()->ReadStateVar(in);
}

void StrBrowserComp::Write (ostream& out) {
    ButtonComp::Write(out);
    unidraw->GetCatalog()->WriteStateVar(_uniqueSel, out);
}

/*****************************************************************************/

StrBrowserComp* StrBrowserView::GetStrBrowserComp() {
    return (StrBrowserComp*) GetSubject();
}

StrBrowserView::StrBrowserView (StrBrowserComp* subj) : ButtonView(subj) { }

ClassId StrBrowserView::GetClassId () { return STRBROWSER_VIEW; }

boolean StrBrowserView::IsA (ClassId id) {
    return STRBROWSER_VIEW == id || ButtonView::IsA(id);
}

Manipulator* StrBrowserView::CreateManipulator (
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
	m = MessageView::CreateManipulator(v, e, rel, tool);
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
    } else if (!tool->IsA(RESHAPE_TOOL)){
        m = MessageView::CreateManipulator(v, e, rel, tool);
    }
    return m;
}

Command* StrBrowserView::InterpretManipulator (Manipulator* m) {
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
	FontVar* fontVar = (FontVar*) ed->GetState("FontVar");

        StrBrowserComp* comp = (StrBrowserComp*) GetStrBrowserComp()->Copy();
        StrBrowserGraphic* g = (StrBrowserGraphic*) comp->GetGraphic();
	g->SetRowsCols(y1-y0+1, x1-x0+1);

	if (colVar != nil) {
            g->SetColors(colVar->GetFgColor(), colVar->GetBgColor());
        }
        if (fontVar != nil) {
            g->SetFont(fontVar->GetFont());
        }
	cmd = new MacroCmd(
            ed, new PasteCmd(ed, new Clipboard(comp)),
            new PlaceCmd(ed, x0, y0, x1-1, y1-1, new Clipboard(comp))
        );

    } else if (!tool->IsA(RESHAPE_TOOL)){
        cmd = MessageView::InterpretManipulator(m);
    }
    return cmd;
}

InfoDialog* StrBrowserView::GetInfoDialog () {
    IBEditor* ibed = (IBEditor*) GetViewer()->GetEditor();
    InfoDialog* info = ButtonView::GetInfoDialog();
    ButtonState* state = info->GetState();
    StrBrowserComp* scomp = GetStrBrowserComp();
    BooleanStateVar* uniqueSel = scomp->GetUniqueSel();
    info->Include(
	new BooleanStateVarView(uniqueSel, " Unique Selection ")
    );
    return info;
}

/*****************************************************************************/

boolean StrBrowserCode::IsA (ClassId id) {
    return STRBROWSER_CODE == id || ButtonCode::IsA(id);
}

ClassId StrBrowserCode::GetClassId () { return STRBROWSER_CODE; }
StrBrowserCode::StrBrowserCode (StrBrowserComp* subj) : ButtonCode(subj) { }

StrBrowserComp* StrBrowserCode::GetStrBrowserComp () {
    return (StrBrowserComp*) GetSubject();
}

boolean StrBrowserCode::Definition (ostream& out) {
    boolean ok = true;
    InteractorComp* icomp = GetIntComp();
    MemberNameVar* mnamer = icomp->GetMemberNameVar();
    SubclassNameVar* snamer = icomp->GetClassNameVar();
    ButtonStateVar* bsVar = icomp->GetButtonStateVar();
    if (
	_emitProperty || _emitBSDecls ||
	_emitBSInits || _emitInstanceDecls || _emitHeaders ||
	_emitFunctionDecls || _emitFunctionInits || _emitClassHeaders
    ) {
        return CodeView::Definition(out);

    } else if (_emitForward) {
        ok = ok && ButtonCode::Definition(out);

    } else if (_emitExpHeader) {
        if (!snamer->IsSubclass() || !bsVar->IsSubclass()) {
            if (_scope) {
                if (
                    mnamer->GetExport()&&!_namelist->Search("strbrowser") &&
                    !snamer->IsSubclass()
                ) {
                    _namelist->Append("strbrowser");
                    out << "#include <InterViews/strbrowser.h>\n";
                }
                if (
                    bsVar->GetExport() && !_namelist->Search("button") &&
                    !bsVar->IsSubclass()
                ) {
                    _namelist->Append("button");
                    out << "#include <InterViews/button.h>\n";
                }
            }
            ok = ok && CodeView::Definition(out);
        } else {
            ok = ok && ButtonCode::Definition(out);
        }
    } else if (_emitCorehHeader) {
        const char* subclass = snamer->GetName();
        const char* bsclass = bsVar->GetSubclassName();
        const char* fwname = GetFirewall();

        if (snamer->IsSubclass() && strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("strbrowser")) {
                _namelist->Append("strbrowser");
                out << "#include <InterViews/strbrowser.h>\n";
            }
        }
        if (bsVar->IsSubclass() && strcmp(bsclass, _classname) == 0) {
            if (!_namelist->Search("button")) {
                _namelist->Append("button");
                out << "#include <InterViews/button.h>\n";
            }
            if (fwname != nil && !_namelist->Search(fwname)) {
                _namelist->Append(fwname);
                out << "#include \"" << fwname << "-core.h\"\n";
            }
        }
    } else if (_emitInstanceInits) {
        InteractorComp* icomp = GetIntComp();
        const char* mname = icomp->GetMemberNameVar()->GetName();

        if (!_instancelist->Find((void*) mname)) {
            _instancelist->Append(new UList((void*)mname));

            StrBrowserComp* strbcomp = GetStrBrowserComp();
            int rows, cols;
    
            int uniqueSel = (int) strbcomp->GetUniqueSel()->GetBooleanState();

            BeginInstantiate(out);
            StrBrowserGraphic* graphic = strbcomp->GetStrBrowserGraphic();
            graphic->GetRowsCols(rows, cols);
            ButtonStateVar* bsVar = strbcomp->GetButtonStateVar();

            out << "(";
            InstanceName(out);
            out << bsVar->GetName() << ", ";
            out << rows << ", " << cols << ", ";
            if (uniqueSel) {
                out << "true";
            } else {
                out << "false";
            }
            out << ")";
            EndInstantiate(out);
        }

    } else if (
        _emitCoreDecls || _emitCoreInits || _emitClassDecls || _emitClassInits
    ) {
	ok = ok && ButtonCode::Definition(out);
        
    } else if (_emitMain) {
	ok = ok && CodeView::Definition(out);
        
    }
    return out.good() && ok;
}

boolean StrBrowserCode::CoreConstDecls(ostream& out) { 
    out << "(const char*, ButtonState*, int, int, boolean);\n";
    return out.good();
}

boolean StrBrowserCode::CoreConstInits(ostream& out) {
    InteractorComp* icomp = GetIntComp();
    SubclassNameVar* snamer = icomp->GetClassNameVar();
    const char* baseclass = snamer->GetBaseClass();
    const char* subclass = snamer->GetName();

    out << "(\n    const char* name, ButtonState* bs,";
    out << "int rows, int cols, boolean u\n) : " << baseclass;
    out << "(name, bs, rows, cols, u) {\n";
    out << "    SetClassName(\"" << subclass << "\");\n";
    out << "}\n\n";
    return out.good();
}

boolean StrBrowserCode::ConstDecls(ostream& out) {
    out << "(const char*, ButtonState*, int, int, boolean);\n";
    return out.good();
}

boolean StrBrowserCode::ConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n    const char* name, ButtonState* bs,";
    out << "int rows, int cols, boolean u\n) : " << coreclass;
    out << "(name, bs, rows, cols, u) {}\n\n";
    return out.good();
}

boolean StrBrowserCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();
    ButtonStateVar* bsVar = GetIntComp()->GetButtonStateVar();

    if (!snamer->IsSubclass() && !_namelist->Search("strbrowser")) {
        _namelist->Append("strbrowser");
        out << "#include <InterViews/strbrowser.h> \n";
    }
    if (!bsVar->IsSubclass() && !_namelist->Search("button")) {
        _namelist->Append("button");
        out << "#include <InterViews/button.h> \n";
    }
    return out.good();
}

/*****************************************************************************/

StrBrowserGraphic::StrBrowserGraphic (
    const char* text, int width, int height, CanvasVar* c, Graphic* g
) : MessageGraphic(text, c, g) {
    _width = width;
    _height = height;
}

void StrBrowserGraphic::Natural (int& w, int& h) {
    FullGraphic gs;
    totalGS(gs);
    PSFont* font = gs.GetFont();
    w = _width - _width % font->Width("n");
    h = _height - _height % font->Height();
}

void StrBrowserGraphic::GetRowsCols (int& rows, int& cols) {
    FullGraphic gs;
    totalGS(gs);
    PSFont* font = gs.GetFont();

    rows = _height / font->Height();
    cols = _width / font->Width("n");

    rows = (rows == 0) ? 1 : rows;
    cols = (cols == 0) ? 1 : cols;
}

void StrBrowserGraphic::SetRowsCols(int rows, int cols) {
    _width = cols;
    _height = rows;
}

Graphic* StrBrowserGraphic::Copy () {
    return new StrBrowserGraphic(GetText(), _width, _height, nil, this);
}

const char* StrBrowserGraphic::GetClassName () { return GetText(); }
ClassId StrBrowserGraphic::GetClassId () { return STRBROWSER_GRAPHIC; }

void StrBrowserGraphic::Read (istream& in) {
    MessageGraphic::Read(in);
    in >> _width >> _height;
}

void StrBrowserGraphic::Write (ostream& out) {
    MessageGraphic::Write(out);
    out << _width << " " << _height << " ";
}

void StrBrowserGraphic::draw (Canvas* c, Graphic* gs) {
    Coord xmax, ymax;
    CanvasVar* cvar = GetCanvasVar();

    if (cvar == nil) {
        Natural(xmax, ymax);
    } else {
        xmax = cvar->xmax();
        ymax = cvar->ymax();
    }
    update(gs);

    static Brush* b;
    if (b == nil) {
        b = new Brush(0xbbbb, 1);
        Ref(b);
    }

    _p->SetBrush(b);
    _p->ClearRect(c, 0, 0, xmax, ymax);
    _p->Rect(c, 0, 0, xmax, ymax);
    _p->Line(c, 0, 0, xmax, ymax);
    _p->Line(c, 0, ymax, xmax, 0);

    const Font* font = _p->GetFont();
    const char* text = GetText();
    int w = font->Width(text);
    int h = font->Height();
    _p->Text(c, text, (xmax+1-w)/2, (ymax+1-h)/2);
}
