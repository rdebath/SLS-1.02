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
 * Button component definitions.
 */

#include "ibbutton.h"
#include "ibclasses.h"
#include "ibcmds.h"
#include "ibdialogs.h"
#include "ibed.h"
#include "ibinteractor.h"
#include "ibvars.h"
#include "ibvarviews.h"

#include <Unidraw/catalog.h>
#include <Unidraw/iterator.h>
#include <Unidraw/manips.h>
#include <Unidraw/ulist.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>
#include <Unidraw/Commands/edit.h>
#include <Unidraw/Tools/tool.h>

#include <InterViews/bitmap.h>
#include <InterViews/painter.h>
#include <InterViews/shape.h>
#include <InterViews/Bitmaps/radioMask.bm>
#include <InterViews/Bitmaps/radio.bm>

#include <stdio.h>
#include <stream.h>
#include <string.h>
#include <stdlib.h>

/*****************************************************************************/

static const int pad = 3;
static const int sep = 3;

/*****************************************************************************/

Bitmap* RadioButtonGraphic::radioMask;
Bitmap* RadioButtonGraphic::radioPlain;

/*****************************************************************************/
ButtonComp::ButtonComp (MessageGraphic* g) : MessageComp(g) { 
    if (g != nil) {
        GetClassNameVar()->SetName(g->GetClassName());
        GetClassNameVar()->SetBaseClass(g->GetClassName());
        IBShape* ibshape = GetShapeVar()->GetShape();
        ibshape->hstr = ibshape->vstr = false;
    }
    _bsVar = nil;
}

ButtonComp::~ButtonComp () {
    delete _bsVar;
}

ClassId ButtonComp::GetClassId () { return BUTTON_COMP; }

boolean ButtonComp::IsA (ClassId id) {
    return BUTTON_COMP == id || MessageComp::IsA(id);
}

StateVar* ButtonComp::GetState (const char* name) {
    StateVar* stateVar = nil;

    if (strcmp(name, "ButtonStateVar") == 0) {
        stateVar = _bsVar;
    } else if (strcmp(name, "AlignStateVar") != 0) {
        stateVar = MessageComp::GetState(name);
    }

    return stateVar;
}

void ButtonComp::SetState (const char* name, StateVar* stateVar) {
    if (strcmp(name, "ButtonStateVar") == 0) {
        ButtonStateVar* bsVar = (ButtonStateVar*) stateVar;
        *_bsVar = *bsVar;

    } else {
        MessageComp::SetState(name, stateVar);
    }
}

InteractorComp& ButtonComp::operator = (InteractorComp& comp) {
    InteractorComp::operator = (comp);
    StateVar* state = comp.GetButtonStateVar();
    if (state != nil) {
        ButtonStateVar* bsVar = (ButtonStateVar*) state;
        if (_bsVar != nil) {
            *_bsVar = *bsVar;
        }
    }
    return *this;
}

boolean ButtonComp::IsRelatableTo (InteractorComp* comp) {
    boolean ok = false;
    if (comp->GetButtonStateVar() != nil) {
        ok = true;
    }
    return ok;
}

void ButtonComp::Relate (InteractorComp* comp) {
    *this = *comp;
    int setting = _bsVar->GetSetting();
    _bsVar->SetSetting(++setting);
}

void ButtonComp::Instantiate () {
    InteractorComp::Instantiate();
    if (_bsVar == nil) {
        _bsVar = new ButtonStateVar();
        
        MessageGraphic* mgr = GetMessageGraphic();
	if (mgr != nil && mgr->GetClassId() == CHECKBOX_GRAPHIC) {
	    _bsVar->SetSetting(1);
	}
        _bsVar->GenNewName();
    }
}

void ButtonComp::Reconfig () {
    MessageComp::Reconfig();
    Shape* shape = GetShapeVar()->GetShape();
    shape->Rigid();
    GetShapeVar()->Notify();
}

void ButtonComp::Interpret(Command* cmd) {
    if (cmd->IsA(GETCONFLICT_CMD)) {
        GetConflictCmd* gcmd = (GetConflictCmd*) cmd;
        const char* cname = gcmd->GetCName();
        MessageComp::Interpret(cmd);
        UList* conflictlist = gcmd->GetConflict();
        ButtonSharedName* bsnamer = _bsVar->GetButtonSharedName();
        const char* buttonname = bsnamer->GetName();
        const char* funcname = bsnamer->GetFuncName();
        if (strcmp(buttonname, cname) == 0 || strcmp(funcname, cname) == 0) {
            conflictlist->Append(new UList(bsnamer));
        }
    } else if (cmd->IsA(GETNAMEVARS_CMD)) {
        MessageComp::Interpret(cmd);
        GetNameVarsCmd* gcmd = (GetNameVarsCmd*) cmd;
        gcmd->AppendExtras(_bsVar->GetButtonSharedName()->GetSubclass());
        
    } else if (!cmd->IsA(ALIGN_CMD)) {
        MessageComp::Interpret(cmd);
    }
}

void ButtonComp::Uninterpret(Command* cmd) {
    if (!cmd->IsA(ALIGN_CMD)) {
        MessageComp::Uninterpret(cmd);
    }
}

void ButtonComp::Read (istream& in) {
    MessageComp::Read(in);
    IBShape* ibshape = GetShapeVar()->GetShape();
    ibshape->hstr = ibshape->vstr = false;
    delete _bsVar;
    _bsVar = (ButtonStateVar*) unidraw->GetCatalog()->ReadStateVar(in);
}

void ButtonComp::Write (ostream& out) {
    MessageComp::Write(out);
    unidraw->GetCatalog()->WriteStateVar(_bsVar, out);
}

/*****************************************************************************/

ButtonView::ButtonView (ButtonComp* subj) : MessageView(subj) { }
ButtonComp* ButtonView::GetButtonComp () { return (ButtonComp*) GetSubject(); }
ClassId ButtonView::GetClassId () { return BUTTON_VIEW; }

boolean ButtonView::IsA (ClassId id) {
    return BUTTON_VIEW == id || MessageView::IsA(id);
}

Manipulator* ButtonView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Manipulator* m = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        m = InteractorView::CreateManipulator(v, e, rel, tool);
    } else {
        m = MessageView::CreateManipulator(v, e, rel, tool);
    }
    return m;
}

Command* ButtonView::InterpretManipulator (Manipulator* m) {
    Command* cmd = nil;
    Tool* tool = m->GetTool();

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        cmd = InteractorView::InterpretManipulator(m);
    } else {
        cmd = MessageView::InterpretManipulator(m);
    }

    return cmd;
}

InfoDialog* ButtonView::GetInfoDialog () {
    IBEditor* ibed = (IBEditor*) GetViewer()->GetEditor();
    InfoDialog* info = MessageView::GetInfoDialog();
    ButtonState* state = info->GetState();
    ButtonComp* bcomp = GetButtonComp();
    ButtonStateVar* bsVar = GetButtonComp()->GetButtonStateVar();

    info->Include(new ButtonStateVarView(bsVar, state, bcomp, ibed));
    return info;
}

/*****************************************************************************/

ClassId ButtonCode::GetClassId () { return BUTTON_CODE; }
boolean ButtonCode::IsA(ClassId id){return BUTTON_CODE==id||CodeView::IsA(id);}
ButtonCode::ButtonCode (ButtonComp* subj) : CodeView(subj) { }
ButtonComp* ButtonCode::GetButtonComp () { return (ButtonComp*) GetSubject(); }

boolean ButtonCode::Definition (ostream& out) {
    boolean ok = true;
    InteractorComp* icomp = GetIntComp();
    MemberNameVar* mnamer = icomp->GetMemberNameVar();
    SubclassNameVar* snamer = icomp->GetClassNameVar();
    ButtonStateVar* bsVar = icomp->GetButtonStateVar();
    SubclassNameVar* bsclass = bsVar->GetButtonSharedName()->GetSubclass();
    const char* bsname = bsclass->GetName();
    if (
	_emitProperty || _emitBSDecls ||
	_emitBSInits || _emitInstanceDecls || _emitHeaders ||
	_emitFunctionDecls || _emitFunctionInits
    ) {
        return CodeView::Definition(out);

    } else if (_emitClassHeaders) {
        ok = ok && CodeView::Definition(out);
        if (bsclass->IsSubclass()) {
            if (
                *_classname == '\0' && !_scope || 
                *_classname != '\0' && _scope || 
                strcmp(bsname, _classname) == 0
            ) {
                ok = ok && CheckToEmitClassHeader(out, bsname);
            }
        }
    } else if (_emitForward) {
        char Func[CHARBUFSIZE];
        char coreclass[CHARBUFSIZE];
        
        const char* fwname = GetFirewall();
        if (fwname != nil) {
            sprintf(coreclass, "%s_core", fwname);
            strcpy(Func, coreclass);
            strcat(Func, "_Func");
        }
        
        if (strcmp(bsname, _classname) == 0) {
            if (fwname != nil && !_namelist->Search(fwname)) {
                _namelist->Append(fwname);
                out << "\n#ifndef " << fwname << "_core_func\n";
                out << "#define " << fwname << "_core_func\n";
                out << "typedef void (" << coreclass << "::*";
                out << Func << ")();\n";
                out << "#endif\n\n";
            }
        }
        if (_scope) {
            ok = ok && CodeView::Definition(out);
            if (
                bsVar->GetExport() && !_bsdeclslist->Search(bsname)
            ) {
                _bsdeclslist->Append(bsname);
                out << "class " << bsname << ";\n";
            }
        }
    } else if (_emitExpHeader) {
        if (!snamer->IsSubclass() && !bsVar->IsSubclass()) {
            if (_scope) {
                if (
                    (mnamer->GetExport() || bsVar->GetExport()) &&
                    !_namelist->Search("button")
                ) {
                    _namelist->Append("button");
                    out << "#include <InterViews/button.h>\n";
                }
            }
        } else {
            if (
                (strcmp(bsname, _classname) == 0 || 
                _scope && bsVar->GetExport()) && bsVar->IsSubclass()
            ) {
                ok = ok && CheckToEmitHeader(out, bsname);
            }
            ok = ok && CodeView::Definition(out);
        }
    } else if (_emitCorehHeader) {
        const char* subclass = snamer->GetName();
        const char* fwname = GetFirewall();
        if (snamer->IsSubclass() && strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("button")) {
                _namelist->Append("button");
                out << "#include <InterViews/button.h>\n";
            }
        }
        if (bsVar->IsSubclass() && strcmp(bsname, _classname) == 0) {
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
        const char* mname = mnamer->GetName();

        if (!_instancelist->Find((void*) mname)) {
            _instancelist->Append(new UList((void*)mname));

            ButtonComp* button = GetButtonComp();
	
            BeginInstantiate(out);
            MessageGraphic* graphic = button->GetMessageGraphic();
            const char* text = graphic->GetText();
            ButtonStateVar* bsVar = button->GetButtonStateVar();
            int setting = bsVar->GetSetting();
            const char* classname = icomp->GetClassNameVar()->GetBaseClass();

            out << "(";
            InstanceName(out);
            out << "\"" << text << "\", " << bsVar->GetName();
	    out << ", " << setting;

            if (strcmp(classname, "CheckBox") == 0) {
	        out << ", " << bsVar->GetInitial();
	    }

            out << ")";
            EndInstantiate(out);
        }

    } else if (
        _emitCoreDecls || _emitCoreInits || _emitClassDecls || _emitClassInits
    ) {
        if (
            strcmp(bsname, _classname) == 0 &&
            !_globallist->Search(_classname)
        ) {
	    _globallist->Append(_classname);
            if (_emitCoreDecls) {
                ok = ok && BSCoreConstDecls(out);

            } else if (_emitCoreInits) {
                ok = ok && BSCoreConstInits(out);

            } else if (_emitClassDecls) {
                ok = ok && BSConstDecls(out);

            } else {
                ok = ok && BSConstInits(out);
            }
        } else {
            ok = ok && CodeView::Definition(out);
        }
    } else if (_emitMain) {
	ok = ok && CodeView::Definition(out);
        
    }
    return out.good() && ok;
}

boolean ButtonCode::CoreConstDecls(ostream& out) { 
    const char* classname =GetIntComp()->GetClassNameVar()->GetBaseClass();

    out << "(const char*, const char*, ButtonState*, int";
    if (strcmp(classname, "CheckBox") == 0) {
        out << ", int";
    }
    out << ");\n";

    return out.good();
}

boolean ButtonCode::CoreConstInits(ostream& out) {
    const char* classname =GetIntComp()->GetClassNameVar()->GetBaseClass();
    const char* subclass = GetIntComp()->GetClassNameVar()->GetName();

    out <<"(\n    const char* name, const char* text, ButtonState* bs, int s";
    if (strcmp(classname, "CheckBox") == 0) {
        out << ", int i\n) : " << classname << "(name, text, bs, s, i) {\n";
        out << "    SetClassName(\"" << subclass << "\");\n";
        out << "}\n\n";

    } else {
        out << "\n) : " << classname << "(name, text, bs, s) {\n";
        out << "    SetClassName(\"" << subclass << "\");\n";
        out << "}\n\n";
    }

    return out.good();
}

boolean ButtonCode::ConstDecls(ostream& out) {
    const char* classname =GetIntComp()->GetClassNameVar()->GetBaseClass();

    out << "(const char*, const char*, ButtonState*, int";
    if (strcmp(classname, "CheckBox") == 0) {
        out << ", int";
    }
    out << ");\n";

    return out.good();
}

boolean ButtonCode::ConstInits(ostream& out) {
    const char* classname =GetIntComp()->GetClassNameVar()->GetBaseClass();
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out <<"(\n    const char* name, const char* text, ButtonState* bs, int s";
    if (strcmp(classname, "CheckBox") == 0) {
        out << ", int i\n) : " << coreclass << "(name, text, bs, s, i) {}\n\n";

    } else {
        out << "\n) : " << coreclass << "(name, text, bs, s) {}\n\n";
    }

    return out.good();
}

boolean ButtonCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();

    if (!snamer->IsSubclass() && !_namelist->Search("button")) {
        _namelist->Append("button");
        out << "#include <InterViews/button.h> \n";
    }
    return out.good();
}

/*****************************************************************************/

PushButtonGraphic::PushButtonGraphic (
    const char* text, CanvasVar* c, Graphic* g
) : MessageGraphic(text, c, g, Center, 3) { }

void PushButtonGraphic::Natural (int& w, int& h) {
    FullGraphic gs;
    totalGS(gs);
    PSFont* font = gs.GetFont();
    w = font->Width(GetText()) + font->Width("    ");
    h = font->Height() + 2*pad;
}

Graphic* PushButtonGraphic::Copy () {
    return new PushButtonGraphic(GetText(), nil, this);
}

const char* PushButtonGraphic::GetClassName () { return "PushButton"; }
ClassId PushButtonGraphic::GetClassId () { return PUSHBUTTON_GRAPHIC; }

void PushButtonGraphic::GetTextPosition (Coord& l, Coord& b, const Font* f) {
    Coord xmax, ymax;

    if (GetCanvasVar() == nil) {
        Natural(xmax, ymax);
    } else {
        xmax = GetCanvasVar()->xmax();
    }
    l = (xmax + 1 - f->Width(GetText()))/2;
    b = pad;
}

void PushButtonGraphic::draw (Canvas* c, Graphic* gs) {
    Coord xmax, ymax;

    if (GetCanvasVar() == nil) {
        Natural(xmax, ymax);
    } else {
        xmax = GetCanvasVar()->xmax();
        ymax = GetCanvasVar()->ymax();
    }
    update(gs);
    _p->ClearRect(c, 0, 0, xmax, ymax);

    int r = min(10*pixels, min(xmax+1, ymax+1)/6);
    Coord x[16], y[16];
    x[0] = 0; y[0] = r;
    x[1] = 0; y[1] = r + r;
    x[2] = 0; y[2] = ymax - r - r;
    x[3] = 0; y[3] = ymax - r;
    x[4] = r; y[4] = ymax;
    x[5] = r + r; y[5] = ymax;
    x[6] = xmax - r - r; y[6] = ymax;
    x[7] = xmax - r; y[7] = ymax;
    x[8] = xmax; y[8] = ymax - r;
    x[9] = xmax; y[9] = ymax - r - r;
    x[10] = xmax; y[10] = r + r;
    x[11] = xmax; y[11] = r;
    x[12] = xmax - r; y[12] = 0;
    x[13] = xmax - r - r; y[13] = 0;
    x[14] = r + r; y[14] = 0;
    x[15] = r; y[15] = 0;
    Coord tx, ty;
    GetTextPosition(tx, ty, _p->GetFont());

    _p->SetColors(gs->GetBgColor(), gs->GetFgColor());
    _p->FillRect(c, 0, 0, xmax, ymax);
    _p->SetColors(gs->GetFgColor(), gs->GetBgColor());
    _p->ClosedBSpline(c, x, y, 16);
    _p->Text(c, GetText(), tx, ty);
}

/*****************************************************************************/

RadioButtonGraphic::RadioButtonGraphic (
    const char* text, CanvasVar* c, Graphic* g
) : MessageGraphic(text, c, g) { 
    if (radioMask == nil) {
        radioMask = new Bitmap(
            radio_mask_bits, radio_mask_width, radio_mask_height
        );
        radioMask->Reference();
        radioPlain = new Bitmap(
            radio_plain_bits, radio_plain_width, radio_plain_height
        );
        radioPlain->Reference();
    }
}

void RadioButtonGraphic::Natural (int& w, int& h) {
    FullGraphic gs;
    totalGS(gs);
    PSFont* font = gs.GetFont();
    h = font->Height();
    w = font->Width(GetText()) + h + sep;
}

void RadioButtonGraphic::GetTextPosition (Coord& l, Coord& b, const Font* f) {
    int h = f->Height();
    int r = radio_plain_width;
    l = r + sep;
    Coord xmax, ymax;
    
    if (GetCanvasVar() == nil) {
        Natural(xmax, ymax);
    } else {
        ymax = GetCanvasVar()->ymax();
    }

    b = (ymax + 1 - h) / 2;
}

Graphic* RadioButtonGraphic::Copy () {
    return new RadioButtonGraphic(GetText(), nil, this);
}

const char* RadioButtonGraphic::GetClassName () { return "RadioButton"; }
ClassId RadioButtonGraphic::GetClassId () { return RADIOBUTTON_GRAPHIC; }

void RadioButtonGraphic::draw (Canvas* c, Graphic* gs) {
    Coord xmax, ymax;

    if (GetCanvasVar() == nil) {
        Natural(xmax, ymax);
    } else {
        xmax = GetCanvasVar()->xmax();
        ymax = GetCanvasVar()->ymax();
    }
    update(gs);
    _p->ClearRect(c, 0, 0, xmax, ymax);

    Coord tx, ty;
    GetTextPosition(tx, ty, _p->GetFont());
    _p->Text(c, GetText(), tx, ty);

    Coord x = 0;
    Coord y = (ymax+1 - radio_plain_height)/2;

    _p->Stencil(c, x, y, radioPlain, radioMask);
}

/*****************************************************************************/

CheckBoxGraphic::CheckBoxGraphic (
    const char* text, CanvasVar* c, Graphic* g
) : MessageGraphic(text, c, g) { }

void CheckBoxGraphic::Natural (int& w, int& h) {
    FullGraphic gs;
    totalGS(gs);
    PSFont* font = gs.GetFont();
    h = font->Height();
    w = font->Width(GetText()) + h + sep;
}

Graphic* CheckBoxGraphic::Copy () {
    return new CheckBoxGraphic(GetText(), nil, this);
}

const char* CheckBoxGraphic::GetClassName () { return "CheckBox"; }
ClassId CheckBoxGraphic::GetClassId () { return CHECKBOX_GRAPHIC; }
inline int HalfRadioButtonSize (int h) { return round(.4*h); }
inline int HalfCheckBoxSize (int h) { return round(.4*h); }

void CheckBoxGraphic::GetTextPosition (Coord& l, Coord& b, const Font* f) {
    int h = f->Height();
    int t = HalfCheckBoxSize(h);
    l = 2*t + sep;
    Coord xmax, ymax;

    if (GetCanvasVar() == nil) {
        Natural(xmax, ymax);
    } else {
        ymax = GetCanvasVar()->ymax();
    }
    b = (ymax + 1 - h) / 2;
}

void CheckBoxGraphic::draw (Canvas* c, Graphic* gs) {
    Coord xmax, ymax;

    if (GetCanvasVar() == nil) {
        Natural(xmax, ymax);
    } else {
        xmax = GetCanvasVar()->xmax();
        ymax = GetCanvasVar()->ymax();
    }
    update(gs);
    _p->ClearRect(c, 0, 0, xmax, ymax);

    int h = _p->GetFont()->Height();
    int t = HalfCheckBoxSize(h);
    Coord tx, ty;
    GetTextPosition(tx, ty, _p->GetFont());
    _p->Text(c, GetText(), tx, ty);

    Coord cx = t;
    Coord cy = (ymax + 1)/2;
    Coord left = cx - t;
    Coord right = cx + t;
    Coord bottom = cy - t;
    Coord top = cy + t;
    _p->Rect(c, left, bottom, right, top);
}

