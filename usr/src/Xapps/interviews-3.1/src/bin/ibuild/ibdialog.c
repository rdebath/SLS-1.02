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
 *  DialogClass implementation
 */

#include "ibclasses.h"
#include "ibcmds.h"
#include "ibdialog.h"
#include "ibdialogs.h"
#include "ibvars.h"
#include "ibvarviews.h"

#include <Unidraw/ulist.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>
#include <Unidraw/catalog.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/

DialogClass::DialogClass (IBGraphic* gr) : MonoSceneClass(gr) {
    if (gr != nil) {
        GetClassNameVar()->SetName("Dialog");
        GetClassNameVar()->SetBaseClass("Dialog");
        GetClassNameVar()->GenNewName();
    }
    _bsVar = nil;
}

DialogClass::~DialogClass () {
    delete _bsVar;
}

void DialogClass::SetState (const char* name, StateVar* stateVar) {
    if (strcmp(name, "ButtonStateVar") == 0) {
        ButtonStateVar* bsVar = (ButtonStateVar*) stateVar;
        *_bsVar = *bsVar;

    } else {
        MonoSceneClass::SetState(name, stateVar);
    }
}

StateVar* DialogClass::GetState (const char* name) {
    StateVar* stateVar = nil;

    if (strcmp(name, "ButtonStateVar") == 0) {
        stateVar = _bsVar;
    } else {
        stateVar = MonoSceneClass::GetState(name);
    }
    return stateVar;
}

InteractorComp& DialogClass::operator = (InteractorComp& comp) {
    StateVar* state = comp.GetButtonStateVar();
    if (state != nil) {
	ButtonStateVar* bsvar = (ButtonStateVar*) state;
	if (_bsVar != nil) {
	    *_bsVar = *bsvar;
	}
    }
    return *this;
}

boolean DialogClass::IsRelatableTo (InteractorComp* comp) {
    boolean ok = false;
    if (comp->GetButtonStateVar() != nil) {
        ok = true;
    }
    return ok;
}

void DialogClass::Relate (InteractorComp* comp) {
    *this = *comp;
    int setting = _bsVar->GetSetting();
    _bsVar->SetSetting(++setting);
}

void DialogClass::Instantiate () {
    MonoSceneClass::Instantiate();
    if (_bsVar == nil) {
        _bsVar = new ButtonStateVar();
	_bsVar->HideSetting();
        _bsVar->GenNewName();
    }
}

void DialogClass::Interpret(Command* cmd) {
    if (cmd->IsA(GETCONFLICT_CMD)) {
        GetConflictCmd* gcmd = (GetConflictCmd*) cmd;
        const char* cname = gcmd->GetCName();
        MonoSceneClass::Interpret(cmd);
        UList* conflictlist = gcmd->GetConflict();
        ButtonSharedName* bsnamer = _bsVar->GetButtonSharedName();
        const char* buttonname = bsnamer->GetName();
        const char* funcname = bsnamer->GetFuncName();
        if (strcmp(buttonname, cname) == 0 || strcmp(funcname, cname) == 0) {
            conflictlist->Append(new UList(bsnamer));
        }

    } else {
        MonoSceneClass::Interpret(cmd);
    }
}

void DialogClass::Read (istream& in) {
    delete _bsVar;
    _bsVar = (ButtonStateVar*) unidraw->GetCatalog()->ReadStateVar(in);
    MonoSceneClass::Read(in);
}

void DialogClass::Write (ostream& out) {
    unidraw->GetCatalog()->WriteStateVar(_bsVar, out);
    MonoSceneClass::Write(out);
}

ClassId DialogClass::GetClassId () { return DIALOG_CLASS; }

boolean DialogClass::IsA (ClassId id) {
    return DIALOG_CLASS == id || MonoSceneClass::IsA(id);
}

/*****************************************************************************/

DialogClassView::DialogClassView (
    DialogClass* subj
) : MonoSceneClassView(subj) { }


DialogClass* DialogClassView::GetDialogClass () {
    return (DialogClass*) GetSubject();
}

ClassId DialogClassView::GetClassId () { return MONOSCENECLASS_VIEW; }

boolean DialogClassView::IsA (ClassId id) {
    return MONOSCENECLASS_VIEW == id || MonoSceneClassView::IsA(id);
}

InfoDialog* DialogClassView::GetInfoDialog () {
    IBEditor* ibed = (IBEditor*) GetViewer()->GetEditor();
    InfoDialog* info = MonoSceneClassView::GetInfoDialog();
    ButtonState* state = info->GetState();

    DialogClass* dclass = GetDialogClass();
    ButtonStateVar* bsVar = dclass->GetButtonStateVar();

    info->Include(new ButtonStateVarView(bsVar, state, dclass, ibed));
    return info;
}

/*****************************************************************************/

DialogClassCode::DialogClassCode (
    DialogClass* subj
) : MonoSceneClassCode(subj) { }

DialogClass* DialogClassCode::GetDialogClass() {
    return (DialogClass*) ComponentView::GetSubject();
}

ClassId DialogClassCode::GetClassId () { return DIALOGCLASS_CODE; }

boolean DialogClassCode::IsA(ClassId id) {
    return DIALOGCLASS_CODE == id || MonoSceneClassCode::IsA(id);
}

boolean DialogClassCode::Definition (ostream& out) {
    char coreclass[CHARBUFSIZE];
    boolean ok = true;

    DialogClass* dclass = GetDialogClass();
    SubclassNameVar* snamer = dclass->GetClassNameVar();
    MemberNameVar* iname = dclass->GetMemberNameVar();
    const char* subclass = snamer->GetName();
    const char* baseclass = snamer->GetBaseClass();
    const char* instance = iname->GetName();

    ButtonStateVar* bsVar = dclass->GetButtonStateVar();
    SubclassNameVar* bsclass = bsVar->GetButtonSharedName()->GetSubclass();
    const char* bsname = bsclass->GetName();

    CodeView::GetCoreClassName(coreclass);

    MemberNameVar* kidname;
    CodeView* kidview = GetKidView();
    if (kidview != nil) {
        kidname = kidview->GetIntComp()->GetMemberNameVar();
    }

    if (
        _emitInstanceInits || _emitClassHeaders || _emitHeaders ||
        _emitProperty || _emitInstanceDecls
    ) {
        ok = ok && MonoSceneClassCode::Definition(out);
        if (_emitClassHeaders) {
            if (
                *_classname == '\0' && !_scope || 
                *_classname != '\0' && _scope || 
                strcmp(bsname, _classname) == 0
            ) {
                ok = ok && CheckToEmitClassHeader(out, bsname);
            }
        }
    } else if (
	_emitFunctionDecls || _emitFunctionInits || 
	_emitBSDecls || _emitBSInits
    ) {
        if (strcmp(subclass, _classname) == 0) {
            if (_emitBSInits) {
                _bsinitslist->Append(bsVar->GetName());
                ok = ok && kidview->Definition(out);
            } else {
                ok = ok && CodeView::Definition(out);
                ok = ok && kidview->Definition(out);
            }
	}

    } else if (_emitCorehHeader) {
        const char* fwname = GetFirewall();
        if (snamer->IsSubclass() && strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("dialog")) {
                _namelist->Append("dialog");
                out << "#include <InterViews/dialog.h>\n";
            }
        } else if (bsVar->IsSubclass() && strcmp(bsname, _classname) == 0) {
            if (!_namelist->Search("button")) {
                _namelist->Append("button");
                out << "#include <InterViews/button.h>\n";
            }
            if (fwname != nil && !_namelist->Search(fwname)) {
                _namelist->Append(fwname);
                out << "#include \"" << fwname << "-core.h\"\n";
            }

        } else {
            if (kidview != nil) {
                ok = ok && kidview->Definition(out); 
            }
        }
    } else if (_emitExpHeader) {
        ok = ok && MonoSceneClassCode::Definition(out);
        if (
            (strcmp(bsname, _classname) == 0 || 
                strcmp(subclass, _classname) == 0 && 
            bsVar->GetExport()) && bsVar->IsSubclass()
        ) {
            ok = ok && CheckToEmitHeader(out, bsname);

        } else if (!_namelist->Search("button")) {
            _namelist->Append("button");
            out << "#include <InterViews/button.h>\n";
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
        if (strcmp(subclass, _classname) == 0) {
            _scope = true;
            if (kidview != nil) {
                ok = ok && kidview->Definition(out); 
            }
            if (
                bsVar->GetExport() && !_bsdeclslist->Search(bsname)
            ) {
                _bsdeclslist->Append(bsname);
                out << "class " << bsname << ";\n";
            }
            _scope = false;
        } else {
            if (kidview != nil) {
                ok = ok && kidview->Definition(out); 
            }
        }
    } else if (
        _emitCreatorHeader || _emitCreatorSubj || _emitCreatorView
    ) {
        if (kidview != nil) {
            ok = ok && kidview->Definition(out); 
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
            ok = ok && MonoSceneClassCode::Definition(out);
        }
    }
    return ok && out.good();
}

boolean DialogClassCode::CoreConstDecls(ostream& out) { 
    boolean ok = true;
    CodeView* kidview = GetKidView();

    out << "(const char*);\n";
    ok = ok && EmitFunctionDecls(this, out);
    out << "protected:\n";
    out << "    Interactor* Interior();\n";
    out << "protected:\n";
    
    _emitExport = true;
    ok = ok && EmitBSDecls(this, out);
    ok = ok && EmitInstanceDecls(kidview, out);
    _emitExport = false;
    
    return out.good();
}

boolean DialogClassCode::CoreConstInits(ostream& out) {
    boolean ok = true;
    DialogClass* dclass = GetDialogClass();
    ButtonStateVar* bsVar = dclass->GetButtonStateVar();
    SubclassNameVar* snamer = dclass->GetClassNameVar();
    const char* baseclass = snamer->GetBaseClass();
    const char* subclass = snamer->GetName();
    
    char ButtonClass[CHARBUFSIZE];
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);
    
    strcpy(ButtonClass, coreclass);
    strcat(ButtonClass, "_Button");
    const char* proc = bsVar->GetFuncName();
    
    boolean export = bsVar->GetExport();
    CodeView* kidview = GetKidView();
    MemberNameVar* kidname;
    if (kidview != nil) {
        kidname = kidview->GetIntComp()->GetMemberNameVar();
    }

    out << " (const char* name) : " << baseclass << "(\n";
    out << "    name, nil, nil\n) {\n";
    out << "    SetClassName(\"" << subclass << "\");\n";
    out << "    if (input != nil) {\n";
    out << "        input->Unreference();\n";
    out << "    }\n";
    out << "    input = allEvents;\n";
    out << "    input->Reference();\n";

    out << "    state = new " << ButtonClass << "(";
    out << bsVar->GetInitial() << ", this";
    if (proc != nil && *proc != '\0') {
        out << ", " << "&" << coreclass << "::" << proc << ");\n";
    } else {
        out << ", nil);\n";
    }
    out << "    Insert(Interior());\n}\n\n";

    out << "Interactor*" << coreclass;
    out << "::Interior() {\n";
    if (export) {
        out << "    " << bsVar->GetName() << " = state;\n";
    } else {
        out << "    ButtonState* ";
        out << bsVar->GetName() << " = state;\n";
    }
    ok = ok && EmitBSInits(this, out);
    ok = ok && EmitInstanceInits(kidview, out);
    out << "    return " << kidname->GetName() << ";\n};\n\n";
    ok = ok && EmitFunctionInits(kidview, out);

    return out.good();
}

boolean DialogClassCode::ConstDecls(ostream& out) {
    boolean ok = true;
    out << "(const char*);\n\n";
    ok = ok && EmitFunctionDecls(this, out);
    
    return out.good();
}

boolean DialogClassCode::ConstInits(ostream& out) {
    boolean ok = true;
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(const char* name) : " << coreclass << "(name) {}\n\n";
    ok = ok && EmitFunctionInits(this, out);
            
    return out.good();
}

boolean DialogClassCode::EmitIncludeHeaders(ostream& out) {
    boolean ok = MonoSceneClassCode::EmitIncludeHeaders(out);
    if (!_namelist->Search("button")) {
        _namelist->Append("button");
        out << "#include <InterViews/button.h> \n";
    }
    return ok && out.good();
}
