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
 * Implementation of EditorComp
 */

#include "ibclasses.h"
#include "ibbutton.h"
#include "ibcmds.h"
#include "ibdialogs.h"
#include "ibed.h"
#include "ibgraphic.h"
#include "ibgrcomp.h"
#include "ibeditor.h"
#include "ibpanelctrl.h"
#include "ibvars.h"
#include "ibvarviews.h"

#include <Unidraw/catalog.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/iterator.h>
#include <Unidraw/manips.h>
#include <Unidraw/selection.h>
#include <Unidraw/viewer.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/ulist.h>

#include <stream.h>
#include <string.h>

/*****************************************************************************/

EditorComp::EditorComp (IBGraphic* gr) : MonoSceneClass(gr) { 
    _viewerVar = nil;
    _curCtrlVar = nil;
    _keymap = nil;
    _selection = nil;
    _igrcomps = new IGraphicComps;
    _igrcomps->Instantiate();

    if (gr != nil) {
        SubclassNameVar* subclass = GetClassNameVar();
        subclass->SetName("Editor");
        subclass->SetBaseClass("Editor");
        subclass->SetMachGen(true);
        subclass->GenNewName();
        subclass->SetAbstract(true);
    }
}

EditorComp::~EditorComp () {
    delete _viewerVar;
    delete _curCtrlVar;
    delete _keymap;
    delete _selection;
    delete _igrcomps;
}

ClassId EditorComp::GetClassId () { return EDITOR_COMP; }

boolean EditorComp::IsA (ClassId id) {
    return EDITOR_COMP == id || MonoSceneClass::IsA(id);
}

void EditorComp::Instantiate () {
    MonoSceneClass::Instantiate();
    if (_viewerVar == nil) {
        _viewerVar = new MemberNameVar("", false, false);
    }
    if (_keymap == nil) {
        _keymap = new MemberNameVar("keymap");
        _keymap->GenNewName();
        MemberSharedName* keynamer = _keymap->GetMemberSharedName();
        SubclassNameVar* kvar = keynamer->GetSubclass();
        kvar->SetBaseClass("KeyMap");
        kvar->SetName("KeyMap");
    }
    if (_selection == nil) {
        _selection = new MemberNameVar("selection");
        _selection->GenNewName();
        MemberSharedName* snamer = _selection->GetMemberSharedName();
        SubclassNameVar* svar = snamer->GetSubclass();
        svar->SetBaseClass("Selection");
        svar->SetName("Selection");
    }
    if (_curCtrlVar == nil) {
        _curCtrlVar = new ButtonStateVar("curCtrl");
        _curCtrlVar->GenNewName();
        _curCtrlVar->HideSetting();
        ButtonSharedName* bsnamer = _curCtrlVar->GetButtonSharedName();
        SubclassNameVar* svar = bsnamer->GetSubclass();
        svar->SetBaseClass("ControlState");
        svar->SetName("ControlState");
    }
}

void EditorComp::Interpret (Command* cmd) {
    if (cmd->IsA(GETCONFLICT_CMD)) {
        GetConflictCmd* gcmd = (GetConflictCmd*) cmd;
        if (gcmd->IsGlobal()) {
            GetExtraConflict(gcmd);
            MonoSceneComp::Interpret(cmd);
        } else if (!gcmd->GetScope()) {
            gcmd->SetScope(true);
            GetExtraConflict(gcmd);
            MonoSceneComp::Interpret(cmd);
        }
        
    } else if (cmd->IsA(GETNAMEVARS_CMD)) {
        GetNameVarsCmd* gcmd = (GetNameVarsCmd*) cmd;

        MonoSceneClass::Interpret(gcmd);
        gcmd->AppendExtras(_viewerVar);
        gcmd->AppendExtras(_viewerVar->GetSubclass());
        gcmd->AppendExtras(_keymap);
        gcmd->AppendExtras(_keymap->GetSubclass());
        gcmd->AppendExtras(_selection);
        gcmd->AppendExtras(_selection->GetSubclass());
        gcmd->AppendExtras(_curCtrlVar);
        gcmd->AppendExtras(_curCtrlVar->GetButtonSharedName());
        gcmd->AppendExtras(_curCtrlVar->GetButtonSharedName()->GetSubclass());

        gcmd->AppendExtras(_igrcomps->GetCClassNameVar());
        gcmd->AppendExtras(_igrcomps->GetGClassNameVar());
        gcmd->AppendExtras(_igrcomps->GetVClassNameVar());
        gcmd->AppendExtras(_igrcomps->GetMemberNameVar());

    } else if (cmd->IsA(IDMAP_CMD)) {
        _igrcomps->Interpret(cmd);
        MonoSceneClass::Interpret(cmd);
        
    } else {
        MonoSceneClass::Interpret(cmd);
    }
}

void EditorComp::GetExtraConflict(GetConflictCmd* gcmd) {
    const char* cname = gcmd->GetCName();
    UList* conflictlist = gcmd->GetConflict();

    const char* curCtrl = _curCtrlVar->GetName();
    const char* key = _keymap->GetName();
    const char* select = _selection->GetName();
    
    const char* ctrl_sub = _curCtrlVar->GetSubclassName();
    const char* key_sub = _keymap->GetSubclass()->GetName();
    const char* sel_sub = _selection->GetSubclass()->GetName();
    
    if (strcmp(curCtrl, cname) == 0) {
        conflictlist->Append(new UList(_curCtrlVar->GetButtonSharedName()));
    }
    if (strcmp(key, cname) == 0) {
        conflictlist->Append(new UList(_keymap->GetMemberSharedName()));
    }
    if (strcmp(select, cname) == 0) {
        conflictlist->Append(new UList(_selection->GetMemberSharedName()));
    }
    if (strcmp(ctrl_sub, cname) == 0) {
        conflictlist->Append(
            new UList(_curCtrlVar->GetButtonSharedName()->GetSubclass())
        );
    }
    if (strcmp(key_sub, cname) == 0) {
        conflictlist->Append(new UList(_keymap->GetSubclass()));
    }
    if (strcmp(sel_sub, cname) == 0) {
        conflictlist->Append(new UList(_selection->GetSubclass()));
    }
    _igrcomps->Interpret(gcmd);
}

void EditorComp::SetState(const char* name, StateVar* stateVar) { 
    if (
        strcmp(name, "ViewerVar") == 0 || strcmp(name, "RelatedVar") == 0
    ) {
        MemberNameVar* memberVar = (MemberNameVar*) stateVar;
        *_viewerVar = *memberVar;

    } else if (strcmp(name, "Keymap") == 0) {
        _keymap = (MemberNameVar*) stateVar;

    } else if (strcmp(name, "Selection") == 0) {
        _selection = (MemberNameVar*) stateVar;

    } else if (strcmp(name, "CurCtrlVar") == 0) {
        _curCtrlVar = (ButtonStateVar*) stateVar;

    } else {
        MonoSceneClass::SetState(name, stateVar);
    }
}

StateVar* EditorComp::GetState (const char* name) {
    StateVar* stateVar = nil;

    if (strcmp(name, "ViewerVar") == 0) {
        stateVar = _viewerVar;

    } else if (strcmp(name, "Keymap") == 0) {
        stateVar = _keymap;

    } else if (strcmp(name, "Selection") == 0) {
        stateVar = _selection;

    } else if (strcmp(name, "CurCtrlVar") == 0) {
        stateVar = _curCtrlVar;

    } else {
        stateVar = MonoSceneClass::GetState(name);
    }

    return stateVar;
}

InteractorComp& EditorComp::operator = (InteractorComp& comp) {
    if (comp.IsA(IBVIEWER_COMP)) {
        MemberNameVar* viewerVar = comp.GetMemberNameVar();

        *_viewerVar = *comp.GetMemberNameVar();
        comp.SetState("EditorVar", GetMemberNameVar());

    } else if (comp.IsA(EDITOR_COMP)) {
        EditorComp* edcomp = (EditorComp*) &comp;
        
        MemberNameVar* viewerVar = edcomp->GetViewerVar();
        ButtonStateVar* curCtrlVar = edcomp->GetButtonStateVar();
        MemberNameVar* keymap = edcomp->GetKeyMap();
        MemberNameVar* selection = edcomp->GetSelection();
        IGraphicComps* igrcomps = edcomp->GetIGraphicComps();
        
        
        *_viewerVar = *viewerVar;
        *_curCtrlVar = *curCtrlVar;
        *_keymap = *keymap;
        *_selection = *selection;
        *(IComp*)_igrcomps = *(IComp*)igrcomps;

        
    } else if (comp.IsA(PANELCONTROL_COMP)) {
        ButtonStateVar* curCtrlVar = comp.GetButtonStateVar();
        *_curCtrlVar = *curCtrlVar;
        comp.SetState("EditorVar", GetMemberNameVar());

    }
    return *this;
}

boolean EditorComp::IsRelatableTo (InteractorComp* comp) {
    boolean ok = false;
    if (
        comp->IsA(IBVIEWER_COMP) || comp->IsA(EDITOR_COMP) ||
        comp->IsA(PANELCONTROL_COMP) || comp->IsA(COMMANDCONTROL_COMP)
    ) {
        ok = true;
    }
    return ok;
}

void EditorComp::Read (istream& in) {
    Catalog* catalog = unidraw->GetCatalog();
    float version = catalog->FileVersion();

    delete _curCtrlVar;
    delete _keymap;
    delete _selection;
    delete _viewerVar;

    _viewerVar = (MemberNameVar*) catalog->ReadStateVar(in);
    _keymap = (MemberNameVar*) catalog->ReadStateVar(in);
    _selection = (MemberNameVar*) catalog->ReadStateVar(in);

    if (version > 1.05) {
        delete _igrcomps;
        _igrcomps = (IGraphicComps*) catalog->ReadComponent(in);
    } else {
        MemberNameVar* gmember = (MemberNameVar*) catalog->ReadStateVar(in);
        if (gmember != nil) {
            *_igrcomps->GetMemberNameVar() = *gmember;
        } 
        delete gmember;
    }
    _curCtrlVar = (ButtonStateVar*) catalog->ReadStateVar(in);
    if (version < 1.05) {
        MemberSharedName* keynamer = _keymap->GetMemberSharedName();
        SubclassNameVar* kvar = keynamer->GetSubclass();
        kvar->SetBaseClass("KeyMap");
        kvar->SetName("KeyMap");

        MemberSharedName* snamer = _selection->GetMemberSharedName();
        SubclassNameVar* selvar = snamer->GetSubclass();
        selvar->SetBaseClass("Selection");
        selvar->SetName("Selection");

        _curCtrlVar->HideSetting();
        ButtonSharedName* bsnamer = _curCtrlVar->GetButtonSharedName();
        SubclassNameVar* svar = bsnamer->GetSubclass();
        svar->SetBaseClass("ControlState");
        svar->SetName("ControlState");

    }
    MonoSceneClass::Read(in);
}

void EditorComp::Write (ostream& out) {
    Catalog* catalog = unidraw->GetCatalog();
    
    catalog->WriteStateVar(_viewerVar, out);
    catalog->WriteStateVar(_keymap, out);
    catalog->WriteStateVar(_selection, out);
    catalog->WriteComponent(_igrcomps, out);
    catalog->WriteStateVar(_curCtrlVar, out);
    MonoSceneClass::Write(out);
}

/*****************************************************************************/

EditorView::EditorView (EditorComp* subj) : MonoSceneClassView(subj) {}

EditorComp* EditorView::GetEditorComp () {
    return (EditorComp*) GetSubject();
}

ClassId EditorView::GetClassId () { return EDITOR_VIEW; }

boolean EditorView::IsA (ClassId id) {
    return EDITOR_VIEW == id || MonoSceneClassView::IsA(id);
}

InfoDialog* EditorView::GetInfoDialog () {
    IBEditor* ibed = (IBEditor*) GetViewer()->GetEditor();
    InfoDialog* info = MonoSceneClassView::GetInfoDialog();
    ButtonState* state = info->GetState();
    EditorComp* edcomp = GetEditorComp();

    MemberNameVar* viewerVar = edcomp->GetViewerVar();
    ButtonStateVar* curCtrlVar = edcomp->GetButtonStateVar();
    MemberNameVar* keymap = edcomp->GetKeyMap();
    MemberNameVar* selection = edcomp->GetSelection();
    IGraphicComps* igrcomps = edcomp->GetIGraphicComps();

    info->Include(new RelatedVarView(
        viewerVar, state, edcomp, "Viewer Name: ")
    );
    info->Include(new SMemberNameVarView(
        keymap, state, edcomp, ibed, "KeyMap")
    );
    info->Include(new SMemberNameVarView(
        selection, state, edcomp, ibed, "Selection")
    );
    info->Include(new SMemberNameVarView(
        igrcomps, state, edcomp, ibed, "GraphicComp")
    );
    info->Include(new CtrlStateVarView(curCtrlVar, state, edcomp, ibed));
    return info;
}

/*****************************************************************************/

EditorCode::EditorCode (EditorComp* subj) : MonoSceneClassCode(subj) {
    _unidraw = true;
}

EditorCode::~EditorCode () { delete _gcode; }

void EditorCode::Update () {
    IGraphicComps* igrcomps = GetEditorComp()->GetIGraphicComps();
    _gcode = (GroupCode*) igrcomps->Create(CODE_VIEW);
    igrcomps->Attach(_gcode);
    _gcode->Update();
    MonoSceneClassCode::Update();
    InteractorComp* subj = GetIntComp();
    Graphic* gr = subj->GetGraphic();
    gr->SetColors(nil, nil);
    gr->SetFont(nil);
}

EditorComp* EditorCode::GetEditorComp() {
    return (EditorComp*) GetSubject();
}

ClassId EditorCode::GetClassId () { return EDITOR_CODE; }

boolean EditorCode::IsA(ClassId id) {
    return EDITOR_CODE == id || MonoSceneClassCode::IsA(id);
}

boolean EditorCode::EmitGroupCode (ostream& out) {
    boolean ok = true;
    EditorComp* edcomp = GetEditorComp();
    SubclassNameVar* snamer = edcomp->GetClassNameVar();
    const char* subclass = snamer->GetName();

    boolean emitGraphicComp = _emitGraphicComp;
    _emitGraphicComp = true;
    if (strcmp(subclass, _classname) == 0) {
        _scope = true;
        ok = ok && _gcode->Definition(out);
        _scope = false;
    } else {
        ok = ok && _gcode->Definition(out);
    }
    _emitGraphicComp = emitGraphicComp;
    return ok;
}

boolean EditorCode::Definition (ostream& out) {
    char coreclass[CHARBUFSIZE];
    boolean ok = true;

    EditorComp* edcomp = GetEditorComp();
    SubclassNameVar* snamer = edcomp->GetClassNameVar();
    MemberNameVar* mnamer = edcomp->GetMemberNameVar();

    const char* subclass = snamer->GetName();
    const char* baseclass = snamer->GetBaseClass();
    const char* mname = mnamer->GetName();

    IComp* icomp = edcomp->GetIGraphicComps();
    MemberNameVar* igrcomps = icomp->GetMemberNameVar();
    MemberNameVar* selection = edcomp->GetSelection();
    MemberNameVar* keymap = edcomp->GetKeyMap();
    MemberNameVar* vnamer = edcomp->GetViewerVar();
    ButtonStateVar* ctrlvar = edcomp->GetButtonStateVar();

    SubclassNameVar* gr_sub = icomp->GetCClassNameVar();
    SubclassNameVar* sel_sub = selection->GetSubclass();
    SubclassNameVar* key_sub = keymap->GetSubclass();
    SubclassNameVar* viewer_sub = vnamer->GetSubclass();
    SubclassNameVar* ctrl_sub = ctrlvar->GetButtonSharedName()->GetSubclass();

    const char* gr_subn = gr_sub->GetName();
    const char* sel_subn = sel_sub->GetName();
    const char* key_subn = key_sub->GetName();
    const char* viewer_subn = viewer_sub->GetName();
    const char* ctrl_subn = ctrl_sub->GetName();

    const char* igrname = igrcomps->GetName();
    const char* selname = selection->GetName();
    const char* keyname = keymap->GetName();
    const char* vname = vnamer->GetName();
    const char* ctrlname = ctrlvar->GetName();

    GetCoreClassName(coreclass);
    CodeView* kidview = GetKidView();
    MemberNameVar* kidname = kidview->GetIntComp()->GetMemberNameVar();
    InteractorComp* dummy;

    if (*vname != '\0' && !Search(vnamer, dummy)) {
        if (_err_count < 10) {
            strcat(_errbuf, mname);
            strcat(
                _errbuf, "'s Viewer is not in the same hierachy.\n"
            );
            _err_count++;
        } 
        return false;
    }
    if (_emitForward) {
        if (strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search(gr_subn)) {
                _namelist->Append(gr_subn);
                out << "class " << gr_subn << ";\n";
            }
            if (!_namelist->Search(sel_subn)) {
                _namelist->Append(sel_subn);
                out << "class " << sel_subn << ";\n";
            }
            if (!_namelist->Search(key_subn)) {
                _namelist->Append(key_subn);
                out << "class " << key_subn << ";\n";
            }
            if (*vname != '\0' && !_namelist->Search(viewer_subn)) {
                _namelist->Append(viewer_subn);
                out << "class " << viewer_subn << ";\n";
            }
            if (!_namelist->Search(ctrl_subn)) {
                _namelist->Append(ctrl_subn);
                out << "class " << ctrl_subn << ";\n";
            }
        }
        ok = ok && EmitGroupCode(out);
        ok = ok && MonoSceneClassCode::Definition(out);
        
    } else if (_emitBSDecls || _emitBSInits) {
        if (strcmp(subclass, _classname) == 0) {
            ButtonStateVar* ctrlvar = edcomp->GetButtonStateVar();
            const char* ctrl = ctrlvar->GetName();
            if (!_bsdeclslist->Search(ctrl)) {
                _bsdeclslist->Append(ctrl);
            }
            if (!_bsinitslist->Search(ctrl)) {
                _bsinitslist->Append(ctrl);
            }
            ok = ok && Iterate(out);
        }
    } else if (_emitExpHeader) {
        ok = ok && EmitGroupCode(out);
        if (strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("grcomp")) {
                _namelist->Append("grcomp");
                out << "#include <Unidraw/Components/grcomp.h>\n";
            }
            if (selection->GetExport()) {
                if (sel_sub->IsSubclass()) {
                    ok = ok && CheckToEmitHeader(out, sel_subn);
                    
                } else if (!_namelist->Search("selection")) {
                    _namelist->Append("selection");
                    out << "#include <Unidraw/selection.h>\n";
                }
            } 
            if (keymap->GetExport()) {
                if (key_sub->IsSubclass()) {
                    ok = ok && CheckToEmitHeader(out, key_subn);
                    
                } else if (!_namelist->Search("keymap")) {
                    _namelist->Append("keymap");
                    out << "#include <Unidraw/keymap.h>\n";
                }
            } 
            if (ctrlvar->GetExport()) {
                if (ctrl_sub->IsSubclass()) {
                    ok = ok && CheckToEmitHeader(out, ctrl_subn);
                    
                } else if (!_namelist->Search("uctrl")) {
                    _namelist->Append("uctrl");
                    out << "#include <Unidraw/uctrl.h>\n";
                }
            } 
        } else if (strcmp(_classname, sel_subn) == 0) {
            ok = ok && CheckToEmitHeader(out, sel_subn);
            
        } else if (strcmp(_classname, key_subn) == 0) {
            ok = ok && CheckToEmitHeader(out, key_subn);
            
        } else if (strcmp(_classname, ctrl_subn) == 0) {
            ok = ok && CheckToEmitHeader(out, ctrl_subn);
        }
        ok = ok && CodeView::Definition(out);
        
    } else if (_emitCorehHeader) {
        ok = ok && EmitGroupCode(out);
        if (strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("editor")) {
                _namelist->Append("editor");
                out << "#include <Unidraw/editor.h>\n";
            }
        } else if (strcmp(sel_subn, _classname) == 0) {
            if (!_namelist->Search("selection")) {
                _namelist->Append("selection");
                out << "#include <Unidraw/selection.h>\n";
            }
        } else if (strcmp(key_subn, _classname) == 0) {
            if (!_namelist->Search("keymap")) {
                _namelist->Append("keymap");
                out << "#include <Unidraw/keymap.h>\n";
            }
        } else if (strcmp(ctrl_subn, _classname) == 0) {
            if (!_namelist->Search("control")) {
                _namelist->Append("control");
                out << "#include <InterViews/control.h>\n";
            }
        } else {
            ok = ok && kidview->Definition(out);
        }
    } else if (_emitHeaders) {
        ok = ok && EmitGroupCode(out);
        ok = ok && MonoSceneClassCode::Definition(out);
        
    } else if (_emitClassHeaders) {
        if (strcmp(subclass, _classname) == 0) {
            _scope = true;
            if (sel_sub->IsSubclass()) {
                ok = ok && CheckToEmitHeader(out, sel_subn);
            }
            if (key_sub->IsSubclass()) {
                ok = ok && CheckToEmitHeader(out, key_subn);
            }
            if (ctrl_sub->IsSubclass()) {
                ok = ok && CheckToEmitHeader(out, ctrl_subn);
            }
            _scope = false;
            
        } else if (strcmp(sel_subn, _classname) == 0) {
            ok = ok && CheckToEmitClassHeader(out, sel_subn);
            
        } else if (strcmp(key_subn, _classname) == 0) {
            ok = ok && CheckToEmitClassHeader(out, key_subn);
            
        } else if (strcmp(ctrl_subn, _classname) == 0) {
            ok = ok && CheckToEmitClassHeader(out, ctrl_subn);
        }
        ok = ok && MonoSceneClassCode::Definition(out);
        ok = ok && EmitGroupCode(out);
    } else if (
        _emitCreatorHeader || _emitCreatorSubj || _emitCreatorView
    ) {
        ok = ok && EmitGroupCode(out);
        ok = ok && Iterate(out);
        
    } else if (
        _emitCoreDecls || _emitCoreInits || _emitClassDecls || _emitClassInits
    ) {
        boolean emitInstanceInits = _emitInstanceInits;
        _emitInstanceInits = false;
        ok = ok && EmitGroupCode(out);
        _emitInstanceInits = emitInstanceInits;
        
        if (
            strcmp(ctrl_subn, _classname) == 0 &&
            !_globallist->Search(_classname)
        ) {
            _globallist->Append(_classname);
            if (_emitCoreDecls) {
                ok = ok && CSCoreConstDecls(out);
                
            } else if (_emitCoreInits) {
                ok = ok && CSCoreConstInits(out);
                
            } else if (_emitClassDecls) {
                ok = ok && CSConstDecls(out);
                
            } else {
                ok = ok && CSConstInits(out);
            }
        } else if (
            strcmp(key_subn, _classname) == 0 &&
            !_globallist->Search(_classname)
        ) {
            _globallist->Append(_classname);
            if (_emitCoreDecls) {
                ok = ok && KeyCoreConstDecls(out);
                
            } else if (_emitCoreInits) {
                ok = ok && KeyCoreConstInits(out);
                
            } else if (_emitClassDecls) {
                ok = ok && KeyConstDecls(out);
                
            } else {
                ok = ok && KeyConstInits(out);
            }
        } else if (
            strcmp(sel_subn, _classname) == 0 &&
            !_globallist->Search(_classname)
        ) {
            _globallist->Append(_classname);
            if (_emitCoreDecls) {
                ok = ok && SelCoreConstDecls(out);
                
            } else if (_emitCoreInits) {
                ok = ok && SelCoreConstInits(out);
                
            } else if (_emitClassDecls) {
                ok = ok && SelConstDecls(out);
                
            } else {
                ok = ok && SelConstInits(out);
            }
        } else {
            ok = ok && MonoSceneClassCode::Definition(out);
        }
    } else {
        ok = ok && MonoSceneClassCode::Definition(out);
    }
    
    return ok && out.good();
}

boolean EditorCode::CoreConstDecls(ostream& out) { 
    boolean ok = true;
    EditorComp* edcomp = GetEditorComp();
    
    IComp* icomp = edcomp->GetIGraphicComps();
    SubclassNameVar* gr_sub = icomp->GetCClassNameVar();
    MemberNameVar* igrcomps = icomp->GetMemberNameVar();
    MemberNameVar* selection = edcomp->GetSelection();
    MemberNameVar* keymap = edcomp->GetKeyMap();
    MemberNameVar* vnamer = edcomp->GetViewerVar();
    
    ButtonStateVar* ctrlvar = edcomp->GetButtonStateVar();
    
    const char* ctrl = ctrlvar->GetName();
    const char* comp = igrcomps->GetName();
    const char* select = selection->GetName();
    const char* key = keymap->GetName();
    const char* vname = vnamer->GetName();
    
    CodeView* kidview = GetKidView();
    
    out << "(const char*);\n";
    out << "    virtual Component* GetComponent();\n";
    if (*vname != '\0' && vnamer->GetExport()) {
        out << "    virtual Viewer* GetViewer(int = 0);\n";
    }
    out << "    virtual KeyMap* GetKeyMap();\n";
    out << "    virtual Tool* GetCurTool();\n";
    out << "    virtual Selection* GetSelection();\n\n";

    out << "    virtual void SetComponent(Component*);\n";
   if (*vname != '\0' && vnamer->GetExport()) {
        out << "    virtual void SetViewer(Viewer*, int = 0);\n";
    }
    out << "    virtual void SetKeyMap(KeyMap*);\n";
    out << "    virtual void SetSelection(Selection*);\n\n";

    ok = ok && EmitFunctionDecls(kidview, out);
    out << "protected:\n";
    out << "    Interactor* Interior();\n";
    out << "protected:\n";
    if (igrcomps->GetExport()) {
        out << "    " << gr_sub->GetName() << "* ";
        out << comp << ";\n";
    }
    if (selection->GetExport()) {
        out << "    " << selection->GetSubclass()->GetName();
        out << "* " << select << ";\n";
    }
    if (keymap->GetExport()) {
        out << "    " << keymap->GetSubclass()->GetName();
        out << "* " << key << ";\n";
    }
    if (ctrlvar->GetExport()) {
        out << "    " << ctrlvar->GetSubclassName() << "* " << ctrl << ";\n";
    }
    _emitExport = true;
    ok = ok && EmitBSDecls(this, out);
    ok = ok && EmitInstanceDecls(kidview, out);
    _emitExport = false;
    out << "private:\n";
    if (!igrcomps->GetExport()) {
        out << "    " << gr_sub->GetName() << "* ";
        out << comp << ";\n";
    }
    if (!selection->GetExport()) {
        out << "    " << selection->GetSubclass()->GetName();
        out << "* " << select << ";\n";
    }
    if (!keymap->GetExport()) {
        out << "    " << keymap->GetSubclass()->GetName();
        out << "* " << key << ";\n";
    }
    if (!ctrlvar->GetExport()) {
        out << "    " << ctrlvar->GetSubclassName() << "* " << ctrl << ";\n";
    }

    return ok && out.good();
}

boolean EditorCode::CoreConstInits(ostream& out) {
    boolean ok = true;
    EditorComp* edcomp = GetEditorComp();

    IComp* icomp = edcomp->GetIGraphicComps();
    SubclassNameVar* comp_sub = icomp->GetCClassNameVar();
    SubclassNameVar* gr_sub = icomp->GetGClassNameVar();
    MemberNameVar* igrcomps = icomp->GetMemberNameVar();

    SubclassNameVar* snamer = edcomp->GetClassNameVar();
    MemberNameVar* mnamer = edcomp->GetMemberNameVar();
    MemberNameVar* selection = edcomp->GetSelection();
    MemberNameVar* keymap = edcomp->GetKeyMap();
    ButtonStateVar* ctrlvar = edcomp->GetButtonStateVar();

    const char* subclass = snamer->GetName();
    const char* baseclass = snamer->GetBaseClass();
    const char* mname = mnamer->GetName();
    const char* ctrl = ctrlvar->GetName();
    const char* comp = igrcomps->GetName();
    const char* select = selection->GetName();
    const char* key = keymap->GetName();

    MemberNameVar* vnamer = edcomp->GetViewerVar();
    const char* vname = vnamer->GetName();

    CodeView* kidview = GetKidView();
    MemberNameVar* kidname = kidview->GetIntComp()->GetMemberNameVar();
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(const char* name) {\n";
    out << "    if (GetWindow() == nil) {\n";
    out << "        ManagedWindow* window = new ApplicationWindow(this);\n";
    out << "        SetWindow(window);\n";
    out << "        Style* s = new Style(Session::instance()->style());\n";
    out << "        s->attribute(\"name\", name);\n";
    out << "        s->attribute(\"iconName\", name);\n";
    out << "        s->alias(String(name));\n";
    out << "        window->style(s);\n";
    out << "    }\n";
    out << "    " << comp << " = new " << comp_sub->GetName() << "(new ";
    out << gr_sub->GetName() << ");\n";
    out << "    " << ctrl << " = new " << ctrlvar->GetSubclassName() << ";\n";
    out << "    " << select << " = new ";
    out << selection->GetSubclass()->GetName() << ";\n";
    out << "    " << key << " = new ";
    out << keymap->GetSubclass()->GetName() << ";\n";
    out << "    Insert(Interior());\n";
    out << "    " << key << "->Execute(CODE_SELECT);\n";
    out << "}\n\n";
    
    out << "Interactor* " << coreclass;
    out << "::Interior() {\n";
    ok = ok && EmitBSInits(this, out);
    ok = ok && EmitInstanceInits(kidview, out);
    out << "    return " << kidname->GetName() << ";\n};\n\n";

    out << "Component* " << subclass;
    out << "_core::GetComponent () { return " << comp << "; }\n";
    if (*vname != '\0' && vnamer->GetExport()) {
        out << "Viewer* " << subclass;
        out << "_core::GetViewer (int id) { return (id == 0) ? ";
        out << vname << ": nil; }\n";
    }
    out << "Selection* " << subclass;
    out << "_core::GetSelection () { return " << select << "; }\n";
    out << "KeyMap* " << subclass;
    out << "_core::GetKeyMap () { return " << key << "; }\n\n";
    out << "Tool* " << subclass <<  "_core::GetCurTool () {\n";
    out << "    Tool* tool = nil;\n";
    out << "    UControl* c = (UControl*) " << ctrl << "->Selection();\n";
    out << "    if (c != nil) {\n";
    out << "        tool = (Tool*) c->GetControlInfo()->GetOwner();\n";
    out << "    }\n";
    out << "    return tool;\n";
    out << "}\n\n";

    out << "void " << subclass << "_core::SetComponent(Component* comp) {\n";
    out << "    if (comp == nil || comp->IsA(GRAPHIC_COMPS)) {\n";
    out << "        " << comp << " = (" << comp_sub->GetName() << "*) comp;\n";
    out << "    }\n";
    out << "}\n\n";
    if (*vname != '\0' && vnamer->GetExport()) {
        out << "void " << subclass;
        out << "_core::SetViewer(Viewer* vname, int) {\n";
        out << "    " << vname << " = (" << vnamer->GetSubclass()->GetName();
        out << "*) vname;\n";
        out << "}\n\n";
    }
    out << "void " << subclass << "_core::SetKeyMap(KeyMap* key) {\n";
    out << "    " << key << " = (" << keymap->GetSubclass()->GetName();
    out << "*) key;\n";
    out << "}\n\n";
    out << "void " << subclass << "_core::SetSelection(Selection* select) {\n";
    out << "    " << select << " = (" << selection->GetSubclass()->GetName();
    out << "*) select;\n";
    out << "}\n\n\n";
    
    ok = ok && EmitFunctionInits(kidview, out);

    return ok && out.good();
}

boolean EditorCode::KeyCoreConstDecls(ostream& out) { 
    EditorComp* edcomp = GetEditorComp();

    MemberNameVar* keymap = edcomp->GetKeyMap();
    SubclassNameVar* key_sub = keymap->GetSubclass();

    const char* subclass = key_sub->GetName();
    const char* baseclass = key_sub->GetBaseClass();

    char Subclass[CHARBUFSIZE];
    strcpy(Subclass, subclass);
    strcat(Subclass, "_core");

    out << "class " << Subclass << " : public " << baseclass << " {\n";
    out << "public:\n";
    out << "    " << Subclass << "();\n";
    out << "};\n\n";

    return out.good();
}

boolean EditorCode::KeyCoreConstInits(ostream& out) {
    EditorComp* edcomp = GetEditorComp();

    MemberNameVar* keymap = edcomp->GetKeyMap();
    SubclassNameVar* key_sub = keymap->GetSubclass();

    const char* subclass = key_sub->GetName();
    const char* baseclass = key_sub->GetBaseClass();

    char Subclass[CHARBUFSIZE];
    strcpy(Subclass, subclass);
    strcat(Subclass, "_core");

    out << Subclass << "::" << Subclass << "(";
    out << ") : " << baseclass << "() {";
    out << "}\n\n";

    return out.good();
}

boolean EditorCode::KeyConstDecls(ostream& out) {
    EditorComp* edcomp = GetEditorComp();

    MemberNameVar* keymap = edcomp->GetKeyMap();
    SubclassNameVar* key_sub = keymap->GetSubclass();

    const char* subclass = key_sub->GetName();
    char Subclass[CHARBUFSIZE];
    strcpy(Subclass, subclass);
    strcat(Subclass, "_core");

    out << "class " << subclass << " : public " << Subclass << " {\n";
    out << "public:\n";
    out << "    " << subclass << "();\n";
    out << "};\n\n";

    return out.good();
}

boolean EditorCode::KeyConstInits(ostream& out) {
    EditorComp* edcomp = GetEditorComp();

    MemberNameVar* keymap = edcomp->GetKeyMap();
    SubclassNameVar* key_sub = keymap->GetSubclass();

    const char* subclass = key_sub->GetName();

    char Subclass[CHARBUFSIZE];
    strcpy(Subclass, subclass);
    strcat(Subclass, "_core");

    out << subclass << "::" << subclass << "(";
    out << ") : " << Subclass << "() {}\n\n";

    return out.good();
}

boolean EditorCode::SelCoreConstDecls(ostream& out) { 
    EditorComp* edcomp = GetEditorComp();

    MemberNameVar* selection = edcomp->GetSelection();
    SubclassNameVar* sel_sub = selection->GetSubclass();

    const char* subclass = sel_sub->GetName();
    const char* baseclass = sel_sub->GetBaseClass();

    char Subclass[CHARBUFSIZE];
    strcpy(Subclass, subclass);
    strcat(Subclass, "_core");

    out << "class " << Subclass << " : public " << baseclass << " {\n";
    out << "public:\n";
    out << "    " << Subclass << "(Selection* = nil);\n";
    out << "};\n\n";

    return out.good();
}

boolean EditorCode::SelCoreConstInits(ostream& out) {
    EditorComp* edcomp = GetEditorComp();

    MemberNameVar* selection = edcomp->GetSelection();
    SubclassNameVar* sel_sub = selection->GetSubclass();

    const char* subclass = sel_sub->GetName();
    const char* baseclass = sel_sub->GetBaseClass();

    char Subclass[CHARBUFSIZE];
    strcpy(Subclass, subclass);
    strcat(Subclass, "_core");

    out << Subclass << "::" << Subclass << "(\n";
    out << "    Selection* sel\n";
    out << ") : " << baseclass << "(sel) {";
    out << "}\n\n";

    return out.good();
}

boolean EditorCode::SelConstDecls(ostream& out) {
    EditorComp* edcomp = GetEditorComp();

    MemberNameVar* selection = edcomp->GetSelection();
    SubclassNameVar* sel_sub = selection->GetSubclass();

    const char* subclass = sel_sub->GetName();
    char Subclass[CHARBUFSIZE];
    strcpy(Subclass, subclass);
    strcat(Subclass, "_core");

    out << "class " << subclass << " : public " << Subclass << " {\n";
    out << "public:\n";
    out << "    " << subclass << "(Selection* = nil);\n";
    out << "};\n\n";

    return out.good();
}

boolean EditorCode::SelConstInits(ostream& out) {
    EditorComp* edcomp = GetEditorComp();

    MemberNameVar* selection = edcomp->GetSelection();
    SubclassNameVar* sel_sub = selection->GetSubclass();

    const char* subclass = sel_sub->GetName();
    char Subclass[CHARBUFSIZE];
    strcpy(Subclass, subclass);
    strcat(Subclass, "_core");

    out << subclass << "::" << subclass << "(\n";
    out << "    Selection* sel\n";
    out << ") : " << Subclass << "(sel) {}\n\n";

    return out.good();
}

boolean EditorCode::EmitIncludeHeaders(ostream& out) {
    boolean ok = MonoSceneClassCode::EmitIncludeHeaders(out);
    if (!_namelist->Search("editor")) {
        _namelist->Append("editor");
        out << "#include <Unidraw/classes.h>\n";
        out << "#include <Unidraw/creator.h>\n";
        out << "#include <Unidraw/globals.h> \n";
        out << "#include <Unidraw/grid.h> \n";
        out << "#include <Unidraw/kybd.h>\n";
        out << "#include <Unidraw/unidraw.h>\n";
        out << "#include <Unidraw/upage.h>\n";
        out << "#include <Unidraw/Tools/tool.h>\n";
    }
    if (!_namelist->Search("uctrl")) {
        _namelist->Append("uctrl");
        out << "#include <Unidraw/uctrl.h>\n";
    }
    if (!_namelist->Search("selection")) {
        _namelist->Append("selection");
        out << "#include <Unidraw/selection.h>\n";
    }
    if (!_namelist->Search("keymap")) {
        _namelist->Append("keymap");
        out << "#include <Unidraw/keymap.h>\n";
    }
    if (!_namelist->Search("grview")) {
        _namelist->Append("grview");
        out << "#include <Unidraw/Components/grview.h> \n";
    }
    if (!_namelist->Search("ctrlinfo")) {
        _namelist->Append("ctrlinfo");
        out << "#include <Unidraw/ctrlinfo.h> \n";
    }
    if (!_namelist->Search("grcomp")) {
        _namelist->Append("grcomp");
        out << "#include <Unidraw/Components/grcomp.h>\n";
    }
    if (!_namelist->Search("control")) {
        _namelist->Append("control");
        out << "#include <InterViews/control.h>\n";
    }
    if (!_namelist->Search("window")) {
        _namelist->Append("window");
        out << "#include <InterViews/window.h>\n";
    }
    if (!_namelist->Search("style")) {
        _namelist->Append("style");
        out << "#include <InterViews/style.h>\n";
    }
    if (!_namelist->Search("session")) {
        _namelist->Append("session");
        out << "#include <InterViews/session.h>\n";
    }
    if (!_namelist->Search("string")) {
        _namelist->Append("string");
        out << "#include <OS/string.h>\n";
    }
    return ok && out.good();
}



