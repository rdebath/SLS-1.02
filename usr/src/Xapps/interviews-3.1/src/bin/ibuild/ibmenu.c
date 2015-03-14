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
 * MenuItem component definitions.
 */

#include "ibclasses.h"
#include "ibcmds.h"
#include "ibcode.h"
#include "ibcreator.h"
#include "ibdialogs.h"
#include "ibed.h"
#include "ibglobals.h"
#include "ibinteractor.h"
#include "ibmenu.h"
#include "ibtools.h"
#include "ibvars.h"
#include "ibvarviews.h"

#include <Unidraw/catalog.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/iterator.h>
#include <Unidraw/manips.h>
#include <Unidraw/statevars.h>
#include <Unidraw/ulist.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/edit.h>
#include <Unidraw/Commands/macro.h>
#include <Unidraw/Commands/align.h>
#include <Unidraw/Tools/tool.h>

#include <InterViews/button.h>
#include <InterViews/painter.h>
#include <InterViews/shape.h>
#include <InterViews/transformer.h>

#include <OS/memory.h>

#include <stdio.h>
#include <stdlib.h>
#include <stream.h>
#include <string.h>
#include <ctype.h>

/*****************************************************************************/

void TransformText(char* name) {
    char* clone = name;
    while(*clone != '\0') {
	if (isspace(*clone)) {
	    int len = strlen(clone)-1;
	    Memory::copy(clone+1, clone, len);
	    clone[len] = '\0';
	} else {
	    clone++;
	}
    }
    clone = name;
    while(*clone != '\0') {
	if (!isalnum(*clone)) {
	    *clone = '_';
	}
	clone++;
    }
    clone = name;
    int len = strlen(clone);
    Memory::copy(clone, clone+1, len);
    clone[len+1] = '\0';
    clone[0] = '_';
}

class StringData : public lbrtData {
public:
    StringData(InteractorComp*, const char* name);
    virtual ~StringData();
public:
    char* _name;
    boolean _tracked;
};

StringData::StringData (
    InteractorComp* icomp, const char* name
) : lbrtData(icomp) {
    _name = strnew(name);
    _tracked = false;
}

StringData::~StringData () {
    delete _name;
}

/*****************************************************************************/

ClassId MenuItemComp::GetClassId () { return MENUITEM_COMP; }

boolean MenuItemComp::IsA (ClassId id) {
    return MENUITEM_COMP == id || MessageComp::IsA(id);
}

MenuItemGraphic* MenuItemComp::GetMenuItemGraphic() {
    return (MenuItemGraphic*) GetGraphic();
}

MenuItemComp::MenuItemComp (MessageGraphic* g) : MessageComp(g) { 
    if (g != nil) {
	GetClassNameVar()->SetName(g->GetClassName());
	GetClassNameVar()->SetBaseClass(g->GetClassName());
        IBShape* ibshape = GetShapeVar()->GetShape();
        ibshape->hstretch = hfil;
    }
    _proc = nil;
}

void MenuItemComp::Instantiate () {
    MessageComp::Instantiate();
    if (_proc == nil) {
        _proc = new TrackNameVar("");
    }
}

void MenuItemComp::Interpret(Command* cmd) {
    if (cmd->IsA(EDIT_CMD)) {
        EditCmd* editcmd = (EditCmd*) cmd;
	editcmd->SetOldText(GetMessageGraphic()->GetText());
        GetMessageGraphic()->SetText(editcmd->GetNewText());
	Reconfig();

	StringData* sd = new StringData(this, _proc->GetName());
	cmd->Store(this, sd);
	if (*_proc->GetName() == '\0' || _proc->GetMachGen()) {
	    char ProcName[CHARBUFSIZE];
	    strcpy(ProcName, editcmd->GetNewText());
	    TransformText(ProcName);
            
            GetFirewallCmd firewallCmd(this);
            firewallCmd.Execute();
            GetConflictCmd conflictCmd(firewallCmd.GetFirewall(), ProcName);
            conflictCmd.Execute();
            UList* cl = conflictCmd.GetConflict();
            for (UList* i = cl->First(); i != cl->End(); i = i->Next()) {
                StateVar* state = (StateVar*) (*i)();
                if (!state->IsA(PROCNAME_VAR)) {
                    *ProcName = '\0';
                    break;
                }
            }

	    _proc->SetName(ProcName);
	    sd->_tracked = true;
	}
        Place(this);
        Propagate(cmd);

    } else if (cmd->IsA(GETCONFLICT_CMD)) {
        GetConflictCmd* gcmd = (GetConflictCmd*) cmd;
        const char* cname = gcmd->GetCName();
        MessageComp::Interpret(cmd);
        UList* conflictlist = gcmd->GetConflict();
        const char* procname = _proc->GetName();
        if (strcmp(procname, cname) == 0) {
            conflictlist->Append(new UList(_proc));
        }
    } else {
	MessageComp::Interpret(cmd);
    }
}

void MenuItemComp::Uninterpret(Command* cmd) {
    if (cmd->IsA(EDIT_CMD)) {
	EditCmd* editcmd = (EditCmd*) cmd;
        editcmd->SetNewText(GetMessageGraphic()->GetText());
        GetMessageGraphic()->SetText(editcmd->GetOldText());
	Reconfig();
        
	StringData* sd = (StringData*) cmd->Recall(this);
	if (sd->_tracked == true) {
	    _proc->SetName(sd->_name);
        }
        Place(this, sd->_l, sd->_b, sd->_r-1, sd->_t-1);
        Unpropagate(cmd);

    } else {
	MessageComp::Uninterpret(cmd);
    }
}

void MenuItemComp::Reconfig () {
    Shape* shape = GetShapeVar()->GetShape();
    int w, h;
    GetMessageGraphic()->Natural(w, h);
    shape->width = w;
    shape->height = h;
    shape->hshrink = shape->vshrink = 4;
    GetShapeVar()->Notify();
}

void MenuItemComp::SetState (const char* name, StateVar* stateVar) {
    if (strcmp(name, "TrackNameVar") == 0) {
        _proc = (TrackNameVar*) stateVar;

    } else {
        MessageComp::SetState(name, stateVar);
    }
}

StateVar* MenuItemComp::GetState (const char* name) {
    StateVar* stateVar = nil;

    if (strcmp(name, "TrackNameVar") == 0) {
        stateVar = _proc;
    } else {
        stateVar = MessageComp::GetState(name);
    }

    return stateVar;
}

InteractorComp& MenuItemComp::operator = (InteractorComp& comp) {
    MessageComp::operator = (comp);
    if  (comp.IsA(MENUITEM_COMP)) {
	MenuItemComp* mcomp = (MenuItemComp*) &comp;
	*_proc = *mcomp->GetTrackNameVar();
    }
    return *this;
}

void MenuItemComp::Read (istream& in) {
    MessageComp::Read(in);
    _proc = (TrackNameVar*) unidraw->GetCatalog()->ReadStateVar(in);
}

void MenuItemComp::Write (ostream& out) {
    MessageComp::Write(out);
    unidraw->GetCatalog()->WriteStateVar(_proc, out);
}

/*****************************************************************************/

MenuItemView::MenuItemView (MenuItemComp* subj) : MessageView(subj) { }

MenuItemComp* MenuItemView::GetMenuItemComp () {
    return (MenuItemComp*) GetSubject();
}

ClassId MenuItemView::GetClassId () { return MENUITEM_VIEW; }

boolean MenuItemView::IsA (ClassId id) {
    return MENUITEM_VIEW == id || MessageView::IsA(id);
}

InfoDialog* MenuItemView::GetInfoDialog () {
    InfoDialog* info = MessageView::GetInfoDialog();
    ButtonState* state = info->GetState();

    MenuItemComp* mcomp = GetMenuItemComp();
    TrackNameVar* proc = mcomp->GetTrackNameVar();
    info->Include(new TrackNameVarView(proc, state, mcomp));

    return info;
}

Manipulator* MenuItemView::CreateManipulator (
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

Command* MenuItemView::InterpretManipulator (Manipulator* m) {
    Command* cmd = nil;
    Tool* tool = m->GetTool();

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        cmd = InteractorView::InterpretManipulator(m);
    } else {
        cmd = MessageView::InterpretManipulator(m);
    }

    return cmd;
}

/*****************************************************************************/

MenuItemCode::MenuItemCode (MenuItemComp* subj) : MessageCode(subj) { }

MenuItemComp* MenuItemCode::GetMenuItemComp(){
    return (MenuItemComp*) GetSubject();
}

ClassId MenuItemCode::GetClassId () { return MENUITEM_CODE; }

boolean MenuItemCode::IsA(ClassId id) {
    return MENUITEM_CODE == id || MessageCode::IsA(id);
}

boolean MenuItemCode::Definition (ostream& out) {
    boolean ok = true;
    if (_emitExpHeader) {
	InteractorComp* icomp = GetIntComp();
	MemberNameVar* mnamer = icomp->GetMemberNameVar();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        if (!snamer->IsSubclass()) {
            if (_scope && mnamer->GetExport() && !_namelist->Search("menu")) {
                _namelist->Append("menu");
                out << "#include <InterViews/menu.h>\n";
            }
        } else {
            ok = ok && CodeView::Definition(out);
        }
    } else if (_emitCorehHeader) {
	InteractorComp* icomp = GetIntComp();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        const char* subclass = snamer->GetName();
        const char* fwname = GetFirewall();
        if (snamer->IsSubclass() && strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("menu")) {
                _namelist->Append("menu");
                out << "#include <InterViews/menu.h>\n";
            }
            if (fwname != nil && !_namelist->Search(fwname)) {
                _namelist->Append(fwname);
                out << "#include \"" << fwname << "-core.h\"\n";
            }
        }
    } else if (_emitForward) {
        char Func[CHARBUFSIZE];
        char coreclass[CHARBUFSIZE];
        
	InteractorComp* icomp = GetIntComp();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        const char* subclass = snamer->GetName();

        const char* fwname = GetFirewall();
        if (fwname != nil) {
            sprintf(coreclass, "%s_core", fwname);
            strcpy(Func, coreclass);
            strcat(Func, "_Func");
        }

        if (strcmp(subclass, _classname) == 0) {
            if (fwname != nil && !_namelist->Search(fwname)) {
                _namelist->Append(fwname);
                out << "\n#ifndef " << fwname << "_core_func\n";
                out << "#define " << fwname << "_core_func\n";
                out << "typedef void ("<<coreclass<< "::*" <<Func<< ")();\n";
                out << "#endif\n\n";
            }

        } else {
            ok = ok && CodeView::Definition(out);
        }
    } else if (
	_emitProperty || _emitInstanceDecls || 
        _emitClassHeaders || _emitHeaders
    ) {
        ok = ok &&  CodeView::Definition(out);

    } else if (_emitInstanceInits) {
        char MenuClass[CHARBUFSIZE];
        char coreclass[CHARBUFSIZE];
        InteractorComp* icomp = GetIntComp();
        const char* mname = icomp->GetMemberNameVar()->GetName();
        
        if (!_emitMain) {
            GetCoreClassName(coreclass);
            strcpy(MenuClass, coreclass);
            strcat(MenuClass, "_Menu");
        } else {
            strcpy(MenuClass, icomp->GetClassNameVar()->GetName());
        }
        if (!_instancelist->Find((void*) mname)) {
            _instancelist->Append(new UList((void*)mname));
            
            MenuItemComp* menuItem = GetMenuItemComp();
            
            const char* proc = menuItem->GetTrackNameVar()->GetName();
            const char* text = menuItem->GetMenuItemGraphic()->GetText();
            boolean export = icomp->GetMemberNameVar()->GetExport();
            Shape* shape = icomp->GetShapeVar()->GetShape();
            
            if (icomp->GetClassNameVar()->IsSubclass()) {
                BeginInstantiate(out);
                out << "(";
                
            } else {
                if (export && !_emitMain) {
                    out << "    " << mname << " = ";
                    out << "new " << MenuClass;
                } else {
                    out << "    " << MenuClass << "* ";
                    out << mname << " = ";
                    out << "new " << MenuClass;
                }
                out << "(";
            }
            InstanceName(out);
            out << "new Message(";
            out << "\"" << text << "\", ";
            Alignment a = menuItem->GetMessageGraphic()->GetAlignment();
            ok = ok && Align(a, out);
            out << ", 2, " << shape->hstretch << ", " << shape->vstretch;
	    out << "))";
            EndInstantiate(out);
            
            if (!_emitMain && proc != nil && *proc != '\0') {
                if (!export || icomp->GetClassNameVar()->IsSubclass()) {
                    out << "    " << mname << "->SetCoreClass(this);\n";
                    out <<"    "<< mname <<"->SetCoreFunc("<< "&" << coreclass;
                    out << "::" << proc << ");\n";
                } else {
                    out << "    ((" << MenuClass << "*)" << mname;
                    out << ")->SetCoreClass(this);\n";
                    out << "    ((" << MenuClass << "*)" << mname;
                    out <<")->SetCoreFunc("<< "&" << coreclass;
                    out << "::" << proc << ");\n";
                } 
            }
        }
    } else if (_emitFunctionDecls) {
	MenuItemComp* menuItem = GetMenuItemComp();
	const char* proc = menuItem->GetTrackNameVar()->GetName();

	if (*proc != '\0' && !_functionlist->Search(proc)) {
            _functionlist->Append(proc);
	    out << "    virtual void " << proc << "();\n";	
	}

    } else if (_emitFunctionInits) {
	char coreclass[CHARBUFSIZE];

        GetCoreClassName(coreclass);
	MenuItemComp* menuItem = GetMenuItemComp();
	const char* proc = menuItem->GetTrackNameVar()->GetName();

	if (*proc != '\0' && !_functionlist->Search(proc)) {
            _functionlist->Append(proc);
	    if (_emitClassInits)  {
                out << "void " << _classname << "::" << proc;
                out << "() {\n    /* unimplemented */\n}\n\n";
            } else {
                out << "void " << coreclass << "::" << proc << "() {}\n";
            }
	}

    } else if (_emitBSDecls || _emitBSInits) {
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
    
boolean MenuItemCode::CoreConstDecls(ostream& out) { 
    char Func[CHARBUFSIZE];
    char coreclass[CHARBUFSIZE];
    const char* fwname = GetFirewall();

    if (fwname != nil) {
        sprintf(coreclass, "%s_core", fwname);
        strcpy(Func, coreclass);
        strcat(Func, "_Func");
    }
    
    out << "(const char*, Interactor*);\n\n";

    if (fwname != nil) {
        out << "    virtual void Do();\n";
        out << "    void SetCoreClass(" << coreclass << "*);\n";
        out << "    void SetCoreFunc(" << Func << ");\n";
        out << "private:\n";
        out << "    " << Func << " _func;\n";
        out << "    " << coreclass << "* _coreclass;\n";
    }
    return out.good();
}

boolean MenuItemCode::CoreConstInits(ostream& out) {
    char Func[CHARBUFSIZE];
    char coreclass[CHARBUFSIZE];
    char mycoreclass[CHARBUFSIZE];
    const char* fwname = GetFirewall();

    if (fwname != nil) {
        sprintf(coreclass, "%s_core", fwname);
        strcpy(Func, coreclass);
        strcat(Func, "_Func");
    }
    GetCoreClassName(mycoreclass);

    InteractorComp* icomp = GetIntComp();
    SubclassNameVar* snamer = icomp->GetClassNameVar();
    const char* baseclass = snamer->GetBaseClass();
    const char* subclass = snamer->GetName();

    out << "(\n    const char* name, Interactor* i\n) : ";
    out << baseclass << "(name, i) {\n";
    out << "    SetClassName(\"" << subclass << "\");\n";
    if (fwname != nil) {
        out << "    _func = nil;\n";
        out << "    _coreclass = nil;\n";
    }
    out << "}\n\n";

    if (fwname != nil) {
        out << "void " << mycoreclass << "::SetCoreClass(" << coreclass;
        out << "* core) {\n";
        out << "    _coreclass = core;\n";
        out << "}\n\n";
        
        out << "void " << mycoreclass << "::SetCoreFunc(" << Func;
        out << " func) {\n";
        out << "    _func = func;\n";
        out << "}\n\n";
        
        out << "void " << mycoreclass << "::Do() {\n";
        out << "    if (_func != nil) {\n";
        out << "        (_coreclass->*_func)();\n";
        out << "    }\n";
        out << "}\n\n";
    } 

    return out.good();
}

boolean MenuItemCode::ConstDecls(ostream& out) {
    out << "(const char*, Interactor*);\n";
    return out.good();
}

boolean MenuItemCode::ConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n    const char* name, Interactor* i\n) : ";
    out << coreclass << "(name, i) {}\n\n";

    return out.good();
}

boolean MenuItemCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();

    if (!snamer->IsSubclass() && !_namelist->Search("menu")) {
        _namelist->Append("menu");
        out << "#include <InterViews/menu.h> \n";
    }
    if (
        strcmp(snamer->GetName(), _classname) != 0 && 
        !_namelist->Search("message")
    ) {
        _namelist->Append("message");
        out << "#include <InterViews/message.h> \n";
    }
    return out.good();
}

/*****************************************************************************/

ClassId MenuBodyComp::GetClassId () { return MENUBODY_COMP; }

boolean MenuBodyComp::IsA (ClassId id) {
    return MENUBODY_COMP == id || VBoxComp::IsA(id);
}

MenuBodyComp::MenuBodyComp () : VBoxComp() { }

/*****************************************************************************/

ClassId MenuBarComp::GetClassId () { return MENUBAR_COMP; }

boolean MenuBarComp::IsA (ClassId id) {
    return MENUBAR_COMP == id || HBoxComp::IsA(id);
}

MenuBarComp::MenuBarComp () : HBoxComp() {
    GetClassNameVar()->SetName("MenuBar");
    GetClassNameVar()->SetBaseClass("MenuBar");
}

/*****************************************************************************/

ClassId MenuBarCode::GetClassId () { return MENUBAR_CODE; }
boolean MenuBarCode::IsA (ClassId id) {
    return MENUBAR_CODE ==id || BoxCode::IsA(id);
}

MenuBarCode::MenuBarCode (MenuBarComp* subj) : BoxCode(subj) { }
MenuBarComp* MenuBarCode::GetMenuBarComp () {
    return (MenuBarComp*) GetSubject(); 
}

boolean MenuBarCode::Definition (ostream& out) {
    boolean ok = true;
    Iterator i;

    if (
        _emitInstanceDecls || _emitForward || 
        _emitProperty || _emitClassHeaders || _emitHeaders
    ) {
        ok = ok && BoxCode::Definition(out);

    } else if (_emitExpHeader) {
	InteractorComp* icomp = GetIntComp();
	MemberNameVar* mnamer = icomp->GetMemberNameVar();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        if (!snamer->IsSubclass()) {
            if (_scope && mnamer->GetExport() && !_namelist->Search("menu")) {
                _namelist->Append("menu");
                out << "#include <InterViews/menu.h>\n";
            }
        } else {
            ok = ok && CodeView::Definition(out);
        }
        ok = ok && Iterate(out);

    } else if (_emitCorehHeader) {
	InteractorComp* icomp = GetIntComp();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        const char* subclass = snamer->GetName();
        if (snamer->IsSubclass() && strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("menu")) {
                _namelist->Append("menu");
                out << "#include <InterViews/menu.h>\n";
            }
        } else {
            ok = ok && Iterate(out);
        }
    } else if (_emitInstanceInits) {
	InteractorComp* icomp = GetIntComp();
        const char* mname = icomp->GetMemberNameVar()->GetName();

	if (!_instancelist->Find((void*) mname)) {
	    _instancelist->Append(new UList((void*)mname));
		
	    BeginInstantiate(out);
            out << "(";
            InstanceName(out, "");
            out << ")";
            EndInstantiate(out);
	}

        ok = ok && Iterate(out);

	if (AllKidsDefined() && !_lock) {
	    _lock = true;
	    for (icomp->First(i); !icomp->Done(i); icomp->Next(i)) {
		InteractorComp* kid = (InteractorComp*) icomp->GetComp(i);
		const char* instance = kid->GetMemberNameVar()->GetName();
                out << "    ";
                if (
                    kid->IsA(MENUITEM_COMP) || kid->IsA(PULLMENU_COMP)
                ) {
                    out << mname << "->Include(";
                    
                } else {
                    out << mname << "->Insert(";
                }
                out << instance << ");\n";	
            }
	}

    } else {
        ok = ok && BoxCode::Definition(out);
    }
    return ok;
}

boolean MenuBarCode::CoreConstDecls(ostream& out) { 
    out << "(const char*);\n";
    return out.good();
}

boolean MenuBarCode::CoreConstInits(ostream& out) {
    InteractorComp* icomp = GetIntComp();
    SubclassNameVar* snamer = icomp->GetClassNameVar();
    const char* baseclass = snamer->GetBaseClass();
    const char* subclass = snamer->GetName();

    out << "(const char* i) : " << baseclass << "(i) {\n";
    out << "    SetClassName(\"" << subclass << "\");\n";
    out << "}\n\n";
    return out.good();
}

boolean MenuBarCode::ConstDecls(ostream& out) {
    out << "(const char*);\n";
    return out.good();
}

boolean MenuBarCode::ConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(const char* i) : " << coreclass << "(i) {}\n\n";
    return out.good();
}

boolean MenuBarCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();

    if (!snamer->IsSubclass() && !_namelist->Search("menu")) {
        _namelist->Append("menu");
        out << "#include <InterViews/menu.h> \n";
    }
    return out.good();
}
/*****************************************************************************/

ClassId PopupMenuComp::GetClassId () { return POPUPMENU_COMP; }

boolean PopupMenuComp::IsA (ClassId id) {
    return POPUPMENU_COMP == id || VBoxComp::IsA(id);
}

PopupMenuComp::PopupMenuComp () : VBoxComp() {
    GetClassNameVar()->SetName("PopupMenu");
    GetClassNameVar()->SetBaseClass("PopupMenu");
}

/*****************************************************************************/

class PullMenuGraphic : public IBGraphic {
public:
    PullMenuGraphic(CanvasVar* = nil, Graphic* = nil, boolean open = false);

    virtual void SetCanvasVar(CanvasVar*);
    virtual void SetColors(PSColor* f, PSColor* b);
    virtual void SetFont(PSFont*);
    virtual void Bequeath();

    MenuItemGraphic* GetMenuItemGraphic();
    void SetMenuItemGraphic(MenuItemGraphic*);
    void SetOpen(boolean);
    boolean GetOpen();
    void SetDamage(boolean);
    boolean GetDamage();

protected:
    virtual void getExtent(float&, float&, float&, float&, float&, Graphic*);
    virtual void draw(Canvas*, Graphic*);
    virtual void drawClipped(Canvas*, Coord, Coord, Coord, Coord, Graphic*);

protected:
    MenuItemGraphic* _menuGraphic;
    boolean _open;
    boolean _damage;
};

PullMenuGraphic::PullMenuGraphic (
    CanvasVar* c, Graphic* g, boolean open
) : IBGraphic(c, g) {
    _menuGraphic = nil;
    _open = open;
}

void PullMenuGraphic::SetMenuItemGraphic(MenuItemGraphic* mgr) {
    _menuGraphic = mgr;
}

MenuItemGraphic* PullMenuGraphic::GetMenuItemGraphic() {
    return _menuGraphic;
}

void PullMenuGraphic::SetDamage (boolean damage) { _damage = damage; }

boolean PullMenuGraphic::GetDamage () { return _damage; }

void PullMenuGraphic::SetOpen(boolean open) { _open = open; }

boolean PullMenuGraphic::GetOpen() { return _open; }

void PullMenuGraphic::SetCanvasVar(CanvasVar* cvar) {
    IBGraphic::SetCanvasVar(cvar);
    if (_menuGraphic != nil) {
	_menuGraphic->SetCanvasVar(cvar);
    }
}

void PullMenuGraphic::Bequeath () {
    Remove(_menuGraphic);
    IBGraphic::Bequeath();
    Append(_menuGraphic);
}

void PullMenuGraphic::SetColors(PSColor* f, PSColor* b) {
    IBGraphic::SetColors(f, b);
    if (_menuGraphic != nil) {
	_menuGraphic->SetColors(f, b);
    }
}

void PullMenuGraphic::SetFont(PSFont* psFont) {
    IBGraphic::SetFont(psFont);
    if (_menuGraphic != nil) {
	_menuGraphic->SetFont(psFont);
    }
}

void PullMenuGraphic::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    if (!_open && _menuGraphic != nil) {
        FullGraphic gstemp;
        concat(_menuGraphic, gs, &gstemp);
        getExtentGraphic(_menuGraphic, l, b, cx, cy, tol, &gstemp);

    } else {
        IBGraphic::getExtent(l, b, cx, cy, tol, gs);
    }
}

void PullMenuGraphic::draw (Canvas* c, Graphic* gs) {
    if (_open) {
	_menuGraphic->SetColors(
            _menuGraphic->GetBgColor(), _menuGraphic->GetFgColor()
        );
	IBGraphic::draw(c, gs);
	_menuGraphic->SetColors(
            _menuGraphic->GetBgColor(), _menuGraphic->GetFgColor()
        );

    } else if (_menuGraphic != nil) {
	FullGraphic gstemp;
        Transformer ttemp;
	gstemp.SetTransformer(&ttemp);

        concatGraphic(_menuGraphic, _menuGraphic, gs, &gstemp);
        drawGraphic(_menuGraphic, c, &gstemp);
	gstemp.SetTransformer(nil); /* to avoid deleting ttemp explicitly */
    }
}

void PullMenuGraphic::drawClipped (
    Canvas* c, Coord l, Coord b, Coord r, Coord t, Graphic* gs
) {
    Graphic::drawClipped(c, l, b, r, t, gs);
}

/*****************************************************************************/

ClassId PullMenuComp::GetClassId () { return PULLMENU_COMP; }

boolean PullMenuComp::IsA (ClassId id) {
    return PULLMENU_COMP == id || SceneComp::IsA(id);
}

PullMenuComp::PullMenuComp (
    MenuItemGraphic* gr
) : SceneComp (new PullMenuGraphic(nil, stdgraphic)) { 
    PullMenuGraphic* pmGraphic = (PullMenuGraphic*) GetGraphic();
    if (gr != nil) {
        GetClassNameVar()->SetName(gr->GetClassName());
        GetClassNameVar()->SetBaseClass(gr->GetClassName());
	pmGraphic->Prepend(gr);
	pmGraphic->SetMenuItemGraphic(gr);
        IBShape* ibshape = GetShapeVar()->GetShape();
        ibshape->hstr = ibshape->vstr = true;
        ibshape->vstretch = 0;
        ibshape->hstretch = 0;
    }
    MenuBodyComp* menuBody = new MenuBodyComp;
    SceneComp::Append(menuBody);
    menuBody->GetGraphic()->SetTag(this);
    _menuGraphic = gr;
}

PullMenuComp::~PullMenuComp () {
    MenuBodyComp* menu = GetMenuBody();

    if (menu != nil) {
        SceneComp::Remove(menu);
        delete menu;
    }
}

void PullMenuComp::SetMenuItemGraphic (MenuItemGraphic* menu) {
    GetClassNameVar()->SetName(menu->GetClassName());
    GetClassNameVar()->SetBaseClass(menu->GetClassName());
    GetGraphic()->Remove(_menuGraphic);
    GetGraphic()->Prepend(menu);
    delete _menuGraphic;
    _menuGraphic = menu;
}

void PullMenuComp::Interpret (Command* cmd) {
    if (cmd->IsA(EDIT_CMD)) {
	EditCmd* editcmd = (EditCmd*) cmd;
	editcmd->SetOldText(GetMenuItemGraphic()->GetText());
	GetMenuItemGraphic()->SetText(editcmd->GetNewText());
	Reconfig();
        StoreCanvas(cmd);
        Place(this);
        Propagate(cmd);

    } else if (cmd->IsA(ALIGN_CMD)) {
        AlignCmd* alignCmd = (AlignCmd*) cmd;
        Alignment a;
	MenuItemGraphic* mg = GetMenuItemGraphic();
	cmd->Store(this, new VoidData((void*) mg->GetAlignment()));
        alignCmd->GetAlignment(a, a);
	mg->SetAlignment(a);
        Notify();
        if (!cmd->GetClipboard()->Includes(this)) {
            SceneComp::Interpret(cmd);

        } else {
            Propagate(cmd);
        }

    } else if (cmd->IsA(COLOR_CMD) || cmd->IsA(FONT_CMD)) {
        GraphicComps::Interpret(cmd);
        if (!cmd->GetClipboard()->Includes(this)) {
            SceneComp::Interpret(cmd);

        } else {
            Propagate(cmd);
        }

    } else if (cmd->IsA(UNGROUP_CMD)) {
        Editor* ed = cmd->GetEditor();
        if (ed->GetComponent() != this) {
            PullMenuGraphic* pmg = (PullMenuGraphic*) GetGraphic();
            pmg->SetDamage(true);
            Notify();
        }
        SceneComp::Interpret(cmd);
        
    } else {
        SceneComp::Interpret(cmd);
    }
}

void PullMenuComp::Uninterpret(Command* cmd) {
    if (cmd->IsA(EDIT_CMD)) {
        EditCmd* editcmd = (EditCmd*) cmd;
        editcmd->SetNewText(GetMenuItemGraphic()->GetText());
        GetMenuItemGraphic()->SetText(editcmd->GetOldText());
	Reconfig();
        RestoreCanvas(cmd);
        Unpropagate(cmd);

    } else if (cmd->IsA(ALIGN_CMD)) {
        VoidData* vd = (VoidData*) cmd->Recall(this);
	Alignment a = (Alignment) vd->_void;
	GetMenuItemGraphic()->SetAlignment(a);
        Notify();
        if (!cmd->GetClipboard()->Includes(this)) {
            SceneComp::Uninterpret(cmd);
        } else {
            Unpropagate(cmd);
        }

    } else if (cmd->IsA(COLOR_CMD) || cmd->IsA(FONT_CMD)) {
        GraphicComps::Uninterpret(cmd);
        if (!cmd->GetClipboard()->Includes(this)) {
            SceneComp::Uninterpret(cmd);

        } else {
            Unpropagate(cmd);
        }

    } else if (cmd->IsA(UNGROUP_CMD)) {
        Editor* ed = cmd->GetEditor();
        if (ed->GetComponent() != this) {
            PullMenuGraphic* pmg = (PullMenuGraphic*) GetGraphic();
            pmg->SetDamage(true);
            Notify();
        }
        SceneComp::Uninterpret(cmd);
        
    } else {
        SceneComp::Uninterpret(cmd);
    }
}

void PullMenuComp::First(Iterator& i) {
    MenuBodyComp* body = GetMenuBody();
    body->First(i);
}
 
void PullMenuComp::Last(Iterator& i) {
    MenuBodyComp* body = GetMenuBody();
    body->Last(i);
}
 
void PullMenuComp::Next(Iterator& i) {
    MenuBodyComp* body = GetMenuBody();
    body->Next(i);
}
 
void PullMenuComp::Prev(Iterator& i) {
    MenuBodyComp* body = GetMenuBody();
    body->Prev(i);
}
 
boolean PullMenuComp::Done(Iterator i) {
    MenuBodyComp* body = GetMenuBody();
    return body->Done(i);
}
 
boolean PullMenuComp::IsEmpty() {
    Iterator i;
    MenuBodyComp* body = GetMenuBody();
    body->First(i);
    return body->Done(i);
}
 
GraphicComp* PullMenuComp::GetComp(Iterator i) {
    MenuBodyComp* body = GetMenuBody();
    return body->GetComp(i);
}
 
void PullMenuComp::SetComp(GraphicComp* comp, Iterator& i) {
    MenuBodyComp* body = GetMenuBody();
    body->SetComp(comp, i);
}
 
void PullMenuComp::Append(GraphicComp* comp) {
    MenuBodyComp* body = GetMenuBody();
    body->Append(comp);
}

void PullMenuComp::Prepend(GraphicComp* comp) {
    MenuBodyComp* body = GetMenuBody();
    body->Prepend(comp);
}

void PullMenuComp::InsertBefore(Iterator i, GraphicComp* comp) {
    MenuBodyComp* body = GetMenuBody();
    body->InsertBefore(i, comp);
}

void PullMenuComp::InsertAfter(Iterator i, GraphicComp* comp) {
    MenuBodyComp* body = GetMenuBody();
    body->InsertAfter(i, comp);
}

void PullMenuComp::Remove(GraphicComp* comp) {
    MenuBodyComp* body = GetMenuBody();
    body->Remove(comp);
}

void PullMenuComp::Remove(Iterator& i) {
    MenuBodyComp* body = GetMenuBody();
    body->Remove(i);
}

void PullMenuComp::Bequeath () {
    MenuBodyComp* body = GetMenuBody();
    Graphic* root = GetGraphic();
    root->Bequeath();
    body->GetGraphic()->Bequeath();
}
 
MenuBodyComp* PullMenuComp::GetMenuBody() {
    Iterator i;
    SceneComp::First(i);
    return (MenuBodyComp*) SceneComp::GetComp(i);
}

void PullMenuComp::Reconfig () {
    Shape* shape = GetShapeVar()->GetShape();
    int w, h;
    _menuGraphic->Natural(w, h);
    shape->width = w;
    shape->height = h;
    shape->hshrink = shape->vshrink = 4;
    GetShapeVar()->Notify();
    MenuBodyComp* mbComp = GetMenuBody();
    mbComp->Reconfig();
}

void PullMenuComp::Resize () {
    InteractorComp::Resize();
    MenuBodyComp* mbComp = GetMenuBody();

    Place(mbComp);
    if (!IsEmpty()) {
        ReAdjust();
    }
    PullMenuGraphic* pmg = (PullMenuGraphic*) GetGraphic();
    pmg->SetDamage(true);
}

void PullMenuComp::ReAdjust () {
    MenuBodyComp* mbComp = GetMenuBody();
    Graphic* mbGraphic = mbComp->GetGraphic();

    if (_menuGraphic->GetClassId() == PRMENU_GRAPHIC) {
        _menuGraphic->Align(Top, mbGraphic, Top);
        _menuGraphic->Align(Right, mbGraphic, Left);
    } else {
        _menuGraphic->Align(Left, mbGraphic, Left);
        _menuGraphic->Align(Bottom, mbGraphic, Top);
    }
    mbGraphic->Bequeath();
    Place(mbComp);
}

void PullMenuComp::Read (istream& in) {
    SceneComp::Read(in);

    /* backward compability */
    IBShape* ibshape = GetShapeVar()->GetShape();
    ibshape->hstr = ibshape->vstr = true;

    Catalog* catalog = unidraw->GetCatalog();
    ClassId id;
    in >> id;
    _menuGraphic = (MenuItemGraphic*) catalog->GetCreator()->Create(id);
    _menuGraphic->Read(in);
    _menuGraphic->SetCanvasVar(GetCanvasVar());

    PullMenuGraphic* pmGraphic = (PullMenuGraphic*) GetGraphic();
    pmGraphic->Prepend(_menuGraphic);
    pmGraphic->SetMenuItemGraphic(_menuGraphic);
}

void PullMenuComp::Write (ostream& out) {
    ClassId id;
    SceneComp::Write(out);
    Catalog* catalog = unidraw->GetCatalog();
    id = _menuGraphic->GetClassId();
    out << " " << id << " ";
    _menuGraphic->Write(out);
}

/*****************************************************************************/

PullMenuView::PullMenuView (PullMenuComp* subj) : SceneView(subj) {
    _menuGraphic = nil;
}

PullMenuComp* PullMenuView::GetPullMenuComp () { 
    return (PullMenuComp*) GetSubject();
}

ClassId PullMenuView::GetClassId () { return PULLMENU_VIEW; }

boolean PullMenuView::IsA (ClassId id) {
    return PULLMENU_VIEW == id || SceneView::IsA(id);
}

Manipulator* PullMenuView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Manipulator* m = nil;
    IBEditor* ed = (IBEditor*) v->GetEditor();

    if (tool->IsA(RESHAPE_TOOL)) {
        rel = new Transformer;
        Painter* painter = InitPainter(_menuGraphic, rel);
        const char* text = _menuGraphic->GetText();
        int size = strlen(text);

        _menuGraphic->TotalTransformation(*rel);
        Coord xpos, ypos;
        _menuGraphic->GetTextPosition(xpos, ypos, _menuGraphic->GetFont());
        rel->Transform(xpos, ypos);
        m = new TextManip(v, text, size, xpos, ypos, painter, 0, tool);

    } else {
        m = SceneView::CreateManipulator(v, e, rel, tool);
    }
    return m;
}

Command* PullMenuView::InterpretManipulator (Manipulator* m) {
    IBEditor* ed = (IBEditor*) m->GetViewer()->GetEditor();
    Tool* tool = m->GetTool();
    Command* cmd = nil;

    if (tool->IsA(RESHAPE_TOOL)) {
        TextManip* tm = (TextManip*) m;
        int size;
        const char* text = tm->GetText(size);
	cmd = new EditCmd(ed, new Clipboard(GetPullMenuComp()), text);

    } else if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        Iterator i;

        cmd = InterpretGraphicCompManip(m);
	Clipboard* cb = cmd->GetClipboard();

        ColorVar* colVar = (ColorVar*) ed->GetState("ColorVar");
        FontVar* fontVar = (FontVar*) ed->GetState("FontVar");

        cb->First(i);
        Graphic* pgr = cb->GetComp(i)->GetGraphic();

        pgr->SetColors(colVar->GetFgColor(), colVar->GetBgColor());
        pgr->SetFont(fontVar->GetFont());
        cmd = new MacroCmd(ed, cmd, new PlaceCmd(ed, cb->Copy()));

    } else {
        cmd = SceneView::InterpretManipulator(m);
    }
    return cmd;
}

void PullMenuView::Interpret(Command* cmd) {
    if (cmd->IsA(TAB_CMD)) {
        TabTool tabTool;
        GetViewer()->UseTool(&tabTool);
	SceneView::Interpret(cmd);
    } else {
        SceneView::Interpret(cmd);
    }
}

Graphic* PullMenuView::GetGraphic () {
    Graphic* g = GraphicView::GetGraphic();

    if (g == nil) {
	PullMenuComp* pullComp = GetPullMenuComp();
	MenuItemGraphic* menuGraphic = pullComp->GetMenuItemGraphic();
        _menuGraphic = (MenuItemGraphic*) menuGraphic->Copy();
	PullMenuGraphic* pmGraphic = new PullMenuGraphic(nil, stdgraphic);
	pmGraphic->Prepend(_menuGraphic);
	pmGraphic->SetMenuItemGraphic(_menuGraphic);

	g = pmGraphic;
	SetGraphic(g);
    }
    return g;
}

void PullMenuView::Update () {
    PullMenuGraphic* pview = (PullMenuGraphic*) GetGraphic();
    PullMenuGraphic* pcomp = (PullMenuGraphic*)GetPullMenuComp()->GetGraphic();
    MenuItemGraphic* menuGraphic = GetPullMenuComp()->GetMenuItemGraphic();

    Viewer* viewer = GetViewer();
    const char* subtext = menuGraphic->GetText();
    const char* viewtext = _menuGraphic->GetText();
    if (
        Different(pcomp, pview) ||
        Different(menuGraphic, _menuGraphic) ||
        _menuGraphic->GetAlignment() != menuGraphic->GetAlignment() ||
        strcmp(subtext, viewtext) != 0 || pcomp->GetDamage()
    ) {
        pcomp->SetDamage(false);
        IncurDamage(pview);
        *(Graphic*) pview = *(Graphic*) pcomp;
        *(Graphic*) _menuGraphic = *(Graphic*) menuGraphic;
        UpdateCanvasVar();
        _menuGraphic->SetText(menuGraphic->GetText());
        _menuGraphic->SetAlignment(menuGraphic->GetAlignment());
        IncurDamage(pview);
    }
    if (viewer != nil && viewer->GetGraphicView() == this) {
	PullMenuGraphic* pmGraphic = (PullMenuGraphic*) pview;
        boolean open = pmGraphic->GetOpen();
	pmGraphic->SetOpen(true);	
        if (!open) {
            IncurDamage(_menuGraphic);
        }
        GraphicViews::Update();
    }
}

/*****************************************************************************/

ClassId PullMenuCode::GetClassId () { return PULLMENU_CODE; }

boolean PullMenuCode::IsA (ClassId id) { 
    return PULLMENU_CODE ==id || MessageCode::IsA(id);
}

PullMenuCode::PullMenuCode (
    PullMenuComp* subj
) : MessageCode((MessageComp*) subj) { }

PullMenuComp* PullMenuCode::GetPullMenuComp () { 
    return (PullMenuComp*) GetSubject(); 
}

boolean PullMenuCode::Definition (ostream& out) {
    boolean ok = true;
    Iterator i;

    if (
	_emitProperty || _emitInstanceDecls || 
        _emitForward || _emitClassHeaders || _emitHeaders
    ) {
        ok = ok && CodeView::Definition(out);
        ok = ok && Iterate(out);

    } else if (_emitExpHeader) {
	InteractorComp* icomp = GetIntComp();
	MemberNameVar* mnamer = icomp->GetMemberNameVar();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        if (!snamer->IsSubclass()) {
            if (_scope && mnamer->GetExport() && !_namelist->Search("menu")) {
                _namelist->Append("menu");
                out << "#include <InterViews/menu.h>\n";
            }
        } else {
            ok = ok && CodeView::Definition(out);
        }
        ok = ok && Iterate(out);

    } else if (_emitCorehHeader) {
	InteractorComp* icomp = GetIntComp();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        const char* subclass = snamer->GetName();
        if (snamer->IsSubclass() && strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("menu")) {
                _namelist->Append("menu");
                out << "#include <InterViews/menu.h>\n";
            }
        } else {
            ok = ok && Iterate(out);
        }
    } else if (_emitInstanceInits) {
        PullMenuComp* pdcomp = GetPullMenuComp();
        const char* mname = pdcomp->GetMemberNameVar()->GetName();
        const char* instanceName = pdcomp->GetInstanceNameVar()->GetName();

        ok = ok && Iterate(out);
        if (AllKidsDefined()) {
            if (!_instancelist->Find((void*) mname)) {
                _instancelist->Append(new UList((void*)mname));

                BeginInstantiate(out);
		MenuItemGraphic* menuGraphic = pdcomp->GetMenuItemGraphic();
		const char* text = menuGraphic->GetText();
                Alignment a = menuGraphic->GetAlignment();
                Shape* shape = pdcomp->GetShapeVar()->GetShape();

                out << "(";
                InstanceName(out);
                out << "new Message(";
                out << "\"" << text << "\", ";
                ok = ok && Align(a, out);
                out << ", 2, " << shape->hstretch << ", " << shape->vstretch;
                out << "))";
                EndInstantiate(out);


		MenuBodyComp* mbc = (MenuBodyComp*) pdcomp->GetMenuBody();
		    
                for ( mbc->First(i); !mbc->Done(i); mbc->Next(i)) {
                    InteractorComp* kid = (InteractorComp*) mbc->GetComp(i);
                    const char* instance =kid->GetMemberNameVar()->GetName();
		    out << "    ";
		    if (
                        kid->IsA(MENUITEM_COMP) || kid->IsA(PULLMENU_COMP) ||
                        kid->IsA(COMMANDCONTROL_COMP)
                    ) {
                        out << mname << "->Include(";

		    } else {
                        out << mname << "->GetScene()->Insert(";

		    }
                    out << instance << ");\n";
                }
            }
	}
    } else if (
	_emitBSDecls || _emitBSInits || 
	_emitFunctionDecls || _emitFunctionInits ||
        _emitCreatorHeader || _emitCreatorSubj || _emitCreatorView
    ) {
        ok = ok && Iterate(out);

    } else if (
        _emitCoreDecls || _emitCoreInits || _emitClassDecls || _emitClassInits
    ) {
	ok = ok && CodeView::Definition(out);
        ok = ok && Iterate(out);
        
    } else if (_emitMain) {
	ok = ok && CodeView::Definition(out);
        
    }
    return ok;
}

boolean PullMenuCode::CoreConstDecls(ostream& out) { 
    out << "(const char*, Interactor* i);\n";
    return out.good();
}

boolean PullMenuCode::CoreConstInits(ostream& out) {
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

boolean PullMenuCode::ConstDecls(ostream& out) {
    out << "(const char*, Interactor* i);\n";
    return out.good();
}

boolean PullMenuCode::ConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n    const char* name, Interactor* i\n) : " << coreclass;
    out << "(name, i) {}\n\n";
    return out.good();
}

boolean PullMenuCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();

    if (!snamer->IsSubclass() && !_namelist->Search("menu")) {
        _namelist->Append("menu");
        out << "#include <InterViews/menu.h> \n";
    }
    if (
        strcmp(snamer->GetName(), _classname) != 0 && 
        !_namelist->Search("message")
    ) {
        _namelist->Append("message");
        out << "#include <InterViews/message.h> \n";
    }
    return out.good();
}

/*****************************************************************************/

MenuItemGraphic::MenuItemGraphic (
    const char* text, CanvasVar* c, Graphic* g, Alignment a
) : MessageGraphic(text, c, g, a, 2) {}

Graphic* MenuItemGraphic::Copy () {
    return new MenuItemGraphic(GetText(), nil, this);

}
const char* MenuItemGraphic::GetClassName () { return "MenuItem"; }
ClassId MenuItemGraphic::GetClassId () { return MENUITEM_GRAPHIC; }

/*****************************************************************************/

PDMenuGraphic::PDMenuGraphic (
    const char* text, CanvasVar* c, Graphic* g, Alignment a
) : MenuItemGraphic(text, c, g, a) { }

Graphic* PDMenuGraphic::Copy () {
    return new PDMenuGraphic(GetText(), nil, this);
}

const char* PDMenuGraphic::GetClassName () { return "PulldownMenu"; }
ClassId PDMenuGraphic::GetClassId () { return PDMENU_GRAPHIC; }

/*****************************************************************************/

PRMenuGraphic::PRMenuGraphic (
    const char* text, CanvasVar* c, Graphic* g, Alignment a
) : MenuItemGraphic(text, c, g, a) { }

Graphic* PRMenuGraphic::Copy () {
    return new PRMenuGraphic(GetText(), nil, this);
}

const char* PRMenuGraphic::GetClassName () { return "PullrightMenu"; }
ClassId PRMenuGraphic::GetClassId () { return PRMENU_GRAPHIC; }

