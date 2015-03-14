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
 * Graphical component definitions.
 */

#include "ibclasses.h"
#include "ibcmds.h"
#include "ibcreator.h"
#include "ibdialogs.h"
#include "ibcomp.h"
#include "ibgrblock.h"
#include "ibmanips.h"
#include "ibscene.h"
#include "ibtools.h"
#include "ibvars.h"
#include "ibvarviews.h"

#include <Unidraw/catalog.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/selection.h>
#include <Unidraw/statevars.h>
#include <Unidraw/iterator.h>
#include <Unidraw/ulist.h>
#include <Unidraw/viewer.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/grview.h>
#include <Unidraw/Commands/edit.h>
#include <Unidraw/Graphic/picture.h>

#include <InterViews/event.h>
#include <InterViews/transformer.h>

#include <stdio.h>
#include <stdlib.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/
static const char* icomp_delim = "%icomp_delim";
static const char tag = '#';
boolean IComp::_release = 1;
/*****************************************************************************/

IComp::IComp (Graphic* gr) : GraphicComps (gr) {
    _gclassNameVar = new SubclassNameVar("Picture", false);
    _gclassNameVar->ref();
    _cclassNameVar = new SubclassNameVar("GraphicComps", false);
    _cclassNameVar->ref();
    _vclassNameVar = new SubclassNameVar("GraphicViews", false);
    _vclassNameVar->ref();
    _compid = new IDVar;
    _memberVar = nil;
    _target = nil;

    if (gr == nil) {
        SetGraphic(new Picture);
    }
}

IComp::~IComp () {
    delete _memberVar;
    delete _compid;
    _gclassNameVar->unref();
    _cclassNameVar->unref();
    _vclassNameVar->unref();
}

ClassId IComp::GetClassId () {
    ClassId iclass = GRAPHIC_COMPS;
    GraphicComp* ikid = GetTarget();
    if (ikid != nil) {
        iclass = ikid->GetClassId();
    }
    return iclass;
}

ClassId IComp::GetSubstId(const char*& delim) {
    delim = icomp_delim;
    return GraphicComps::GetClassId();
}

boolean IComp::IsA (ClassId id) {
    GraphicComp* ikid = GetTarget();
    return (ikid == nil) ? GraphicComps::IsA(id) : ikid->IsA(id);
}

IComp& IComp::operator = (IComp& icomp) {
    _gclassNameVar->unref();
    _cclassNameVar->unref();
    _vclassNameVar->unref();

    _gclassNameVar = icomp.GetGClassNameVar();
    _cclassNameVar = icomp.GetCClassNameVar();
    _vclassNameVar = icomp.GetVClassNameVar();

    _gclassNameVar->ref();
    _cclassNameVar->ref();
    _vclassNameVar->ref();

    *_memberVar = *icomp.GetMemberNameVar();
    *(IBNameVar*)_compid = *(IBNameVar*)icomp.GetCIDVar();

    return *this;
}

void IComp::Instantiate() {
    Catalog* catalog = unidraw->GetCatalog();
    if (_memberVar == nil) {
	char buf[CHARBUFSIZE];
	sprintf(buf, "_%s_0", GetClassNameVar()->GetName());
	_memberVar = new MemberNameVar(buf);
        _memberVar->GenNewName();
    }
    catalog->Forget(this);    /* this is magic */
}

void IComp::SetTarget (GraphicComp* grcomp) {
    GetTarget();
    if (_target != nil) {
        Remove(_target);
        delete _target;
    }
    Append(grcomp);
    _target = grcomp;
}

GraphicComp* IComp::GetTarget () {
    if (_target == nil) {
        Iterator i;
        First(i);
        _target = GetComp(i);
    }
    return _target;
}
        
void IComp::Interpret (Command* cmd) {
    if (cmd->IsA(GETFIREWALL_CMD) || cmd->IsA(GETTOPLEVEL_CMD)) {
        GetParent()->Interpret(cmd);

    } else if (cmd->IsA(GETCONFLICT_CMD)) {
        GetConflictCmd* gcmd = (GetConflictCmd*) cmd;
        const char* cname = gcmd->GetCName();
        UList* conflictlist = gcmd->GetConflict();
        const char* member = _memberVar->GetName();
        const char* classname = _gclassNameVar->GetName();
        if (strcmp(member, cname) == 0) {
            conflictlist->Append(new UList(_memberVar->GetMemberSharedName()));
        }
        if (strcmp(classname, cname) == 0) {
            conflictlist->Append(new UList(_gclassNameVar));
        }
        if (IsAComponent()) {
            if (strcmp(_cclassNameVar->GetName(), cname) == 0) {
                conflictlist->Append(new UList(_cclassNameVar));
            }
            if (strcmp(_vclassNameVar->GetName(), cname) == 0) {
                conflictlist->Append(new UList(_vclassNameVar));
            }
        }
    } else if (cmd->IsA(COMPCHECK_CMD)) {
        CompCheckCmd* ccmd = (CompCheckCmd*) cmd;
        IComp* icomp = ccmd->GetTarget();
        if (icomp != this) {
            if (strcmp(ccmd->GetCName(), _cclassNameVar->GetName()) == 0) {
                if (
                    strcmp(ccmd->GetVName(), _vclassNameVar->GetName()) != 0 ||
                    strcmp(ccmd->GetGName(), _gclassNameVar->GetName()) != 0 
                ) {
                    ccmd->Check(false);
                }
            }
        }
    } else if (cmd->IsA(INFO_CMD)) {
        
    } else if (cmd->IsA(GETNAMEVARS_CMD)) {
        GetNameVarsCmd* gcmd = (GetNameVarsCmd*) cmd;
        gcmd->SetClassNameVar(GetClassNameVar());
        gcmd->SetMemberNameVar(_memberVar);
        if (IsAComponent()) {
            gcmd->AppendExtras(_gclassNameVar);
            gcmd->AppendExtras(_vclassNameVar);
        }

    } else if (cmd->IsA(SCAN_CMD)) {
        ScanCmd* scmd = (ScanCmd*) cmd;
        const char* sclass = scmd->GetClassName();
        if (*sclass == '\0' || scmd->GetScope()) {
            if (IsA(scmd->GetTargetId())) {
                scmd->SetSucceeded(true);
            }
        }
    } else if (cmd->IsA(EDIT_CMD)) {
        EditCmd* editcmd = (EditCmd*) cmd;
        Remove(GetTarget());
        _target = editcmd->SwapComp(_target);
        Append(_target);
        Notify();
        Propagate(cmd);

    } else if (cmd->IsA(IDMAP_CMD)) {
        if (IsAComponent()) {
            IDMap* idmap = IDVar::GetIDMap();
            idmap->Add(_compid, _cclassNameVar);
        }
    } else if (
        !cmd->IsA(SCENE_CMD) && !cmd->IsA(MONOSCENE_CMD) && 
        !cmd->IsA(NAVIGATE_CMD)
    ) {
        GraphicComp::Interpret(cmd);
        Propagate(cmd);
    }
}

void IComp::Uninterpret (Command* cmd) {
    if (cmd->IsA(EDIT_CMD)) {
        EditCmd* editcmd = (EditCmd*) cmd;
        Remove(GetTarget());
        _target = editcmd->SwapComp(_target);
        Append(_target);
        Notify();
        Unpropagate(cmd);

    } else if (cmd->IsA(INFO_CMD)) {
        
    } else if (
        !cmd->IsA(SCENE_CMD) && !cmd->IsA(MONOSCENE_CMD) && 
        !cmd->IsA(NAVIGATE_CMD)
    ) {
        GraphicComp::Uninterpret(cmd);
        Unpropagate(cmd);
    }
}

void IComp::Propagate (Command* cmd) {
    Clipboard* cb = cmd->GetClipboard();
    if (
        (cb != nil && cb->Includes(this)) ||
        cmd->GetEditor()->GetComponent() == (Component*) this
    ) {
        GetFirewallCmd firewallCmd(this);
        firewallCmd.Execute();
        InteractorComp* firewall = firewallCmd.GetFirewall();
        if (firewall != nil && firewall->IsANewScope()) {
            MonoSceneClass* mfirewall = (MonoSceneClass*) firewall;
            mfirewall->Clone(cmd);
        }
    } 
}

void IComp::Unpropagate (Command* cmd) {
    Clipboard* cb = cmd->GetClipboard();
    if (
        (cb != nil && cb->Includes(this)) ||
        cmd->GetEditor()->GetComponent() == (Component*) this
    ) {
        GetFirewallCmd firewallCmd(this);
        firewallCmd.Execute();
        InteractorComp* firewall = firewallCmd.GetFirewall();
        if (firewall != nil && firewall->IsANewScope()) {
            MonoSceneClass* mfirewall = (MonoSceneClass*) firewall;
            mfirewall->UnClone(cmd);
        }
    }
}

GrBlockComp* IComp::GetGrBlockComp () {
    Component* parent = GetParent();
    if (parent != nil && !parent->IsA(INTERACTOR_COMP)) {
        IComp* iparent = (IComp*) parent;
        parent = iparent->GetGrBlockComp();
    }
    return (GrBlockComp*) parent;
}

void IComp::SetState(const char* name, StateVar* stateVar) { 
    if (strcmp(name, "ClassNameVar") == 0) {
        SubclassNameVar* classNameVar = (SubclassNameVar*) stateVar;
        classNameVar->ref();
        GetClassNameVar()->unref();
        if (IsAComponent()) {
            _cclassNameVar = classNameVar;
            _cclassNameVar->ref();
        } else {
            _gclassNameVar = classNameVar;
            _gclassNameVar->ref();
        }
    } else if (strcmp(name, "MemberNameVar") == 0) {
        MemberNameVar* memberVar = (MemberNameVar*) stateVar;
        *_memberVar = *memberVar;
    }
}

StateVar* IComp::GetState (const char* name) {
    StateVar* stateVar = nil;

    if (strcmp(name, "ClassNameVar") == 0) {
        stateVar = GetClassNameVar();

    } else if (strcmp(name, "MemberNameVar") == 0) {
        stateVar = _memberVar;
    }

    return stateVar;
}

boolean IComp::IsAComponent () {
    boolean flag = false;
    Component* grbcomp = GetGrBlockComp();
    if (
        grbcomp != nil &&
        (
            grbcomp->IsA(IBVIEWER_COMP) || grbcomp->IsA(PANELCONTROL_COMP) ||
            grbcomp->IsA(COMMANDCONTROL_COMP) || grbcomp->IsA(EDITOR_COMP)
        )
    ) {
        flag = true;
    }
    return flag;
}
        
SubclassNameVar* IComp::GetClassNameVar () {
    if (IsAComponent()) {
        return _cclassNameVar;
    } else {
        return _gclassNameVar;
    }
}

void IComp::Read (istream& in) {
    if (_release) {
        GraphicComps::Read(in);
        ReadStateVars(in);
    } else {
        GetTarget()->Read(in);
        Append(_target);
        ReadStateVars(in);
    }
}

void IComp::Write (ostream& out) {
    if (_release) {
        GraphicComps::Write(out);
        WriteStateVars(out);
    } else {
        GetGraphic()->Bequeath();
        GetTarget()->Write(out);
        WriteStateVars(out);
    }
}

void IComp::ReadStateVars(istream& in) {
    char hint;
    Catalog* catalog = unidraw->GetCatalog();
    in >> hint;
    if (hint == tag) {
        delete _memberVar;
        _gclassNameVar->unref();
        _gclassNameVar = (SubclassNameVar*) catalog->ReadStateVar(in);
        _memberVar = (MemberNameVar*) catalog->ReadStateVar(in);
        _gclassNameVar->ref();

        float version = unidraw->GetCatalog()->FileVersion();
        if (version > 1.05 || !IBCreator::GetLock()) {
            _cclassNameVar->unref();
            _cclassNameVar = (SubclassNameVar*) catalog->ReadStateVar(in);
            _cclassNameVar->ref();
            _vclassNameVar->unref();
            _vclassNameVar = (SubclassNameVar*) catalog->ReadStateVar(in);
            _vclassNameVar->ref();
            int origid = _compid->GetOrigID();
            delete _compid;
            _compid = (IDVar*) catalog->ReadStateVar(in);
            _compid->SetOrigID(origid);

        }
    } else {
        in.putback(hint);
    }
}

void IComp::WriteStateVars(ostream& out) {
    Catalog* catalog = unidraw->GetCatalog();
    out << " " << tag << " ";
    catalog->WriteStateVar(_gclassNameVar, out);
    catalog->WriteStateVar(_memberVar, out);
    catalog->WriteStateVar(_cclassNameVar, out);
    catalog->WriteStateVar(_vclassNameVar, out);
    catalog->WriteStateVar(_compid, out);
}

/*****************************************************************************/

IView::IView (IComp* subj) : GraphicViews (subj) {}

IComp* IView::GetIComp () { return (IComp*) GetSubject(); }

InfoDialog* IView::GetInfoDialog () {
    InfoDialog* info;
    IComp* icomp = GetIComp();
    boolean iscomp = icomp->IsAComponent();
    
    if (iscomp) {
        info = new InfoDialog("Component Information");
    } else {
        info = new InfoDialog("Graphic Information");
    }
    ButtonState* state = info->GetState();
    SubclassNameVar* classNameVar = icomp->GetClassNameVar();
    MemberNameVar* memberVar = icomp->GetMemberNameVar();

    if (iscomp) {
        IDVar* cidvar = icomp->GetCIDVar();
        SubclassNameVar* vclassNameVar = icomp->GetVClassNameVar();
        SubclassNameVar* gclassNameVar = icomp->GetGClassNameVar();

        ICompNameVarView* s = new ICompNameVarView(
            classNameVar, state, icomp, "Component"
        );
        SubclassNameVarView* v = new SubclassNameVarView(
            vclassNameVar, state, icomp, "ComponentView"
        );
        SubclassNameVarView* g = new SubclassNameVarView(
            gclassNameVar, state, icomp, "Graphic"
        );
        s->SetViewNameVarView(v);
        s->SetGraphicNameVarView(g);
        
        IDVarView* i = new IDVarView(cidvar, state, "Component");
        s->SetIDVarView(i);

        info->Include(s);
        info->Include(i);
        info->Include(new MemberNameVarView(memberVar, state, icomp));
        info->Include(v);
        info->Include(g);

   } else {
        info->Include(
            new SubclassNameVarView(classNameVar, state, icomp, "Graphic")
        );
        info->Include(new MemberNameVarView(memberVar, state, icomp));
    }
    return info;
}

GraphicView* IView::GetKidView () {
    Iterator i;
    First(i);
    return GetView(i);
}

void IView::DrawHandles () {
    GraphicView* kidview = GetKidView();
    if (kidview != nil) {
        kidview->DrawHandles();
    }
}

void IView::RedrawHandles () {
    GraphicView* kidview = GetKidView();
    if (kidview != nil) {
        kidview->RedrawHandles();
    }
}

void IView::EraseHandles () {
    GraphicView* kidview = GetKidView();
    if (kidview != nil) {
        kidview->EraseHandles();
    }
}

void IView::InitHandles () {
    GraphicView* kidview = GetKidView();
    if (kidview != nil) {
        kidview->InitHandles();
    }
}

Manipulator* IView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Manipulator* m = nil;
    GraphicView* kidview = GetKidView();
    
    if (tool->IsA(EXAMINE_TOOL)) {
        Editor* ed = v->GetEditor();
        ExamineMenu* popup = new ExamineMenu;
        ExamineTool* etool = (ExamineTool*) tool;
        Selection* selPath = etool->GetSelPath();

        Control* info = etool->CreateInfoEntry(selPath, ed);
        Control* att = etool->CreatePropsEntry(selPath, ed);
        if (info != nil) {
            popup->Include(info);
        } else {
            return nil;
        }
        
        if (att != nil) {
            popup->Include(att);
        }
        boolean success = false;
        GraphicComp* edcomp = (GraphicComp*) ed->GetComponent();
        GraphicComp* subject = GetIComp();
        GraphicComp* grblock = ((IComp*) subject)->GetGrBlockComp();

        for(;;) {
            if (subject == edcomp) {
                break;
            } else if (subject == grblock) {
                success = true;
                break;
            }
            subject = (GraphicComp*) subject->GetParent();
        }
        if (success) {
            popup->Include(
                new CommandItem(
                    " Graphics ", Center, new IdrawCmd(
                        ed, GetIComp()->GetGrBlockComp()
                    )
                )
            );
        }
        m = new PopupManip(v, popup, tool);

    } else if (
        tool->IsA(MOVE_TOOL) || tool->IsA(SCALE_TOOL) ||
        tool->IsA(STRETCH_TOOL) || tool->IsA(ROTATE_TOOL)
    ) {
        m = GraphicViews::CreateManipulator(v, e, rel, tool);

    } else if (kidview != nil) {
        Transformer* t = GetGraphic()->GetTransformer();
        if (t != nil) {
            rel->Premultiply(t);
        }
        m = kidview->CreateManipulator(v, e, rel, tool);

    } else {
        m = GraphicViews::CreateManipulator(v, e, rel, tool);
    }
    return m;
        
}

Command* IView::InterpretManipulator (Manipulator* m) {
    Editor* ed = m->GetViewer()->GetEditor();
    Tool* tool = m->GetTool();
    GraphicView* kidview = GetKidView();
    Command* cmd = nil;

    if (tool->IsA(RESHAPE_TOOL)) {
        cmd = kidview->InterpretManipulator(m);
        if (cmd != nil && cmd->IsA(REPLACE_CMD)) {
            ReplaceCmd* rcmd = (ReplaceCmd*) cmd;
            GraphicComp* replacee=(GraphicComp*)rcmd->GetReplacement()->Copy();
            delete rcmd;
            cmd = new EditCmd(ed, new Clipboard(GetIComp()), replacee);

        } else {
            delete cmd;
            cmd = nil;
        }
    } else if (
        tool->IsA(MOVE_TOOL) || tool->IsA(SCALE_TOOL) ||
        tool->IsA(STRETCH_TOOL) || tool->IsA(ROTATE_TOOL)
    ) {
        cmd = GraphicViews::InterpretManipulator(m);

    } else if (kidview != nil) {
        cmd = kidview->InterpretManipulator(m);
        if (cmd != nil && cmd->IsA(PASTE_CMD)) {
            Iterator i;
            Catalog* catalog = unidraw->GetCatalog();
            ClassId id = GetIComp()->GetClassId();
            Clipboard* cb = cmd->GetClipboard();
            cb->First(i);
            GraphicComp* grcomp = cb->GetComp(i);
            cb->Remove(i);
            IComp* icomp = (IComp*) catalog->GetCreator()->Create(id);
            icomp->SetTarget(grcomp); 
            cb->Append(icomp);
        }
    } else {
        cmd = GraphicViews::InterpretManipulator(m);
    }

    return cmd;
}

Selection* IView::SelectAll() { return new Selection; }
Selection* IView::ViewContaining(Coord, Coord) { return new Selection; }
Selection* IView::ViewsContaining(Coord, Coord) { return new Selection; }

Selection* IView::ViewIntersecting(Coord, Coord, Coord, Coord) {
    return new Selection; 
}
Selection* IView::ViewsIntersecting(Coord, Coord, Coord, Coord) {
    return new Selection; 
}
Selection* IView::ViewsWithin(Coord, Coord, Coord, Coord) {
    return new Selection; 
}

void IView::Interpret(Command* cmd) {
    if (cmd->IsA(INFO_CMD)) {
        InfoCmd* info = (InfoCmd*) cmd;
        info->SetInfoDialog(GetInfoDialog());
        
    } else {
        GraphicViews::Interpret(cmd);
    }
}

/*****************************************************************************/

IPSView::IPSView (GraphicComps* icomp) : PostScriptViews (icomp) {
    _kidps = nil;
}

void IPSView::Update () {
    Iterator i;
    PostScriptViews::Update();
    First(i);
    _kidps = GetView(i);
}

boolean IPSView::Emit (ostream& out) {
    return _kidps->Emit(out);
}

boolean IPSView::Definition (ostream& out) {
    return _kidps->Definition(out);
}

ClassId IPSView::GetClassId () { return IPS_VIEW; }

boolean IPSView::IsA (ClassId id) {
    return IPS_VIEW == id || PostScriptViews::IsA(id);
}



