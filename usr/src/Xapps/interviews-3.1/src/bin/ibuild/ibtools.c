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
 * Implementation of user interface builder-specific tools.
 */

#include "ibadjuster.h"
#include "ibbutton.h"
#include "ibclasses.h"
#include "ibcmds.h"
#include "ibinteractor.h"
#include "ibgrblock.h"
#include "ibgrcomp.h"
#include "ibmanips.h"
#include "ibscroller.h"
#include "ibtools.h"
#include "ibvars.h"

#include <Unidraw/iterator.h>
#include <Unidraw/editor.h>
#include <Unidraw/manips.h>
#include <Unidraw/selection.h>
#include <Unidraw/viewer.h>
#include <Unidraw/Components/grview.h>

#include <InterViews/event.h>
#include <InterViews/message.h>
#include <InterViews/shape.h>
#include <string.h>

/*****************************************************************************/

CommandItem::CommandItem(
    const char* str, Alignment al, Command* cmd
) : MenuItem(str, al) {
    _executed = false;
    _cmd = cmd;
}

CommandItem::~CommandItem() {
    if (!_executed) {
	delete _cmd;
    }
}

void CommandItem::Do () {
    _executed = true; 
    _cmd->Execute(); 
    if (_cmd->Reversible()) {
        _cmd->Log();
    } else {
        delete _cmd;
    }
}

/*****************************************************************************/

void ComputeViewPath (Event& e, GraphicView* views, Selection* s){
    Selection* newSel =
        views->ViewIntersecting(e.x-SLOP, e.y-SLOP, e.x+SLOP, e.y+SLOP);
    
    Iterator i;
    if (newSel != nil) {
        for (newSel->First(i); !newSel->Done(i); newSel->Next(i)) {
            GraphicView* gv = newSel->GetView(i);
            if (gv != nil) {
                s->Append(gv);
                ComputeViewPath(e, gv, s);
            }
        }
        delete newSel;
    }
}

const char* GetName (GraphicComp* comp) {
    SubclassNameVar* snamer = (SubclassNameVar*)comp->GetState(
        "ClassNameVar"
    );
    return snamer->GetName();
}

const char* GetName (GraphicComp* comp, boolean shift, boolean mname) {
    static char Name[CHARBUFSIZE];
    SubclassNameVar* snamer = (SubclassNameVar*)comp->GetState(
        "ClassNameVar"
    );
    strcpy(Name, snamer->GetName());
    if (shift) {
        strcat(Name, " ");
        if (mname) {
            MemberNameVar* mnamer = (MemberNameVar*) comp->GetState(
                "MemberNameVar"
            );
            strcat(Name, mnamer->GetName());
        } else {
            InstanceNameVar* inamer = (InstanceNameVar*) comp->GetState(
                "InstanceNameVar"
            );
            strcat(Name, inamer->GetName());
        }
    }
    return Name;
}

/*****************************************************************************/

TabTool::TabTool () {} 

Manipulator* TabTool::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel
) {
    Iterator i;
    Selection* s = v->GetSelection();
    s->First(i);
    GraphicView* gv = s->GetView(i);
    return gv->CreateManipulator(v, e, rel, this);
}


ClassId TabTool::GetClassId () { return TAB_TOOL; }
boolean TabTool::IsA (ClassId id) {return TAB_TOOL==id||ReshapeTool::IsA(id);}

/*****************************************************************************/

ClassId ExamineTool::GetClassId () { return EXAMINE_TOOL; }
boolean ExamineTool::IsA (ClassId id) {return EXAMINE_TOOL==id||Tool::IsA(id);}

ExamineTool::ExamineTool (ControlInfo* m) : Tool(m) { _selPath = nil; }

ExamineTool::~ExamineTool () { delete _selPath; }

Tool* ExamineTool::Copy () { return new ExamineTool(CopyControlInfo()); }

Manipulator* ExamineTool::CreateManipulator (
    Viewer* v, Event& e, Transformer* t
){
    GraphicView* views = v->GetGraphicView();
    Selection* s = v->GetSelection();
    _selPath = new Selection;
    Manipulator* m = nil;

    s->Clear();
    ComputeViewPath(e, views, _selPath);
    _shift = e.shift_is_down();

    if (!_selPath->IsEmpty()) {
        Iterator i;
        _selPath->First(i);
	GraphicView* gv = _selPath->GetView(i);
        s->Append(gv);
        s->Update();
        _selPath->Last(i);
	gv = _selPath->GetView(i);
        
        m = gv->CreateManipulator(v, e, t, this);
    }
    delete _selPath;
    return m;
}

Control* ExamineTool::CreateInfoEntry (Selection* s, Editor* ed) {
    Control* ctrl = nil;
    Iterator i;

    if (s->Number() > 1) {
        Menu* m = new H_PullrightMenu(new Message("Info...", Center, 2, hfil));
        ctrl = m;

        for (s->First(i); !s->Done(i); s->Next(i)) {
	    GraphicView* view = (GraphicView*) s->GetView(i);
            GraphicComp* comp = view->GetGraphicComp();
            InfoCmd* cmd = new InfoCmd(ed, view);
            m->Include(
                new CommandItem(GetName(comp, _shift, true), Center, cmd)
            );
        }
    } else {
	s->First(i);
	GraphicView* view = (GraphicView*) s->GetView(i);
        ctrl = new CommandItem("Info...", Center, new InfoCmd(ed, view));
    }
    return ctrl;
}


Control* ExamineTool::CreatePropsEntry (Selection* s, Editor* ed) {
    Control* ctrl = nil;
    Iterator i;

    if (s->Number() > 1) {
        for (s->First(i); !s->Done(i); s->Next(i)) {
            GraphicComp* comp = s->GetView(i)->GetGraphicComp();
            if (comp->IsA(INTERACTOR_COMP)) {
                if (ctrl == nil) {
                    ctrl = new H_PullrightMenu(
                        new Message("Props...", Center, 2, hfil)
                    );
                }
                Menu* m = (Menu*) ctrl;
                PropsCmd* cmd = new PropsCmd(ed, (InteractorComp*) comp);
                m->Include(
                    new CommandItem(GetName(comp, _shift, false), Center, cmd)
                );
            }
        }
    } else {
	s->First(i);
        GraphicComp* comp = s->GetView(i)->GetGraphicComp();
        if (comp->IsA(INTERACTOR_COMP)) {
            ctrl = new CommandItem(
                "Props...", Center, new PropsCmd(ed, (InteractorComp*) comp)
            );
        }
    }
    return ctrl;
}

/*****************************************************************************/

ClassId NarrowTool::GetClassId () { return NARROW_TOOL; }
boolean NarrowTool::IsA (ClassId id) {return NARROW_TOOL==id||Tool::IsA(id);}

NarrowTool::NarrowTool (ControlInfo* m) : Tool(m) { _popup = false; }

Tool* NarrowTool::Copy () { return new NarrowTool(CopyControlInfo()); }

Manipulator* NarrowTool::CreateManipulator (Viewer* v, Event& e,Transformer*){
    Manipulator* m = nil;
    _popup = false;
    GraphicView* views = v->GetGraphicView();
    if (!e.shift_is_down()) {
        Selection* s = v->GetSelection(), *newSel = new Selection;
        
        s->Clear();
        ComputeViewPath(e, views, newSel);
        
        if (!newSel->IsEmpty()) {
            Iterator i;
            newSel->First(i);
            GraphicView* gv = newSel->GetView(i);
            
            s->Append(gv);
            s->Update();

            m = CreatePopupManip(newSel, v);
            _popup = true;
        }
        
        delete newSel;
    } else {
        m = new NarrowManip(v);
    }
    return m;
}

Command* NarrowTool::InterpretManipulator(Manipulator* manip) {
    Command* ncmd = nil;

    if (!_popup) {
        Iterator i;
        NarrowManip* nmanip = (NarrowManip*) manip;
        Editor* ed = nmanip->GetViewer()->GetEditor();
        GraphicComp* parent = nmanip->GetParent();
        if (
            parent != nil && 
            (parent->IsA(SCENE_COMP) || parent->IsA(IGRAPHIC_COMPS))
        ){
            ncmd = new NavigateCmd(ed, false, parent, nmanip->GetKid());
        }
    }
    return ncmd;
}

static void RecurPopupInclude(
    Editor* ed, PopupMenu* popup, GraphicComp* kid
) {
    GraphicComp* parent = (GraphicComp*) kid->GetParent();
    if (parent != nil) {
        RecurPopupInclude(ed, popup, parent);
        NavigateCmd* cmd = new NavigateCmd(ed, false, parent, kid);
        popup->Include(new CommandItem(GetName(parent), Center, cmd));
    }
}

Control* NarrowTool::CreateViewEntry (Selection* s, Editor* ed) {
    Control* ctrl = nil;
    Iterator i;

    if (s->Number() >= 1) {
        H_PopupMenu* popup = new H_PopupMenu;
        ctrl = popup;

        s->First(i);
        GraphicComp* kid = s->GetView(i)->GetGraphicComp();
        RecurPopupInclude(ed, popup, kid);
        popup->LockPosition();

        for (;!s->Done(i); s->Next(i)) {
            GraphicView* gv = s->GetView(i);
            GraphicComp* parent = gv->GetGraphicComp();
            if (parent->IsA(SCENE_COMP) || parent->IsA(IGRAPHIC_COMPS)){
                GraphicComp* mykid = nil;
                Iterator j(i);
                s->Next(j);
                if (!s->Done(j)) {
                    mykid = (GraphicComp*) s->GetView(j)->GetGraphicComp();
                }
                NavigateCmd* cmd = new NavigateCmd(ed, false, parent, mykid);
                popup->Include(new CommandItem(GetName(parent), Center, cmd));
            }
        }
    }
    return ctrl;
}

Manipulator* NarrowTool::CreatePopupManip (Selection* s, Viewer* v) {
    Manipulator* retval = nil;
    Editor* ed = v->GetEditor();
    Control* view = CreateViewEntry(s, ed);

    if (view != nil) {
        retval = new PopupManip(v, (PopupMenu*) view, this);
    }
    return retval;
}

/*****************************************************************************/

ClassId RelateTool::GetClassId () { return RELATE_TOOL; }
boolean RelateTool::IsA (ClassId id) {return RELATE_TOOL==id||Tool::IsA(id);}

RelateTool::RelateTool (ControlInfo* m) : Tool(m) { }
Tool* RelateTool::Copy () { return new RelateTool(CopyControlInfo()); }

Manipulator* RelateTool::CreateManipulator (Viewer* v, Event&, Transformer*){
    return new RelateManip(v, this);
}

Command* RelateTool::InterpretManipulator(Manipulator* m) {
    Command* cmd = nil;
    InteractorComp* src, *dest;
    RelateManip* rm = (RelateManip*) m;
    Editor* ed = rm->GetViewer()->GetEditor();
    rm->GetSrcDest(src, dest);
    if (src != nil) {
        cmd = new RelateCmd(ed, src, dest);
    }
    return cmd;
}

/*****************************************************************************/

ClassId IBGraphicCompTool::GetClassId () { return IBGRAPHIC_COMP_TOOL; }

boolean IBGraphicCompTool::IsA (ClassId id) {
    return IBGRAPHIC_COMP_TOOL == id || GraphicCompTool::IsA(id);
}

IBGraphicCompTool::IBGraphicCompTool () { }

IBGraphicCompTool::IBGraphicCompTool (
    ControlInfo* m, GraphicComp* proto
) : GraphicCompTool(m, proto) { }

Tool* IBGraphicCompTool::Copy () {
    return new IBGraphicCompTool(CopyControlInfo(), GetPrototype());
}
