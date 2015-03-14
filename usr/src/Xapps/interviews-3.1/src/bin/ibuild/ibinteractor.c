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
 * Interactor component definitions.
 */

#include "ibclasses.h"
#include "ibcmds.h"
#include "ibdialogs.h"
#include "ibed.h"
#include "ibframe.h"
#include "ibgraphic.h"
#include "ibglobals.h"
#include "ibinteractor.h"
#include "ibmanips.h"
#include "ibtools.h"
#include "ibvars.h"
#include "ibvarviews.h"

#include <Unidraw/catalog.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/iterator.h>
#include <Unidraw/manips.h>
#include <Unidraw/selection.h>
#include <Unidraw/statevars.h>
#include <Unidraw/ulist.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/brushcmd.h>
#include <Unidraw/Commands/colorcmd.h>
#include <Unidraw/Commands/edit.h>
#include <Unidraw/Commands/font.h>
#include <Unidraw/Commands/macro.h>
#include <Unidraw/Commands/patcmd.h>
#include <Unidraw/Commands/transforms.h>
#include <Unidraw/Graphic/geomobjs.h>
#include <Unidraw/Graphic/graphic.h>
#include <Unidraw/Graphic/pspaint.h>
#include <Unidraw/Tools/tool.h>

#include <InterViews/button.h>
#include <InterViews/event.h>
#include <InterViews/rubrect.h>
#include <InterViews/shape.h>
#include <InterViews/transformer.h>

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/

lbrtData::lbrtData (InteractorComp* icomp) : VoidData(nil) {
    float gcx, gcy;
    CanvasVar* ivar = icomp->GetCanvasVar();
    int bodywidth = ivar->Width();
    int bodyheight = ivar->Height();
    icomp->GetGraphic()->GetCenter(gcx, gcy);
    
    _l = round(gcx) - bodywidth/2;
    _b = round(gcy) - bodyheight/2;
    _r = round(gcx) + (bodywidth+1)/2;
    _t = round(gcy) + (bodyheight+1)/2;

    _ibshape = icomp->GetShapeVar()->GetShape()->Copy();
    _tr = new Transformer(icomp->GetGraphic()->GetTransformer());
}

lbrtData::~lbrtData () {
    delete _ibshape;
    delete _tr;
}

class FontData : public lbrtData {
public:
    FontData(InteractorComp*, PSFont*);
};

FontData::FontData (InteractorComp* icomp, PSFont* psfont) : lbrtData(icomp) {
    _void = psfont;
}

/*****************************************************************************/
class PropsData : public Data {
public:
    PropsData(InteractorComp*);
    virtual ~PropsData();
    Props* GetProps();
    StateVar* GetStateVar();
private:
    Props* _props;
    StateVar* _state;
};

PropsData::PropsData (InteractorComp* icomp) {
    _props = icomp->GetProps()->Copy();
    _state = icomp->GetInstanceNameVar()->Copy();
}

PropsData::~PropsData () {
    delete _props;
    delete _state;
}

Props* PropsData::GetProps () { return _props; }
StateVar* PropsData::GetStateVar () { return _state; }

/*****************************************************************************/

Props::Props (const char* text) {
    _props = new char[CHARBUFSIZE];
    _size = CHARBUFSIZE;
    SetPropsText(text);
}

Props::~Props () {
    delete _props;
}

Props* Props::Copy () {
    Props* copy = new Props("");
    copy->SetPropsText(_props);
    return copy;
}

void Props::Read (istream& in) {
    char* props = unidraw->GetCatalog()->ReadString(in);
    in >> _size;
    delete _props;
    _props = new char[_size];
    strcpy(_props, props);
    delete props;
}

void Props::Write(ostream& out) {
    unidraw->GetCatalog()->WriteString(_props, out);
    out << " " << _size << " ";
}

void Props::SetPropsText(const char* text) {
    int len = strlen(text);
    if (len < _size) {
        strcpy(_props, text);
    } else {
        delete _props;
        _props = strnew(text);
        _size = len + 1;
    }
}
/*****************************************************************************/

ClassId InteractorComp::GetClassId () { return INTERACTOR_COMP; }

boolean InteractorComp::IsA (ClassId id) {
    return INTERACTOR_COMP == id || GraphicComps::IsA(id);
}

InteractorComp::InteractorComp (IBGraphic* g) : GraphicComps(g) {
    _classNameVar = new SubclassNameVar("Interactor", false);
    _classNameVar->ref();
    _shapeVar = new ShapeVar(new IBShape);
    _props = new Props("");
    _memberVar = nil;
    _instanceNameVar = nil;
    _canvasVar = nil;
}

InteractorComp::~InteractorComp () {
    delete _canvasVar;
    delete _props;
    delete _shapeVar;
    delete _instanceNameVar;
    delete _memberVar;
    _classNameVar->unref();
}

void InteractorComp::Propagate (Command* cmd) {
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

void InteractorComp::Unpropagate (Command* cmd) {
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

void InteractorComp::Interpret (Command* cmd) {
    Editor* ed = cmd->GetEditor();
    Coord l, b, r, t;

    if (cmd->IsA(PLACE_CMD)) {
	Iterator i;
	PlaceCmd* placeCmd = (PlaceCmd*) cmd;
	Clipboard* cb = placeCmd->GetClipboard();
	Selection* s = ed->GetSelection();

	if (cb == nil) {
            if (s->IsEmpty()) {
                return;
            }
            cmd->SetClipboard(cb = new Clipboard);
            cb->Init(s);
	}

	for (cb->First(i); !cb->Done(i); cb->Next(i)) {
	    InteractorComp* parent = (InteractorComp*) cb->GetComp(i);
	    parent->StoreCanvas(placeCmd);
	    parent->Reconfig();
            if (placeCmd->Placement(l, b, r, t)) {
                Place(parent, l, b, r, t);
            } else {
                Place(parent);
            }
        }
	unidraw->Update();

    } else if (cmd->IsA(INFO_CMD)) {
        
    } else if (cmd->IsA(FONT_CMD)) {
	PSFont* font = ((FontCmd*) cmd)->GetFont();
        
        cmd->Store(this, new FontData(this, GetGraphic()->GetFont()));
        GetGraphic()->SetFont(font);
	Reconfig();
	Place(this);
        Propagate(cmd);

    } else if (cmd->IsA(COLOR_CMD)) {
        GraphicComps::Interpret(cmd);
        Propagate(cmd);

    } else if (cmd->IsA(PROPS_CMD)) {
	PropsData* props = (PropsData*) cmd->Recall(this);
	if (props == nil) {
	    props = new PropsData(this);
	    cmd->Store(this, props);
	} else {
            Props* pclone = _props->Copy();
            InstanceNameVar* sclone = 
                (InstanceNameVar*) _instanceNameVar->Copy();
            Props* porig = props->GetProps();
            InstanceNameVar* sorig = (InstanceNameVar*) props->GetStateVar();

            pclone->SetPropsText(_props->GetPropsText());
            _props->SetPropsText(porig->GetPropsText());
            porig->SetPropsText(pclone->GetPropsText());
            
            *sclone = *_instanceNameVar;
            *_instanceNameVar = *sorig;
            *sorig = *sclone;
            
            delete pclone;
            delete sclone;
            Propagate(cmd);
	}

    } else if (cmd->IsA(GETFIREWALL_CMD)) {
        GetFirewallCmd* gcmd = (GetFirewallCmd*) cmd;
        InteractorComp* parent = (InteractorComp*) GetParent();
        
        if (IsANewScope()) {
            gcmd->SetFirewall(this);
        } else if (parent != nil && parent->IsANewScope()) {
            gcmd->SetFirewall(parent);
        } else if (parent == (InteractorComp*) GetRoot()) {
            gcmd->SetFirewall(this);
        } else if (parent != nil) {
            parent->Interpret(cmd);
        }
        
    } else if (cmd->IsA(GETTOPLEVEL_CMD)) {
        GetTopLevelCmd* gcmd = (GetTopLevelCmd*) cmd;
        InteractorComp* parent = (InteractorComp*) GetParent();

        if (parent == (InteractorComp*) GetRoot()) {
            gcmd->SetTopLevel(this);
        } else if (parent != nil) {
            parent->Interpret(cmd);
        }
        
    } else if (cmd->IsA(GETCONFLICT_CMD)) {
        GetConflictCmd* gcmd = (GetConflictCmd*) cmd;
        const char* cname = gcmd->GetCName();
        UList* conflictlist = gcmd->GetConflict();
        const char* member = nil;
        const char* instance = nil;
        const char* classname = nil;
        if (_memberVar != nil) {
            member = _memberVar->GetName();
        }
        if (_instanceNameVar != nil) {
            instance = _instanceNameVar->GetName();
        }
        if (_classNameVar != nil) {
            classname = _classNameVar->GetName();
        }
        if (member != nil && strcmp(member, cname) == 0) {
            conflictlist->Append(new UList(_memberVar->GetMemberSharedName()));
            gcmd->SetCTarget(this);
        }
        if (instance != nil && strcmp(instance, cname) == 0) {
            conflictlist->Append(new UList(_instanceNameVar));
        }
        if (classname != nil && strcmp(classname, cname) == 0) {
            conflictlist->Append(new UList(_classNameVar));
        }
        
    } else if (cmd->IsA(GETNAMEVARS_CMD)) {
        GetNameVarsCmd* gcmd = (GetNameVarsCmd*) cmd;
        gcmd->SetClassNameVar(_classNameVar);
        gcmd->SetInstanceNameVar(_instanceNameVar);
        gcmd->SetMemberNameVar(_memberVar);
        
    } else if (cmd->IsA(SCAN_CMD)) {
        ScanCmd* scmd = (ScanCmd*) cmd;
        const char* iclass = _classNameVar->GetName();
        const char* sclass = scmd->GetClassName();
        if (*sclass == '\0' || scmd->GetScope()) {
            if (IsA(scmd->GetTargetId())) {
                scmd->SetSucceeded(true);
            }
        }

    } else if (
	!cmd->IsA(GLUEVISIBILITY_CMD) && 
	!cmd->IsA(ALIGN_CMD) && !cmd->IsA(BRUSH_CMD) && 
        !cmd->IsA(UNGROUP_CMD) && !cmd->IsA(GETCLONES_CMD)
    ) {
        GraphicComps::Interpret(cmd);
    }
}

void InteractorComp::Uninterpret (Command* cmd) {
    Editor* ed = cmd->GetEditor();

    if (cmd->IsA(PLACE_CMD)) {
	Iterator i;
	Clipboard* cb = cmd->GetClipboard();

	for (cb->First(i); !cb->Done(i); cb->Next(i)) {
	    InteractorComp* parent = (InteractorComp*) cb->GetComp(i);
	    parent->RestoreCanvas(cmd);
	}
        unidraw->Update();

    } else if (cmd->IsA(INFO_CMD)) {
        
    } else if (cmd->IsA(FONT_CMD)) {
        FontData* fd = (FontData*) cmd->Recall(this);
	PSFont* font = (PSFont*) fd->_void;
        GetGraphic()->SetFont(font);
	Reconfig();
	Place(this, fd->_l, fd->_b, fd->_r-1, fd->_t-1);
        Unpropagate(cmd);

    } else if (cmd->IsA(COLOR_CMD)) {
        GraphicComps::Uninterpret(cmd);
        Unpropagate(cmd);

    } else if (cmd->IsA(PROPS_CMD)) {
	PropsData* props = (PropsData*) cmd->Recall(this);

        Props* pclone = _props->Copy();
        InstanceNameVar* sclone = 
            (InstanceNameVar*) _instanceNameVar->Copy();
        Props* porig = props->GetProps();
        InstanceNameVar* sorig = (InstanceNameVar*) props->GetStateVar();
        
        pclone->SetPropsText(_props->GetPropsText());
        _props->SetPropsText(porig->GetPropsText());
        porig->SetPropsText(pclone->GetPropsText());
        
        *sclone = *_instanceNameVar;
        *_instanceNameVar = *sorig;
        *sorig = *sclone;
            
        delete pclone;
        delete sclone;
        Unpropagate(cmd);

    } else if (
	!cmd->IsA(GLUEVISIBILITY_CMD) && 
	!cmd->IsA(ALIGN_CMD) && !cmd->IsA(BRUSH_CMD) && 
        !cmd->IsA(UNGROUP_CMD) && !cmd->IsA(GETCLONES_CMD)
    ) {
	GraphicComps::Uninterpret(cmd);
    }
}

void InteractorComp::StoreCanvas(Command* cmd) {
    lbrtData* prevData = (lbrtData*) cmd->Recall(this);
    Iterator i;
    if (prevData == nil && GetCanvasVar() != nil) {
        lbrtData* lbrtbox = new lbrtData(this);
        cmd->Store(this, lbrtbox);
        for (First(i); !Done(i); Next(i)) {
            InteractorComp* kid = (InteractorComp*) GetComp(i);
            kid->StoreCanvas(cmd);
        }
    }
}

void InteractorComp::RestoreCanvas(Command* cmd) {
    Iterator i;
    lbrtData* d = (lbrtData*) cmd->Recall(this);
    if (d != nil) {
        Place(this, d->_l, d->_b, d->_r-1, d->_t-1);
        *GetShapeVar()->GetShape() = *d->_ibshape;
        Transformer* tr = GetGraphic()->GetTransformer();
        if (tr == nil) {
            GetGraphic()->SetTransformer(new Transformer(d->_tr));
        } else {
            *tr = *d->_tr;
        }
        Notify();
        for (First(i); !Done(i); Next(i)) {
            InteractorComp* kid = (InteractorComp*) GetComp(i);
            kid->RestoreCanvas(cmd);
        }
    }
}

InteractorComp& InteractorComp::operator = (InteractorComp&) { return *this;}

void InteractorComp::Relate (InteractorComp* comp) {
    *this = *comp;
}

void InteractorComp::Unrelate (InteractorComp* comp) {
    *this = *comp;
}

void InteractorComp::Instantiate() {
    if (_instanceNameVar == nil) {
	_instanceNameVar = new InstanceNameVar;
        _instanceNameVar->GenNewName();
    }
    if (_memberVar == nil) {
	char buf[CHARBUFSIZE];
	sprintf(buf, "_%s_0", _classNameVar->GetName());
	_memberVar = new MemberNameVar(buf);
        _memberVar->GenNewName();
        _memberVar->SetSubclass(_classNameVar);
    }
}

InteractorComp* InteractorComp::GetIComp (Iterator i) {
    return (InteractorComp*) GetComp(i);
}

IBGraphic* InteractorComp::GetIBGraphic () { 
    return (IBGraphic*) GetGraphic();
}

void InteractorComp::Reconfig () {
    Iterator i;
    for (First(i); !Done(i); Next(i)) {
        GetIComp(i)->Reconfig();
    }
}

void InteractorComp::Resize () { }

void InteractorComp::SetState(const char* name, StateVar* stateVar) { 
    if (strcmp(name, "CanvasVar") == 0) {
        _canvasVar = (CanvasVar*) stateVar;

    } else if (strcmp(name, "ClassNameVar") == 0) {
        SubclassNameVar* classNameVar = (SubclassNameVar*) stateVar;
        classNameVar->ref();
        _classNameVar->unref();
        _classNameVar = classNameVar;

    } else if (strcmp(name, "MemberNameVar") == 0) {
        MemberNameVar* memberVar = (MemberNameVar*) stateVar;
        *_memberVar = *memberVar;

    } else if (strcmp(name, "InstanceNameVar") == 0) {
        _instanceNameVar = (InstanceNameVar*) stateVar;

    } else if (strcmp(name, "ShapeVar") == 0) {
        _shapeVar = (ShapeVar*) stateVar;
    }
}

StateVar* InteractorComp::GetState (const char* name) {
    StateVar* stateVar = nil;

    if (strcmp(name, "CanvasVar") == 0) {
        stateVar = _canvasVar;

    } else if (strcmp(name, "ClassNameVar") == 0) {
        stateVar = _classNameVar;

    } else if (strcmp(name, "MemberNameVar") == 0) {
        stateVar = _memberVar;

    } else if (strcmp(name, "InstanceNameVar") == 0) {
        stateVar = _instanceNameVar;

    } else if (strcmp(name, "ShapeVar") == 0) {
        stateVar = _shapeVar;
    }

    return stateVar;
}

void InteractorComp::Read (istream& in) {
    GraphicComp::Read(in);
    Catalog* catalog = unidraw->GetCatalog();
    
    delete _canvasVar;
    delete _shapeVar;
    delete _instanceNameVar;
    delete _memberVar;
    _classNameVar->unref();

    _canvasVar = (CanvasVar*) catalog->ReadStateVar(in);
    _classNameVar = (SubclassNameVar*) catalog->ReadStateVar(in);
    _memberVar = (MemberNameVar*) catalog->ReadStateVar(in);
    _instanceNameVar = (InstanceNameVar*) catalog->ReadStateVar(in);
    _shapeVar = (ShapeVar*) catalog->ReadStateVar(in);
    _classNameVar->ref();
    if (_memberVar != nil) {
        _memberVar->SetSubclass(_classNameVar);
    }

    _props->Read(in);

}

void InteractorComp::Write (ostream& out) {
    GraphicComp::Write(out);
    Catalog* catalog = unidraw->GetCatalog();
    
    catalog->WriteStateVar(_canvasVar, out);
    catalog->WriteStateVar(_classNameVar, out);
    catalog->WriteStateVar(_memberVar, out);
    catalog->WriteStateVar(_instanceNameVar, out);
    catalog->WriteStateVar(_shapeVar, out);
    _props->Write(out);
}

void InteractorComp::InitCanvas (InteractorComp* comp) {
    comp->_canvasVar = new CanvasVar;
    IBGraphic* g = comp->GetIBGraphic();
    g->SetCanvasVar(comp->_canvasVar);
}

void InteractorComp::Place (InteractorComp* comp) {
    comp->Instantiate();
    if (comp->_canvasVar == nil) {
        InitCanvas(comp);
    }
    Shape* shape = comp->GetShapeVar()->GetShape();
    comp->_canvasVar->SetSize(shape->width, shape->height);
    comp->Resize();
    comp->Notify();
}

void InteractorComp::Place (
    InteractorComp* comp, Coord l, Coord b, Coord r, Coord t
) {
    Iterator i;
    float cx, cy;

    comp->Instantiate();
    if (comp->_canvasVar == nil) {
        InitCanvas(comp);
    }
    IBGraphic* g = comp->GetIBGraphic();
    comp->GetCanvasVar()->SetSize(r-l+1, t-b+1);
    comp->Resize();
    g->GetCenter(cx, cy);
    g->Translate(float(l+r)/2 - cx, float(b+t)/2 - cy);
    comp->Notify();
}

/*****************************************************************************/

InteractorComp* InteractorView::GetInteractorComp () {
    return (InteractorComp*) GetSubject();
}

InteractorView::InteractorView (InteractorComp* subj) : GraphicViews(subj) { }
ClassId InteractorView::GetClassId () { return INTERACTOR_VIEW; }

boolean InteractorView::IsA (ClassId id) {
    return INTERACTOR_VIEW == id || GraphicViews::IsA(id);
}

InfoDialog* InteractorView::GetInfoDialog () {
    InfoDialog* info = new InfoDialog;
    ButtonState* state = info->GetState();

    InteractorComp* icomp = GetInteractorComp();
    SubclassNameVar* classNameVar = icomp->GetClassNameVar();
    MemberNameVar* memberVar = icomp->GetMemberNameVar();
    CanvasVar* canvasVar = icomp->GetCanvasVar();
    ShapeVar* shapeVar = icomp->GetShapeVar();

    info->Include(new SubclassNameVarView(classNameVar, state, icomp));
    info->Include(new MemberNameVarView(memberVar, state, icomp));
    info->Include(new CanvasVarView(canvasVar));
    info->Include(new ShapeVarView(shapeVar, state));

    return info;
}

Graphic* InteractorView::GetGraphic () {
    Graphic* g = GraphicView::GetGraphic();

    if (g == nil) {
        g = GetGraphicComp()->GetGraphic();
	g = g->Copy();
        SetGraphic(g);
    }
    return g;
}

void InteractorView::GetABSCoord(
    Editor* ed, Coord& x0, Coord& y0, Coord& x1, Coord& y1
) {
    Transformer fulltrans;
    GraphicComp* grcomp = (GraphicComp*) ed->GetComponent();
    Graphic* g = grcomp->GetGraphic();
    g->TotalTransformation(fulltrans);
    fulltrans.TransformRect(x0, y0, x1, y1);
}

Manipulator* InteractorView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Manipulator* m = nil;

    if (tool->IsA(STRETCH_TOOL)) {
        Selection* s = v->GetSelection(), *newSel;
        GraphicView* views = v->GetGraphicView();
        s->Clear();
        newSel = views->ViewIntersecting(
            e.x-SLOP, e.y-SLOP, e.x+SLOP, e.y+SLOP
        );
        s->Exclusive(newSel);
        delete newSel;
        m = GraphicViews::CreateManipulator(v, e, rel, tool);

    } else if (tool->IsA(EXAMINE_TOOL)) {
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
        m = new PopupManip(v, popup, tool);

    } else {
        m = GraphicViews::CreateManipulator(v, e, rel, tool);
    }
    return m;
        
}

Command* InteractorView::InterpretManipulator (Manipulator* m) {
    Command* cmd = nil;
    Tool* tool = m->GetTool();

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        cmd = InterpretGraphicCompManip(m);

        if (cmd != nil) {
            IBEditor* ed = (IBEditor*) cmd->GetEditor();
	    Clipboard* cb = cmd->GetClipboard();

	    if (!tool->IsA(IBGRAPHIC_COMP_TOOL)) {
	        Iterator i;
        	ColorVar* colVar = (ColorVar*) ed->GetState("ColorVar");
		FontVar* fontVar = (FontVar*) ed->GetState("FontVar");

		for(cb->First(i); !cb->Done(i); cb->Next(i)) {
		    Graphic* gr = cb->GetComp(i)->GetGraphic();

		    if (IsA(ADJUSTER_VIEW)) {
		        gr->SetColors(
			    colVar->GetFgColor(), stdgraphic->GetBgColor()
                        );
		    } else {
		        gr->SetColors(
                            colVar->GetFgColor(), colVar->GetBgColor()
                        );
		    }
		    gr->SetFont(fontVar->GetFont());
		}
                cmd = new MacroCmd(ed, cmd, new PlaceCmd(ed, cb->Copy()));
	    }
        }
    } else if (tool->IsA(STRETCH_TOOL)) {
	Viewer* v = m->GetViewer();
        DragManip* dm = (DragManip*) m;
        Editor* ed = v->GetEditor();
	Transformer* rel = dm->GetTransformer();
        RubberRect* rr = (RubberRect*) dm->GetRubberband();
	Coord x0, y0, x1, y1;
        rr->GetCurrent(x0, y0, x1, y1);
	NormalRect(x0, y0, x1, y1);

        if (rel != nil) {
            rel->InvTransformRect(x0, y0, x1, y1);
        }
	GetABSCoord(ed, x0, y0, x1, y1);
	cmd = new PlaceCmd(ed, x0, y0, x1-1, y1-1);

    } else {
        cmd = GraphicViews::InterpretManipulator(m);
    }
    return cmd;
}

void InteractorView::Interpret(Command* cmd) {
    if (cmd->IsA(INFO_CMD)) {
        InfoCmd* info = (InfoCmd*) cmd;
        InteractorComp* icomp = GetInteractorComp();
        ShapeVar* svar = icomp->GetShapeVar();
        info->SetInfoDialog(GetInfoDialog());
        info->SetOrigIBShape(svar->GetShape()->Copy());
        info->SetIBShape(svar->GetShape());

    } else {
        GraphicViews::Interpret(cmd);
    }
}

boolean InteractorView::UpdateCanvasVar () {
    IBGraphic* gcomp = GetInteractorComp()->GetIBGraphic();
    IBGraphic* gview = (IBGraphic*) GetGraphic();
    CanvasVar* ccomp = gcomp->GetCanvasVar();
    CanvasVar* cview = gview->GetCanvasVar();
    boolean changed = ccomp != cview;

    if (changed) {
        if (ccomp == nil) {
            delete cview;
            gview->SetCanvasVar(nil);

        } else if (cview == nil) {
            gview->SetCanvasVar((CanvasVar*) ccomp->Copy());

        } else {
            *cview = *ccomp;
        }
    }
    return changed;
}

boolean InteractorView::Different (Graphic* g1, Graphic* g2) {
    boolean different = true;

    BoxObj box1;
    BoxObj box2;

    g1->GetBox(box1);
    g2->GetBox(box2);

    if (
        g1->GetFgColor() == g2->GetFgColor() &&
        g1->GetBgColor() == g2->GetBgColor() &&
        g1->BgFilled() == g2->BgFilled() &&
        g1->GetPattern() == g2->GetPattern() &&
        g1->GetBrush() == g2->GetBrush() &&
        g1->GetFont() == g2->GetFont() &&
        box1._left == box2._left &&
        box1._bottom == box2._bottom &&
        box1._right == box2._right &&
        box1._top == box2._top
    ) {
        Transformer identity;
        Transformer* t1 = g1->GetTransformer();
        Transformer* t2 = g2->GetTransformer();

        if (t1 == t2) {
            different = false;
        } else if (t1 == nil) {
            different = *t2 != identity;
        } else if (t2 == nil) {
            different = *t1 != identity;
        } else {
            different = *t1 != *t2;
        }
    }
    return different;
}

/*****************************************************************************/

ClassId HVComp::GetClassId () { return HVCOMP; }
boolean HVComp::IsA (ClassId id) {return HVCOMP==id ||InteractorComp::IsA(id);}
HVGraphic* HVComp::GetHVGraphic () { return (HVGraphic*) GetGraphic(); }
HVGraphic* HVComp::InitGraphic (Orientation, int) { return nil; }
HVComp::HVComp (HVGraphic* g) : InteractorComp(g) { }

void HVComp::Reconfig () {
    Shape* shape = GetShapeVar()->GetShape();
    HVGraphic* g = GetHVGraphic();

    if (g->GetOrientation() == Horizontal) {
        g->SetShape(shape->width, shape->hshrink, shape->hstretch);
    } else {
        g->SetShape(shape->height, shape->vshrink, shape->vstretch);
    }
}

void HVComp::Read (istream& in) {
    InteractorComp::Read(in);
    int orient, w;
    in >> orient;
    in >> w;

    HVGraphic* g = InitGraphic(orient, w);
    g->ReadGS(in);
    SetGraphic(g);
    Reconfig();
}

void HVComp::Write (ostream& out) {
    InteractorComp::Write(out);
    HVGraphic* g = GetHVGraphic();
    out << g->GetOrientation() << " ";
    out << g->MinorAxisSize() << " ";
    g->WriteGS(out);
}

/*****************************************************************************/

HVComp* HVView::InitComp (Coord, Coord, Coord, Coord) { return nil; }
HVComp* HVView::GetHVComp () { return (HVComp*) GetSubject(); }
ClassId HVView::GetClassId () { return HVVIEW; }
boolean HVView::IsA (ClassId id) {return HVVIEW==id ||InteractorView::IsA(id);}

HVView::HVView (HVComp* subj) : InteractorView(subj) { }

void HVView::Update () {
    HVGraphic* gcomp = GetHVComp()->GetHVGraphic();
    HVGraphic* gview = (HVGraphic*) GetGraphic();
    int nat, shr, str;

    IncurDamage(gview);
    *(Graphic*)gview = *(Graphic*)gcomp;
    gcomp->GetShape(nat, shr, str);
    gview->SetShape(nat, shr, str);
    UpdateCanvasVar();
    IncurDamage(gview);
    EraseHandles();
}

Manipulator* HVView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Manipulator* m = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        HVGraphic* g = GetHVComp()->GetHVGraphic();
        Coord xminor = g->MinorAxisSize();
        Coord yminor = xminor;

        if (rel != nil) {
            Coord x0 = 0, y0 = 0;
            rel->Transform(x0, y0);
            rel->Transform(xminor, yminor);
            xminor = abs(xminor - x0);
            yminor = abs(yminor - y0);
        }
        Orientation orient = g->GetOrientation();
        Coord x0, y0, x1, y1;
        Side side;

        if (orient == Horizontal) {
            x0 = x1 = e.x;
            y0 = e.y - yminor/2;
            y1 = y0 + yminor - 1;
            side = RightSide;

        } else {
            x0 = e.x - xminor/2;
            x1 = x0 + xminor - 1;
            y0 = y1 = e.y;
            side = BottomSide;
        }            
        Rubberband* rub = new StretchingRect(nil, nil, x0, y0, x1, y1, side);
        m = new DragManip(v, rub, rel, tool);

    } else {
        m = InteractorView::CreateManipulator(v, e, rel, tool);
    }
    return m;
}

Command* HVView::InterpretManipulator (Manipulator* m) {
    DragManip* dm = (DragManip*) m;
    Editor* ed = dm->GetViewer()->GetEditor();
    Tool* tool = dm->GetTool();
    Transformer* rel = dm->GetTransformer();
    Command* cmd = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        StretchingRect* stretchRect = (StretchingRect*) dm->GetRubberband();
        Coord x0, y0, x1, y1;
        stretchRect->GetCurrent(x0, y0, x1, y1);
        NormalRect(x0, y0, x1, y1);

        if (rel != nil) {
            rel->InvTransformRect(x0, y0, x1, y1);
        }
	GetABSCoord(ed, x0, y0, x1, y1);

        HVComp* comp = InitComp(x0, y0, x1, y1);
	Graphic* gr = comp->GetGraphic();
        ColorVar* colVar = (ColorVar*) ed->GetState("ColorVar");
	if (colVar != nil) {
	    gr->SetColors(colVar->GetFgColor(), colVar->GetBgColor());
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

