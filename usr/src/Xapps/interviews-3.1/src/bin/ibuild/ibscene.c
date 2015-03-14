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
 * Implementation of SceneComp and SceneView subclasses.
 */

#include "ibclasses.h"
#include "ibcmds.h"
#include "ibdialogs.h"
#include "ibglobals.h"
#include "ibgraphic.h"
#include "ibscene.h"
#include "ibtools.h"
#include "ibvars.h"
#include "ibvarviews.h"

#include <InterViews/transformer.h>

#include <Unidraw/catalog.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/iterator.h>
#include <Unidraw/manips.h>
#include <Unidraw/selection.h>
#include <Unidraw/viewer.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/ulist.h>
#include <Unidraw/Commands/command.h>
#include <Unidraw/Commands/macro.h>
#include <Unidraw/Tools/tool.h>

#include <stream.h>
#include <string.h>

/*****************************************************************************/

SceneComp::SceneComp (IBGraphic* gr) : InteractorComp(gr) { 
    if (gr == nil) {
	Graphic* graphic = GetGraphic();
	delete graphic;
	SetGraphic(new IBGraphic);
    }
    GetClassNameVar()->SetName("Scene");
    GetClassNameVar()->SetBaseClass("Scene");
}

ClassId SceneComp::GetClassId () { return SCENE_COMP; }

boolean SceneComp::IsA (ClassId id) {
    return SCENE_COMP == id || InteractorComp::IsA(id);
}

/*****************************************************************************/

static boolean copylock = true;

/*****************************************************************************/

void SceneComp::Interpret (Command* cmd) {
    Iterator i;
    Editor* ed = cmd->GetEditor();

    if (cmd->IsA(GLUEVISIBILITY_CMD) || cmd->IsA(GETCLONES_CMD)) {
        for (First(i); !Done(i); Next(i)) {
            GetComp(i)->Interpret(cmd);
        }

    } else if (cmd->IsA(PASTE_CMD)) {
        Clipboard* cb = cmd->GetClipboard();
        Iterator i;

        if (cb == nil) {
            Clipboard* globalcb = unidraw->GetCatalog()->GetClipboard();

            if (globalcb->IsEmpty()) {
                return;
            }
            copylock = false;
            cmd->SetClipboard(cb = globalcb->DeepCopy());
            copylock = true;
        }

        for (cb->First(i); !cb->Done(i);) {
            GraphicComp* kid = cb->GetComp(i);
            if (!kid->IsA(INTERACTOR_COMP)) {
                cb->Remove(i);
            } else {
                cb->Next(i);
            }
        }
        
        InteractorComp::Interpret(cmd);
        Propagate(cmd);

    } else if (
        cmd->IsA(DELETE_CMD) || cmd->IsA(CUT_CMD) || 
        cmd->IsA(FRONT_CMD) ||
        cmd->IsA(BACK_CMD) || cmd->IsA(DUP_CMD)
    ) {
        InteractorComp::Interpret(cmd);
        Propagate(cmd);

    } else if (cmd->IsA(SCAN_CMD)) {
        ScanCmd* scmd = (ScanCmd*) cmd;
        InteractorComp::Interpret(cmd);
        if (!scmd->Succeeded()) {
            for (First(i); !Done(i) && !scmd->Succeeded(); Next(i)) {
                GetComp(i)->Interpret(cmd);
            }
        }
        
    } else if (cmd->IsA(GETCONFLICT_CMD)) {
        InteractorComp::Interpret(cmd);
        for (First(i); !Done(i); Next(i)) {
            GetComp(i)->Interpret(cmd);
        }
    } else if (cmd->IsA(COMPCHECK_CMD) || cmd->IsA(IDMAP_CMD)) {
        for (First(i); !Done(i); Next(i)) {
            GetComp(i)->Interpret(cmd);
        }
        
    } else if (cmd->IsA(MONOSCENE_CMD)) {
        MonoSceneCmd* mcmd = (MonoSceneCmd*) cmd;
        Component* edComp = mcmd->GetEditor()->GetComponent();
        if (edComp == (Component*) this) {
            Iterator i, j;
            Clipboard cbtmp;
            
            Clipboard* cb = mcmd->GetClipboard();
            Clipboard* mcb = mcmd->GetMSClipboard();
            for (
                cb->First(i), mcb->First(j) ; 
                !cb->Done(i); cb->Next(i), mcb->Next(j)
            ) {
                InteractorComp* kid = (InteractorComp*) cb->GetComp(i);
                InteractorComp* parent = (InteractorComp*) mcb->GetComp(j);
                cbtmp.Clear();
                cbtmp.Append(kid);
                kid->StoreCanvas(mcmd->GetLogCmd());
                Group(&cbtmp, parent, cmd);
                lbrtData* lbrtbox = (lbrtData*) cmd->Recall(parent);
                if (lbrtbox == nil) {
                    lbrtbox = new lbrtData(kid);
                    cmd->Store(parent, lbrtbox);
                }
                parent->Reconfig();
                Place(parent);
            }
            Notify();
            SelectClipboard(mcb, ed);
            unidraw->Update();
            Propagate(cmd);
        }
    } else if (cmd->IsA(SCENE_CMD)) {
        SceneCmd* scmd = (SceneCmd*) cmd;
        SceneComp* group = (SceneComp*) scmd->GetGroup();
        Component* edComp = scmd->GetEditor()->GetComponent();

        if (edComp == (Component*) this) {
            Iterator i;
            InteractorComp::Interpret(cmd);
            group->Reconfig();
            Clipboard* cb = cmd->GetClipboard();
            Command* lcmd = scmd->GetLogCmd();
            for (cb->First(i); !cb->Done(i); cb->Next(i)) {
                InteractorComp* kid = (InteractorComp*) cb->GetComp(i);
                kid->StoreCanvas(lcmd);
            }
            Place(group);
            Propagate(cmd);
        } else {
            InteractorComp::Interpret(cmd);
        }
        
    } else if (cmd->IsA(UNGROUP_CMD)) {
        Component* edComp = cmd->GetEditor()->GetComponent();
        
        if (edComp == (Component*) this) {
            GraphicComps::Interpret(cmd);
            Propagate(cmd);
        } else {
            GraphicComps::Interpret(cmd);
        }

    } else if (cmd->IsA(REORDER_CMD)) {
        ReorderCmd* rcmd = (ReorderCmd*) cmd;
        Clipboard* cb;
        cb = rcmd->GetClipboard();
        Selection* s = ed->GetSelection();
        if (cb == nil) {
            rcmd->SetClipboard(cb = new Clipboard);
            cb->Init(s);
        }
        Iterator i, pos;
        cb->First(i);
        GraphicComp* leader = cb->GetComp(i);
        SetComp(leader, pos);
        for (cb->Next(i); !cb->Done(i); cb->Next(i)) {
            GraphicComp* comp = cb->GetComp(i);
            StorePosition(comp, rcmd);
            Remove(comp);
            InsertAfter(pos, comp);
            SetComp(comp, pos);
        }
        if (_canvasVar != nil) {
            lbrtData d(this);
            StoreCanvas(rcmd->GetLogCmd());
            Reconfig();
            Place(this, d._l, d._b, d._r-1, d._t-1);
            unidraw->Update();
        }
        Propagate(cmd);

    } else if (
        cmd->IsA(FONT_CMD) || cmd->IsA(BRUSH_CMD) || 
        cmd->IsA(COLOR_CMD) || cmd->IsA(ALIGN_CMD)
    ) {
        
        for (First(i); !Done(i); Next(i)) {
            GetComp(i)->Interpret(cmd);
        }
        Propagate(cmd);

    } else {
        InteractorComp::Interpret(cmd);
    }
}

void SceneComp::Uninterpret (Command* cmd) {
    Iterator i;
    Editor* ed = cmd->GetEditor();

    if (cmd->IsA(GLUEVISIBILITY_CMD)) {
        for (Last(i); !Done(i); Prev(i)) {
            GetComp(i)->Uninterpret(cmd);
        }

    } else if (
        cmd->IsA(DELETE_CMD) || cmd->IsA(CUT_CMD) || 
        cmd->IsA(PASTE_CMD) || cmd->IsA(FRONT_CMD) ||
        cmd->IsA(BACK_CMD) || cmd->IsA(DUP_CMD)
    ) {
        InteractorComp::Uninterpret(cmd);
        Unpropagate(cmd);

    } else if (cmd->IsA(MONOSCENE_CMD)) {
        MonoSceneCmd* mcmd = (MonoSceneCmd*) cmd;
        Component* edComp = mcmd->GetEditor()->GetComponent();
        if (edComp == (Component*) this) {
            Iterator i;
            Clipboard* cb = cmd->GetClipboard();
            for (cb->Last(i); !cb->Done(i); cb->Prev(i)) {
                InteractorComp* kid = (InteractorComp*) cb->GetComp(i);
                InteractorComp* parent = (InteractorComp*) kid->GetParent();
                parent->Notify();
                
                lbrtData* d = (lbrtData*) cmd->Recall(parent);
                if (d != nil) {
                    Place(kid, d->_l, d->_b, d->_r-1, d->_t-1);
                }
                parent->Bequeath();
                unidraw->CloseDependents(parent);
                RestorePosition(kid, cmd);
                kid->RestoreCanvas(mcmd->GetLogCmd());
                Remove(parent);
            }
            Notify();
            SelectClipboard(cb, ed);
            unidraw->Update();
            Unpropagate(cmd);
        }
        
    } else if (cmd->IsA(SCENE_CMD)) {
        SceneCmd* scmd = (SceneCmd*) cmd;
        SceneComp* group = (SceneComp*) scmd->GetGroup();
        Component* edComp = scmd->GetEditor()->GetComponent();

        if (edComp == (Component*) this) {
            Iterator i;
            Clipboard* cb = cmd->GetClipboard();
            Command* lcmd = scmd->GetLogCmd();
            for (cb->First(i); !cb->Done(i); cb->Next(i)) {
                InteractorComp* kid = (InteractorComp*) cb->GetComp(i);
                kid->RestoreCanvas(lcmd);
            }
            InteractorComp::Uninterpret(cmd);
            Unpropagate(cmd);
        } else {
            InteractorComp::Uninterpret(cmd);
        }
        
    } else if (cmd->IsA(UNGROUP_CMD)) {
        Component* edComp = cmd->GetEditor()->GetComponent();

        if (edComp == (Component*) this) {
            GraphicComps::Uninterpret(cmd);
            Unpropagate(cmd);
        } else {
            GraphicComps::Uninterpret(cmd);
        }

    } else if (cmd->IsA(REORDER_CMD)) {
        ReorderCmd* rcmd = (ReorderCmd*) cmd;
        Iterator i;
        Clipboard* cb = cmd->GetClipboard();
        cb->First(i);
        for (cb->Next(i); !cb->Done(i); cb->Next(i)) {
            RestorePosition(cb->GetComp(i), cmd);
        }
        RestoreCanvas(rcmd->GetLogCmd());
        unidraw->Update();
        Unpropagate(cmd);

    } else if (
        cmd->IsA(FONT_CMD) || cmd->IsA(BRUSH_CMD) || 
        cmd->IsA(COLOR_CMD) || cmd->IsA(ALIGN_CMD)
    ) {
        
        for (First(i); !Done(i); Next(i)) {
            GetComp(i)->Uninterpret(cmd);
        }
        Unpropagate(cmd);

    } else {
        InteractorComp::Uninterpret(cmd);
    }
}

void SceneComp::Read (istream& in) {
    InteractorComp::Read(in);

    IBGraphic* g = GetIBGraphic();
    int count;
    in >> count;

    for (int i = 0; i < count; ++i) {
        Append((GraphicComp*) unidraw->GetCatalog()->ReadComponent(in));
    }

    g->FillBg(ReadBgFilled(in));
    g->SetBrush(ReadBrush(in));
    PSColor* fg = ReadColor(in);
    PSColor* bg = ReadColor(in);
    g->SetColors(fg, bg);
    g->SetFont(ReadFont(in));
    g->SetPattern(ReadPattern(in));
    
    Transformer* t = ReadTransformer(in);
    g->SetTransformer(t);
    Unref(t);

    g->SetCanvasVar(GetCanvasVar());
}

void SceneComp::Write (ostream& out) {
    InteractorComp::Write(out);

    Iterator i;
    int count = 0;
    IBGraphic* g = GetIBGraphic();

    for (First(i); !Done(i); Next(i), ++count);
    out << count << "\n";

    for (First(i); !Done(i); Next(i)) {
        GraphicComp* comp = GetComp(i);
        unidraw->GetCatalog()->WriteComponent(comp, out);
        out << "\n";
    }

    WriteBgFilled(g->BgFilled(), out);
    WriteBrush(g->GetBrush(), out);
    WriteColor(g->GetFgColor(), out);
    WriteColor(g->GetBgColor(), out);
    WriteFont(g->GetFont(), out);
    WritePattern(g->GetPattern(), out);
    WriteTransformer(g->GetTransformer(), out);
}

/*****************************************************************************/

SceneView::SceneView (SceneComp* subj) : InteractorView(subj) { _isel = nil; }
SceneComp* SceneView::GetSceneComp () { return (SceneComp*) GetSubject(); }
ClassId SceneView::GetClassId () { return SCENE_VIEW; }

boolean SceneView::IsA (ClassId id) {
    return SCENE_VIEW == id || InteractorView::IsA(id);
}

Graphic* SceneView::GetGraphic () {
    Graphic* g = GraphicView::GetGraphic();

    if (g == nil) {
        g = new IBGraphic;
        SetGraphic(g);
    }
    return g;
}

Manipulator* SceneView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Manipulator* m = nil;
    if (tool->IsA(RESHAPE_TOOL)) {
	Iterator i;
	_isel = ViewIntersecting(e.x-SLOP,e.y-SLOP,e.x+SLOP,e.y+SLOP);
	if (!_isel->IsEmpty()) {
	    _isel->First(i);
	    GraphicView* gv = _isel->GetView(i);

            Transformer* t = GetGraphic()->GetTransformer();
            if (t != nil) {
                rel->Premultiply(t);
            }
	    m = gv->CreateManipulator(v, e, rel, tool);
	}

    } else {
	m = InteractorView::CreateManipulator(v, e, rel, tool);
    }
    return m;
}

Command* SceneView::InterpretManipulator (Manipulator* m) {
    Tool* tool = m->GetTool();
    Command* cmd = nil;

    if (tool->IsA(RESHAPE_TOOL)) {
	Iterator i;
        _isel->First(i);

	GraphicView* gv = _isel->GetView(i);
	cmd = gv->InterpretManipulator(m);
        delete _isel;
        _isel = nil;

    } else {
	cmd = InteractorView::InterpretManipulator(m);
    }
    return cmd;
}

/*****************************************************************************/

class KidData : public Data {
public:
    KidData(InteractorComp*);
    virtual ~KidData();

    void SetKid(InteractorComp* kid);
    InteractorComp* GetKid();
private:
    InteractorComp* _kid;
};

InteractorComp* KidData::GetKid() { return _kid; }
void KidData::SetKid(InteractorComp* kid) { _kid = kid; }

KidData::KidData (InteractorComp* kid) { _kid = kid; }

KidData::~KidData () { delete _kid; }

/*****************************************************************************/

static boolean HasKids (Clipboard* cb) {
    boolean retval = false;
    Iterator i, j;
    int count;
    for (cb->First(i); !cb->Done(i); cb->Next(i)) {
        GraphicComp* icomp = cb->GetComp(i);
        for (
            icomp->First(j), count = 0; 
            !icomp->Done(j); icomp->Next(j),
            count++
        );
        if (count > 1) {
            retval = true;
            break;
        }
    }
    return retval;
}
        
/*****************************************************************************/

MonoSceneComp::MonoSceneComp (IBGraphic* gr) : SceneComp(gr) {
    GetClassNameVar()->SetName("MonoScene");
    GetClassNameVar()->SetBaseClass("MonoScene");
}

ClassId MonoSceneComp::GetClassId () { return MONOSCENE_COMP; }

boolean MonoSceneComp::IsA (ClassId id) {
    return MONOSCENE_COMP == id || SceneComp::IsA(id);
}

InteractorComp* MonoSceneComp::GetKid () {
    Iterator i;
    First(i);
    return (InteractorComp*) GetComp(i);
}

void MonoSceneComp::Interpret (Command* cmd) {
    Editor* ed = cmd->GetEditor();
    if (cmd->IsA(PASTE_CMD)) {
        Clipboard* cb = cmd->GetClipboard();
        Iterator i;
        
        if (cb == nil) {
            Clipboard* globalcb = unidraw->GetCatalog()->GetClipboard();
            
            if (globalcb->IsEmpty()) {
                return;
            }
            copylock = false;
            cmd->SetClipboard(cb = globalcb->DeepCopy());
            copylock = true;
        }
        
        for (cb->First(i); !cb->Done(i); cb->Next(i)) {
            InteractorComp* newKid = (InteractorComp*) cb->GetComp(i);
            InteractorComp* oldKid = GetKid();
            if (oldKid != nil) {
                KidData* kidData = (KidData*) cmd->Recall(newKid);
                if (kidData == nil) {
                    KidData* kidData = new KidData(oldKid);
                    cmd->Store(newKid, kidData);
                } else {
                    kidData->SetKid(oldKid);
                }
                unidraw->CloseDependents(oldKid);
                Remove(oldKid);
            }
            Append(newKid);
        }
        Notify();
        SelectClipboard(cb, ed);
        unidraw->Update();
        Propagate(cmd);

    } else if (cmd->IsA(UNGROUP_CMD)) {
        Component* edComp = cmd->GetEditor()->GetComponent();
        if (edComp == (Component*) this) {
            Clipboard* cb = cmd->GetClipboard();
            if (!HasKids(cb)) {
                SceneComp::Interpret(cmd);
            } else {
                delete cb;
                cmd->SetClipboard(new Clipboard);
            }
        } else {
            SceneComp::Interpret(cmd);
        }
        
    } else if (!cmd->IsA(DUP_CMD)) {
        SceneComp::Interpret(cmd);
    }
}

void MonoSceneComp::Uninterpret (Command* cmd) {
    Editor* ed = cmd->GetEditor();
    if (cmd->IsA(PASTE_CMD)) {
        Clipboard* cb = cmd->GetClipboard();
        if (cb != nil) {
            Selection* s = ed->GetSelection();
            Iterator i;
            
            s->Clear();
            for (cb->First(i); !cb->Done(i); cb->Next(i)) {
                InteractorComp* newKid = (InteractorComp*) cb->GetComp(i);
                Remove(newKid);
                KidData* kidData = (KidData*) cmd->Recall(newKid);
                if (kidData != nil) {
                    InteractorComp* oldKid = kidData->GetKid();
                    Append(oldKid);
                    kidData->SetKid(newKid);
                } else {
                    kidData = new KidData(newKid);
                    cmd->Store(newKid, kidData);
                }
            }
            Notify();
            unidraw->Update();
        }
        Unpropagate(cmd);

    } else if (!cmd->IsA(DUP_CMD)) {
        SceneComp::Uninterpret(cmd);
    }
}

void MonoSceneComp::Reconfig () {
    InteractorComp* kid = GetKid();
    if (kid != nil) {
        kid->Reconfig();
	*GetShapeVar() = *kid->GetShapeVar();
    }
}

void MonoSceneComp::Resize () {
    float cx, cy;
    Coord x1, y1, x2, y2;

    CanvasVar* cvar = GetCanvasVar();
    int w = cvar->Width(), h = cvar->Height();
    GetGraphic()->GetCenter(cx, cy);

    x1 = round(cx) - w/2;
    y1 = round(cy) - h/2;
    x2 = round(cx) + (w+1)/2;
    y2 = round(cy) + (h+1)/2;

    InteractorComp* kid = GetKid();
    if (kid != nil) {
        Place(kid, x1, y1, x2-1, y2-1);
    }
}

/*****************************************************************************/

MonoSceneView::MonoSceneView (MonoSceneComp* subj) : SceneView(subj) { }

MonoSceneComp* MonoSceneView::GetMonoSceneComp () { 
    return (MonoSceneComp*) GetSubject(); 
}

ClassId MonoSceneView::GetClassId () { return MONOSCENE_VIEW; }

boolean MonoSceneView::IsA (ClassId id) {
    return MONOSCENE_VIEW == id || SceneView::IsA(id);
}

/*****************************************************************************/

ClassId MonoSceneCode::GetClassId () { return MONOSCENE_CODE; }

boolean MonoSceneCode::IsA(ClassId id) {
    return MONOSCENE_CODE == id || CodeView::IsA(id);
}

MonoSceneCode::MonoSceneCode (GraphicComp* icomp) : CodeView (icomp) {}

CodeView* MonoSceneCode::GetKidView () {
    Iterator i;
    First(i);
    return (CodeView*) GetView(i);
}

/*****************************************************************************/


static UList* DupUList(UList* orig) {
    UList* dup = new UList;
    for (UList* i = orig->First();i != orig->End();i = i->Next()) {
        dup->Append(new UList((*i)()));
    }
    return dup;
}

/*****************************************************************************/

class MonoSceneData : public VoidData {
public:
    MonoSceneData(InteractorComp*);
    virtual ~MonoSceneData();
};

MonoSceneData::MonoSceneData (InteractorComp* i) : VoidData(i) {}

MonoSceneData::~MonoSceneData () {
    InteractorComp* icomp = (InteractorComp*) _void;
    delete icomp;
}

/*****************************************************************************/

MonoSceneClass::MonoSceneClass (IBGraphic* gr) : MonoSceneComp(gr) {
    if (gr != nil) {
        SubclassNameVar* subclass = GetClassNameVar();
        subclass->SetMachGen(true);
        subclass->GenNewName();
        subclass->SetAbstract(true);
    }
}

void MonoSceneClass::Interpret (Command* cmd) {
    if (cmd->IsA(GETCONFLICT_CMD)) {
        GetConflictCmd* gcmd = (GetConflictCmd*) cmd;
        if (gcmd->IsGlobal()) {
            MonoSceneComp::Interpret(cmd);
        } else if (!gcmd->GetScope()) {
            gcmd->SetScope(true);
            MonoSceneComp::Interpret(cmd);
        }

    } else if (cmd->IsA(GETCLONES_CMD)) {
        GetClonesCmd* gcCmd = (GetClonesCmd*) cmd;
        MonoSceneClass* orig = gcCmd->GetOriginal();
        const char* origname = orig->GetClassNameVar()->GetName();
        const char* thisname = GetClassNameVar()->GetName();
        if (orig != this && strcmp(origname, thisname) == 0) {
            gcCmd->GetCloneList()->Append(new UList(this));
        } else {
            MonoSceneComp::Interpret(cmd);
        }
            
    } else if (cmd->IsA(SCAN_CMD)) {
        Iterator i;
        ScanCmd* scmd = (ScanCmd*) cmd;
        const char* iclass = _classNameVar->GetName();
        const char* sclass = scmd->GetClassName();
        if (*sclass == '\0') {
            InteractorComp::Interpret(cmd);

        } else if (strcmp(iclass, sclass) == 0) {
            scmd->SetScope(true);
            SceneComp::Interpret(cmd);
            scmd->SetScope(false);

        } else if (!scmd->GetScope()) {
            SceneComp::Interpret(cmd);
        }
    } else {
        MonoSceneComp::Interpret(cmd);
    }
}

void MonoSceneClass::Clone (Command* cmd) {
    GetClonesCmd cloneCmd(this);
    cloneCmd.Execute();
    UList* clonelist = cloneCmd.GetCloneList();
    for (UList* i = clonelist->First();i != clonelist->End();i = i->Next()) {
        MonoSceneClass* clone = (MonoSceneClass*) (*i) ();
        InteractorComp* ckid = clone->GetKid();
        InteractorComp* mkid = GetKid();
        if (cmd != nil) {
            MonoSceneData* msdata = (MonoSceneData*)cmd->Recall(clone);
            if (msdata == nil) {
                cmd->Store(clone, new MonoSceneData(ckid));
                if (mkid != nil) {
                    boolean unique = IBNameVar::GetUniqueFlag();
                    IBNameVar::SetUniqueFlag(false);
                    boolean localcopy = copylock;
                    copylock = true;
                    InteractorComp* cmkid = 
                        (InteractorComp*)mkid->Copy();
                    copylock = localcopy;
                    IBNameVar::SetUniqueFlag(unique);
                    clone->Append(cmkid);
                    if (ckid != nil) {
                        Graphic* cgr = ckid->GetGraphic();
                        cgr->Align(Center, cmkid->GetGraphic(), Center);
                        clone->Remove(ckid);
                        clone->Reconfig();
                    }
                }
                clone->Place(clone);
            } else {
                InteractorComp* okid = (InteractorComp*) msdata->_void;
                if (ckid != nil) {
                    clone->Remove(ckid);
                }
                msdata->_void = ckid;
                if (okid != nil) {
                    clone->Append(okid);
                }
                clone->Notify();
            }
        } else {
            if (mkid != nil) {
                Remove(mkid);
                delete mkid;
            }
            if (ckid != nil) {
                boolean unique = IBNameVar::GetUniqueFlag();
                IBNameVar::SetUniqueFlag(false);
                boolean localcopy = copylock;
                copylock = true;
                InteractorComp* cmkid = (InteractorComp*) ckid->Copy();
                copylock = localcopy;
                IBNameVar::SetUniqueFlag(unique);
                Append(cmkid);
                Reconfig();
                Place(this);
            }
            break;
        }
    }
    unidraw->Update();
}

void MonoSceneClass::UnClone (Command* cmd) {
    GetClonesCmd cloneCmd(this);
    cloneCmd.Execute();
    UList* clonelist = cloneCmd.GetCloneList();
    for (UList* i = clonelist->First();i != clonelist->End();i = i->Next()) {
        MonoSceneClass* clone = (MonoSceneClass*) (*i) ();
        MonoSceneData* msdata = (MonoSceneData*) cmd->Recall(clone);
        if (msdata != nil) {
            InteractorComp* ckid = (InteractorComp*) msdata->_void;
            InteractorComp* okid = clone->GetKid();
            if (okid != nil) {
                clone->Remove(okid);
            }
            msdata->_void = okid;
            if (ckid != nil) {
                clone->Append(ckid);
            }
            clone->Notify();
        } 
    }
    unidraw->Update();
}
            
void MonoSceneClass::Instantiate () {
    if (_instanceNameVar == nil) {
        MonoSceneComp::Instantiate();
        GetMemberNameVar()->SetExport(true);
    } else {
        MonoSceneComp::Instantiate();
    }
}

void MonoSceneClass::Read(istream& in) {
    boolean unique = IBNameVar::GetUniqueFlag();
    IBNameVar::SetUniqueFlag(false);
    MonoSceneComp::Read(in);
    IBNameVar::SetUniqueFlag(unique);
    if (_memberVar != nil) {
        _memberVar->GenNewName();
    }
    if (_instanceNameVar != nil) {
        _instanceNameVar->GenNewName();
    }
    if (!copylock) {
        Clone(nil);
    }
}

ClassId MonoSceneClass::GetClassId () { return MONOSCENE_CLASS; }

boolean MonoSceneClass::IsA (ClassId id) {
    return MONOSCENE_CLASS == id || MonoSceneComp::IsA(id);
}

/*****************************************************************************/

MonoSceneClassView::MonoSceneClassView (
    MonoSceneClass* subj
) : MonoSceneView(subj) { }


MonoSceneClass* MonoSceneClassView::GetMonoSceneClass () {
    return (MonoSceneClass*) GetSubject();
}

ClassId MonoSceneClassView::GetClassId () { return MONOSCENECLASS_VIEW; }

boolean MonoSceneClassView::IsA (ClassId id) {
    return MONOSCENECLASS_VIEW == id || MonoSceneView::IsA(id);
}

/*****************************************************************************/

MonoSceneClassCode::MonoSceneClassCode (
    MonoSceneClass* subj
) : MonoSceneCode(subj) {}

void MonoSceneClassCode::Update () {
    MonoSceneCode::Update();
    InteractorComp* subj = GetIntComp();
    Graphic* gr = subj->GetGraphic();
    gr->SetColors(nil, nil);
    gr->SetFont(nil);
}

MonoSceneClass* MonoSceneClassCode::GetMonoSceneClass() {
    return (MonoSceneClass*) GetSubject();
}

ClassId MonoSceneClassCode::GetClassId () { return MONOSCENECLASS_CODE; }

boolean MonoSceneClassCode::IsA(ClassId id) {
    return MONOSCENECLASS_CODE == id || MonoSceneCode::IsA(id);
}

boolean MonoSceneClassCode::Definition (ostream& out) {
    char coreclass[CHARBUFSIZE];
    boolean ok = true;

    MonoSceneClass* mclass = GetMonoSceneClass();
    SubclassNameVar* snamer = mclass->GetClassNameVar();
    MemberNameVar* mnamer = mclass->GetMemberNameVar();
    const char* subclass = snamer->GetName();
    const char* baseclass = snamer->GetBaseClass();
    const char* mname = mnamer->GetName();

    GetCoreClassName(coreclass);
    CodeView* kidview = GetKidView();
    MemberNameVar* kidname;
    if (kidview != nil) {
        kidname = kidview->GetIntComp()->GetMemberNameVar();
    }

    if (_emitInstanceInits) {
        if (!_instancelist->Find((void*) mname)) {
            _instancelist->Append(new UList((void*)mname));

            BeginInstantiate(out);
            out << "(";
            InstanceName(out, ")");
            EndInstantiate(out);
	}

    } else if (_emitClassHeaders || _emitHeaders || _emitForward) {
        if (_scope || *_classname == '\0') {
            if (!_emitHeaders) {
                ok = ok && CodeView::Definition(out);
            }
        } else {
            if (strcmp(subclass, _classname) == 0) {
                _scope = true;
                ok = ok && CodeView::Definition(out);
                if (kidview != nil) {
                    ok = ok && kidview->Definition(out); 
                }
                _scope = false;
            } else {
                if (kidview != nil) {
                    ok = ok && kidview->Definition(out); 
                }
            }
        }
    } else if (
	_emitBSDecls || _emitBSInits || 
	_emitFunctionDecls || _emitFunctionInits ||
        _emitCreatorHeader || _emitCreatorSubj || _emitCreatorView
    ) {
	ok = true;

    } else if (_emitProperty) {
        ok = ok && CodeView::Definition(out);
        if (kidview != nil) {
            ok = ok && kidview->Definition(out); 
        }
        
    } else if (_emitInstanceDecls || _emitExpHeader) {
        ok = ok && CodeView::Definition(out);

    } else if (_emitCorehHeader) {
        if (strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("scene")) {
                _namelist->Append("scene");
                out << "#include <InterViews/scene.h>\n";
            }
            if (!_namelist->Search("window")) {
                _namelist->Append("window");
                out << "#include <InterViews/window.h>\n";
            }
        } else {
            if (kidview != nil) {
                ok = ok && kidview->Definition(out); 
            }
        }
    } else if (
        _emitCoreDecls || _emitCoreInits || _emitClassDecls || _emitClassInits
    ) {
        if (strcmp(subclass, _classname) == 0) {
            ok = ok && CodeView::Definition(out);
            CleanUp();
        
        } else {
            if (kidview != nil) {
                ok = kidview->Definition(out) && ok; 
            }
        }

    }
    return ok && out.good();
}

boolean MonoSceneClassCode::CoreConstDecls(ostream& out) { 
    boolean ok = true;
    CodeView* kidview = GetKidView();

    out << "(const char*);\n";
    if (kidview != nil) {
        ok = ok && EmitFunctionDecls(kidview, out);
    }
    out << "protected:\n";
    out << "    Interactor* Interior();\n";
    out << "protected:\n";
    
    _emitExport = true;
    if (kidview != nil) {
        ok = ok && EmitBSDecls(kidview, out);
        ok = ok && EmitInstanceDecls(kidview, out);
    }
    _emitExport = false;
    
    return ok && out.good();
}

boolean MonoSceneClassCode::CoreConstInits(ostream& out) {
    boolean ok = true;
    CodeView* kidview = GetKidView();
    MemberNameVar* kidname;
    if (kidview != nil) {
        kidname = kidview->GetIntComp()->GetMemberNameVar();
    }
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);
    const char* subclass = GetIntComp()->GetClassNameVar()->GetName();

    out << "(const char* name) {\n";
    out << "    SetClassName(\"" << subclass << "\");\n";

    out << "    SetInstance(name);\n";
    out << "    if (input != nil) {\n";
    out << "        input->Unreference();\n";
    out << "    }\n";
    out << "    input = allEvents;\n";
    out << "    input->Reference();\n";
    out << "    Insert(Interior());\n";
    out << "}\n\n";
    
    out << "Interactor* " << coreclass;
    out << "::Interior() {\n";
    if (kidview != nil) {
        ok = ok && EmitBSInits(kidview, out);
        ok = ok && EmitInstanceInits(kidview, out);
        out << "    return " << kidname->GetName() << ";\n};\n\n";
        ok = ok && EmitFunctionInits(kidview, out);
    }

    return ok && out.good();
}

boolean MonoSceneClassCode::ConstDecls(ostream& out) {
    boolean ok = true;
    CodeView* kidview = GetKidView();

    out << "(const char*);\n\n";
    if (kidview != nil) {
        ok = ok && EmitFunctionDecls(kidview, out);
    }

    return ok && out.good();
}

boolean MonoSceneClassCode::ConstInits(ostream& out) {
    boolean ok = true;
    CodeView* kidview = GetKidView();
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(const char* name) : " << coreclass << "(name) {}\n\n";
    if (kidview != nil) {
        ok = ok && EmitFunctionInits(kidview, out);
    }
            
    return ok && out.good();
}

boolean MonoSceneClassCode::EmitIncludeHeaders(ostream& out) {
    if (!_namelist->Search("canvas")) {
        _namelist->Append("canvas");
        out << "#include <InterViews/canvas.h> \n";
    }
    if (!_namelist->Search("painter")) {
        _namelist->Append("painter");
        out << "#include <InterViews/painter.h> \n";
    }
    if (!_namelist->Search("sensor")) {
        _namelist->Append("sensor");
        out << "#include <InterViews/sensor.h> \n";
    }
    if (!_namelist->Search("perspective")) {
        _namelist->Append("perspective");
        out << "#include <InterViews/perspective.h> \n";
    }
    
    return out.good();
}

