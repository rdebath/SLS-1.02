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
 * Implementation of IGraphicComps and IGraphicViews and derived classes.
 */

#include "ibgrcomp.h"
#include "ibclasses.h"
#include "ibcmds.h"
#include "ibvars.h"
#include <Unidraw/catalog.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/iterator.h>
#include <Unidraw/manips.h>
#include <Unidraw/selection.h>
#include <Unidraw/viewer.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/Graphic/picture.h>
#include <Unidraw/Tools/tool.h>
#include <InterViews/transformer.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/
static const char* igrcomps_delim = "%igrcomps_delim";
/*****************************************************************************/

IGraphicComps::IGraphicComps (Graphic* gr) : IComp(gr) {
    _gclassNameVar->SetName("Picture");
    _gclassNameVar->SetBaseClass("Picture");
    _cclassNameVar->SetName("GraphicComps");
    _cclassNameVar->SetBaseClass("GraphicComps");
    _vclassNameVar->SetName("GraphicViews");
    _vclassNameVar->SetBaseClass("GraphicViews");
    _compid->SetOrigID(GRAPHIC_COMPS);

    if (gr == nil) {
        SetGraphic(new Picture);
    }
}

void IGraphicComps::Instantiate () {
    Iterator i;
    IComp::Instantiate();
    for (First(i); !Done(i); Next(i)) {
        IComp* ikid = (IComp*) GetComp(i);
        ikid->Instantiate();
    }
}

GraphicComp* IGraphicComps::GetTarget () {
    return nil;
}
        
void IGraphicComps::Interpret (Command* cmd) {
    Iterator i;
    if (cmd->IsA(SCAN_CMD)) {
        ScanCmd* scmd = (ScanCmd*) cmd;
        IComp::Interpret(cmd);
        if (!scmd->Succeeded()) {
            for (First(i); !Done(i) && !scmd->Succeeded(); Next(i)) {
                GetComp(i)->Interpret(cmd);
            }
        }
    } else if (cmd->IsA(PASTE_CMD)) {
        Clipboard* cb = cmd->GetClipboard();
        Iterator i;

        if (cb == nil) {
            Clipboard* globalcb = unidraw->GetCatalog()->GetClipboard();

            if (globalcb->IsEmpty()) {
                return;
            }
            cmd->SetClipboard(cb = globalcb->DeepCopy());
        }

        for (cb->First(i); !cb->Done(i);) {
            GraphicComp* kid = cb->GetComp(i);
            if (kid->IsA(INTERACTOR_COMP)) {
                cb->Remove(i);
            } else {
                cb->Next(i);
            }
        }
        GraphicComps::Interpret(cmd);
        for (cb->First(i); !cb->Done(i); cb->Next(i)) {
            IComp* kid = (IComp*)cb->GetComp(i);
            kid->Instantiate();
        }
        Propagate(cmd);

    } else if (cmd->IsA(GETCONFLICT_CMD) || cmd->IsA(IDMAP_CMD)) {
        IComp::Interpret(cmd);
        for (First(i); !Done(i); Next(i)) {
            GetComp(i)->Interpret(cmd);
        }
        
    } else if (cmd->IsA(EDIT_CMD)) {
        IComp::Interpret(cmd);
        Propagate(cmd);

    } else if (
        cmd->IsA(INFO_CMD) || cmd->IsA(GETNAMEVARS_CMD) ||
        cmd->IsA(GETFIREWALL_CMD) || cmd->IsA(GETTOPLEVEL_CMD)
    ) {
        IComp::Interpret(cmd);

    } else if (cmd->IsA(UNGROUP_CMD)) {
        Component* edComp = cmd->GetEditor()->GetComponent();
        
        if (edComp == (Component*) this) {
            GraphicComps::Interpret(cmd);
            Propagate(cmd);
        } else {
            GraphicComps::Interpret(cmd);
        }

    } else if (cmd->IsA(COMPCHECK_CMD)) {
        CompCheckCmd* ccmd = (CompCheckCmd*) cmd;
        Iterator i;
        IComp::Interpret(cmd);
        for (First(i); !Done(i) && ccmd->IsOK(); Next(i)) {
            GetComp(i)->Interpret(cmd);
        }
    } else if (
        !cmd->IsA(SCENE_CMD) && !cmd->IsA(MONOSCENE_CMD) && 
        !cmd->IsA(NAVIGATE_CMD)
    ) {
        GraphicComps::Interpret(cmd);
        Propagate(cmd);
    }
}

void IGraphicComps::Uninterpret (Command* cmd) {
    if (cmd->IsA(EDIT_CMD)) {
        IComp::Uninterpret(cmd);
        Unpropagate(cmd);

    } else if (cmd->IsA(INFO_CMD)) {
        IComp::Uninterpret(cmd);

    } else if (cmd->IsA(UNGROUP_CMD)) {
        Component* edComp = cmd->GetEditor()->GetComponent();

        if (edComp == (Component*) this) {
            GraphicComps::Uninterpret(cmd);
            Unpropagate(cmd);
        } else {
            GraphicComps::Uninterpret(cmd);
        }
    } else if (
        !cmd->IsA(SCENE_CMD) && !cmd->IsA(MONOSCENE_CMD) && 
        !cmd->IsA(NAVIGATE_CMD)
    ) {
        GraphicComps::Uninterpret(cmd);
        Unpropagate(cmd);
    }
}

void IGraphicComps::Read (istream& in) {
    GraphicComps::Read(in);
    ReadStateVars(in);
}

void IGraphicComps::Write (ostream& out) {
    GraphicComps::Write(out);
    WriteStateVars(out);
}

ClassId IGraphicComps::GetSubstId(const char*& delim) {
    delim = igrcomps_delim;
    return GRAPHIC_COMPS;
}

ClassId IGraphicComps::GetClassId () { return IGRAPHIC_COMPS; }

boolean IGraphicComps::IsA (ClassId id) {
    return IGRAPHIC_COMPS == id || IComp::IsA(id);
}

/*****************************************************************************/

IGraphicViews::IGraphicViews (IGraphicComps* subj) : IView (subj) { _isel=nil;}

Selection* IGraphicViews::SelectAll() { return GraphicViews::SelectAll(); }
Selection* IGraphicViews::ViewContaining(Coord x, Coord y) {
    return GraphicViews::ViewContaining(x, y);
}
Selection* IGraphicViews::ViewsContaining(Coord x, Coord y) {
    return GraphicViews::ViewsContaining(x, y);
}
Selection* IGraphicViews::ViewIntersecting(
    Coord l, Coord b , Coord r, Coord t
) {
    return GraphicViews::ViewIntersecting(l, b, r, t);
}
Selection* IGraphicViews::ViewsIntersecting(
    Coord l, Coord b, Coord r , Coord t
) {
    return GraphicViews::ViewsIntersecting(l, b, r, t);
}
Selection* IGraphicViews::ViewsWithin(Coord l, Coord b, Coord r, Coord t) {
    return GraphicViews::ViewsWithin(l, b, r, t);
}

Manipulator* IGraphicViews::CreateManipulator (
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
	m = GraphicViews::CreateManipulator(v, e, rel, tool);
    }
    return m;
}

Command* IGraphicViews::InterpretManipulator (Manipulator* m) {
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
	cmd = GraphicViews::InterpretManipulator(m);
    }
    return cmd;
}

void IGraphicViews::DrawHandles () {
    GraphicViews::DrawHandles();
}

void IGraphicViews::RedrawHandles () {
    GraphicViews::RedrawHandles();
}

void IGraphicViews::EraseHandles () {
    GraphicViews::EraseHandles();
}

void IGraphicViews::InitHandles () {
    GraphicViews::InitHandles();
}

/*****************************************************************************/

ClassId GroupCode::GetClassId () { return IGROUP_CODE; }

boolean GroupCode::IsA(ClassId id) {
    return IGROUP_CODE == id || GraphicCodeView::IsA(id);
}

GroupCode::GroupCode (IGraphicComps* subj) : GraphicCodeView(subj) {}

IGraphicComps* GroupCode::GetIGraphicComps () {
    return (IGraphicComps*) GetSubject(); 
}

const char* GroupCode::GetGHeader () { return "picture"; }
const char* GroupCode::GetCVHeader () { return "grview"; }

boolean GroupCode::Definition (ostream& out) {
    Iterator i;
    boolean ok = true;

    IGraphicComps* grcomps = GetIGraphicComps();
    Graphic* picture= grcomps->GetGraphic();

    SubclassNameVar* cnamer = grcomps->GetCClassNameVar();
    SubclassNameVar* vnamer = grcomps->GetVClassNameVar();
    SubclassNameVar* gnamer = grcomps->GetGClassNameVar();
    MemberNameVar* mnamer = grcomps->GetMemberNameVar();

    const char* mname = mnamer->GetName();
    const char* cname = cnamer->GetName();
    const char* vname = vnamer->GetName();
    const char* gname = gnamer->GetName();

    if (_emitInstanceDecls || _emitGraphicState) {
	ok = ok && GraphicCodeView::Definition(out);
        ok = ok && Iterate(out);

    } else if (_emitExpHeader) {
        ok = ok && GraphicCodeView::Definition(out);
        if (_emitGraphicComp) {
            if (!cnamer->IsSubclass() || !vnamer->IsSubclass()) {
                if (_scope && mnamer->GetExport()) {
                    if (!_namelist->Search("grcomp")) {
                        _namelist->Append("grcomp");
                        out << "#include <Unidraw/Components/grcomp.h> \n";
                    } 
                    if (!_namelist->Search("grview")) {
                        _namelist->Append("grview");
                        out << "#include <Unidraw/Components/grview.h> \n";
                    } 
                }
            }
        }
        ok = ok && Iterate(out);

    } else if (_emitCorehHeader) {
        boolean found = false;

        if (gnamer->IsSubclass() && strcmp(gname, _classname) == 0) {
            if (!_namelist->Search("picture")) {
                _namelist->Append("picture");
                out << "#include <Unidraw/Graphic/picture.h> \n";
                found = true;
            }
        }
        if (_emitGraphicComp) {
            if (cnamer->IsSubclass() && strcmp(cname, _classname) == 0) {
                if (!_namelist->Search("grcomp")) {
                    _namelist->Append("grcomp");
                    out << "#include <Unidraw/Components/grcomp.h> \n";
                    found = true;
                }
            }
            if (vnamer->IsSubclass() && strcmp(vname, _classname) == 0) {
                if (!_namelist->Search("grview")) {
                    _namelist->Append("grview");
                    out << "#include <Unidraw/Components/grview.h> \n";
                    found = true;
                }
            }
        }
        if (!found) {
            ok = ok && Iterate(out);
        }
    } else if (_emitInstanceInits) {
        out << "    {\n";
        if (_emitGraphicComp) {
            out << "        " << mname << "_gr";
        } else {
            out << "        " << mname;
        }
        out << " = new " << gname << ";\n";
        ok = WriteGraphicInits(picture, out);
        if (_emitGraphicComp) {
            out << "        " << mname << " = new " << cname << "(";
            out << mname << "_gr);\n";
        }
        out << "    }\n";

        for (First(i); !Done(i); Next(i)) {
            GraphicCodeView* cv = (GraphicCodeView*) GetView(i);
            ok = ok && cv->Definition(out);
            const char* kmname = cv->GetIComp()->GetMemberNameVar()->GetName();
            
            out << "    " << mname << "->Append(";
            out << kmname << ");\n";
        }

    } else {
	ok = ok && GraphicCodeView::Definition(out);
        ok = ok && Iterate(out);
    }
    return ok && out.good();
}

boolean GroupCode::GCoreConstDecls(ostream& out) { 
    out << "(Graphic* = nil);\n";
    return out.good();
}

boolean GroupCode::GCoreConstInits(ostream& out) {
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* baseclass = gnamer->GetBaseClass();

    out << "(Graphic* gr) : ";
    out << baseclass << "(gr) {}\n\n";

    return out.good();
}

boolean GroupCode::GConstDecls(ostream& out) {
    out << "(Graphic* = nil);\n";
    out << "    virtual Graphic* Copy();\n";
    return out.good();
}

boolean GroupCode::GConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* gname = gnamer->GetName();

    out << "(Graphic* gr) : ";
    out << coreclass << "(gr) {}\n\n";
    out << "Graphic* " << gname << "::Copy () {\n";
    out << "    return new " << gname << "(this);\n";
    out << "}\n\n";

    return out.good();
}

boolean GroupCode::CCoreConstInits(ostream& out) {
    IComp* icomp = GetIComp();
    SubclassNameVar* cnamer = icomp->GetCClassNameVar();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* cname = cnamer->GetName();
    const char* gname = gnamer->GetName();
    const char* cbname = cnamer->GetBaseClass();
    int cid = icomp->GetCIDVar()->GetID();


    out << "(" << gname << "* gr) : ";
    out << cbname << "(gr) {\n";
    out << "    if (gr == nil) {\n";
    out << "        gr = new " << gname << ";\n";
    out << "        SetGraphic(gr);\n";
    out << "    }\n";
    out << "}\n\n";

    out << "ClassId " << cname << "_core::GetClassId () { return ";
    out << cid << ";}\n";
    out << "boolean " << cname << "_core::IsA (ClassId id) {\n";
    out << "    return id == " << cid << " || " << cbname << "::IsA(id);\n";
    out << "}\n\n";

    return out.good();
}

boolean GroupCode::CConstDecls(ostream& out) {
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* gname = gnamer->GetName();

    out << "(" << gname << "* = nil);\n\n";
    return out.good();
}

boolean GroupCode::CConstInits(ostream& out) {
    IComp* icomp = GetIComp();
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* gname = gnamer->GetName();

    out << "(" << gname << "* gr) : ";
    out << coreclass << "(gr) {}\n\n";

    return out.good();
}

boolean GroupCode::VCoreConstDecls (ostream& out) {
    boolean ok = GraphicCodeView::VCoreConstDecls(out);
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    if (gnamer->IsSubclass() && ok) {
        out << "    virtual Graphic* GetGraphic();\n";
    }
    return ok && out.good();
}

boolean GroupCode::VCoreConstInits (ostream& out) {
    boolean ok = GraphicCodeView::VCoreConstInits(out);
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    SubclassNameVar* vnamer = icomp->GetVClassNameVar();
    const char* gname = gnamer->GetName();
    const char* vname = vnamer->GetName();
    
    if (gnamer->IsSubclass() && ok) {
        out << "Graphic* " << vname << "_core::GetGraphic () {\n";
        out << "    Graphic* g = GraphicView::GetGraphic();\n\n";
        out << "    if (g == nil) {\n";
        out << "        g = new " << gname << ";\n";
        out << "        Iterator i;\n\n";
        out << "        for (First(i); !Done(i); Next(i)) {\n";
        out << "            g->Append(GetView(i)->GetGraphic());\n";
        out << "        }\n";
        out << "        SetGraphic(g);\n";
        out << "    }\n";
        out << "    return g;\n";
        out << "}\n\n";
    }
    return ok && out.good();
}

boolean GroupCode::EmitIncludeHeaders (ostream& out) {
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    SubclassNameVar* cnamer = icomp->GetCClassNameVar();
    SubclassNameVar* vnamer = icomp->GetVClassNameVar();
    
    const char* gname = gnamer->GetName();
    const char* gheader = GetGHeader();

    if (!gnamer->IsSubclass() && !_namelist->Search(gheader)) {
        _namelist->Append(gheader);
        out << "#include <Unidraw/Graphic/" << gheader << ".h> \n";
    }
    if (_emitGraphicComp) {
        if (!cnamer->IsSubclass()) {
            if (!_namelist->Search("grcomp")) {
                _namelist->Append("grcomp");
                out << "#include <Unidraw/Components/grcomp.h> \n";
            } 
        }
        if (*_classname == '\0' || _scope) {
            if (!vnamer->IsSubclass() && !_namelist->Search("grview")) {
                _namelist->Append("grview");
                out << "#include <Unidraw/Components/grview.h> \n";
            }
        } 
        if (
            strcmp(vnamer->GetName(), _classname) == 0 && gnamer->IsSubclass()
        ) {
            if (!_namelist->Search("iterator")) {
                _namelist->Append("iterator");
                out << "#include <Unidraw/iterator.h> \n";
            }
        }
    }
    return out.good();
}


