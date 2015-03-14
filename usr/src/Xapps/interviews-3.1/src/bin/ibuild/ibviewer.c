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
 * Viewer component definitions 
 */

#include "ibclasses.h"
#include "ibgrcomp.h"
#include "ibcmds.h"
#include "ibdialogs.h"
#include "ibed.h"
#include "ibvars.h"
#include "ibvarviews.h"
#include "ibviewer.h"

#include <Unidraw/catalog.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/iterator.h>
#include <Unidraw/selection.h>
#include <Unidraw/viewer.h>
#include <Unidraw/ulist.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/upage.h>
#include <Unidraw/Commands/colorcmd.h>
#include <Unidraw/Graphic/pspaint.h>

#include <stream.h>
#include <string.h>

/*****************************************************************************/

IBViewerComp::IBViewerComp (IBViewerGraphic* gr) : GrBlockComp(gr) { 
    _edVar = nil;

    if (gr != nil) {
        int w, h;
        gr->GetShape(w, h);
        SubclassNameVar* subclass = GetClassNameVar();
        subclass->SetName("Viewer");
        subclass->SetBaseClass("Viewer");
        UPage page(w, h);
        _page = page.GetGraphic()->Copy();
        GetGraphic()->Append(_page);
    }
}

IBViewerComp::~IBViewerComp () {
    delete _edVar;
}

ClassId IBViewerComp::GetClassId () { return IBVIEWER_COMP; }

boolean IBViewerComp::IsA (ClassId id) {
    return IBVIEWER_COMP == id || GrBlockComp::IsA(id);
}

IBViewerGraphic* IBViewerComp::GetIBViewerGraphic() {
    return (IBViewerGraphic*) GetGrBlockGraphic();
}

void IBViewerComp::Instantiate () {
    GrBlockComp::Instantiate();
    if (_edVar == nil) {
        _edVar = new MemberNameVar("", false, false);
    }
    _memberVar->SetExport(true);
}

void IBViewerComp::Reconfig () {
    int w, h;
    GetIBViewerGraphic()->GetShape(w, h);
    Shape* shape = GetShapeVar()->GetShape();
    shape->width = w;
    shape->height = h;
}

void IBViewerComp::Resize () {
    Iterator i;
    Picture* topgr = (Picture*) GetTop()->GetGraphic();
    Graphic* grblockgr = GetGrBlockGraphic();

    if (!topgr->IsEmpty()) {
        topgr->Align(Center, grblockgr, Center);
        topgr->Align(Center, _page, Center);

    } else {
        grblockgr->Align(Center, _page, Center);
    }
    SubNotify();
}

void IBViewerComp::Interpret(Command* cmd) {
    if (!cmd->IsA(FONT_CMD) && !cmd->IsA(BRUSH_CMD) && !cmd->IsA(ALIGN_CMD)) {
        GrBlockComp::Interpret(cmd);
    }
}

void IBViewerComp::Uninterpret(Command* cmd) {
    if (!cmd->IsA(FONT_CMD) && !cmd->IsA(BRUSH_CMD) && !cmd->IsA(ALIGN_CMD)) {
        GrBlockComp::Uninterpret(cmd);
    }
}

void IBViewerComp::SetState(const char* name, StateVar* stateVar) { 
    if (
        strcmp(name, "EditorVar") == 0 || strcmp(name, "RelatedVar") == 0
    ) {
        MemberNameVar* memberVar = (MemberNameVar*) stateVar;
        *_edVar = *memberVar;

    } else {
        GrBlockComp::SetState(name, stateVar);
    }
}

StateVar* IBViewerComp::GetState (const char* name) {
    StateVar* stateVar = nil;

    if (strcmp(name, "EditorVar") == 0) {
        stateVar = _edVar;

    } else {
        stateVar = GrBlockComp::GetState(name);
    }

    return stateVar;
}

InteractorComp& IBViewerComp::operator = (InteractorComp& comp) {
    if (comp.IsA(IBVIEWER_COMP)) {
        IBViewerComp* vcomp = (IBViewerComp*) &comp;
        *_edVar = *vcomp->GetEditorVar();

    } else if (comp.IsA(EDITOR_COMP)) {
        *_edVar = *comp.GetMemberNameVar();
        comp.SetState("ViewerVar", GetMemberNameVar());

    }
    return *this;
}

boolean IBViewerComp::IsRelatableTo (InteractorComp* comp) {
    boolean ok = false;
    if (comp->IsA(IBVIEWER_COMP) || comp->IsA(EDITOR_COMP)) {
        ok = true;
    }
    return ok;
}

void IBViewerComp::Read (istream& in) {
    Catalog* catalog = unidraw->GetCatalog();

    GrBlockComp::Read(in);
    int w, h;
    IBViewerGraphic* vgr = GetIBViewerGraphic();
    vgr->GetShape(w, h);
    UPage page(w, h);
    _page = page.GetGraphic()->Copy();
    GetGraphic()->Append(_page);
    GetGrBlockGraphic()->Align(Center, _page, Center);

    delete _edVar;
    _edVar = (MemberNameVar*) catalog->ReadStateVar(in);
    if (_memberVar != nil) {
        _memberVar->SetExport(true);
    }
}

void IBViewerComp::Write (ostream& out) {
    GrBlockComp::Write(out);
    Catalog* catalog = unidraw->GetCatalog();
    
    catalog->WriteStateVar(_edVar, out);
}

/*****************************************************************************/

IBViewerView::IBViewerView (
    IBViewerComp* subj
) : GrBlockView(subj) { _page = nil;}

IBViewerView::~IBViewerView () {
    GetGraphic()->Remove(_page);
    delete _page;
}

IBViewerComp* IBViewerView::GetIBViewerComp () {
    return (IBViewerComp*) GetSubject();
}

ClassId IBViewerView::GetClassId () { return IBVIEWER_VIEW; }

boolean IBViewerView::IsA (ClassId id) {
    return IBVIEWER_VIEW == id || GrBlockView::IsA(id);
}

GraphicComp* IBViewerView::CreateProtoComp (
    Editor* ed, Coord x0, Coord y0, Coord x1, Coord y1
) {
    ColorVar* colVar = (ColorVar*) ed->GetState("ColorVar");
    
    IBViewerComp* vcomp = (IBViewerComp*) GetIBViewerComp();
    IBViewerGraphic* vgr = vcomp->GetIBViewerGraphic();
    vgr->SetShape(x1-x0, y1-y0);
    IBViewerComp* vcopy = (IBViewerComp*) vcomp->Copy();
    
    vgr = vcopy->GetIBViewerGraphic();
    vgr->SetColors(nil, colVar->GetBgColor());
    return vcopy;
}

Graphic* IBViewerView::GetGraphic () {
    Graphic* g = GraphicView::GetGraphic();

    if (g == nil) {
        _page = GetIBViewerComp()->GetPage()->Copy();
        g = GrBlockView::GetGraphic();
        g->Append(_page);
    }
    return g;
}

void IBViewerView::Update () {
    IBViewerComp* vcomp = GetIBViewerComp();
    GrBlockPicture* grvcomp = (GrBlockPicture*) vcomp->GetGraphic();
    GrBlockPicture* grvview = (GrBlockPicture*) GetGraphic();

    GrBlockGraphic* grblockgr = vcomp->GetGrBlockGraphic();
    Graphic* page = vcomp->GetPage();

    Viewer* viewer = GetViewer();
    
    if (viewer != nil && viewer->GetGraphicView() == this) {
        boolean clipped = grvview->GetClip();
        if (clipped) {
            grvview->SetClip(false);
            IncurDamage(grvview);
            grvview->Remove(_grblockgr);
            grvview->Remove(_page);
        }
    } else {
        if (
            Different(grvcomp, grvview) || Different(grblockgr, _grblockgr) ||
            Different(page, _page)
        ) {
            IncurDamage(grvview);
            *(Graphic*)grvview = *(Graphic*)grvcomp;
            *(Graphic*)_grblockgr = *(Graphic*)grblockgr;
            *(Graphic*)_page = *(Graphic*)page;
            UpdateCanvasVar();
            IncurDamage(grvview);
        }
    }
    GraphicViews::Update();
}

InfoDialog* IBViewerView::GetInfoDialog () {
    IBEditor* ibed = (IBEditor*) GetViewer()->GetEditor();

    InfoDialog* info = new InfoDialog;
    ButtonState* state = info->GetState();

    IBViewerComp* icomp = GetIBViewerComp();
    SubclassNameVar* classNameVar = icomp->GetClassNameVar();
    MemberNameVar* memberVar = icomp->GetMemberNameVar();
    CanvasVar* canvasVar = icomp->GetCanvasVar();
    ShapeVar* shapeVar = icomp->GetShapeVar();
    MemberNameVar* edVar = icomp->GetEditorVar();

    info->Include(new SubclassNameVarView(classNameVar, state, icomp));
    info->Include(
        new MemberNameVarView(memberVar, state, icomp, "Member Name: ", false)
    );
    info->Include(new CanvasVarView(canvasVar));
    info->Include(new ShapeVarView(shapeVar, state));
    info->Include(new RelatedVarView(edVar, state, icomp, "Editor Name: "));
    return info;
}

/*****************************************************************************/

IBViewerCode::IBViewerCode (IBViewerComp* subj) : GrBlockCode(subj) {
    _unidraw = true;
}

IBViewerComp* IBViewerCode::GetIBViewerComp() {
    return (IBViewerComp*) GetSubject();
}

ClassId IBViewerCode::GetClassId () { return IBVIEWER_CODE; }

boolean IBViewerCode::IsA(ClassId id) {
    return IBVIEWER_CODE == id || GrBlockCode::IsA(id);
}

boolean IBViewerCode::Definition (ostream& out) {
    char coreclass[CHARBUFSIZE];
    boolean ok = true;
    Iterator i;

    IBViewerComp* viewerComp = GetIBViewerComp();
    ShapeVar* svar = viewerComp->GetShapeVar();
    Shape* shape = svar->GetShape();
    SubclassNameVar* snamer = viewerComp->GetClassNameVar();
    MemberNameVar* mnamer = viewerComp->GetMemberNameVar();

    const char* subclass = snamer->GetName();
    const char* baseclass = snamer->GetBaseClass();
    const char* mname = mnamer->GetName();

    MemberNameVar* edVar = viewerComp->GetEditorVar();
    const char* edname = edVar->GetName();
    InteractorComp* dummy;

    int w = shape->width;
    int h = shape->height;

    GetCoreClassName(coreclass);

    if (*edname == '\0') {
        if (_err_count < 10) {
            strcat(_errbuf, mname);
            strcat(_errbuf, " has undefined Editor.\n");
            _err_count++;
        }
        return false;

    } else if (!Search(edVar, dummy)) {
        if (_err_count < 10) {
            strcat(_errbuf, mname);
            strcat(
                _errbuf, "'s Editor is not in the same hierachy.\n"
            );
            _err_count++;
        }
        return false;
    }

    _emitGraphicComp = true;

    if (
        _emitClassHeaders || _emitHeaders || _emitProperty || _emitForward ||
        _emitInstanceDecls
    ) {
        ok = ok && GrBlockCode::Definition(out);

    } else if (_emitExpHeader) {
        if (!snamer->IsSubclass()) {
            if (_scope && mnamer->GetExport()&&!_namelist->Search("viewer")) {
                _namelist->Append("viewer");
                out << "#include <Unidraw/viewer.h> \n";
            }
        } else {
            ok = CodeView::Definition(out);
        }
        ok = ok && Iterate(out);

    } else if (_emitCorehHeader) {
        if (snamer->IsSubclass() && strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("viewer")) {
                _namelist->Append("viewer");
                out << "#include <Unidraw/viewer.h>\n";
            }
        }
        ok = ok && Iterate(out);

    } else if (_emitInstanceInits) {
        if (!_instancelist->Find((void*)mname)) {
            _instancelist->Append(new UList((void*)mname));

            ok = ok && EmitGraphicState(out);

            out << "    GraphicComps* " << mname;
            out << "_comp = (GraphicComps*) GetComponent();\n";
            out << "    GraphicView* " << mname;
            out << "_view = (GraphicView*) " << mname;
            out << "_comp->Create(COMPONENT_VIEW);\n";
            out << "    " << mname << "_comp->Attach(" << mname << "_view);\n";
            out << "    " << mname << "_view->Update();\n";
            out << "    UPage* " << mname << "_page = new UPage(";
            out << w << ", " << h << ");\n";
            out << "    Grid* " << mname << "_grid = new Grid(";
            out << w << ", " << h << ", 8.0, 8.0);\n";
            out << "    " << mname << "_grid->Visibility(false);\n";

            BeginInstantiate(out);
            out << "(";
            InstanceName(out);
            out << "this, " << mname;
            out << "_view, " << mname << "_page, " << mname << "_grid, ";
            out << w << ", " << h << ");\n";

            for(First(i); !Done(i); Next(i)) {
                CodeView* kid = (CodeView*) GetView(i);
                ok = ok && EmitInstanceDecls(kid, out);
            }
            ok = ok && Iterate(out);
            for(First(i); !Done(i); Next(i)) {
                CodeView* kid = (CodeView*) GetView(i);
                MemberNameVar* kmnamer = kid->GetIComp()->GetMemberNameVar();
                const char* kmname = kmnamer->GetName();
                out << "    " << mname << "_comp->Append(";
                out << kmname << ");\n";
            }
            out << "    " << mname;
            out << "_page->GetGraphic()->Align(Center, " << mname;
            out << "_comp->GetGraphic(), Center);\n";
            out << "    " << mname << "_comp->Notify();\n";
        }

    } else if (
	_emitBSDecls || _emitBSInits ||
	_emitFunctionDecls || _emitFunctionInits
    ) {
	ok = true;

    } else if (
        _emitCreatorHeader || _emitCreatorSubj || _emitCreatorView
    ) {
        ok = ok && Iterate(out);

    } else if (
        _emitCoreDecls || _emitCoreInits || _emitClassDecls || _emitClassInits
    ) {
	ok = ok && GrBlockCode::Definition(out);
        
    } else if (_emitMain) {
	ok = ok && GrBlockCode::Definition(out);
        
    }
    _emitGraphicComp = false;

    return out.good() && ok;
}

boolean IBViewerCode::CoreConstDecls(ostream& out) { 
    out << "(\n";
    out << "        const char*, Editor*, GraphicView*, UPage*,\n";
    out << "        Grid* = nil, Coord = 0, Coord = 0, Orientation = Normal\n";
    out << "   );\n";
    return out.good();
}

boolean IBViewerCode::CoreConstInits(ostream& out) {
    InteractorComp* icomp = GetIntComp();
    SubclassNameVar* snamer = icomp->GetClassNameVar();
    const char* baseclass = snamer->GetBaseClass();
    const char* subclass = snamer->GetName();

    out << "(\n";
    out << "    const char* n, Editor* ed, GraphicView* gv, ";
    out << "UPage* page, Grid* grid,\n";
    out << "    Coord w, Coord h, Orientation o\n) : ";
    out << baseclass << "(n, ed, gv, page, grid, w, h, o) {\n";
    out << "    SetClassName(\"" << subclass << "\");\n";
    out << "}\n\n";

    return out.good();
}

boolean IBViewerCode::ConstDecls(ostream& out) {
    out << "(\n";
    out << "        const char*, Editor*, GraphicView*, UPage*,\n";
    out << "        Grid* = nil, Coord = 0, Coord = 0, Orientation = Normal\n";
    out << "   );\n";
    return out.good();
}

boolean IBViewerCode::ConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n";
    out << "    const char* n, Editor* ed, GraphicView* gv, ";
    out << "UPage* page, Grid* grid,\n";
    out << "    Coord w, Coord h, Orientation o\n) : ";
    out << coreclass << "(n, ed, gv, page, grid, w, h, o) {}\n\n";

    return out.good();
}

boolean IBViewerCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();

    GrBlockCode::EmitIncludeHeaders(out);
    
    if (!snamer->IsSubclass() && !_namelist->Search("viewer")) {
        _namelist->Append("viewer");
        out << "#include <Unidraw/viewer.h>\n";
    }
    return out.good();
}

/*****************************************************************************/

IBViewerGraphic::IBViewerGraphic (
    CanvasVar* c, Graphic* g
) : GrBlockGraphic(c, g) {}

Graphic* IBViewerGraphic::Copy () {
    Graphic* copy = new IBViewerGraphic(nil, this);
    return copy;
}

ClassId IBViewerGraphic::GetClassId () { return IBVIEWER_GRAPHIC; }
boolean IBViewerGraphic::IsA (ClassId id) { return IBVIEWER_GRAPHIC == id; }

void IBViewerGraphic::Read (istream& in) {
    GrBlockGraphic::Read(in);
    in >> _w >> _h;
}

void IBViewerGraphic::Write (ostream& out) {
    GrBlockGraphic::Write(out);
    out << " " << _w << " " << _h << " ";
}

