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
 * Glue component definitions.
 */

#include "ibclasses.h"
#include "ibcmds.h"
#include "ibed.h"
#include "ibglue.h"
#include "ibmanips.h"
#include "ibvars.h"

#include <Unidraw/clipboard.h>
#include <Unidraw/ulist.h>
#include <Unidraw/Graphic/graphic.h>
#include <Unidraw/Graphic/pspaint.h>
#include <Unidraw/iterator.h>
#include <Unidraw/Commands/colorcmd.h>
#include <Unidraw/Tools/tool.h>
#include <Unidraw/viewer.h>

#include <InterViews/paint.h>
#include <InterViews/painter.h>
#include <InterViews/rubrect.h>
#include <InterViews/shape.h>
#include <InterViews/transformer.h>

#include <math.h>
#include <stdlib.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/

static Pattern* horizBars;
static Pattern* vertBars;

/*****************************************************************************/

GlueGraphic* GlueComp::GetGlueGraphic () { return (GlueGraphic*) GetGraphic();}
ClassId GlueComp::GetClassId () { return GLUE_COMP; }
boolean GlueComp::IsA (ClassId id) {return GLUE_COMP == id || HVComp::IsA(id);}

GlueComp::GlueComp (GlueGraphic* g) : HVComp(g) {
    if (g != nil) {
	IBShape* ibshape = GetShapeVar()->GetShape();
        int nat, shr, str;
        g->GetShape(nat, shr, str);

        if (g->GetOrientation() == Horizontal) {
            ibshape->Rect(nat, 0);
            ibshape->Rigid(shr, str, vfil, vfil);
            GetClassNameVar()->SetName("HGlue");
            GetClassNameVar()->SetBaseClass("HGlue");
	    ibshape->hnat = ibshape->hstr = ibshape->hshr = true;
        } else {
            ibshape->Rect(0, nat);
            ibshape->Rigid(hfil, hfil, shr, str);
            GetClassNameVar()->SetName("VGlue");
            GetClassNameVar()->SetBaseClass("VGlue");
	    ibshape->vnat = ibshape->vstr = ibshape->vshr = true;
        }
    }
}

void GlueComp::Interpret (Command* cmd) {
    if (cmd->IsA(GLUEVISIBILITY_CMD)) {
        PSBrush* br = ((GlueVisibilityCmd*) cmd)->GetBrush();
        GlueGraphic* g = GetGlueGraphic();
        cmd->Store(this, new VoidData(g->GetBrush()));
        g->SetBrush(br);
        Notify();

    } else if (cmd->IsA(COLOR_CMD)) {
        ColorCmd* colorcmd = (ColorCmd*) cmd;
        if (colorcmd->GetFgColor() == nil) {
            HVComp::Interpret(cmd);
        }
    } else if (!cmd->IsA(BRUSH_CMD) && !cmd->IsA(FONT_CMD)) {
        HVComp::Interpret(cmd);
    }
}

void GlueComp::Uninterpret (Command* cmd) {
    if (cmd->IsA(GLUEVISIBILITY_CMD)) {
        VoidData* vd = (VoidData*) cmd->Recall(this);
        PSBrush* br = (PSBrush*) vd->_void;
        GlueGraphic* g = GetGlueGraphic();
        g->SetBrush(br);
        Notify();

    } else if (cmd->IsA(COLOR_CMD)) {
        ColorCmd* colorcmd = (ColorCmd*) cmd;
        if (colorcmd->GetFgColor() == nil) {
            HVComp::Uninterpret(cmd);
        }
    } else if (!cmd->IsA(BRUSH_CMD) && !cmd->IsA(FONT_CMD)) {
        HVComp::Uninterpret(cmd);
    }
}

HVGraphic* GlueComp::InitGraphic (Orientation o, int) {
    return new GlueGraphic(0, o, GetCanvasVar(), nil);
}

/*****************************************************************************/

ClassId GlueView::GetClassId () { return GLUE_VIEW; }
boolean GlueView::IsA (ClassId id) {return GLUE_VIEW == id || HVView::IsA(id);}
GlueView::GlueView (GlueComp* subj) : HVView(subj) { _rigid = false;}

Manipulator* GlueView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Manipulator* m = nil;

    if (tool->IsA(IBGRAPHIC_COMP_TOOL)) {
	m = InteractorView::CreateManipulator(v, e, rel, tool);

    } else if (tool->IsA(GRAPHIC_COMP_TOOL)) {
	_rigid = e.shift;
	DragManip* dm = (DragManip*) HVView::CreateManipulator(v, e, rel, tool);
	m = new DragManip(v, dm->GetRubberband(), rel, tool);
	delete dm;
    } else {
	m = HVView::CreateManipulator(v, e, rel, tool);
    }
    return m;
}

Command* GlueView::InterpretManipulator (Manipulator* m) {
    DragManip* dm = (DragManip*) m;
    Editor* ed = dm->GetViewer()->GetEditor();
    Tool* tool = dm->GetTool();
    Transformer* rel = dm->GetTransformer();
    Command* cmd = nil;

    if (tool->IsA(IBGRAPHIC_COMP_TOOL)) {
	cmd = InteractorView::InterpretManipulator(m);

    } else if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        Coord x0, y0, x1, y1;
        StretchingRect* stretchRect = (StretchingRect*) dm->GetRubberband();
        stretchRect->GetCurrent(x0, y0, x1, y1);
        NormalRect(x0, y0, x1, y1);

        if (rel != nil) {
            rel->InvTransformRect(x0, y0, x1, y1);
        }
        GetABSCoord(ed, x0, y0, x1, y1);

        HVComp* comp = InitComp(x0, y0, x1, y1);
        GlueGraphic* gr = (GlueGraphic*) comp->GetGraphic();
	ColorVar* colVar = (ColorVar*) ed->GetState("ColorVar");
	if (colVar != nil) {
            gr->SetColors(colVar->GetFgColor(), colVar->GetBgColor());
        }

	_rigid = _rigid || dm->GraspEvent().shift;
	if (_rigid) {
	    IBShape* ibshape = comp->GetShapeVar()->GetShape();
            if (gr->GetOrientation() == Horizontal) {
                ibshape->Rigid(0, 0, vfil, vfil);
            } else {
                ibshape->Rigid(hfil, hfil, 0, 0);
            }
	}
        cmd = new MacroCmd(
            ed, new PasteCmd(ed, new Clipboard(comp)),
            new PlaceCmd(ed, x0, y0, x1-1, y1-1, new Clipboard(comp))
        );

    } else {
        cmd = HVView::InterpretManipulator(m);
    }
    return cmd;
}

HVComp* GlueView::InitComp (Coord x0, Coord y0, Coord x1, Coord y1) {
    HVGraphic* gs = GetHVComp()->GetHVGraphic();
    Orientation orient = gs->GetOrientation();
    int nat = (orient == Horizontal) ? abs(x0 - x1) : abs(y0 - y1);
    GlueGraphic* g = new GlueGraphic(nat, orient, nil, gs);
    return new GlueComp(g);
}

/*****************************************************************************/

ClassId GlueCode::GetClassId () { return GLUE_CODE; }
boolean GlueCode::IsA(ClassId id) { return GLUE_CODE==id || CodeView::IsA(id);}

GlueCode::GlueCode (GlueComp* subj) : CodeView(subj) {}

void GlueCode::Update () {
    CodeView::Update();
    InteractorComp* subj = GetIntComp();
    Graphic* gr = subj->GetGraphic();
    gr->SetColors(nil, gr->GetBgColor());
    gr->SetFont(nil);
}

GlueComp* GlueCode::GetGlueComp () { return (GlueComp*) GetSubject(); }

boolean GlueCode::Definition (ostream& out) {
    boolean ok = true;
    if (
	_emitInstanceDecls || _emitForward || 
        _emitProperty || _emitClassHeaders || _emitHeaders
    ) {
        return CodeView::Definition(out);

    } else if (_emitExpHeader) {
	InteractorComp* icomp = GetIntComp();
	MemberNameVar* mnamer = icomp->GetMemberNameVar();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        if (!snamer->IsSubclass()) {
            if (_scope && mnamer->GetExport() && !_namelist->Search("glue")) {
                _namelist->Append("glue");
                out << "#include <InterViews/glue.h>\n";
            }
        } else {
            ok = ok && CodeView::Definition(out);
        }

    } else if (_emitCorehHeader) {
	InteractorComp* icomp = GetIntComp();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        const char* subclass = snamer->GetName();
        if (snamer->IsSubclass() && strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("glue")) {
                _namelist->Append("glue");
                out << "#include <InterViews/glue.h>\n";
            }
        }
    } else if (_emitInstanceInits) {
        int nat, shr, str;
        InteractorComp* icomp = GetIntComp();
        const char* mname = icomp->GetMemberNameVar()->GetName();

        if (!_instancelist->Find((void*) mname)) {
            _instancelist->Append(new UList((void*)mname));
            
            char coreclass[CHARBUFSIZE];
            char HGlueClass[CHARBUFSIZE];
            char VGlueClass[CHARBUFSIZE];
            
            GetCoreClassName(coreclass);
            strcpy(HGlueClass, coreclass);
            strcat(HGlueClass, "_HGlue");
            strcpy(VGlueClass, coreclass);
            strcat(VGlueClass, "_VGlue");
            
            boolean export = icomp->GetMemberNameVar()->GetExport();
            const char* classname = icomp->GetClassNameVar()->GetName();
            if (icomp->GetClassNameVar()->IsSubclass()) {
                BeginInstantiate(out);
                out << "(";

            } else {
                if (export && !_emitMain) {
                    out << "    " << mname << " = new ";
                } else {
                    out << "    " << classname << "* ";
                    out << mname << " = new ";
                }
                if (strcmp(classname, "HGlue") == 0) {
                    out << HGlueClass << "(";
                } else {
                    out << VGlueClass << "(";
                }
            }            
            Shape* shape = GetGlueComp()->GetShapeVar()->GetShape();
	    GlueGraphic* gr = GetGlueComp()->GetGlueGraphic();

	    if (gr->GetOrientation() == Horizontal) {
		nat = shape->width;
		shr = shape->hshrink;
		str = shape->hstretch;
	    } else {
		nat = shape->height;
                shr = shape->vshrink;
                str = shape->vstretch;
	    }
            InstanceName(out);
            out << nat << ", " << shr << ", " << str << ")";
            EndInstantiate(out);
	}

    } else if (
        _emitBSDecls || _emitBSInits || 
	_emitFunctionDecls || _emitFunctionInits
    ) {
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

boolean GlueCode::CoreConstDecls(ostream& out) { 
    out << "(const char*, int nat, int shr, int str);\n\n";
    out << "protected:\n";
    out << "    virtual void Resize();\n";
    return out.good();
}

boolean GlueCode::CoreConstInits(ostream& out) {
    InteractorComp* icomp = GetIntComp();
    SubclassNameVar* snamer = icomp->GetClassNameVar();
    const char* subclass = snamer->GetName();
    const char* baseclass = snamer->GetBaseClass();
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n    const char* name, int nat, int shr, int str\n) : ";
    out << baseclass << "(name, nat, shr, str) {\n";
    out << "    SetClassName(\"" << subclass << "\");\n";
    out << "}\n\n";

    out << "void " << coreclass << "::Resize () {\n";
    out << "    " << baseclass << "::Resize();\n";
    out << "    canvas->SetBackground(output->GetBgColor());\n";
    out << "}\n\n";
    return out.good();
}

boolean GlueCode::ConstDecls(ostream& out) {
    out << "(const char*, int nat, int shr, int str);\n";
    return out.good();
}

boolean GlueCode::ConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n    const char* name, int nat, int shr, int str\n) : ";
    out << coreclass << "(name, nat, shr, str) {}\n\n";

    return out.good();
}

boolean GlueCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();

    if (!snamer->IsSubclass() && !_namelist->Search("glue")) {
        _namelist->Append("glue");
        out << "#include <InterViews/glue.h> \n";
    }
    return out.good();
}

/*****************************************************************************/

GlueGraphic::GlueGraphic (
    int nat, Orientation o, CanvasVar* c, Graphic* g
) : HVGraphic(c, g) {
    Init(nat, nat, (o == Horizontal) ? hfil : vfil, o);
}

int GlueGraphic::MinorAxisSize () { return round(.5*cm); }

Graphic* GlueGraphic::Copy () {
    return new GlueGraphic(_natural, GetOrientation(), nil, this);
}

void GlueGraphic::draw (Canvas* c, Graphic* gs) {
    if (horizBars == nil) {
        horizBars = new Pattern(0x8888);
        vertBars = new Pattern(0xf000);
        Ref(horizBars);
        Ref(vertBars);
    }
    update(gs);
    Coord xmax, ymax;

    if (GetCanvasVar() == nil) {
        if (_orientation == Horizontal) {
            xmax = _natural;
            ymax = MinorAxisSize() - 1;
        } else {
            xmax = MinorAxisSize() - 1;
            ymax = _natural;
        }
    } else {
        xmax = GetCanvasVar()->xmax();
        ymax = GetCanvasVar()->ymax();
    }
    if (GetBrush()->None()) {
        _p->ClearRect(c, 0, 0, xmax, ymax);
    } else {
        _p->SetPattern((_orientation == Horizontal) ? vertBars : horizBars);
        _p->FillRect(c, 0, 0, xmax, ymax);
    }
}
