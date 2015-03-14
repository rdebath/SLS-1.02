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
 * Border component definitions.
 */

#include "ibborder.h"
#include "ibclasses.h"
#include "ibcmds.h"
#include "ibed.h"
#include "ibvars.h"

#include <Unidraw/iterator.h>
#include <Unidraw/ulist.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/manips.h>
#include <Unidraw/viewer.h>
#include <Unidraw/Commands/macro.h>
#include <Unidraw/Commands/colorcmd.h>
#include <Unidraw/Graphic/geomobjs.h>
#include <Unidraw/Graphic/graphic.h>
#include <Unidraw/Tools/tool.h>

#include <InterViews/paint.h>
#include <InterViews/painter.h>
#include <InterViews/rubrect.h>
#include <InterViews/shape.h>
#include <InterViews/transformer.h>

#include <math.h>
#include <stream.h>
#include <string.h>
#include <stdlib.h>

/*****************************************************************************/

BorderGraphic* BorderComp::GetBorderGraphic () {
    return (BorderGraphic*) GetGraphic();
}

ClassId BorderComp::GetClassId () { return BORDER_COMP; }
boolean BorderComp::IsA (ClassId id) {return BORDER_COMP==id||HVComp::IsA(id);}

BorderComp::BorderComp (BorderGraphic* g) : HVComp(g) {
    if (g != nil) {
        ShapeVar* shapeVar = GetShapeVar();
        Shape* shape = shapeVar->GetShape();
        int nat, shr, str;
        g->GetShape(nat, shr, str);

        if (g->GetOrientation() == Horizontal) {
            shape->Rect(nat, g->MinorAxisSize());
            shape->Rigid(shr, str, 0, 0);
            GetClassNameVar()->SetName("HBorder");
            GetClassNameVar()->SetBaseClass("HBorder");

        } else {
            shape->Rect(g->MinorAxisSize(), nat);
            shape->Rigid(0, 0, shr, str);
            GetClassNameVar()->SetName("VBorder");
            GetClassNameVar()->SetBaseClass("VBorder");
        }
    }
}

void BorderComp::Interpret (Command* cmd) {
    if (cmd->IsA(BRUSH_CMD) && !cmd->IsA(GLUEVISIBILITY_CMD)) {
	BrushCmd* brushcmd = (BrushCmd*) cmd;

	BorderGraphic* g = GetBorderGraphic();
	int w = g->MinorAxisSize();
	cmd->Store(this, new VoidData((void*) w));
        w = brushcmd->GetBrush()->Width();

	Shape* shape = GetShapeVar()->GetShape();
	int cw, ch;
        cw = _canvasVar->Width();
        ch = _canvasVar->Height();
        if (g->GetOrientation() == Horizontal) {
            shape->height = w; 
            _canvasVar->SetSize(cw, w);
        } else {
            shape->width = w;
            _canvasVar->SetSize(w, ch);
        }
        g->SetMinorAxisSize(w);
        Reconfig();
        Notify();
        Propagate(cmd);

    } else if (cmd->IsA(COLOR_CMD)) {
        ColorCmd* colorcmd = (ColorCmd*) cmd;
        if (colorcmd->GetFgColor() != nil) {
            HVComp::Interpret(cmd);
        }
    } else if (!cmd->IsA(FONT_CMD)){
        HVComp::Interpret(cmd);
    }
}

void BorderComp::Uninterpret (Command* cmd) {
    if (cmd->IsA(BRUSH_CMD) && !cmd->IsA(GLUEVISIBILITY_CMD)) {
	BorderGraphic* g = GetBorderGraphic();
        VoidData* bd = (VoidData*) cmd->Recall(this);
        int w = (int) bd->_void;

	int cw, ch;
        cw = _canvasVar->Width();
        ch = _canvasVar->Height();
	Shape* shape = GetShapeVar()->GetShape();
        if (g->GetOrientation() == Horizontal) {
            shape->height = w;
            _canvasVar->SetSize(cw, w);
        } else {
            shape->width = w;
            _canvasVar->SetSize(w, ch);
        }
        g->SetMinorAxisSize(w);
        Reconfig();
        Notify();
        Unpropagate(cmd);

    } else if (cmd->IsA(COLOR_CMD)) {
        ColorCmd* colorcmd = (ColorCmd*) cmd;
        if (colorcmd->GetFgColor() != nil) {
            HVComp::Uninterpret(cmd);
        }
    } else if (!cmd->IsA(FONT_CMD)){
        HVComp::Uninterpret(cmd);
    }
}

HVGraphic* BorderComp::InitGraphic (Orientation o, int w) {
    return new BorderGraphic(o, GetCanvasVar(), nil, 0, w);
}

/*****************************************************************************/

ClassId BorderView::GetClassId () { return BORDER_VIEW; }
boolean BorderView::IsA (ClassId id) {return BORDER_VIEW==id||HVView::IsA(id);}
BorderView::BorderView (BorderComp* subj) : HVView(subj) { }

void BorderView::Update () {
    int w;
    BorderGraphic* gcomp = (BorderGraphic*) GetGraphicComp()->GetGraphic();
    BorderGraphic* gview = (BorderGraphic*) GetGraphic();
    w = gcomp->MinorAxisSize();
    gview->SetMinorAxisSize(w);
    HVView::Update();
    gview->SetBrush(stdgraphic->GetBrush());
}

Command* BorderView::InterpretManipulator (Manipulator* m) {
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

        BrushVar* brVar = (BrushVar*) ed->GetState("BrushVar");
	int w = brVar->GetBrush()->Width();
        HVGraphic* gs = GetHVComp()->GetHVGraphic();
	Orientation orient = gs->GetOrientation();
	if(orient == Horizontal) {
	    y0 = y0 - w/2;
	    y1 = y0 + w;
	} else {
	    x0 = x0 - w/2;
	    x1 = x0 + w;
	}
        HVComp* comp = InitComp(x0, y0, x1, y1);
	HVGraphic* gr = (HVGraphic*) comp->GetGraphic();

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

HVComp* BorderView::InitComp (Coord l, Coord b, Coord r, Coord t) {
    HVGraphic* gs = GetHVComp()->GetHVGraphic();
    BorderGraphic* g;
    Orientation orient = gs->GetOrientation();
    if (orient == Horizontal) {
        g = new BorderGraphic(orient, nil, gs, 0, t-b);
    } else {
        g = new BorderGraphic(orient, nil, gs, 0, r-l);
    }
    return new BorderComp(g);
}

/*****************************************************************************/

ClassId BorderCode::GetClassId () { return BORDER_CODE; }
boolean BorderCode::IsA(ClassId id){return BORDER_CODE==id||CodeView::IsA(id);}

BorderCode::BorderCode (BorderComp* subj) : CodeView(subj) {}

void BorderCode::Update () {
    CodeView::Update();
    InteractorComp* subj = GetIntComp();
    Graphic* gr = subj->GetGraphic();
    gr->SetColors(gr->GetFgColor(), nil);
    gr->SetFont(nil);
}

BorderComp* BorderCode::GetBorderComp () { return (BorderComp*) GetSubject(); }

boolean BorderCode::Definition (ostream& out) {
    boolean ok = true;
    if (
        _emitProperty || _emitInstanceDecls || 
        _emitForward || _emitClassHeaders || _emitHeaders
    ) {
	return CodeView::Definition(out);
        
    } else if (_emitExpHeader) {
	InteractorComp* icomp = GetIntComp();
	MemberNameVar* mnamer = icomp->GetMemberNameVar();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        
        if (!snamer->IsSubclass()) {
            if (
                _scope && mnamer->GetExport() && !_namelist->Search("border")
            ) {
                _namelist->Append("border");
                out << "#include <InterViews/border.h>\n";
            }
        } else {
            ok = ok && CodeView::Definition(out);
        }
    } else if (_emitCorehHeader) {
	InteractorComp* icomp = GetIntComp();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        const char* subclass = snamer->GetName();
        if (snamer->IsSubclass() && strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("border")) {
                _namelist->Append("border");
                out << "#include <InterViews/border.h>\n";
            }
        }
        
    } else if (_emitInstanceInits) {
        InteractorComp* icomp = GetIntComp();
	const char* mname = icomp->GetMemberNameVar()->GetName();

	if (!_instancelist->Find((void*)mname)) {
            _instancelist->Append(new UList((void*) mname));
            BorderComp* bcomp = (BorderComp*) icomp;
	    int width = bcomp->GetBorderGraphic()->MinorAxisSize();

            BeginInstantiate(out);
            out << "(";
            InstanceName(out, ", ");
            out << width << " )";
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

boolean BorderCode::CoreConstDecls(ostream& out) { 
    out << "(const char*, int);\n";
    return out.good();
}

boolean BorderCode::CoreConstInits(ostream& out) {
    InteractorComp* icomp = GetIntComp();
    SubclassNameVar* snamer = icomp->GetClassNameVar();
    const char* subclass = snamer->GetName();
    const char* baseclass = snamer->GetBaseClass();

    out << "(\n    const char* name, int w\n) : " << baseclass;
    out << "(name, w) {\n";
    out << "    SetClassName(\"" << subclass << "\");\n";
    out << "}\n\n";
    return out.good();
}

boolean BorderCode::ConstDecls(ostream& out) {
    out << "(const char*, int);\n";
    return out.good();
}

boolean BorderCode::ConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n    const char* name, int w\n) : " << coreclass;
    out << "(name, w) {}\n\n";
    return out.good();
}

boolean BorderCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();

    if (!snamer->IsSubclass() && !_namelist->Search("border")) {
        _namelist->Append("border");
        out << "#include <InterViews/border.h> \n";
    }
    return out.good();
}

/*****************************************************************************/

BorderGraphic::BorderGraphic (
    Orientation o, CanvasVar* c, Graphic* g, int nat, int w
) : HVGraphic(c, g) {
    int f = (o == Horizontal) ? hfil : vfil;
    Init(nat, f, f, o, w);
}

Graphic* BorderGraphic::Copy () {
    return new BorderGraphic(
        GetOrientation(), nil, this, _natural, _minorAxisSize
    );
}

void BorderGraphic::draw (Canvas* c, Graphic* gs) {
    update(gs);
    Coord xmax, ymax;
    CanvasVar* cvar = GetCanvasVar();

    if (cvar == nil) {
        if (_orientation == Horizontal) {
            xmax = _natural;
            ymax = MinorAxisSize() - 1;
        } else {
            xmax = MinorAxisSize() - 1;
            ymax = _natural;
        }
    } else {
	xmax = cvar->xmax();
	ymax = cvar->ymax();	
    }
    _p->FillRect(c, 0, 0, xmax, ymax);
}

