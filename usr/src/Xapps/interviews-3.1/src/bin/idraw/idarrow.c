/*
 * Copyright (c) 1990, 1991 Stanford University
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
 * Implementation of ArrowLineComp, ArrowLineView, PSArrowLine
 */

#include "idarrow.h"
#include "idarrows.h"
#include "idvars.h"
#include "idclasses.h"
#include "idcmds.h"

#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/manips.h>
#include <Unidraw/statevars.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/datas.h>
#include <Unidraw/Commands/edit.h>
#include <Unidraw/Commands/transforms.h>
#include <Unidraw/Components/line.h>
#include <Unidraw/Graphic/lines.h>
#include <Unidraw/Graphic/picture.h>
#include <Unidraw/Graphic/util.h>
#include <Unidraw/Tools/tool.h>

#include <InterViews/rubcurve.h>
#include <InterViews/rubline.h>
#include <InterViews/rubverts.h>
#include <InterViews/transformer.h>

#include <math.h>
#include <stdio.h>
#include <stream.h>

/*****************************************************************************/

class _ArrowData : public Data {
public:
    _ArrowData(boolean, boolean);
public:
    boolean _head : 16;
    boolean _tail : 16;
};

_ArrowData::_ArrowData (boolean head, boolean tail) {
    _head = head;
    _tail = tail;
}

/****************************************************************************/

ArrowLineComp::ArrowLineComp (ArrowLine* graphic) : LineComp(graphic) { }
ArrowLine* ArrowLineComp::GetArrowLine () { return (ArrowLine*) GetGraphic(); }
ClassId ArrowLineComp::GetClassId() { return ARROWLINE_COMP; }

ClassId ArrowLineComp::GetSubstId (const char*& delim) {
    delim = "%END_ARROWLINE_COMP%";
    return LineComp::GetClassId();
}

boolean ArrowLineComp::IsA (ClassId id) {
    return ARROWLINE_COMP == id || LineComp::IsA(id);
}

Component* ArrowLineComp::Copy () {
    return new ArrowLineComp((ArrowLine*) GetGraphic()->Copy());
}

void ArrowLineComp::Interpret (Command* cmd) {
    if (cmd->IsA(ARROW_CMD)) {
	ArrowLine* line = GetArrowLine();

	if (line != nil) {
	    ArrowCmd* arrowCmd = (ArrowCmd*) cmd;
	    cmd->Store(this, new _ArrowData(line->Head(), line->Tail()));
	    line->SetArrows(arrowCmd->Head(), arrowCmd->Tail());
	    Notify();
	}

    } else if (cmd->IsA(PATTERN_CMD)) {
	GraphicComp::Interpret(cmd);

    } else {
	LineComp::Interpret(cmd);
    }
}

void ArrowLineComp::Uninterpret (Command* cmd) {
    if (cmd->IsA(ARROW_CMD)) {
	ArrowLine* line = GetArrowLine();

	if (line != nil) {
            _ArrowData* ad = (_ArrowData*) cmd->Recall(this);

            if (ad != nil) {
                line->SetArrows(ad->_head, ad->_tail);
                Notify();
            }
	}

    } else {
	LineComp::Uninterpret(cmd);
    }
}

void ArrowLineComp::Read (istream& in) {
    LineComp::Read(in);
    Line* line = GetLine();
    Coord x0, y0, x1, y1;
    int h, t;
    float scale;
    
    line->GetOriginal(x0, y0, x1, y1);

    in >> h >> t >> scale;

    ArrowLine* arrow = new ArrowLine(x0, y0, x1, y1, h, t, scale, line);
    arrow->SetPattern(ReadPattern(in));

    SetGraphic(arrow);
    delete line;
}

void ArrowLineComp::Write (ostream& out) {
    LineComp::Write(out);
    ArrowLine* arrow = GetArrowLine();
    
    out << arrow->Head() << " " << arrow->Tail() << " ";
    out << arrow->ArrowScale() << " ";

    WritePattern(arrow->GetPattern(), out);
}

/****************************************************************************/

ArrowLineComp* ArrowLineView::GetArrowLineComp () {
    return (ArrowLineComp*) GetSubject();
}

ArrowLineView::ArrowLineView (ArrowLineComp* subj) : LineView(subj) { }
ClassId ArrowLineView::GetClassId () { return ARROWLINE_VIEW; }

boolean ArrowLineView::IsA (ClassId id) {
    return ARROWLINE_VIEW == id || LineView::IsA(id);
}

void ArrowLineView::Update () {
    ArrowLine* line = (ArrowLine*) GetGraphic();
    ArrowLine* subj = GetArrowLineComp()->GetArrowLine();

    IncurDamage(line);
    *line = *subj;
    IncurDamage(line);
    EraseHandles();
}

Command* ArrowLineView::InterpretManipulator (Manipulator* m) {
    DragManip* dm = (DragManip*) m;
    Editor* ed = dm->GetViewer()->GetEditor();
    Tool* tool = dm->GetTool();
    Transformer* rel = dm->GetTransformer();
    Command* cmd = nil;
    ArrowVar* aVar = (ArrowVar*) ed->GetState("ArrowVar");

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        RubberLine* rl = (RubberLine*) dm->GetRubberband();
        Coord x0, y0, x1, y1;
        rl->GetCurrent(x0, y0, x1, y1);

        if (x0 != x1 || y0 != y1) {
            BrushVar* brVar = (BrushVar*) ed->GetState("BrushVar");
            ColorVar* colVar = (ColorVar*) ed->GetState("ColorVar");
            PatternVar* patVar = (PatternVar*) ed->GetState("PatternVar");

            if (rel != nil) {
                rel = new Transformer(rel);
                rel->Invert();
            }

            ArrowLine* aline = new ArrowLine(
                x0, y0, x1, y1, aVar->Head(), aVar->Tail(), 
                dm->GetViewer()->GetMagnification(), stdgraphic
            );

            if (brVar != nil) aline->SetBrush(brVar->GetBrush());
            if (patVar != nil) { aline->SetPattern(patVar->GetPattern()); }

            if (colVar != nil) {
                aline->SetColors(colVar->GetFgColor(), colVar->GetBgColor());
            }

            aline->SetTransformer(rel);
            Unref(rel);
            cmd = new PasteCmd(ed, new Clipboard(new ArrowLineComp(aline)));
        }

    } else if (tool->IsA(RESHAPE_TOOL)) {
        RubberLine* rl = (RubberLine*) dm->GetRubberband();
        Coord epx0, epy0, epx1, epy1;
        GetEndpoints(epx0, epy0, epx1, epy1);

        Coord x0, y0, x1, y1;
        rl->GetCurrent(x0, y0, x1, y1);

        if (x0 == epx1 && y0 == epy1) {
            x0 = x1; y0 = y1;
            x1 = epx1; y1 = epy1;
        }

        if (rel != nil) {
            rel = new Transformer(rel);
            rel->Invert();
        }
        ArrowLine* orig = GetArrowLineComp()->GetArrowLine();
        ArrowLine* aline = new ArrowLine(
	    x0, y0, x1, y1, orig->Head(), orig->Tail(),
            dm->GetViewer()->GetMagnification(), GetGraphic()
	);
        aline->SetTransformer(rel);
        Unref(rel);
        cmd = new ReplaceCmd(ed, new ArrowLineComp(aline));

    } else {
        cmd = LineView::InterpretManipulator(m);
    }
    return cmd;
}

/****************************************************************************/

PSArrowLine::PSArrowLine (ArrowLineComp* subj) : PSLine (subj) { }
ClassId PSArrowLine::GetClassId () { return PS_ARROWLINE; }

boolean PSArrowLine::IsA (ClassId id) {
    return PS_ARROWLINE == id || PSLine::IsA(id);
}

boolean PSArrowLine::Definition (ostream& out) {
    ArrowLineComp* comp = (ArrowLineComp*) GetSubject();
    ArrowLine* aline = comp->GetArrowLine();

    Coord x0, y0, x1, y1;
    aline->GetOriginal(x0, y0, x1, y1);
    float arrow_scale = aline->ArrowScale();

    out << "Begin " << MARK << " Line\n";
    MinGS(out);
    out << MARK << "\n";
    out << x0 << " " << y0 << " " << x1 << " " << y1 << " Line\n";
    out << MARK << " " << arrow_scale << "\n";
    out << "End\n\n";

    return out.good();
}

// this code is entirely for compatibility with older versions of idraw

void PSArrowLine::Brush (ostream& out) {
    ArrowLineComp* comp = (ArrowLineComp*) GetSubject();
    PSBrush* brush = (PSBrush*) GetGraphicComp()->GetGraphic()->GetBrush();
    boolean head, tail;
    head = comp->GetArrowLine()->Head();
    tail = comp->GetArrowLine()->Tail();

    if (brush == nil) {
	out << MARK << " b u\n";

    } else if (brush->None()) {
	out << "none SetB " << MARK << " b n\n";

    } else {
	int p = brush->GetLinePattern();
	out << MARK << " b " << p << "\n";

	int w = brush->Width();
	out << w << " " << head << " " << tail << " ";

	const int* dashpat = brush->GetDashPattern();
	int dashpatsize = brush->GetDashPatternSize();
	int dashoffset = brush->GetDashOffset();

	if (dashpatsize <= 0) {
	    out << "[] " << dashoffset << " ";
	} else {
	    out << "[";

	    for (int i = 0; i < dashpatsize - 1; i++) {
		out << dashpat[i] << " ";
	    }
	    out << dashpat[i] << "] " << dashoffset << " ";
	}
	out << "SetB\n";
    }

}

/****************************************************************************/

ArrowMultiLineComp::ArrowMultiLineComp (ArrowMultiLine* g) : MultiLineComp(g){}

ArrowMultiLine* ArrowMultiLineComp::GetArrowMultiLine () {
    return (ArrowMultiLine*) GetGraphic();
}

ClassId ArrowMultiLineComp::GetClassId() { return ARROWMULTILINE_COMP; }

ClassId ArrowMultiLineComp::GetSubstId (const char*& delim) {
    delim = "%END_ARROWMULTILINE_COMP%";
    return MultiLineComp::GetClassId();
}

boolean ArrowMultiLineComp::IsA (ClassId id) {
    return ARROWMULTILINE_COMP == id || MultiLineComp::IsA(id);
}

Component* ArrowMultiLineComp::Copy () {
    return new ArrowMultiLineComp((ArrowMultiLine*) GetGraphic()->Copy());
}

void ArrowMultiLineComp::Interpret (Command* cmd) {
    if (cmd->IsA(ARROW_CMD)) {
	ArrowMultiLine* amline = GetArrowMultiLine();

	if (amline != nil) {
	    ArrowCmd* arrowCmd = (ArrowCmd*) cmd;
	    cmd->Store(this, new _ArrowData(amline->Head(), amline->Tail()));
	    amline->SetArrows(arrowCmd->Head(), arrowCmd->Tail());
	    Notify();
	}

    } else if (cmd->IsA(PATTERN_CMD)) {
	GraphicComp::Interpret(cmd);

    } else {
	MultiLineComp::Interpret(cmd);
    }
}

void ArrowMultiLineComp::Uninterpret (Command* cmd) {
    if (cmd->IsA(ARROW_CMD)) {
	ArrowMultiLine* amline = GetArrowMultiLine();

	if (amline != nil) {
            _ArrowData* ad = (_ArrowData*) cmd->Recall(this);

            if (ad != nil) {
                amline->SetArrows(ad->_head, ad->_tail);
                Notify();
            }
	}

    } else {
	MultiLineComp::Uninterpret(cmd);
    }
}

void ArrowMultiLineComp::Read (istream& in) {
    MultiLineComp::Read(in);
    SF_MultiLine* ml = GetMultiLine();
    Coord* x, *y;
    int count, h, t;
    float scale;

    const Coord* cx, * cy;
    count = ml->GetOriginal(cx, cy);
    x = (Coord*)cx; y = (Coord*)cy;

    in >> h >> t >> scale;

    ArrowMultiLine* aml = new ArrowMultiLine(x, y, count, h, t, scale, ml);

    SetGraphic(aml);
    delete ml;
}

void ArrowMultiLineComp::Write (ostream& out) {
    MultiLineComp::Write(out);
    ArrowMultiLine* aml = GetArrowMultiLine();

    out << aml->Head() << " " << aml->Tail() << " " <<aml->ArrowScale() << " ";
}

/****************************************************************************/

ArrowMultiLineComp* ArrowMultiLineView::GetArrowMultiLineComp () {
    return (ArrowMultiLineComp*) GetSubject();
}

ArrowMultiLineView::ArrowMultiLineView (
    ArrowMultiLineComp* s
) : MultiLineView(s) { }

ClassId ArrowMultiLineView::GetClassId () { return ARROWMULTILINE_VIEW; }

boolean ArrowMultiLineView::IsA (ClassId id) {
    return ARROWMULTILINE_VIEW == id || MultiLineView::IsA(id);
}

void ArrowMultiLineView::Update () {
    ArrowMultiLine* amline = (ArrowMultiLine*) GetGraphic();
    ArrowMultiLine* subj = GetArrowMultiLineComp()->GetArrowMultiLine();

    IncurDamage(amline);
    *amline = *subj;
    IncurDamage(amline);
    EraseHandles();
}

Command* ArrowMultiLineView::InterpretManipulator (Manipulator* m) {
    DragManip* dm = (DragManip*) m;
    Editor* ed = dm->GetViewer()->GetEditor();
    Tool* tool = dm->GetTool();
    Transformer* rel = dm->GetTransformer();
    Command* cmd = nil;
    ArrowVar* aVar = (ArrowVar*) ed->GetState("ArrowVar");

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        GrowingVertices* gv = (GrowingVertices*) dm->GetRubberband();
        Coord* x, *y;
        int n;
        gv->GetCurrent(x, y, n);

        if (n > 2 || x[0] != x[1] || y[0] != y[1]) {
            BrushVar* brVar = (BrushVar*) ed->GetState("BrushVar");
            PatternVar* patVar = (PatternVar*) ed->GetState("PatternVar");
            ColorVar* colVar = (ColorVar*) ed->GetState("ColorVar");

            if (rel != nil) {
                rel = new Transformer(rel);
                rel->Invert();
            }
            ArrowMultiLine* aml = new ArrowMultiLine(
                x, y, n, aVar->Head(), aVar->Tail(), 
                dm->GetViewer()->GetMagnification(), stdgraphic
            );

            if (brVar != nil) aml->SetBrush(brVar->GetBrush());
            if (patVar != nil) aml->SetPattern(patVar->GetPattern());

            if (colVar != nil) {
                aml->SetColors(colVar->GetFgColor(), colVar->GetBgColor());
            }
            aml->SetTransformer(rel);
            Unref(rel);
            cmd = new PasteCmd(ed, new Clipboard(new ArrowMultiLineComp(aml)));
        }
        delete x;
        delete y;

    } else if (tool->IsA(RESHAPE_TOOL)) {
        GrowingVertices* gv = (GrowingVertices*) dm->GetRubberband();
        Coord* x, *y;
        int n, pt;
        gv->RemoveVertex();
        gv->GetCurrent(x, y, n, pt);

	if (rel != nil) {
            rel = new Transformer(rel);
            rel->Invert();
        }

        ArrowMultiLine* orig = GetArrowMultiLineComp()->GetArrowMultiLine();
        ArrowMultiLine* aml = new ArrowMultiLine(
            x, y, n, orig->Head(), orig->Tail(), 
            dm->GetViewer()->GetMagnification(), GetGraphic()
        );
        delete x;
        delete y;
        aml->SetTransformer(rel);
        Unref(rel);
        cmd = new ReplaceCmd(ed, new ArrowMultiLineComp(aml));

    } else {
        cmd = MultiLineView::InterpretManipulator(m);
    }
    return cmd;
}

/****************************************************************************/

PSArrowMultiLine::PSArrowMultiLine (ArrowMultiLineComp* s) : PSMultiLine(s) { }
ClassId PSArrowMultiLine::GetClassId () { return PS_ARROWLINE; }

boolean PSArrowMultiLine::IsA (ClassId id) { 
    return PS_ARROWMULTILINE == id || PSMultiLine::IsA(id);
}

boolean PSArrowMultiLine::Definition (ostream& out) {
    ArrowMultiLineComp* comp = (ArrowMultiLineComp*) GetSubject();
    ArrowMultiLine* aml = comp->GetArrowMultiLine();

    const Coord* x, *y;
    int n = aml->GetOriginal(x, y);
    float arrow_scale = aml->ArrowScale();

    out << "Begin " << MARK << " " << Name() << "\n";
    MinGS(out);
    out << MARK << " " << n << "\n";
    for (int i = 0; i < n; i++) {
        out << x[i] << " " << y[i] << "\n";
    }
    out << n << " " << Name() << "\n";
    out << MARK << " " << arrow_scale << "\n";
    out << "End\n\n";

    return out.good();
}

// this code is entirely for compatibility with older versions of idraw

void PSArrowMultiLine::Brush (ostream& out) {
    ArrowMultiLineComp* comp = (ArrowMultiLineComp*) GetSubject();
    PSBrush* brush = (PSBrush*) GetGraphicComp()->GetGraphic()->GetBrush();
    boolean head, tail;
    head = comp->GetArrowMultiLine()->Head();
    tail = comp->GetArrowMultiLine()->Tail();

    if (brush == nil) {
	out << MARK << " b u\n";

    } else if (brush->None()) {
	out << "none SetB " << MARK << " b n\n";

    } else {
	int p = brush->GetLinePattern();
	out << MARK << " b " << p << "\n";

	int w = brush->Width();
	out << w << " " << head << " " << tail << " ";

	const int* dashpat = brush->GetDashPattern();
	int dashpatsize = brush->GetDashPatternSize();
	int dashoffset = brush->GetDashOffset();

	if (dashpatsize <= 0) {
	    out << "[] " << dashoffset << " ";
	} else {
	    out << "[";

	    for (int i = 0; i < dashpatsize - 1; i++) {
		out << dashpat[i] << " ";
	    }
	    out << dashpat[i] << "] " << dashoffset << " ";
	}
	out << "SetB\n";
    }
}

/****************************************************************************/

ArrowSplineComp::ArrowSplineComp (ArrowOpenBSpline* g) : SplineComp(g) {}

ArrowOpenBSpline* ArrowSplineComp::GetArrowOpenBSpline () {
    return (ArrowOpenBSpline*) GetGraphic();
}

ClassId ArrowSplineComp::GetClassId() { return ARROWSPLINE_COMP; }

ClassId ArrowSplineComp::GetSubstId (const char*& delim) {
    delim = "%END_ARROWSPLINE_COMP%";
    return SplineComp::GetClassId();
}

boolean ArrowSplineComp::IsA (ClassId id) {
    return ARROWSPLINE_COMP == id || SplineComp::IsA(id);
}

Component* ArrowSplineComp::Copy () {
    return new ArrowSplineComp((ArrowOpenBSpline*) GetGraphic()->Copy());
}

void ArrowSplineComp::Interpret (Command* cmd) {
    if (cmd->IsA(ARROW_CMD)) {
	ArrowOpenBSpline* amline = GetArrowOpenBSpline();

	if (amline != nil) {
	    ArrowCmd* arrowCmd = (ArrowCmd*) cmd;
	    cmd->Store(this, new _ArrowData(amline->Head(), amline->Tail()));
	    amline->SetArrows(arrowCmd->Head(), arrowCmd->Tail());
	    Notify();
	}

    } else if (cmd->IsA(PATTERN_CMD)) {
	GraphicComp::Interpret(cmd);

    } else {
	SplineComp::Interpret(cmd);
    }
}

void ArrowSplineComp::Uninterpret (Command* cmd) {
    if (cmd->IsA(ARROW_CMD)) {
	ArrowOpenBSpline* amline = GetArrowOpenBSpline();

	if (amline != nil) {
            _ArrowData* ad = (_ArrowData*) cmd->Recall(this);

            if (ad != nil) {
                amline->SetArrows(ad->_head, ad->_tail);
                Notify();
            }
	}

    } else {
	SplineComp::Uninterpret(cmd);
    }
}

void ArrowSplineComp::Read (istream& in) {
    SplineComp::Read(in);
    SFH_OpenBSpline* spl = GetSpline();
    Coord* x, *y;
    int count, h, t;
    float scale;

    const Coord* cx, * cy;
    count = spl->GetOriginal(cx, cy);
    x = (Coord*)cx; y = (Coord*)cy;

    in >> h >> t >> scale;

    ArrowOpenBSpline* as = new ArrowOpenBSpline(x, y, count, h, t, scale, spl);

    SetGraphic(as);
    delete spl;
}

void ArrowSplineComp::Write (ostream& out) {
    SplineComp::Write(out);
    ArrowOpenBSpline* as = GetArrowOpenBSpline();

    out << as->Head() << " " << as->Tail() << " " << as->ArrowScale() << " ";
}

/****************************************************************************/

ArrowSplineComp* ArrowSplineView::GetArrowSplineComp () {
    return (ArrowSplineComp*) GetSubject();
}

ArrowSplineView::ArrowSplineView (ArrowSplineComp* s) : SplineView(s) { }
ClassId ArrowSplineView::GetClassId () { return ARROWSPLINE_VIEW; }

boolean ArrowSplineView::IsA (ClassId id) {
    return ARROWSPLINE_VIEW == id || SplineView::IsA(id);
}

void ArrowSplineView::Update () {
    ArrowOpenBSpline* amline = (ArrowOpenBSpline*) GetGraphic();
    ArrowOpenBSpline* subj = GetArrowSplineComp()->GetArrowOpenBSpline();

    IncurDamage(amline);
    *amline = *subj;
    IncurDamage(amline);
    EraseHandles();
}

Command* ArrowSplineView::InterpretManipulator (Manipulator* m) {
    DragManip* dm = (DragManip*) m;
    Editor* ed = dm->GetViewer()->GetEditor();
    Tool* tool = dm->GetTool();
    Transformer* rel = dm->GetTransformer();
    Command* cmd = nil;
    ArrowVar* aVar = (ArrowVar*) ed->GetState("ArrowVar");

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        GrowingVertices* gv = (GrowingVertices*) dm->GetRubberband();
        Coord* x, *y;
        int n;
        gv->GetCurrent(x, y, n);

        if (n > 2 || x[0] != x[1] || y[0] != y[1]) {
            BrushVar* brVar = (BrushVar*) ed->GetState("BrushVar");
            PatternVar* patVar = (PatternVar*) ed->GetState("PatternVar");
            ColorVar* colVar = (ColorVar*) ed->GetState("ColorVar");

            if (rel != nil) {
                rel = new Transformer(rel);
                rel->Invert();
            }
            ArrowOpenBSpline* aml = new ArrowOpenBSpline(
                x, y, n, aVar->Head(), aVar->Tail(), 
                dm->GetViewer()->GetMagnification(), stdgraphic
            );

            if (brVar != nil) aml->SetBrush(brVar->GetBrush());
            if (patVar != nil) aml->SetPattern(patVar->GetPattern());

            if (colVar != nil) {
                aml->SetColors(colVar->GetFgColor(), colVar->GetBgColor());
            }
            aml->SetTransformer(rel);
            Unref(rel);
            cmd = new PasteCmd(ed, new Clipboard(new ArrowSplineComp(aml)));
        }
        delete x;
        delete y;

    } else if (tool->IsA(RESHAPE_TOOL)) {
        GrowingVertices* gv = (GrowingVertices*) dm->GetRubberband();
        Coord* x, *y;
        int n, pt;
        gv->RemoveVertex();
        gv->GetCurrent(x, y, n, pt);

        if (rel != nil) {
            rel = new Transformer(rel);
            rel->Invert();
        }

        ArrowOpenBSpline* orig = GetArrowSplineComp()->GetArrowOpenBSpline();
        ArrowOpenBSpline* aml = new ArrowOpenBSpline(
            x, y, n, orig->Head(), orig->Tail(), 
            dm->GetViewer()->GetMagnification(), GetGraphic()
        );
        delete x;
        delete y;
        aml->SetTransformer(rel);
        Unref(rel);
        cmd = new ReplaceCmd(ed, new ArrowSplineComp(aml));

    } else {
        cmd = SplineView::InterpretManipulator(m);
    }
    return cmd;
}

/****************************************************************************/

PSArrowSpline::PSArrowSpline (ArrowSplineComp* s) : PSSpline(s) { }
ClassId PSArrowSpline::GetClassId () { return PS_ARROWLINE; }

boolean PSArrowSpline::IsA (ClassId id) { 
    return PS_ARROWSPLINE == id || PSSpline::IsA(id);
}

boolean PSArrowSpline::Definition (ostream& out) {
    ArrowSplineComp* comp = (ArrowSplineComp*) GetSubject();
    ArrowOpenBSpline* aml = comp->GetArrowOpenBSpline();

    const Coord* x, *y;
    int n = aml->GetOriginal(x, y);
    float arrow_scale = aml->ArrowScale();

    out << "Begin " << MARK << " " << Name() << "\n";
    MinGS(out);
    out << MARK << " " << n << "\n";
    for (int i = 0; i < n; i++) {
        out << x[i] << " " << y[i] << "\n";
    }
    out << n << " " << Name() << "\n";
    out << MARK << " " << arrow_scale << "\n";
    out << "End\n\n";

    return out.good();
}

// this code is entirely for compatibility with older versions of idraw

void PSArrowSpline::Brush (ostream& out) {
    ArrowSplineComp* comp = (ArrowSplineComp*) GetSubject();
    PSBrush* brush = (PSBrush*) GetGraphicComp()->GetGraphic()->GetBrush();
    boolean head, tail;
    head = comp->GetArrowOpenBSpline()->Head();
    tail = comp->GetArrowOpenBSpline()->Tail();

    if (brush == nil) {
	out << MARK << " b u\n";

    } else if (brush->None()) {
	out << "none SetB " << MARK << " b n\n";

    } else {
	int p = brush->GetLinePattern();
	out << MARK << " b " << p << "\n";

	int w = brush->Width();
	out << w << " " << head << " " << tail << " ";

	const int* dashpat = brush->GetDashPattern();
	int dashpatsize = brush->GetDashPatternSize();
	int dashoffset = brush->GetDashOffset();

	if (dashpatsize <= 0) {
	    out << "[] " << dashoffset << " ";
	} else {
	    out << "[";

	    for (int i = 0; i < dashpatsize - 1; i++) {
		out << dashpat[i] << " ";
	    }
	    out << dashpat[i] << "] " << dashoffset << " ";
	}
	out << "SetB\n";
    }
}
