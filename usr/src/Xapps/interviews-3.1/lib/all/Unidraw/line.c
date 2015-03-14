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
 * Line and MultiLine component definitions.
 */

#include <Unidraw/classes.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/manips.h>
#include <Unidraw/selection.h>
#include <Unidraw/statevars.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/align.h>
#include <Unidraw/Commands/edit.h>
#include <Unidraw/Commands/transforms.h>

#include <Unidraw/Components/line.h>

#include <Unidraw/Graphic/lines.h>

#include <Unidraw/Tools/tool.h>

#include <InterViews/event.h>
#include <IV-2_6/InterViews/rubcurve.h>
#include <IV-2_6/InterViews/rubline.h>
#include <IV-2_6/InterViews/rubverts.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

#include <math.h>
#include <stream.h>

/*****************************************************************************/

ClassId LineComp::GetClassId () { return LINE_COMP; }

boolean LineComp::IsA (ClassId id) {
    return LINE_COMP == id || GraphicComp::IsA(id);
}

Component* LineComp::Copy () {
    return new LineComp((Line*) GetGraphic()->Copy());
}

LineComp::LineComp (Line* graphic) : GraphicComp(graphic) { }
Line* LineComp::GetLine () { return (Line*) GetGraphic(); }

void LineComp::Interpret (Command* cmd) {
    if (!cmd->IsA(PATTERN_CMD)) {
        GraphicComp::Interpret(cmd);
    }
}

void LineComp::Read (istream& in) {
    GraphicComp::Read(in);
    Coord x0, y0, x1, y1;

    in >> x0 >> y0 >> x1 >> y1;
    Line* line = new Line(x0, y0, x1, y1);

    line->FillBg(ReadBgFilled(in));
    PSColor* fg = ReadColor(in);
    PSColor* bg = ReadColor(in);
    line->SetColors(fg, bg);
    line->SetBrush(ReadBrush(in));

    Transformer* t = ReadTransformer(in);
    line->SetTransformer(t);
    Unref(t);

    SetGraphic(line);
}

void LineComp::Write (ostream& out) {
    GraphicComp::Write(out);
    Line* line = GetLine();
    Coord x0, y0, x1, y1;

    line->GetOriginal(x0, y0, x1, y1);
    out << x0 << " " << y0 << " " << x1 << " " << y1 << " ";

    WriteBgFilled(line->BgFilled(), out);
    WriteColor(line->GetFgColor(), out);
    WriteColor(line->GetBgColor(), out);
    WriteBrush(line->GetBrush(), out);
    WriteTransformer(line->GetTransformer(), out);
}

/****************************************************************************/

LineComp* LineView::GetLineComp () { return (LineComp*) GetSubject(); }
ClassId LineView::GetClassId () { return LINE_VIEW; }

boolean LineView::IsA (ClassId id) {
    return LINE_VIEW == id || GraphicView::IsA(id);
}

LineView::LineView (LineComp* subj) : GraphicView(subj) { }

void LineView::Interpret (Command* cmd) {
    if (cmd->IsA(ALIGNTOGRID_CMD)) {
        Line* line = (Line*) GetGraphic();
        Transformer total;
        line->TotalTransformation(total);

        Coord x0, y0, x1, y1;
        float tx0, ty0;

        line->GetOriginal(x0, y0, x1, y1);
        total.Transform(float(x0), float(y0), tx0, ty0);
        ((AlignToGridCmd*) cmd)->Align(this, tx0, ty0);

    } else {
        GraphicView::Interpret(cmd);
    }
}

void LineView::Update () {
    Graphic* line = GetGraphic();

    IncurDamage(line);
    *line = *GetLineComp()->GetGraphic();
    IncurDamage(line);
    EraseHandles();
}

void LineView::CreateHandles () {
    Coord x[2], y[2];
    Viewer* v = GetViewer();
    
    if (v != nil) {
        GetEndpoints(x[0], y[0], x[1], y[1]);
        _handles = new RubberHandles(nil, nil, x, y, 2, 0, HANDLE_SIZE);
        v->InitRubberband(_handles);
    }
}

Manipulator* LineView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Coord x0, y0, x1, y1;
    Rubberband* rub = nil;
    Manipulator* m = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        v->Constrain(e.x, e.y);
        rub = new RubberLine(nil, nil, e.x, e.y, e.x, e.y);
        m = new DragManip(
	    v, rub, rel, tool, DragConstraint(HorizOrVert | Gravity)
	);

    } else if (tool->IsA(MOVE_TOOL)) {
        v->Constrain(e.x, e.y);
        GetEndpoints(x0, y0, x1, y1);
	rub = new SlidingLine(nil, nil, x0, y0, x1, y1, e.x, e.y);
        m = new DragManip(
	    v, rub, rel, tool, DragConstraint(HorizOrVert | Gravity)
	);

    } else if (tool->IsA(SCALE_TOOL)) {
        v->Constrain(e.x, e.y);
        GetEndpoints(x0, y0, x1, y1);
        rub = new ScalingLine(nil, nil, x0, y0, x1, y1, (x0+x1)/2, (y0+y1)/2);
        m = new DragManip(v, rub, rel, tool, Gravity);

    } else if (tool->IsA(ROTATE_TOOL)) {
        v->Constrain(e.x, e.y);
        GetEndpoints(x0, y0, x1, y1);
        rub = new RotatingLine(
            nil, nil, x0, y0, x1, y1, (x0+x1)/2, (y0+y1)/2, e.x, e.y
        );
        m = new DragManip(v, rub, rel, tool, Gravity);

    } else if (tool->IsA(RESHAPE_TOOL)) {
        v->Constrain(e.x, e.y);
        GetEndpoints(x0, y0, x1, y1);
	PointObj p1(x0, y0), p2(x1, y1), cp(e.x, e.y);

	if (p1.Distance(cp) < p2.Distance(cp)) {
	    rub = new RubberLine(nil, nil, x1, y1, e.x, e.y);
	} else {
	    rub = new RubberLine(nil, nil, x0, y0, e.x, e.y);
	}
        m = new DragManip(
	    v, rub, rel, tool, DragConstraint(HorizOrVert | Gravity)
	);

    } else {
        m = GraphicView::CreateManipulator(v, e, rel, tool);
    }
    return m;
}

Command* LineView::InterpretManipulator (Manipulator* m) {
    DragManip* dm = (DragManip*) m;
    Editor* ed = dm->GetViewer()->GetEditor();
    Tool* tool = dm->GetTool();
    Transformer* rel = dm->GetTransformer();
    Command* cmd = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        RubberLine* rl = (RubberLine*) dm->GetRubberband();
        Coord x0, y0, x1, y1;
        rl->GetCurrent(x0, y0, x1, y1);

        if (x0 != x1 || y0 != y1) {
            BrushVar* brVar = (BrushVar*) ed->GetState("BrushVar");
            ColorVar* colVar = (ColorVar*) ed->GetState("ColorVar");

            if (rel != nil) {
                rel = new Transformer(rel);
                rel->Invert();
            }

            Graphic* pg = GetGraphicComp()->GetGraphic();
            Line* line = new Line(x0, y0, x1, y1, pg);

            if (brVar != nil) line->SetBrush(brVar->GetBrush());

            if (colVar != nil) {
                line->SetColors(colVar->GetFgColor(), colVar->GetBgColor());
            }
            line->SetTransformer(rel);
            Unref(rel);
            cmd = new PasteCmd(ed, new Clipboard(new LineComp(line)));
        }

    } else if (tool->IsA(MOVE_TOOL)) {
        Coord x0, y0, x1, y1, dummy1, dummy2;
        float fx0, fy0, fx1, fy1;

        SlidingLine* sl = (SlidingLine*) dm->GetRubberband();
        sl->GetOriginal(x0, y0, dummy1, dummy2);
        sl->GetCurrent(x1, y1, dummy1, dummy2);

        if (rel != nil) {
            rel->InvTransform(float(x0), float(y0), fx0, fy0);
            rel->InvTransform(float(x1), float(y1), fx1, fy1);
        }
        cmd = new MoveCmd(ed, fx1 - fx0, fy1 - fy0);

    } else if (tool->IsA(SCALE_TOOL)) {
        ScalingLine* sl = (ScalingLine*) dm->GetRubberband();
        float sxy = sl->CurrentScaling();

        cmd = new ScaleCmd(ed, sxy, sxy);

    } else if (tool->IsA(ROTATE_TOOL)) {
        RotatingLine* rl = (RotatingLine*) dm->GetRubberband();
        float angle = rl->CurrentAngle() - rl->OriginalAngle();

        cmd = new RotateCmd(ed, angle);

    } else if (tool->IsA(RESHAPE_TOOL)) {
        RubberLine* rl = (RubberLine*) dm->GetRubberband();
        Coord x0, y0, x1, y1;
        rl->GetCurrent(x0, y0, x1, y1);

        if (rel != nil) {
            rel = new Transformer(rel);
            rel->Invert();
        }
        Line* line = new Line(x0, y0, x1, y1, GetGraphic());
        line->SetTransformer(rel);
        Unref(rel);
	cmd = new ReplaceCmd(ed, new LineComp(line));

    } else {
        cmd = GraphicView::InterpretManipulator(m);
    }
    return cmd;
}

void LineView::GetEndpoints (Coord& x0, Coord& y0, Coord& x1, Coord& y1) {
    Line* line = (Line*) GetGraphic();
    Transformer t;

    line->GetOriginal(x0, y0, x1, y1);
    line->TotalTransformation(t);
    t.Transform(x0, y0);
    t.Transform(x1, y1);
}

Graphic* LineView::GetGraphic () {
    Graphic* graphic = GraphicView::GetGraphic();

    if (graphic == nil) {
        LineComp* lineComp = GetLineComp();
        graphic = lineComp->GetGraphic()->Copy();
        SetGraphic(graphic);
    }
    return graphic;
}

/****************************************************************************/

ClassId PSLine::GetClassId () { return PS_LINE; }

boolean PSLine::IsA (ClassId id) { 
    return PS_LINE == id || PostScriptView::IsA(id);
}

PSLine::PSLine (LineComp* subj) : PostScriptView(subj) { }

boolean PSLine::Definition (ostream& out) {
    Coord x0, y0, x1, y1;

    LineComp* comp = (LineComp*) GetSubject();
    comp->GetLine()->GetOriginal(x0, y0, x1, y1);

    out << "Begin " << MARK << " Line\n";
    MinGS(out);
    out << MARK << "\n";
    out << x0 << " " << y0 << " " << x1 << " " << y1 << " Line\n";
    out << "End\n\n";

    return out.good();
}    

/****************************************************************************/

ClassId MultiLineComp::GetClassId () { return MULTILINE_COMP; }

boolean MultiLineComp::IsA (ClassId id) {
    return MULTILINE_COMP == id || VerticesComp::IsA(id);
}

Component* MultiLineComp::Copy () {
    return new MultiLineComp((SF_MultiLine*) GetGraphic()->Copy());
}

MultiLineComp::MultiLineComp (SF_MultiLine* graphic) : VerticesComp(graphic) {}

SF_MultiLine* MultiLineComp::GetMultiLine () {
    return (SF_MultiLine*) GetGraphic();
}

void MultiLineComp::Read (istream& in) {
    VerticesComp::Read(in);
    Coord* x, *y;
    int count;

    ReadVertices(in, x, y, count);
    SF_MultiLine* ml = new SF_MultiLine(x, y, count);
    delete x;
    delete y;

    ml->FillBg(ReadBgFilled(in));
    PSColor* fg = ReadColor(in);
    PSColor* bg = ReadColor(in);
    ml->SetColors(fg, bg);
    ml->SetBrush(ReadBrush(in));
    ml->SetPattern(ReadPattern(in));

    Transformer* t = ReadTransformer(in);
    ml->SetTransformer(t);
    Unref(t);

    SetGraphic(ml);
}

void MultiLineComp::Write (ostream& out) {
    VerticesComp::Write(out);
    SF_MultiLine* ml = GetMultiLine();
    const Coord* x, *y;
    int count = ml->GetOriginal(x, y);

    WriteVertices(x, y, count, out);

    WriteBgFilled(ml->BgFilled(), out);
    WriteColor(ml->GetFgColor(), out);
    WriteColor(ml->GetBgColor(), out);
    WriteBrush(ml->GetBrush(), out);
    WritePattern(ml->GetPattern(), out);
    WriteTransformer(ml->GetTransformer(), out);
}

/****************************************************************************/

MultiLineView::MultiLineView (MultiLineComp* subj) : VerticesView(subj) { }

MultiLineComp* MultiLineView::GetMultiLineComp () { 
    return (MultiLineComp*) GetSubject();
}

ClassId MultiLineView::GetClassId () { return MULTILINE_VIEW; }

boolean MultiLineView::IsA (ClassId id) {
    return MULTILINE_VIEW == id || VerticesView::IsA(id);
}

boolean MultiLineView::VertexChanged () { 
    SF_MultiLine* gview = (SF_MultiLine*) GetGraphic();
    SF_MultiLine* gsubj =
        (SF_MultiLine*) GetMultiLineComp()->GetGraphic();

    return *gview != *gsubj;
}

Manipulator* MultiLineView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Manipulator* m = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        v->Constrain(e.x, e.y);
        Coord x[1], y[1];
        x[0] = e.x;
        y[0] = e.y;
        GrowingVertices* rub = new GrowingMultiLine(
            nil, nil, x, y, 1, -1, HANDLE_SIZE
        );
        m = new VertexManip(
	    v, rub, rel, tool, DragConstraint(HorizOrVert | Gravity)
	);

    } else if (tool->IsA(RESHAPE_TOOL)) {
	Coord* x, *y;
	int n;

        v->Constrain(e.x, e.y);
	GetVertices(x, y, n);
        GrowingMultiLine* rub = new GrowingMultiLine(
            nil, nil, x, y, n, ClosestPoint(x, y, n, e.x, e.y), HANDLE_SIZE
        );
	delete x;
	delete y;

        m = new VertexManip(
	    v, rub, rel, tool, DragConstraint(HorizOrVert | Gravity)
	);

    } else {
        m = VerticesView::CreateManipulator(v, e, rel, tool);
    }
    return m;
}

Command* MultiLineView::InterpretManipulator (Manipulator* m) {
    DragManip* dm = (DragManip*) m;
    Editor* ed = dm->GetViewer()->GetEditor();
    Tool* tool = dm->GetTool();
    Transformer* rel = dm->GetTransformer();
    Command* cmd = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        GrowingVertices* gv = (GrowingVertices*) dm->GetRubberband();
        Coord* x, *y;
        int n, pt;
        gv->GetCurrent(x, y, n, pt);

        if (n > 2 || x[0] != x[1] || y[0] != y[1]) {
            BrushVar* brVar = (BrushVar*) ed->GetState("BrushVar");
            PatternVar* patVar = (PatternVar*) ed->GetState("PatternVar");
            ColorVar* colVar = (ColorVar*) ed->GetState("ColorVar");

            if (rel != nil) {
                rel = new Transformer(rel);
                rel->Invert();
            }

            Graphic* pg = GetGraphicComp()->GetGraphic();
            SF_MultiLine* polygon = new SF_MultiLine(x, y, n, pg);

            if (brVar != nil) polygon->SetBrush(brVar->GetBrush());
            if (patVar != nil) polygon->SetPattern(patVar->GetPattern());

            if (colVar != nil) {
                polygon->SetColors(colVar->GetFgColor(), colVar->GetBgColor());
            }
            polygon->SetTransformer(rel);
            Unref(rel);
            cmd = new PasteCmd(ed, new Clipboard(new MultiLineComp(polygon)));
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

        SF_MultiLine* polygon = new SF_MultiLine(x, y, n, GetGraphic());
	delete x;
	delete y;
        polygon->SetTransformer(rel);
        Unref(rel);
	cmd = new ReplaceCmd(ed, new MultiLineComp(polygon));

    } else {
        cmd = VerticesView::InterpretManipulator(m);
    }
    return cmd;
}

/*****************************************************************************/

ClassId PSMultiLine::GetClassId () { return PS_MULTILINE; }

boolean PSMultiLine::IsA (ClassId id) { 
    return PS_MULTILINE == id || PSVertices::IsA(id);
}

PSMultiLine::PSMultiLine (MultiLineComp* subj) : PSVertices(subj) { }
const char* PSMultiLine::Name () { return "MLine"; }
