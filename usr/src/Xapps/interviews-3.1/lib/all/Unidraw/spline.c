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
 * Spline component definitions.
 */

#include <Unidraw/classes.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/manips.h>
#include <Unidraw/statevars.h>
#include <Unidraw/viewer.h>
#include <Unidraw/Commands/edit.h>
#include <Unidraw/Components/spline.h>
#include <Unidraw/Graphic/splines.h>
#include <Unidraw/Tools/tool.h>

#include <InterViews/event.h>
#include <IV-2_6/InterViews/rubcurve.h>
#include <IV-2_6/InterViews/rubverts.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

#include <stream.h>

/****************************************************************************/

ClassId SplineComp::GetClassId () { return SPLINE_COMP; }

boolean SplineComp::IsA (ClassId id) {
    return SPLINE_COMP == id || VerticesComp::IsA(id);
}

Component* SplineComp::Copy () {
    return new SplineComp((SFH_OpenBSpline*) GetGraphic()->Copy());
}

SplineComp::SplineComp (SFH_OpenBSpline* graphic) : VerticesComp(graphic) { }

SFH_OpenBSpline* SplineComp::GetSpline () {
    return (SFH_OpenBSpline*) GetGraphic();
}

void SplineComp::Read (istream& in) {
    VerticesComp::Read(in);
    Coord* x, *y;
    int count;

    ReadVertices(in, x, y, count);
    SFH_OpenBSpline* spline = new SFH_OpenBSpline(x, y, count);
    delete x;
    delete y;

    spline->FillBg(ReadBgFilled(in));
    PSColor* fg = ReadColor(in);
    PSColor* bg = ReadColor(in);
    spline->SetColors(fg, bg);
    spline->SetBrush(ReadBrush(in));
    spline->SetPattern(ReadPattern(in));

    Transformer* t = ReadTransformer(in);
    spline->SetTransformer(t);
    Unref(t);

    SetGraphic(spline);
}

void SplineComp::Write (ostream& out) {
    VerticesComp::Write(out);
    SFH_OpenBSpline* spline = GetSpline();
    const Coord* x, *y;
    int count = spline->GetOriginal(x, y);

    WriteVertices(x, y, count, out);

    WriteBgFilled(spline->BgFilled(), out);
    WriteColor(spline->GetFgColor(), out);
    WriteColor(spline->GetBgColor(), out);
    WriteBrush(spline->GetBrush(), out);
    WritePattern(spline->GetPattern(), out);
    WriteTransformer(spline->GetTransformer(), out);
}

/****************************************************************************/

SplineView::SplineView (SplineComp* subj) : VerticesView(subj) { }
SplineComp* SplineView::GetSplineComp () { return (SplineComp*) GetSubject(); }
ClassId SplineView::GetClassId () { return SPLINE_VIEW; }

boolean SplineView::IsA (ClassId id) {
    return SPLINE_VIEW == id || VerticesView::IsA(id);
}

boolean SplineView::VertexChanged () { 
    SFH_OpenBSpline* gview = (SFH_OpenBSpline*) GetGraphic();
    SFH_OpenBSpline* gsubj = (SFH_OpenBSpline*) GetSplineComp()->GetGraphic();

    return *gview != *gsubj;
}

Manipulator* SplineView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Manipulator* m = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        v->Constrain(e.x, e.y);
        Coord x[1], y[1];
        x[0] = e.x;
        y[0] = e.y;
        GrowingVertices* rub = new GrowingBSpline(
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
        GrowingBSpline* rub = new GrowingBSpline(
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

Command* SplineView::InterpretManipulator (Manipulator* m) {
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
            SFH_OpenBSpline* spline = new SFH_OpenBSpline(x, y, n, pg);

            if (brVar != nil) spline->SetBrush(brVar->GetBrush());
            if (patVar != nil) spline->SetPattern(patVar->GetPattern());

            if (colVar != nil) {
                spline->SetColors(colVar->GetFgColor(), colVar->GetBgColor());
            }
            spline->SetTransformer(rel);
            Unref(rel);
            cmd = new PasteCmd(ed, new Clipboard(new SplineComp(spline)));
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

        SFH_OpenBSpline* spline = new SFH_OpenBSpline(x, y, n, GetGraphic());
	delete x;
	delete y;
        spline->SetTransformer(rel);
        Unref(rel);
        cmd = new ReplaceCmd(ed, new SplineComp(spline));

    } else {
        cmd = VerticesView::InterpretManipulator(m);
    }
    return cmd;
}

/*****************************************************************************/

ClassId PSSpline::GetClassId () { return PS_SPLINE; }

boolean PSSpline::IsA (ClassId id) { 
    return PS_SPLINE == id || PSVertices::IsA(id);
}

PSSpline::PSSpline (SplineComp* subj) : PSVertices(subj) { }
const char* PSSpline::Name () { return "BSpl"; }

/*****************************************************************************/

ClassId ClosedSplineComp::GetClassId () { return CLOSEDSPLINE_COMP; }

boolean ClosedSplineComp::IsA (ClassId id) {
    return CLOSEDSPLINE_COMP == id || VerticesComp::IsA(id);
}

Component* ClosedSplineComp::Copy () {
    return new ClosedSplineComp((SFH_ClosedBSpline*) GetGraphic()->Copy());
}

ClosedSplineComp::ClosedSplineComp (
    SFH_ClosedBSpline* graphic
) : VerticesComp(graphic) { }

SFH_ClosedBSpline* ClosedSplineComp::GetClosedSpline () {
    return (SFH_ClosedBSpline*) GetGraphic();
}

void ClosedSplineComp::Read (istream& in) {
    VerticesComp::Read(in);
    Coord* x, *y;
    int count;

    ReadVertices(in, x, y, count);
    SFH_ClosedBSpline* spline = new SFH_ClosedBSpline(x, y, count);
    delete x;
    delete y;

    spline->FillBg(ReadBgFilled(in));
    PSColor* fg = ReadColor(in);
    PSColor* bg = ReadColor(in);
    spline->SetColors(fg, bg);
    spline->SetBrush(ReadBrush(in));
    spline->SetPattern(ReadPattern(in));

    Transformer* t = ReadTransformer(in);
    spline->SetTransformer(t);
    Unref(t);

    SetGraphic(spline);
}

void ClosedSplineComp::Write (ostream& out) {
    VerticesComp::Write(out);
    SFH_ClosedBSpline* spline = GetClosedSpline();
    const Coord* x, *y;
    int count = spline->GetOriginal(x, y);

    WriteVertices(x, y, count, out);

    WriteBgFilled(spline->BgFilled(), out);
    WriteColor(spline->GetFgColor(), out);
    WriteColor(spline->GetBgColor(), out);
    WriteBrush(spline->GetBrush(), out);
    WritePattern(spline->GetPattern(), out);
    WriteTransformer(spline->GetTransformer(), out);
}

/****************************************************************************/

ClosedSplineView::ClosedSplineView (
    ClosedSplineComp* subj
) : VerticesView(subj) { }

ClosedSplineComp* ClosedSplineView::GetClosedSplineComp () { 
    return (ClosedSplineComp*) GetSubject();
}

ClassId ClosedSplineView::GetClassId () { return CLOSEDSPLINE_VIEW; }

boolean ClosedSplineView::IsA (ClassId id) {
    return CLOSEDSPLINE_VIEW == id || VerticesView::IsA(id);
}

boolean ClosedSplineView::VertexChanged () { 
    SFH_ClosedBSpline* gview = (SFH_ClosedBSpline*) GetGraphic();
    SFH_ClosedBSpline* gsubj
        = (SFH_ClosedBSpline*) GetClosedSplineComp()->GetGraphic();

    return *gview != *gsubj;
}

Manipulator* ClosedSplineView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Manipulator* m = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        v->Constrain(e.x, e.y);
        Coord x[1], y[1];
        x[0] = e.x;
        y[0] = e.y;
        GrowingVertices* rub = new GrowingClosedBSpline(
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
        GrowingClosedBSpline* rub = new GrowingClosedBSpline(
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

Command* ClosedSplineView::InterpretManipulator (Manipulator* m) {
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
            SFH_ClosedBSpline* cbs = new SFH_ClosedBSpline(x, y, n, pg);

            if (brVar != nil) cbs->SetBrush(brVar->GetBrush());
            if (patVar != nil) cbs->SetPattern(patVar->GetPattern());

            if (colVar != nil) {
                cbs->SetColors(colVar->GetFgColor(), colVar->GetBgColor());
            }
            cbs->SetTransformer(rel);
            Unref(rel);
            cmd = new PasteCmd(ed, new Clipboard(new ClosedSplineComp(cbs)));
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

        SFH_ClosedBSpline* cbs = new SFH_ClosedBSpline(x, y, n, GetGraphic());
	delete x;
	delete y;
        cbs->SetTransformer(rel);
        Unref(rel);
        cmd = new ReplaceCmd(ed, new ClosedSplineComp(cbs));

    } else {
        cmd = VerticesView::InterpretManipulator(m);
    }
    return cmd;
}

/*****************************************************************************/

ClassId PSClosedSpline::GetClassId () { return PS_CLOSEDSPLINE; }

boolean PSClosedSpline::IsA (ClassId id) { 
    return PS_CLOSEDSPLINE == id || PSVertices::IsA(id);
}

PSClosedSpline::PSClosedSpline (ClosedSplineComp* subj) : PSVertices(subj) { }
const char* PSClosedSpline::Name () { return "CBSpl"; }
