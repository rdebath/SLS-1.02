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
 * Polygon component definitions.
 */

#include <Unidraw/classes.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/manips.h>
#include <Unidraw/statevars.h>
#include <Unidraw/viewer.h>
#include <Unidraw/Commands/edit.h>
#include <Unidraw/Components/polygon.h>
#include <Unidraw/Graphic/polygons.h>
#include <Unidraw/Tools/tool.h>

#include <InterViews/event.h>
#include <IV-2_6/InterViews/rubline.h>
#include <IV-2_6/InterViews/rubverts.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

#include <stream.h>

/****************************************************************************/

ClassId PolygonComp::GetClassId () { return POLYGON_COMP; }

boolean PolygonComp::IsA (ClassId id) {
    return POLYGON_COMP == id || VerticesComp::IsA(id);
}

Component* PolygonComp::Copy () { 
    return new PolygonComp((SF_Polygon*) GetGraphic()->Copy());
}

PolygonComp::PolygonComp (SF_Polygon* graphic) : VerticesComp(graphic) { }

SF_Polygon* PolygonComp::GetPolygon () {
    return (SF_Polygon*) GetGraphic();
}

void PolygonComp::Read (istream& in) {
    VerticesComp::Read(in);
    Coord* x, *y;
    int count;

    ReadVertices(in, x, y, count);
    SF_Polygon* poly = new SF_Polygon(x, y, count);
    delete x;
    delete y;

    poly->FillBg(ReadBgFilled(in));
    PSColor* fg = ReadColor(in);
    PSColor* bg = ReadColor(in);
    poly->SetColors(fg, bg);
    poly->SetBrush(ReadBrush(in));
    poly->SetPattern(ReadPattern(in));

    Transformer* t = ReadTransformer(in);
    poly->SetTransformer(t);
    Unref(t);

    SetGraphic(poly);
}

void PolygonComp::Write (ostream& out) {
    VerticesComp::Write(out);
    SF_Polygon* poly = GetPolygon();
    const Coord* x, *y;
    int count = poly->GetOriginal(x, y);

    WriteVertices(x, y, count, out);

    WriteBgFilled(poly->BgFilled(), out);
    WriteColor(poly->GetFgColor(), out);
    WriteColor(poly->GetBgColor(), out);
    WriteBrush(poly->GetBrush(), out);
    WritePattern(poly->GetPattern(), out);
    WriteTransformer(poly->GetTransformer(), out);
}

/****************************************************************************/

PolygonView::PolygonView (PolygonComp* subj) : VerticesView(subj) { }

PolygonComp* PolygonView::GetPolygonComp () { 
    return (PolygonComp*) GetSubject();
}

ClassId PolygonView::GetClassId () { return POLYGON_VIEW; }

boolean PolygonView::IsA (ClassId id) {
    return POLYGON_VIEW == id || VerticesView::IsA(id);
}

boolean PolygonView::VertexChanged () { 
    SF_Polygon* gview = (SF_Polygon*) GetGraphic();
    SF_Polygon* gsubj = (SF_Polygon*) GetPolygonComp()->GetGraphic();

    return *gview != *gsubj;
}

Manipulator* PolygonView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Manipulator* m = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        v->Constrain(e.x, e.y);
        Coord x[1], y[1];
        x[0] = e.x;
        y[0] = e.y;
        GrowingVertices* rub = new GrowingPolygon(
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
        GrowingPolygon* rub = new GrowingPolygon(
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

Command* PolygonView::InterpretManipulator (Manipulator* m) {
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
            SF_Polygon* polygon = new SF_Polygon(x, y, n, pg);

            if (brVar != nil) polygon->SetBrush(brVar->GetBrush());
            if (patVar != nil) polygon->SetPattern(patVar->GetPattern());

            if (colVar != nil) {
                polygon->SetColors(colVar->GetFgColor(), colVar->GetBgColor());
            }
            polygon->SetTransformer(rel);
            Unref(rel);
            cmd = new PasteCmd(ed, new Clipboard(new PolygonComp(polygon)));
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

        SF_Polygon* polygon = new SF_Polygon(x, y, n, GetGraphic());
	delete x;
	delete y;
        polygon->SetTransformer(rel);
        Unref(rel);
        cmd = new ReplaceCmd(ed, new PolygonComp(polygon));

    } else {
        cmd = VerticesView::InterpretManipulator(m);
    }
    return cmd;
}

/*****************************************************************************/

ClassId PSPolygon::GetClassId () { return PS_POLYGON; }

boolean PSPolygon::IsA (ClassId id) { 
    return PS_POLYGON == id || PSVertices::IsA(id);
}

PSPolygon::PSPolygon (PolygonComp* subj) : PSVertices(subj) { }
const char* PSPolygon::Name () { return "Poly"; }
