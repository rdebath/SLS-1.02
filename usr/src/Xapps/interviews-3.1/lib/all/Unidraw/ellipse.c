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
 * Ellipse component definitions.
 */

#include <Unidraw/classes.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/grid.h>
#include <Unidraw/manips.h>
#include <Unidraw/selection.h>
#include <Unidraw/statevars.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/align.h>
#include <Unidraw/Commands/edit.h>
#include <Unidraw/Commands/transforms.h>

#include <Unidraw/Components/ellipse.h>

#include <Unidraw/Graphic/ellipses.h>

#include <Unidraw/Tools/tool.h>

#include <InterViews/event.h>
#include <IV-2_6/InterViews/rubcurve.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

#include <stream.h>

/*****************************************************************************/

ClassId EllipseComp::GetClassId () { return ELLIPSE_COMP; }

boolean EllipseComp::IsA (ClassId id) {
    return ELLIPSE_COMP == id || GraphicComp::IsA(id);
}

Component* EllipseComp::Copy () {
    return new EllipseComp((SF_Ellipse*) GetGraphic()->Copy());
}

EllipseComp::EllipseComp (SF_Ellipse* graphic) : GraphicComp(graphic) { }
SF_Ellipse* EllipseComp::GetEllipse () { return (SF_Ellipse*) GetGraphic(); }

void EllipseComp::Read (istream& in) {
    GraphicComp::Read(in);
    Coord x0, y0;
    int r1, r2;

    in >> x0 >> y0 >> r1 >> r2;
    SF_Ellipse* ellipse = new SF_Ellipse(x0, y0, r1, r2);

    ellipse->FillBg(ReadBgFilled(in));
    PSColor* fg = ReadColor(in);
    PSColor* bg = ReadColor(in);
    ellipse->SetColors(fg, bg);
    ellipse->SetBrush(ReadBrush(in));
    ellipse->SetPattern(ReadPattern(in));

    Transformer* t = ReadTransformer(in);
    ellipse->SetTransformer(t);
    Unref(t);

    SetGraphic(ellipse);
}

void EllipseComp::Write (ostream& out) {
    GraphicComp::Write(out);
    SF_Ellipse* ellipse = GetEllipse();
    Coord x0, y0;
    int r1, r2;

    ellipse->GetOriginal(x0, y0, r1, r2);
    out << x0 << " " << y0 << " " << r1 << " " << r2 << " ";

    WriteBgFilled(ellipse->BgFilled(), out);
    WriteColor(ellipse->GetFgColor(), out);
    WriteColor(ellipse->GetBgColor(), out);
    WriteBrush(ellipse->GetBrush(), out);
    WritePattern(ellipse->GetPattern(), out);
    WriteTransformer(ellipse->GetTransformer(), out);
}

/*****************************************************************************/

EllipseComp* EllipseView::GetEllipseComp () { 
    return (EllipseComp*) GetSubject();
}

ClassId EllipseView::GetClassId () { return ELLIPSE_VIEW; }

boolean EllipseView::IsA (ClassId id) {
    return ELLIPSE_VIEW == id || GraphicView::IsA(id);
}

EllipseView::EllipseView (EllipseComp* subj) : GraphicView(subj) { }

void EllipseView::Interpret (Command* cmd) {
    if (cmd->IsA(ALIGNTOGRID_CMD)) {
        float cx, cy;
        GetGraphic()->GetCenter(cx, cy);
        ((AlignToGridCmd*) cmd)->Align(this, cx, cy);

    } else {
        GraphicView::Interpret(cmd);
    }
}

void EllipseView::Update () {
    Graphic* ellipse = GetGraphic();

    IncurDamage(ellipse);
    *ellipse = *GetEllipseComp()->GetGraphic();
    IncurDamage(ellipse);
    EraseHandles();
}

Manipulator* EllipseView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Rubberband* rub = nil;
    Manipulator* m = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        v->Constrain(e.x, e.y);
        rub = new RubberEllipse(nil, nil, e.x, e.y, e.x, e.y);
        m = new DragManip(
	    v, rub, rel, tool, DragConstraint(XYEqual | Gravity)
	);
    } else {
        m = GraphicView::CreateManipulator(v, e, rel, tool);
    }
    return m;
}

Command* EllipseView::InterpretManipulator (Manipulator* m) {
    DragManip* dm = (DragManip*) m;
    Editor* ed = dm->GetViewer()->GetEditor();
    Tool* tool = dm->GetTool();
    Transformer* rel = dm->GetTransformer();
    Command* cmd = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        RubberEllipse* re = (RubberEllipse*) dm->GetRubberband();
        Coord x, y, dummy1, dummy2;
        re->GetCurrent(x, y, dummy1, dummy2);

        if (dummy1 != x || dummy2 != y) {
            BrushVar* brVar = (BrushVar*) ed->GetState("BrushVar");
            PatternVar* patVar = (PatternVar*) ed->GetState("PatternVar");
            ColorVar* colVar = (ColorVar*) ed->GetState("ColorVar");
            Coord xr, yr;
            re->CurrentRadii(xr, yr);

            if (rel != nil) {
                rel = new Transformer(rel);
                rel->Invert();
            }

            Graphic* pg = GetGraphicComp()->GetGraphic();
            SF_Ellipse* ellipse = new SF_Ellipse(x, y, xr, yr, pg);

            if (brVar != nil) ellipse->SetBrush(brVar->GetBrush());
            if (patVar != nil) ellipse->SetPattern(patVar->GetPattern());

            if (colVar != nil) {
                ellipse->SetColors(colVar->GetFgColor(), colVar->GetBgColor());
            }
            ellipse->SetTransformer(rel);
            Unref(rel);
            cmd = new PasteCmd(ed, new Clipboard(new EllipseComp(ellipse)));
        }

    } else {
        cmd = GraphicView::InterpretManipulator(m);
    }
    return cmd;
}

Graphic* EllipseView::GetGraphic () {
    Graphic* graphic = GraphicView::GetGraphic();
    
    if (graphic == nil) {
        EllipseComp* ellipseComp = GetEllipseComp();
        graphic = ellipseComp->GetGraphic()->Copy();
        SetGraphic(graphic);
    }
    return graphic;
}

/*****************************************************************************/

PSEllipse::PSEllipse (EllipseComp* subj) : PostScriptView(subj) { }
ClassId PSEllipse::GetClassId () { return PS_ELLIPSE; }

boolean PSEllipse::IsA (ClassId id) { 
    return PS_ELLIPSE == id || PostScriptView::IsA(id);
}

boolean PSEllipse::Definition (ostream& out) {
    Coord x0, y0;
    int rx, ry;

    EllipseComp* comp = (EllipseComp*) GetSubject();
    comp->GetEllipse()->GetOriginal(x0, y0, rx, ry);

    out << "Begin " << MARK << " Elli\n";
    MinGS(out);
    out << MARK << "\n";
    out << x0 << " " << y0 << " " << rx << " " << ry << " Elli\n";
    out << "End\n\n";

    return out.good();
}
