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
 * Rect component definitions.
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

#include <Unidraw/Components/polygon.h>
#include <Unidraw/Components/rect.h>

#include <Unidraw/Graphic/polygons.h>

#include <Unidraw/Tools/tool.h>

#include <InterViews/event.h>
#include <IV-2_6/InterViews/rubcurve.h>
#include <IV-2_6/InterViews/rubgroup.h>
#include <IV-2_6/InterViews/rubline.h>
#include <IV-2_6/InterViews/rubrect.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

#include <stream.h>

/*****************************************************************************/

ClassId RectComp::GetClassId () { return RECT_COMP; }

boolean RectComp::IsA (ClassId id) {
    return RECT_COMP == id || GraphicComp::IsA(id);
}

Component* RectComp::Copy () {
    return new RectComp((SF_Rect*) GetGraphic()->Copy());
}

RectComp::RectComp (SF_Rect* graphic) : GraphicComp(graphic) { }
SF_Rect* RectComp::GetRect () { return (SF_Rect*) GetGraphic(); }

void RectComp::Read (istream& in) {
    GraphicComp::Read(in);
    Coord x0, y0, x1, y1;

    in >> x0 >> y0 >> x1 >> y1;
    SF_Rect* rect = new SF_Rect(x0, y0, x1, y1);

    rect->FillBg(ReadBgFilled(in));
    PSColor* fg = ReadColor(in);
    PSColor* bg = ReadColor(in);
    rect->SetColors(fg, bg);
    rect->SetBrush(ReadBrush(in));
    rect->SetPattern(ReadPattern(in));

    Transformer* t = ReadTransformer(in);
    rect->SetTransformer(t);
    Unref(t);

    SetGraphic(rect);
}

void RectComp::Write (ostream& out) {
    GraphicComp::Write(out);
    SF_Rect* rect = GetRect();
    Coord x0, y0, x1, y1;

    rect->GetOriginal(x0, y0, x1, y1);
    out << x0 << " " << y0 << " " << x1 << " " << y1 << " ";

    WriteBgFilled(rect->BgFilled(), out);
    WriteColor(rect->GetFgColor(), out);
    WriteColor(rect->GetBgColor(), out);
    WriteBrush(rect->GetBrush(), out);
    WritePattern(rect->GetPattern(), out);
    WriteTransformer(rect->GetTransformer(), out);
}

/*****************************************************************************/

RectComp* RectView::GetRectComp () { return (RectComp*) GetSubject(); }
ClassId RectView::GetClassId () { return RECT_VIEW; }

boolean RectView::IsA (ClassId id) {
    return RECT_VIEW == id || GraphicView::IsA(id);
}

RectView::RectView (RectComp* subj) : GraphicView(subj) { }

void RectView::Interpret (Command* cmd) {
    if (cmd->IsA(ALIGNTOGRID_CMD)) {
        SF_Rect* rect = (SF_Rect*) GetGraphic();
        Transformer total;
        rect->TotalTransformation(total);

        Coord x0, y0, x1, y1;
        float tx0, ty0;

        rect->GetOriginal(x0, y0, x1, y1);
        total.Transform(float(x0), float(y0), tx0, ty0);
        ((AlignToGridCmd*) cmd)->Align(this, tx0, ty0);

    } else {
        GraphicView::Interpret(cmd);
    }
}

void RectView::Update () {
    Graphic* rect = GetGraphic();

    IncurDamage(rect);
    *rect = *GetRectComp()->GetGraphic();
    IncurDamage(rect);
    EraseHandles();
}

void RectView::CreateHandles () {
    Coord x[4], y[4];
    Viewer* v = GetViewer();
    
    if (v != nil) {
        GetCorners(x, y);
        _handles = new RubberHandles(nil, nil, x, y, 4, 0, HANDLE_SIZE);
        v->InitRubberband(_handles);
    }
}

Manipulator* RectView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Coord x[5], y[5];
    Rubberband* rub = nil;
    Manipulator* m = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        v->Constrain(e.x, e.y);
        rub = new RubberRect(nil, nil, e.x, e.y, e.x, e.y);
        m = new DragManip(
	    v, rub, rel, tool, DragConstraint(XYEqual | Gravity)
	);

    } else if (tool->IsA(RESHAPE_TOOL)) {
	RubberGroup* rub = new RubberGroup(nil, nil);
	Coord x[4], y[4];
        v->Constrain(e.x, e.y);
	GetCorners(x, y);
	_reshapeCorner = ClosestPoint(x, y, 4, e.x, e.y);

	if (_reshapeCorner > 0) {
	    rub->Append(
		new RubberLine(
                    nil, nil, x[_reshapeCorner-1], y[_reshapeCorner-1], e.x,e.y
                )
	    );
	} else { 
	    rub->Append(new RubberLine(nil,nil,x[3],y[3],e.x,e.y));
	}

	if (_reshapeCorner < 3) {
	    rub->Append(
		new RubberLine(
                    nil, nil, x[_reshapeCorner+1], y[_reshapeCorner+1], e.x,e.y
                )
	    );
	} else { 
	    rub->Append(new RubberLine(nil, nil, x[0], y[0], e.x, e.y));
	}
        m = new DragManip(
	    v, rub, rel, tool, DragConstraint(HorizOrVert | Gravity)
	);

    } else if (tool->IsA(MOVE_TOOL)) {
        v->Constrain(e.x, e.y);
        GetCorners(x, y);
        x[4] = x[0]; y[4] = y[0];
	rub = new SlidingLineList(nil, nil, x, y, 5, e.x, e.y);
        m = new DragManip(
	    v, rub, rel, tool, DragConstraint(HorizOrVert | Gravity)
	);

    } else if (tool->IsA(SCALE_TOOL)) {
        v->Constrain(e.x, e.y);
        GetCorners(x, y);
        x[4] = x[0]; y[4] = y[0];
        rub = new ScalingLineList(nil,nil,x,y,5, (x[0]+x[2])/2, (y[0]+y[2])/2);
        m = new DragManip(v, rub, rel, tool, Gravity);

    } else if (tool->IsA(ROTATE_TOOL)) {
        v->Constrain(e.x, e.y);
        GetCorners(x, y);
        x[4] = x[0]; y[4] = y[0];
        rub = new RotatingLineList(
            nil, nil, x, y, 5, (x[0]+x[2])/2, (y[0]+y[2])/2, e.x, e.y
        );
        m = new DragManip(v, rub, rel, tool, Gravity);

    } else {
        m = GraphicView::CreateManipulator(v, e, rel, tool);
    }
    return m;
}

Command* RectView::InterpretManipulator (Manipulator* m) {
    DragManip* dm = (DragManip*) m;
    Editor* ed = dm->GetViewer()->GetEditor();
    Tool* tool = dm->GetTool();
    Transformer* rel = dm->GetTransformer();
    Command* cmd = nil;

    if (tool->IsA(GRAPHIC_COMP_TOOL)) {
        RubberRect* rr = (RubberRect*) dm->GetRubberband();
        Coord x0, y0, x1, y1;
        rr->GetCurrent(x0, y0, x1, y1);

        if (x0 != x1 || y0 != y1) {
            BrushVar* brVar = (BrushVar*) ed->GetState("BrushVar");
            PatternVar* patVar = (PatternVar*) ed->GetState("PatternVar");
            ColorVar* colVar = (ColorVar*) ed->GetState("ColorVar");

            if (rel != nil) {
                rel = new Transformer(rel);
                rel->Invert();
            }

            Graphic* pg = GetGraphicComp()->GetGraphic();
            SF_Rect* rect = new SF_Rect(x0, y0, x1, y1, pg);

            if (brVar != nil) rect->SetBrush(brVar->GetBrush());
            if (patVar != nil) rect->SetPattern(patVar->GetPattern());

            if (colVar != nil) {
                rect->SetColors(colVar->GetFgColor(), colVar->GetBgColor());
            }
            rect->SetTransformer(rel);
            Unref(rel);
            cmd = new PasteCmd(ed, new Clipboard(new RectComp(rect)));
        }

    } else if (tool->IsA(RESHAPE_TOOL)) {
        RubberGroup* rubberGroup = (RubberGroup*) dm->GetRubberband();
	RubberLine* rubberLine = (RubberLine*) rubberGroup->First();
        SF_Polygon* polygon;
        Coord x[4], y[4];
	Coord x0, y0;
        
	GetCorners(x, y);
	rubberLine->GetCurrent(x0, y0, x[_reshapeCorner], y[_reshapeCorner]);

        if (rel != nil) {
            rel = new Transformer(rel);
            rel->Invert();
        }
        polygon = new SF_Polygon(x, y, 4, GetGraphic());
        polygon->SetTransformer(rel);
        Unref(rel);
        cmd = new ReplaceCmd(ed, new PolygonComp(polygon));

    } else if (tool->IsA(MOVE_TOOL)) {
        SlidingLineList* sll;
        Transformer* rel = dm->GetTransformer();
        Coord* ox, *oy, *cx, *cy;
        float fx0, fy0, fx1, fy1;
        int n;

        sll = (SlidingLineList*) dm->GetRubberband();
        sll->GetOriginal(ox, oy, n);
        sll->GetCurrent(cx, cy, n);
        if (rel != nil) {
            rel->InvTransform(float(ox[0]), float(oy[0]), fx0, fy0);
            rel->InvTransform(float(cx[0]), float(cy[0]), fx1, fy1);
        }
        delete ox; delete oy; delete cx; delete cy;
        cmd = new MoveCmd(ed, fx1 - fx0, fy1 - fy0);

    } else if (tool->IsA(SCALE_TOOL)) {
        ScalingLineList* sll = (ScalingLineList*) dm->GetRubberband();
        float sxy = sll->CurrentScaling();

        cmd = new ScaleCmd(ed, sxy, sxy);

    } else if (tool->IsA(ROTATE_TOOL)) {
        RotatingLineList* rll = (RotatingLineList*) dm->GetRubberband();
        float angle = rll->CurrentAngle() - rll->OriginalAngle();

        cmd = new RotateCmd(ed, angle);

    } else {
        cmd = GraphicView::InterpretManipulator(m);
    }
    return cmd;
}

void RectView::GetCorners (Coord* x, Coord* y) {
    SF_Rect* rect = (SF_Rect*) GetGraphic();
    Coord tx[4], ty[4];
    Transformer t;

    rect->GetOriginal(tx[0], ty[0], tx[2], ty[2]);
    rect->GetOriginal(tx[3], ty[1], tx[1], ty[3]);
    rect->TotalTransformation(t);
    t.TransformList((Coord*) tx, (Coord*) ty, 4, x, y);
}

Graphic* RectView::GetGraphic () {
    Graphic* graphic = GraphicView::GetGraphic();
    
    if (graphic == nil) {
        RectComp* rectComp = GetRectComp();
        graphic = rectComp->GetGraphic()->Copy();
        SetGraphic(graphic);
    }
    return graphic;
}

/*****************************************************************************/

PSRect::PSRect (RectComp* subj) : PostScriptView(subj) { }
ClassId PSRect::GetClassId () { return PS_RECT; }

boolean PSRect::IsA (ClassId id) { 
    return PS_RECT == id || PostScriptView::IsA(id);
}

boolean PSRect::Definition (ostream& out) {
    Coord l, b, r, t;

    RectComp* comp = (RectComp*) GetSubject();
    comp->GetRect()->GetOriginal(l, b, r, t);

    out << "Begin " << MARK << " Rect\n";
    MinGS(out);
    out << MARK << "\n";
    out << l << " " << b << " " << r << " " << t << " Rect\n";
    out << "End\n\n";

    return out.good();
}
