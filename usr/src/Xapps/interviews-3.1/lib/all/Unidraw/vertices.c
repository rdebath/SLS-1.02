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
 * Vertices component definitions.
 */

#include <Unidraw/classes.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/align.h>

#include <Unidraw/Components/vertices.h>

#include <Unidraw/Graphic/util.h>
#include <Unidraw/Graphic/verts.h>

#include <IV-2_6/InterViews/rubcurve.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

#include <stream.h>

/****************************************************************************/

ClassId VerticesComp::GetClassId () { return VERTICES_COMP; }

boolean VerticesComp::IsA (ClassId id) {
    return VERTICES_COMP == id || GraphicComp::IsA(id);
}

VerticesComp::VerticesComp (Vertices* graphic) : GraphicComp(graphic) { }
Vertices* VerticesComp::GetVertices () { return (Vertices*) GetGraphic(); }

/****************************************************************************/

VerticesComp* VerticesView::GetVerticesComp () { 
    return (VerticesComp*) GetSubject();
}

ClassId VerticesView::GetClassId () { return VERTICES_VIEW; }

boolean VerticesView::IsA (ClassId id) {
    return VERTICES_VIEW == id || GraphicView::IsA(id);
}

VerticesView::VerticesView (VerticesComp* subj) : GraphicView(subj) { }

void VerticesView::Update () {
    Graphic* vertices = GetGraphic();

    IncurDamage(vertices);
    if (VertexChanged()) {
        // unimplemented
    }
    *vertices = *GetVerticesComp()->GetGraphic();
    IncurDamage(vertices);
    EraseHandles();
}

void VerticesView::CreateHandles () {
    Coord* x, *y;
    int n;
    Viewer* v = GetViewer();
    
    if (v != nil) {
        GetVertices(x, y, n);
        _handles = new RubberHandles(nil, nil, x, y, n, 0, HANDLE_SIZE);
        v->InitRubberband(_handles);
        delete x;
        delete y;
    }
}

boolean VerticesView::VertexChanged () { return false; }

void VerticesView::Interpret (Command* cmd) {
    if (cmd->IsA(ALIGNTOGRID_CMD)) {
        Vertices* verts = (Vertices*) GetGraphic();
        Transformer total;
        verts->TotalTransformation(total);

        float tx0, ty0;
        const Coord* x, *y;
        int n = verts->GetOriginal(x, y);
        total.Transform(float(x[0]), float(y[0]), tx0, ty0);
        ((AlignToGridCmd*) cmd)->Align(this, tx0, ty0);

    } else {
        GraphicView::Interpret(cmd);
    }
}

void VerticesView::GetVertices (Coord*& x, Coord*& y, int& n) {
    Vertices* vertices = (Vertices*) GetGraphic();
    Transformer t;
    const Coord* origx, *origy;

    n = vertices->GetOriginal(origx, origy);
    ArrayDup(origx, origy, n, x, y);
    vertices->TotalTransformation(t);
    t.TransformList(x, y, n);
}

Graphic* VerticesView::GetGraphic () {
    Graphic* graphic = GraphicView::GetGraphic();

    if (graphic == nil) {
        VerticesComp* verticesComp = GetVerticesComp();
        graphic = verticesComp->GetGraphic()->Copy();
        SetGraphic(graphic);
    }
    return graphic;
}

/****************************************************************************/

ClassId PSVertices::GetClassId () { return PS_VERTICES; }

boolean PSVertices::IsA (ClassId id) { 
    return PS_VERTICES == id || PostScriptView::IsA(id);
}

PSVertices::PSVertices (VerticesComp* subj) : PostScriptView(subj) { }
const char* PSVertices::Name () { return ""; }

boolean PSVertices::Definition (ostream& out) {
    const Coord* x;
    const Coord* y;
    int n;

    VerticesComp* comp = (VerticesComp*) GetSubject();
    n = comp->GetVertices()->GetOriginal(x, y);

    out << "Begin " << MARK << " " << Name() << "\n";
    MinGS(out);
    out << MARK << " " << n << "\n";
    for (int i = 0; i < n; i++) {
        out << x[i] << " " << y[i] << "\n";
    }
    out << n << " " << Name() << "\n";
    out << "End\n\n";

    return out.good();
}
