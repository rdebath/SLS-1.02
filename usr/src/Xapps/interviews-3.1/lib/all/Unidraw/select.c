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
 * Select tool definitions.
 */

#include <Unidraw/classes.h>
#include <Unidraw/editor.h>
#include <Unidraw/globals.h>
#include <Unidraw/iterator.h>
#include <Unidraw/manips.h>
#include <Unidraw/selection.h>
#include <Unidraw/viewer.h>
#include <Unidraw/Components/grview.h>
#include <Unidraw/Tools/select.h>

#include <InterViews/event.h>
#include <IV-2_6/InterViews/rubrect.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

ClassId SelectTool::GetClassId () { return SELECT_TOOL; }

boolean SelectTool::IsA (ClassId id) {
    return SELECT_TOOL == id || Tool::IsA(id);
}

SelectTool::SelectTool (ControlInfo* m) : Tool(m) { }
Tool* SelectTool::Copy () { return new SelectTool(CopyControlInfo()); }

Manipulator* SelectTool::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel
) {
    Manipulator* m = nil;
    GraphicView* views = v->GetGraphicView();
    Selection* s = v->GetSelection(), *newSel;

    newSel = views->ViewIntersecting(e.x-SLOP, e.y-SLOP, e.x+SLOP, e.y+SLOP);
    if (e.shift) {
        Localize(s, v);
    } else {
	s->Clear();
    }
    if (newSel->IsEmpty()) {		// select w/RubberRect if nothing hit
	m = new DragManip(v, new RubberRect(nil,nil, e.x,e.y,e.x,e.y), rel);
    } else {				// else user selected object directly
	s->Exclusive(newSel);
    }
    delete newSel;
    return m;
}

Command* SelectTool::InterpretManipulator (Manipulator* m) {
    DragManip* dm = (DragManip*) m;
    Viewer* viewer = dm->GetViewer();
    GraphicView* views = viewer->GetGraphicView();
    Selection* s = viewer->GetSelection();
    RubberRect* rr = (RubberRect*) dm->GetRubberband();
    Selection* newSel;
    Coord l, b, r, t;

    rr->GetCurrent(l, b, r, t);
    newSel = views->ViewsWithin(l, b, r, t);
    s->Exclusive(newSel);
    delete newSel;

    return nil;
}

void SelectTool::Localize (Selection* s, Viewer* v) {
    Iterator i;

    for (s->First(i); !s->Done(i);) {
        GraphicView* view = s->GetView(i);

        if (view->GetViewer() != v) {
            s->Remove(i);
            view->EraseHandles();

        } else {
            s->Next(i);
        }
    }
}
