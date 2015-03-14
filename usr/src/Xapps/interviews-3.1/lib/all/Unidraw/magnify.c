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
 * Magnify tool definitions.
 */

#include <Unidraw/classes.h>
#include <Unidraw/globals.h>
#include <Unidraw/manips.h>
#include <Unidraw/viewer.h>
#include <Unidraw/Tools/magnify.h>

#include <InterViews/event.h>
#include <IV-2_6/InterViews/perspective.h>
#include <IV-2_6/InterViews/rubrect.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

ClassId MagnifyTool::GetClassId () { return MAGNIFY_TOOL; }

boolean MagnifyTool::IsA (ClassId id) {
    return MAGNIFY_TOOL == id || Tool::IsA(id);
}

MagnifyTool::MagnifyTool (ControlInfo* m) : Tool(m) { }
Tool* MagnifyTool::Copy () { return new MagnifyTool(CopyControlInfo()); }

Manipulator* MagnifyTool::CreateManipulator (
    Viewer* v, Event& e, Transformer*
) {
    RubberRect* rr = new RubberRect(nil, nil, e.x, e.y, e.x, e.y);
    return new DragManip(v, rr);
}

Command* MagnifyTool::InterpretManipulator (Manipulator* m) {
    Viewer* v = m->GetViewer();
    DragManip* dm = (DragManip*) m;
    RubberRect* rr = (RubberRect*) dm->GetRubberband();
    Coord l, r, b, t;

    rr->GetCurrent(l, b, r, t);
    v->Magnify(l, b, r, t);
    return nil;
}
