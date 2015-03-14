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
 * Reshape tool definitions.
 */

#include <Unidraw/classes.h>
#include <Unidraw/globals.h>
#include <Unidraw/iterator.h>
#include <Unidraw/manip.h>
#include <Unidraw/selection.h>
#include <Unidraw/viewer.h>
#include <Unidraw/Components/grview.h>
#include <Unidraw/Tools/reshape.h>

#include <InterViews/event.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

ClassId ReshapeTool::GetClassId () { return RESHAPE_TOOL; }

boolean ReshapeTool::IsA (ClassId id) {
    return RESHAPE_TOOL == id || Tool::IsA(id);
}

ReshapeTool::ReshapeTool (ControlInfo* m) : Tool(m) { }
Tool* ReshapeTool::Copy () { return new ReshapeTool(CopyControlInfo()); }

Manipulator* ReshapeTool::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel
) {
    GraphicView* views = v->GetGraphicView();
    Selection* s = v->GetSelection(), *newSel;
    GraphicView* gv;
    Manipulator* m = nil;
    Iterator i;

    newSel = views->ViewIntersecting(e.x-SLOP, e.y-SLOP, e.x+SLOP, e.y+SLOP);
    if (!newSel->IsEmpty()) {
        s->Clear();
        newSel->First(i);
	gv = newSel->GetView(i);
        s->Append(gv);
        m = gv->CreateManipulator(v, e, rel, this);
    }
    delete newSel;
    return m;
}

Command* ReshapeTool::InterpretManipulator (Manipulator* m) {
    Selection* s;
    Command* cmd = nil;
    Iterator i;
    
    if (m != nil) {
        s = m->GetViewer()->GetSelection();
        s->First(i);
        cmd = s->GetView(i)->InterpretManipulator(m);
        s->Update();
    }
    return cmd;
}
