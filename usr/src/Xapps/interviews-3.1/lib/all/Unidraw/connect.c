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
 * Connect tool definitions.
 */

#include <Unidraw/classes.h>
#include <Unidraw/globals.h>
#include <Unidraw/selection.h>
#include <Unidraw/viewer.h>
#include <Unidraw/Components/connector.h>
#include <Unidraw/Tools/connect.h>

#include <InterViews/event.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

ConnectTool::ConnectTool (ControlInfo* m) : Tool(m) { }
ClassId ConnectTool::GetClassId () { return CONNECT_TOOL; }

boolean ConnectTool::IsA (ClassId id) {
    return CONNECT_TOOL == id || Tool::IsA(id);
}

Tool* ConnectTool::Copy () {
    return new ConnectTool(CopyControlInfo());
}

Manipulator* ConnectTool::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel
) {
    GraphicView* views = v->GetGraphicView();
    Selection* s = v->GetSelection();
    Manipulator* m = nil;

    _source = views->ConnectorIntersecting(
        e.x-SLOP, e.y-SLOP, e.x+SLOP, e.y+SLOP
    );
    if (_source == nil) {
	s->Clear();
    } else {
        m = _source->CreateManipulator(v, e, rel, this);
    }
    return m;
}

Command* ConnectTool::InterpretManipulator (Manipulator* m) {
    return (_source == nil) ? nil : _source->InterpretManipulator(m);
}
