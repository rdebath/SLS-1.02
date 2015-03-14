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
 * Graphic component tool definitions.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/editor.h>
#include <Unidraw/manip.h>
#include <Unidraw/selection.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>
#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/grview.h>
#include <Unidraw/Tools/grcomptool.h>

#include <InterViews/event.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

ClassId GraphicCompTool::GetClassId () { return GRAPHIC_COMP_TOOL; }

boolean GraphicCompTool::IsA (ClassId id) {
    return GRAPHIC_COMP_TOOL == id || Tool::IsA(id);
}

GraphicCompTool::GraphicCompTool () { }

GraphicCompTool::GraphicCompTool (
    ControlInfo* m, GraphicComp* proto
) : Tool(m) {
    Init(proto);
}

GraphicCompTool::~GraphicCompTool () {
    delete _protoview;
    delete _prototype;
}

Tool* GraphicCompTool::Copy () {
    return new GraphicCompTool(CopyControlInfo(), GetPrototype());
}

Manipulator* GraphicCompTool::CreateManipulator (
    Viewer* v, Event& e, Transformer* t
) {
    v->GetSelection()->Clear();
    return _protoview->CreateManipulator(v, e, t, this);
}

Command* GraphicCompTool::InterpretManipulator (Manipulator* m) {
    return _protoview->InterpretManipulator(m);
}

void GraphicCompTool::Init (GraphicComp* prototype) {
    _prototype = prototype;
    _protoview = (GraphicView*) _prototype->Create(COMPONENT_VIEW);
    _prototype->Attach(_protoview);
    _protoview->Update();
}

void GraphicCompTool::Read (istream& in) {
    Tool::Read(in);
    Init((GraphicComp*) unidraw->GetCatalog()->ReadComponent(in));
}

void GraphicCompTool::Write (ostream& out) {
    Tool::Write(out);
    unidraw->GetCatalog()->WriteComponent(GetPrototype(), out);
}
