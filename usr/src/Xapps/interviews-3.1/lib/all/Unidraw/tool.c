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
 * Implementation of Tool and derived classes.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/ctrlinfo.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/Tools/tool.h>

#include <stream.h>

/*****************************************************************************/

ClassId Tool::GetClassId () { return TOOL; }
ClassId Tool::GetSubstId (const char*&) { return UNDEFINED_CLASS; }
boolean Tool::IsA (ClassId id) { return TOOL == id; }
Tool::Tool (ControlInfo* m) { Tool::SetControlInfo(m); }

Tool::~Tool () { 
    if (_ctrlInfo != nil) {
        delete _ctrlInfo;
    }
    unidraw->GetCatalog()->Forget(this);
}

Tool* Tool::Copy () { return new Tool(CopyControlInfo()); }

void Tool::Read (istream& in) {
    SetControlInfo(unidraw->GetCatalog()->ReadControlInfo(in));
}

void Tool::Write (ostream& out) {
    unidraw->GetCatalog()->WriteControlInfo(GetControlInfo(), out);
}

Manipulator* Tool::CreateManipulator (Viewer*, Event&, Transformer*) {
    return nil;
}

Command* Tool::InterpretManipulator (Manipulator*) { return nil; }

void Tool::SetControlInfo (ControlInfo* m) {
    _ctrlInfo = m;
    if (m != nil) {
	m->SetOwner(this);
    }
}

ControlInfo* Tool::GetControlInfo () { return _ctrlInfo; }

ControlInfo* Tool::CopyControlInfo () {
    return (_ctrlInfo == nil) ? nil : _ctrlInfo->Copy();
}
