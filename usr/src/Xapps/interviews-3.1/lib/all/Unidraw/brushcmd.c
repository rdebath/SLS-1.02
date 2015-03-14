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
 * Brush command implmentation.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/editor.h>
#include <Unidraw/statevars.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/Commands/brushcmd.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

ClassId BrushCmd::GetClassId () { return BRUSH_CMD; }

boolean BrushCmd::IsA (ClassId id) {
    return BRUSH_CMD == id || Command::IsA(id);
}

BrushCmd::BrushCmd (ControlInfo* c, PSBrush* b) : Command(c) {
    _br = b;
}

BrushCmd::BrushCmd (Editor* ed, PSBrush* b) : Command(ed) { _br = b; }

Command* BrushCmd::Copy () {
    Command* copy = new BrushCmd(CopyControlInfo(), GetBrush());
    InitCopy(copy);
    return copy;
}

void BrushCmd::Execute () {
    BrushVar* brVar = (BrushVar*) GetEditor()->GetState("BrushVar");

    if (brVar != nil) {
        brVar->SetBrush(GetBrush());
    }
    Command::Execute();
}

void BrushCmd::Read (istream& in) {
    Command::Read(in);
    _br = unidraw->GetCatalog()->ReadBrush(in);
}

void BrushCmd::Write (ostream& out) {
    Command::Write(out); 
    unidraw->GetCatalog()->WriteBrush(GetBrush(), out);
}
