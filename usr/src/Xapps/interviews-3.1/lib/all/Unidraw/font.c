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
 * Font command implementation.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/editor.h>
#include <Unidraw/statevars.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/Commands/font.h>

/*****************************************************************************/

ClassId FontCmd::GetClassId () { return FONT_CMD; }

boolean FontCmd::IsA (ClassId id) {
    return FONT_CMD == id || Command::IsA(id);
}

FontCmd::FontCmd (ControlInfo* c, PSFont* f) : Command(c) {_font = f; }
FontCmd::FontCmd (Editor* ed, PSFont* f) : Command(ed) { _font = f; }

Command* FontCmd::Copy () {
    Command* copy = new FontCmd(CopyControlInfo(), GetFont());
    InitCopy(copy);
    return copy;
}

void FontCmd::Execute () {
    FontVar* _fontVar = (FontVar*) GetEditor()->GetState("FontVar");

    if (_fontVar != nil) {
        _fontVar->SetFont(GetFont());
    }
    Command::Execute();
}

void FontCmd::Read (istream& in) {
    Command::Read(in);
    _font = unidraw->GetCatalog()->ReadFont(in);
}

void FontCmd::Write (ostream& out) {
    Command::Write(out);
    unidraw->GetCatalog()->WriteFont(GetFont(), out);
}
