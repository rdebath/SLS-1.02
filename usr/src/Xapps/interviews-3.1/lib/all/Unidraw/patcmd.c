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
 * Pattern command implemention.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/editor.h>
#include <Unidraw/statevars.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/Commands/patcmd.h>

/*****************************************************************************/

ClassId PatternCmd::GetClassId () { return PATTERN_CMD; }

boolean PatternCmd::IsA (ClassId id) {
    return PATTERN_CMD == id || Command::IsA(id);
}

PatternCmd::PatternCmd (ControlInfo* c, PSPattern* pat) : Command(c) {
    _pat = pat;
}

PatternCmd::PatternCmd (Editor* ed, PSPattern* pat) : Command(ed) {_pat = pat;}

Command* PatternCmd::Copy () {
    Command* copy = new PatternCmd(CopyControlInfo(), GetPattern());
    InitCopy(copy);
    return copy;
}

void PatternCmd::Execute () {
    PatternVar* _patVar = (PatternVar*) GetEditor()->GetState("PatternVar");

    if (_patVar != nil) {
        _patVar->SetPattern(GetPattern());
    }
    Command::Execute();
}

void PatternCmd::Read (istream& in) {
    Command::Read(in);
    _pat = unidraw->GetCatalog()->ReadPattern(in);
}

void PatternCmd::Write (ostream& out) {
    Command::Write(out); 
    unidraw->GetCatalog()->WritePattern(GetPattern(), out);
}
