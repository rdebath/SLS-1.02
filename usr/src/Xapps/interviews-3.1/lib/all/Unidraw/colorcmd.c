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
 * Color command implementation.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/editor.h>
#include <Unidraw/statevars.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/Commands/colorcmd.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

ClassId ColorCmd::GetClassId () { return COLOR_CMD; }
boolean ColorCmd::IsA (ClassId id) {return COLOR_CMD==id || Command::IsA(id);}

ColorCmd::ColorCmd (ControlInfo* c, PSColor* fg, PSColor* bg) : Command(c) {
    _fg = fg;
    _bg = bg;
}

ColorCmd::ColorCmd (Editor* ed, PSColor* fg, PSColor* bg) : Command(ed) { 
    _fg = fg;
    _bg = bg;
}

Command* ColorCmd::Copy () {
    Command* copy = new ColorCmd(CopyControlInfo(), GetFgColor(),GetBgColor());
    InitCopy(copy);
    return copy;
}

void ColorCmd::Execute () {
    ColorVar* colorVar = (ColorVar*) GetEditor()->GetState("ColorVar");

    if (colorVar != nil) {
        PSColor* fg = (_fg == nil) ? colorVar->GetFgColor() : _fg;
        PSColor* bg = (_bg == nil) ? colorVar->GetBgColor() : _bg;

        colorVar->SetColors(fg, bg);
    }
    Command::Execute();
}

void ColorCmd::Read (istream& in) {
    Command::Read(in);
    Catalog* catalog = unidraw->GetCatalog();

    _fg = catalog->ReadColor(in);
    _bg = catalog->ReadColor(in);
}

void ColorCmd::Write (ostream& out) {
    Command::Write(out);
    Catalog* catalog = unidraw->GetCatalog();

    catalog->WriteColor(GetFgColor(), out);
    catalog->WriteColor(GetBgColor(), out);
}
