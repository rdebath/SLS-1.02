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
 * MacroCmd implementation.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/globals.h>
#include <Unidraw/iterator.h>
#include <Unidraw/ulist.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/Commands/macro.h>

#include <stream.h>

/*****************************************************************************/

ClassId MacroCmd::GetClassId () { return MACRO_CMD; }

boolean MacroCmd::IsA (ClassId id) {
    return MACRO_CMD == id || Command::IsA(id);
}

MacroCmd::MacroCmd (ControlInfo* c) : Command(c) { _cmds = new UList; }

MacroCmd::MacroCmd(
    Editor* ed, Command* c1, Command* c2, Command* c3, Command* c4
) : Command(ed) {
    _cmds = new UList;

    if (c1 != nil) {
        Append(c1, c2, c3, c4);
    }
}

Command* MacroCmd::Cmd (UList* r) { return (Command*) (*r)(); }

Command* MacroCmd::Copy () {
    MacroCmd* macro;
    Iterator i;

    macro = new MacroCmd(CopyControlInfo());
    InitCopy(macro);

    for (First(i); !Done(i); Next(i)) {
        macro->Append(GetCommand(i));
    }
    return macro;
}

void MacroCmd::Execute () {
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
        GetCommand(i)->Execute();
    }
}

void MacroCmd::Unexecute () {
    Iterator i;

    for (Last(i); !Done(i); Prev(i)) {
        GetCommand(i)->Unexecute();
    }
}

void MacroCmd::SetEditor (Editor* ed) {
    Command::SetEditor(ed);
    Iterator i;
    
    for (First(i); !Done(i); Next(i)) {
        GetCommand(i)->SetEditor(ed);
    }
}

boolean MacroCmd::Reversible () {
    Iterator i;
    
    for (First(i); !Done(i); Next(i)) {
        if (GetCommand(i)->Reversible()) {
            return true;
        }
    }
    return false;
}

UList* MacroCmd::Elem (Iterator& i) { return (UList*) i.GetValue(); }
void MacroCmd::First (Iterator& i) { i.SetValue(_cmds->First()); }
void MacroCmd::Last (Iterator& i) { i.SetValue(_cmds->Last()); }
void MacroCmd::Next (Iterator& i) { i.SetValue(Elem(i)->Next()); }
void MacroCmd::Prev (Iterator& i) { i.SetValue(Elem(i)->Prev()); }
boolean MacroCmd::Done (Iterator& i) { return Elem(i) == _cmds->End(); }
Command* MacroCmd::GetCommand (Iterator& i) { return Cmd(Elem(i)); }

void MacroCmd::SetCommand (Command* cmd, Iterator& i) {
    i.SetValue(_cmds->Find(cmd));
}

void MacroCmd::Append (Command* c1, Command* c2, Command* c3, Command* c4) {
    _cmds->Append(new UList(c1));

    if (c2 != nil) {
        _cmds->Append(new UList(c2));

        if (c3 != nil) {
            _cmds->Append(new UList(c3));

            if (c4 != nil) {
                _cmds->Append(new UList(c4));
            }
        }
    }
}

void MacroCmd::Prepend (Command* c1, Command* c2, Command* c3, Command* c4) {
    _cmds->Prepend(new UList(c1));

    if (c2 != nil) {
        _cmds->Prepend(new UList(c2));

        if (c3 != nil) {
            _cmds->Prepend(new UList(c3));

            if (c4 != nil) {
                _cmds->Prepend(new UList(c4));
            }
        }
    }
}

void MacroCmd::InsertBefore (Iterator& i, Command* cmd) {
    Elem(i)->Append(new UList(cmd));
}

void MacroCmd::InsertAfter (Iterator& i, Command* cmd) {
    Elem(i)->Prepend(new UList(cmd));
}

void MacroCmd::Remove (Iterator& i) {
    UList* doomed = Elem(i);
    Command* cmd = Cmd(doomed);

    Next(i);
    _cmds->Remove(doomed);
    delete doomed;
}

void MacroCmd::Remove (Command* cmd) { _cmds->Delete(cmd); }

MacroCmd::~MacroCmd () { 
    while (!_cmds->IsEmpty()) {
	UList* cur = _cmds->First();
	_cmds->Remove(cur);
        Command* cmd = Cmd(cur);
	delete cmd;
	delete cur;
    }
    delete _cmds;
}

void MacroCmd::Read (istream& in) {
    Command::Read(in);
    
    int count;
    in >> count;

    for (int i = 0; i < count; ++i) {
        Append(unidraw->GetCatalog()->ReadCommand(in));
    }
}

void MacroCmd::Write (ostream& out) {
    Command::Write(out);
    Iterator i;
    int count = 0;

    for (First(i); !Done(i); Next(i), ++count);
    out << count << "\n";

    for (First(i); !Done(i); Next(i)) {
        Command* cmd = GetCommand(i);
        unidraw->GetCatalog()->WriteCommand(cmd, out);
        out << "\n";
    }
}
