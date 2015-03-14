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
 * Implementation of structuring commands.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/globals.h>
#include <Unidraw/iterator.h>
#include <Unidraw/selection.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/datas.h>
#include <Unidraw/Commands/struct.h>

#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/grview.h>

#include <Unidraw/Graphic/graphic.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

ClassId GroupCmd::GetClassId () { return GROUP_CMD; }

boolean GroupCmd::IsA (ClassId id) {
    return GROUP_CMD == id || Command::IsA(id);
}

GroupCmd::GroupCmd (ControlInfo* c, GraphicComp* d) : Command(c) { Init(d); }
GroupCmd::GroupCmd (Editor* ed, GraphicComp* d) : Command(ed) { Init(d); }

void GroupCmd::Init (GraphicComp* dest) {
    _group = dest;
    _executed = false;
}

GroupCmd::~GroupCmd () {
    if (!_executed) {
        delete _group;
    }
}

Command* GroupCmd::Copy () {
    GraphicComp* dest = (_group == nil) ? nil : (GraphicComp*) _group->Copy();
    Command* copy = new GroupCmd(CopyControlInfo(), dest);
    InitCopy(copy);
    return copy;
}

void GroupCmd::Execute () {
    Clipboard* cb = GetClipboard();

    if (cb == nil) {
        SetClipboard(cb = new Clipboard);
        Editor* ed = GetEditor();
        Selection* s = ed->GetSelection();

        if (s->Number() > 1) {
            Iterator i;
            GraphicView* views = ed->GetViewer()->GetGraphicView();
            s->Sort(views);

            for (s->First(i); !s->Done(i); s->Next(i)) {
                s->GetView(i)->Interpret(this);
            }
        }

    } else {
        Clipboard* oldcb = cb;
        SetClipboard(cb = new Clipboard);

        Iterator i;
        for (oldcb->First(i); !oldcb->Done(i); oldcb->Next(i)) {
            oldcb->GetComp(i)->Interpret(this);
        }
        delete oldcb;
    }

    if (!cb->IsEmpty()) {
        if (_group == nil) {
            SetGroup(new GraphicComps);
        }
        _group->Interpret(this);
        _executed = true;
    }
}

void GroupCmd::Unexecute () {
    _group->Uninterpret(this);
    _executed = false;

    Clipboard* cb = GetClipboard();
    Iterator i;

    for (cb->Last(i); !cb->Done(i); cb->Prev(i)) {
        cb->GetComp(i)->Uninterpret(this);
    }
}

boolean GroupCmd::Reversible () {
    Clipboard* cb = GetClipboard();
    return cb == nil || !cb->IsEmpty();
}

void GroupCmd::Read (istream& in) {
    Command::Read(in);
    _group = (GraphicComp*) unidraw->GetCatalog()->ReadComponent(in);
}

void GroupCmd::Write (ostream& out) {
    Command::Write(out);
    unidraw->GetCatalog()->WriteComponent(_group, out);
}

/*****************************************************************************/

ClassId UngroupCmd::GetClassId () { return UNGROUP_CMD; }

boolean UngroupCmd::IsA (ClassId id) {
    return UNGROUP_CMD == id || Command::IsA(id);
}

UngroupCmd::UngroupCmd (ControlInfo* c) : Command(c) { Init(); }
UngroupCmd::UngroupCmd (Editor* ed) : Command(ed) { Init(); }

void UngroupCmd::Init () {
    _kids = nil;
    _executed = false;
}

UngroupCmd::~UngroupCmd () {
    if (_executed) {
        GetClipboard()->DeleteComps();
        delete _kids;
    }
}

Command* UngroupCmd::Copy () {
    Command* copy = new UngroupCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void UngroupCmd::Execute () {
    Clipboard* cb = GetClipboard();
    Editor* ed = GetEditor();

    if (cb == nil) {
        Selection* s = ed->GetSelection();

        if (s->IsEmpty()) {
            return;
        }

        SetClipboard(cb = new Clipboard);
        GraphicView* views = ed->GetViewer()->GetGraphicView();
        s->Sort(views);
        Iterator i;

        for (s->First(i); !s->Done(i); s->Next(i)) {
            s->GetView(i)->Interpret(this);
        }

    } else {
        Clipboard* oldcb = cb;
        SetClipboard(cb = new Clipboard);

        Iterator i;
        for (oldcb->First(i); !oldcb->Done(i); oldcb->Next(i)) {
            oldcb->GetComp(i)->Interpret(this);
        }
        delete oldcb;
    }

    if (!cb->IsEmpty()) {
        ed->GetComponent()->Interpret(this);
        _executed = true;
    }
}

void UngroupCmd::Unexecute () {
    GetEditor()->GetComponent()->Uninterpret(this);
    _executed = false;

    Clipboard* cb = GetClipboard();
    Iterator i;

    for (cb->Last(i); !cb->Done(i); cb->Prev(i)) {
        cb->GetComp(i)->Uninterpret(this);
    }
}

/*****************************************************************************/

ClassId FrontCmd::GetClassId () { return FRONT_CMD; }
boolean FrontCmd::IsA (ClassId id) { return FRONT_CMD==id || Command::IsA(id);}

FrontCmd::FrontCmd (ControlInfo* c) : Command(c) { }
FrontCmd::FrontCmd (Editor* ed) : Command(ed) { }

Command* FrontCmd::Copy () {
    Command* copy = new FrontCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void FrontCmd::Execute () {
    Clipboard* cb = GetClipboard();
    Editor* ed = GetEditor();

    if (cb == nil) {
        Selection* s = ed->GetSelection();

        if (s->IsEmpty()) {
            return;
        }

        SetClipboard(cb = new Clipboard);
        GraphicView* views = ed->GetViewer()->GetGraphicView();
        s->Sort(views);
        Iterator i;

        for (s->First(i); !s->Done(i); s->Next(i)) {
            s->GetView(i)->Interpret(this);
        }

    } else {
        Clipboard* oldcb = cb;
        SetClipboard(cb = new Clipboard);

        Iterator i;
        for (oldcb->First(i); !oldcb->Done(i); oldcb->Next(i)) {
            oldcb->GetComp(i)->Interpret(this);
        }
        delete oldcb;
    }

    if (!cb->IsEmpty()) {
        ed->GetComponent()->Interpret(this);
    }
}

void FrontCmd::Unexecute () {
    GetEditor()->GetComponent()->Uninterpret(this);

    Clipboard* cb = GetClipboard();
    Iterator i;

    for (cb->Last(i); !cb->Done(i); cb->Prev(i)) {
        cb->GetComp(i)->Uninterpret(this);
    }
}

/*****************************************************************************/

ClassId BackCmd::GetClassId () { return BACK_CMD; }
boolean BackCmd::IsA (ClassId id) { return BACK_CMD == id || Command::IsA(id);}

BackCmd::BackCmd (ControlInfo* c) : Command(c) { }
BackCmd::BackCmd (Editor* ed) : Command(ed) { }

Command* BackCmd::Copy () {
    Command* copy = new BackCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void BackCmd::Execute () {
    Clipboard* cb = GetClipboard();
    Editor* ed = GetEditor();

    if (cb == nil) {
        Selection* s = ed->GetSelection();

        if (s->IsEmpty()) {
            return;
        }

        SetClipboard(cb = new Clipboard);
        GraphicView* views = ed->GetViewer()->GetGraphicView();
        s->Sort(views);
        Iterator i;

        for (s->First(i); !s->Done(i); s->Next(i)) {
            s->GetView(i)->Interpret(this);
        }

    } else {
        Clipboard* oldcb = cb;
        SetClipboard(cb = new Clipboard);

        Iterator i;
        for (oldcb->First(i); !oldcb->Done(i); oldcb->Next(i)) {
            oldcb->GetComp(i)->Interpret(this);
        }
        delete oldcb;
    }

    if (!cb->IsEmpty()) {
        ed->GetComponent()->Interpret(this);
    }
}

void BackCmd::Unexecute () {
    GetEditor()->GetComponent()->Uninterpret(this);

    Clipboard* cb = GetClipboard();
    Iterator i;

    for (cb->Last(i); !cb->Done(i); cb->Prev(i)) {
        cb->GetComp(i)->Uninterpret(this);
    }
}
