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
 * Implementation of editing commands.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/dialogs.h>
#include <Unidraw/editor.h>
#include <Unidraw/iterator.h>
#include <Unidraw/selection.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/datas.h>
#include <Unidraw/Commands/edit.h>

#include <Unidraw/Commands/transforms.h>
#include <Unidraw/Components/connector.h>

#include <Unidraw/Graphic/graphic.h>

#include <IV-2_6/_enter.h>

#include <stream.h>

/*****************************************************************************/

ClassId UndoCmd::GetClassId () { return UNDO_CMD; }
boolean UndoCmd::IsA (ClassId id) { return UNDO_CMD == id || Command::IsA(id);}

UndoCmd::UndoCmd (ControlInfo* c) : Command(c) { }
UndoCmd::UndoCmd (Editor* ed) : Command(ed) { }

Command* UndoCmd::Copy () {
    Command* copy = new UndoCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void UndoCmd::Execute () { unidraw->Undo(GetEditor()->GetComponent()); }
boolean UndoCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId RedoCmd::GetClassId () { return REDO_CMD; }
boolean RedoCmd::IsA (ClassId id) { return REDO_CMD == id || Command::IsA(id);}

RedoCmd::RedoCmd (ControlInfo* c) : Command(c) { }
RedoCmd::RedoCmd (Editor* ed) : Command(ed) { }

Command* RedoCmd::Copy () {
    Command* copy = new RedoCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void RedoCmd::Execute () { unidraw->Redo(GetEditor()->GetComponent()); }
boolean RedoCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId CutCmd::GetClassId () { return CUT_CMD; }
boolean CutCmd::IsA (ClassId id) { return CUT_CMD == id || Command::IsA(id); }

CutCmd::CutCmd (ControlInfo* c, Clipboard* cb) : Command(c, cb) {
    _executed = false;
}

CutCmd::CutCmd (Editor* ed, Clipboard* cb) : Command(ed, cb) {
    _executed = false;
}

CutCmd::~CutCmd () {
    if (_clipboard != nil && _executed) {
        _clipboard->DeleteComps();
    }
}

Command* CutCmd::Copy () {
    Command* copy = new CutCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void CutCmd::Execute () {
    GetEditor()->GetComponent()->Interpret(this);
    _executed = true;
}

void CutCmd::Unexecute () {
    GetEditor()->GetComponent()->Uninterpret(this);
    _executed = false;
}

/*****************************************************************************/

ClassId CopyCmd::GetClassId () { return COPY_CMD; }
boolean CopyCmd::IsA (ClassId id) { return COPY_CMD == id || Command::IsA(id);}

CopyCmd::CopyCmd (ControlInfo* c, Clipboard* cb) : Command(c, cb) { }
CopyCmd::CopyCmd (Editor* ed, Clipboard* cb) : Command(ed, cb) { }

CopyCmd::~CopyCmd () {
    if (_clipboard != nil) {
        _clipboard->DeleteComps();
    }
}

Command* CopyCmd::Copy () {
    Command* copy = new CopyCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void CopyCmd::Execute () {
    Editor* editor = GetEditor();
    Selection* s = editor->GetSelection();

    if (!s->IsEmpty()) {
        Clipboard* cb = GetClipboard();
        cb = (cb == nil) ? unidraw->GetCatalog()->GetClipboard() : cb; 

        GraphicView* views = editor->GetViewer()->GetGraphicView();
        s->Sort(views);

        cb->DeleteComps();
        cb->CopyInit(s);
    }
}

boolean CopyCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId PasteCmd::GetClassId () { return PASTE_CMD; }
boolean PasteCmd::IsA (ClassId id) { return PASTE_CMD==id || Command::IsA(id);}

PasteCmd::PasteCmd (ControlInfo* c, Clipboard* cb) : Command(c, cb) {
    _executed = false;
}

PasteCmd::PasteCmd (Editor* ed, Clipboard* cb) : Command(ed, cb) {
    _executed = false;
}

PasteCmd::~PasteCmd () {
    if (_clipboard != nil && !_executed) {
        _clipboard->DeleteComps();
    }
}

Command* PasteCmd::Copy () {
    Command* copy = new PasteCmd(CopyControlInfo(), DeepCopyClipboard());
    InitCopy(copy);
    return copy;
}

void PasteCmd::Execute () {
    GetEditor()->GetComponent()->Interpret(this);
    _executed = true;
}

void PasteCmd::Unexecute () {
    GetEditor()->GetComponent()->Uninterpret(this);
    _executed = false;
}

boolean PasteCmd::Reversible () {
    Clipboard* cb = GetClipboard();
    Clipboard* globalcb = unidraw->GetCatalog()->GetClipboard();
   
    return (cb != nil && !cb->IsEmpty()) || !globalcb->IsEmpty();
}

/*****************************************************************************/

ClassId ReplaceCmd::GetClassId () { return REPLACE_CMD; }

boolean ReplaceCmd::IsA (ClassId id) {
    return REPLACE_CMD == id || MacroCmd::IsA(id);
}

ReplaceCmd::ReplaceCmd (ControlInfo* c, GraphicComp* gc) : MacroCmd(c) {
    Init(gc);
}

ReplaceCmd::ReplaceCmd (Editor* ed, GraphicComp* gc) : MacroCmd(ed) {
    Init(gc);
}

void ReplaceCmd::Init (GraphicComp* gc) {
    if (gc != nil) {
        Append(
            new DeleteCmd(GetEditor()),
            new PasteCmd(GetEditor(), new Clipboard(gc))
        );
    }
}

Command* ReplaceCmd::Copy () {
    Command* copy = new ReplaceCmd(CopyControlInfo(), GetReplacement());
    InitCopy(copy);
    return copy;
}

GraphicComp* ReplaceCmd::GetReplacement () {
    GraphicComp* repl = nil;
    Iterator i;
    Last(i);
    
    if (!Done(i)) {
        Clipboard* cb = GetCommand(i)->GetClipboard();
        cb->First(i);
        repl = cb->GetComp(i);
    }
    return repl;
}

/*****************************************************************************/

ClassId DupCmd::GetClassId () { return DUP_CMD; }

boolean DupCmd::IsA (ClassId id) {
    return DUP_CMD == id || Command::IsA(id);
}

DupCmd::DupCmd (ControlInfo* c, Clipboard* cb) : Command(c, cb) {
    _executed = false;
}

DupCmd::DupCmd (Editor* ed, Clipboard* cb) : Command(ed, cb) {
    _executed = false;
}

DupCmd::~DupCmd () {
    if (_clipboard != nil && !_executed) {
        _clipboard->DeleteComps();
    }
}

Command* DupCmd::Copy () {
    Command* copy = new DupCmd(CopyControlInfo(), GetClipboard());
    InitCopy(copy);
    return copy;
}

void DupCmd::Execute () {
    GetEditor()->GetComponent()->Interpret(this);
    _executed = true;
}

void DupCmd::Unexecute () {
    GetEditor()->GetComponent()->Uninterpret(this);
    _executed = false;
}

/*****************************************************************************/

ClassId DeleteCmd::GetClassId () { return DELETE_CMD; }

boolean DeleteCmd::IsA (ClassId id) {
    return DELETE_CMD == id || Command::IsA(id);
}

DeleteCmd::DeleteCmd (ControlInfo* c, Clipboard* cb) : Command(c,cb) {
    _executed = false;
}

DeleteCmd::DeleteCmd (Editor* ed, Clipboard* cb) : Command(ed, cb) {
    _executed = false;
}

DeleteCmd::~DeleteCmd () { 
    if (_clipboard != nil && _executed) {
        _clipboard->DeleteComps();
    }
}

Command* DeleteCmd::Copy () {
    Command* copy = new DeleteCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void DeleteCmd::Execute () {
    GetEditor()->GetComponent()->Interpret(this);
    _executed = true;
}

void DeleteCmd::Unexecute () {
    GetEditor()->GetComponent()->Uninterpret(this);
    _executed = false;
}

/*****************************************************************************/

ClassId SlctAllCmd::GetClassId () { return SLCTALL_CMD; }

boolean SlctAllCmd::IsA (ClassId id) {
    return SLCTALL_CMD == id || Command::IsA(id);
}

SlctAllCmd::SlctAllCmd (ControlInfo* c) : Command(c) { }
SlctAllCmd::SlctAllCmd (Editor* ed) : Command(ed) { }

Command* SlctAllCmd::Copy () {
    Command* copy = new SlctAllCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void SlctAllCmd::Execute () {
    Editor* editor = GetEditor();
    Selection* newSel = new Selection;
    Selection* s;
    Viewer* viewer;

    s = editor->GetSelection();
    delete s;
    for (int i = 0; (viewer = editor->GetViewer(i)) != nil; ++i) {
        s = viewer->GetGraphicView()->SelectAll();
        newSel->Merge(s);
        delete s;
    }
    editor->SetSelection(newSel);
    newSel->Update();
}

boolean SlctAllCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId ConnectCmd::GetClassId () { return CONNECT_CMD; }

boolean ConnectCmd::IsA (ClassId id) {
    return CONNECT_CMD == id || Command::IsA(id);
}

ConnectCmd::ConnectCmd (
    ControlInfo* c, Connector* s, Connector* t
) : Command(c) {
    _source = s;
    _target = t;
}

ConnectCmd::ConnectCmd (Editor* ed, Connector* s, Connector* t) : Command(ed) {
    _source = s;
    _target = t;
}

Command* ConnectCmd::Copy () {
    Connector* s, *t;

    GetConnectors(s, t);
    Command* copy = new ConnectCmd(CopyControlInfo(), s, t);
    InitCopy(copy);
    return copy;
}

void ConnectCmd::Execute () {
    Mobility s_m = _source->GetMobility();
    Graphic* s_gs = _source->GetGraphic();
    Graphic* t_gs = _target->GetGraphic();

    Store(_source, new MobilityData(s_m, s_gs));
    Store(_target, new GSData(t_gs));

    _source->SetMobility(Floating);
    _source->Connect(_target);

    unidraw->Update();
}

void ConnectCmd::Unexecute () {
    MobilityData* md = (MobilityData*) Recall(_source);
    GSData* gsd = (GSData*) Recall(_target);

    _source->SetMobility(md->_mobility);
    _source->Disconnect(_target);
    *_source->GetGraphic() = *md->_gs;
    _source->Notify();    

    *_target->GetGraphic() = *gsd->_gs;
    _target->Notify();

    unidraw->Update();
}

boolean ConnectCmd::Reversible () { return true; }

void ConnectCmd::GetConnectors (Connector*& s, Connector*& t) {
    s = _source;
    t = _target;
}

void ConnectCmd::Read (istream& in) {
    Command::Read(in);

    Catalog* catalog = unidraw->GetCatalog();
    _source = (Connector*) catalog->ReadComponent(in);
    _target = (Connector*) catalog->ReadComponent(in);
}

void ConnectCmd::Write (ostream& out) {
    Command::Write(out);

    Catalog* catalog = unidraw->GetCatalog();
    catalog->WriteComponent(_source, out);
    catalog->WriteComponent(_target, out);
}

/*****************************************************************************/

ClassId MobilityCmd::GetClassId () { return MOBILITY_CMD; }

boolean MobilityCmd::IsA (ClassId id) {
    return MOBILITY_CMD == id || Command::IsA(id);
}

MobilityCmd::MobilityCmd (ControlInfo* c, Mobility m) : Command(c) {
    _mobility = m;
}

MobilityCmd::MobilityCmd (Editor* ed, Mobility m) : Command(ed) {
    _mobility = m;
}

Command* MobilityCmd::Copy () {
    Command* copy = new MobilityCmd(CopyControlInfo(), GetMobility());
    InitCopy(copy);
    return copy;
}

Mobility MobilityCmd::GetMobility () { return _mobility; }

void MobilityCmd::Read (istream& in) {
    Command::Read(in);
    int mobility;
    
    in >> mobility;
    _mobility = Mobility(mobility);
}

void MobilityCmd::Write (ostream& out) {
    Command::Write(out);
    int mobility = _mobility;
    out << mobility << " ";
}
