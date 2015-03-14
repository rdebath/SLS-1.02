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
 * Implementation of Unidraw class.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/editor.h>
#include <Unidraw/globals.h>
#include <Unidraw/iterator.h>
#include <Unidraw/statevars.h>
#include <Unidraw/ulist.h>
#include <Unidraw/umap.h>
#include <Unidraw/unidraw.h>

#include <Unidraw/Components/connector.h>
#include <Unidraw/Components/csolver.h>

#include <Unidraw/Commands/macro.h>

#include <InterViews/event.h>
#include <InterViews/window.h>
#include <IV-2_6/InterViews/world.h>

#include <IV-2_6/_enter.h>

#include <stdlib.h>

/*****************************************************************************/

static const int DEFAULT_HISTLEN = 20;

/*****************************************************************************/

class History : public UMapElem {
public:
    History(Component*);
    virtual ~History();

    virtual void* id();
    virtual void* tag();
public:
    Component* _comp;
    UList* _past;
    UList* _future;
};

History::History (Component* comp) {
    _comp = comp;
    _past = new UList;
    _future = new UList;
}

History::~History () {
    delete _past;
    delete _future;
}

void* History::id () { return _comp; }
void* History::tag () { return this; }

/*****************************************************************************/

class HistoryMap : public UMap {
public:
    HistoryMap();

    void Register(Component*);
    void Unregister(Component*);
    boolean Registered(Component*);

    History* GetHistory(Component*);
    History* GetHistory(int index);
    Component* GetComponent(History*);
    Component* GetComponent(int index);
};

HistoryMap::HistoryMap () { }

void HistoryMap::Register (Component* comp) {
    UMap::Register(new History(comp));
}

void HistoryMap::Unregister (Component* comp) {
    UMapElem* elem = FindId((void*) comp);

    if (elem != nil) {
        UMap::Unregister(elem);
        delete elem;
    }
}

boolean HistoryMap::Registered (Component* comp) {
    return FindId((void*) comp) != nil;
}

History* HistoryMap::GetHistory (Component* comp) {
    UMapElem* elem = FindId((void*) comp);
    return (elem == nil) ? nil : (History*) elem->tag();
}

History* HistoryMap::GetHistory (int index) { 
    return (History*) Elem(index)->tag();
}

Component* HistoryMap::GetComponent (History* hist) {
    UMapElem* elem = FindTag((void*) hist);
    return (elem == nil) ? nil : (Component*) elem->id();
}

Component* HistoryMap::GetComponent (int index) {
    return (Component*) Elem(index)->id();
}


/*****************************************************************************/

class DirtyCmd : public Command {
public:
    DirtyCmd(ControlInfo*);
    DirtyCmd(Editor* = nil);

    virtual void Execute();
    virtual void Unexecute();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

ClassId DirtyCmd::GetClassId () { return DIRTY_CMD; }
boolean DirtyCmd::IsA (ClassId id) {return DIRTY_CMD==id || Command::IsA(id);}

DirtyCmd::DirtyCmd (ControlInfo* c) : Command(c) { }
DirtyCmd::DirtyCmd (Editor* ed) : Command(ed) { }

Command* DirtyCmd::Copy () {
    Command* copy = new DirtyCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void DirtyCmd::Execute () { 
    ModifStatusVar* mv = (ModifStatusVar*) _editor->GetState("ModifStatusVar");

    if (mv != nil) {
	mv->SetModifStatus(true);
    }
}

void DirtyCmd::Unexecute () {
    ModifStatusVar* mv = (ModifStatusVar*) _editor->GetState("ModifStatusVar");

    if (mv != nil) {
	mv->SetModifStatus(false);
    }
}

/*****************************************************************************/

Unidraw::Unidraw (
    Catalog* c, int& argc, char** argv, OptionDesc* od, PropertyData* pd
) {
    Init(c, new World(c->GetName(), argc, argv, od, pd));
}

Unidraw::Unidraw (Catalog* c, World* w) {
    Init(c, w);
}

void Unidraw::Init (Catalog* c, World* w) {
    csolver = new CSolver;
    unidraw = this;

    _catalog = c;
    _world = w;
    _catalog->Init(_world);

    _editors = new UList;
    _deadEditors = new UList;

    alive(true);
    updated(false);

    _histories = new HistoryMap;

    InitAttributes();
}

Unidraw::~Unidraw () {
    CloseAll();
    ClearHistory();
    delete _histories;
    delete _editors;
    delete _deadEditors;
    delete _catalog;
    delete _world;
}

void Unidraw::InitAttributes () {
    const char* attrib = GetWorld()->GetAttribute("history");
    _histlen = (attrib == nil) ? DEFAULT_HISTLEN : atoi(attrib);
}

void Unidraw::Mark (Editor* editor) {
    UList* e = _editors->Find(editor);
    _editors->Remove(e);
    _deadEditors->Append(e);
}

void Unidraw::Sweep () {
    while (!_deadEditors->IsEmpty()) {
        UList* doomed = _deadEditors->First();
        _deadEditors->Remove(doomed);

	Editor* ed = editor(doomed);
	Component* comp = ed->GetComponent();
        Resource::unref(editor(doomed));
        delete doomed;

	DeleteComponent(comp);
    }
}

void Unidraw::DeleteComponent (Component* comp) {
    Component* root = (comp == nil) ? nil : comp->GetRoot();

    if (
        root != nil &&
        GetCatalog()->GetName(root) == nil &&
        FindAny(root) == nil && FindAnyDead(root) == nil
    ) {
        delete root;
    }
}

void Unidraw::Run () {
    Session* session = GetWorld()->session();
    Event e;
    alive(true);

    while (alive() && !session->done()) {
	updated(false);

	session->read(e);
	e.handle();

	Process();
	Sweep();

	if (updated()) {
	    Update(true);
	}
    }
}

void Unidraw::DoUpdate () {
    csolver->Solve();

    for (UList* e = _editors->First(); e != _editors->End(); e = e->Next()) {
        editor(e)->Update();
    }
}

void Unidraw::Update (boolean immediate) {
    if (immediate) {
        DoUpdate();
    }
    updated(!immediate);
}

void Unidraw::Quit () { alive(false); }

void Unidraw::Open (Editor* editor) {
    ManagedWindow* w = editor->GetWindow();
    if (w == nil) {
	w = new ApplicationWindow(editor);
	editor->SetWindow(w);
    }
    w->map();
    _editors->Append(new UList(editor));
    Resource::ref(editor);
    editor->Open();
}

void Unidraw::Close (Editor* editor) {
    editor->Close();
    Mark(editor);
    editor->GetWindow()->unmap();
}

void Unidraw::CloseDependents (Component* comp) {
    Iterator i;

    for (First(i); !Done(i);) {
        Editor* ed = GetEditor(i);
        Next(i);

        if (ed->DependsOn(comp)) {
            Close(ed);
            ed->SetComponent(nil);
        }
    }
}

void Unidraw::CloseAll () {
    UList* u = _editors->First();

    while (u != _editors->End()) {
        Close(editor(u));
        u = _editors->First();
    }
}

boolean Unidraw::Opened (Editor* ed) {
    for (UList* u = _editors->First(); u != _editors->End(); u = u->Next()) {
        if (editor(u) == ed) {
            return true;
        }
    }
    return false;
}

Editor* Unidraw::editor (UList* u) { return (Editor*) (*u)(); }
Command* Unidraw::command (UList* r) { return (Command*) (*r)(); }
UList* Unidraw::elem (Iterator i) { return (UList*) i.GetValue(); }
void Unidraw::First (Iterator& i) { i.SetValue(_editors->First()); }
void Unidraw::Next (Iterator& i) { i.SetValue(elem(i)->Next()); }
boolean Unidraw::Done (Iterator i) { return elem(i) == _editors->End(); }
Editor* Unidraw::GetEditor (Iterator i) { return editor(elem(i)); }

Editor* Unidraw::Find (Component* comp) {
    for (UList* u = _editors->First(); u != _editors->End(); u = u->Next()) {
        Editor* ed = editor(u);

        if (ed->GetComponent() == comp) {
            return ed;
        }
    }
    return nil;
}

Editor* Unidraw::FindAny (Component* comp) { return FindAny(comp, _editors); }

Editor* Unidraw::FindAnyDead (Component* comp) {
    return FindAny(comp, _deadEditors);
}

Editor* Unidraw::FindAny (Component* comp, UList* editors) {
    comp = comp->GetRoot();

    for (UList* u = editors->First(); u != editors->End(); u = u->Next()) {
        Editor* ed = editor(u);
        Component* test = ed->GetComponent();

        if (test != nil && test->GetRoot() == comp) {
            return ed;
        }
    }
    return nil;
}

void Unidraw::SetHistoryLength (int hl) { _histlen = hl; }
int Unidraw::GetHistoryLength () { return _histlen; }

void Unidraw::ClearHistory (Editor* ed) {
    Component* comp = ed->GetComponent();

    if (comp != nil && FindAny(comp) == nil) {
        ClearHistory(comp);
    }
}

void Unidraw::ClearHistory (Component* comp) {
    if (comp == nil) {
        for (int i = 0; i < _histories->Count(); ++i) {
            History* history = _histories->GetHistory(i);

            if (history != nil) {
                ClearHistory(history->_past);
                ClearHistory(history->_future);
            }
        }

    } else {
        History* history = _histories->GetHistory(comp->GetRoot());

        if (history != nil) {
            ClearHistory(history->_past);
            ClearHistory(history->_future);
        }
    }
}

void Unidraw::ClearHistory (UList* hist, int start) {
    UList* doomed = (*hist)[start];
    UList* next;

    if (doomed != nil) {
        while (doomed != hist->End()) {
            next = doomed->Next();
            hist->Remove(doomed);

            Command* cmd = command(doomed);
            Editor* ed = cmd->GetEditor();

	    Resource::unref(ed);

            delete cmd;
            delete doomed;
            doomed = next;
        }
    }
}

void Unidraw::Log (Command* cmd) {
    if (cmd->Reversible()) {
        Editor* ed = cmd->GetEditor();
        Component* comp = ed->GetComponent()->GetRoot();

        UList* past, *future;
        GetHistory(comp, past, future);

        Resource::ref(ed);
        ClearHistory(future);

	if (IsClean(ed)) {
	    DirtyCmd* dc = new DirtyCmd(ed);
	    dc->Execute();
            cmd = new MacroCmd(ed, cmd, dc);
	}
	    
        past->Prepend(new UList(cmd));
        ClearHistory(past, _histlen+1);
    }
}

void Unidraw::Process () {
    /* do nothing by default */
}

boolean Unidraw::IsClean (Editor* ed) {
    ModifStatusVar* mv = (ModifStatusVar*) ed->GetState("ModifStatusVar");

    return (mv != nil && !mv->GetModifStatus());
}

void Unidraw::Undo (Component* comp, int n) {
    UList* past, *future;
    GetHistory(comp, past, future);
    UList* cur = past->First();

    for (int i = 0; i < n && cur != past->End(); ++i) {
        Command* cmd = command(cur);
        cmd->Unexecute();
        past->Remove(cur);
        future->Prepend(cur);
        cur = past->First();
    }
}

void Unidraw::Redo (Component* comp, int n) {
    UList* past, *future;
    GetHistory(comp, past, future);
    UList* cur = future->First();

    for (int i = 0; i < n && cur != future->End(); ++i) {
        Command* cmd = command(cur);
        cmd->Execute();
        future->Remove(cur);
        past->Prepend(cur);
        cur = future->First();
    }
}

void Unidraw::GetHistory (Component* comp, UList*& past, UList*& future) {
    Component* root = comp->GetRoot();
    if (!_histories->Registered(comp)) {
        _histories->Register(comp);
    }

    History* history = _histories->GetHistory(root);
    past = history->_past;
    future = history->_future;
}
