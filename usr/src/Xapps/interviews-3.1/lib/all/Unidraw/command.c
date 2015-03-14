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
 * Command implemementation.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/ctrlinfo.h>
#include <Unidraw/editor.h>
#include <Unidraw/iterator.h>
#include <Unidraw/selection.h>
#include <Unidraw/uhash.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/grview.h>

#include <Unidraw/Commands/command.h>
#include <Unidraw/Commands/datas.h>

#include <stream.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

static const int DATACACHE_SIZE = 255;

/*****************************************************************************/

class DataElem : public UHashElem {
public:
    DataElem(Data*, Component*);
    DataElem(DataElem*);
    virtual ~DataElem();

    Data* data();
    Component* comp();

    void data(Data*);
    void comp(Component*);
private:
    Data* _data;
    Component* _comp;
};

DataElem::DataElem (Data* d, Component* c) {
    _data = d;
    _data->Reference();
    _comp = c;
}

DataElem::DataElem (DataElem* de) {
    _data = de->_data;
    _data->Reference();
    _comp = de->_comp;
}

DataElem::~DataElem () { Unref(_data); }

inline Data* DataElem::data () { return _data; }
inline Component* DataElem::comp () { return _comp; }

inline void DataElem::data (Data* d) {
    Unref(_data);
    _data = d;
    _data->Reference();
}

inline void DataElem::comp (Component* c) { _comp = c; }

/*****************************************************************************/

class DataCache : public UHashTable {
public:
    DataCache();

    DataElem* GetData(Iterator);
    DataElem* Find(Component*);
};

DataCache::DataCache () : UHashTable(DATACACHE_SIZE) { }

DataElem* DataCache::GetData (Iterator i) { return (DataElem*) GetElem(i); }

DataElem* DataCache::Find (Component* comp) {
    return (DataElem*) UHashTable::Find(comp);
}

/*****************************************************************************/

ClassId Command::GetClassId () { return COMMAND; }
ClassId Command::GetSubstId (const char*&) { return UNDEFINED_CLASS; }
boolean Command::IsA (ClassId id) { return COMMAND == id; }

Command::Command (ControlInfo* m, Clipboard* cb) {
    Command::SetControlInfo(m);
    _editor = nil;
    _clipboard = cb;
    _cache = nil;
}

Command::Command (Editor* ed, Clipboard* cb) {
    _ctrlInfo = nil;
    _editor = ed;
    _clipboard = cb;
    _cache = nil;
}

Command::~Command () { 
    if (_ctrlInfo != nil) {
        delete _ctrlInfo;
    }
    if (_clipboard != nil) {
        delete _clipboard;
    }
    delete _cache;
    unidraw->GetCatalog()->Forget(this);
}

DataCache* Command::CopyData () {
    DataCache* copy = new DataCache;

    if (_cache != nil) {
        Iterator i;
        
        for (_cache->First(i); !_cache->Done(i); _cache->Next(i)) {
            DataElem* de = _cache->GetData(i);
            copy->Register(de->comp(), new DataElem(de));
        }
    }
    return copy;
}

void Command::SetData (DataCache* dc) { _cache = dc; }

void Command::InitCopy (Command* cmd) {
    cmd->SetEditor(GetEditor());
    cmd->SetData(CopyData());
    cmd->SetClipboard(CopyClipboard());
}

Command* Command::Copy () {
    Command* copy = new Command(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void Command::Read (istream& in) {
    SetControlInfo(unidraw->GetCatalog()->ReadControlInfo(in));
}

void Command::Write (ostream& out) {
    unidraw->GetCatalog()->WriteControlInfo(GetControlInfo(), out);
}

void Command::SetControlInfo (ControlInfo* m) {
    _ctrlInfo = m;
    if (m != nil) m->SetOwner(this);
}

void Command::First (Iterator&) { }
void Command::Last (Iterator&) { }
void Command::Next (Iterator&) { }
void Command::Prev (Iterator&) { }
boolean Command::Done (Iterator&) { return true; }

void Command::SetEditor (Editor* ed) { _editor = ed; }
void Command::SetClipboard (Clipboard* cb) { _clipboard = cb; }
ControlInfo* Command::GetControlInfo () { return _ctrlInfo; }
Editor* Command::GetEditor () { return _editor; }
Clipboard* Command::GetClipboard () { return _clipboard; }

void Command::Store (Component* comp, Data* data) {
    if (_cache == nil) {
        _cache = new DataCache;
    }

    DataElem* existing = _cache->Find(comp);
    
    if (existing == nil) {
        _cache->Register(comp, new DataElem(data, comp));
    } else {
        existing->data(data);
    }
}

Data* Command::Recall (Component* comp) {
    Data* data = nil;

    if (_cache != nil) {
        DataElem* de = _cache->Find(comp);
        data = (de == nil) ? nil : de->data();
    }
    return data;
}

GraphicComp* Command::GetGraphicComp () {
    Component* comp = _editor->GetComponent();
    
    if (comp == nil) {
        Clipboard* cb = GetClipboard();
        Iterator i;

        if (cb != nil && !cb->IsEmpty()) {
            cb->First(i);
            comp = cb->GetComp(i)->GetParent();
        }
    }
    return (
        comp != nil && comp->IsA(GRAPHIC_COMP)
    ) ? (GraphicComp*) comp : nil;
}

void Command::Execute () {
    Selection* s = _editor->GetSelection();
    Clipboard* cb = GetClipboard();

    if (!s->IsEmpty() || (cb != nil && !cb->IsEmpty())) {
        Iterator i;

        if (cb == nil) {
            SetClipboard(cb = new Clipboard);
            cb->Init(s);
        }

        for (cb->First(i); !cb->Done(i); cb->Next(i)) {
            cb->GetComp(i)->Interpret(this);
        }
        unidraw->Update();
    }
}

void Command::Unexecute () {
    Clipboard* cb = GetClipboard();

    if (cb != nil) {
        Iterator i;

        for (cb->Last(i); !cb->Done(i); cb->Prev(i)) {
            cb->GetComp(i)->Uninterpret(this);
        }
        unidraw->Update();
    }
}

boolean Command::Reversible () {
    boolean reversible = true;
    Clipboard* cb = GetClipboard();

    if (cb == nil) {
        Editor* ed = GetEditor();
        Selection* s = (ed == nil) ? nil : ed->GetSelection();
        reversible = (s == nil) || !s->IsEmpty();

    } else {
        reversible = !cb->IsEmpty();
    }
    return reversible;
}

void Command::Log () { unidraw->Log(this); }

ControlInfo* Command::CopyControlInfo () {
    ControlInfo* ci = GetControlInfo();
    return (ci == nil) ? nil : ci->Copy();
}

Clipboard* Command::CopyClipboard () {
    Clipboard* cb = GetClipboard();
    return (cb == nil) ? nil : cb->Copy();
}

Clipboard* Command::DeepCopyClipboard () {
    Clipboard* cb = GetClipboard();
    return (cb == nil) ? nil : cb->DeepCopy();
}
