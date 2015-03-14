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
 * Implementation of Component class.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/creator.h>
#include <Unidraw/iterator.h>
#include <Unidraw/ulist.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/Components/component.h>
#include <Unidraw/Components/compview.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

ClassId Component::GetClassId () { return COMPONENT; }
ClassId Component::GetSubstId (const char*&) { return UNDEFINED_CLASS; }
boolean Component::IsA (ClassId id) { return COMPONENT == id; }
Component::Component () { _views = new UList; }
Component* Component::Copy () { return nil; }
void Component::Read (istream&) { }
void Component::Write (ostream&) { }
void Component::Update () { }
StateVar* Component::GetState (const char*) { return nil; }
TransferFunct* Component::GetTransferFunct() { return nil; }

Component::~Component () {
    UList* cur = _views->First();

    while (cur != _views->End()) {
        Detach(View(cur));
	cur = _views->First();
    }
    delete _views;

    unidraw->GetCatalog()->Forget(this);
    unidraw->ClearHistory(this);
}    

void Component::Attach (ComponentView* view) {
    _views->Prepend(new UList(view));
    view->SetSubject(this);
}

void Component::Detach (ComponentView* view) {
    _views->Delete(view);
    view->SetSubject(nil);
}

ComponentView* Component::Create (ClassId viewId) {
    ClassId gv = Combine(GetClassId(), viewId);

    return (ComponentView*) unidraw->GetCatalog()->GetCreator()->Create(gv);
}

ComponentView* Component::View (UList* r) {
    return (ComponentView*) (*r)();
}

void Component::Notify () {
    for (UList* u = _views->First(); u != _views->End(); u = u->Next()) {
	View(u)->Update();
    }
}

void Component::Interpret (Command*) { }
void Component::Uninterpret (Command*) { }
Component* Component::GetParent () { return nil; }
void Component::SetParent (Component*, Component*) { }
void Component::First (Iterator&) { }
void Component::Last (Iterator&) { }
void Component::Next (Iterator&) { }
void Component::Prev (Iterator&) { }
boolean Component::Done (Iterator) { return true; }

Component* Component::GetRoot () {
    Component* cur, *parent = this;

    do {
        cur = parent;
        parent = cur->GetParent();
    } while (parent != nil);

    return cur;
}
