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
 * Implementation of StateVar base class.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/statevar.h>
#include <Unidraw/stateview.h>
#include <Unidraw/ulist.h>
#include <Unidraw/unidraw.h>

/*****************************************************************************/

inline StateVarView* view (UList* u) { return (StateVarView*) (*u)(); }

StateVar::StateVar () {
    _views = new UList;
    _conn = nil;
}

StateVar::~StateVar () {
    while (!_views->IsEmpty()) {
        Detach(view(_views->First()));
    }
    delete _views;
}

void StateVar::Attach (StateVarView* view) {
    _views->Append(new UList(view));
    view->SetSubject(this);
}

void StateVar::Detach (StateVarView* view) {
    _views->Delete(view);
    view->SetSubject(nil);
}

void StateVar::Notify () {
    for (UList* v = _views->First(); v != _views->End(); v = v->Next()) {
        view(v)->Update();
    }
}

StateVar& StateVar::operator = (StateVar&) { return *this; }
StateVar* StateVar::Copy () { return nil; }
ClassId StateVar::GetClassId () { return STATE_VAR; }
ClassId StateVar::GetSubstId (const char*&) { return UNDEFINED_CLASS; }
boolean StateVar::IsA (ClassId id) { return STATE_VAR == id; }
void StateVar::SetBinding (Connector* conn) { _conn = conn; Notify(); }
Connector* StateVar::GetBinding () { return _conn; }

void StateVar::Read (istream& in) {
    _conn = (Connector*) unidraw->GetCatalog()->ReadComponent(in);
}

void StateVar::Write (ostream& out) {
    unidraw->GetCatalog()->WriteComponent((Component*) _conn, out);
}
