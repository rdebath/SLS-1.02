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
 * ComponentView implementation.
 */

#include <Unidraw/classes.h>
#include <Unidraw/iterator.h>
#include <Unidraw/Components/component.h>
#include <Unidraw/Components/compview.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

ClassId ComponentView::GetClassId () { return COMPONENT_VIEW; }
boolean ComponentView::IsA (ClassId id) { return COMPONENT_VIEW == id; }

ComponentView::ComponentView (Component* subj) {
    _subject = subj;
    if (subj != nil) {
	subj->Attach(this);
    }
}

ComponentView::~ComponentView () {
    Component* subj = GetSubject();

    if (subj != nil) {
        subj->Detach(this);
    }
}

void ComponentView::Update () { }
void ComponentView::Interpret (Command* c) { GetSubject()->Interpret(c); }
void ComponentView::Uninterpret (Command* c) { GetSubject()->Uninterpret(c); }

ComponentView* ComponentView::GetParent () { return nil; }
void ComponentView::SetSubject (Component* c) { _subject = c; }
void ComponentView::SetParent (ComponentView*, ComponentView*) { }
void ComponentView::First (Iterator&) { }
void ComponentView::Last (Iterator&) { }
void ComponentView::Next (Iterator&) { }
void ComponentView::Prev (Iterator&) { }
boolean ComponentView::Done (Iterator) { return true; }
Component* ComponentView::GetSubject () { return _subject; }
