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
 * ExternView implementation.
 */

#include <Unidraw/classes.h>
#include <Unidraw/iterator.h>
#include <Unidraw/Components/externview.h>

/*****************************************************************************/

ClassId ExternView::GetClassId () { return EXTERN_VIEW; }

boolean ExternView::IsA (ClassId id) {
    return EXTERN_VIEW == id || ComponentView::IsA(id);
}

ExternView::ExternView (Component* subj) : ComponentView(subj) { }
boolean ExternView::Emit (ostream& out) { return Definition(out); }
boolean ExternView::Definition (ostream&) { return true; }
ExternView* ExternView::GetView (Iterator) { return nil; }
void ExternView::SetView (ExternView*, Iterator&) { }

/*****************************************************************************/

ClassId PreorderView::GetClassId () { return PREORDER_VIEW; }

boolean PreorderView::IsA (ClassId id) {
    return PREORDER_VIEW == id || ExternView::IsA(id);
}

PreorderView::PreorderView (Component* subj) : ExternView(subj) { }

boolean PreorderView::Definition (ostream& out) {
    Iterator i;
    boolean ok = true;

    for (First(i); ok && !Done(i); Next(i)) {
        ok = GetView(i)->Definition(out);
    }
    return ok;
}

/*****************************************************************************/

ClassId InorderView::GetClassId () { return INORDER_VIEW; }

boolean InorderView::IsA (ClassId id) {
    return INORDER_VIEW == id || ExternView::IsA(id);
}

InorderView::InorderView (Component* subj) : ExternView(subj) { }

boolean InorderView::Definition (ostream&) {
    boolean ok = true;

    // unimplemented

    return ok;
}

/*****************************************************************************/

ClassId PostorderView::GetClassId () { return POSTORDER_VIEW; }

boolean PostorderView::IsA (ClassId id) {
    return POSTORDER_VIEW == id || ExternView::IsA(id);
}

PostorderView::PostorderView (Component* subj) : ExternView(subj) { }

boolean PostorderView::Definition (ostream& out) {
    Iterator i;
    boolean ok = true;

    for (Last(i); ok && !Done(i); Prev(i)) {
        ok = GetView(i)->Definition(out);
    }
    return ok;
}
