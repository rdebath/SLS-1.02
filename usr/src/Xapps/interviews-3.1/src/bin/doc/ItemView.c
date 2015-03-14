/*
 * Copyright (c) 1991 Stanford University
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
 * ItemView
 */

#include "ItemView.h"

#include "Item.h"
#include "DocViewer.h"

#include "doc-listener.h"

ItemView::ItemView (
    DocumentViewer* viewer, ItemView* parent
) : MonoGlyph(nil), Handler() {
    _listener = new Listener(nil, this);
    _viewer = viewer;
    _parent = parent;
    body(_listener);
}

ItemView::~ItemView () { }

boolean ItemView::event (Event& e) {
    if (_viewer->focus() != this) {
        if (
            e.type() == Event::down && e.pointer_button() == Event::left
            && e.handler() == this
        ) {
            e.unread();
            _viewer->focus(this);
        }
    } else {
        if (e.type() == Event::key) {
            keystroke(e);
        } else if (e.type() == Event::down) {
            _listener->motion(true);
            if (e.pointer_button() == Event::left) {
                select(e);
            } else if (e.pointer_button() == Event::middle) {
                manipulate(e);
            } else if (e.pointer_button() == Event::right) {
                menu(e);
            }
            _listener->motion(false);
        }
    }
    return true;
}

void ItemView::allocate (
    Canvas* canvas, const Allocation& allocation, Extension& extension
) {
    MonoGlyph::allocate(canvas, allocation, extension);
    mark_selection();
}

void ItemView::keystroke (Event&) { }

void ItemView::select (Event&) { }

void ItemView::manipulate (Event&) { }

void ItemView::menu (Event&) { }

void ItemView::repair () { }

void ItemView::update () { }

void ItemView::activate (boolean) { }

void ItemView::mark_selection () { }

boolean ItemView::command (const char* command) {
    if (_parent != nil) {
        return _parent->command(command);
    } else {
        return _viewer->command(command);
    }
}
