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
 * Item
 */

#include "Item.h"
#include "Document.h"

Item::Item (Document* document, Item* parent, long style, long source) {
    _style = style;
    _source = source;
    _document = document;
    _parent = parent;
}

Item::~Item () { }

Document* Item::document () {
    return _document;
}

void Item::style (long style) {
    _style = style;
}

long Item::style () {
    return _style;
}

void Item::source (long source) {
    _source = source;
}

long Item::source () {
    return _source;
}

void Item::read (istream&) { }

void Item::write (ostream&) { }

void Item::label (const char*) { }

void Item::change (Item*) { }

Glyph* Item::view (ItemView*, DocumentViewer*) {
    return nil;
}

void Item::notify () {
    if (_parent != nil) {
        _parent->change(this);
        _parent->notify();
    } else if (_document != nil) {
        _document->notify();
    }
}
