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
 * RefItem
 */

#include "RefItem.h"

#include "RefView.h"
#include "Document.h"
#include "DocViewer.h"

#include <OS/list.h>
#include <string.h>

class RefViewInfo {
public:
    RefView* _view;
};

declareList(RefViewInfo_List,RefViewInfo)
implementList(RefViewInfo_List,RefViewInfo)

RefItem::RefItem (
    Document* document, Item* parent, long style, long source
) : Item (document, parent, style, source) {
    _label = nil;
    _text = nil;
    _view = nil;
    if (_document != nil) {
        _document->relabel();
    }
}

RefItem::~RefItem () {
    if (_label != nil) {
        delete _label;
    }
    if (_text != nil) {
        delete _text;
    }
    if (_view != nil) {
        while (_view->count() > 0) {
            RefViewInfo& view = _view->item_ref(0);
            _view->remove(0);
        }
        delete _view;
    }
}

void RefItem::name (const char* label) {
    delete _label;
    _label = strcpy(new char[strlen(label) + 1], label);
}

const char* RefItem::name () {
    return _label;
}

void RefItem::read (istream& in) {
    if (_document != nil) {
        _document->read(in, this, _style, _source);
    }
}

void RefItem::write (ostream& out) {
    if (_document != nil) {
        _document->write(out, this, _style, _source);
    }
}

void RefItem::label (const char*) {
    if (_document != nil) {
        const char* text = _document->label(_label);
        if (_text == nil || strcmp(_text, text) != 0) {
            if (_text != nil) {
                delete _text;
            }
            _text = strcpy(new char[strlen(text) + 1], text);
            if (_parent != nil) {
                _parent->change(this);
            }
            _document->relabel();
            if (_view != nil) {
                long count = _view->count();
                for (long i = 0; i < count; ++i) {
                    RefViewInfo& info = _view->item_ref(i);
                    info._view->update();
                }
            }
        }
    }
}

Glyph* RefItem::view (ItemView* parent, DocumentViewer* viewer) {
    return viewer->view(parent, this);
}

void RefItem::attach (RefView* view) {
    if (_view == nil) {
        _view = new RefViewInfo_List();
    }
    RefViewInfo info;
    info._view = view;
    _view->append(info);
}

void RefItem::detach (RefView* view) {
    if (_view != nil) {
        long count = _view->count();
        for (long i = 0; i < count; ++i) {
            RefViewInfo& info = _view->item_ref(i);
            if (info._view == view) {
                _view->remove(i);
                break;
            }
        }
    }
}

void RefItem::notify () {
    if (_view != nil) {
        long count = _view->count();
        for (long i = 0; i < count; ++i) {
            RefViewInfo& info = _view->item_ref(i);
            info._view->update();
        }
    }
    Item::notify();
}

const char* RefItem::text () {
    return _text;
}
