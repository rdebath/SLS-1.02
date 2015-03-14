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
 * CounterItem
 */

#include "CounterItem.h"

#include "CounterView.h"
#include "Document.h"
#include "DocViewer.h"

#include <OS/list.h>
#include <string.h>

class CounterViewInfo {
public:
    CounterView* _view;
};

declareList(CounterViewInfo_List,CounterViewInfo)
implementList(CounterViewInfo_List,CounterViewInfo)

CounterItem::CounterItem (
    Document* document, Item* parent, long style, long source
) : Item (document, parent, style, source) {
    _name = nil;
    _view = nil;
    if (_document != nil) {
        _document->relabel();
    }
}

CounterItem::~CounterItem () {
    if (_name != nil) {
        delete _name;
    }
    if (_document != nil) {
        _document->relabel();
    }
    if (_view != nil) {
        while (_view->count() > 0) {
            CounterViewInfo& view = _view->item_ref(0);
            _view->remove(0);
        }
        delete _view;
    }
}

void CounterItem::name (const char* name) {
    delete _name;
    _name = strcpy(new char[strlen(name) + 1], name);
}

const char* CounterItem::name () {
    return _name;
}

void CounterItem::read (istream& in) {
    if (_document != nil) {
        _document->read(in, this, _style, _source);
    }
}

void CounterItem::write (ostream& out) {
    if (_document != nil) {
        _document->write(out, this, _style, _source);
    }
}

void CounterItem::label (const char* context) {
    if (_document != nil) {
        _document->step(_name, context);
    }
}

Glyph* CounterItem::view (ItemView* parent, DocumentViewer* viewer) {
    return viewer->view(parent, this);
}

void CounterItem::attach (CounterView* view) {
    if (_view == nil) {
        _view = new CounterViewInfo_List();
    }
    CounterViewInfo info;
    info._view = view;
    _view->append(info);
}

void CounterItem::detach (CounterView* view) {
    if (_view != nil) {
        long count = _view->count();
        for (long i = 0; i < count; ++i) {
            CounterViewInfo& info = _view->item_ref(i);
            if (info._view == view) {
                _view->remove(i);
                break;
            }
        }
    }
}

void CounterItem::notify () {
    if (_view != nil) {
        long count = _view->count();
        for (long i = 0; i < count; ++i) {
            CounterViewInfo& info = _view->item_ref(i);
            info._view->update();
        }
    }
    Item::notify();
}
