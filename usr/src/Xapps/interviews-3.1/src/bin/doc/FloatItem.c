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
 * FloatItem
 */

#include "FloatItem.h"

#include "FloatView.h"
#include "Document.h"
#include "DocViewer.h"
#include "TextItem.h"
#include <OS/list.h>

#include <string.h>

class FloatViewInfo {
public:
    FloatView* _view;
};

declareList(FloatViewInfo_List,FloatViewInfo)
implementList(FloatViewInfo_List,FloatViewInfo)

FloatItem::FloatItem (
    Document* document, Item* parent, long style, long source
) : Item (document, parent, style, source) {
    _context = nil;
    _item = nil;
    _view = nil;
}

FloatItem::~FloatItem () {
    if (_document != nil) {
        _document->remove_float(_item);
    }
    if (_context != nil) {
        delete _context;
    }
    if (_item != nil) {
        _item->unref();
    }
    if (_view != nil) {
        while (_view->count() > 0) {
            FloatViewInfo& view = _view->item_ref(0);
            _view->remove(0);
        }
        delete _view;
    }
}

void FloatItem::style (long style) {
    Item::style(style);
    if (_item != nil) {
        _item->style(style);
    }
}

long FloatItem::style () {
    return Item::style();
}

void FloatItem::item (TextItem* item) {
    if (item != nil) {
        item->ref();
    }
    if (item != _item && _document != nil) {
        if (_item != nil) {
            _document->remove_float(_item);
        }
        if (item != nil) {
            _document->insert_float(item);
        }
    }
    if (_item != nil) {
        _item->unref();
    }
    _item = item;
}

TextItem* FloatItem::item () {
    return _item;
}

void FloatItem::context (const char* context) {
    delete _context;
    _context = strcpy(new char[strlen(context) + 1], context);
}

const char* FloatItem::context () {
    return _context;
}

void FloatItem::read (istream& in) {
    if (_document != nil) {
        _document->read(in, this, _style, _source);
    }
}

void FloatItem::write (ostream& out) {
    if (_document != nil) {
        _document->write(out, this, _style, _source);
    }
}

void FloatItem::label (const char*) {
    if (_item != nil) {
        _item->label(_context);
    }
}

void FloatItem::change (Item* item) {
    if (item == _item && _document != nil) {
        _document->change_float(item);
    }
}

Glyph* FloatItem::view (ItemView* parent, DocumentViewer* viewer) {
    return viewer->view(parent, this);
}

void FloatItem::attach (FloatView* view) {
    if (_view == nil) {
        _view = new FloatViewInfo_List();
    }
    FloatViewInfo info;
    info._view = view;
    _view->append(info);
}

void FloatItem::detach (FloatView* view) {
    if (_view != nil) {
        long count = _view->count();
        for (long i = 0; i < count; ++i) {
            FloatViewInfo& info = _view->item_ref(i);
            if (info._view == view) {
                _view->remove(i);
                break;
            }
        }
    }
}

void FloatItem::notify () {
    if (_view != nil) {
        long count = _view->count();
        for (long i = 0; i < count; ++i) {
            FloatViewInfo& info = _view->item_ref(i);
            info._view->update();
        }
    }
    if (_document != nil) {
        _document->notify();
    }
}
