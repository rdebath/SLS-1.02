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
 * LabelItem
 */

#include "LabelItem.h"

#include "LabelView.h"
#include "Document.h"
#include "DocViewer.h"

#include <OS/list.h>
#include <string.h>

class LabelViewInfo {
public:
    LabelView* _view;
};

declareList(LabelViewInfo_List,LabelViewInfo)
implementList(LabelViewInfo_List,LabelViewInfo)

LabelItem::LabelItem (
    Document* document, Item* parent, long style, long source
) : Item (document, parent, style, source) {
    _name = nil;
    _view = nil;
    if (_document != nil) {
        _document->relabel();
    }
}

LabelItem::~LabelItem () {
    if (_document != nil) {
        _document->label(_name, "?");
        _document->relabel();
    }
    if (_name != nil) {
        delete _name;
    }
    if (_view != nil) {
        while (_view->count() > 0) {
            LabelViewInfo& view = _view->item_ref(0);
            _view->remove(0);
        }
        delete _view;
    }
}

void LabelItem::name (const char* name) {
    delete _name;
    _name = strcpy(new char[strlen(name) + 1], name);
}

const char* LabelItem::name () {
    return _name;
}

void LabelItem::read (istream& in) {
    if (_document != nil) {
        _document->read(in, this, _style, _source);
    }
}

void LabelItem::write (ostream& out) {
    if (_document != nil) {
        _document->write(out, this, _style, _source);
    }
}

void LabelItem::label (const char* context) {
    if (_document != nil) {
        _document->label(_name, _document->label(context));
    }
}

Glyph* LabelItem::view (ItemView* parent, DocumentViewer* viewer) {
    return viewer->view(parent, this);
}

void LabelItem::attach (LabelView* view) {
    if (_view == nil) {
        _view = new LabelViewInfo_List();
    }
    LabelViewInfo info;
    info._view = view;
    _view->append(info);
}

void LabelItem::detach (LabelView* view) {
    if (_view != nil) {
        long count = _view->count();
        for (long i = 0; i < count; ++i) {
            LabelViewInfo& info = _view->item_ref(i);
            if (info._view == view) {
                _view->remove(i);
                break;
            }
        }
    }
}

void LabelItem::notify () {
    if (_view != nil) {
        long count = _view->count();
        for (long i = 0; i < count; ++i) {
            LabelViewInfo& info = _view->item_ref(i);
            info._view->update();
        }
    }
    Item::notify();
}
