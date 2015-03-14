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
 * PagenumberItem
 */

#include "PagenoItem.h"

#include "PagenoView.h"
#include "Document.h"
#include "DocViewer.h"

#include <OS/list.h>

#include <string.h>

class PagenumberViewInfo {
public:
    PagenumberView* _view;
};

declareList(PagenumberViewInfo_List,PagenumberViewInfo)
implementList(PagenumberViewInfo_List,PagenumberViewInfo)

PagenumberItem::PagenumberItem (
    Document* document, Item* parent, long style, long source
) : Item (document, parent, style, source) {
    _sample = nil;
    _view = nil;
}

PagenumberItem::~PagenumberItem () {
    delete _sample;
    if (_view != nil) {
        while (_view->count() > 0) {
            PagenumberViewInfo& view = _view->item_ref(0);
            _view->remove(0);
        }
        delete _view;
    }
}

void PagenumberItem::sample (const char* sample) {
    delete _sample;
    if (sample != nil) {
        _sample = strcpy(new char[strlen(sample) + 1], sample);
    } else {
        _sample = nil;
    }
}

const char* PagenumberItem::sample () {
    return _sample;
}

void PagenumberItem::read (istream& in) {
    if (_document != nil) {
        _document->read(in, this, _style, _source);
    }
}

void PagenumberItem::write (ostream& out) {
    if (_document != nil) {
        _document->write(out, this, _style, _source);
    }
}

void PagenumberItem::attach (PagenumberView* view) {
    if (_view == nil) {
        _view = new PagenumberViewInfo_List();
    }
    PagenumberViewInfo info;
    info._view = view;
    _view->append(info);
}

void PagenumberItem::detach (PagenumberView* view) {
    if (_view != nil) {
        long count = _view->count();
        for (long i = 0; i < count; ++i) {
            PagenumberViewInfo& info = _view->item_ref(i);
            if (info._view == view) {
                _view->remove(i);
                break;
            }
        }
    }
}

Glyph* PagenumberItem::view (ItemView* parent, DocumentViewer* viewer) {
    return viewer->view(parent, this);
}
