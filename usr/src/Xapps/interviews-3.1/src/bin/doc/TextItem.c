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
 * TextItem
 */

#include "TextItem.h"

#include "Document.h"
#include "DocViewer.h"
#include "TextView.h"

#include <OS/list.h>

#include <ctype.h>
#include <stdio.h>
#include <string.h>

class TextInfo {
public:
    unsigned char _code;
    unsigned char _style;
    unsigned short _source;
    unsigned short _item;
};

class TextComponentInfo {
public:
    Item* _item;
};

class TextViewInfo {
public:
    TextView* _view;
};

declareList(TextInfo_List,TextInfo)
implementList(TextInfo_List,TextInfo)

declareList(TextComponentInfo_List,TextComponentInfo)
implementList(TextComponentInfo_List,TextComponentInfo)

declareList(TextViewInfo_List,TextViewInfo)
implementList(TextViewInfo_List,TextViewInfo)

TextItem::TextItem (
    Document* document, Item* parent, long style, long source,
    const char* parameters, int size_hint
) : Item(document, parent, style, source) {
    _parameters = strcpy(new char[strlen(parameters)+1], parameters);
    _text = new TextInfo_List(size_hint);
    _component = nil;
    _view = nil;
}

TextItem::~TextItem () {
    delete _parameters;
    delete _text;
    if (_component != nil) {
        while (_component->count() > 0) {
            TextComponentInfo& info = _component->item_ref(0);
            if (info._item != nil) {
                info._item->unref();
            }
            _component->remove(0);
        }
        delete _component;
    }
    if (_view != nil) {
        while (_view->count() > 0) {
            TextViewInfo& info = _view->item_ref(0);
            _view->remove(0);
        }
        delete _view;
    }
}

void TextItem::style (long style) {
    Item::style(style);
    replace(0, _text->count(), style);
}

long TextItem::style () {
    return Item::style();
}

void TextItem::parameters (const char* parameters) {
    delete _parameters;
    _parameters = strcpy(new char[strlen(parameters)+1], parameters);
}

const char* TextItem::parameters () {
    return _parameters;
}

float TextItem::format_width () {
    if (_parameters == nil || strlen(_parameters) == 0) {
        return 0;
    } else {
        float metric = _document->convert_metric(_parameters);
        if (metric <= 0) {
            return _document->document_metric("formatwidth") + metric;
        } else {
            return metric;
        }
    }
}

void TextItem::read (istream& in) {
    if (_document != nil) {
        _document->read(in, this, _style, _source);
    }
}

void TextItem::write (ostream& out) {
    if (_document != nil) {
        _document->write(out, this, _style, _source);
    }
}

Glyph* TextItem::view (ItemView* parent, DocumentViewer* viewer) {
    return viewer->view(parent, this);
}

void TextItem::label (const char* context) {
    long c = item_count();
    for (long i = 0; i < c; ++i) {
        Item* ii = item(i);
        if (ii != nil) {
            ii->label(context);
        }
    }
}

void TextItem::change (Item* component) {
    long c = item_count();
    for (long i = 0; i < c; ++i) {
        Item* ii = item(i);
        if (ii == component) {
            if (_view != nil) {
                long c = _view->count();
                for (long j = 0; j < c; ++j) {
                    TextViewInfo& info = _view->item_ref(j);
                    info._view->item_changed(i, 1);
                }
            }
        }
    }
    Item::change(component);
}

void TextItem::attach (TextView* view) {
    if (_view == nil) {
        _view = new TextViewInfo_List();
    }
    TextViewInfo info;
    info._view = view;
    _view->append(info);
}

void TextItem::detach (TextView* view) {
    if (_view != nil) {
        long count = _view->count();
        for (long i = 0; i < count; ++i) {
            TextViewInfo& info = _view->item_ref(i);
            if (info._view == view) {
                _view->remove(i);
                break;
            }
        }
    }
}

void TextItem::notify () {
    if (_view != nil) {
        long count = _view->count();
        for (long i = 0; i < count; ++i) {
            TextViewInfo& info = _view->item_ref(i);
            info._view->update();
        }
    }
    Item::notify();
}

long TextItem::item_count () {
    return _text->count();
}

Item* TextItem::item (long index) {
    if (index < 0 || index >= _text->count()) {
        return nil;
    } else {
        TextInfo& info = _text->item_ref(index);
        if (info._item > 0) {
            return _component->item_ref(info._item)._item;
        } else {
            return nil;
        }
    }
}

long TextItem::item_code (long index) {
    if (index < 0 || index >= _text->count()) {
        return 0;
    } else {
        return _text->item_ref(index)._code;
    }
}

long TextItem::item_style (long index) {
    if (index < 0 || index >= _text->count()) {
        return _style;
    } else {
        return _text->item_ref(index)._style;
    }
}

long TextItem::item_source (long index) {
    if (index < 0 || index >= _text->count()) {
        return _source;
    } else {
        return _text->item_ref(index)._source;
    }
}

long TextItem::insert (
    long index, long code, long style, long source, Item* item
) {
    TextInfo text;
    text._code = (unsigned char)code;
    text._style = (unsigned char)style;
    text._source = (unsigned short)source;
    if (item != nil) {
        TextComponentInfo component;
        if (_component == nil) {
            component._item = nil;
            _component = new TextComponentInfo_List();
            _component->append(component);
        }
        text._item = (unsigned short)(_component->count());
        component._item = item;
        component._item->ref();
        _component->append(component);
    } else {
        text._item = 0;
    }
    _text->insert(index, text);
    if (_view != nil) {
        long count = _view->count();
        for (long i = 0; i < count; ++i) {
            TextViewInfo& info = _view->item_ref(i);
            info._view->item_inserted(index, 1);
        }
    }
    if (_document != nil) {
        _document->touch(true);
    }
    return index + 1;
}

long TextItem::remove (long index, long count) {
    if (_view != nil) {
        long c = _view->count();
        for (long i = 0; i < c; ++i) {
            TextViewInfo& info = _view->item_ref(i);
            info._view->item_removed(index, count);
        }
    }
    for (long i = index; i < index + count; ++i) {
        TextInfo& text = _text->item_ref(index);
        if (text._item != 0) {
            TextComponentInfo& component = _component->item_ref(text._item);
            if (component._item != nil) {
                component._item->unref();
                component._item = nil;
            }
        }
        _text->remove(index);
    }
    if (_document != nil) {
        _document->touch(true);
    }
    return index;
}

void TextItem::replace (long index, long count, long style) {
    for (long i = index; i < index + count; ++i) {
        TextInfo& text = _text->item_ref(i);
        text._style = (unsigned char)style;
        if (text._item != 0) {
            TextComponentInfo& component = _component->item_ref(text._item);
            if (component._item != nil) {
                component._item->style(style);
                component._item->notify();
            }
        }
    }
    if (_view != nil) {
        long c = _view->count();
        for (long i = 0; i < c; ++i) {
            TextViewInfo& info = _view->item_ref(i);
            info._view->item_replaced(index, count);
        }
    }
    if (_document != nil) {
        _document->touch(true);
    }
}
