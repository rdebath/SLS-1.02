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
 * TabularItem
 */

#include "TabularItem.h"

#include "Document.h"
#include "TabularView.h"
#include "TextItem.h"
#include "DocViewer.h"
#include <OS/list.h>

#include <string.h>

class CellInfo {
public:
    TextItem* _text;
    long _row;
    long _column;
};

class ColumnInfo {
public:
    ColumnAlignment _alignment;
    ColumnSeparator _separator;
};

class RowInfo {
public:
    RowSeparator _separator;
};

class TabularViewInfo {
public:
    TabularView* _view;
};

static void initialize_text (TextItem* text, const char* contents) {
    const char* p = contents;
    long index = 0;
    while (*p != '\0') {
        index = text->insert(index, *p, 1, 0, nil);
        ++p;
    }
}

declareList(CellInfo_List,CellInfo)
implementList(CellInfo_List,CellInfo)

declareList(ColumnInfo_List,ColumnInfo)
implementList(ColumnInfo_List,ColumnInfo)

declareList(RowInfo_List,RowInfo)
implementList(RowInfo_List,RowInfo)

declareList(TabularViewInfo_List,TabularViewInfo)
implementList(TabularViewInfo_List,TabularViewInfo)

TabularItem::TabularItem (
    Document* document, Item* parent, long style, long source
) : Item(document, parent, style, source) {
    _cell = new CellInfo_List();
    _column = new ColumnInfo_List();
    _row = new RowInfo_List();
    _view = nil;
    ColumnInfo column;
    column._separator = ColumnSeparatorSingle;
    _column->append(column);
    RowInfo row;
    row._separator = RowSeparatorSingle;
    _row->append(row);
}

TabularItem::~TabularItem () {
    while (_cell->count() > 0) {
        CellInfo& cell = _cell->item_ref(0);
        cell._text->unref();
        _cell->remove(0);
    }
    delete _cell;
    delete _column;
    delete _row;
    if (_view != nil) {
        while (_view->count() > 0) {
            TabularViewInfo& view = _view->item_ref(0);
            _view->remove(0);
        }
        delete _view;
    }
}

void TabularItem::style (long style) {
    Item::style(style);
    long c = _cell->count();
    for (long i = 0; i < c; ++i) {
        CellInfo& cell = _cell->item_ref(i);
        cell._text->style(style);
    }
}

long TabularItem::style () {
    return Item::style();
}

void TabularItem::write (ostream& out) {
    if (_document != nil) {
        _document->write(out, this, _style, _source);
    }
}
    
void TabularItem::read (istream& in) {
    if (_document != nil) {
        _document->read(in, this, _style, _source);
    }
}

Glyph* TabularItem::view (ItemView* parent, DocumentViewer* viewer) {
    return viewer->view(parent, this);
}

void TabularItem::label (const char* context) {
    long c = _cell->count();
    for (long i = 0; i < c; ++i) {
        CellInfo& cell = _cell->item_ref(i);
        cell._text->label(context);
    }
}

void TabularItem::change (Item* component) {
    long c = _cell->count();
    for (long i = 0; i < c; ++i) {
        CellInfo& cell = _cell->item_ref(i);
        if (cell._text == component) {
            if (_view != nil) {
                long c = _view->count();
                for (long j = 0; j < c; ++j) {
                    TabularViewInfo& info = _view->item_ref(j);
                    info._view->cell_changed(cell._row, cell._column);
                }
            }
        }
    }
    Item::change(component);
}

void TabularItem::attach (TabularView* view) {
    if (_view == nil) {
        _view = new TabularViewInfo_List();
    }
    TabularViewInfo info;
    info._view = view;
    _view->append(info);
}

void TabularItem::detach (TabularView* view) {
    if (_view != nil) {
        long count = _view->count();
        for (long i = 0; i < count; ++i) {
            TabularViewInfo& info = _view->item_ref(i);
            if (info._view == view) {
                _view->remove(i);
                break;
            }
        }
    }
}

void TabularItem::notify () {
    if (_view != nil) {
        long count = _view->count();
        for (long i = 0; i < count; ++i) {
            TabularViewInfo& info = _view->item_ref(i);
            info._view->update();
        }
    }
    Item::notify();
}

long TabularItem::row_count () {
    return _row->count() - 1;
}

long TabularItem::column_count () {
    return _column->count() - 1;
}

void TabularItem::insert_row (long index, const char* text) {
    long count;
    count = _cell->count();
    for (long i = 0; i < count; ++i) {
        CellInfo& info = _cell->item_ref(i);
        if (info._row >= index) {
            info._row += 1;
        }
    }
    count = _column->count();
    for (long c = 0; c < count; ++c) {
        CellInfo info;
        info._text = new TextItem(_document, this, 1, 0, "");
        info._text->ref();
        initialize_text(info._text, text);
        info._row = index;
        info._column = c;
        _cell->append(info);
    }
    RowInfo row;
    if (index > 0) {
        row._separator = _row->item_ref(index-1)._separator;
    } else {
        row._separator = RowSeparatorSingle;
    }
    _row->insert(index, row);
    if (_view != nil) {
        count = _view->count();
        for (long v = 0; v < count; ++v) {
            TabularViewInfo& view = _view->item_ref(v);
            view._view->row_inserted(index);
        }
    }
    if (_document != nil) {
        _document->touch(true);
    }
}

void TabularItem::remove_row (long index) {
    long count;
    count = _cell->count();
    for (long i = 0; i < count; ++i) {
        CellInfo& info = _cell->item_ref(i);
        if (info._row > index) {
            info._row -= 1;
        } else if (info._row == index) {
            info._text->unref();
            info._row = -1;
        }
    }
    _row->remove(index);
    if (_view != nil) {
        count = _view->count();
        for (long v = 0; v < count; ++v) {
            TabularViewInfo& view = _view->item_ref(v);
            view._view->row_removed(index);
        }
    }
    if (_document != nil) {
        _document->touch(true);
    }
}

void TabularItem::insert_column (
    long index, const char* text, ColumnAlignment alignment
) {
    long count;
    count = _cell->count();
    for (long i = 0; i < count; ++i) {
        CellInfo& info = _cell->item_ref(i);
        if (info._column >= index) {
            info._column += 1;
        }
    }
    count = _row->count();
    for (long r = 0; r < count; ++r) {
        CellInfo info;
        info._text = new TextItem(_document, this, 1, 0, "");
        info._text->ref();
        initialize_text(info._text, text);
        info._column = index;
        info._row = r;
        _cell->append(info);
    }
    ColumnInfo column;
    if (index > 0) {
        column._separator = _column->item_ref(index-1)._separator;
    } else {
        column._separator = ColumnSeparatorSingle;
    }
    column._alignment = alignment;
    _column->insert(index, column);
    if (_view != nil) {
        count = _view->count();
        for (long v = 0; v < count; ++v) {
            TabularViewInfo& view = _view->item_ref(v);
            view._view->column_inserted(index);
        }
    }
    if (_document != nil) {
        _document->touch(true);
    }
}

void TabularItem::remove_column (long index) {
    long count;
    count = _cell->count();
    for (long i = 0; i < count; ++i) {
        CellInfo& info = _cell->item_ref(i);
        if (info._column > index) {
            info._column -= 1;
        } else if (info._column == index) {
            info._text->unref();
            info._column = -1;
        }
    }
    _column->remove(index);
    if (_view != nil) {
        count = _view->count();
        for (long v = 0; v < count; ++v) {
            TabularViewInfo& view = _view->item_ref(v);
            view._view->column_removed(index);
        }
    }
    if (_document != nil) {
        _document->touch(true);
    }
}

RowSeparator TabularItem::row_separator (long index) {
    return _row->item_ref(index)._separator;
}

ColumnAlignment TabularItem::column_alignment (long index) {
    return _column->item_ref(index)._alignment;
}

ColumnSeparator TabularItem::column_separator (long index) {
    return _column->item_ref(index)._separator;
}

void TabularItem::change_row_separator (long index, RowSeparator separator) {
    RowInfo& row = _row->item_ref(index);
    if (row._separator != separator) {
        row._separator = separator;
        if (_view != nil) {
            long count = _view->count();
            for (long v = 0; v < count; ++v) {
                TabularViewInfo& view = _view->item_ref(v);
                view._view->row_separator_changed(index);
            }
        }
    }
    if (_document != nil) {
        _document->touch(true);
    }
}

void TabularItem::change_column_separator (
    long index, ColumnSeparator separator
) {
    ColumnInfo& column = _column->item_ref(index);
    if (column._separator != separator) {
        column._separator = separator;
        if (_view != nil) {
            long count = _view->count();
            for (long v = 0; v < count; ++v) {
                TabularViewInfo& view = _view->item_ref(v);
                view._view->column_separator_changed(index);
            }
        }
    }
    if (_document != nil) {
        _document->touch(true);
    }
}

TextItem* TabularItem::cell (long row, long column) {
    long count = _cell->count();
    for (long i = 0; i < count; ++i) {
        CellInfo& cell = _cell->item_ref(i);
        if (cell._row == row && cell._column == column) {
            return cell._text;
        }
    }
    return nil;
}
