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

#ifndef TabularItem_h
#define TabularItem_h

#include "Item.h"

class CellInfo_List;
class ColumnInfo_List;
class RowInfo_List;
class TabularView;
class TabularViewInfo_List;
class TextItem;

enum ColumnAlignment {
    ColumnAlignLeft, ColumnAlignCenter, ColumnAlignRight
};

enum ColumnSeparator {
    ColumnSeparatorOff, ColumnSeparatorSingle
};

enum RowSeparator {
    RowSeparatorOff, RowSeparatorSingle
};

class TabularItem : public Item {
public:
    TabularItem (Document*, Item* parent, long style, long source);

    virtual void style (long);
    virtual long style ();

    virtual void read (istream&);
    virtual void write (ostream&);
    virtual void label (const char*);
    virtual void change (Item*);

    virtual Glyph* view (ItemView* parent, DocumentViewer*);

    virtual void attach (TabularView* view);
    virtual void detach (TabularView* view);
    virtual void notify ();

    long row_count ();
    long column_count ();
    void insert_row (long index, const char* text);
    void remove_row (long index);
    void insert_column (
        long index, const char* text, ColumnAlignment = ColumnAlignLeft
    );
    void remove_column (long index);
    void change_column_separator (long index, ColumnSeparator);
    void change_row_separator (long index, RowSeparator);

    RowSeparator row_separator (long index);
    ColumnAlignment column_alignment (long index);
    ColumnSeparator column_separator (long index);
    TextItem* cell (long row, long column);
protected:
    virtual ~TabularItem ();

    CellInfo_List* _cell;
    ColumnInfo_List* _column;
    RowInfo_List* _row;
    TabularViewInfo_List* _view;
};

#endif
